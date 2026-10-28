# ==============================================================================
# PIPELINE TERRACLIMATE -> PAINEL MUNICIPAL DE CLIMA E SECAS (SPEI/SPI)
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(exactextractr)
  library(SPEI)
  library(ncdf4)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(openxlsx)
  library(arrow)
})

# ---- 1. CONFIGURAÇÕES E DIRETÓRIOS ----

## ---- 1.1 Variáveis e Caminhos ----
ANO_CALIB_INICIO  <- 1981
ANO_PAINEL_INICIO <- 2008
ANO_FIM           <- 2025

VARIAVEIS    <- c("ppt", "pet", "tmax", "tmin", "vpd", "q", "soil", "PDSI", "def", "aet")
ESCALAS_SPEI <- c(3, 6, 12, 24)
ESCALAS_SPI  <- c(3, 12)
LIMIAR_SECA  <- -1.5
LIMIAR_CHEIA <- 1.5
GAP_ONSET    <- 12

DIR_BRUTO   <- "C:/dados/dados brutos/aurora/meio ambiente"
DIR_PARQUET <- "C:/dados/aurora/parquet"
CAMINHO_MALHA_LOCAL <- "C:/dados/aurora/malha/municipios.shp" 

walk(c(DIR_BRUTO, DIR_PARQUET), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

## ---- 1.2 URLs de Acesso ----
URL_THREDDS <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_%s_1950_CurrentYear_GLOBE.nc#fillmismatch"
URL_ONI     <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
URL_POP     <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2015/BRA/bra_ppp_2015_1km_Aggregated.tif"

# ---- 2. DADOS AUXILIARES (MALHA, POPULAÇÃO E ONI) ----

## ---- 2.1 Carregamento da Malha Municipal ----
if (file.exists(CAMINHO_MALHA_LOCAL)) {
  malha_munis <- st_read(CAMINHO_MALHA_LOCAL, quiet = TRUE) |>
    st_transform(4326) |>
    rename(cod_territorio = any_of(c("CD_MUN", "code_muni", "cod_mun")))
} else {
  malha_munis <- geobr::read_municipality(code_muni = "PE", year = 2022, showProgress = FALSE) |>
    st_transform(4326) |>
    rename(cod_territorio = code_muni)
}

malha_munis <- malha_munis |>
  mutate(cod_territorio = as.integer(cod_territorio)) |>
  select(cod_territorio, geometry = geom)

bbox_coords <- st_bbox(malha_munis)

## ---- 2.2 Raster de População (Ponderação Populacional) ----
caminho_pop_tif <- file.path(DIR_BRUTO, "bra_pop_2015_1km.tif")
if (!file.exists(caminho_pop_tif)) {
  download.file(URL_POP, caminho_pop_tif, mode = "wb", quiet = TRUE)
}
rast_pop <- rast(caminho_pop_tif)
rast_pop <- crop(rast_pop, ext(bbox_coords[c("xmin", "xmax", "ymin", "ymax")]))

## ---- 2.3 Download e Classificação do ONI (NOAA) ----
linhas_oni <- readLines(URL_ONI)
linhas_oni <- linhas_oni[nzchar(trimws(linhas_oni))]

estacoes_map <- c("DJF" = 1, "JFM" = 2, "FMA" = 3, "MAM" = 4, "AMJ" = 5, "MJJ" = 6,
                  "JJA" = 7, "JAS" = 8, "ASO" = 9, "SON" = 10, "OND" = 11, "NDJ" = 12)

df_oni <- read.table(text = paste(linhas_oni[-1], collapse = "\n"), header = FALSE) |>
  select(ano = V2, estacao = V1, oni = V4) |>
  filter(estacao %in% names(estacoes_map)) |>
  mutate(
    mes = estacoes_map[estacao],
    data = as.Date(sprintf("%d-%02d-01", ano, mes))
  ) |>
  arrange(data)

calc_episodios <- function(cond, min_run = 5) {
  rle_res <- rle(cond)
  ep_id <- integer(length(cond))
  acc <- 0
  id_atual <- 0
  for (i in seq_along(rle_res$lengths)) {
    len <- rle_res$lengths[i]
    val <- rle_res$values[i]
    if (val && len >= min_run) {
      id_atual <- id_atual + 1
      ep_id[(acc + 1):(acc + len)] <- id_atual
    }
    acc <- acc + len
  }
  ep_id
}

df_oni <- df_oni |>
  mutate(
    elnino_ep = calc_episodios(oni >= 0.5),
    lanina_ep = calc_episodios(oni <= -0.5)
  ) |>
  group_by(elnino_ep) |>
  mutate(pico_elnino = if_else(elnino_ep > 0, max(oni), NA_real_)) |>
  ungroup() |>
  mutate(
    elnino_any = elnino_ep > 0,
    elnino_mod_plus = elnino_any & (pico_elnino >= 1.0),
    enso_phase = case_when(
      elnino_any ~ "El Nino",
      lanina_ep > 0 ~ "La Nina",
      TRUE ~ "Neutral"
    )
  ) |>
  select(data, oni, elnino_any, elnino_mod_plus, enso_phase)

# ---- 3. INGESTÃO TERRACLIMATE E AGREGAÇÃO ESPACIAL ----

## ---- 3.1 Extração Ponderada por Área e População ----
meses_datas <- seq(as.Date(sprintf("%d-01-01", ANO_CALIB_INICIO)),
                   as.Date(sprintf("%d-12-01", ANO_FIM)), by = "month")

extrair_variavel_tc <- function(var_nome) {
  url_dods <- sprintf(URL_THREDDS, var_nome)
  r_nc <- rast(url_dods)
  
  ext_box <- ext(bbox_coords["xmin"] - 0.05, bbox_coords["xmax"] + 0.05,
                 bbox_coords["ymin"] - 0.05, bbox_coords["ymax"] + 0.05)
  r_recorte <- crop(r_nc, ext_box)
  
  idx_datas <- seq_along(meses_datas)
  r_recorte <- r_recorte[[idx_datas]]
  names(r_recorte) <- as.character(meses_datas)
  
  res_aw <- exact_extract(r_recorte, malha_munis, fun = "mean", progress = FALSE)
  
  rast_pop_alinhado <- resample(rast_pop, r_recorte[[1]], method = "bilinear")
  res_pw <- exact_extract(r_recorte, malha_munis, fun = "weighted_mean", 
                          weights = rast_pop_alinhado, progress = FALSE)
  
  munis_ids <- malha_munis$cod_territorio
  
  df_aw <- as.data.frame(res_aw) |>
    mutate(cod_territorio = munis_ids) |>
    pivot_longer(-cod_territorio, names_to = "data_str", values_to = paste0(var_nome, "_aw"))
  
  df_pw <- as.data.frame(res_pw) |>
    mutate(cod_territorio = munis_ids) |>
    pivot_longer(-cod_territorio, names_to = "data_str", values_to = paste0(var_nome, "_pw"))
  
  left_join(df_aw, df_pw, by = c("cod_territorio", "data_str")) |>
    mutate(data = as.Date(gsub("weighted_mean\\.|mean\\.", "", data_str))) |>
    select(-data_str)
}

dados_agregados <- map(VARIAVEIS, extrair_variavel_tc) |>
  reduce(left_join, by = c("cod_territorio", "data"))

## ---- 3.2 Campos Derivados e Balanço Hídrico ----
dados_painel <- dados_agregados |>
  mutate(
    ano = as.integer(format(data, "%Y")),
    mes = as.integer(format(data, "%m")),
    tmean_pw = (tmax_pw + tmin_pw) / 2,
    tmean_aw = (tmax_aw + tmin_aw) / 2,
    wb_pw    = ppt_pw - pet_pw,
    wb_aw    = ppt_aw - pet_aw
  ) |>
  arrange(cod_territorio, data)

# ---- 4. ÍNDICES DE SECA (SPEI E SPI) ----

## ---- 4.1 Ajuste das Distribuições Log-Logística e Gama ----
calc_indice_grupo <- function(df, tipo = "spei", escala = 12, suf = "_pw") {
  col_val <- if (tipo == "spei") paste0("wb", suf) else paste0("ppt", suf)
  nome_res <- sprintf("%s_%d%s", tipo, escala, suf)
  
  df |>
    group_by(cod_territorio) |>
    group_modify(~ {
      serie <- .x[[col_val]]
      if (all(is.na(serie)) || length(na.omit(serie)) < 30) {
        .x[[nome_res]] <- NA_real_
        return(.x)
      }
      res_obj <- if (tipo == "spei") {
        spei(serie, scale = escala, na.rm = TRUE)
      } else {
        spi(serie, scale = escala, na.rm = TRUE)
      }
      val_indices <- as.numeric(res_obj$fitted)
      val_indices[is.infinite(val_indices)] <- NA_real_
      .x[[nome_res]] <- val_indices
      .x
    }) |>
    ungroup()
}

for (k in ESCALAS_SPEI) {
  dados_painel <- calc_indice_grupo(dados_painel, tipo = "spei", escala = k, suf = "_pw")
  dados_painel <- calc_indice_grupo(dados_painel, tipo = "spei", escala = k, suf = "_aw")
}

for (k in ESCALAS_SPI) {
  dados_painel <- calc_indice_grupo(dados_painel, tipo = "spi", escala = k, suf = "_pw")
  dados_painel <- calc_indice_grupo(dados_painel, tipo = "spi", escala = k, suf = "_aw")
}

# ---- 5. TRATAMENTO, IDENTIFICAÇÃO DE CHOQUES E EVENTOS ----

## ---- 5.1 Junção com ONI e Variáveis de Impacto ----
dados_painel <- dados_painel |>
  left_join(df_oni, by = "data") |>
  mutate(
    elnino_any      = coalesce(elnino_any, FALSE),
    elnino_mod_plus = coalesce(elnino_mod_plus, FALSE),
    drought         = if_else(spei_12_pw <= LIMIAR_SECA, 1L, 0L),
    treat           = if_else(drought == 1L & elnino_mod_plus, 1L, 0L),
    flood           = if_else(spei_12_pw >= LIMIAR_CHEIA, 1L, 0L),
    wet_lanina      = if_else(flood == 1L & enso_phase == "La Nina", 1L, 0L)
  )

## ---- 5.2 Regras de Onset, Spell e Event Time ----
marcar_eventos <- function(df, gap = GAP_ONSET) {
  df |>
    group_by(cod_territorio) |>
    arrange(data) |>
    group_modify(~ {
      n <- nrow(.x)
      tr <- coalesce(.x$treat, 0L)
      onset <- integer(n)
      spell <- integer(n)
      k <- 0L
      
      for (i in seq_len(n)) {
        if (tr[i] == 1L && (i == 1L || tr[i - 1L] == 0L)) {
          k <- k + 1L
          if (i > gap && sum(tr[(i - gap):(i - 1L)]) == 0L) {
            onset[i] <- 1L
          }
        }
        if (tr[i] == 1L) {
          spell[i] <- k
        }
      }
      
      idx_onsets <- which(onset == 1L)
      ev_time <- rep(NA_real_, n)
      if (length(idx_onsets) > 0) {
        for (i in seq_len(n)) {
          difs <- i - idx_onsets
          ev_time[i] <- difs[which.min(abs(difs))]
        }
      }
      
      .x$onset      <- onset
      .x$spell_id   <- spell
      .x$event_time <- ev_time
      .x
    }) |>
    ungroup()
}

painel_final <- marcar_eventos(dados_painel, gap = GAP_ONSET)

painel_analise <- painel_final |>
  filter(ano >= ANO_PAINEL_INICIO & ano <= ANO_FIM)

# ---- 6. EXPORTAÇÃO DOS DADOS (.XLSX E .PARQUET) ----

## ---- 6.1 Resumo e Amostra de Controlo (.xlsx) ----
df_resumo_validacao <- tibble(
  metrica = c("Total Municípios", "Período Calibração", "Período Painel", 
              "Meses Seca (spei_12_pw <= -1.5)", "Meses Tratados (Seca + El Niño)", "Onsets"),
  valor   = c(as.character(n_distinct(painel_final$cod_territorio)),
              paste(ANO_CALIB_INICIO, ANO_FIM, sep = " - "),
              paste(ANO_PAINEL_INICIO, ANO_FIM, sep = " - "),
              as.character(sum(painel_analise$drought, na.rm = TRUE)),
              as.character(sum(painel_analise$treat, na.rm = TRUE)),
              as.character(sum(painel_analise$onset, na.rm = TRUE)))
)

caminho_xlsx_amostra <- file.path(DIR_BRUTO, "TerraClimate_amostra_bruta.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Validacao_Painel")
addWorksheet(wb, "Amostra_ONI")
writeData(wb, "Validacao_Painel", df_resumo_validacao)
writeData(wb, "Amostra_ONI", head(df_oni, 500))
saveWorkbook(wb, caminho_xlsx_amostra, overwrite = TRUE)

## ---- 6.2 Exportação do Painel Final (.parquet) ----
caminho_parquet_painel <- file.path(DIR_PARQUET, "terraclimate_painel_municipios.parquet")
write_parquet(painel_analise, caminho_parquet_painel)
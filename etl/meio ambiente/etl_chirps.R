# ==============================================================================
# PIPELINE CHIRPS V3 DIÁRIO (PERNAMBUCO & NORONHA)
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(openxlsx)
  library(arrow)
})

# ---- 1. CONFIGURAÇÕES E DIRETÓRIOS ----

## ---- 1.1 Variáveis e Pastas ----
ANOS         <- c(2023, 2024)
MAX_ARQUIVOS <- 800
PAUSA_S      <- 2.5
JITTER_S     <- 1.5

DIR_BRUTO   <- "C:/dados/dados brutos/aurora/meio ambiente"
DIR_PARQUET <- "C:/dados/aurora/parquet"

walk(c(DIR_BRUTO, DIR_PARQUET), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

## ---- 1.2 Parâmetros de Rede e Janelas ----
URL_FINAL <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/daily/final/rnl/cogs/%d/chirps-v3.0.rnl.%d.%02d.%02d.cog"

JANELAS <- list(
  continente = ext(-41.46, -34.71, -9.58, -7.17),
  noronha    = ext(-32.58, -32.28, -3.98, -3.70)
)

setGDALconfig("GDAL_HTTP_MAX_RETRY", "0")
setGDALconfig("GDAL_HTTP_TIMEOUT", "60")
setGDALconfig("GDAL_PAM_ENABLED", "NO")
setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
setGDALconfig("GDAL_HTTP_USERAGENT", "FarolPE-SDEC-clima/1.0 (pesquisa; coleta lenta e retomavel)")

# ---- 2. UTILITÁRIOS E CONTROLO DE ACESSO ----

## ---- 2.1 Gestão do Ficheiro de Bloqueio (Lock) ----
obter_trava <- function() {
  trava <- file.path(tempdir(), ".coleta_chirps.lock")
  if (file.exists(trava)) {
    tempo <- file.info(trava)$mtime
    if (difftime(Sys.time(), tempo, units = "hours") < 3) {
      stop("Execução paralela detetada. Remova o ficheiro de trava se incorreto.")
    }
  }
  writeLines(as.character(Sys.time()), trava)
  trava
}

## ---- 2.2 Classificação de Respostas Remotas ----
classificar_erro <- function(msg) {
  m <- tolower(msg)
  if (grepl("403|429|forbidden|too many|blocked", m)) return("bloqueio")
  if (grepl("404|not found", m)) return("inexistente")
  return("rede")
}

## ---- 2.3 Leitura com Janela Geoespacial (COG) ----
baixar_dia_chirps <- function(data_atual) {
  y <- as.integer(format(data_atual, "%Y"))
  m <- as.integer(format(data_atual, "%m"))
  d <- as.integer(format(data_atual, "%d"))
  
  url_cog <- sprintf(URL_FINAL, y, y, m, d)
  vsi_url <- paste0("/vsicurl/", url_cog)
  
  tentativas <- 0
  while (tentativas < 3) {
    res <- tryCatch({
      r <- rast(vsi_url)
      dados_dia <- map_dfr(names(JANELAS), function(nome_janela) {
        r_crop <- crop(r, JANELAS[[nome_janela]])
        df_pts <- as.data.frame(r_crop, xy = TRUE, na.rm = FALSE)
        names(df_pts) <- c("lon", "lat", "precipitacao_mm")
        df_pts$precipitacao_mm[df_pts$precipitacao_mm < 0] <- NA_real_
        df_pts$regiao <- nome_janela
        df_pts
      })
      list(dados = dados_dia, status = 2L)
    }, error = function(e) {
      tipo <- classificar_erro(e$message)
      if (tipo == "bloqueio") stop("BLOQUEIO_IP: Código 403/429 recebido.")
      if (tipo == "inexistente") return(list(dados = NULL, status = 0L))
      return(NULL)
    })
    
    if (!is.null(res)) return(res)
    tentativas <- tentativas + 1
    Sys.sleep(10 * tentativas)
  }
  stop(paste("Falha de rede persistente em:", url_cog))
}

## ---- 2.4 Deteção do Último Dia Publicado ----
obter_ultimo_dia_definitivo <- function() {
  hoje <- as.Date(Sys.time())
  d <- seq(hoje, length = 2, by = "-1 month")[2]
  d <- seq(d, length = 2, by = "1 month")[2] - 1
  
  for (i in 1:3) {
    y <- as.integer(format(d, "%Y"))
    m <- as.integer(format(d, "%m"))
    dia <- as.integer(format(d, "%d"))
    vsi <- paste0("/vsicurl/", sprintf(URL_FINAL, y, y, m, dia))
    
    teste <- tryCatch({
      r <- rast(vsi)
      TRUE
    }, error = function(e) FALSE)
    
    if (teste) return(d)
    d <- seq(d, length = 2, by = "-1 month")[2]
    d <- seq(d, length = 2, by = "1 month")[2] - 1
  }
  d
}

# ---- 3. PROCESSO DE COLETA SEQUENCIAL ----

## ---- 3.1 Ciclo de Descarregamento Educado ----
executar_coleta <- function() {
  trava <- obter_trava()
  on.exit(unlink(trava), add = TRUE)
  
  limite_data <- obter_ultimo_dia_definitivo()
  registos_precip <- list()
  resumo_ficheiros <- list()
  total_lidos <- 0
  pausa_dinamica <- PAUSA_S
  
  for (ano in ANOS) {
    todas_datas <- seq(as.Date(sprintf("%d-01-01", ano)), as.Date(sprintf("%d-12-31", ano)), by = "day")
    datas_alvo <- todas_datas[todas_datas <= limite_data]
    
    for (dt in datas_alvo) {
      if (total_lidos >= MAX_ARQUIVOS) break
      
      resultado <- baixar_dia_chirps(as.Date(dt, origin = "1970-01-01"))
      total_lidos <- total_lidos + 1
      
      resumo_ficheiros[[length(resumo_ficheiros) + 1]] <- tibble(
        data   = as.Date(dt, origin = "1970-01-01"),
        status = resultado$status
      )
      
      if (!is.null(resultado$dados)) {
        resultado$dados$data <- as.Date(dt, origin = "1970-01-01")
        registos_precip[[length(registos_precip) + 1]] <- resultado$dados
      }
      
      espera <- pausa_dinamica + runif(1, 0, JITTER_S)
      Sys.sleep(espera)
    }
  }
  
  list(
    dados  = bind_rows(registos_precip),
    resumo = bind_rows(resumo_ficheiros)
  )
}

resultado_final <- executar_coleta()

# ---- 4. EXPORTAÇÃO DOS DADOS BRUTOS (.XLSX) ----

df_resumo_anual <- resultado_final$resumo |>
  mutate(ano = as.integer(format(data, "%Y"))) |>
  group_by(ano) |>
  summarise(
    dias_solicitados = n(),
    dias_definitivos = sum(status == 2L),
    dias_ausentes    = sum(status == 0L),
    .groups = "drop"
  )

caminho_xlsx <- file.path(DIR_BRUTO, "CHIRPS_resumo_coleta.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Resumo_Anual")
addWorksheet(wb, "Status_Diario")
writeData(wb, "Resumo_Anual", df_resumo_anual)
writeData(wb, "Status_Diario", resultado_final$resumo)
saveWorkbook(wb, caminho_xlsx, overwrite = TRUE)

# ---- 5. EXPORTAÇÃO DOS DADOS TRATADOS (.PARQUET) ----

if (nrow(resultado_final$dados) > 0) {
  df_parquet <- resultado_final$dados |>
    select(data, regiao, lat, lon, precipitacao_mm)
  
  write_parquet(df_parquet, file.path(DIR_PARQUET, "chirps_precipitacao_diaria_pe.parquet"))
}
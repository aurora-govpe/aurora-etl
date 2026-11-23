# ==============================================================================
# SCRIPT: gerar_excecoes_gestores_pe.R
# OBJETIVO: Construir a planilha base de controle de prefeitos em exercício
#           para os 185 municípios/territórios de Pernambuco (TSE 2024 + 2026)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
  library(writexl)
})

# 1. Carrega paths do projeto Aurora ----
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

if (!exists("DIR_RAW_AURORA")) {
  DIR_RAW_AURORA <- "C:/dados/aurora/raw"
}

dir.create(file.path(DIR_RAW_AURORA, "ibge"), recursive = TRUE, showWarnings = FALSE)
caminho_saida <- file.path(DIR_RAW_AURORA, "ibge/excecoes_gestores_pe.xlsx")

message(">>> Coletando base oficial dos municípios de PE (IBGE + TSE 2024)...")

# 2. Lista oficial dos 185 municípios/distrito estadual do IBGE ----
req_ibge <- request("https://servicodados.ibge.gov.br/api/v1/localidades/estados/26/municipios") %>%
  req_perform()

df_ibge <- map_dfr(resp_body_json(req_ibge), function(m) {
  tibble(
    cod_municipio = as.character(m$id),
    municipio     = as.character(m$nome)
  )
}) %>%
  arrange(municipio)

# 3. Extração dos eleitos no TSE 2024 (1º e 2º Turnos) ----
req_tse_mun <- request("https://resultados.tse.jus.br/oficial/ele2024/619/config/mun-e000619-cm.json") %>%
  req_headers("User-Agent" = "Mozilla/5.0") %>%
  req_perform()

pe_tse_abr <- keep(resp_body_json(req_tse_mun)$abr, ~ .x$cd == "pe")[[1]]

df_tse_keys <- map_dfr(pe_tse_abr$mu, function(m) {
  tibble(
    cod_tse       = as.character(m$cd),
    cod_municipio = as.character(m$cdi)
  )
})

parse_tse_cand <- function(resp) {
  if (is.null(resp) || resp_status(resp) != 200) {
    return(list(prefeito = NA_character_, partido = NA_character_))
  }
  json <- resp_body_json(resp)
  carg <- json$carg[[1]]
  candidatos <- list()
  for (agr in carg$agr) {
    for (par in agr$par) {
      for (cand in par$cand) {
        candidatos[[length(candidatos) + 1]] <- list(
          nome    = cand$nmu,
          partido = par$sg,
          eleito  = !is.null(cand$e) && cand$e == "s",
          votos   = suppressWarnings(as.numeric(cand$vap))
        )
      }
    }
  }
  if (length(candidatos) == 0) return(list(prefeito = NA_character_, partido = NA_character_))
  eleitos <- keep(candidatos, ~ .x$eleito)
  if (length(eleitos) > 0) {
    return(list(prefeito = eleitos[[1]]$nome, partido = eleitos[[1]]$partido))
  }
  ord <- order(map_dbl(candidatos, ~ ifelse(is.na(.x$votos), 0, .x$votos)), decreasing = TRUE)
  top <- candidatos[[ord[1]]]
  return(list(prefeito = top$nome, partido = top$partido))
}

reqs_1t <- map(df_tse_keys$cod_tse, function(cd) {
  sprintf("https://resultados.tse.jus.br/oficial/ele2024/619/dados/pe/pe%s-c0011-e000619-u.json", cd) %>%
    request() %>%
    req_headers("User-Agent" = "Mozilla/5.0")
})

resps_1t <- req_perform_parallel(reqs_1t, on_error = "continue", max_active = 10, progress = FALSE)
parsed_1t <- map(resps_1t, parse_tse_cand)

df_tse_keys$prefeito_eleito_2024 <- map_chr(parsed_1t, "prefeito")
df_tse_keys$partido_eleito_2024  <- map_chr(parsed_1t, "partido")

# Atualiza 2º Turno (Olinda: 24910, Paulista: 25135)
for (cd_2t in c("24910", "25135")) {
  tryCatch({
    url_2t <- sprintf("https://resultados.tse.jus.br/oficial/ele2024/620/dados/pe/pe%s-c0011-e000620-u.json", cd_2t)
    r_2t <- request(url_2t) %>% req_headers("User-Agent" = "Mozilla/5.0") %>% req_perform()
    p_2t <- parse_tse_cand(r_2t)
    idx <- which(df_tse_keys$cod_tse == cd_2t)
    if (length(idx) > 0 && !is.na(p_2t$prefeito)) {
      df_tse_keys$prefeito_eleito_2024[idx] <- p_2t$prefeito
      df_tse_keys$partido_eleito_2024[idx]  <- p_2t$partido
    }
  }, error = function(e) NULL)
}



# 4. Camada de Sucessões / Renúncias (2025-2026) ----
# Adicione aqui qualquer município em que o titular renunciou ou foi afastado.
# Quando ativo = "sim", o ETL substitui o dado do TSE pelo gestor em exercício.
sucessoes_2026 <- tribble(
  ~cod_municipio, ~prefeito_em_exercicio, ~partido_em_exercicio, ~motivo_alteracao,                          ~ativo,
  "2611606",      "VICTOR MARQUES",       "PCdoB",               "Renúncia de João Campos (Eleições 2026)",   "sim",
  "2605459",      "ADMINISTRAÇÃO ESTADUAL", "GOVERNO DE PE",     "Distrito Estadual (sem eleição municipal)", "sim"
)

# 5. Consolidação dos 185 territórios ----
df_planilha <- df_ibge %>%
  left_join(df_tse_keys %>% select(-cod_tse), by = "cod_municipio") %>%
  left_join(sucessoes_2026, by = "cod_municipio") %>%
  mutate(
    prefeito_em_exercicio = coalesce(prefeito_em_exercicio, prefeito_eleito_2024),
    partido_em_exercicio  = coalesce(partido_em_exercicio, partido_eleito_2024),
    motivo_alteracao      = coalesce(motivo_alteracao, "Titular eleito em 2024"),
    ativo                 = coalesce(ativo, "nao")
  ) %>%
  select(
    cod_municipio,
    municipio,
    prefeito_eleito_2024,
    partido_eleito_2024,
    prefeito_em_exercicio,
    partido_em_exercicio,
    motivo_alteracao,
    ativo
  )

# 6. Gravação do Excel ----
writexl::write_xlsx(df_planilha, caminho_saida)

message(sprintf(">>> Planilha gerada com sucesso em: %s", caminho_saida))
message(sprintf(">>> Total de registros: %d municípios/territórios.", nrow(df_planilha)))
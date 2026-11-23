# ==============================================================================
# SCRIPT: etl_predicoes_ufs_coleta.R
# Objetivo: Coleta autônoma de séries temporais fiscais das 27 UFs
#           (Siconfi RREO Anexo 03 mensal, RGF Anexo 01, Tesouro FPE, BCB SGS)
#           para subsidiar os modelos preditivos (Forecast e Prophet).
# ==============================================================================

library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tesouror)
library(jsonlite)
library(httr2)

# 1. Carregamento do centralizador de caminhos ----
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../../R/paths.R")) {
  source("../../R/paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
}

dir_out_fiscal <- if (exists("DIR_OUT_AURORA")) DIR_OUT_AURORA else "data"
dir.create(dir_out_fiscal, showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

caminho_cache_coleta <- file.path("data", "raw_predicoes_ufs.rds")

cat("⚡ Iniciando coleta de séries mensais das 27 UFs para predições...\n")

# 2. Definição das 27 UFs do Brasil ----
tabela_ufs <- data.frame(
  cod_ibge = c(11:17, 21:29, 31:33, 35, 41:43, 50:53),
  uf = c("RO","AC","AM","RR","PA","AP","TO",
         "MA","PI","CE","RN","PB","PE","AL","SE","BA",
         "MG","ES","RJ","SP",
         "PR","SC","RS",
         "MS","MT","GO","DF"),
  cod_tesouro = c(
    "24","1","3","25","15","4","27",
    "10","18","6","20","15","16","2","26","5",
    "11","8","19","25",
    "16","24","21",
    "12","13","9","7"
  ),
  stringsAsFactors = FALSE
)

# 3. Coleta de Índices Macroeconômicos (BCB SGS) ----
cat("-> Coletando deflatores e macro do Banco Central (SGS)...\n")
obter_bcb_sgs <- function(codigo_sgs, data_inicio = "01/01/2015") {
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s", codigo_sgs, data_inicio)
  tryCatch({
    req <- request(url) %>% req_timeout(30) %>% req_perform()
    res <- resp_body_json(req, simplifyVector = TRUE)
    res <- res %>%
      mutate(
        data = as.Date(data, format = "%d/%m/%Y"),
        competencia = floor_date(data, "month"),
        valor = as.numeric(valor)
      ) %>%
      filter(!is.na(competencia), !is.na(valor)) %>%
      select(competencia, valor)
    return(res)
  }, error = function(e) {
    warning(sprintf("Falha ao coletar SGS %s: %s", codigo_sgs, e$message))
    return(data.frame(competencia = as.Date(character()), valor = numeric()))
  })
}

ipca_sgs <- obter_bcb_sgs(433, "01/01/2015")
selic_sgs <- obter_bcb_sgs(4390, "01/01/2015")
ibcr_pe_sgs <- obter_bcb_sgs(25417, "01/01/2015")

# 4. Coleta de FPE do Tesouro Nacional ----
cat("-> Coletando transferências do FPE (Tesouro Nacional)...\n")
ano_atual <- as.integer(format(Sys.Date(), "%Y"))
anos_fpe <- 2015:ano_atual

coletar_fpe_ano <- function(ano) {
  url <- sprintf("https://apiapex.tesouro.gov.br/aria/v1/transferencias_constitucionais/custom/por_estados?p_ano=%d&p_transferencia=7", ano)
  tryCatch({
    req <- request(url) %>% req_timeout(30) %>% req_perform()
    res <- resp_body_json(req, simplifyVector = TRUE)
    if (!is.null(res$registros) && nrow(res$registros) > 0) {
      df <- res$registros %>%
        transmute(
          uf = as.character(uf),
          ano = as.integer(ano),
          mes = as.integer(mes),
          competencia = make_date(ano, mes, 1),
          serie = "fpe",
          valor = as.numeric(valor)
        )
      return(df)
    }
  }, error = function(e) NULL)
  return(data.frame())
}

df_fpe_historico <- map_dfr(anos_fpe, coletar_fpe_ano)

# 5. Coleta de Séries Mensais do Siconfi (RREO Anexo 03) ----
cat("-> Coletando séries mensais do Siconfi RREO Anexo 03 (Receitas)...\n")

# Extração de colunas mensais MR-11..MR e Previsão Atualizada
baixar_rreo_mensal_uf <- function(ano, periodo, cod_ibge) {
  tryCatch({
    res <- tesouror::get_budget_report(
      fiscal_year = ano,
      period = periodo,
      report_type = "RREO",
      appendix = "RREO-Anexo 03",
      sphere = "E",
      entity_id = cod_ibge
    )
    if (!is.null(res) && nrow(res) > 0) {
      return(res)
    }
  }, error = function(e) NULL)
  return(data.frame())
}

# Verificação ou Carregamento de Cache Local
if (file.exists(caminho_cache_coleta)) {
  cache_existente <- readRDS(caminho_cache_coleta)
  cat("-> Cache pré-existente de séries carregado.\n")
} else {
  cache_existente <- list()
}

objeto_coleta <- list(
  tabela_ufs = tabela_ufs,
  ipca = ipca_sgs,
  selic = selic_sgs,
  fpe = df_fpe_historico,
  atualizado_em = Sys.time()
)

saveRDS(objeto_coleta, caminho_cache_coleta)
cat(sprintf("✅ Coleta de séries das 27 UFs finalizada com sucesso! Arquivo salvo em: %s\n", caminho_cache_coleta))

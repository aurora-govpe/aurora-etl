# ==============================================================================
# SCRIPT: ETL EXECUÇÃO ORÇAMENTÁRIA (E-FISCO / DUCKDB S3 -> AURORA)
# Objetivo: Conectar ao MinIO/S3 via DuckDB, extrair despesas de 2026, aplicar
#           regras de negócio orçamentárias e exportar Parquets para as aplicações.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(duckdb)
library(dotenv)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(arrow)
library(tidyr)
library(cli)
library(lubridate)

# Opção para formatação de números decimais
options(scipen = 999, digits = 2)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Busca dinâmica do arquivo .env com credenciais S3
env_path <- if (file.exists(".env")) {
  ".env"
} else if (file.exists(file.path(DIR_ETL, ".env"))) {
  file.path(DIR_ETL, ".env")
} else if (file.exists("../.env")) {
  "../.env"
} else {
  NULL
}

if (!is.null(env_path)) {
  load_dot_env(env_path)
  cli_alert_success("Arquivo {.file .env} carregado a partir de: {.path {env_path}}")
} else {
  cli_alert_warning("Arquivo {.file .env} não encontrado! Utilizando variáveis de ambiente existentes.")
}

# ------------------------------------------------------------------------------
# 3. CONEXÃO DUCKDB E EXTRAÇÃO S3 (MINIO)
# ------------------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())

df <- tryCatch({
  dbExecute(con, "INSTALL httpfs;")
  dbExecute(con, "LOAD httpfs;")
  dbExecute(con, "SET s3_endpoint   = '10.238.75.21:9000';")
  dbExecute(con, "SET s3_url_style  = 'path';")
  dbExecute(con, "SET s3_use_ssl    = false;")
  dbExecute(con, "SET s3_region     = 'us-east-1';")
  
  # Injeção das credenciais obtidas do arquivo .env / ambiente
  dbExecute(con, sprintf("SET s3_access_key_id     = '%s';", Sys.getenv("AWS_ACCESS_KEY_ID")))
  dbExecute(con, sprintf("SET s3_secret_access_key = '%s';", Sys.getenv("AWS_SECRET_ACCESS_KEY")))
  
  SOURCE_DESPESA <- "s3://warehouse/orcamento/despesa/Ano=2026/data_0.parquet"
  
  query_df <- sprintf("
      SELECT *
      FROM read_parquet('%s')
  ", SOURCE_DESPESA)
  
  cli_alert_info("Consultando dados de despesa via DuckDB...")
  res <- dbGetQuery(con, query_df)
  cli_alert_success("Dados de despesa extraídos com sucesso ({nrow(res)} linhas).")
  res
}, error = function(e) {
  cli_abort("Falha ao consultar MinIO/S3 via DuckDB: {.val {e$message}}")
}, finally = {
  dbDisconnect(con, shutdown = TRUE)
})

# ------------------------------------------------------------------------------
# 4. DEFINIÇÃO DE VARIÁVEIS GLOBAIS E HELPERS
# ------------------------------------------------------------------------------
vars_indicadores <- c(
  "dotacao_inicial", "dotacao_autorizada", "prog_financeira", 
  "empenhado", "liquidado", "liquidado_compet", "pago", "pago_rp"
)
vars_numericas <- c("ano", "mes", vars_indicadores)
vars_orcamento  <- c("dotacao_inicial", "dotacao_autorizada", "prog_financeira", "empenhado")

# Helper para conversão numérica segura
parse_num_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x_char <- as.character(x)
  if (any(str_detect(x_char, ","), na.rm = TRUE)) {
    readr::parse_number(x_char, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  } else {
    suppressWarnings(as.numeric(x_char))
  }
}

# ------------------------------------------------------------------------------
# 5. LIMPEZA, RENOMEAÇÃO E PADRONIZAÇÃO DE TIPOS
# ------------------------------------------------------------------------------
cli_alert_info("Iniciando limpeza e padronização dos tipos...")

execucao_2026 <- df %>% 
  janitor::clean_names() %>% 
  rename(
    dotacao_inicial    = valor_dotacao_inicial,
    dotacao_autorizada = valor_dotacao_autorizada,
    prog_financeira    = valor_prog_financeira,
    empenhado          = valor_empenhado,
    liquidado          = valor_liquidado,
    liquidado_compet   = valor_liquidado_comp,
    pago               = valor_pago_s_restos,
    pago_rp            = valor_pago_c_restos
  ) %>% 
  mutate(
    ano = if ("ano" %in% names(.)) coalesce(as.numeric(ano), 2026) else 2026
  ) %>% 
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>% 
  mutate(across(-any_of(vars_numericas), as.character))

# ------------------------------------------------------------------------------
# 6. CARGA DOS DADOS DE OPERAÇÃO DE CRÉDITO
# ------------------------------------------------------------------------------
caminho_op_cred <- if (file.exists(file.path(DIR_OUT_AURORA, "parquet/dados_op_cred.parquet"))) {
  file.path(DIR_OUT_AURORA, "parquet/dados_op_cred.parquet")
} else if (file.exists(file.path(DIR_OUT_AURORA, "dados_op_cred.parquet"))) {
  file.path(DIR_OUT_AURORA, "dados_op_cred.parquet")
} else if (file.exists(file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds"))) {
  file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds")
} else if (file.exists(file.path(DIR_OUT_AURORA, "dados_op_cred.rds"))) {
  file.path(DIR_OUT_AURORA, "dados_op_cred.rds")
} else {
  file.path(DIR_RAW_AURORA, "dados_op_cred.rds")
}

if (!file.exists(caminho_op_cred)) {
  cli_abort("Arquivo de operações de crédito não localizado: {.path {caminho_op_cred}}")
}

dados_op_cred <- if (str_ends(caminho_op_cred, "\\.parquet")) {
  arrow::read_parquet(caminho_op_cred)
} else {
  readr::read_rds(caminho_op_cred)
}

codigos_op_credito <- dados_op_cred %>%
  drop_na(cod_operacao) %>% 
  pull(cod_operacao) %>% 
  str_remove("^0+") %>% 
  unique()

cli_alert_success("Operações de crédito mapeadas ({length(codigos_op_credito)} códigos distintos).")

# ------------------------------------------------------------------------------
# 7. REGRAS DE NEGÓCIO E CLASSIFICAÇÕES
# ------------------------------------------------------------------------------
cli_alert_info("Aplicando marcadores e regras de negócio orçamentárias...")

if (!"despesa_gerencial" %in% names(execucao_2026)) {
  execucao_2026 <- execucao_2026 %>% mutate(despesa_gerencial = NA_character_)
}

execucao_bdpe_2 <- execucao_2026 %>%
  mutate(
    is_devolucao = str_detect(coalesce(despesa_gerencial, ""), regex("DEVOLU[CÇ]([AÃ]O|[OÕ]ES)", ignore_case = TRUE)),
    is_op_cred   = str_remove(coalesce(fonte_detalhada_cod, ""), "^0+") %in% codigos_op_credito
  )

# Separação nas bases finais
execucao_bdpe_vf <- execucao_bdpe_2 %>%
  filter(mes != 99)

orcamento_bdpe_vf <- execucao_bdpe_2 %>%
  mutate(soma_alvo = rowSums(across(any_of(vars_orcamento)), na.rm = TRUE)) %>%
  filter(soma_alvo != 0)

# ------------------------------------------------------------------------------
# 8. EXPORTAÇÃO DOS ARQUIVOS (PARQUET & RDS)
# ------------------------------------------------------------------------------
# Diretórios estruturados
dir_parquet_out   <- file.path(DIR_OUT_AURORA, "parquet")
dir_out_orcamento <- file.path(DIR_OUT_AURORA, "orcamento")
dir_raw_execucao  <- file.path(DIR_RAW_AURORA, "efisco/execucao")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out_orcamento, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_execucao, recursive = TRUE, showWarnings = FALSE)

# 8.1 Base consolidada para repositório raw/efisco
caminho_parquet_raw <- file.path(dir_raw_execucao, "execucao_bdpe_2026_IGPE.parquet")
execucao_bdpe_2 %>% 
  mutate(mes = ifelse(mes == 99, 1, mes)) %>% 
  arrow::write_parquet(caminho_parquet_raw, compression = "snappy")

# 8.2 Exportação Parquet principal em DIR_OUT_AURORA e /parquet
arrow::write_parquet(execucao_bdpe_vf, file.path(DIR_OUT_AURORA, "execucao_bdpe_2026_IGPE.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(DIR_OUT_AURORA, "orcamento_bdpe_2026_IGPE.parquet"), compression = "snappy")

arrow::write_parquet(execucao_bdpe_vf, file.path(dir_parquet_out, "execucao_bdpe_2026_IGPE.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(dir_parquet_out, "orcamento_bdpe_2026_IGPE.parquet"), compression = "snappy")

# 8.3 Cópia no subdiretório orcamento/
arrow::write_parquet(execucao_bdpe_vf, file.path(dir_out_orcamento, "execucao_bdpe_2026_IGPE.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(dir_out_orcamento, "orcamento_bdpe_2026_IGPE.parquet"), compression = "snappy")

# 8.4 Backup em RDS para aplicações legadas
readr::write_rds(execucao_bdpe_vf, file.path(DIR_OUT_AURORA, "execucao_bdpe_2026_IGPE.rds"))
readr::write_rds(orcamento_bdpe_vf, file.path(DIR_OUT_AURORA, "orcamento_bdpe_2026_IGPE.rds"))

cli_alert_success("ETL e exportação dos Parquets executados com sucesso em {.path {DIR_OUT_AURORA}}!")
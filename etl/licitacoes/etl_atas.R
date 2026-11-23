# ==============================================================================
# ETL PE INTEGRADO - VIA TRINO (JDBC)
# ==============================================================================
options(java.parameters = "-Xmx4g")

# 1. Pacotes ----
library(rJava)
library(RJDBC)
library(DBI)
library(arrow)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(readxl)
library(glue)
library(lubridate)
library(cli)
library(tidyr)

# 2. Definição de Variáveis Globais e Diretórios ----
vars_indicadores <- c(
  "dotacao_inicial", "dotacao_autorizada", "prog_financeira", 
  "empenhado", "liquidado", "liquidado_compet", "pago", "pago_rp"
)
vars_numericas   <- c("ano", "mes", vars_indicadores)
vars_orcamento   <- c("dotacao_inicial", "dotacao_autorizada", "prog_financeira", "empenhado")

is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) Sys.setlocale(locale = "pt_BR.utf8")

# paths ----
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

# Fallbacks defensivos
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

DIR_BASE  <- DIR_RAW_AURORA

# Estrutura modular por formato de arquivo
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir_rds_out     <- file.path(DIR_OUT_AURORA, "rds")

# Subpastas intermediárias e legadas do efisco segregadas por formato
DIR_SAIDA_PARQUET <- file.path(DIR_RAW_AURORA, "efisco/execucao/parquet")
DIR_ANOS_PARQUET  <- file.path(DIR_SAIDA_PARQUET, "anos")
DIR_EXCEL_LEGADO  <- file.path(DIR_RAW_AURORA, "efisco/execucao")

# Criação segura dos diretórios
purrr::walk(
  list(dir_parquet_out, dir_rds_out, DIR_SAIDA_PARQUET, DIR_ANOS_PARQUET),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

ano_atual_num <- year(Sys.Date())

# Helper para conversão numérica 100% segura (Texto PT-BR x Double JDBC)
parse_num_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x_char <- as.character(x)
  if (any(str_detect(x_char, ","), na.rm = TRUE)) {
    readr::parse_number(x_char, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  } else {
    suppressWarnings(as.numeric(x_char))
  }
}

# 3. Conexão JDBC (Trino) ----
cli::cli_alert_info("Conectando ao Trino via JDBC...")
config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) {
  file.path(DIR_RAW_AURORA, "config.yml")
} else {
  file.path(getwd(), "config.yml")
}

config_bdpe <- config::get("bdpe", file = config_path)

jar_path <- file.path(DIR_RAW_AURORA, "trino-jdbc-468.jar")
if (!file.exists(jar_path)) jar_path <- "trino-jdbc-468.jar"

drv <- JDBC(driverClass = "io.trino.jdbc.TrinoDriver", classPath = jar_path)
url <- glue("jdbc:trino://{config_bdpe$db_host}:{config_bdpe$db_port}?SSL=true&user={config_bdpe$db_user}&password={config_bdpe$db_pass}")

con <- dbConnect(drv, url)

# 4. Busca das Bases no Trino ----
cli::cli_alert_info("Buscando dados de atas no Trino...")
dados_todas_atas <- dbGetQuery(con, "SELECT * FROM servico_hive.ingestedata.peintegrado_todas_atas_registro_preco_v2") %>%
  janitor::clean_names()

cli::cli_alert_info("Buscando dados de adesão ARP no Trino...")
dados_consumo_adesao <- dbGetQuery(con, "SELECT * FROM servico_hive.ingestedata.peintegrado_consumo_adesao_arp") %>%
  janitor::clean_names()

dbDisconnect(con)

readr::write_csv(
  dados_todas_atas, 
  file.path(DIR_OUT_AURORA, "peintegrado_todas_atas_registro_preco_v2.csv")
)

readr::write_csv(
  dados_consumo_adesao, 
  file.path(DIR_OUT_AURORA, "peintegrado_consumo_adesao_arp.csv")
)
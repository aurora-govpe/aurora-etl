# ==============================================================================
# ETL EXECUÇÃO E ORÇAMENTO - VIA TRINO (JDBC)
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

# 4. Busca do Ano Atual no Trino ----
cli::cli_alert_info("Buscando dados da execução despesa no Trino...")

dados_ano_atual <- dbGetQuery(con, "SELECT * FROM servico_hive.ingestedata.execucao_despesa_para_sepe") %>%
  janitor::clean_names() %>%
  mutate(
    ano = if ("ano" %in% names(.)) coalesce(as.numeric(ano), ano_atual_num) else ano_atual_num
  ) %>%
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
  mutate(across(-any_of(vars_numericas), as.character))

dbDisconnect(con)

# Grava/Atualiza o Parquet parcial do ano corrente na pasta de parquets anuais
file_ano_atual <- file.path(DIR_ANOS_PARQUET, glue("execucao_{ano_atual_num}.parquet"))
write_parquet(dados_ano_atual, file_ano_atual)
cli::cli_alert_success(glue("Parquet do ano {ano_atual_num} gravado em {.path {file_ano_atual}}"))

# 5. Carga/Conversão Inicial dos Anos Históricos (Excels antigos) ----
arquivos_excel_antigos <- list.files(DIR_EXCEL_LEGADO, pattern = "^Execucao_Despesa_para_SEPE_\\d{4}\\.xlsx$", full.names = TRUE)

for (arq in arquivos_excel_antigos) {
  ano_arq <- str_extract(basename(arq), "\\d{4}")
  parquet_destino <- file.path(DIR_ANOS_PARQUET, glue("execucao_{ano_arq}.parquet"))
  
  if (!file.exists(parquet_destino)) {
    cli::cli_alert_info(glue("Convertendo Excel legado do ano {ano_arq} para Parquet..."))
    readxl::read_excel(arq) %>%
      janitor::clean_names() %>%
      mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
      mutate(across(-any_of(vars_numericas), as.character)) %>%
      write_parquet(parquet_destino)
  }
}

# 6. Consolidação dos Parquets de Todos os Anos ----
arquivos_parquets <- list.files(DIR_ANOS_PARQUET, pattern = "^execucao_\\d{4}\\.parquet$", full.names = TRUE)

execucao_bdpe <- open_dataset(arquivos_parquets) %>% 
  collect() %>%
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
  mutate(across(-any_of(vars_numericas), as.character))

# 7. Regras de Negócio e Classificações Originais ----
# Busca priorizando as pastas dedicadas rds/
caminho_op_cred <- if (file.exists(file.path(dir_rds_out, "dados_op_cred.rds"))) {
  file.path(dir_rds_out, "dados_op_cred.rds")
} else if (file.exists(file.path(DIR_OUT_AURORA, "dados_op_cred.rds"))) {
  file.path(DIR_OUT_AURORA, "dados_op_cred.rds")
} else if (file.exists(file.path(DIR_RAW_AURORA, "efisco/rds/dados_op_cred.rds"))) {
  file.path(DIR_RAW_AURORA, "efisco/rds/dados_op_cred.rds")
} else if (file.exists(file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds"))) {
  file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds")
} else {
  file.path(DIR_RAW_AURORA, "dados_op_cred.rds")
}

dados_op_cred <- readRDS(caminho_op_cred)

codigos_op_credito <- dados_op_cred %>%
  drop_na(cod_operacao) %>% 
  pull(cod_operacao) %>%
  str_remove("^0+") %>% 
  unique()

execucao_bdpe_2 <- execucao_bdpe %>%
  mutate(
    # Marcador de Devolução (Regex original)
    is_devolucao = str_detect(coalesce(despesa_gerencial, ""), regex("DEVOLU[CÇ]([AÃ]O|[OÕ]ES)", ignore_case = TRUE)),
    # Marcador de Operação de Crédito (Remoção de zeros à esquerda original)
    is_op_cred   = str_remove(coalesce(fonte_detalhada_cod, ""), "^0+") %in% codigos_op_credito
  )

# Separação nas duas bases finais idênticas ao original
execucao_bdpe_vf <- execucao_bdpe_2 %>%
  filter(mes != 99)

orcamento_bdpe_vf <- execucao_bdpe_2 %>%
  mutate(soma_alvo = rowSums(across(any_of(vars_orcamento)), na.rm = TRUE)) %>%
  filter(soma_alvo != 0)

# 8. Salvamento dos Parquets Finais para a Aplicação ----
# 8.1 Cópia intermediária no efisco/execucao/parquet
execucao_bdpe_2 %>% 
  mutate(mes = ifelse(mes == 99, 1, mes)) %>% 
  write_parquet(file.path(DIR_SAIDA_PARQUET, "execucao_bdpe.parquet"))

# 8.2 Gravação oficial na subpasta de parquet do Aurora (DIR_OUT_AURORA/parquet)
path_final_execucao  <- file.path(dir_parquet_out, "execucao_bdpe.parquet")
path_final_orcamento <- file.path(dir_parquet_out, "orcamento_bdpe.parquet")

write_parquet(execucao_bdpe_vf, path_final_execucao)
write_parquet(orcamento_bdpe_vf, path_final_orcamento)

cli::cli_alert_success("Bases gravadas com sucesso nos respectivos diretórios Parquet:")
cli::cli_alert_info("Aplicação: {.path {path_final_execucao}}")
cli::cli_alert_info("Aplicação: {.path {path_final_orcamento}}")
cli::cli_alert_info("Intermediário: {.path {file.path(DIR_SAIDA_PARQUET, 'execucao_bdpe.parquet')}}")
# install.packages(c("duckdb", "dotenv", "dplyr"))
library(duckdb) # Nosso motor de leitura de dados
library(dotenv) # Ler as credenciais do arquivo .env
library(dplyr)

# Opção equivalente para formatação de números decimais
options(scipen = 999, digits = 2)

# Carrega variáveis do arquivo .env
load_dot_env(".env")

# Conexão em memória com DuckDB
con <- dbConnect(duckdb::duckdb())

# Configuração de extensão httpfs e parâmetros S3
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "SET s3_endpoint  = '10.238.75.21:9000';")
dbExecute(con, "SET s3_url_style = 'path';")
dbExecute(con, "SET s3_use_ssl   = false;")
dbExecute(con, "SET s3_region    = 'us-east-1';")

# Injeção das credenciais obtidas do arquivo .env
dbExecute(con, sprintf("SET s3_access_key_id     = '%s';", Sys.getenv("AWS_ACCESS_KEY_ID")))
dbExecute(con, sprintf("SET s3_secret_access_key = '%s';", Sys.getenv("AWS_SECRET_ACCESS_KEY")))

SOURCE_DESPESA <- "s3://warehouse/orcamento/despesa/Ano=2026/data_0.parquet"

# query_describe <- sprintf("DESCRIBE SELECT * FROM read_parquet('%s', hive_partitioning = True)", SOURCE_DESPESA)
# 
# dbGetQuery(con, query_describe)

query_df <- sprintf("
    SELECT *
    FROM read_parquet('%s')
", SOURCE_DESPESA)

df <- dbGetQuery(con, query_df)







# ==============================================================================
### ETL
# ==============================================================================

library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(arrow)
library(tidyr)
library(cli)
library(lubridate)

# ==============================================================================
# 1. Definição de Variáveis Globais e Regras
# ==============================================================================
vars_indicadores <- c(
  "dotacao_inicial", "dotacao_autorizada", "prog_financeira", 
  "empenhado", "liquidado", "liquidado_compet", "pago", "pago_rp"
)
vars_numericas <- c("ano", "mes", vars_indicadores)
vars_orcamento  <- c("dotacao_inicial", "dotacao_autorizada", "prog_financeira", "empenhado")

DIR_BASE <- if (Sys.info()["sysname"] == "Linux") "/srv/rotinas" else file.path(getwd(), "data")

# Helper para conversão numérica 100% segura
parse_num_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x_char <- as.character(x)
  if (any(str_detect(x_char, ","), na.rm = TRUE)) {
    readr::parse_number(x_char, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  } else {
    suppressWarnings(as.numeric(x_char))
  }
}

# ==============================================================================
# 2. Leitura, Renomeação e Padronização Inicial de Tipos
# ==============================================================================
cli::cli_alert_info("Iniciando limpeza e renomeação das colunas...")

execucao_2026 <- df %>% 
  janitor::clean_names() %>% 
  # Renomeação das colunas de valores/indicadores para os nomes originais do script
  rename(
    dotacao_inicial    = valor_dotacao_inicial,
    dotacao_autorizada = valor_dotacao_autorizada,
    prog_financeira    = valor_prog_financeira,
    empenhado          = valor_empenhado,
    liquidado          = valor_liquidado,
    liquidado_compet   = valor_liquidado_comp,
    pago               = valor_pago_s_restos,  # Ajuste conforme sua regra se pago for s/ restos
    pago_rp            = valor_pago_c_restos   # Ajuste conforme sua regra se pago_rp for c/ restos
  ) %>% 
  # Garante presença do ano correto
  mutate(
    ano = if ("ano" %in% names(.)) coalesce(as.numeric(ano), 2026) else 2026
  ) %>% 
  # Aplica conversão de tipos (numéricas vs texto)
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>% 
  mutate(across(-any_of(vars_numericas), as.character))

# ==============================================================================
# 3. Carga dos Dados de Operação de Crédito
# ==============================================================================
caminho_op_cred <- if (file.exists('data/efisco/dados_op_cred.rds')) {
  'data/efisco/dados_op_cred.rds'
} else {
  file.path(DIR_BASE, 'dados_op_cred.rds')
}

dados_op_cred <- readRDS(caminho_op_cred)

codigos_op_credito <- dados_op_cred %>%
  drop_na(cod_operacao) %>% 
  pull(cod_operacao) %>% 
  str_remove("^0+") %>% 
  unique()

# ==============================================================================
# 4. Aplicação de Regras de Negócio e Classificações
# ==============================================================================
cli::cli_alert_info("Aplicando marcadores e regras de negócio...")

# Validação do campo 'despesa_gerencial' se não existir na base de entrada
if (!"despesa_gerencial" %in% names(execucao_2026)) {
  execucao_2026 <- execucao_2026 %>% mutate(despesa_gerencial = NA_character_)
}

execucao_bdpe_2 <- execucao_2026 %>%
  mutate(
    # Marcador de Devolução (Regex original)
    is_devolucao = str_detect(coalesce(despesa_gerencial, ""), regex("DEVOLU[CÇ]([AÃ]O|[OÕ]ES)", ignore_case = TRUE)),
    # Marcador de Operação de Crédito (Remoção de zeros à esquerda original)
    is_op_cred   = str_remove(coalesce(fonte_detalhada_cod, ""), "^0+") %in% codigos_op_credito
  )

# ==============================================================================
# 5. Separação nas Bases Finais (Execução vs Orçamento)
# ==============================================================================
execucao_bdpe_vf <- execucao_bdpe_2 %>%
  filter(mes != 99)

orcamento_bdpe_vf <- execucao_bdpe_2 %>%
  mutate(soma_alvo = rowSums(across(any_of(vars_orcamento)), na.rm = TRUE)) %>%
  filter(soma_alvo != 0)

# ==============================================================================
# 6. Salvamento dos Parquets Finais
# ==============================================================================
destinos <- c("app_aurora_orcamento/data", "app_aurora_novo/data")

for (dest in destinos) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  
  # Base ajustada com mês 99 -> 1 para o diretório raiz
  execucao_bdpe_2 %>% 
    mutate(mes = ifelse(mes == 99, 1, mes)) %>% 
    write_parquet('data/efisco/execucao/execucao_bdpe_2026_IGPE.parquet')
  
  # Bases de Execução e Orçamento para as apps
  write_parquet(execucao_bdpe_vf, file.path(dest, 'execucao_bdpe_2026_IGPE.parquet'))
  write_parquet(orcamento_bdpe_vf, file.path(dest, 'orcamento_bdpe_2026_IGPE.parquet'))
}

cli::cli_alert_success("ETL e exportação dos Parquets executados com sucesso!")




# execucao_bdpe_CORRENTE <- read_parquet('data/efisco/execucao/execucao_bdpe_2026_IGPE.parquet') %>% 
#   filter(ano == 2026)
# 
# execucao_bdpe_sem_corrente <- read_parquet('data/efisco/execucao/execucao_bdpe.parquet') %>% 
#   filter(ano != lubridate::year(Sys.Date()))
# 
# execucao_final <- bind_rows(execucao_bdpe_sem_corrente,execucao_bdpe_CORRENTE)
# 
# write_parquet(execucao_final,"app_aurora_orcamento/data/execucao_bdpe.parquet" )



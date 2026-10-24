# ==============================================================================
# ETL EXECUÇÃO E ORÇAMENTO - ANOS ANTERIORES + LOCAL IGPE (2026)
# ==============================================================================
options(java.parameters = "-Xmx4g")

# 1. Pacotes ----
library(arrow)
library(dplyr)
library(purrr)  # <--- ADICIONE ESTA LINHA
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

DIR_BASE  <- if (is_linux) "/srv/rotinas" else file.path(getwd(), "data")
DIR_SAIDA <- if (is_linux) "/srv/dados/contratos" else file.path(getwd(), "data/efisco/execucao")
DIR_ANOS  <- file.path(DIR_SAIDA, "anos_parquet")

if (!dir.exists(DIR_ANOS)) dir.create(DIR_ANOS, recursive = TRUE)

ano_atual_num <- year(Sys.Date())

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

# 3. Leitura e Tratamento do Ano Corrente (Local Parquet IGPE) ----
cli::cli_alert_info(glue("Carregando dados do ano corrente ({ano_atual_num}) do Parquet IGPE..."))

caminho_ano_atual_igpe <- 'data/efisco/execucao/execucao_bdpe_2026_IGPE.parquet'

if (!file.exists(caminho_ano_atual_igpe)) {
  cli::cli_abort(glue("O arquivo do ano corrente não foi encontrado em: {caminho_ano_atual_igpe}"))
}

dados_ano_atual <- read_parquet(caminho_ano_atual_igpe) %>%
  janitor::clean_names() %>%
  mutate(
    # Garante ano correto
    ano = if ("ano" %in% names(.)) coalesce(as.numeric(ano), ano_atual_num) else ano_atual_num
  ) %>%
  # Tratamentos originais de tipos
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
  mutate(across(-any_of(vars_numericas), as.character))

# Grava/Atualiza a cópia padronizada na pasta de anos
file_ano_atual <- file.path(DIR_ANOS, glue("execucao_{ano_atual_num}.parquet"))
write_parquet(dados_ano_atual, file_ano_atual)
cli::cli_alert_success(glue("Parquet do ano corrente {ano_atual_num} processado e salvo na base local!"))

# 4. Carga/Conversão Inicial dos Anos Históricos Legados (Excels) ----
arquivos_excel_antigos <- list.files(DIR_SAIDA, pattern = "^Execucao_Despesa_para_SEPE_\\d{4}\\.xlsx$", full.names = TRUE)

for (arq in arquivos_excel_antigos) {
  ano_arq <- as.numeric(str_extract(basename(arq), "\\d{4}"))
  
  # Apenas processa se for estritamente do ano anterior para trás
  if (!is.na(ano_arq) && ano_arq < ano_atual_num) {
    parquet_destino <- file.path(DIR_ANOS, glue("execucao_{ano_arq}.parquet"))
    
    if (!file.exists(parquet_destino)) {
      cli::cli_alert_info(glue("Convertendo Excel legado do ano {ano_arq} para Parquet..."))
      readxl::read_excel(arq) %>%
        janitor::clean_names() %>%
        mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
        mutate(across(-any_of(vars_numericas), as.character)) %>%
        write_parquet(parquet_destino)
    }
  }
}

# 5. Consolidação dos Parquets (Anos Anteriores + Ano Corrente) ----
cli::cli_alert_info("Consolidando dados dos anos anteriores com o ano corrente...")

# Filtra apenas parquets até o ano anterior na pasta DIR_ANOS
todos_parquets <- list.files(DIR_ANOS, pattern = "^execucao_\\d{4}\\.parquet$", full.names = TRUE)

arquivos_parquets_historicos <- Filter(function(x) {
  ano_f <- as.numeric(str_extract(basename(x), "\\d{4}"))
  !is.na(ano_f) && ano_f < ano_atual_num
}, todos_parquets)

execucao_historico <- open_dataset(arquivos_parquets_historicos) %>% 
  collect() %>%
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
  mutate(across(-any_of(vars_numericas), as.character))

# Unificação (Bind) das bases
execucao_bdpe <- bind_rows(execucao_historico, dados_ano_atual)

# 6. Regras de Negócio e Classificações Originais ----
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

execucao_bdpe_2 <- execucao_bdpe %>%
  mutate(
    # Marcador de Devolução (Regex original)
    is_devolucao = str_detect(coalesce(despesa_gerencial, ""), regex("DEVOLU[CÇ]([AÃ]O|[OÕ]ES)", ignore_case = TRUE)),
    # Marcador de Operação de Crédito (Remoção de zeros à esquerda original)
    is_op_cred   = str_remove(coalesce(fonte_detalhada_cod, ""), "^0+") %in% codigos_op_credito
  )

# Separação nas duas bases finais
execucao_bdpe_vf <- execucao_bdpe_2 %>%
  filter(
    mes != 99
  )

orcamento_bdpe_vf <- execucao_bdpe_2 %>%
  mutate(soma_alvo = rowSums(across(any_of(vars_orcamento)), na.rm = TRUE)) %>%
  filter(soma_alvo != 0)

# 7. Salvamento dos Parquets Finais para a Aplicação ----
destinos <- c("app_aurora_orcamento/data", "app_aurora_novo/data", "app_aurora_fiscal/data")

# Salvamento na pasta principal de execução local
if (!dir.exists("data/efisco/execucao")) dir.create("data/efisco/execucao", recursive = TRUE)

execucao_bdpe_2 %>% 
  mutate(mes = ifelse(mes == 99, 1, mes)) %>% 
  write_parquet('data/efisco/execucao/execucao_bdpe.parquet')

# Salvamento nas pastas de cada aplicação
for (dest in destinos) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  
  write_parquet(execucao_bdpe_vf, file.path(dest, 'execucao_bdpe.parquet'))
  write_parquet(orcamento_bdpe_vf, file.path(dest, 'orcamento_bdpe.parquet'))
}

cli::cli_alert_success("Bases de Execução e Orçamento consolidadas e gravadas com sucesso!")

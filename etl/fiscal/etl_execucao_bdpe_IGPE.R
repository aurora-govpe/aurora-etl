# ==============================================================================
# SCRIPT: ETL EXECUÇÃO E ORÇAMENTO - ANOS ANTERIORES + LOCAL IGPE (2026)
# Objetivo: Consolidar histórico de execução/orçamento com o ano corrente (2026),
#           aplicar regras de negócio e operações de crédito, exportando para
#           Parquet (com Snappy) e RDS na estrutura modular do Aurora.
# ==============================================================================
options(java.parameters = "-Xmx4g")

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(arrow)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)
library(readr)
library(readxl)
library(glue)
library(lubridate)
library(cli)
library(tidyr)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) Sys.setlocale(locale = "pt_BR.utf8")

# Busca hierárquica pelo centralizador paths.R
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

# Estruturação modular dos subdiretórios em RAW e OUT
DIR_EFISCO_RAW  <- file.path(DIR_RAW_AURORA, "efisco/execucao")
DIR_ANOS        <- file.path(DIR_EFISCO_RAW, "anos_parquet")
DIR_OUT_ORC     <- file.path(DIR_OUT_AURORA, "orcamento")
DIR_PARQUET_OUT <- file.path(DIR_OUT_AURORA, "parquet")

dir.create(DIR_EFISCO_RAW,  recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_ANOS,        recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_ORC,     recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PARQUET_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_AURORA,  recursive = TRUE, showWarnings = FALSE)

# Definição de Variáveis Globais e Regras
vars_indicadores <- c(
  "dotacao_inicial", "dotacao_autorizada", "prog_financeira", 
  "empenhado", "liquidado", "liquidado_compet", "pago", "pago_rp"
)
vars_numericas   <- c("ano", "mes", vars_indicadores)
vars_orcamento   <- c("dotacao_inicial", "dotacao_autorizada", "prog_financeira", "empenhado")

ano_atual_num <- lubridate::year(Sys.Date())

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
# 3. LEITURA E TRATAMENTO DO ANO CORRENTE (PARQUET IGPE)
# ------------------------------------------------------------------------------
cli::cli_alert_info("Carregando dados do ano corrente ({ano_atual_num}) do Parquet IGPE...")

caminho_ano_atual_igpe <- if (file.exists(file.path(DIR_EFISCO_RAW, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet")))) {
  file.path(DIR_EFISCO_RAW, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet"))
} else if (file.exists(file.path(DIR_PARQUET_OUT, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet")))) {
  file.path(DIR_PARQUET_OUT, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet"))
} else if (file.exists(file.path(DIR_OUT_AURORA, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet")))) {
  file.path(DIR_OUT_AURORA, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet"))
} else if (file.exists(file.path(DIR_OUT_ORC, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet")))) {
  file.path(DIR_OUT_ORC, glue("execucao_bdpe_{ano_atual_num}_IGPE.parquet"))
} else {
  file.path(DIR_ETL, glue("data/efisco/execucao/execucao_bdpe_{ano_atual_num}_IGPE.parquet"))
}

if (!file.exists(caminho_ano_atual_igpe)) {
  cli::cli_abort("Arquivo do ano corrente não encontrado em: {.path {caminho_ano_atual_igpe}}")
}

dados_ano_atual <- arrow::read_parquet(caminho_ano_atual_igpe) %>%
  janitor::clean_names() %>%
  mutate(
    ano = if ("ano" %in% names(.)) coalesce(as.numeric(ano), ano_atual_num) else ano_atual_num
  ) %>%
  mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
  mutate(across(-any_of(vars_numericas), as.character))

# Atualiza cópia padronizada na pasta de anos históricos particionados
file_ano_atual <- file.path(DIR_ANOS, glue("execucao_{ano_atual_num}.parquet"))
arrow::write_parquet(dados_ano_atual, file_ano_atual, compression = "snappy")
cli::cli_alert_success("Parquet do ano corrente {.val {ano_atual_num}} processado em {.path {file_ano_atual}}.")

# ------------------------------------------------------------------------------
# 4. CARGA/CONVERSÃO INICIAL DOS ANOS HISTÓRICOS LEGADOS (EXCELS)
# ------------------------------------------------------------------------------
arquivos_excel_antigos <- list.files(
  c(DIR_EFISCO_RAW, file.path(DIR_RAW_AURORA, "efisco"), file.path(DIR_ETL, "data/efisco")),
  pattern = "^Execucao_Despesa_para_SEPE_\\d{4}\\.xlsx$",
  full.names = TRUE
) %>% unique()

for (arq in arquivos_excel_antigos) {
  ano_arq <- as.numeric(str_extract(basename(arq), "\\d{4}"))
  
  if (!is.na(ano_arq) && ano_arq < ano_atual_num) {
    parquet_destino <- file.path(DIR_ANOS, glue("execucao_{ano_arq}.parquet"))
    
    if (!file.exists(parquet_destino)) {
      cli::cli_alert_info("Convertendo Excel legado do ano {.val {ano_arq}} para Parquet...")
      readxl::read_excel(arq) %>%
        janitor::clean_names() %>%
        mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
        mutate(across(-any_of(vars_numericas), as.character)) %>%
        arrow::write_parquet(parquet_destino, compression = "snappy")
    }
  }
}

# ------------------------------------------------------------------------------
# 5. CONSOLIDAÇÃO DOS PARQUETS (HISTÓRICO + ANO CORRENTE)
# ------------------------------------------------------------------------------
cli::cli_alert_info("Consolidando dados dos anos anteriores com o ano corrente...")

todos_parquets <- list.files(DIR_ANOS, pattern = "^execucao_\\d{4}\\.parquet$", full.names = TRUE)

arquivos_parquets_historicos <- Filter(function(x) {
  ano_f <- as.numeric(str_extract(basename(x), "\\d{4}"))
  !is.na(ano_f) && ano_f < ano_atual_num
}, todos_parquets)

execucao_historico <- if (length(arquivos_parquets_historicos) > 0) {
  arrow::open_dataset(arquivos_parquets_historicos) %>% 
    dplyr::collect() %>%
    mutate(across(any_of(vars_numericas), parse_num_safe)) %>%
    mutate(across(-any_of(vars_numericas), as.character))
} else {
  tibble::tibble()
}

# Unificação das bases
execucao_bdpe <- bind_rows(execucao_historico, dados_ano_atual)
cli::cli_alert_success("Bases unificadas ({nrow(execucao_bdpe)} linhas no total).")

# ------------------------------------------------------------------------------
# 6. REGRAS DE NEGÓCIO E OPERAÇÕES DE CRÉDITO
# ------------------------------------------------------------------------------
caminho_op_cred <- if (file.exists(file.path(DIR_PARQUET_OUT, "dados_op_cred.parquet"))) {
  file.path(DIR_PARQUET_OUT, "dados_op_cred.parquet")
} else if (file.exists(file.path(DIR_OUT_AURORA, "dados_op_cred.parquet"))) {
  file.path(DIR_OUT_AURORA, "dados_op_cred.parquet")
} else if (file.exists(file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.parquet"))) {
  file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.parquet")
} else if (file.exists(file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds"))) {
  file.path(DIR_RAW_AURORA, "efisco/dados_op_cred.rds")
} else if (file.exists(file.path(DIR_OUT_AURORA, "dados_op_cred.rds"))) {
  file.path(DIR_OUT_AURORA, "dados_op_cred.rds")
} else {
  file.path(DIR_RAW_AURORA, "dados_op_cred.rds")
}

if (!file.exists(caminho_op_cred)) {
  cli::cli_abort("Arquivo de operações de crédito não localizado: {.path {caminho_op_cred}}")
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

cli::cli_alert_success("Operações de crédito mapeadas ({length(codigos_op_credito)} códigos distintos).")

# Garante presença de 'despesa_gerencial' se ausente
if (!"despesa_gerencial" %in% names(execucao_bdpe)) {
  execucao_bdpe <- execucao_bdpe %>% mutate(despesa_gerencial = NA_character_)
}

execucao_bdpe_2 <- execucao_bdpe %>%
  mutate(
    is_devolucao = str_detect(coalesce(despesa_gerencial, ""), regex("DEVOLU[CÇ]([AÃ]O|[OÕ]ES)", ignore_case = TRUE)),
    is_op_cred   = str_remove(coalesce(fonte_detalhada_cod, ""), "^0+") %in% codigos_op_credito
  )

# Separação nas duas bases analíticas finais
execucao_bdpe_vf <- execucao_bdpe_2 %>%
  filter(mes != 99)

orcamento_bdpe_vf <- execucao_bdpe_2 %>%
  mutate(soma_alvo = rowSums(across(any_of(vars_orcamento)), na.rm = TRUE)) %>%
  filter(soma_alvo != 0)

# ------------------------------------------------------------------------------
# 7. EXPORTAÇÃO DOS ARQUIVOS FINAIS (PARQUET & RDS)
# ------------------------------------------------------------------------------
# 7.1 Cópia consolidada bruta para a pasta raw (ajuste de mês 99 -> 1)
execucao_bdpe_2 %>% 
  mutate(mes = ifelse(mes == 99, 1, mes)) %>% 
  arrow::write_parquet(file.path(DIR_EFISCO_RAW, "execucao_bdpe.parquet"), compression = "snappy")

# 7.2 Exportações em Parquet (Raiz, subpasta parquet/ e subpasta orcamento/)
arrow::write_parquet(execucao_bdpe_vf,  file.path(DIR_OUT_AURORA, "execucao_bdpe.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(DIR_OUT_AURORA, "orcamento_bdpe.parquet"), compression = "snappy")

arrow::write_parquet(execucao_bdpe_vf,  file.path(DIR_PARQUET_OUT, "execucao_bdpe.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(DIR_PARQUET_OUT, "orcamento_bdpe.parquet"), compression = "snappy")

arrow::write_parquet(execucao_bdpe_vf,  file.path(DIR_OUT_ORC, "execucao_bdpe.parquet"), compression = "snappy")
arrow::write_parquet(orcamento_bdpe_vf, file.path(DIR_OUT_ORC, "orcamento_bdpe.parquet"), compression = "snappy")

# 7.3 Backups em RDS para ferramentas legadas
readr::write_rds(execucao_bdpe_vf,  file.path(DIR_OUT_AURORA, "execucao_bdpe.rds"))
readr::write_rds(orcamento_bdpe_vf, file.path(DIR_OUT_AURORA, "orcamento_bdpe.rds"))

cli::cli_alert_success("Bases de Execução e Orçamento consolidadas salvas em Parquet e RDS em {.path {DIR_OUT_AURORA}}!")
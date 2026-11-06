# ==============================================================================
# SCRIPT: etl_vulnerabilidade_pe.R
# Objetivo: Extração, tratamento e consolidação de indicadores de vulnerabilidade
#           (CadÚnico, Abastecimento de Água e Esgotamento Sanitário) para PE.
#           Exportação local (CSV, RDS, Parquet) e sincronização no Google Drive.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. BIBLIOTECAS
# ------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)
library(arrow)
library(cli)
library(readr)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
is_linux <- Sys.info()[["sysname"]] == "Linux"
if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

# Busca hierárquica pelo centralizador paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Diretórios de saída locais
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)

# Autenticação Google via Service Account
json_auth <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(json_auth)) {
  drive_auth(path = json_auth)
  gs4_auth(path = json_auth)
  cli_alert_success("Autenticação Google realizada via {.path {json_auth}}")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {json_auth}}")
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO E TRATAMENTO
# ------------------------------------------------------------------------------

## 3.1 População Municipal (IBGE API) ----
dados_populacao_mun <- ibger::ibge_variables(6579, localities = "N6")

dados_populacao_mun_pe <- dados_populacao_mun %>% 
  mutate(
    period = as.integer(period),
    value  = as.integer(value)
  ) %>%
  filter(
    substr(as.character(locality_id), 1, 2) == "26",
    period == max(period, na.rm = TRUE)
  ) %>% 
  mutate(codigo = as.integer(locality_id))

cli_alert_success("População IBGE carregada ({nrow(dados_populacao_mun_pe)} municípios).")

## 3.2 Pessoas no Cadastro Único ----
caminho_cad_unico <- file.path(DIR_RAW_AURORA, "vulnerabilidade/cad_unico.xlsx")

if (!file.exists(caminho_cad_unico)) {
  cli_abort("Arquivo do Cadastro Único não encontrado em: {.path {caminho_cad_unico}}")
}

df_cad_pessoas <- readxl::read_xlsx(caminho_cad_unico) %>%
  clean_names() %>%
  mutate(
    codigo = as.integer(cod_ibge), 
    pessoas_cad_unico = as.numeric(pessoas)
  ) %>%
  filter(str_starts(as.character(codigo), "26")) %>%
  select(-cod_ibge, -pessoas)

df_cad_pessoas_2 <- df_cad_pessoas %>%
  left_join(
    dados_populacao_mun_pe,
    by = "codigo"
  ) %>%
  mutate(
    tx_pessoas_cad_unico = round(pessoas_cad_unico / value, 4), 
    unidade_territorial  = toupper(municipio),
    uf                   = "PE"
  ) %>%
  select(
    codigo, 
    unidade_territorial, 
    uf, 
    referencia, 
    pessoas_cad_unico, 
    tx_pessoas_cad_unico
  )

cli_alert_success("Indicador de pessoas no CadÚnico consolidado ({nrow(df_cad_pessoas_2)} registros).")

## 3.3 Abastecimento de Água ----
caminho_agua <- file.path(DIR_RAW_AURORA, "vulnerabilidade/cad_agua_rede.csv")

if (!file.exists(caminho_agua)) {
  cli_abort("Arquivo de abastecimento de água não encontrado em: {.path {caminho_agua}}")
}

df_agua_rede <- fread(caminho_agua, encoding = "Latin-1") %>% 
  clean_names() %>%
  rename(
    agua_distribuicao = quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_rede_geral_de_distribuicao,
    agua_cisterna     = quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_cisterna,
    agua_poco         = quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_poco_ou_nascente,
    agua_outros       = quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_outras_formas,
    agua_NA           = quantidade_de_familias_cadastradas_em_domicilios_sem_informacao_sobre_a_forma_de_abastecimento_de_agua
  ) %>%
  mutate(
    total_domicilios_agua = agua_distribuicao + agua_poco + agua_cisterna + agua_outros + agua_NA,
    pct_agua_distribuicao = round((agua_distribuicao / total_domicilios_agua) * 100, 2)
  ) %>%
  filter(uf == "PE")

cli_alert_success("Indicador de abastecimento de água consolidado ({nrow(df_agua_rede)} registros).")

## 3.4 Esgotamento Sanitário ----
caminho_esgoto <- file.path(DIR_RAW_AURORA, "vulnerabilidade/cad_agua_esgoto.csv")

if (!file.exists(caminho_esgoto)) {
  cli_abort("Arquivo de esgotamento sanitário não encontrado em: {.path {caminho_esgoto}}")
}

df_agua_esgoto <- fread(caminho_esgoto, encoding = "Latin-1") %>% 
  clean_names() %>%
  rename(
    esgoto_distribuicao     = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_rede_coletora_de_esgoto_ou_pluvial,
    esgoto_fossa_septica    = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_fossa_septica,
    esgoto_fossa_rudimentar = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_fossa_rudimentar,
    esgoto_vala             = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_vala_a_ceu_aberto,
    esgoto_rio_lago_mar     = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_direto_para_rio_lago_ou_mar,
    esgoto_outros           = quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_outras_formas,
    esgoto_NA               = quantidade_de_familias_cadastradas_em_domicilios_sem_informacao_sobre_a_forma_de_escoamento_sanitario
  ) %>%
  mutate(
    total_domicilios_esgoto = esgoto_distribuicao + esgoto_fossa_septica + esgoto_fossa_rudimentar + esgoto_vala + esgoto_rio_lago_mar + esgoto_outros + esgoto_NA,
    pct_esgoto_rede         = round((esgoto_distribuicao / total_domicilios_esgoto) * 100, 2)
  ) %>%
  filter(uf == "PE")

cli_alert_success("Indicador de esgotamento sanitário consolidado ({nrow(df_agua_esgoto)} registros).")

# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO LOCAL (CSV, RDS, PARQUET)
# ------------------------------------------------------------------------------
# Definição dos caminhos locais
path_local_cad_csv     <- file.path(DIR_OUT_AURORA, "indicador_cad_pessoas_pe.csv")
path_local_cad_rds     <- file.path(DIR_OUT_AURORA, "indicador_cad_pessoas_pe.rds")
path_local_cad_parquet <- file.path(dir_parquet_out, "indicador_cad_pessoas_pe.parquet")

path_local_agua_csv     <- file.path(DIR_OUT_AURORA, "indicador_agua_pe.csv")
path_local_agua_rds     <- file.path(DIR_OUT_AURORA, "indicador_agua_pe.rds")
path_local_agua_parquet <- file.path(dir_parquet_out, "indicador_agua_pe.parquet")

path_local_esgoto_csv     <- file.path(DIR_OUT_AURORA, "indicador_esgoto_pe.csv")
path_local_esgoto_rds     <- file.path(DIR_OUT_AURORA, "indicador_esgoto_pe.rds")
path_local_esgoto_parquet <- file.path(dir_parquet_out, "indicador_esgoto_pe.parquet")

# Exportações Locais
# CadÚnico
write.csv(df_cad_pessoas_2, path_local_cad_csv, row.names = FALSE)
readr::write_rds(df_cad_pessoas_2, path_local_cad_rds)
arrow::write_parquet(df_cad_pessoas_2, path_local_cad_parquet)

# Água
write.csv(df_agua_rede, path_local_agua_csv, row.names = FALSE)
readr::write_rds(df_agua_rede, path_local_agua_rds)
arrow::write_parquet(df_agua_rede, path_local_agua_parquet)

# Esgoto
write.csv(df_agua_esgoto, path_local_esgoto_csv, row.names = FALSE)
readr::write_rds(df_agua_esgoto, path_local_esgoto_rds)
arrow::write_parquet(df_agua_esgoto, path_local_esgoto_parquet)

cli_alert_success("Arquivos locais (CSV, RDS, Parquet) salvos em {.path {DIR_OUT_AURORA}}.")

# ------------------------------------------------------------------------------
# 5. SINCRONIZAÇÃO NO GOOGLE DRIVE
# ------------------------------------------------------------------------------
id_pasta_drive <- googledrive::as_id("11SbyKS9ZFN5mCDpIKorUmWEePIaMlQA-")

upload_seguro <- function(caminho_arquivo, nome_remoto, id_pasta) {
  if (file.exists(caminho_arquivo)) {
    tryCatch({
      drive_upload(
        media     = caminho_arquivo,
        path      = id_pasta,
        name      = nome_remoto,
        overwrite = TRUE
      )
      cli_alert_success("Arquivo {.file {nome_remoto}} enviado ao Google Drive com sucesso!")
    }, error = function(e) {
      cli_alert_danger("Erro ao subir {.file {nome_remoto}}: {.val {e$message}}")
    })
  } else {
    cli_alert_danger("Arquivo para upload não encontrado em: {.path {caminho_arquivo}}")
  }
}

# Uploads dos CSVs para a pasta indicada
upload_seguro(path_local_cad_csv,    "indicador_cad_pessoas_pe.csv", id_pasta_drive)
upload_seguro(path_local_agua_csv,   "indicador_agua_pe.csv",        id_pasta_drive)
upload_seguro(path_local_esgoto_csv, "indicador_esgoto_pe.csv",      id_pasta_drive)

# Uploads dos Parquets correspondentes
upload_seguro(path_local_cad_parquet,    "indicador_cad_pessoas_pe.parquet", id_pasta_drive)
upload_seguro(path_local_agua_parquet,   "indicador_agua_pe.parquet",        id_pasta_drive)
upload_seguro(path_local_esgoto_parquet, "indicador_esgoto_pe.parquet",      id_pasta_drive)

cli_alert_success("Processo de ETL e sincronização finalizado com sucesso!")
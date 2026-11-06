# ==============================================================================
# SCRIPT: PROCESSAMENTO DE INDICADORES DE SEGURANÇA PÚBLICA (MVI / CVP / VCM)
# Objetivo: Download de dados brutos de segurança via Google Drive, consolidação
#           municipal e estadual, exportação local (RDS/XLSX/Parquet) e Google Sheets.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(stringr)
library(googledrive)
library(googlesheets4)
library(glue)
library(httr)
library(qs2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(cli)
library(arrow)
library(readr)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
options(scipen = 999)

# Busca hierárquica pelo paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# ------------------------------------------------------------------------------
# 3. AUTENTICAÇÃO GOOGLE
# ------------------------------------------------------------------------------
AUTH_PATH <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(AUTH_PATH)) {
  gs4_auth(path = AUTH_PATH, scopes = "https://www.googleapis.com/auth/spreadsheets")
  drive_auth(path = AUTH_PATH)
  cli_alert_success("Autenticação Google realizada com sucesso via {.path {AUTH_PATH}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {AUTH_PATH}}")
}

# ------------------------------------------------------------------------------
# 4. DOWNLOAD DAS BASES BRUTAS VIA GOOGLE DRIVE
# ------------------------------------------------------------------------------
id_trad     <- "1n6mENgDHVkldzdX3VRCi3Yn3eRahnyk6-mqnL1dp6vU"
id_cvli     <- "1m2KKyU7ORe4dmVWrKXd4YE3gljAlE8em"
id_cvp      <- "1LY3SuMQeoe7-apRovE5Qt4eJ5NAzHN84"
id_rfv      <- "1nR12-gI83J1cV99HS0uck3dEjbfQbuzO"
id_tent_mvi <- "1tIhZQ6IjDS_PPP6Eu1wh2E5q1zDS_guL"
id_vcm      <- "1oEeICivsTYV_nl-mUH9A_oyKZoG4lnR5"

drive_download(as_id(id_trad),     path = file.path(DIR_RAW_AURORA, "traducao.xlsx"),     overwrite = TRUE)
drive_download(as_id(id_cvli),     path = file.path(DIR_RAW_AURORA, "ppv_cvli.qs2"),      overwrite = TRUE)
drive_download(as_id(id_cvp),      path = file.path(DIR_RAW_AURORA, "cvp_df.qs2"),        overwrite = TRUE)
drive_download(as_id(id_rfv),      path = file.path(DIR_RAW_AURORA, "rfv_tbl.qs2"),        overwrite = TRUE)
drive_download(as_id(id_tent_mvi), path = file.path(DIR_RAW_AURORA, "ppv_tent_cvli.qs2"), overwrite = TRUE)
drive_download(as_id(id_vcm),      path = file.path(DIR_RAW_AURORA, "vcm_tbl.qs2"),        overwrite = TRUE)

cli_alert_success("Bases brutas de segurança baixadas com sucesso em {.path {DIR_RAW_AURORA}}.")

# ------------------------------------------------------------------------------
# 5. CARREGAMENTO E PROCESSAMENTO DOS DADOS
# ------------------------------------------------------------------------------
traducao_ibge <- readxl::read_xlsx(file.path(DIR_RAW_AURORA, "traducao.xlsx"))

# Extração do mês de referência máximo
data_maxima <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2")) %>% 
  select(ppv_data) %>% 
  filter(ppv_data == max(ppv_data)) %>% 
  mutate(
    mes = c(
      "Janeiro", "Fevereiro", "Março", "Abril",
      "Maio", "Junho", "Julho", "Agosto",
      "Setembro", "Outubro", "Novembro", "Dezembro"
    )[lubridate::month(ppv_data)]
  ) %>% 
  select(mes) %>% 
  distinct() %>% 
  pull(1)

map_natureza <- tibble::tibble(
  ppv_violencia_natureza = c(
    "ESTUPRO DE VULNERAVEL POR VIOLENCIA DOMESTICA/FAMILIAR", 
    "ESTUPRO POR VIOLENCIA DOMESTICA/FAMILIAR", 
    "ESTUPRO DE VULNERAVEL", 
    "ESTUPRO", 
    "ESTUPRO COLETIVO", 
    "ESTUPRO CORRETIVO", 
    "LESAO CORPORAL", 
    "LESAO CORPORAL POR VIOLENCIA DOMESTICA/FAMILIAR", 
    "AMEACA POR VIOLENCIA DOMESTICA/FAMILIAR", 
    "AMEACA"
  ), 
  tipo = c(rep("vs", 6), rep("lesao", 2), rep("ameaca", 2))
)

# VCM (Violência Contra a Mulher)
vcm_tbl <- qs2::qs_read(file.path(DIR_RAW_AURORA, "vcm_tbl.qs2")) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  select(codibge, ppv_municipio, VCM, ano, ppv_violencia_natureza) %>% 
  filter(ano >= 2015) %>% 
  left_join(map_natureza, by = "ppv_violencia_natureza") %>% 
  filter(!is.na(tipo)) %>% 
  group_by(ano, codibge, ppv_municipio, tipo) %>% 
  summarise(vcm = sum(VCM, na.rm = FALSE), .groups = "drop") %>% 
  pivot_wider(
    names_from = tipo,
    values_from = vcm,
    names_prefix = "vcm_"
  )

# Tentativa de CVLI
tentativa_cvli <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_tent_cvli.qs2")) %>% 
  mutate(
    ppv_municipio = case_when(
      ppv_municipio == "BELEM DE SAO FRANCISCO" ~ "BELEM DO SAO FRANCISCO",
      ppv_municipio == "IGUARACI"                ~ "IGUARACY",
      ppv_municipio == "ITAMARACA"               ~ "ILHA DE ITAMARACA",
      ppv_municipio == "LAGOA DO ITAENGA"        ~ "LAGOA DE ITAENGA",
      ppv_municipio == "SAO CAETANO"             ~ "SAO CAITANO",
      TRUE                                      ~ ppv_municipio
    )
  ) %>% 
  filter(ano >= 2015) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  select(codibge, ppv_municipio, ppv_oc_tent, ano) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(
    tent_mvi = sum(ppv_oc_tent, na.rm = TRUE), 
    .groups = "drop"
  )

# PPV / CVLI
ppv_cvli <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2")) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  select(codibge, ppv_municipio, ppv_oc_cvli, ano) %>% 
  filter(ano >= 2015) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(mvi = sum(ppv_oc_cvli, na.rm = FALSE), .groups = "drop")

# Feminicídio
feminicidio <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2")) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  filter(ppv_motivacao_geral == "FEMINICIDIO") %>% 
  select(codibge, ppv_municipio, ppv_oc_cvli, ano) %>% 
  filter(ano >= 2015) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(feminicidio = sum(ppv_oc_cvli, na.rm = FALSE), .groups = "drop")

# CVP (Crimes Violentos contra o Patrimônio)
ppv_cvp <- qs2::qs_read(file.path(DIR_RAW_AURORA, "cvp_df.qs2")) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  select(codibge, ppv_municipio, ppv_oc_cvp, ano) %>% 
  filter(ano >= 2015) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(cvp = sum(ppv_oc_cvp, na.rm = FALSE), .groups = "drop")

# RFV (Roubo e Furto de Veículos)
ppv_rfv <- qs2::qs_read(file.path(DIR_RAW_AURORA, "rfv_tbl.qs2")) %>% 
  full_join(traducao_ibge, by = "ppv_municipio") %>% 
  select(codibge, ppv_municipio, RFV, ano) %>% 
  filter(ano >= 2015) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(rfv = sum(RFV, na.rm = FALSE), .groups = "drop")

# ------------------------------------------------------------------------------
# 6. CONSOLIDAÇÃO MUNICIPAL E ESTADUAL
# ------------------------------------------------------------------------------
df_mvi_cvp <- full_join(ppv_cvli, ppv_cvp, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(feminicidio, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(ppv_rfv, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(tentativa_cvli, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(vcm_tbl, by = c("codibge", "ano", "ppv_municipio")) %>% 
  filter(!is.na(codibge)) %>% 
  mutate(
    mes_referencia = data_maxima,
    codibge        = as.character(codibge)
  )

# Dataframe sumarizado para Pernambuco (total do estado por ano)
df_pe <- df_mvi_cvp %>% 
  group_by(ano, mes_referencia) %>% 
  summarise(
    mvi         = sum(mvi, na.rm = TRUE),
    cvp         = sum(cvp, na.rm = TRUE),
    feminicidio = sum(feminicidio, na.rm = TRUE),
    rfv         = sum(rfv, na.rm = TRUE),
    tent_mvi    = sum(tent_mvi, na.rm = TRUE),
    vcm_vs      = sum(vcm_vs, na.rm = TRUE),
    vcm_lesao   = sum(vcm_lesao, na.rm = TRUE),
    vcm_ameaca  = sum(vcm_ameaca, na.rm = TRUE),
    .groups     = "drop"
  ) %>% 
  mutate(estado = "Pernambuco") %>% 
  select(estado, ano, mes_referencia, everything())

# ------------------------------------------------------------------------------
# 7. EXPORTAÇÕES LOCAIS E GOOGLE SHEETS
# ------------------------------------------------------------------------------
# Criação de diretórios estruturados (incluindo subpastas parquet)
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir_parquet_raw <- file.path(DIR_RAW_AURORA, "parquet")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_RAW_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_raw, recursive = TRUE, showWarnings = FALSE)

# --- 7.1 Exportação Excel (Multiabas) ---
caminho_out_xlsx <- file.path(DIR_OUT_AURORA, "df_mvi_cvp.xlsx")
caminho_raw_xlsx <- file.path(DIR_RAW_AURORA, "df_mvi_cvp.xlsx")

lista_exportacao <- list("Municipios" = df_mvi_cvp, "Pernambuco" = df_pe)

writexl::write_xlsx(lista_exportacao, caminho_out_xlsx)
writexl::write_xlsx(lista_exportacao, caminho_raw_xlsx)
cli_alert_success("Arquivos Excel gravados com sucesso em {.path {caminho_out_xlsx}}.")

# --- 7.2 Exportação RDS ---
readr::write_rds(df_mvi_cvp, file.path(DIR_OUT_AURORA, "df_mvi_cvp_municipios.rds"))
readr::write_rds(df_pe,      file.path(DIR_OUT_AURORA, "df_mvi_cvp_pe.rds"))
readr::write_rds(df_mvi_cvp, file.path(DIR_RAW_AURORA, "df_mvi_cvp_municipios.rds"))
readr::write_rds(df_pe,      file.path(DIR_RAW_AURORA, "df_mvi_cvp_pe.rds"))
cli_alert_success("Arquivos RDS gravados com sucesso.")

# --- 7.3 Exportação Parquet ---
arrow::write_parquet(df_mvi_cvp, file.path(dir_parquet_out, "df_mvi_cvp_municipios.parquet"))
arrow::write_parquet(df_pe,      file.path(dir_parquet_out, "df_mvi_cvp_pe.parquet"))
arrow::write_parquet(df_mvi_cvp, file.path(dir_parquet_raw, "df_mvi_cvp_municipios.parquet"))
arrow::write_parquet(df_pe,      file.path(dir_parquet_raw, "df_mvi_cvp_pe.parquet"))
cli_alert_success("Arquivos Parquet gravados com sucesso em {.path {dir_parquet_out}}.")

# --- 7.4 Sincronização Google Sheets ---
id_planilha <- "1yPbzgctnAvE9p_Vuj_-EbepV_FsTkMQ3H0BNAQfdMtE"

tryCatch({
  sheet_write(
    data  = df_mvi_cvp,
    ss    = id_planilha,
    sheet = "Municipios"
  )
  sheet_write(
    data  = df_pe,
    ss    = id_planilha,
    sheet = "Pernambuco"
  )
  cli_alert_success("Planilha Google Sheets atualizada com sucesso nas abas 'Municipios' e 'Pernambuco'!")
}, error = function(e) {
  cli_alert_danger("Falha ao gravar no Google Sheets: {.val {e$message}}")
})
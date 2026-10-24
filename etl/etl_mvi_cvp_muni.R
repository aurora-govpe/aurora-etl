library(stringr)
library(googledrive)
library(googlesheets4)
library(glue)
library(httr)
library(qs2)
library(dplyr)
library(tidyr)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

## config ----
AUTH_PATH <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"

gs4_auth(
  path = AUTH_PATH,
  scopes = "https://www.googleapis.com/auth/spreadsheets"
)

drive_auth(
  path = AUTH_PATH)

id_trad <- "1n6mENgDHVkldzdX3VRCi3Yn3eRahnyk6-mqnL1dp6vU"
id_cvli <- "1m2KKyU7ORe4dmVWrKXd4YE3gljAlE8em"
id_cvp <- "1LY3SuMQeoe7-apRovE5Qt4eJ5NAzHN84"
id_rfv <- "1nR12-gI83J1cV99HS0uck3dEjbfQbuzO"
id_tent_mvi <- "1tIhZQ6IjDS_PPP6Eu1wh2E5q1zDS_guL"
id_vcm <- "1oEeICivsTYV_nl-mUH9A_oyKZoG4lnR5"

drive_download(
  as_id(id_trad),
  path = file.path(DIR_RAW_AURORA, "traducao.xlsx",
  overwrite = TRUE
)

drive_download(
  as_id(id_cvli),
  path = file.path(DIR_RAW_AURORA, "ppv_cvli.qs2",
  overwrite = TRUE
)

drive_download(
  as_id(id_cvp),
  path = file.path(DIR_RAW_AURORA, "cvp_df.qs2",
  overwrite = TRUE
)

drive_download(
  as_id(id_rfv),
  path = file.path(DIR_RAW_AURORA, "rfv_tbl.qs2",
  overwrite = TRUE
)

drive_download(
  as_id(id_tent_mvi),
  path = file.path(DIR_RAW_AURORA, "ppv_tent_cvli.qs2",
  overwrite = TRUE
)

drive_download(
  as_id(id_vcm),
  path = file.path(DIR_RAW_AURORA, "vcm_tbl.qs2",
  overwrite = TRUE
)


traducao_ibge <- readxl::read_xlsx(file.path(DIR_RAW_AURORA, "traducao.xlsx")

data_maxima <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2") %>% 
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
  ppv_violencia_natureza = c("ESTUPRO DE VULNERAVEL POR VIOLENCIA DOMESTICA/FAMILIAR", "ESTUPRO POR VIOLENCIA DOMESTICA/FAMILIAR", "ESTUPRO DE VULNERAVEL", "ESTUPRO", "ESTUPRO COLETIVO", "ESTUPRO CORRETIVO", "LESAO CORPORAL", "LESAO CORPORAL POR VIOLENCIA DOMESTICA/FAMILIAR", "AMEACA POR VIOLENCIA DOMESTICA/FAMILIAR", "AMEACA" ), 
  tipo = c(
    rep("vs", 6), rep("lesao", 2), rep("ameaca", 2)))


vcm_tbl <- qs2::qs_read(file.path(DIR_RAW_AURORA, "vcm_tbl.qs2") %>% 
  full_join(traducao_ibge) %>% 
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

tentativa_cvli <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_tent_cvli.qs2") %>% 
  mutate(
    ppv_municipio = case_when(
      ppv_municipio == "BELEM DE SAO FRANCISCO" ~ "BELEM DO SAO FRANCISCO",
      ppv_municipio == "IGUARACI"               ~ "IGUARACY",
      ppv_municipio == "ITAMARACA"               ~ "ILHA DE ITAMARACA",
      ppv_municipio == "LAGOA DO ITAENGA"        ~ "LAGOA DE ITAENGA",
      ppv_municipio == "SAO CAETANO"             ~ "SAO CAITANO",
      TRUE                                       ~ ppv_municipio
    )
  ) %>% 
  filter(ano >= 2015) %>% 
  full_join(traducao_ibge) %>% 
  select(codibge, ppv_municipio, ppv_oc_tent, ano) %>% 
  group_by(ano, codibge, ppv_municipio) %>% 
  summarise(
    tent_mvi = sum(ppv_oc_tent, na.rm = TRUE), 
    .groups = "drop"
  )


ppv_cvli <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2") %>% 
  full_join(traducao_ibge) %>% 
  select(codibge, ppv_municipio, ppv_oc_cvli, ano)  %>% 
  filter(ano >= 2015) %>% 
  group_by(ano,codibge, ppv_municipio ) %>% 
  summarise(mvi = sum(ppv_oc_cvli, na.rm = F))

feminicidio <- qs2::qs_read(file.path(DIR_RAW_AURORA, "ppv_cvli.qs2") %>% 
  full_join(traducao_ibge) %>% 
  filter(ppv_motivacao_geral == "FEMINICIDIO") %>% 
  select(codibge, ppv_municipio, ppv_oc_cvli, ano)  %>% 
  filter(ano >= 2015) %>% 
  group_by(ano,codibge, ppv_municipio ) %>% 
  summarise(feminicidio = sum(ppv_oc_cvli, na.rm = F))


ppv_cvp <- qs2::qs_read(file.path(DIR_RAW_AURORA, "cvp_df.qs2") %>% 
  full_join(traducao_ibge) %>% 
  select(codibge, ppv_municipio, ppv_oc_cvp, ano)  %>% 
  filter(ano >= 2015) %>% 
  group_by(ano,codibge, ppv_municipio )  %>% 
  summarise(cvp = sum(ppv_oc_cvp, na.rm = F))

ppv_rfv <- qs2::qs_read(file.path(DIR_RAW_AURORA, "rfv_tbl.qs2") %>% 
  full_join(traducao_ibge) %>% 
  select(codibge, ppv_municipio, RFV, ano)  %>% 
  filter(ano >= 2015) %>% 
  group_by(ano,codibge, ppv_municipio )  %>% 
  summarise(rfv = sum(RFV, na.rm = F))

df_mvi_cvp <- full_join(ppv_cvli, ppv_cvp, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(feminicidio, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(ppv_rfv, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(tentativa_cvli, by = c("codibge", "ano", "ppv_municipio")) %>% 
  full_join(vcm_tbl, by = c("codibge", "ano", "ppv_municipio")) %>% 
  filter(!is.na(codibge)) %>% 
  mutate(mes_referencia = data_maxima) %>% 
  mutate(codibge = as.character(codibge))

writexl::write_xlsx(df_mvi_cvp, file.path(DIR_RAW_AURORA, "df_mvi_cvp.xlsx")

# id_planilha <- "1yPbzgctnAvE9p_Vuj_-EbepV_FsTkMQ3H0BNAQfdMtE"
# 
# # 5. Sobrescreve o conteúdo no Google Sheets mantendo o mesmo ID
# sheet_write(
#   data = df_mvi_cvp,
#   ss = id_planilha,
#   sheet = 1 # Sobrescreve a primeira aba (ou informe o nome exato: "Página1")
# )


# Criar o dataframe sumarizado para Pernambuco (total do estado por ano)
df_pe <- df_mvi_cvp %>% 
  group_by(ano, mes_referencia) %>% 
  summarise(
    mvi = sum(mvi, na.rm = TRUE),
    cvp = sum(cvp, na.rm = TRUE),
    feminicidio = sum(feminicidio, na.rm = TRUE),
    rfv = sum(rfv, na.rm = TRUE),
    tent_mvi = sum(tent_mvi, na.rm = TRUE),
    vcm_vs = sum(vcm_vs, na.rm = TRUE),
    vcm_lesao = sum(vcm_lesao, na.rm = TRUE),
    vcm_ameaca = sum(vcm_ameaca, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(estado = "Pernambuco") %>% 
  select(estado, ano, mes_referencia, everything()) # Organiza as colunas

# Salvando localmente em Excel com múltiplas abas no destino central e raw
writexl::write_xlsx(
  list("Municipios" = df_mvi_cvp, "Pernambuco" = df_pe), 
  file.path(DIR_OUT_AURORA, "df_mvi_cvp.xlsx")
)
writexl::write_xlsx(
  list("Municipios" = df_mvi_cvp, "Pernambuco" = df_pe), 
  file.path(DIR_RAW_AURORA, "df_mvi_cvp.xlsx")
)

id_planilha <- "1yPbzgctnAvE9p_Vuj_-EbepV_FsTkMQ3H0BNAQfdMtE"

# 5. Sobrescreve o conteúdo no Google Sheets mantendo o mesmo ID

# Escreve a primeira aba com os dados dos municípios
sheet_write(
  data = df_mvi_cvp,
  ss = id_planilha,
  sheet = "Municipios" # Nomeia a aba como "Municipios"
)

# Cria/Sobrescreve a segunda aba com os dados agregados de Pernambuco
sheet_write(
  data = df_pe,
  ss = id_planilha,
  sheet = "Pernambuco" # Nomeia a aba como "Pernambuco"
)

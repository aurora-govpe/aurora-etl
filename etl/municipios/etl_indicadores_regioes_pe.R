# ==============================================================================
# SCRIPT: etl_indicadores_rd_pe.R
# Objetivo: Extração, tratamento e consolidação multi-fonte de indicadores
#           estritamente agregáveis por Região de Desenvolvimento (RD) de PE.
#           Aplica soma regional para volumes/estoques, recomposição exata de
#           taxas/razões e ingestão direta de indicadores pré-calculados por RD.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(googledrive)
library(readxl)
library(janitor)
library(arrow)
library(cli)
library(readr)
library(jsonlite)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
is_linux <- Sys.info()[["sysname"]] == "Linux"
if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

dir_parquet_out     <- file.path(DIR_OUT_AURORA, "parquet")
dir_raw_ibge        <- file.path(DIR_RAW_AURORA, "ibge")
dir_raw_indicadores <- file.path(DIR_RAW_AURORA, "indicadores")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_ibge, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_indicadores, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. DICIONÁRIO TERRITORIAL: MUNICÍPIOS (185) -> REGIÕES DE DESENVOLVIMENTO (12)
# ------------------------------------------------------------------------------
df_depara_rd <- tribble(
  ~cod_municipio, ~cod_rd, ~nm_rd,
  # 01 - Metropolitana (RMR - 15 municípios incluindo Fernando de Noronha)
  "2600054", "01", "Metropolitana", "2601052", "01", "Metropolitana",
  "2602902", "01", "Metropolitana", "2603454", "01", "Metropolitana",
  "2605459", "01", "Metropolitana", "2606804", "01", "Metropolitana",
  "2607208", "01", "Metropolitana", "2607604", "01", "Metropolitana",
  "2607752", "01", "Metropolitana", "2607901", "01", "Metropolitana",
  "2609402", "01", "Metropolitana", "2609600", "01", "Metropolitana",
  "2610707", "01", "Metropolitana", "2611606", "01", "Metropolitana",
  "2613701", "01", "Metropolitana",
  # 02 - Mata Norte (19 municípios)
  "2600302", "02", "Mata Norte", "2602704", "02", "Mata Norte",
  "2603009", "02", "Mata Norte", "2603603", "02", "Mata Norte",
  "2604007", "02", "Mata Norte", "2604403", "02", "Mata Norte",
  "2605301", "02", "Mata Norte", "2605905", "02", "Mata Norte",
  "2606101", "02", "Mata Norte", "2607653", "02", "Mata Norte",
  "2607802", "02", "Mata Norte", "2608503", "02", "Mata Norte",
  "2609006", "02", "Mata Norte", "2609501", "02", "Mata Norte",
  "2610509", "02", "Mata Norte", "2610608", "02", "Mata Norte",
  "2615201", "02", "Mata Norte", "2615409", "02", "Mata Norte",
  "2616308", "02", "Mata Norte",
  # 03 - Mata Sul (24 municípios)
  "2600401", "03", "Mata Sul", "2600807", "03", "Mata Sul",
  "2601409", "03", "Mata Sul", "2601508", "03", "Mata Sul",
  "2601805", "03", "Mata Sul", "2602209", "03", "Mata Sul",
  "2604205", "03", "Mata Sul", "2604908", "03", "Mata Sul",
  "2605202", "03", "Mata Sul", "2605509", "03", "Mata Sul",
  "2606309", "03", "Mata Sul", "2607950", "03", "Mata Sul",
  "2608255", "03", "Mata Sul", "2609154", "03", "Mata Sul",
  "2610004", "03", "Mata Sul", "2611408", "03", "Mata Sul",
  "2611507", "03", "Mata Sul", "2611705", "03", "Mata Sul",
  "2611804", "03", "Mata Sul", "2612604", "03", "Mata Sul",
  "2613305", "03", "Mata Sul", "2614204", "03", "Mata Sul",
  "2614857", "03", "Mata Sul", "2616506", "03", "Mata Sul",
  # 04 - Agreste Setentrional (19 municípios)
  "2602506", "04", "Agreste Setentrional", "2603801", "04", "Agreste Setentrional",
  "2604452", "04", "Agreste Setentrional", "2604809", "04", "Agreste Setentrional",
  "2605004", "04", "Agreste Setentrional", "2605806", "04", "Agreste Setentrional",
  "2608008", "04", "Agreste Setentrional", "2608909", "04", "Agreste Setentrional",
  "2609105", "04", "Agreste Setentrional", "2609808", "04", "Agreste Setentrional",
  "2610103", "04", "Agreste Setentrional", "2612307", "04", "Agreste Setentrional",
  "2612703", "04", "Agreste Setentrional", "2612802", "04", "Agreste Setentrional",
  "2613107", "04", "Agreste Setentrional", "2614501", "04", "Agreste Setentrional",
  "2614709", "04", "Agreste Setentrional", "2615003", "04", "Agreste Setentrional",
  "2616209", "04", "Agreste Setentrional",
  # 05 - Agreste Central (26 municípios)
  "2600203", "05", "Agreste Central", "2600609", "05", "Agreste Central",
  "2601300", "05", "Agreste Central", "2601706", "05", "Agreste Central",
  "2601904", "05", "Agreste Central", "2602100", "05", "Agreste Central",
  "2602407", "05", "Agreste Central", "2602605", "05", "Agreste Central",
  "2603207", "05", "Agreste Central", "2603504", "05", "Agreste Central",
  "2604106", "05", "Agreste Central", "2605103", "05", "Agreste Central",
  "2606408", "05", "Agreste Central", "2607000", "05", "Agreste Central",
  "2607505", "05", "Agreste Central", "2608057", "05", "Agreste Central",
  "2610905", "05", "Agreste Central", "2611309", "05", "Agreste Central",
  "2611903", "05", "Agreste Central", "2612000", "05", "Agreste Central",
  "2612406", "05", "Agreste Central", "2612455", "05", "Agreste Central",
  "2613008", "05", "Agreste Central", "2613503", "05", "Agreste Central",
  "2614600", "05", "Agreste Central", "2615102", "05", "Agreste Central",
  # 06 - Agreste Meridional (26 municípios)
  "2600500", "06", "Agreste Meridional", "2600906", "06", "Agreste Meridional",
  "2602308", "06", "Agreste Meridional", "2602803", "06", "Agreste Meridional",
  "2603108", "06", "Agreste Meridional", "2603306", "06", "Agreste Meridional",
  "2603405", "06", "Agreste Meridional", "2603702", "06", "Agreste Meridional",
  "2604700", "06", "Agreste Meridional", "2606002", "06", "Agreste Meridional",
  "2606903", "06", "Agreste Meridional", "2607109", "06", "Agreste Meridional",
  "2608206", "06", "Agreste Meridional", "2608305", "06", "Agreste Meridional",
  "2608404", "06", "Agreste Meridional", "2608453", "06", "Agreste Meridional",
  "2608602", "06", "Agreste Meridional", "2610301", "06", "Agreste Meridional",
  "2610400", "06", "Agreste Meridional", "2610806", "06", "Agreste Meridional",
  "2611002", "06", "Agreste Meridional", "2612109", "06", "Agreste Meridional",
  "2612471", "06", "Agreste Meridional", "2613206", "06", "Agreste Meridional",
  "2615508", "06", "Agreste Meridional", "2616100", "06", "Agreste Meridional",
  # 07 - Sertão do Moxotó (7 municípios)
  "2601201", "07", "Sertão do Moxotó", "2602001", "07", "Sertão do Moxotó",
  "2604601", "07", "Sertão do Moxotó", "2606606", "07", "Sertão do Moxotó",
  "2607406", "07", "Sertão do Moxotó", "2609303", "07", "Sertão do Moxotó",
  "2613602", "07", "Sertão do Moxotó",
  # 08 - Sertão do Pajeú (17 municípios)
  "2600104", "08", "Sertão do Pajeú", "2602951", "08", "Sertão do Pajeú",
  "2603157", "08", "Sertão do Pajeú", "2603900", "08", "Sertão do Pajeú",
  "2605152", "08", "Sertão do Pajeú", "2605608", "08", "Sertão do Pajeú",
  "2606507", "08", "Sertão do Pajeú", "2607307", "08", "Sertão do Pajeú",
  "2607703", "08", "Sertão do Pajeú", "2611533", "08", "Sertão do Pajeú",
  "2612554", "08", "Sertão do Pajeú", "2612901", "08", "Sertão do Pajeú",
  "2613404", "08", "Sertão do Pajeú", "2613800", "08", "Sertão do Pajeú",
  "2613909", "08", "Sertão do Pajeú", "2614659", "08", "Sertão do Pajeú",
  "2615706", "08", "Sertão do Pajeú",
  # 09 - Sertão de Itaparica (7 municípios)
  "2601607", "09", "Sertão de Itaparica", "2603926", "09", "Sertão de Itaparica",
  "2605707", "09", "Sertão de Itaparica", "2608024", "09", "Sertão de Itaparica",
  "2608107", "09", "Sertão de Itaparica", "2611051", "09", "Sertão de Itaparica",
  "2614402", "09", "Sertão de Itaparica",
  # 10 - Sertão Central (8 municípios)
  "2604304", "10", "Sertão Central", "2609204", "10", "Sertão Central",
  "2609709", "10", "Sertão Central", "2612208", "10", "Sertão Central",
  "2613354", "10", "Sertão Central", "2614105", "10", "Sertão Central",
  "2615300", "10", "Sertão Central", "2616001", "10", "Sertão Central",
  # 11 - Sertão do Araripe (10 municípios)
  "2601102", "11", "Sertão do Araripe", "2602050", "11", "Sertão do Araripe",
  "2605350", "11", "Sertão do Araripe", "2606200", "11", "Sertão do Araripe",
  "2607158", "11", "Sertão do Araripe", "2609451", "11", "Sertão do Araripe",
  "2609907", "11", "Sertão do Araripe", "2612505", "11", "Sertão do Araripe",
  "2613156", "11", "Sertão do Araripe", "2615607", "11", "Sertão do Araripe",
  # 12 - Sertão do São Francisco (7 municípios)
  "2600708", "12", "Sertão do São Francisco", "2603058", "12", "Sertão do São Francisco",
  "2604502", "12", "Sertão do São Francisco", "2608750", "12", "Sertão do São Francisco",
  "2609857", "12", "Sertão do São Francisco", "2611101", "12", "Sertão do São Francisco",
  "2612653", "12", "Sertão do São Francisco"
) %>%
  mutate(ibge6 = substr(str_trim(cod_municipio), 1, 6))

# Lookup único das 12 RDs (para bases que já vêm agregadas por RD)
df_rds_ref <- df_depara_rd %>%
  distinct(cod_rd, nm_rd)

# Função auxiliar para normalizar nomes/códigos de RD vindos de planilhas externas
padronizar_rd_externa <- function(x) {
  x_clean <- str_trim(as.character(x)) %>%
    str_to_lower() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  case_when(
    str_detect(x_clean, "^0?1\\b|metropolitana|\\brmr\\b")       ~ "01",
    str_detect(x_clean, "^0?2\\b|mata norte")                    ~ "02",
    str_detect(x_clean, "^0?3\\b|mata sul")                      ~ "03",
    str_detect(x_clean, "^0?4\\b|agreste setentrional")          ~ "04",
    str_detect(x_clean, "^0?5\\b|agreste central")               ~ "05",
    str_detect(x_clean, "^0?6\\b|agreste meridional")            ~ "06",
    str_detect(x_clean, "^0?7\\b|moxoto")                        ~ "07",
    str_detect(x_clean, "^0?8\\b|pajeu")                         ~ "08",
    str_detect(x_clean, "^0?9\\b|itaparica")                     ~ "09",
    str_detect(x_clean, "^10\\b|sertao central")                 ~ "10",
    str_detect(x_clean, "^11\\b|araripe")                        ~ "11",
    str_detect(x_clean, "^12\\b|sao francisco")                  ~ "12",
    TRUE ~ NA_character_
  )
}

# Função auxiliar padronizada para soma regional (aceita IBGE 6 ou 7 dígitos e formato BR/US)
somar_por_rd <- function(df_mun) {
  df_base <- df_mun %>%
    mutate(
      ibge6 = substr(str_remove(str_trim(as.character(cod_municipio)), "\\.0$"), 1, 6),
      val_limpo = ifelse(
        str_detect(as.character(indicador), ","),
        str_replace_all(str_remove_all(as.character(indicador), "\\."), ",", "."),
        as.character(indicador)
      ),
      val_num = suppressWarnings(as.numeric(val_limpo))
    ) %>%
    filter(!is.na(val_num)) %>%
    inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6")
  
  if (nrow(df_base) == 0) {
    cli_alert_warning("Aviso: 0 registros cruzados em somar_por_rd(). Verifique cod_municipio e indicador.")
    return(tibble())
  }
  
  df_base %>%
    group_by(cod_rd, nm_rd, ano, nm_indicador, mes_referencia) %>%
    summarise(
      indicador = as.character(sum(val_num, na.rm = TRUE)),
      .groups   = "drop"
    )
}

# ------------------------------------------------------------------------------
# 4. AUTENTICAÇÃO GOOGLE E DICIONÁRIO DE INDICADORES
# ------------------------------------------------------------------------------
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
} else {
  "data/r-sheets-hugo-0788cc358cb5.json"
}

if (file.exists(auth_path)) {
  drive_auth(path = auth_path)
  cli_alert_success("Autenticação Google realizada via {.path {auth_path}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {auth_path}}")
}

url_lista <- "https://docs.google.com/spreadsheets/d/1WNuK75MyAHYofzvd6wkiVIG-FHpwqruIDyUMtkfBlOI/edit?gid=1805893996#gid=1805893996"
caminho_indicador <- tempfile(fileext = ".xlsx")
drive_download(file = googledrive::as_id(url_lista), path = caminho_indicador, overwrite = TRUE)

if (!file.exists(caminho_indicador)) {
  cli_abort("Arquivo de lista de indicadores não encontrado.")
}

lista_de_indicadores <- readxl::read_excel(caminho_indicador, sheet = "Municípios")
cli_alert_success("Dicionário de indicadores carregado ({nrow(lista_de_indicadores)} registros).")

# ------------------------------------------------------------------------------
# 5. EXTRAÇÃO E AGREGAÇÃO REGIONAL TEMÁTICA
# ------------------------------------------------------------------------------

## 5.1 Demografia (Consumo do produto agregado de etl_demografia_rd.R) ----
caminho_demografia_rd <- file.path(DIR_RAW_AURORA, "indicadores/demografia/indicadores_demograficos_rd_pe.parquet")

if (file.exists(caminho_demografia_rd)) {
  df_demog_rd_raw <- arrow::read_parquet(caminho_demografia_rd) %>%
    mutate(
      cod_rd         = as.character(cod_rd),
      nm_rd          = as.character(nm_rd),
      ano            = as.numeric(ano),
      indicador      = as.character(round(as.numeric(valor), 2)),
      mes_referencia = NA_character_
    )
  
  df_pop_rd <- df_demog_rd_raw %>%
    filter(sigla_indicador == "pop_total") %>%
    transmute(cod_rd, nm_rd, ano, pop_rd = as.numeric(indicador))
  
  df_demografia_rd <- df_demog_rd_raw %>%
    filter(sigla_indicador != "pop_total") %>%
    select(cod_rd, nm_rd, ano, nm_indicador, indicador, mes_referencia)
  
  cli_alert_success("Demografia por RD carregada ({nrow(df_demografia_rd)} registros e denominador populacional pronto).")
} else {
  cli_alert_warning("Arquivo de demografia por RD não encontrado: {.path {caminho_demografia_rd}}")
  df_pop_rd        <- tibble()
  df_demografia_rd <- tibble()
}

## 5.2 PIB Total, Participação (%) e PIB per capita (Recalculado) ----
url_pib_total <- "https://docs.google.com/spreadsheets/d/1zvd0k7mBkDLg3hVH1mWyUDmf8D3y99Yo/edit?gid=1848370469#gid=1848370469"
caminho_temp_total <- tempfile(fileext = ".xlsx")
drive_download(file = googledrive::as_id(url_pib_total), path = caminho_temp_total, overwrite = TRUE)

df_pib_total_raw <- readxl::read_excel(caminho_temp_total)
colnames(df_pib_total_raw) <- c("cod_municipio", "nm_municipio", "pib_total")

df_pib_soma_rd <- df_pib_total_raw %>%
  mutate(
    ibge6          = substr(str_remove(str_trim(as.character(cod_municipio)), "\\.0$"), 1, 6),
    ano            = 2023,
    pib_total      = as.numeric(pib_total),
    mes_referencia = NA_character_
  ) %>%
  inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
  group_by(cod_rd, nm_rd, ano, mes_referencia) %>%
  summarise(pib_rd = sum(pib_total, na.rm = TRUE), .groups = "drop") %>%
  mutate(pib_perc = (pib_rd / sum(pib_rd, na.rm = TRUE)) * 100)

df_pib_total_rd <- bind_rows(
  df_pib_soma_rd %>%
    transmute(cod_rd, nm_rd, ano, nm_indicador = "PIB Total (R$ 1.000)", indicador = as.character(pib_rd), mes_referencia),
  df_pib_soma_rd %>%
    transmute(cod_rd, nm_rd, ano, nm_indicador = "Participação no PIB Estadual (%)", indicador = as.character(round(pib_perc, 2)), mes_referencia)
)

if (nrow(df_pop_rd) > 0) {
  df_pib_pcap_rd <- df_pib_soma_rd %>%
    inner_join(df_pop_rd, by = c("cod_rd", "nm_rd", "ano")) %>%
    filter(pop_rd > 0) %>%
    transmute(
      cod_rd,
      nm_rd,
      ano,
      nm_indicador   = "PIB per capita",
      indicador      = as.character(round((pib_rd * 1000) / pop_rd, 2)),
      mes_referencia = NA_character_
    )
} else {
  df_pib_pcap_rd <- tibble()
}
cli_alert_success("PIB Total, Participação e PIB per capita recalculados por RD.")

## 5.3 Valor Adicionado Bruto (VAB Setorial) ----
url_vab <- "https://docs.google.com/spreadsheets/d/19vqJZ-KeABzIf18gNW0-EU-_MJS311un/edit?usp=drive_web&ouid=100127074425655821794&rtpof=true"
caminho_temp_vab <- tempfile(fileext = ".xlsx")
drive_download(file = googledrive::as_id(url_vab), path = caminho_temp_vab, overwrite = TRUE)

df_vab_mun <- readxl::read_excel(caminho_temp_vab) %>%
  janitor::clean_names() %>%
  filter(sigla_da_unidade_da_federacao == "PE") %>%
  select(
    cod_municipio = codigo_do_municipio,
    ano,
    vab_agro = contains("agropecuaria"),
    vab_ind  = contains("industria"),
    vab_serv = contains("dos_servicos"),
    vab_adm  = valor_adicionado_bruto_da_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_a_precos_correntes_r_1_000
  ) %>%
  pivot_longer(
    cols = starts_with("vab_"),
    names_to = "nm_indicador",
    values_to = "indicador"
  ) %>%
  mutate(
    nm_indicador = case_when(
      nm_indicador == "vab_agro" ~ "VAB Agropecuária (R$ 1.000)",
      nm_indicador == "vab_ind"  ~ "VAB Indústria (R$ 1.000)",
      nm_indicador == "vab_serv" ~ "VAB Serviços (R$ 1.000)",
      nm_indicador == "vab_adm"  ~ "VAB Administração Pública (R$ 1.000)"
    ),
    cod_municipio  = as.character(cod_municipio),
    indicador      = as.character(indicador),
    mes_referencia = NA_character_
  )

df_vab_rd <- somar_por_rd(df_vab_mun)
cli_alert_success("VAB setorial somado por RD.")

## 5.4 Segurança Pública (Soma de Ocorrências por RD) ----
url_seguranca <- "https://docs.google.com/spreadsheets/d/1yPbzgctnAvE9p_Vuj_-EbepV_FsTkMQ3H0BNAQfdMtE/edit?gid=1743084514#gid=1743084514"
caminho_temp_seg <- tempfile(fileext = ".xlsx")
drive_download(file = googledrive::as_id(url_seguranca), path = caminho_temp_seg, overwrite = TRUE)

df_seguranca_mun <- readxl::read_excel(caminho_temp_seg) %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = c(mvi, cvp, feminicidio, rfv, tent_mvi, vcm_ameaca, vcm_lesao, vcm_vs),
    names_to = "nm_indicador",
    values_to = "indicador"
  ) %>%
  mutate(
    nm_indicador = case_when(
      nm_indicador == "mvi"         ~ "Mortes Violentas Intencionais (MVI)",
      nm_indicador == "cvp"         ~ "Crimes Violentos contra o Patrimônio (CVP)",
      nm_indicador == "feminicidio" ~ "Feminicídio",
      nm_indicador == "rfv"         ~ "Roubos e Furtos de Veículos (RFV)",
      nm_indicador == "tent_mvi"    ~ "Tentativa de MVI",
      nm_indicador == "vcm_ameaca"  ~ "Violência contra a Mulher - Ameaças",
      nm_indicador == "vcm_lesao"   ~ "Violência contra a Mulher - Lesões",
      nm_indicador == "vcm_vs"      ~ "Violência contra a Mulher - Sexual"
    ),
    ano            = as.numeric(ano),
    cod_municipio  = as.character(codibge),
    indicador      = as.character(indicador),
    mes_referencia = as.character(mes_referencia)
  ) %>%
  select(cod_municipio, ano, nm_indicador, indicador, mes_referencia)

df_seguranca_rd <- somar_por_rd(df_seguranca_mun)
cli_alert_success("Segurança Pública somada por RD.")

## 5.5 Vulnerabilidade Social (Água, Esgoto e CadÚnico Recalculados) ----
id_pasta_vulnerabilidade <- "11SbyKS9ZFN5mCDpIKorUmWEePIaMlQA-"
arquivos_vulnerabilidade <- drive_ls(googledrive::as_id(id_pasta_vulnerabilidade))

df_agua_rede   <- NULL
df_agua_esgoto <- NULL
df_cad_pessoas <- NULL

for (i in seq_len(nrow(arquivos_vulnerabilidade))) {
  id_arq    <- arquivos_vulnerabilidade$id[i]
  nome_arq  <- arquivos_vulnerabilidade$name[i]
  mime_tipo <- arquivos_vulnerabilidade$drive_resource[[i]]$mimeType
  
  is_gsheet  <- grepl("spreadsheet", mime_tipo)
  is_csv     <- grepl("csv", mime_tipo) | grepl("\\.csv$", nome_arq, ignore.case = TRUE)
  is_parquet <- grepl("parquet", mime_tipo) | grepl("\\.parquet$", nome_arq, ignore.case = TRUE)
  
  caminho_temp <- tempfile()
  
  if (is_gsheet) {
    caminho_temp <- paste0(caminho_temp, ".xlsx")
    drive_download(as_id(id_arq), path = caminho_temp, type = "xlsx", overwrite = TRUE)
    temp_df <- readxl::read_excel(caminho_temp)
  } else if (is_csv) {
    caminho_temp <- paste0(caminho_temp, ".csv")
    drive_download(as_id(id_arq), path = caminho_temp, overwrite = TRUE)
    temp_df <- readr::read_csv(caminho_temp, show_col_types = FALSE)
  } else if (is_parquet) {
    caminho_temp <- paste0(caminho_temp, ".parquet")
    drive_download(as_id(id_arq), path = caminho_temp, overwrite = TRUE)
    temp_df <- arrow::read_parquet(caminho_temp)
  } else if (grepl("\\.xlsx?$", nome_arq, ignore.case = TRUE)) {
    caminho_temp <- paste0(caminho_temp, ".xlsx")
    drive_download(as_id(id_arq), path = caminho_temp, overwrite = TRUE)
    temp_df <- readxl::read_excel(caminho_temp)
  } else {
    next
  }
  
  temp_df <- janitor::clean_names(temp_df)
  
  # Mesma lógica exata do etl_caracter_mun.R
  if ("agua_distribuicao" %in% colnames(temp_df)) {
    df_agua_rede <- temp_df
  } else if ("esgoto_distribuicao" %in% colnames(temp_df)) {
    df_agua_esgoto <- temp_df
  } else if ("pessoas" %in% colnames(temp_df) || grepl("cad_pessoas", nome_arq, ignore.case = TRUE)) {
    df_cad_pessoas <- temp_df
  }
}

# 5.5.1 Água: Soma direta das colunas numéricas por RD + Recálculo de % Água de Rede
df_agua_rd <- tibble()
if (!is.null(df_agua_rede)) {
  df_agua_agreg <- df_agua_rede %>%
    mutate(ibge6 = substr(str_remove(str_trim(as.character(codigo)), "\\.0$"), 1, 6)) %>%
    inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
    group_by(cod_rd, nm_rd, referencia) %>%
    summarise(
      across(
        any_of(c(
          "agua_distribuicao", "agua_poco", "agua_cisterna",
          "agua_outros", "agua_na", "total_domicilios_agua"
        )),
        ~ sum(as.numeric(.), na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      pct_agua_distribuicao = ifelse(
        total_domicilios_agua > 0,
        round((agua_distribuicao / total_domicilios_agua) * 100, 2),
        NA_real_
      )
    )
  
  df_agua_rd <- df_agua_agreg %>%
    pivot_longer(
      cols = -c(cod_rd, nm_rd, referencia),
      names_to = "nm_indicador_raw",
      values_to = "valor_num"
    ) %>%
    mutate(
      ano = as.numeric(str_extract(referencia, "\\d{4}$")),
      nm_indicador = case_when(
        nm_indicador_raw == "agua_distribuicao"     ~ "Domicílios com Água de Rede",
        nm_indicador_raw == "agua_poco"             ~ "Domicílios com Água de Poço",
        nm_indicador_raw == "agua_cisterna"         ~ "Domicílios com Água de Cisterna",
        nm_indicador_raw == "agua_outros"           ~ "Domicílios com Água (Outros)",
        nm_indicador_raw == "agua_na"               ~ "Domicílios com Água (Não Informado)",
        nm_indicador_raw == "total_domicilios_agua" ~ "Total de Domicílios (Água)",
        nm_indicador_raw == "pct_agua_distribuicao" ~ "% Água de Rede",
        TRUE ~ nm_indicador_raw
      ),
      indicador      = as.character(valor_num),
      mes_referencia = as.character(referencia)
    ) %>%
    filter(!is.na(valor_num)) %>%
    select(cod_rd, nm_rd, ano, nm_indicador, indicador, mes_referencia)
}

# 5.5.2 Esgoto: Soma direta das colunas numéricas por RD + Recálculo de % Esgoto de Rede
df_esgoto_rd <- tibble()
if (!is.null(df_agua_esgoto)) {
  df_esgoto_agreg <- df_agua_esgoto %>%
    mutate(ibge6 = substr(str_remove(str_trim(as.character(codigo)), "\\.0$"), 1, 6)) %>%
    inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
    group_by(cod_rd, nm_rd, referencia) %>%
    summarise(
      across(
        any_of(c(
          "esgoto_distribuicao", "esgoto_fossa_septica", "esgoto_fossa_rudimentar",
          "esgoto_vala", "esgoto_rio_lago_mar", "esgoto_outros",
          "esgoto_na", "total_domicilios_esgoto"
        )),
        ~ sum(as.numeric(.), na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      pct_esgoto_rede = ifelse(
        total_domicilios_esgoto > 0,
        round((esgoto_distribuicao / total_domicilios_esgoto) * 100, 2),
        NA_real_
      )
    )
  
  df_esgoto_rd <- df_esgoto_agreg %>%
    pivot_longer(
      cols = -c(cod_rd, nm_rd, referencia),
      names_to = "nm_indicador_raw",
      values_to = "valor_num"
    ) %>%
    mutate(
      ano = as.numeric(str_extract(referencia, "\\d{4}$")),
      nm_indicador = case_when(
        nm_indicador_raw == "esgoto_distribuicao"     ~ "Domicílios com Esgoto de Rede",
        nm_indicador_raw == "esgoto_fossa_septica"    ~ "Domicílios com Fossa Séptica",
        nm_indicador_raw == "esgoto_fossa_rudimentar" ~ "Domicílios com Fossa Rudimentar",
        nm_indicador_raw == "esgoto_vala"             ~ "Domicílios com Esgoto em Vala",
        nm_indicador_raw == "esgoto_rio_lago_mar"     ~ "Domicílios com Esgoto em Rio/Lago/Mar",
        nm_indicador_raw == "esgoto_outros"           ~ "Domicílios com Esgoto (Outros)",
        nm_indicador_raw == "esgoto_na"               ~ "Domicílios com Esgoto (Não Informado)",
        nm_indicador_raw == "total_domicilios_esgoto" ~ "Total de Domicílios (Esgoto)",
        nm_indicador_raw == "pct_esgoto_rede"         ~ "% Esgoto de Rede",
        TRUE ~ nm_indicador_raw
      ),
      indicador      = as.character(valor_num),
      mes_referencia = as.character(referencia)
    ) %>%
    filter(!is.na(valor_num)) %>%
    select(cod_rd, nm_rd, ano, nm_indicador, indicador, mes_referencia)
}

# 5.5.3 CadÚnico: Soma de pessoas + Recálculo da Taxa Populacional por RD
df_cad_rd <- tibble()
if (!is.null(df_cad_pessoas)) {
  df_cad_agreg <- df_cad_pessoas %>%
    mutate(
      ibge6 = substr(str_remove(str_trim(as.character(codigo)), "\\.0$"), 1, 6),
      ano   = as.numeric(str_extract(referencia_cad_unico, "\\d{4}$"))
    ) %>%
    inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
    group_by(cod_rd, nm_rd, ano, referencia_cad_unico) %>%
    summarise(
      pessoas = sum(as.numeric(pessoas), na.rm = TRUE),
      .groups = "drop"
    )
  
  df_cad_soma <- df_cad_agreg %>%
    transmute(
      cod_rd,
      nm_rd,
      ano,
      nm_indicador   = "Pessoas no Cad Único",
      indicador      = as.character(pessoas),
      mes_referencia = as.character(referencia_cad_unico)
    )
  
  df_tx_cad_rd <- tibble()
  if (nrow(df_pop_rd) > 0) {
    df_tx_cad_rd <- df_cad_agreg %>%
      inner_join(df_pop_rd, by = c("cod_rd", "nm_rd", "ano")) %>%
      filter(pop_rd > 0) %>%
      transmute(
        cod_rd,
        nm_rd,
        ano,
        nm_indicador   = "Tx. Pessoas no Cad Único",
        indicador      = as.character(round((pessoas / pop_rd) * 100, 2)),
        mes_referencia = as.character(referencia_cad_unico)
      )
  }
  
  df_cad_rd <- bind_rows(df_cad_soma, df_tx_cad_rd)
}

df_vulnerabilidade_rd <- bind_rows(df_agua_rd, df_esgoto_rd, df_cad_rd)
cli_alert_success("Vulnerabilidade Social (somas e taxas recalculadas) processada por RD.")

## 5.6 RAIS Emprego & Tecnologia (Estoques Absolutos + Crescimento Regional) ----
caminho_rais <- file.path(DIR_RAW_AURORA, "rais/df_rais_tecnologia.parquet")

if (file.exists(caminho_rais)) {
  df_rais_raw <- arrow::read_parquet(caminho_rais)
  
  df_rais_abs_mun <- df_rais_raw %>%
    filter(
      sigla_indicador %in% c(
        "estab_agro", "estab_ind", "estab_serv", "estab_alta_tec",
        "emp_agro", "emp_ind", "emp_serv", "emp_me_do"
      )
    ) %>%
    mutate(
      cod_municipio  = as.character(id_municipio),
      ano            = as.numeric(ano),
      indicador      = as.character(valor),
      mes_referencia = NA_character_
    ) %>%
    select(cod_municipio, ano, nm_indicador, indicador, mes_referencia)
  
  df_rais_rd <- somar_por_rd(df_rais_abs_mun)
  
  df_cresc_hightec_rd <- df_rais_raw %>%
    filter(sigla_indicador == "estab_alta_tec") %>%
    mutate(
      ibge6     = substr(str_remove(str_trim(as.character(id_municipio)), "\\.0$"), 1, 6),
      ano       = as.numeric(ano),
      valor_num = suppressWarnings(as.numeric(valor))
    ) %>%
    filter(!is.na(valor_num), !is.na(ano)) %>%
    inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
    group_by(cod_rd, nm_rd, ano) %>%
    summarise(estab_rd = sum(valor_num, na.rm = TRUE), .groups = "drop") %>%
    group_by(cod_rd, nm_rd) %>%
    filter(ano %in% c(max(ano), max(ano) - 3)) %>%
    arrange(cod_rd, ano) %>%
    filter(n() == 2) %>%
    summarise(
      ano     = max(ano),
      val_t3  = first(estab_rd),
      val_t   = last(estab_rd),
      val_arc = ifelse(
        (val_t + val_t3) == 0, 0,
        ((val_t - val_t3) / ((val_t + val_t3) / 2)) * 100
      ),
      .groups = "drop"
    ) %>%
    transmute(
      cod_rd,
      nm_rd,
      ano,
      nm_indicador   = "Crescimento Trienal de Empresas High-Tech (%)",
      indicador      = as.character(round(val_arc, 2)),
      mes_referencia = NA_character_
    )
  
  cli_alert_success("RAIS (estoques absolutos e crescimento High-Tech) somados por RD.")
} else {
  cli_alert_warning("Arquivo RAIS não encontrado: {.path {caminho_rais}}")
  df_rais_rd          <- tibble()
  df_cresc_hightec_rd <- tibble()
}

## 5.7 Comércio Exterior (Valores e Pesos Absolutos) ----
caminho_comex <- file.path(DIR_RAW_AURORA, "comex/df_comex_long.parquet")

if (file.exists(caminho_comex)) {
  df_comex_mun <- arrow::read_parquet(caminho_comex) %>%
    filter(!str_detect(tolower(nm_indicador), "taxa|tx\\.|%|percentual|participa[cç][aã]o|saldo|corrente")) %>%
    mutate(
      cod_municipio  = as.character(cod_municipio),
      ano            = as.numeric(ano),
      indicador      = as.character(indicador),
      mes_referencia = as.character(mes_referencia)
    ) %>%
    select(cod_municipio, ano, nm_indicador, indicador, mes_referencia)
  
  df_comex_rd <- somar_por_rd(df_comex_mun)
  cli_alert_success("Comércio Exterior somado por RD.")
} else {
  cli_alert_warning("Arquivo Comex não encontrado: {.path {caminho_comex}}")
  df_comex_rd <- tibble()
}

## 5.8 Saúde por RD (Ingestão Direta do CSV no Google Drive) ----
url_saude_rd <- "https://drive.google.com/file/d/1Q00lIGtgK0dE3SGyiqLAi4SVVwR4BMcc/view?usp=sharing"
caminho_temp_saude <- tempfile(fileext = ".csv")

df_saude_rd <- tryCatch({
  drive_download(file = googledrive::as_id(url_saude_rd), path = caminho_temp_saude, overwrite = TRUE)
  
  # Detecta delimitador automaticamente (; ou ,)
  primeira_linha <- readLines(caminho_temp_saude, n = 1, warn = FALSE)
  delim_detectado <- ifelse(str_count(primeira_linha, ";") >= str_count(primeira_linha, ","), ";", ",")
  
  df_saude_raw <- readr::read_delim(
    caminho_temp_saude,
    delim = delim_detectado,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
    janitor::clean_names()
  
  res <- df_saude_raw %>%
    mutate(cod_rd = padronizar_rd_externa(rd)) %>%
    filter(!is.na(cod_rd)) %>%
    inner_join(df_rds_ref, by = "cod_rd") %>%
    pivot_longer(
      cols = c(tx_evitaveis, tx_dcnt, tx_att, razao_de_mm, tx_infantil, tx_infancia, icsap, pre_natal),
      names_to = "nm_indicador_raw",
      values_to = "indicador_raw"
    ) %>%
    mutate(
      nm_indicador = case_when(
        nm_indicador_raw == "tx_evitaveis" ~ "Tx. Evitáveis",
        nm_indicador_raw == "tx_dcnt"      ~ "Tx. DCNT",
        nm_indicador_raw == "tx_att"       ~ "Tx. ATT",
        nm_indicador_raw == "razao_de_mm"  ~ "Razão de MM",
        nm_indicador_raw == "tx_infantil"  ~ "Tx. Infantil",
        nm_indicador_raw == "tx_infancia"  ~ "Tx. Infância",
        nm_indicador_raw == "icsap"        ~ "ICSAP",
        nm_indicador_raw == "pre_natal"    ~ "Pré-Natal",
        TRUE ~ nm_indicador_raw
      ),
      ano = as.numeric(ano),
      # Remove o símbolo '%' (presente em Pré-Natal) e normaliza vírgula/ponto decimal
      val_sem_pct = str_remove_all(str_trim(indicador_raw), "%"),
      val_limpo = ifelse(
        str_detect(val_sem_pct, ","),
        str_replace_all(str_remove_all(val_sem_pct, "\\."), ",", "."),
        val_sem_pct
      ),
      val_num        = suppressWarnings(as.numeric(val_limpo)),
      indicador      = as.character(val_num),
      mes_referencia = NA_character_
    ) %>%
    filter(!is.na(val_num)) %>%
    select(cod_rd, nm_rd, ano, nm_indicador, indicador, mes_referencia)
  
  cli_alert_success("Saúde por RD processada ({nrow(res)} registros).")
  res
}, error = function(e) {
  cli_alert_warning("Erro ao processar arquivo de Saúde por RD: {e$message}")
  tibble()
})
# ------------------------------------------------------------------------------
# 6. EMPILHAMENTO E ENRIQUECIMENTO DE METADADOS
# ------------------------------------------------------------------------------
df_indicadores_rd_empilhados <- bind_rows(
  df_demografia_rd,
  df_pib_total_rd,
  df_pib_pcap_rd,
  df_vab_rd,
  df_seguranca_rd,
  df_vulnerabilidade_rd,
  df_rais_rd,
  df_cresc_hightec_rd,
  df_comex_rd,
  df_saude_rd
) %>%
  left_join(
    lista_de_indicadores %>% distinct(nm_indicador, .keep_all = TRUE),
    by = "nm_indicador"
  ) %>%
  filter(!is.na(indicador)) %>%
  select(
    cod_rd,
    nm_rd,
    ano,
    nm_indicador,
    nm_fantasia,
    indicador,
    mes_referencia,
    sigla_indicador,
    grupo,
    tipo,
    polaridade,
    fonte,
    hiperlink,
    everything()
  ) %>%
  arrange(sigla_indicador, cod_rd, ano)

cli_alert_success("Indicadores por RD consolidados: {nrow(df_indicadores_rd_empilhados)} registros totais.")

# ------------------------------------------------------------------------------
# 7. CÁLCULO DE RANKINGS REGIONAIS (1 a 12) E ESTRUTURAÇÃO FINAL
# ------------------------------------------------------------------------------
df_indicadores_rd_final <- df_indicadores_rd_empilhados %>%
  group_by(nm_indicador, ano, mes_referencia) %>%
  mutate(
    val_num_temp = suppressWarnings(as.numeric(indicador)),
    rank_rd      = ifelse(is.na(val_num_temp), NA_integer_, min_rank(desc(val_num_temp)))
  ) %>%
  select(-val_num_temp) %>%
  ungroup()

df_indicadores_rd_final %>% View()
cli_alert_success("Rankings entre as 12 RDs calculados com sucesso.")

# ------------------------------------------------------------------------------
# 8. EXPORTAÇÃO LOCAL (RDS & PARQUET)
# ------------------------------------------------------------------------------
readr::write_rds(df_indicadores_rd_empilhados, file.path(DIR_OUT_AURORA, "df_indicadores_rd_empilhados.rds"))
readr::write_rds(df_indicadores_rd_final, file.path(DIR_OUT_AURORA, "df_indicadores_rd_final.rds"))

caminho_parquet_rd <- file.path(dir_parquet_out, "df_indicadores_rd.parquet")
arrow::write_parquet(
  df_indicadores_rd_final,
  caminho_parquet_rd,
  compression = "snappy"
)

arrow::write_parquet(
  df_indicadores_rd_final,
  file.path(dir_raw_indicadores, "df_indicadores_rd.parquet"),
  compression = "snappy"
)

cli_alert_success("Arquivo Parquet oficial das RDs salvo em {.path {caminho_parquet_rd}}.")
cli_alert_success("Processamento de etl_indicadores_rd_pe.R concluído com sucesso!")


# ------------------------------------------------------------------------------
# 9. EXPORTAÇÃO EXCEL
# ------------------------------------------------------------------------------

df_indicadores_rd_final_xlsx <- df_indicadores_rd_final %>% 
  select(cod_rd, nm_rd, ano, nm_indicador, indicador, mes_referencia, sigla_indicador)


# Exportação para Google Sheets ----
cli::cli_alert_info("Iniciando exportação para o Google Sheets...")

# 1. Autenticação via Service Account (padrão DIR_RAW_AURORA)
google_token_path <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(google_token_path)) {
  googlesheets4::gs4_auth(path = google_token_path)
} else {
  cli::cli_warn("Token JSON não encontrado em {.path {google_token_path}}. Tentando autenticação padrão...")
  googlesheets4::gs4_auth()
}

# 2. ID da Planilha de destino
sheet_id_destino <- "15Azw7s3RQZPX4uQ_xsaMB43w1cpvPQ2RsN-U3meceUE"

# 3. Escrita da base consolidada na primeira aba (gid = 0)
googlesheets4::sheet_write(
  data  = df_indicadores_rd_final_xlsx,
  ss    = sheet_id_destino,
  sheet = 1
)

cli::cli_alert_success("Base {.val df_indicadores_ufs} exportada com sucesso para o Google Sheets!")

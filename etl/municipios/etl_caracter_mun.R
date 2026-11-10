# ==============================================================================
# SCRIPT: etl_caracter_mun.R
# Objetivo: Extração, tratamento e consolidação multi-fonte de indicadores
#           municipais de Pernambuco (IDHM, PIB, CAPAG, VAB, Segurança, Educação,
#           Saúde, Vulnerabilidade, Demografia, Emprego/Tecnologia, Comex e Anatel).
#           Empilhamento final com cálculo de ranking estadual e exportação (RDS/Parquet).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(googledrive)
library(readxl)
library(janitor)
library(yaml)
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

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Estrutura de diretórios de saída
dir_parquet_out       <- file.path(DIR_OUT_AURORA, "parquet")
dir_raw_ibge          <- file.path(DIR_RAW_AURORA, "ibge")
dir_raw_indicadores   <- file.path(DIR_RAW_AURORA, "indicadores")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_ibge, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_indicadores, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. AUTENTICAÇÃO GOOGLE
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

# ------------------------------------------------------------------------------
# 4. DICIONÁRIO DE INDICADORES
# ------------------------------------------------------------------------------
caminho_lista_ind <- file.path(DIR_RAW_AURORA, "indicadores/lista de indicadores.xlsx")

if (!file.exists(caminho_lista_ind)) {
  cli_abort("Arquivo de lista de indicadores não encontrado: {.path {caminho_lista_ind}}")
}

lista_de_indicadores <- readxl::read_excel(caminho_lista_ind, sheet = "Sheet1")
cli_alert_success("Dicionário de indicadores carregado ({nrow(lista_de_indicadores)} registros).")

# ------------------------------------------------------------------------------
# 5. EXTRAÇÃO E TRATAMENTO TEMÁTICO
# ------------------------------------------------------------------------------

## 5.1 IDHM 2010 ----
url_idhm <- "https://docs.google.com/spreadsheets/d/1avZ8RQ4DzNYm_dV3Z961qlT3rNWGSw0a/edit?gid=1884946112#gid=1884946112"
caminho_temp_idhm <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_idhm), path = caminho_temp_idhm, overwrite = TRUE)

df_idhm_long <- readxl::read_excel(caminho_temp_idhm) %>%
  janitor::clean_names() %>%
  mutate(
    ano = 2010,
    nm_indicador = "IDHM"
  ) %>%
  select(
    cod_municipio = cod_municipio_ibge,
    nm_municipio = nome_municipio,
    ano,
    nm_indicador,
    indicador = idh_municipal_2010
  ) %>%
  mutate(
    cod_municipio = as.character(cod_municipio),
    indicador = as.character(indicador), 
    mes_referencia = NA_character_
  )
cli_alert_success("IDHM processado.")

## 5.2 PIB per capita ----
url_pib_pcap <- "https://docs.google.com/spreadsheets/d/1fr6W50Y7L17bRr6Q2LVPNFz6XW8yeauC/edit?gid=19523155#gid=19523155"
caminho_temp_pcap <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_pib_pcap), path = caminho_temp_pcap, overwrite = TRUE)

df_pib_pcap_long <- readxl::read_excel(caminho_temp_pcap) %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = starts_with("pcap"),
    names_to = "coluna_ano",
    values_to = "indicador"
  ) %>%
  mutate(
    ano = as.numeric(str_extract(coluna_ano, "\\d{4}")),
    nm_indicador = "PIB per capita"
  ) %>%
  select(
    cod_municipio = cod_municipio_ibge,
    nm_municipio = munic,
    ano,
    nm_indicador,
    indicador
  ) %>%
  mutate(
    cod_municipio = as.character(cod_municipio),
    indicador = as.character(indicador), 
    mes_referencia = NA_character_
  )
cli_alert_success("PIB per capita processado.")

## 5.3 PIB Total e Participação (%) ----
url_pib_total <- "https://docs.google.com/spreadsheets/d/1zvd0k7mBkDLg3hVH1mWyUDmf8D3y99Yo/edit?gid=1848370469#gid=1848370469"
caminho_temp_total <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_pib_total), path = caminho_temp_total, overwrite = TRUE)

df_pib_total_raw <- readxl::read_excel(caminho_temp_total)
colnames(df_pib_total_raw) <- c("cod_municipio", "nm_municipio", "pib_total")

df_pib_total_long <- df_pib_total_raw %>%
  mutate(
    ano = 2023,
    pib_total = as.numeric(pib_total),
    pib_perc = (pib_total / sum(pib_total, na.rm = TRUE))
  ) %>%
  pivot_longer(
    cols = c(pib_total, pib_perc),
    names_to = "nm_indicador",
    values_to = "indicador"
  ) %>%
  mutate(
    nm_indicador = case_when(
      nm_indicador == "pib_total" ~ "PIB Total (R$ 1.000)",
      nm_indicador == "pib_perc"  ~ "Participação no PIB Estadual (%)"
    )
  ) %>%
  select(
    cod_municipio,
    nm_municipio,
    ano,
    nm_indicador,
    indicador
  ) %>%
  mutate(
    cod_municipio = as.character(cod_municipio),
    indicador = as.character(indicador), 
    mes_referencia = NA_character_
  )
cli_alert_success("PIB Total e Participação processados.")

## 5.4 CAPAG 2026 ----
url_capag <- "https://docs.google.com/spreadsheets/d/1OD2C_8mNchuI_3gwH3yhoXf0sY2QD-QqXGHmICmYX0Q/edit?gid=0#gid=0"
caminho_temp_capag <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_capag), path = caminho_temp_capag, overwrite = TRUE)

df_capag_long <- readxl::read_excel(caminho_temp_capag, sheet = 1) %>%
  janitor::clean_names() %>%
  filter(uf == "PE") %>%
  rename_with(
    ~ paste0("capag_", .), 
    .cols = indicador_1:publicou_rreo & !contains("capag_rebaixada")
  ) %>%
  mutate(across(c(capag, starts_with("capag_")), as.character)) %>%
  pivot_longer(
    cols = c(capag, starts_with("capag_")),
    names_to = "nm_indicador_raw",
    values_to = "indicador"
  ) %>%
  mutate(
    ano = 2026,
    cod_municipio = as.character(codigo_municipio_completo),
    nm_municipio = nome_municipio,
    nm_indicador = case_when(
      nm_indicador_raw == "capag"                        ~ "CAPAG",
      nm_indicador_raw == "capag_indicador_1"            ~ "CAPAG - Indicador 1",
      nm_indicador_raw == "capag_nota_1"                 ~ "CAPAG - Nota 1",
      nm_indicador_raw == "capag_indicador_2"            ~ "CAPAG - Indicador 2",
      nm_indicador_raw == "capag_nota_2"                 ~ "CAPAG - Nota 2",
      nm_indicador_raw == "capag_indicador_3"            ~ "CAPAG - Indicador 3",
      nm_indicador_raw == "capag_nota_3"                 ~ "CAPAG - Nota 3",
      nm_indicador_raw == "capag_icf"                    ~ "CAPAG - ICF",
      nm_indicador_raw == "capag_origem_da_nota_final"   ~ "CAPAG - Origem da Nota Final",
      nm_indicador_raw == "capag_possui_dca_2025"        ~ "CAPAG - Possui DCA 2025",
      nm_indicador_raw == "capag_indicador_3_antigo"     ~ "CAPAG - Indicador 3 Antigo",
      nm_indicador_raw == "capag_possui_dca_2024"        ~ "CAPAG - Possui DCA 2024",
      nm_indicador_raw == "capag_deducao_negativa"       ~ "CAPAG - Dedução Negativa",
      nm_indicador_raw == "capag_dcb_zerada_ou_negativa" ~ "CAPAG - DCB Zerada ou Negativa",
      nm_indicador_raw == "capag_of_negativa"            ~ "CAPAG - OF Negativa",
      nm_indicador_raw == "capag_publicou_rgf"           ~ "CAPAG - Publicou RGF",
      nm_indicador_raw == "capag_publicou_rreo"          ~ "CAPAG - Publicou RREO",
      TRUE ~ nm_indicador_raw
    ),
    mes_referencia = NA_character_
  ) %>%
  filter(!is.na(indicador)) %>%
  select(
    cod_municipio,
    nm_municipio,
    ano,
    nm_indicador,
    indicador, 
    mes_referencia
  )
cli_alert_success("CAPAG processado.")

## 5.5 Valor Adicionado Bruto (VAB) ----
url_vab <- "https://docs.google.com/spreadsheets/d/19vqJZ-KeABzIf18gNW0-EU-_MJS311un/edit?usp=drive_web&ouid=100127074425655821794&rtpof=true"
caminho_temp_vab <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_vab), path = caminho_temp_vab, overwrite = TRUE)

df_vab_long <- readxl::read_excel(caminho_temp_vab) %>%
  janitor::clean_names() %>%
  filter(sigla_da_unidade_da_federacao == "PE") %>%
  select(
    cod_municipio = codigo_do_municipio,
    nm_municipio = nome_do_municipio,
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
    cod_municipio = as.character(cod_municipio),
    indicador = as.character(indicador), 
    mes_referencia = NA_character_
  )
cli_alert_success("VAB setorial processado.")

## 5.6 Segurança Pública ----
url_seguranca <- "https://docs.google.com/spreadsheets/d/1yPbzgctnAvE9p_Vuj_-EbepV_FsTkMQ3H0BNAQfdMtE/edit?gid=1743084514#gid=1743084514"
caminho_temp_seg <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_seguranca), path = caminho_temp_seg, overwrite = TRUE)

df_seguranca_long <- readxl::read_excel(caminho_temp_seg) %>%
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
    ano = as.numeric(ano),
    cod_municipio = as.character(codibge),
    indicador = as.character(indicador),
    mes_referencia = as.character(mes_referencia)
  ) %>%
  select(
    cod_municipio,
    nm_municipio = ppv_municipio,
    ano,
    nm_indicador,
    indicador, 
    mes_referencia
  )
cli_alert_success("Segurança Pública processada.")

## 5.7 Educação e Creche ----
url_educacao <- "https://docs.google.com/spreadsheets/d/12UyPDZ5-IPz7-kqcZEuQ6VOXeHFf3fw6/edit?gid=1967164265#gid=1967164265"
caminho_temp_educacao <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_educacao), path = caminho_temp_educacao, overwrite = TRUE)

df_educacao_long <- readxl::read_excel(caminho_temp_educacao) %>%
  janitor::clean_names() %>%
  mutate(
    nm_indicador = indicador,
    mes_referencia = NA_character_,
    ano = as.numeric(ano),
    cod_municipio = as.character(cod_municipio),
    indicador = as.character(valor)
  ) %>%
  select(
    cod_municipio,
    nm_municipio = municipio,
    ano,
    nm_indicador,
    indicador,
    mes_referencia
  ) %>%
  filter(
    !is.na(nm_indicador),
    !nm_indicador %in% c("Atendimento Creche")
  )

url_atliqcreche <- "https://docs.google.com/spreadsheets/d/1j4YgyWGfFM9moubTf6OvyyH6f0HbaLS_/edit?gid=265454410#gid=265454410"
caminho_temp_atliqcreche <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_atliqcreche), path = caminho_temp_atliqcreche, overwrite = TRUE)

df_atliqcreche_long <- readxl::read_excel(
  caminho_temp_atliqcreche, 
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names() %>%
  mutate(codigo_ibge = as.character(codigo_ibge)) %>%
  filter(str_starts(codigo_ibge, "^26"), nchar(codigo_ibge) == 7) %>%
  mutate(across(matches("^x\\d{4}$"), as.character)) %>%
  select(
    cod_municipio = codigo_ibge,
    nm_municipio = localidade,
    matches("^x\\d{4}$")
  ) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "ano",
    values_to = "indicador"
  ) %>%
  mutate(
    ano = as.numeric(str_remove(ano, "^x")),
    nm_indicador = "Atendimento em Creche (Líquido)",
    mes_referencia = NA_character_
  ) %>%
  select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia) %>%
  filter(!is.na(indicador))

df_abrinq_long <- df_atliqcreche_long
cli_alert_success("Educação e Creches processadas.")

## 5.8 Saúde ----
url_saude <- "https://docs.google.com/spreadsheets/d/18QWoCNTpWAryfvizO6L76KewDAME5TXajNYPdspEf5k/edit?gid=1945143474#gid=1945143474"
caminho_temp_saude <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_saude), path = caminho_temp_saude, overwrite = TRUE)

df_saude_long <- readxl::read_excel(caminho_temp_saude) %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = -c(cod_municipio_ibge, municipio, ano),
    names_to = "nm_indicador_raw",
    values_to = "indicador"
  ) %>%
  mutate(
    nm_indicador = case_when(
      nm_indicador_raw == "tx_evitaveis" ~ "Tx. Evitáveis",
      nm_indicador_raw == "tx_dcnt"      ~ "Tx. DCNT",
      nm_indicador_raw == "tx_att"       ~ "Tx. ATT",
      nm_indicador_raw == "razao_de_mm"  ~ "Razão de MM",
      nm_indicador_raw == "tx_infantil"  ~ "Tx. Infantil",
      nm_indicador_raw == "tx_infancia"  ~ "Tx. Infância",
      nm_indicador_raw == "pre_natal"    ~ "Pré-Natal",
      nm_indicador_raw == "icsap"        ~ "ICSAP",
      TRUE ~ nm_indicador_raw
    ),
    ano = as.numeric(ano),
    cod_municipio = as.character(cod_municipio_ibge),
    indicador = as.character(indicador), 
    mes_referencia = NA_character_
  ) %>%
  select(
    cod_municipio,
    nm_municipio = municipio,
    ano,
    nm_indicador,
    indicador,
    mes_referencia
  )
cli_alert_success("Saúde processada.")

## 5.8.1 Crianças de 5 a 10 anos abaixo do peso ideal ----
# Fonte: Observatório da Criança e do Adolescente (Fundação Abrinq)
# Bruto: raw/saude/Proporção de crianças de 5 a 10 anos de idade abaixo do peso ideal.xlsx
# Aba 2, colunas "AAAA (%)" e "AAAA (absoluto)"; "-" = sem dado.
nm_peso_pct <- "Crianças de 5 a 10 anos abaixo do peso ideal (%)"
nm_peso_abs <- "Crianças de 5 a 10 anos abaixo do peso ideal (nº)"

df_peso_ideal_long <- readxl::read_excel(
  file.path(DIR_RAW_AURORA, "saude/Proporção de crianças de 5 a 10 anos de idade abaixo do peso ideal.xlsx"),
  sheet = 2,
  skip = 2,
  col_types = "text"
) %>%
  rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
  rename(cod_municipio = `Código IBGE`, nm_municipio = Localidade) %>%
  filter(str_starts(cod_municipio, "26"), nchar(cod_municipio) == 7) %>%
  select(cod_municipio, nm_municipio, matches("^[0-9]{4} \\((%|absoluto)\\)$")) %>%
  pivot_longer(
    cols = -c(cod_municipio, nm_municipio),
    names_to = "col_orig",
    values_to = "indicador"
  ) %>%
  mutate(
    ano = as.numeric(str_extract(col_orig, "^[0-9]{4}")),
    eh_abs = str_detect(col_orig, "absoluto"),
    nm_indicador = if_else(eh_abs, nm_peso_abs, nm_peso_pct),
    valor = suppressWarnings(as.numeric(indicador)),
    indicador = case_when(
      is.na(valor) ~ NA_character_,
      eh_abs       ~ as.character(valor),
      TRUE         ~ as.character(round(valor, 2))  # remove ruído de ponto flutuante (3.5000000000000004)
    ),
    mes_referencia = NA_character_
  ) %>%
  filter(!is.na(indicador)) %>%
  select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
cli_alert_success("Peso ideal (5 a 10 anos) processado.")

## 5.9 Vulnerabilidade Social (Drive e Arquivos Locais) ----
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
    temp_df <- readr::read_csv(caminho_temp)
  } else if (is_parquet) {
    caminho_temp <- paste0(caminho_temp, ".parquet")
    drive_download(as_id(id_arq), path = caminho_temp, overwrite = TRUE)
    temp_df <- arrow::read_parquet(caminho_temp)
  } else if (grepl("\\.xlsx?$", nome_arq, ignore.case = TRUE)) {
    caminho_temp <- paste0(caminho_temp, ".xlsx")
    drive_download(as_id(id_arq), path = caminho_temp, overwrite = TRUE)
    temp_df <- readxl::read_excel(caminho_temp)
  } else {
    cli_alert_warning("Formato não suportado ignorado: {nome_arq}")
    next
  }
  
  temp_df <- janitor::clean_names(temp_df)
  
  if ("agua_distribuicao" %in% colnames(temp_df)) {
    df_agua_rede <- temp_df
  } else if ("esgoto_distribuicao" %in% colnames(temp_df)) {
    df_agua_esgoto <- temp_df
  } else if ("pessoas_cad_unico" %in% colnames(temp_df) | grepl("cad_pessoas", nome_arq, ignore.case = TRUE)) { 
    df_cad_pessoas <- temp_df
  }
}

df_agua_long <- df_agua_rede %>%
  pivot_longer(
    cols = -c(codigo, unidade_territorial, uf, referencia),
    names_to = "nm_indicador_raw",
    values_to = "indicador"
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
    cod_municipio = as.character(codigo),
    indicador = as.character(indicador),
    mes_referencia = referencia
  ) %>%
  select(cod_municipio, nm_municipio = unidade_territorial, ano, nm_indicador, indicador, mes_referencia)

df_esgoto_long <- df_agua_esgoto %>%
  pivot_longer(
    cols = -c(codigo, unidade_territorial, uf, referencia),
    names_to = "nm_indicador_raw",
    values_to = "indicador"
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
    cod_municipio = as.character(codigo),
    indicador = as.character(indicador), 
    mes_referencia = referencia
  ) %>%
  select(cod_municipio, nm_municipio = unidade_territorial, ano, nm_indicador, indicador, mes_referencia)

df_cad_pessoas_long <- df_cad_pessoas %>%
  pivot_longer(
    cols = -c(codigo, unidade_territorial, uf, referencia),
    names_to = "nm_indicador_raw",
    values_to = "indicador"
  ) %>%
  mutate(
    ano = as.numeric(str_extract(referencia, "\\d{4}$")),
    nm_indicador = case_when(
      nm_indicador_raw == "tx_pessoas_cad_unico" ~ "Tx. Pessoas no Cad Único",
      nm_indicador_raw == "pessoas_cad_unico"    ~ "Pessoas no Cad Único",
      TRUE ~ nm_indicador_raw
    ),
    cod_municipio = as.character(codigo),
    indicador = as.character(indicador), 
    mes_referencia = referencia
  ) %>%
  select(cod_municipio, nm_municipio = unidade_territorial, ano, nm_indicador, indicador, mes_referencia)

df_vulnerabilidade_long <- bind_rows(
  df_agua_long,
  df_esgoto_long,
  df_cad_pessoas_long
)

url_urb <- "https://docs.google.com/spreadsheets/d/1eoH5dTmx5xipoV_plL7jIPB5yvQNHx1rOqIk3_UZM9o/edit?gid=0#gid=0"
caminho_temp_urb <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_urb), path = caminho_temp_urb, overwrite = TRUE)

df_urbanizacao_long <- readxl::read_excel(caminho_temp_urb) %>% 
  mutate(
    mes_referencia = NA_character_, 
    ano = as.numeric(ano), 
    indicador = as.character(indicador)
  ) %>%
  select(-any_of("sigla_indicador"))

cli_alert_success("Vulnerabilidade e Urbanização processadas.")

## 5.10 IVCAD (Config YAML & Excel) ----
caminho_config_ivcad <- file.path(DIR_RAW_AURORA, "indicadores/ivcad/config.yml")
caminho_dados_ivcad  <- file.path(DIR_RAW_AURORA, "indicadores/ivcad/ivcad_municipios.xlsx")

if (file.exists(caminho_config_ivcad) && file.exists(caminho_dados_ivcad)) {
  config_ivcad <- read_yaml(caminho_config_ivcad)
  mes_ano_referencia_ivcad <- config_ivcad$indicador$mes_referencia
  ano_config_ivcad <- as.numeric(substr(mes_ano_referencia_ivcad, 4, 7))
  
  df_ivcad_long <- readxl::read_excel(caminho_dados_ivcad) %>%
    janitor::clean_names() %>%
    filter(str_starts(cod_ibge, "^26")) %>%
    select(
      cod_municipio = cod_ibge,
      nm_municipio  = municipio,
      ivcad, 
      nc, 
      dpi, 
      dca, 
      tqa, 
      dr, 
      ch
    ) %>%
    pivot_longer(
      cols = c(ivcad, nc, dpi, dca, tqa, dr, ch),
      names_to = "nm_indicador_raw",
      values_to = "indicador"
    ) %>%
    mutate(
      ano            = ano_config_ivcad,
      mes_referencia = mes_ano_referencia_ivcad,
      cod_municipio  = as.character(cod_municipio),
      indicador      = as.character(indicador),
      nm_indicador   = case_when(
        nm_indicador_raw == "ivcad" ~ "Índice de Vulnerabilidade (IVCAD)",
        nm_indicador_raw == "nc"    ~ "Necessidade de Cuidados (NC)",
        nm_indicador_raw == "dpi"   ~ "Desenvolvimento na Primeira Infância (DPI)",
        nm_indicador_raw == "dca"   ~ "Desenvolvimento de Crianças e Adolescentes (DCA)",
        nm_indicador_raw == "tqa"   ~ "Trabalho e Qualificação de Adultos (TQA)",
        nm_indicador_raw == "dr"    ~ "Disponibilidade de Recursos (DR)",
        nm_indicador_raw == "ch"    ~ "Condições Habitacionais (CH)"
      )
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  cli_alert_success("IVCAD processado.")
} else {
  cli_alert_warning("Arquivos do IVCAD não encontrados em {.path {DIR_RAW_AURORA}}.")
  df_ivcad_long <- tibble()
}

## 5.11 Demografia Parquet ----
caminho_demografia <- file.path(DIR_RAW_AURORA, "indicadores/demografia/indicadores_demograficos_pe.parquet")

if (file.exists(caminho_demografia)) {
  df_demografia_long <- arrow::read_parquet(caminho_demografia) %>%
    mutate(
      cod_municipio  = as.character(locality_id),
      nm_municipio   = as.character(locality_name),
      ano            = as.numeric(ano),
      indicador      = as.character(valor),
      mes_referencia = NA_character_
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  cli_alert_success("Demografia processada.")
} else {
  cli_alert_warning("Arquivo de demografia não encontrado: {.path {caminho_demografia}}")
  df_demografia_long <- tibble()
}

## 5.12 RAIS Emprego & Tecnologia ----
caminho_rais <- file.path(DIR_RAW_AURORA, "rais/df_rais_tecnologia.parquet")

if (file.exists(caminho_rais)) {
  df_rais_raw <- arrow::read_parquet(caminho_rais)
  
  df_rais_long <- df_rais_raw %>%
    filter(
      sigla_indicador %in% c(
        'sal_avg_sup', 'sal_avg_me_do', 'sal_avg_criativas', 'sal_avg_infocom',
        'sal_avg_alta_tec', 'sal_avg_agro', 'sal_avg_ind', 'sal_avg_serv',
        'estab_agro', 'pct_estab_agro', 'estab_ind', 'pct_estab_ind',
        'estab_serv', 'pct_estab_serv', 'pct_estab_infocom',
        'pct_estab_ind_criativas', 'pct_estab_alta_tec', 'pct_estab_me_do',
        'emp_agro', 'pct_emp_agro', 'emp_ind', 'pct_emp_ind',
        'emp_serv', 'pct_emp_serv', 'pct_emp_criativas', 'pct_emp_infocom',
        'pct_emp_alta_tec', 'pct_emp_me_do', 'pct_emp_me_do_ind',
        'pct_emp_me_do_infocom', 'pct_emp_me_do_criativas', 'pct_emp_me_do_alta_tec',
        'estab_alta_tec', 'emp_me_do'
      )
    ) %>%
    mutate(
      cod_municipio  = as.character(id_municipio),
      nm_municipio   = NA_character_,
      ano            = as.numeric(ano),
      indicador      = as.character(valor),
      mes_referencia = NA_character_
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  
  df_cresc_hightec_long <- df_rais_raw %>%
    filter(sigla_indicador == "estab_alta_tec") %>%
    mutate(
      cod_municipio = as.character(id_municipio),
      ano           = as.numeric(ano),
      valor_num     = suppressWarnings(as.numeric(valor))
    ) %>%
    filter(!is.na(valor_num), !is.na(ano)) %>%
    group_by(cod_municipio) %>%
    filter(ano %in% c(max(ano), max(ano) - 3)) %>%
    arrange(cod_municipio, ano) %>%
    filter(n() == 2) %>%
    summarise(
      ano            = max(ano),
      val_t3         = first(valor_num),
      val_t          = last(valor_num),
      val_arc        = ifelse(
        (val_t + val_t3) == 0, 0,
        ((val_t - val_t3) / ((val_t + val_t3) / 2)) * 100
      ),
      .groups        = "drop"
    ) %>%
    mutate(
      nm_municipio   = NA_character_,
      nm_indicador   = "Crescimento Trienal de Empresas High-Tech (%)",
      indicador      = as.character(round(val_arc, 2)),
      mes_referencia = NA_character_
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  
  cli_alert_success("RAIS Emprego e Tecnologia processados.")
} else {
  cli_alert_warning("Arquivo RAIS não encontrado: {.path {caminho_rais}}")
  df_rais_long <- tibble()
  df_cresc_hightec_long <- tibble()
}

## 5.13 Comércio Exterior ----
caminho_comex <- file.path(DIR_RAW_AURORA, "comex/df_comex_long.parquet")

if (file.exists(caminho_comex)) {
  df_comex_long <- arrow::read_parquet(caminho_comex) %>%
    mutate(
      cod_municipio  = as.character(cod_municipio),
      nm_municipio   = as.character(nm_municipio),
      ano            = as.numeric(ano),
      indicador      = as.character(indicador),
      mes_referencia = as.character(mes_referencia)
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  cli_alert_success("Comércio Exterior processado.")
} else {
  cli_alert_warning("Arquivo Comex não encontrado: {.path {caminho_comex}}")
  df_comex_long <- tibble()
}

## 5.14 Conectividade (Anatel) ----
caminho_anatel <- file.path(DIR_RAW_AURORA, "anatel/df_anatel_ibc_long.parquet")

if (file.exists(caminho_anatel)) {
  df_anatel_long <- arrow::read_parquet(caminho_anatel) %>%
    mutate(
      cod_municipio  = as.character(cod_municipio),
      nm_municipio   = as.character(nm_municipio),
      ano            = as.numeric(ano),
      indicador      = as.character(indicador),
      mes_referencia = as.character(mes_referencia)
    ) %>%
    select(cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia)
  cli_alert_success("Conectividade Anatel processada.")
} else {
  cli_alert_warning("Arquivo Anatel não encontrado: {.path {caminho_anatel}}")
  df_anatel_long <- tibble()
}

# ------------------------------------------------------------------------------
# 6. EMPILHAMENTO E ENRIQUECIMENTO METADADOS
# ------------------------------------------------------------------------------
df_indicadores_empilhados <- bind_rows(
  df_idhm_long, 
  df_pib_pcap_long, 
  df_pib_total_long,
  df_capag_long,
  df_vab_long,
  df_seguranca_long,
  df_educacao_long,
  df_abrinq_long,
  df_saude_long,
  df_peso_ideal_long,
  df_vulnerabilidade_long,
  df_urbanizacao_long,
  df_ivcad_long,
  df_demografia_long,
  df_rais_long,
  df_cresc_hightec_long,
  df_comex_long,
  df_anatel_long
) %>%
  left_join(
    lista_de_indicadores %>% distinct(nm_indicador, .keep_all = TRUE),
    by = "nm_indicador"
  ) %>%
  filter(!is.na(indicador)) %>%
  mutate(
    nm_municipio = str_remove(nm_municipio, " \\(PE\\)| - [A-Z]{2}$"),
    indicador = case_when(
      # já vem em % (0 a 100): não aplicar a conversão de proporção (0-1) para %
      sigla_indicador == "peso_abaixo_5_10_pct" ~ indicador,
      tipo == "percentual" & suppressWarnings(as.numeric(indicador)) > 0 & suppressWarnings(as.numeric(indicador)) <= 1 ~ as.character(as.numeric(indicador) * 100),
      sigla_indicador %in% c("pib_pct", "pct_agua_rede", "pct_esgoto_rede") ~ as.character(round(as.numeric(indicador), 2)),
      TRUE ~ indicador
    )
  ) %>%
  select(
    cod_municipio,
    nm_municipio,
    ano,
    nm_indicador,
    nm_fantasia,
    indicador,
    mes_referencia,
    sigla_indicador,
    grupo,
    tipo,
    polaridade,
    everything()
  )

cli_alert_success("Indicadores consolidados: {nrow(df_indicadores_empilhados)} registros totais.")

# ------------------------------------------------------------------------------
# 7. CÁLCULO DE RANKINGS E ESTRUTURAÇÃO FINAL
# ------------------------------------------------------------------------------
df_indicadores_final <- df_indicadores_empilhados %>%
  mutate(
    cod_ibge = cod_municipio,
    ibge6    = substr(as.character(cod_municipio), 1, 6)
  ) %>%
  group_by(sigla_indicador, ano) %>%
  mutate(
    val_num_temp = suppressWarnings(as.numeric(indicador)),
    rank_pe      = ifelse(is.na(val_num_temp), NA_integer_, min_rank(desc(val_num_temp)))
  ) %>%
  select(-val_num_temp) %>%
  ungroup()

cli_alert_success("Rankings estaduais calculados com sucesso.")

# ------------------------------------------------------------------------------
# 8. EXPORTAÇÃO LOCAL (RDS & PARQUET)
# ------------------------------------------------------------------------------

# --- 8.1 RDS ---
caminho_rds_out <- file.path(DIR_OUT_AURORA, "df_indicadores_empilhados.rds")
readr::write_rds(df_indicadores_empilhados, caminho_rds_out)
readr::write_rds(df_indicadores_final, file.path(DIR_OUT_AURORA, "df_indicadores_final.rds"))

# Cópia defensiva em raw/ibge
readr::write_rds(df_indicadores_empilhados, file.path(dir_raw_ibge, "df_indicadores_empilhados.rds"))
cli_alert_success("Bases RDS salvas em {.path {DIR_OUT_AURORA}}.")

# --- 8.2 Parquet Oficial dos Apps ---
caminho_parquet_out <- file.path(dir_parquet_out, "df_indicadores.parquet")
arrow::write_parquet(
  df_indicadores_final,
  caminho_parquet_out, 
  compression = "snappy"
)

# Cópia defensiva em raw/indicadores
arrow::write_parquet(
  df_indicadores_final,
  file.path(dir_raw_indicadores, "df_indicadores.parquet"), 
  compression = "snappy"
)
cli_alert_success("Arquivo Parquet oficial salvo em {.path {caminho_parquet_out}}.")

cli_alert_success("Processamento de etl_caracter_mun.R concluído com sucesso!")
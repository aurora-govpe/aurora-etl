# ==============================================================================
# SCRIPT: etl_demografia_rd.R
# Objetivo: Processamento das coortes demográficas de PE agregadas por
#           Região de Desenvolvimento (12 RDs) e cálculo exato dos índices.
# ==============================================================================

library(arrow)
library(dplyr)
library(stringr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE
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

# ------------------------------------------------------------------------------
# 2. DICIONÁRIO TERRITORIAL: MUNICÍPIOS (185) -> RDs (12)
# ------------------------------------------------------------------------------
df_depara_rd <- tribble(
  ~cod_municipio, ~cod_rd, ~nm_rd,
  # 01 - Metropolitana (15)
  "2600054", "01", "Metropolitana", "2601052", "01", "Metropolitana",
  "2602902", "01", "Metropolitana", "2603454", "01", "Metropolitana",
  "2605459", "01", "Metropolitana", "2606804", "01", "Metropolitana",
  "2607208", "01", "Metropolitana", "2607604", "01", "Metropolitana",
  "2607752", "01", "Metropolitana", "2607901", "01", "Metropolitana",
  "2609402", "01", "Metropolitana", "2609600", "01", "Metropolitana",
  "2610707", "01", "Metropolitana", "2611606", "01", "Metropolitana",
  "2613701", "01", "Metropolitana",
  # 02 - Mata Norte (19)
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
  # 03 - Mata Sul (24)
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
  # 04 - Agreste Setentrional (19)
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
  # 05 - Agreste Central (26)
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
  # 06 - Agreste Meridional (26)
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
  # 07 - Sertão do Moxotó (7)
  "2601201", "07", "Sertão do Moxotó", "2602001", "07", "Sertão do Moxotó",
  "2604601", "07", "Sertão do Moxotó", "2606606", "07", "Sertão do Moxotó",
  "2607406", "07", "Sertão do Moxotó", "2609303", "07", "Sertão do Moxotó",
  "2613602", "07", "Sertão do Moxotó",
  # 08 - Sertão do Pajeú (17)
  "2600104", "08", "Sertão do Pajeú", "2602951", "08", "Sertão do Pajeú",
  "2603157", "08", "Sertão do Pajeú", "2603900", "08", "Sertão do Pajeú",
  "2605152", "08", "Sertão do Pajeú", "2605608", "08", "Sertão do Pajeú",
  "2606507", "08", "Sertão do Pajeú", "2607307", "08", "Sertão do Pajeú",
  "2607703", "08", "Sertão do Pajeú", "2611533", "08", "Sertão do Pajeú",
  "2612554", "08", "Sertão do Pajeú", "2612901", "08", "Sertão do Pajeú",
  "2613404", "08", "Sertão do Pajeú", "2613800", "08", "Sertão do Pajeú",
  "2613909", "08", "Sertão do Pajeú", "2614659", "08", "Sertão do Pajeú",
  "2615706", "08", "Sertão do Pajeú",
  # 09 - Sertão de Itaparica (7)
  "2601607", "09", "Sertão de Itaparica", "2603926", "09", "Sertão de Itaparica",
  "2605707", "09", "Sertão de Itaparica", "2608024", "09", "Sertão de Itaparica",
  "2608107", "09", "Sertão de Itaparica", "2611051", "09", "Sertão de Itaparica",
  "2614402", "09", "Sertão de Itaparica",
  # 10 - Sertão Central (8)
  "2604304", "10", "Sertão Central", "2609204", "10", "Sertão Central",
  "2609709", "10", "Sertão Central", "2612208", "10", "Sertão Central",
  "2613354", "10", "Sertão Central", "2614105", "10", "Sertão Central",
  "2615300", "10", "Sertão Central", "2616001", "10", "Sertão Central",
  # 11 - Sertão do Araripe (10)
  "2601102", "11", "Sertão do Araripe", "2602050", "11", "Sertão do Araripe",
  "2605350", "11", "Sertão do Araripe", "2606200", "11", "Sertão do Araripe",
  "2607158", "11", "Sertão do Araripe", "2609451", "11", "Sertão do Araripe",
  "2609907", "11", "Sertão do Araripe", "2612505", "11", "Sertão do Araripe",
  "2613156", "11", "Sertão do Araripe", "2615607", "11", "Sertão do Araripe",
  # 12 - Sertão do São Francisco (7)
  "2600708", "12", "Sertão do São Francisco", "2603058", "12", "Sertão do São Francisco",
  "2604502", "12", "Sertão do São Francisco", "2608750", "12", "Sertão do São Francisco",
  "2609857", "12", "Sertão do São Francisco", "2611101", "12", "Sertão do São Francisco",
  "2612653", "12", "Sertão do São Francisco"
) %>%
  mutate(ibge6 = substr(str_trim(cod_municipio), 1, 6))

# ------------------------------------------------------------------------------
# 3. CARGA E PROCESSAMENTO REGIONAL DAS COORTES
# ------------------------------------------------------------------------------
painel_demografico <- read_parquet(
  file.path(DIR_RAW_AURORA, "indicadores/demografia/painel_demografico_pe.parquet")
)

painel_demografico %>% View()

ano_atual <- as.numeric(format(Sys.Date(), "%Y"))

indicadores_rd_wide <- painel_demografico %>%
  filter(as.numeric(ano) <= ano_atual) %>%
  mutate(ibge6 = substr(str_remove(str_trim(as.character(locality_id)), "\\.0$"), 1, 6)) %>%
  inner_join(df_depara_rd %>% select(ibge6, cod_rd, nm_rd), by = "ibge6") %>%
  group_by(cod_rd, nm_rd, ano) %>%
  summarise(
    pop_total   = sum(pop_projetada_calib, na.rm = TRUE),
    pop_0_14    = sum(pop_projetada_calib[coorte %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    pop_15_64   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", 
                                                        "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", 
                                                        "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos")], na.rm = TRUE),
    pop_65_mais = sum(pop_projetada_calib[str_detect(coorte, "65|70|75|80|85|90|95|100")], na.rm = TRUE),
    pop_60_mais = sum(pop_projetada_calib[str_detect(coorte, "60|65|70|75|80|85|90|95|100")], na.rm = TRUE),
    pop_80_mais = sum(pop_projetada_calib[str_detect(coorte, "80|85|90|95|100")], na.rm = TRUE),
    pop_15_24   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos")], na.rm = TRUE),
    pop_20_29   = sum(pop_projetada_calib[coorte %in% c("20 a 24 anos", "25 a 29 anos")], na.rm = TRUE),
    pop_50_59   = sum(pop_projetada_calib[coorte %in% c("50 a 54 anos", "55 a 59 anos")], na.rm = TRUE),
    pop_55_64   = sum(pop_projetada_calib[coorte %in% c("55 a 59 anos", "60 a 64 anos")], na.rm = TRUE),
    pop_15_29   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Subdimensão 1.1: Estrutura Etária & Dependência
    rd_pop             = ((pop_0_14 + pop_65_mais) / pop_15_64) * 100,
    rd_jovem           = (pop_0_14 / pop_15_64) * 100,
    rd_idosa           = (pop_65_mais / pop_15_64) * 100,
    pct_pia            = (pop_15_64 / pop_total) * 100,
    # Subdimensão 1.2: Envelhecimento Populacional
    ind_envelhecimento = (pop_60_mais / pop_0_14) * 100,
    pct_longevidade    = (pop_80_mais / pop_60_mais) * 100,
    ind_ir_pia         = (pop_20_29 / pop_50_59) * 100,
    # Subdimensão 1.5: Dinâmica da Juventude
    ind_renov_trabalho = (pop_15_24 / pop_55_64) * 100,
    pct_jovem          = (pop_15_29 / pop_total) * 100
  )

indicadores_demograficos_rd_pe <- indicadores_rd_wide %>%
  select(
    cod_rd, nm_rd, ano,
    pop_total, rd_pop, rd_jovem, rd_idosa, pct_pia,
    ind_envelhecimento, pct_longevidade, ind_ir_pia,
    ind_renov_trabalho, pct_jovem
  ) %>%
  pivot_longer(
    cols = -c(cod_rd, nm_rd, ano),
    names_to = "sigla_indicador",
    values_to = "valor"
  ) %>%
  mutate(
    grupo = "demografia",
    nm_indicador = case_when(
      sigla_indicador == "pop_total"          ~ "População Total",
      sigla_indicador == "rd_pop"             ~ "Razão de Dependência Total",
      sigla_indicador == "rd_jovem"           ~ "Razão de Dependência Jovem",
      sigla_indicador == "rd_idosa"           ~ "Razão de Dependência Idosa",
      sigla_indicador == "pct_pia"            ~ "Peso da População em Idade Ativa",
      sigla_indicador == "ind_envelhecimento" ~ "Índice de Envelhecimento",
      sigla_indicador == "pct_longevidade"    ~ "Taxa de Superlongevidade",
      sigla_indicador == "ind_ir_pia"         ~ "Índice de Renovação da PIA",
      sigla_indicador == "ind_renov_trabalho" ~ "Razão de Renovação do Trabalho",
      sigla_indicador == "pct_jovem"          ~ "Peso Relativo da Juventude"
    )
  ) %>%
  select(cod_rd, nm_rd, ano, grupo, sigla_indicador, nm_indicador, valor)

# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO DO PARQUET DEMOGRÁFICO REGIONAL
# ------------------------------------------------------------------------------
dir.create(file.path(DIR_RAW_AURORA, "indicadores/demografia"), recursive = TRUE, showWarnings = FALSE)
path_saida <- file.path(DIR_RAW_AURORA, "indicadores/demografia/indicadores_demograficos_rd_pe.parquet")

write_parquet(indicadores_demograficos_rd_pe, path_saida)
message(sprintf("--> Indicadores demográficos por RD salvos em '%s'", path_saida))
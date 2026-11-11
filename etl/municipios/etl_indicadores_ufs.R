# ==============================================================================
# SCRIPT: etl_indicadores_ufs.R
# Objetivo: Extração, tratamento e consolidação multi-fonte de indicadores
#           estaduais do Brasil (IDEB, Educação Infantil, Dependência Adm,
#           CNUC, IBGE, Segurança Pública, Saúde, Escolarização, MapBiomas,
#           Energia, Saneamento SINISA e Agricultura PAM/IBGE).
#           Enriquecimento com metadados e exportação (Parquet com Snappy e RDS).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(arrow)
library(readxl)
library(janitor)
library(tidyverse)
library(readr)
library(cli)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (ESTRATÉGIA MODULAR AURORA)
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

# Fallbacks defensivos e sincronização de diretórios
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

DIR_DADOS <- DIR_OUT_AURORA

# Estrutura de saída
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir.create(DIR_OUT_AURORA,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. MAPEAMENTO DE CAMINHOS DE ENTRADA (DIR_RAW_AURORA)
# ------------------------------------------------------------------------------
caminho_ideb            <- file.path(DIR_RAW_AURORA, "educacao/IDEB/divulgacao_regioes_ufs_ideb_2025.xlsx")
caminho_creches         <- file.path(DIR_RAW_AURORA, "educacao/infantil/Taxa bruta de matrículas em creches.xlsx")
caminho_pre_escola      <- file.path(DIR_RAW_AURORA, "educacao/infantil/Taxa bruta de matrículas em pré-escola.xlsx")
caminho_liq_creches     <- file.path(DIR_RAW_AURORA, "educacao/infantil/Taxa líquida de matrículas em creches.xlsx")
caminho_liq_pre_escola  <- file.path(DIR_RAW_AURORA, "educacao/infantil/Taxa líquida de matrículas em pré-escola.xlsx")
caminho_abandono        <- file.path(DIR_RAW_AURORA, "educacao/Indicadores Educacionais/Taxa de abandono no ensino médio segundo dependência administrativa.xlsx")
caminho_distorcao       <- file.path(DIR_RAW_AURORA, "educacao/Indicadores Educacionais/Taxa de distorção idade-série no ensino médio segundo dependência administrativa.xlsx")
caminho_escolarizacao   <- file.path(DIR_RAW_AURORA, "educacao/EM/Taxa de escolarização bruta e líquida no ensino médio.xlsx")
caminho_cnuc            <- file.path(DIR_RAW_AURORA, "meio ambiente/cnuc_2026_07.csv")
caminho_ibge            <- file.path(DIR_RAW_AURORA, "ibge/df_indicadores_ibge_ufs_br.parquet")

caminho_consultas       <- file.path(DIR_RAW_AURORA, "saude/Consultas Médicas por tipo de gestão.csv")
caminho_exames          <- file.path(DIR_RAW_AURORA, "saude/Exames (Procedimentos com Finalidade Diagnóstica) por tipo de gestão.csv")
caminho_cirurgias       <- file.path(DIR_RAW_AURORA, "saude/Procedimentos Cirúrgicos por tipo de gestão.csv")

caminho_indicador       <- file.path(DIR_RAW_AURORA, "indicadores/lista de indicadores.xlsx")
caminho_seguranca       <- file.path(DIR_RAW_AURORA, "seguranca/df_seguranca_publica_forum_nacional.xlsx")
caminho_mapbiomas       <- file.path(DIR_RAW_AURORA, "meio ambiente/indicadores_mapbiomas.parquet")
caminho_energia         <- file.path(DIR_RAW_AURORA, "energia/dados_potencia_energia_uf.xlsx")
caminho_agua            <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_agua_sinisa.xlsx")
caminho_esgoto          <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_esgoto_sinisa.xlsx")
caminho_agricultura     <- file.path(DIR_RAW_AURORA, "agricultura/dados_pam_uf_2020_2025.xlsx")
caminho_suape           <- file.path(DIR_RAW_AURORA, "aeroportuaria/porto_suape_carga.xlsx")

# ------------------------------------------------------------------------------
# 4. DICIONÁRIOS E FUNÇÕES AUXILIARES
# ------------------------------------------------------------------------------
if (!file.exists(caminho_indicador)) {
  cli_abort("Arquivo de lista de indicadores não encontrado em: {.path {caminho_indicador}}")
}

tb_ufs           <- read_excel(caminho_indicador, sheet = "dic_UFs") %>%
  mutate(cod_uf = as.character(cod_uf))

meta_indicadores <- read_excel(caminho_indicador, sheet = "UFs")

padronizar_rede <- function(x) {
  case_when(
    str_detect(x, regex("^Total|^Taxa de", ignore_case = TRUE)) ~ "total",
    str_detect(x, regex("^P[uú]blica", ignore_case = TRUE))      ~ "publica",
    str_detect(x, regex("^Estadual", ignore_case = TRUE))        ~ "estadual",
    str_detect(x, regex("^Municipal", ignore_case = TRUE))       ~ "municipal",
    str_detect(x, regex("^Privada", ignore_case = TRUE))         ~ "privada",
    str_detect(x, regex("^Federal", ignore_case = TRUE))         ~ "federal",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------------------------
# 5. EXTRAÇÃO E TRATAMENTO TEMÁTICO
# ------------------------------------------------------------------------------

## 5.1 IDEB (Estados) ----
extrair_ideb_uf <- function(nome_aba, etapa) {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_ideb, sheet = nome_aba, skip = 9, .name_repair = "unique")
    )
  )
  names(df_raw)[1:2] <- c("nm_uf_orig", "rede")
  
  df_raw %>%
    inner_join(tb_ufs, by = "nm_uf_orig") %>%
    mutate(rede_limpa = padronizar_rede(rede)) %>%
    filter(!is.na(rede_limpa)) %>%
    mutate(
      sigla_indicador = paste0("ideb_", etapa, "_", rede_limpa),
      mes_referencia  = NA_character_
    ) %>%
    select(cod_uf, sg_uf, nm_uf, sigla_indicador, mes_referencia, starts_with("VL_OBSERVADO_")) %>%
    mutate(across(starts_with("VL_OBSERVADO_"), as.character)) %>%
    pivot_longer(
      cols         = starts_with("VL_OBSERVADO_"), 
      names_to     = "ano", 
      names_prefix = "VL_OBSERVADO_", 
      values_to    = "indicador"
    ) %>%
    mutate(ano = as.numeric(ano)) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA))
}

df_ideb_ufs <- if (file.exists(caminho_ideb)) {
  bind_rows(
    extrair_ideb_uf("UF e Regiões (AI)", "5ano"),
    extrair_ideb_uf("UF e Regiões (AF)", "9ano"),
    extrair_ideb_uf("UF e Regiões (EM)", "3ano")
  )
} else {
  cli_alert_warning("Arquivo IDEB ausente em {.path {caminho_ideb}}")
  tibble()
}
cli_alert_success("IDEB estadual processado.")

## 5.2 Educação Infantil (Estados) ----
extrair_infantil_uf <- function(caminho_arquivo, prefixo_sigla) {
  if (!file.exists(caminho_arquivo)) return(tibble())
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_arquivo, sheet = 2, skip = 2, col_types = "text")
    )
  )
  
  df_raw %>%
    rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
    rename(cod_uf = `Código IBGE`) %>%
    mutate(cod_uf = as.character(cod_uf)) %>%
    inner_join(tb_ufs, by = "cod_uf") %>%
    select(cod_uf, sg_uf, nm_uf, matches("^[0-9]{4}")) %>%
    mutate(across(matches("^[0-9]{4}"), as.character)) %>%
    pivot_longer(
      cols      = matches("^[0-9]{4}"), 
      names_to  = "col_orig", 
      values_to = "indicador"
    ) %>%
    mutate(
      ano             = as.numeric(str_extract(col_orig, "^[0-9]{4}")),
      metrica         = if_else(str_detect(col_orig, regex("absoluto", ignore_case = TRUE)), "abs", "pct"),
      sigla_indicador = paste0(prefixo_sigla, "_", metrica),
      mes_referencia  = NA_character_,
      indicador       = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_infantil_ufs <- bind_rows(
  extrair_infantil_uf(caminho_creches, "tx_creche"),
  extrair_infantil_uf(caminho_pre_escola, "tx_pre_escola"),
  extrair_infantil_uf(caminho_liq_creches, "tx_liq_creche"),
  extrair_infantil_uf(caminho_liq_pre_escola, "tx_liq_pre_escola")
)
cli_alert_success("Educação Infantil processada.")

## 5.3 Dependência Administrativa (Estados) ----
extrair_dependencia_uf <- function(caminho_arquivo, prefixo_sigla) {
  if (!file.exists(caminho_arquivo)) return(tibble())
  
  abas <- excel_sheets(caminho_arquivo)
  abas_dados <- abas[abas != "Índice"]
  
  map_dfr(abas_dados, function(aba) {
    df_raw <- suppressWarnings(
      suppressMessages(
        read_excel(caminho_arquivo, sheet = aba, skip = 2, col_types = "text")
      )
    )
    
    nome_rede_aba <- str_remove(aba, "^[0-9]+\\s*-\\s*") %>% str_trim()
    rede_limpa <- padronizar_rede(nome_rede_aba)
    
    df_raw %>%
      rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
      rename(cod_uf = `Código IBGE`) %>%
      mutate(cod_uf = as.character(cod_uf)) %>%
      inner_join(tb_ufs, by = "cod_uf") %>%
      select(cod_uf, sg_uf, nm_uf, matches("^[0-9]{4}$")) %>%
      mutate(across(matches("^[0-9]{4}$"), as.character)) %>%
      pivot_longer(
        cols      = matches("^[0-9]{4}$"), 
        names_to  = "ano", 
        values_to = "indicador"
      ) %>%
      mutate(
        ano             = as.numeric(ano),
        sigla_indicador = paste0(prefixo_sigla, "_", rede_limpa),
        mes_referencia  = NA_character_,
        indicador       = as.character(indicador)
      ) %>%
      filter(!indicador %in% c("-", "ND", "nan", NA)) %>%
      select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  })
}

df_dependencia_ufs <- bind_rows(
  extrair_dependencia_uf(caminho_abandono, "aba_em"),
  extrair_dependencia_uf(caminho_distorcao, "tdi_em")
)
cli_alert_success("Indicadores educacionais por dependência processados.")

## 5.4 Unidades de Conservação (CNUC) ----
extrair_cnuc_uf <- function() {
  if (!file.exists(caminho_cnuc)) {
    cli_alert_warning("Arquivo CNUC ausente em {.path {caminho_cnuc}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    read_delim(
      caminho_cnuc, 
      delim     = ";", 
      locale    = locale(encoding = "latin1"),
      col_types = cols(.default = col_character())
    )
  )
  
  ano_limite <- lubridate::year(Sys.Date())
  
  df_limpo <- df_raw %>%
    select(
      sg_uf       = UF,
      ano_criacao = `Ano de Criação`,
      esfera      = `Esfera Administrativa`,
      grupo       = Grupo,
      area_raw    = `Área soma biomas`
    ) %>%
    separate_rows(sg_uf, sep = ",\\s*") %>%
    mutate(
      sg_uf       = str_trim(sg_uf),
      ano_criacao = as.numeric(ano_criacao),
      area_ha     = area_raw %>% 
        str_remove_all("\\.") %>% 
        str_replace(",", ".") %>% 
        as.numeric(),
      area_ha     = coalesce(area_ha, 0)
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf), by = "sg_uf") %>%
    filter(!is.na(ano_criacao))
  
  df_longo <- df_limpo %>%
    mutate(
      esfera_cat = case_when(
        str_detect(esfera, regex("^Fed", ignore_case = TRUE)) ~ "fed",
        str_detect(esfera, regex("^Est", ignore_case = TRUE)) ~ "est",
        str_detect(esfera, regex("^Mun", ignore_case = TRUE)) ~ "mun",
        TRUE ~ NA_character_
      ),
      grupo_cat  = case_when(
        str_detect(grupo, regex("Integral", ignore_case = TRUE)) ~ "pi",
        str_detect(grupo, regex("Sustent", ignore_case = TRUE))  ~ "us",
        TRUE ~ NA_character_
      )
    )
  
  base_total  <- df_longo %>% mutate(recorte = "total")
  base_esfera <- df_longo %>% filter(!is.na(esfera_cat)) %>% mutate(recorte = esfera_cat)
  base_grupo  <- df_longo %>% filter(!is.na(grupo_cat)) %>% mutate(recorte = grupo_cat)
  
  bind_rows(base_total, base_esfera, base_grupo) %>%
    group_by(cod_uf, sg_uf, nm_uf, recorte, ano = ano_criacao) %>%
    summarise(
      novas_ucs = n(),
      nova_area = sum(area_ha, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    complete(
      nesting(cod_uf, sg_uf, nm_uf, recorte),
      ano = min(ano):ano_limite,
      fill = list(novas_ucs = 0, nova_area = 0)
    ) %>%
    arrange(cod_uf, recorte, ano) %>%
    group_by(cod_uf, sg_uf, nm_uf, recorte) %>%
    mutate(
      qtd_uc  = cumsum(novas_ucs),
      area_uc = cumsum(nova_area)
    ) %>%
    ungroup() %>%
    pivot_longer(
      cols      = c(qtd_uc, area_uc),
      names_to  = "metrica",
      values_to = "indicador"
    ) %>%
    mutate(
      sigla_indicador = paste0(metrica, "_", recorte),
      mes_referencia  = NA_character_,
      indicador       = as.character(round(indicador, 2))
    ) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_cnuc_ufs <- extrair_cnuc_uf()
cli_alert_success("Unidades de Conservação (CNUC) processadas.")

## 5.5 Indicadores Demográficos e Econômicos (IBGE) ----
extrair_ibge_uf <- function() {
  caminho_real_ibge <- if (file.exists(caminho_ibge)) {
    caminho_ibge
  } else if (file.exists(str_replace(caminho_ibge, "\\.parquet$", ".xlsx"))) {
    str_replace(caminho_ibge, "\\.parquet$", ".xlsx")
  } else {
    NULL
  }
  
  if (is.null(caminho_real_ibge)) {
    cli_alert_warning("Arquivo IBGE ausente em {.path {caminho_ibge}}")
    return(tibble())
  }
  
  df_raw <- if (str_ends(caminho_real_ibge, "\\.parquet$")) {
    arrow::read_parquet(caminho_real_ibge)
  } else {
    read_excel(caminho_real_ibge)
  }
  
  df_raw %>%
    mutate(cod_uf = as.character(cod_ibge)) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    select(-any_of(c("nivel_geo", "nome_local", "cod_ibge"))) %>%
    distinct() %>%
    pivot_longer(
      cols      = -c(cod_uf, sg_uf, nm_uf, ano),
      names_to  = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano            = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador      = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_ibge_ufs <- extrair_ibge_uf()
cli_alert_success("Indicadores IBGE estaduais processados.")

## 5.6 Segurança Pública ----
if (file.exists(caminho_seguranca)) {
  # MVI
  df_mvi_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "mvi"))) %>%
    rename(sg_uf = sigla_uf, mvi = n_ab_mvi) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      mvi    = as.character(mvi) %>% str_remove_all("\\."),
      tx_mvi = as.character(tx_mvi) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(mvi, tx_mvi), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # FEMINICIDIO
  df_feminicidio_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "feminicidio"))) %>%
    rename(sg_uf = sigla_uf) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      feminicidio    = as.character(feminicidio) %>% str_remove_all("\\."),
      tx_feminicidio = as.character(tx_feminicidio) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(feminicidio, tx_feminicidio), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # CVP (Roubo Total)
  df_cvp_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "cvp"))) %>%
    rename(sg_uf = sigla_uf, cvp = n_ab_roubo_tot, tx_cvp = tx_roubo_tot) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      n_ab_cvp    = as.character(cvp) %>% str_remove_all("\\."),
      tx_cvp = as.character(tx_cvp) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(n_ab_cvp, tx_cvp), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # População carcerária
  df_carceraria_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "populacao_presa"))) %>%
    rename(sg_uf = sigla_uf) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      pop_carceraria = as.character(n_ab_presos_sistema_penitenciario) %>% str_remove_all("\\."),
      pop_custodia   = as.character(n_ab_sob_custodia) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(pop_carceraria, pop_custodia), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # População Socioeducanda
  df_socioeducanda_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "socioeducandos"))) %>%
    rename(sg_uf = sigla_uf) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      pop_socioeducanda = as.character(n_ab_socioeducandos_fechado) %>% str_remove_all("\\."),
      tx_socioeducanda  = as.character(tx_socioeducandos_fechado) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(pop_socioeducanda, tx_socioeducanda), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # RFV
  df_rfv_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "rfv"))) %>%
    rename(sg_uf = sigla_uf, rfv = n_ab_rfv) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      rfv    = as.character(rfv) %>% str_remove_all("\\."),
      tx_rfv = as.character(tx_rfv) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(rfv, tx_rfv), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  
  # Celulares
  df_celulares_ufs <- suppressWarnings(suppressMessages(read_excel(caminho_seguranca, sheet = "celulares"))) %>%
    rename(sg_uf = sigla_uf, rf_celular = qtd_rf_celulares, tx_rfcelular = taxa_rf_celulares) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      rf_celular   = as.character(rf_celular) %>% str_remove_all("\\."),
      tx_rfcelular = as.character(tx_rfcelular) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(cols = c(rf_celular, tx_rfcelular), names_to = "sigla_indicador", values_to = "indicador") %>%
    mutate(ano = as.numeric(ano), mes_referencia = NA_character_, indicador = as.character(indicador)) %>%
    filter(ano >= 2020, !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
} else {
  cli_alert_warning("Arquivo de Segurança ausente em {.path {caminho_seguranca}}")
  df_mvi_ufs <- df_feminicidio_ufs <- df_rfv_ufs <- df_celulares_ufs <- df_cvp_ufs <- df_carceraria_ufs <- df_socioeducanda_ufs <- tibble()
}
cli_alert_success("Segurança Pública processada.")

## 5.7 Saúde por Tipo de Gestão ----
extrair_saude_gestao_uf <- function(caminho_arquivo, prefixo_sigla) {
  if (!file.exists(caminho_arquivo)) return(tibble())
  
  df_raw <- suppressWarnings(
    read_csv(caminho_arquivo, col_types = cols(.default = col_character()))
  ) %>%
    rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
    clean_names()
  
  nome_col_valor <- names(df_raw)[ncol(df_raw)]
  
  df_base <- df_raw %>%
    rename(nm_uf = uf) %>%
    mutate(
      ano       = as.numeric(ano),
      gestao    = str_to_lower(str_trim(gestao)),
      valor_num = as.numeric(str_replace(str_trim(.data[[nome_col_valor]]), ",", "."))
    ) %>%
    filter(!is.na(valor_num)) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "nm_uf")
  
  df_gestao <- df_base %>%
    mutate(sigla_indicador = paste0(prefixo_sigla, "_", gestao)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, valor_num, sigla_indicador)
  
  df_total <- df_base %>%
    group_by(cod_uf, sg_uf, nm_uf, ano) %>%
    summarise(valor_num = sum(valor_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(sigla_indicador = paste0(prefixo_sigla, "_total")) %>%
    select(cod_uf, sg_uf, nm_uf, ano, valor_num, sigla_indicador)
  
  bind_rows(df_gestao, df_total) %>%
    mutate(
      mes_referencia = NA_character_,
      indicador      = as.character(valor_num)
    ) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_saude_gestao_ufs <- bind_rows(
  extrair_saude_gestao_uf(caminho_consultas, "consultas_med"),
  extrair_saude_gestao_uf(caminho_exames, "exames"),
  extrair_saude_gestao_uf(caminho_cirurgias, "proc_cirurgicos")
)
cli_alert_success("Saúde por tipo de gestão processada.")

## 5.8 Escolarização no Ensino Médio ----
extrair_escolarizacao_em_uf <- function() {
  if (!file.exists(caminho_escolarizacao)) {
    cli_alert_warning("Arquivo de escolarização ausente em {.path {caminho_escolarizacao}}")
    return(tibble())
  }
  
  map2_dfr(
    c(2, 3), # Aba 2: Taxa Bruta | Aba 3: Taxa Líquida
    c("tx_esc_bruta_em", "tx_esc_liq_em"),
    function(aba_num, sigla) {
      df_raw <- suppressWarnings(
        suppressMessages(
          read_excel(caminho_escolarizacao, sheet = aba_num, skip = 2, col_types = "text")
        )
      )
      
      df_raw %>%
        rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
        rename(cod_uf = `Código IBGE`) %>%
        mutate(cod_uf = as.character(cod_uf)) %>%
        inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
        select(cod_uf, sg_uf, nm_uf, matches("^[0-9]{4}$")) %>%
        pivot_longer(
          cols      = matches("^[0-9]{4}$"), 
          names_to  = "ano", 
          values_to = "indicador"
        ) %>%
        mutate(
          ano             = as.numeric(ano),
          sigla_indicador = sigla,
          mes_referencia  = NA_character_,
          indicador       = as.character(indicador)
        ) %>%
        filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
        select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
    }
  )
}

df_escolarizacao_ufs <- extrair_escolarizacao_em_uf()
cli_alert_success("Taxa de escolarização no Ensino Médio processada.")

## 5.9 MapBiomas (Estados) ----
extrair_mapbiomas_uf <- function() {
  caminho_real_mapbiomas <- if (file.exists(caminho_mapbiomas)) {
    caminho_mapbiomas
  } else if (file.exists(str_replace(caminho_mapbiomas, "\\.parquet$", ".xlsx"))) {
    str_replace(caminho_mapbiomas, "\\.parquet$", ".xlsx")
  } else {
    NULL
  }
  
  if (is.null(caminho_real_mapbiomas)) {
    cli_alert_warning("Arquivo MapBiomas ausente em {.path {caminho_mapbiomas}}")
    return(tibble())
  }
  
  df_raw <- if (str_ends(caminho_real_mapbiomas, "\\.parquet$")) {
    arrow::read_parquet(caminho_real_mapbiomas)
  } else {
    read_excel(caminho_real_mapbiomas)
  }
  
  df_raw %>%
    clean_names() %>%
    rename(nm_uf = state) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "nm_uf") %>%
    select(
      cod_uf,
      sg_uf,
      nm_uf,
      ano,
      desmatamento_ha,
      recuperacao_ha,
      area_total_ha,
      desmatamento_pct,
      var_desmatamento_pct,
      recuperacao_pct
    ) %>%
    pivot_longer(
      cols      = -c(cod_uf, sg_uf, nm_uf, ano),
      names_to  = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano            = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador      = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_mapbiomas_ufs <- extrair_mapbiomas_uf()
cli_alert_success("MapBiomas processado.")

## 5.10 Energia (Eólica e Solar) ----
extrair_energia_uf <- function() {
  caminho_real_energia <- if (file.exists(caminho_energia)) {
    caminho_energia
  } else if (file.exists(str_replace(caminho_energia, "\\.xlsx$", ".csv"))) {
    str_replace(caminho_energia, "\\.xlsx$", ".csv")
  } else {
    NULL
  }
  
  if (is.null(caminho_real_energia)) {
    cli_alert_warning("Arquivo de Energia ausente em {.path {caminho_energia}}")
    return(tibble())
  }
  
  df_raw <- if (str_ends(caminho_real_energia, "\\.csv$")) {
    read_delim(caminho_real_energia, delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."), col_types = cols(.default = col_character()))
  } else {
    read_excel(caminho_real_energia, col_types = "text")
  }
  
  df_raw %>%
    clean_names() %>%
    rename(sg_uf = uf) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      sigla_indicador = case_when(
        str_detect(energia, regex("E[oó]lica", ignore_case = TRUE)) ~ "qtd_energia_eolica",
        str_detect(energia, regex("Solar", ignore_case = TRUE))  ~ "qtd_energia_solar",
        TRUE ~ NA_character_
      ),
      valor_num = as.numeric(potencia_fiscalizada_k_w),
      # %>%
      #   str_remove_all("\\.") %>%
      #   str_replace(",", ".") %>%
      #   as.numeric(),
      ano            = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador      = as.character(valor_num)
    ) %>%
    filter(!is.na(sigla_indicador), !is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_energia_ufs <- extrair_energia_uf()
cli_alert_success("Energia (Eólica e Solar) processada.")

## 5.11 Abastecimento de Água (SINISA) ----
extrair_agua_sinisa_uf <- function() {
  if (!file.exists(caminho_agua)) {
    cli_alert_warning("Arquivo do SINISA Água ausente em {.path {caminho_agua}}")
    return(tibble())
  }
  
  abas <- excel_sheets(caminho_agua)
  aba_alvo <- abas[str_detect(abas, regex("^estados?$", ignore_case = TRUE))]
  if (length(aba_alvo) == 0) aba_alvo <- abas[1]
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_agua, sheet = aba_alvo, col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    rename(
      cod_uf = cod_ibge,
      ano    = ano_referencia
    ) %>%
    mutate(
      cod_uf = as.character(str_trim(cod_uf)),
      ano    = as.numeric(ano)
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    select(cod_uf, sg_uf, nm_uf, ano, everything()) %>%
    pivot_longer(
      cols      = c(iag0001, 
                    iag0002, 
                    iag0003, 
                    iag0004, 
                    iag0005, 
                    iag0006, 
                    iag2006, 
                    iag2013, 
                    ifa1001, 
                    ifa2002
      ),
      names_to  = "metrica_orig",
      values_to = "indicador_raw"
    ) %>%
    mutate(
      sigla_indicador = case_when(
        metrica_orig == "iag0001" ~ "abastecimento_pop_agua_pct",
        metrica_orig == "iag0002" ~ "abastecimento_pop_urbana_agua_pct",
        metrica_orig == "iag0003" ~ "abastecimento_pop_rural_agua_pct",
        metrica_orig == "iag0004" ~ "abastecimento_dom_agua_pct",
        metrica_orig == "iag0005" ~ "abastecimento_dom_urbana_agua_pct",
        metrica_orig == "iag0006" ~ "abastecimento_dom_rural_agua_pct",
        metrica_orig == "iag2006" ~ "consumo_total_medio_agua",
        metrica_orig == "iag2013" ~ "perdas_distribuicao_agua",
        metrica_orig == "ifa1001" ~ "tarifa_media_agua",
        metrica_orig == "ifa2002" ~ "custo_medio_agua",
        TRUE ~ NA_character_
      ),
      valor_num = indicador_raw %>%
        str_remove_all("\\.") %>%
        str_replace(",", ".") %>%
        as.numeric(),
      mes_referencia = NA_character_,
      indicador      = as.character(valor_num)
    ) %>%
    filter(!is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_agua_ufs <- extrair_agua_sinisa_uf()
cli_alert_success("Abastecimento de Água (Total e Urbano) processado.")

## 5.12 Esgoto (SINISA) ----
extrair_esgoto_sinisa_uf <- function() {
  if (!file.exists(caminho_esgoto)) {
    cli_alert_warning("Arquivo do SINISA Esgoto ausente em {.path {caminho_esgoto}}")
    return(tibble())
  }
  
  abas <- excel_sheets(caminho_esgoto)
  aba_alvo <- abas[str_detect(abas, regex("^estados?$", ignore_case = TRUE))]
  if (length(aba_alvo) == 0) aba_alvo <- abas[1]
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_esgoto, sheet = aba_alvo, col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    rename(
      cod_uf = cod_ibge,
      ano    = ano_referencia
    ) %>%
    mutate(
      cod_uf = as.character(str_trim(cod_uf)),
      ano    = as.numeric(ano)
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    select(cod_uf, sg_uf, nm_uf, ano, everything()) %>%
    pivot_longer(
      cols      = c(ies0001, 
                    ies0002, 
                    ies0003, 
                    ies0004, 
                    ies0005, 
                    ies0006, 
                    ife1001, 
                    ife2002, 
                    ies2003, 
                    ies2004
      ),
      names_to  = "metrica_orig",
      values_to = "indicador_raw"
    ) %>%
    mutate(
      sigla_indicador = case_when(
        metrica_orig == "ies0001" ~ "abastecimento_pop_esgoto_pct",
        metrica_orig == "ies0002" ~ "abastecimento_pop_urbana_esgoto_pct",
        metrica_orig == "ies0003" ~ "abastecimento_pop_rural_esgoto_pct",
        metrica_orig == "ies0004" ~ "abastecimento_dom_esgoto_pct",
        metrica_orig == "ies0005" ~ "abastecimento_dom_urbana_esgoto_pct",
        metrica_orig == "ies0006" ~ "abastecimento_dom_rural_esgoto_pct",
        metrica_orig == "ies2003" ~ "esgoto_tratado_agua_consumida",
        metrica_orig == "ies2004" ~ "esgoto_tratado_coletado",
        metrica_orig == "ife1001" ~ "tarifa_media_esgoto",
        metrica_orig == "ife2002" ~ "custo_medio_esgoto",
        TRUE ~ NA_character_
      ),
      valor_num = indicador_raw %>%
        str_remove_all("\\.") %>%
        str_replace(",", ".") %>%
        as.numeric(),
      mes_referencia = NA_character_,
      indicador      = as.character(valor_num)
    ) %>%
    filter(!is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_esgoto_ufs <- extrair_esgoto_sinisa_uf()
cli_alert_success("Esgoto (Total e Urbano) processado.")

## 5.13 Agricultura PAM (IBGE) ----
extrair_agricultura_uf <- function() {
  if (!file.exists(caminho_agricultura)) {
    cli_alert_warning("Arquivo de Agricultura ausente em {.path {caminho_agricultura}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_agricultura, col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    mutate(
      cod_uf    = as.character(str_trim(cod_uf)),
      ano       = as.numeric(ano),
      valor_num = as.numeric(str_replace_all(valor_producao_hec_colhido, ",", "."))
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf) %>% distinct(), by = "cod_uf") %>%
    mutate(
      sigla_indicador = "valor_producao_hec_colhido",
      mes_referencia  = NA_character_,
      indicador       = as.character(valor_num)
    ) %>%
    filter(!is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, ano, nm_uf, indicador, mes_referencia, sigla_indicador)
}

df_agricultura_ufs <- extrair_agricultura_uf()
cli_alert_success("Agricultura (Valor da Produção por Hectare Colhido) processada.")


## 5.14 Suape ----
extrair_suape_uf <- function() {
  if (!file.exists(caminho_suape)) {
    cli_alert_warning("Arquivo de Suape ausente em {.path {caminho_suape}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_suape, col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    filter(indicador_base == "Porto de Suape - Carga bruta total") %>%
    mutate(
      cod_uf    = as.character(str_trim(cod_uf)),
      ano       = as.numeric(ano),
      valor_num = as.numeric(valor)
      # %>%
      #   str_remove_all("\\.") %>%
      #   str_replace(",", ".") %>%
      #   as.numeric()
    ) %>%
    filter(!is.na(valor_num)) %>%
    # Agrupamento por UF e Ano para somar os valores (ex: dados mensais ou por operação)
    group_by(cod_uf, ano) %>%
    summarise(
      valor_total = sum(valor_num, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    mutate(
      sigla_indicador = "carga_bruta_suape",
      mes_referencia  = NA_character_,
      indicador       = as.character(round(valor_total, 2))
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_suape_ufs <- extrair_suape_uf()
cli_alert_success("Suape (Carga Bruta) processada.")

# ------------------------------------------------------------------------------
# 6. CONSOLIDAÇÃO FINAL E ENRIQUECIMENTO DE METADADOS
# ------------------------------------------------------------------------------
cli_alert_info("Consolidando bases e cruzando dicionário de indicadores...")

df_indicadores_ufs <- bind_rows(
  df_ideb_ufs, 
  df_infantil_ufs, 
  df_dependencia_ufs, 
  df_cnuc_ufs,
  df_ibge_ufs,
  df_mvi_ufs,
  df_feminicidio_ufs,
  df_rfv_ufs,
  df_celulares_ufs,
  df_cvp_ufs,
  df_carceraria_ufs,
  df_socioeducanda_ufs,
  df_saude_gestao_ufs,
  df_escolarizacao_ufs,
  df_mapbiomas_ufs,
  df_energia_ufs,
  df_agua_ufs,
  df_esgoto_ufs,
  df_agricultura_ufs,
  df_suape_ufs,
) %>%
  left_join(meta_indicadores, by = "sigla_indicador") %>%
  select(
    cod_uf,
    sg_uf,
    nm_uf,
    ano,
    nm_indicador,
    nm_fantasia,
    indicador,
    mes_referencia,
    sigla_indicador,
    grupo,
    tipo,
    polaridade
  ) %>%
  arrange(sigla_indicador, cod_uf, ano)

cli_alert_success("Base consolidada com {nrow(df_indicadores_ufs)} registros totais.")

# ------------------------------------------------------------------------------
# 7. EXPORTAÇÃO UNIFICADA (PARQUET & RDS)
# ------------------------------------------------------------------------------
caminho_parquet_sub  <- file.path(dir_parquet_out, "df_indicadores_ufs.parquet")
caminho_parquet_root <- file.path(DIR_OUT_AURORA, "df_indicadores_ufs.parquet")
caminho_rds_root     <- file.path(DIR_OUT_AURORA, "df_indicadores_ufs.rds")

# Gravação Parquet (Snappy)
arrow::write_parquet(df_indicadores_ufs, caminho_parquet_sub,  compression = "snappy")
arrow::write_parquet(df_indicadores_ufs, caminho_parquet_root, compression = "snappy")

# Gravação RDS (Backup)
readr::write_rds(df_indicadores_ufs, caminho_rds_root)

cli_alert_success("Arquivos exportados com sucesso:")
cli_alert_info("Parquet: {.path {caminho_parquet_sub}}")
cli_alert_info("RDS:     {.path {caminho_rds_root}}")

df_indicadores_ufs %>% View()

# ==============================================================================
# SCRIPT: etl_indicadores_ufs.R
# Objetivo: Extração, tratamento e consolidação multi-fonte de indicadores
#           estaduais do Brasil (IDEB, Educação Infantil, Dependência Adm,
#           CNUC, IBGE, Segurança Pública, Saúde, Escolarização, MapBiomas,
#           Energia, Saneamento SINISA, Agricultura PAM/IBGE, Suape,
#           Aeroportos e Déficit Habitacional).
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
library(jsonlite)
library(googledrive)

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
# 4. MAPEAMENTO DE CAMINHOS DE ENTRADA (DIR_RAW_AURORA)
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

# caminho_indicador       <- file.path(DIR_RAW_AURORA, "indicadores/lista de indicadores.xlsx")
caminho_seguranca       <- file.path(DIR_RAW_AURORA, "seguranca/df_seguranca_publica_forum_nacional.xlsx")
caminho_mapbiomas       <- file.path(DIR_RAW_AURORA, "meio ambiente/indicadores_mapbiomas.parquet")

caminho_energia         <- file.path(DIR_RAW_AURORA, "energia/dados_potencia_energia_uf.xlsx")
caminho_agua            <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_agua_sinisa.xlsx")
caminho_esgoto          <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_esgoto_sinisa.xlsx")
caminho_agricultura     <- file.path(DIR_RAW_AURORA, "agricultura/dados_pam_uf_2020_2025.xlsx")
caminho_suape           <- file.path(DIR_RAW_AURORA, "aeroportuaria/porto_suape_carga.xlsx")
caminho_passageiros     <- file.path(DIR_RAW_AURORA, "aeroportuaria/df_passageiros_uf_ano.xlsx")

caminho_turismo     <- file.path(DIR_DADOS, "parquet/pms_iatur_uf.parquet")

# Observatório da Criança e do Adolescente (Fundação Abrinq)
caminho_obitos_mat  <- file.path(DIR_RAW_AURORA, "saude/Número de óbitos maternos segundo cor_raça.xlsx")
caminho_acid_cor    <- file.path(DIR_RAW_AURORA, "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo cor_raça.xlsx")
caminho_acid_idade  <- file.path(DIR_RAW_AURORA, "cidadania/Notificações acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade.xlsx")
caminho_acid_sexo   <- file.path(DIR_RAW_AURORA, "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo sexo.xlsx")
caminho_acid_evol   <- file.path(DIR_RAW_AURORA, "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo evolução dos casos.xlsx")
caminho_mort        <- file.path(DIR_RAW_AURORA, "saude/Taxa de mortalidade atribuída a fontes de água inadequadas, saneamento inadequado e falta de higiene segundo grupos etários.xlsx")
caminho_ti_econ     <- file.path(DIR_RAW_AURORA, "cidadania/População de 5 a 17 anos de idade em situação de trabalho infantil que realizaram atividades econômicas ou autoconsumo (a partir de 2016).xlsx")
caminho_ti_tip      <- file.path(DIR_RAW_AURORA, "cidadania/População de 5 a 17 anos de idade em situação de trabalho infantil que realizaram atividades identificadas na Lista TIP das piores formas de trabalho infantil (a partir de 2016).xlsx")
caminho_ti_autoc    <- file.path(DIR_RAW_AURORA, "cidadania/População de cinco a 17 anos de idade em situação de trabalho infantil que realizaram apenas atividades de autoconsumo (a partir de 2016).xlsx")
caminho_ti_agro     <- file.path(DIR_RAW_AURORA, "cidadania/População com até 14 anos de idade ocupada em estabelecimentos agropecuários.xlsx")
caminho_tip_ocup    <- file.path(DIR_RAW_AURORA, "cidadania/Proporção da população ocupada de 14 a 17 anos de idade desempenhando atividades identificadas às piores formas do trabalho infantil (Lista TIP).xlsx")
caminho_ti_ocup     <- file.path(DIR_RAW_AURORA, "cidadania/Proporção de adolescentes de 14 a 17 anos de idade em situação de trabalho infantil em relação à população ocupada desta faixa etária.xlsx")
caminho_gini        <- file.path(DIR_RAW_AURORA, "economia/Coeficiente de Gini de desigualdade.xlsx")
caminho_liberdade   <- file.path(DIR_RAW_AURORA, "cidadania/Número de adolescentes cumprindo medidas de privação e de restrição de liberdade.xlsx")
caminho_casamentos  <- file.path(DIR_RAW_AURORA, "cidadania/Número de casamentos registrados crianças e adolescentes do sexo feminino.xlsx")
caminho_suic_escol  <- file.path(DIR_RAW_AURORA, "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo escolaridade.xlsx")
caminho_suic_sexo   <- file.path(DIR_RAW_AURORA, "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo sexo.xlsx")
caminho_suic_cor    <- file.path(DIR_RAW_AURORA, "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo cor_raça.xlsx")
caminho_suic_idade  <- file.path(DIR_RAW_AURORA, "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo grupos etários.xlsx")
caminho_lesao_cor   <- file.path(DIR_RAW_AURORA, "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo cor_raça.xlsx")
caminho_lesao_sexo  <- file.path(DIR_RAW_AURORA, "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo sexo.xlsx")
caminho_lesao_escol <- file.path(DIR_RAW_AURORA, "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo escolaridade.xlsx")
caminho_lesao_idade <- file.path(DIR_RAW_AURORA, "saude/Notificações de lesões autoprovocadas segundo grupos etários.xlsx")

## Lista de indicadores ----
url_lista <- "https://docs.google.com/spreadsheets/d/1WNuK75MyAHYofzvd6wkiVIG-FHpwqruIDyUMtkfBlOI/edit?gid=1805893996#gid=1805893996"
caminho_indicador <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_lista), path = caminho_indicador, overwrite = TRUE)

## Déficit Habitacional (Google Drive) ----
url_deficit_hab <- "https://docs.google.com/spreadsheets/d/1AwlL17JBmASv8Jurnkb6zhfXc0fzGQwp/edit?gid=1984136610#gid=1984136610"
caminho_deficit_hab <- tempfile(fileext = ".xlsx")

tryCatch({
  drive_download(file = googledrive::as_id(url_deficit_hab), path = caminho_deficit_hab, overwrite = TRUE)
}, error = function(e) {
  cli_alert_warning("Falha ao baixar planilha de Déficit Habitacional do Google Drive: {e$message}")
})

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
    ) %>%
    filter(!is.na(valor_num)) %>%
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


## 5.15 Passageiros ----
extrair_passageiros_uf <- function() {
  if (!file.exists(caminho_passageiros)) {
    cli_alert_warning("Arquivo de Passageiros ausente em {.path {caminho_passageiros}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_passageiros, col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    mutate(
      sg_uf     = as.character(uf),
      ano       = as.numeric(ano),
      valor_num = as.numeric(total_geral)
    ) %>%
    filter(!is.na(valor_num)) %>%
    group_by(sg_uf, ano) %>%
    summarise(
      valor_total = sum(valor_num, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      sigla_indicador = "movimentacao_aero",
      mes_referencia  = NA_character_,
      indicador       = as.character(round(valor_total, 2))
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_passageiros_ufs <- extrair_passageiros_uf()
cli_alert_success("Aeroportos (Passageiros) processada.")


## 5.16 Déficit Habitacional (FJP / Google Drive) ----
extrair_deficit_hab_uf <- function() {
  if (!file.exists(caminho_deficit_hab)) {
    cli_alert_warning("Arquivo de Déficit Habitacional ausente em {.path {caminho_deficit_hab}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_deficit_hab, sheet = "Deficit_habitacional", col_types = "text")
    )
  )
  
  df_raw %>%
    clean_names() %>%
    mutate(
      nm_uf = str_trim(regiao),
      ano   = as.numeric(ano),
      # Trata separadores de milhar (.) e decimal (,) antes de converter
      valor_num = as.numeric(deficit_habitacional)
    ) %>%
    # O inner_join com tb_ufs mantém apenas as UFs e descarta as linhas de regiões (ex: Norte)
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "nm_uf") %>%
    filter(!is.na(ano), !is.na(valor_num)) %>%
    mutate(
      sigla_indicador = "deficit_hab",
      mes_referencia  = NA_character_,
      indicador       = as.character(valor_num)
    ) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_deficit_hab_ufs <- extrair_deficit_hab_uf()
cli_alert_success("Déficit Habitacional processado.")


## 5.17 Turismo (IATUR / PMS - IBGE) ----
# extrair_turismo_uf <- function() {
#   if (!file.exists(caminho_turismo)) {
#     cli_alert_warning("Arquivo de turismo ausente em {.path {caminho_turismo}}")
#     return(tibble())
#   }
#   
#   df_raw <- suppressWarnings(
#     suppressMessages(
#       arrow::read_parquet(caminho_turismo)
#     )
#   )
#   
#   df_raw %>%
#     clean_names() %>%
#     # Mantém apenas o número-índice (var_codigo == "7168"), evitando misturar com taxas de variação (%)
#     filter(var_codigo == "7168") %>%
#     mutate(
#       cod_uf    = as.character(str_trim(uf_codigo)),
#       ano       = lubridate::year(as.Date(data)),
#       valor_num = as.numeric(valor),
#       sigla_indicador = case_when(
#         tipo_codigo == "56727" | str_detect(tipo_indice, regex("receita", ignore_case = TRUE)) ~ "iatur_receita_idx",
#         tipo_codigo == "56728" | str_detect(tipo_indice, regex("volume", ignore_case = TRUE))  ~ "iatur_volume_idx",
#         TRUE ~ NA_character_
#       )
#     ) %>%
#     filter(!is.na(sigla_indicador), !is.na(ano), !is.na(valor_num)) %>%
#     # Média anual do número-índice por UF e tipo de índice
#     group_by(cod_uf, ano, sigla_indicador) %>%
#     summarise(
#       valor_anual = mean(valor_num, na.rm = TRUE),
#       .groups     = "drop"
#     ) %>%
#     inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
#     mutate(
#       mes_referencia = NA_character_,
#       indicador      = as.character(round(valor_anual, 2))
#     ) %>%
#     select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
# }

# df_turismo_ufs <- extrair_turismo_uf()
# cli_alert_success("Turismo (IATUR) processado.")

## 5.17 Turismo (IATUR / PMS - IBGE - Série Mensal) ----
extrair_turismo_uf <- function() {
  if (!file.exists(caminho_turismo)) {
    cli_alert_warning("Arquivo de turismo ausente em {.path {caminho_turismo}}")
    return(tibble())
  }
  
  df_raw <- suppressWarnings(
    suppressMessages(
      arrow::read_parquet(caminho_turismo)
    )
  )
  
  df_raw %>%
    clean_names() %>%
    # Mantém apenas o número-índice (var_codigo == "7168"), evitando misturar com taxas de variação (%)
    filter(var_codigo == "7168") %>%
    mutate(
      cod_uf         = as.character(str_trim(uf_codigo)),
      data_ref       = as.Date(data),
      ano            = lubridate::year(data_ref),
      # Extrai o mês ("01" a "12") a partir do mes_codigo (ex: "201101" -> "01")
      mes_referencia = str_sub(str_trim(as.character(mes_codigo)), 5, 6),
      valor_num      = as.numeric(valor),
      sigla_indicador = case_when(
        tipo_codigo == "56727" | str_detect(tipo_indice, regex("receita", ignore_case = TRUE)) ~ "iatur_receita_idx",
        tipo_codigo == "56728" | str_detect(tipo_indice, regex("volume", ignore_case = TRUE))  ~ "iatur_volume_idx",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(sigla_indicador), !is.na(ano), !is.na(mes_referencia), !is.na(valor_num)) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    mutate(
      indicador = as.character(round(valor_num, 2))
    ) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_turismo_ufs <- extrair_turismo_uf() %>%
  filter(ano >= "2020")

df_turismo_ufs %>% View()
cli_alert_success("Turismo (IATUR - mensal) processado.")

## 5.18 Observatório da Criança e do Adolescente (Fundação Abrinq) ----
# Planilhas baixadas do Observatório da Criança e do Adolescente (observatoriocrianca.org.br).
# Todas seguem o mesmo layout: aba "Índice" + uma aba por recorte, skip = 2, colunas
# Localidade | Código IBGE | Código DataSUS | anos ("AAAA", "AAAA (%)", "AAAA (absoluto)" ou
# "Trimestre 1/2012"). As tabelas abas_* dizem qual sigla sai de cada aba: `sigla` recebe a
# coluna do ano (ou do %) e `sigla_abs` a coluna "(absoluto)". Os nomes das abas vêm truncados
# da fonte; o comentário de cada linha traz o rótulo completo da aba (célula A2).

extrair_abrinq_uf <- function(caminho_arquivo, abas_siglas, traco_como_zero = FALSE) {
  if (!file.exists(caminho_arquivo)) {
    cli_alert_warning("Planilha Abrinq não encontrada: {.path {caminho_arquivo}}")
    return(tibble())
  }

  map_dfr(seq_len(nrow(abas_siglas)), function(i) {
    df_raw <- suppressWarnings(
      suppressMessages(
        read_excel(caminho_arquivo, sheet = abas_siglas$aba[i], skip = 2, col_types = "text")
      )
    )

    df_raw %>%
      rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
      rename(cod_uf = `Código IBGE`) %>%
      mutate(cod_uf = str_trim(as.character(cod_uf))) %>%
      inner_join(tb_ufs %>% distinct(cod_uf, sg_uf, nm_uf), by = "cod_uf") %>%
      select(cod_uf, sg_uf, nm_uf, matches("[0-9]{4}")) %>%
      pivot_longer(
        cols      = -c(cod_uf, sg_uf, nm_uf),
        names_to  = "col_orig",
        values_to = "indicador"
      ) %>%
      mutate(
        ano     = as.numeric(str_extract(col_orig, "[0-9]{4}")),
        metrica = case_when(
          str_detect(col_orig, "absoluto")           ~ "abs",
          str_detect(col_orig, "\\(%\\)|^Trimestre") ~ "pct",
          TRUE                                       ~ "valor"
        ),
        sigla_indicador = if_else(metrica == "abs", abas_siglas$sigla_abs[i], abas_siglas$sigla[i]),
        mes_referencia  = if_else(str_detect(col_orig, "^Trimestre"), paste0("T", str_extract(col_orig, "(?<=Trimestre )[1-4]")), NA_character_),
        indicador       = str_trim(indicador),
        indicador       = if_else(traco_como_zero & indicador %in% "-", "0", indicador)
      ) %>%
      distinct(cod_uf, sigla_indicador, ano, mes_referencia, .keep_all = TRUE) %>%  # algumas abas repetem a coluna de um ano
      filter(!is.na(sigla_indicador), !is.na(ano), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
      mutate(indicador = if_else(metrica == "pct", as.character(round(as.numeric(indicador), 2)), indicador)) %>%
      select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  })
}

## 5.18.1 Óbitos maternos por cor/raça ----
# Decisão D-001 (Kattine, 10/11/2026): de 2016 em diante o zero vem como "-" na planilha
# (de 2000 a 2015 o 0 é explícito). "-" = 0 óbito.

abas_obitos_mat <- tribble(
  ~aba,                              ~sigla,                ~sigla_abs,
  "1 -   Indígena",                  "obitos_mat_indigena", NA_character_,  # Indígena
  "2 -   Preta",                     "obitos_mat_preta",    NA_character_,  # Preta
  "3 -   Branca",                    "obitos_mat_branca",   NA_character_,  # Branca
  "4 -   Amarela",                   "obitos_mat_amarela",  NA_character_,  # Amarela
  "5 -   Parda",                     "obitos_mat_parda",    NA_character_,  # Parda
  "6 -   Ignorada",                  "obitos_mat_ignorada", NA_character_,  # Ignorado
  "7 -   Número de óbitos maternos", "obitos_mat_total",    NA_character_  # Total de óbitos maternos no ano
)

df_obitos_mat_ufs <- extrair_abrinq_uf(caminho_obitos_mat, abas_obitos_mat, traco_como_zero = TRUE)
cli_alert_success("Óbitos maternos por cor/raça processado(s).")

## 5.18.2 Acidentes de trabalho de crianças e adolescentes (5 a 17 anos) ----
# Decisão D-002 (Kattine, 11/11/2026): "-" = 0 notificação. A soma dos 185 municípios
# de PE bate com o total da UF.

abas_acid_cor <- tribble(
  ~aba,                              ~sigla,                   ~sigla_abs,
  "2 -   Notificações de acidentes", "acid_trab_5_17_total",   NA_character_,  # Total de acidentes relacionados ao trabalho de crianças e adolescentes entre 5 e 17 anos idade
  "3 -   Amarela",                   "acid_trab_cor_amarela",  NA_character_,  # Amarela
  "4 -   Indígena",                  "acid_trab_cor_indigena", NA_character_,  # Indígena
  "5 -   Preta",                     "acid_trab_cor_preta",    NA_character_,  # Preta
  "6 -   Parda",                     "acid_trab_cor_parda",    NA_character_,  # Parda
  "7 -   Ignorada",                  "acid_trab_cor_ignorada", NA_character_,  # Ignorado
  "8 -   Branca",                    "acid_trab_cor_branca",   NA_character_  # Branca
)

abas_acid_idade <- tribble(
  ~aba,                          ~sigla,            ~sigla_abs,
  "1 -   5 a 9 anos de idade",   "acid_trab_5_9",   NA_character_,  # 5 a 9 anos de idade
  "4 -   10 a 14 anos de idade", "acid_trab_10_14", NA_character_,  # 10 a 14 anos de idade
  "5 -   15 a 17 anos de idade", "acid_trab_15_17", NA_character_  # 15 a 17 anos de idade
)

abas_acid_sexo <- tribble(
  ~aba,              ~sigla,                     ~sigla_abs,
  "2 -   Ignorado",  "acid_trab_sexo_ignorado",  NA_character_,  # Sexo ignorado
  "3 -   Feminino",  "acid_trab_sexo_feminino",  NA_character_,  # Feminino
  "4 -   Masculino", "acid_trab_sexo_masculino", NA_character_  # Masculino
)

abas_acid_evol <- tribble(
  ~aba,                              ~sigla,                          ~sigla_abs,
  "1 -   Ignorada",                  "acid_trab_evol_ignorada",       NA_character_,  # Ignorado/Branco
  "2 -   Incapacidade parcial perm", "acid_trab_evol_incap_parcial",  NA_character_,  # Incapacidade parcial permanente
  "4 -   Incapacidade total perman", "acid_trab_evol_incap_total",    NA_character_,  # Incapacidade total permanente
  "5 -   Outra",                     "acid_trab_evol_outra",          NA_character_,  # Outra
  "6 -   Óbito por outras causas",   "acid_trab_evol_obito_outras",   NA_character_,  # Óbito por outras causas
  "7 -   Óbito pelo acidente",       "acid_trab_evol_obito_acidente", NA_character_,  # Óbito pelo acidente
  "8 -   Incapacidade temporária",   "acid_trab_evol_incap_temp",     NA_character_,  # Incapacidade temporária
  "9 -   Cura",                      "acid_trab_evol_cura",           NA_character_  # Cura
)

df_acid_trab_ufs <- bind_rows(
  extrair_abrinq_uf(caminho_acid_cor, abas_acid_cor, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_acid_idade, abas_acid_idade, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_acid_sexo, abas_acid_sexo, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_acid_evol, abas_acid_evol, traco_como_zero = TRUE)
)
cli_alert_success("Acidentes de trabalho de crianças e adolescentes (5 a 17 anos) processado(s).")

## 5.18.3 Mortalidade por saneamento inadequado (taxa e nº de óbitos por faixa etária) ----
# Aqui o "-" é descartado (o 0 aparece explícito; "-" = não calculado).
# A aba "Idade ignorada" só tem o nº de óbitos (a taxa é toda "-").

abas_mort <- tribble(
  ~aba,                              ~sigla,                    ~sigla_abs,
  "4 -   0 a 4 anos de idade",       "mort_sanea_taxa_0_4",     "mort_sanea_abs_0_4",  # zero a quatro anos de idade
  "9 -   5 a 9 anos de idade",       "mort_sanea_taxa_5_9",     "mort_sanea_abs_5_9",  # cinco a nove anos de idade
  "6 -   10 a 14 anos de idade",     "mort_sanea_taxa_10_14",   "mort_sanea_abs_10_14",  # dez a 14 anos de idade
  "1 -   15 a 19 anos de idade",     "mort_sanea_taxa_15_19",   "mort_sanea_abs_15_19",  # 15 a 19 anos de idade
  "3 -   20 a 29 anos de idade",     "mort_sanea_taxa_20_29",   "mort_sanea_abs_20_29",  # 20 a 29 anos de idade
  "10 -   30 a 39 anos de idade",    "mort_sanea_taxa_30_39",   "mort_sanea_abs_30_39",  # 30 a 39 anos de idade
  "2 -   40 a 49 anos de idade",     "mort_sanea_taxa_40_49",   "mort_sanea_abs_40_49",  # 40 a 49 anos de idade
  "8 -   50 anos ou mais de idade",  "mort_sanea_taxa_50_mais", "mort_sanea_abs_50_mais",  # 50 anos ou mais de idade
  "7 -   Idade ignorada",            NA_character_,             "mort_sanea_abs_ignorada",  # Idade ignorada
  "5 -   Taxa de mortalidade atrib", "mort_sanea_taxa_total",   "mort_sanea_abs_total"  # Taxa de mortalidade total
)

df_mort_sanea_ufs <- extrair_abrinq_uf(caminho_mort, abas_mort)
cli_alert_success("Mortalidade por saneamento inadequado (taxa e nº de óbitos por faixa etária) processado(s).")

## 5.18.4 Trabalho infantil (5 a 17 anos), Lista TIP e ocupação agropecuária ----
# Só UFs, exceto a ocupação em estabelecimentos agropecuários (municípios e UFs, só 2017).
# tip_ocup e ti_ocup são trimestrais: ano + mes_referencia = "T1".."T4".

abas_ti_econ <- tribble(
  ~aba,                              ~sigla,              ~sigla_abs,
  "1 -   População de 5 a 17 anos ", "ti_econ_total_pct", "ti_econ_total_abs",  # Total de população de 5 a 17 anos de idade que realizaram atividades econômicas
  "3 -   5 a 9 anos de idade",       "ti_econ_5_9_pct",   "ti_econ_5_9_abs",  # 5 a 9 anos de idade
  "4 -   10 a 13 anos de idade",     "ti_econ_10_13_pct", "ti_econ_10_13_abs",  # 10 a 13 anos de idade
  "5 -   14 a 15 anos de idade",     "ti_econ_14_15_pct", "ti_econ_14_15_abs",  # 14 a 15 anos de idade
  "2 -   16 a 17 anos de idade",     "ti_econ_16_17_pct", "ti_econ_16_17_abs"  # 16 a 17 anos de idade
)

abas_ti_tip <- tribble(
  ~aba,                              ~sigla,             ~sigla_abs,
  "3 -   População de 5 a 17 anos ", "ti_tip_total_pct", "ti_tip_total_abs",  # Total de população de 5 a 17 anos de idade que realizaram atividades econômicas
  "1 -   5 a 9 anos de idade",       "ti_tip_5_9_pct",   "ti_tip_5_9_abs",  # 5 a 9 anos de idade
  "4 -   10 a 13 anos de idade",     "ti_tip_10_13_pct", "ti_tip_10_13_abs",  # 10 a 13 anos de idade
  "2 -   14 a 15 anos de idade",     "ti_tip_14_15_pct", "ti_tip_14_15_abs",  # 14 a 15 anos de idade
  "5 -   16 a 17 anos de idade",     "ti_tip_16_17_pct", "ti_tip_16_17_abs"  # 16 a 17 anos de idade
)

abas_ti_autoc <- tribble(
  ~aba,                              ~sigla,               ~sigla_abs,
  "4 -   População de 5 a 17 anos ", "ti_autoc_total_abs", NA_character_,  # Total de população de 5 a 17 anos de idade que realizaram atividades econômicas
  "5 -   5 a 9 anos de idade",       "ti_autoc_5_9_abs",   NA_character_,  # 5 a 9 anos de idade
  "2 -   10 a 13 anos de idade",     "ti_autoc_10_13_abs", NA_character_,  # 10 a 13 anos de idade
  "3 -   14 e 15 anos de idade",     "ti_autoc_14_15_abs", NA_character_,  # 14 a 15 anos de idade
  "1 -   16 e 17 anos de idade",     "ti_autoc_16_17_abs", NA_character_  # 16 a 17 anos de idade
)

abas_ti_agro <- tribble(
  ~aba,                         ~sigla,                ~sigla_abs,
  "1 -   Até 14 anos de idade", "ocup_agro_ate14_pct", "ocup_agro_ate14_abs"  # Menores de 14 anos de idade
)

abas_tip_ocup <- tribble(
  ~aba,                              ~sigla,               ~sigla_abs,
  "1 -   Proporção da população oc", "tip_ocup_14_17_pct", "tip_ocup_14_17_abs"  # Proporção da população ocupada de 14 a 17 anos de idade desempenhando atividades identificadas às piores formas do trabalho infantil (Lista TIP)
)

abas_ti_ocup <- tribble(
  ~aba,                              ~sigla,              ~sigla_abs,
  "1 -   Proporção de adolescentes", "ti_ocup_14_17_pct", "ti_ocup_14_17_abs"  # Proporção de adolescentes de 14 a 17 anos de idade em situação de trabalho infantil em relação à população ocupada
)

df_trab_infantil_ufs <- bind_rows(
  extrair_abrinq_uf(caminho_ti_econ, abas_ti_econ),
  extrair_abrinq_uf(caminho_ti_tip, abas_ti_tip),
  extrair_abrinq_uf(caminho_ti_autoc, abas_ti_autoc),
  extrair_abrinq_uf(caminho_ti_agro, abas_ti_agro),
  extrair_abrinq_uf(caminho_tip_ocup, abas_tip_ocup),
  extrair_abrinq_uf(caminho_ti_ocup, abas_ti_ocup)
) %>%
  filter(
    # Decisão D-004 (Kattine, 12/11/2026): apenas autoconsumo, 2022 é cópia de 2019 na fonte em 4 faixas
    !(str_detect(sigla_indicador, "^ti_autoc_(5_9|10_13|14_15|16_17)_abs$") & ano == 2022),
    # Decisão D-005 (Kattine, 12/11/2026): % da Lista TIP de 2022 com denominador errado na fonte (4 faixas)
    !(str_detect(sigla_indicador, "^ti_tip_(5_9|10_13|14_15|16_17)_pct$") & ano == 2022),
    # Decisão D-006 (Kattine, 12/11/2026): % de 2017 das atividades econômicas 16-17 anos ~2x o esperado
    !(sigla_indicador == "ti_econ_16_17_pct" & ano == 2017)
  )
cli_alert_success("Trabalho infantil (5 a 17 anos), Lista TIP e ocupação agropecuária processado(s).")

## 5.18.5 Coeficiente de Gini ----
# Só UFs.

abas_gini <- tribble(
  ~aba,                              ~sigla,              ~sigla_abs,
  "1 -   Coeficiente Gini de desig", "gini_desigualdade", NA_character_  # Coeficiente Gini
)

df_gini_ufs <- extrair_abrinq_uf(caminho_gini, abas_gini)
cli_alert_success("Coeficiente de Gini processado(s).")

## 5.18.6 Adolescentes em medidas socioeducativas de privação/restrição de liberdade ----
# Só UFs. O "-" é descartado (anos sem levantamento; ex.: "Internação sanção" só tem 2023).

abas_liberdade <- tribble(
  ~aba,                              ~sigla,                              ~sigla_abs,
  "3 - Tipo de medida sócio-educat", "socioedu_medida_internacao",        NA_character_,  # Tipo de medida sócio-educativa / Internação
  "5 - Tipo de medida sócio-educat", "socioedu_medida_intern_provisoria", NA_character_,  # Tipo de medida sócio-educativa / Internação provisória
  "2 - Tipo de medida sócio-educat", "socioedu_medida_semiliberdade",     NA_character_,  # Tipo de medida sócio-educativa / Semiliberdade
  "1 - Tipo de medida sócio-educat", "socioedu_medida_intern_sancao",     NA_character_,  # Tipo de medida sócio-educativa / Internação Sanção
  "4 - Tipo de medida sócio-educat", "socioedu_medida_outros",            NA_character_,  # Outros
  "6 -   Número de adolescentes cu", "socioedu_medida_total",             NA_character_  # Total de adolescentes
)

df_socioedu_ufs <- extrair_abrinq_uf(caminho_liberdade, abas_liberdade)
cli_alert_success("Adolescentes em medidas socioeducativas de privação/restrição de liberdade processado(s).")

## 5.18.7 Casamentos de crianças e adolescentes do sexo feminino ----
# Decisão D-007 (Kattine, 16/11/2026): "-" = 0 casamento. Em PE a soma dos municípios
# bate com a UF em todos os anos (2003-2024).

abas_casamentos <- tribble(
  ~aba,                              ~sigla,                      ~sigla_abs,
  "1 -   Número de casamentos de m", "casamento_fem_ate19_total", NA_character_,  # Total de casamentos de meninas menores de 19 anos
  "2 -   15 a 19 anos de idade",     "casamento_fem_15_19",       NA_character_,  # 15 a 19 anos
  "3 -   Até 15 anos de idade",      "casamento_fem_ate15",       NA_character_  # Menores de 15 anos
)

df_casamentos_ufs <- extrair_abrinq_uf(caminho_casamentos, abas_casamentos, traco_como_zero = TRUE)
cli_alert_success("Casamentos de crianças e adolescentes do sexo feminino processado(s).")

## 5.18.8 Óbitos por suicídio de indivíduos até 19 anos ----
# Decisão D-008 (Kattine, 17/11/2026): "-" = 0 óbito (a soma das categorias bate com o total).
# O total até 19 anos sai só da planilha por escolaridade.

abas_suic_escol <- tribble(
  ~aba,                              ~sigla,                          ~sigla_abs,
  "1 -   Número de óbitos de indiv", "suicidio_ate19_total",          NA_character_,  # Número de óbitos de indivíduos de até 19 anos de idade por suicídio
  "5 -   Nenhuma escolaridade",      "suicidio_ate19_escol_nenhuma",  NA_character_,  # Nenhuma escolaridade
  "3 -   1 a 3 de anos",             "suicidio_ate19_escol_1_3",      NA_character_,  # 1 a 3 de anos
  "6 -   4 a 7 anos",                "suicidio_ate19_escol_4_7",      NA_character_,  # 4 a 7 anos
  "4 -   8 a 11 anos",               "suicidio_ate19_escol_8_11",     NA_character_,  # 8 a 11 anos
  "7 -   12 anos e mais",            "suicidio_ate19_escol_12_mais",  NA_character_,  # 12 anos e mais
  "2 -   Escolaridade ignorada",     "suicidio_ate19_escol_ignorada", NA_character_  # Escolaridade ignorada
)

abas_suic_sexo <- tribble(
  ~aba,              ~sigla,                          ~sigla_abs,
  "1 -   Feminino",  "suicidio_ate19_sexo_feminino",  NA_character_,  # Feminino
  "2 -   Masculino", "suicidio_ate19_sexo_masculino", NA_character_  # Masculino
)

abas_suic_cor <- tribble(
  ~aba,                      ~sigla,                        ~sigla_abs,
  "3 -   Branca",            "suicidio_ate19_cor_branca",   NA_character_,  # Branca
  "4 -   Preta",             "suicidio_ate19_cor_preta",    NA_character_,  # Preta
  "7 -   Parda",             "suicidio_ate19_cor_parda",    NA_character_,  # Parda
  "5 -   Amarela",           "suicidio_ate19_cor_amarela",  NA_character_,  # Amarela
  "2 -   Indígena",          "suicidio_ate19_cor_indigena", NA_character_,  # Indígena
  "1 -   Cor raça ignorada", "suicidio_ate19_cor_ignorada", NA_character_  # Cor/raça ignorada
)

abas_suic_idade <- tribble(
  ~aba,                              ~sigla,                       ~sigla_abs,
  "1 - Número de óbitos de criança", "suicidio_ate19_idade_5_9",   NA_character_,  # 5 a 9 anos de idade
  "13 - Número de óbitos de crianç", "suicidio_ate19_idade_10_14", NA_character_,  # 10 a 14 anos de idade
  "6 - Número de óbitos de criança", "suicidio_ate19_idade_15_19", NA_character_  # 15 a 19 anos de idade
)

df_suicidio_ufs <- bind_rows(
  extrair_abrinq_uf(caminho_suic_escol, abas_suic_escol, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_suic_sexo, abas_suic_sexo, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_suic_cor, abas_suic_cor, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_suic_idade, abas_suic_idade, traco_como_zero = TRUE)
) %>%
  # Decisão D-009 (Kattine, 17/11/2026): na planilha por grupos etários, 2007 é cópia de 2008 e
  # 2020 diverge das outras três planilhas (PE: 34 contra 25). Os dois anos ficam de fora.
  filter(!(str_starts(sigla_indicador, "suicidio_ate19_idade_") & ano %in% c(2007, 2020)))
cli_alert_success("Óbitos por suicídio de indivíduos até 19 anos processado(s).")

## 5.18.9 Notificações de lesões autoprovocadas de crianças e adolescentes até 19 anos ----
# Decisão D-010 (Kattine, 18/11/2026): "-" = 0 notificação (a soma das categorias bate com o total).
# O total até 19 anos sai só da planilha por cor/raça.

abas_lesao_cor <- tribble(
  ~aba,                              ~sigla,                              ~sigla_abs,
  "3 -   Notificações de lesões au", "lesao_autoprov_ate19_total",        NA_character_,  # Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade
  "7 -   Branca",                    "lesao_autoprov_ate19_cor_branca",   NA_character_,  # Branca
  "4 -   Preta",                     "lesao_autoprov_ate19_cor_preta",    NA_character_,  # Preta
  "1 -   Parda",                     "lesao_autoprov_ate19_cor_parda",    NA_character_,  # Parda
  "6 -   Amarela",                   "lesao_autoprov_ate19_cor_amarela",  NA_character_,  # Amarela
  "2 -   Indígena",                  "lesao_autoprov_ate19_cor_indigena", NA_character_,  # Indígena
  "5 -   Cor raça ignorada",         "lesao_autoprov_ate19_cor_ignorada", NA_character_  # Cor/raça ignorada
)

abas_lesao_sexo <- tribble(
  ~aba,                  ~sigla,                                ~sigla_abs,
  "2 -   Feminino",      "lesao_autoprov_ate19_sexo_feminino",  NA_character_,  # Feminino
  "4 -   Masculino",     "lesao_autoprov_ate19_sexo_masculino", NA_character_,  # Masculino
  "3 -   Sexo ignorado", "lesao_autoprov_ate19_sexo_ignorado",  NA_character_  # Sexo ignorado
)

abas_lesao_escol <- tribble(
  ~aba,                              ~sigla,                                         ~sigla_abs,
  "8 -   Analfabeto",                "lesao_autoprov_ate19_escol_analfabeto",        NA_character_,  # Analfabeto
  "4 -   1ª a 4ª série incompleta ", "lesao_autoprov_ate19_escol_ef_1_4_incompleta", NA_character_,  # 1ª a 4ª série incompleta do Ensino Fundamental
  "5 -   4ª série completa do Ensi", "lesao_autoprov_ate19_escol_ef_4_completa",     NA_character_,  # 4ª série completa do Ensino Fundamental
  "11 -   5ª a 8ª série incompleta", "lesao_autoprov_ate19_escol_ef_5_8_incompleta", NA_character_,  # 5ª a 8ª série incompleta do Ensino Fundamental
  "10 -   Ensino Fundamental compl", "lesao_autoprov_ate19_escol_ef_completo",       NA_character_,  # Ensino Fundamental completo
  "2 -   Ensino Médio incompleto",   "lesao_autoprov_ate19_escol_em_incompleto",     NA_character_,  # Ensino Médio incompleto
  "9 -   Ensino Médio completo",     "lesao_autoprov_ate19_escol_em_completo",       NA_character_,  # Ensino Médio completo
  "3 -   Educação Superior incompl", "lesao_autoprov_ate19_escol_sup_incompleta",    NA_character_,  # Educação superior incompleta
  "1 -   Educação Superior complet", "lesao_autoprov_ate19_escol_sup_completa",      NA_character_,  # Educação superior completa
  "6 -   Não se aplica",             "lesao_autoprov_ate19_escol_nao_se_aplica",     NA_character_,  # Não se aplica
  "12 -   Escolaridade ignorada",    "lesao_autoprov_ate19_escol_ignorada",          NA_character_  # Escolaridade ignorada
)

abas_lesao_idade <- tribble(
  ~aba,                              ~sigla,                               ~sigla_abs,
  "9 - Número de casos entre crian", "lesao_autoprov_ate19_idade_menor_1", NA_character_,  # Menores de 1 ano de idade
  "7 - Número de casos entre crian", "lesao_autoprov_ate19_idade_1_4",     NA_character_,  # 1 a 4 anos de idade
  "1 - Número de casos entre crian", "lesao_autoprov_ate19_idade_5_9",     NA_character_,  # 5 a 9 anos de idade
  "2 - Número de casos entre crian", "lesao_autoprov_ate19_idade_10_14",   NA_character_,  # 10 a 14 anos de idade
  "6 - Número de casos entre crian", "lesao_autoprov_ate19_idade_15_19",   NA_character_  # 15 a 19 anos de idade
)

df_lesoes_ufs <- bind_rows(
  extrair_abrinq_uf(caminho_lesao_cor, abas_lesao_cor, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_lesao_sexo, abas_lesao_sexo, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_lesao_escol, abas_lesao_escol, traco_como_zero = TRUE),
  extrair_abrinq_uf(caminho_lesao_idade, abas_lesao_idade, traco_como_zero = TRUE)
) %>%
  # Decisão D-011 (Kattine, 18/11/2026): na planilha por sexo, 2024 é cópia de 2023 na fonte.
  filter(!(str_starts(sigla_indicador, "lesao_autoprov_ate19_sexo_") & ano == 2024))
cli_alert_success("Notificações de lesões autoprovocadas de crianças e adolescentes até 19 anos processado(s).")

df_abrinq_ufs <- bind_rows(
  df_obitos_mat_ufs,
  df_acid_trab_ufs,
  df_mort_sanea_ufs,
  df_trab_infantil_ufs,
  df_gini_ufs,
  df_socioedu_ufs,
  df_casamentos_ufs,
  df_suicidio_ufs,
  df_lesoes_ufs
)
cli_alert_success("Indicadores Abrinq (UFs) consolidados: {nrow(df_abrinq_ufs)} registros.")

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
  df_passageiros_ufs,
  df_deficit_hab_ufs,
  df_turismo_ufs,
  df_abrinq_ufs
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
    any_of(c("eixo", "grupo", "subdimensao", "tipo", "natureza", "polaridade", "fonte", "hiperlink"))
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

# ------------------------------------------------------------------------------
# 7.1 GERAÇÃO E EXPORTAÇÃO DE METADADOS (JSON PARA FRONTEND)
# ------------------------------------------------------------------------------
cli_alert_info("Gerando dicionário JSON de metadados dos indicadores...")

# Normaliza possíveis typos nos nomes das colunas vindas do Excel
meta_normalizado <- meta_indicadores %>%
  rename_with(~ case_when(
    . == "direcao_desejave"    ~ "direcao_desejavel",
    . == "limpacto_planeamento" ~ "impacto_planejamento",
    TRUE ~ .
  ))

cols_esperadas <- c(
  "sigla_indicador", "nm_indicador", "nm_fantasia",
  "fonte", "hiperlink", "grupo", "eixo", "tipo", "polaridade",
  "subdimensao", "natureza", "formula_katex", "clp",
  "interpretacao", "limiar_corte", "direcao_desejavel", "impacto_planejamento"
)

for (col in cols_esperadas) {
  if (!col %in% names(meta_normalizado)) {
    meta_normalizado[[col]] <- NA_character_
  }
}

df_meta_limpo <- meta_normalizado %>%
  filter(!is.na(sigla_indicador), str_trim(sigla_indicador) != "") %>%
  mutate(across(all_of(cols_esperadas), ~ na_if(str_trim(as.character(.)), "")))

lista_metadados <- list()

for (i in seq_len(nrow(df_meta_limpo))) {
  row <- df_meta_limpo[i, ]
  
  # Sub-objeto referencia_analise (mantém compatibilidade com seu JS atual)
  ref_analise <- list(
    interpretacao        = row$interpretacao,
    limiar_corte         = row$limiar_corte,
    direcao_desejavel    = row$direcao_desejavel,
    impacto_planejamento = row$impacto_planejamento
  ) %>%
    discard(is.na)
  
  # Objeto completo com todos os metadados da planilha
  item_meta <- list(
    cd_indicador       = row$sigla_indicador,
    nm_indicador       = row$nm_indicador,
    nm_fantasia        = row$nm_fantasia,
    eixo               = row$eixo,
    grupo              = row$grupo,
    subdimensao        = row$subdimensao,
    tipo               = row$tipo,
    natureza           = row$natureza,
    polaridade         = row$polaridade,
    clp                = row$clp,
    fonte              = row$fonte,
    hiperlink          = row$hiperlink,
    formula_katex      = row$formula_katex,
    referencia_analise = ref_analise
  ) %>%
    discard(~ is.null(.) || (length(.) == 1 && is.na(.)))
  
  # 1. Indexa pela sigla (ex: "tx_mvi")
  lista_metadados[[row$sigla_indicador]] <- item_meta
  
  # 2. Indexa pelo nome oficial (ex: "Educação"), se diferente da sigla
  if (!is.na(row$nm_indicador) && row$nm_indicador != row$sigla_indicador) {
    lista_metadados[[row$nm_indicador]] <- item_meta
  }
  
  # 3. Indexa também pelo nome fantasia, caso exista e seja diferente dos anteriores
  if (!is.na(row$nm_fantasia) && 
      row$nm_fantasia != row$sigla_indicador && 
      (is.na(row$nm_indicador) || row$nm_fantasia != row$nm_indicador)) {
    lista_metadados[[row$nm_fantasia]] <- item_meta
  }
}

# Garante que a subpasta json/ exista antes de salvar
caminho_json_meta <- file.path(DIR_OUT_AURORA, "json", "metadados_indicadores_ufs.json")
dir.create(dirname(caminho_json_meta), recursive = TRUE, showWarnings = FALSE)

jsonlite::write_json(
  x          = lista_metadados,
  path       = caminho_json_meta,
  auto_unbox = TRUE,
  pretty     = TRUE
)

cli_alert_success("Metadados JSON exportados com sucesso em: {.path {caminho_json_meta}}")


# # Exportação para Google Sheets ----
# cli::cli_alert_info("Iniciando exportação para o Google Sheets...")
# 
# # 1. Autenticação via Service Account (padrão DIR_RAW_AURORA)
# google_token_path <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
# 
# if (file.exists(google_token_path)) {
#   googlesheets4::gs4_auth(path = google_token_path)
# } else {
#   cli::cli_warn("Token JSON não encontrado em {.path {google_token_path}}. Tentando autenticação padrão...")
#   googlesheets4::gs4_auth()
# }
# 
# # 2. ID da Planilha de destino
# sheet_id_destino <- "1UpOV_rqZWU19eNzA0B1KyqFisAxMR1UhCqN6eMchGQo"
# 
# # 3. Escrita da base consolidada na primeira aba (gid = 0)
# googlesheets4::sheet_write(
#   data  = df_indicadores_ufs,
#   ss    = sheet_id_destino,
#   sheet = 1
# )
# 
# cli::cli_alert_success("Base {.val df_indicadores_ufs} exportada com sucesso para o Google Sheets!")
library(arrow)
library(readxl)
library(janitor)
library(tidyverse)
library(cli)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# 1. Caminhos -----------------------------------------------------------------
caminho_regioes         <- file.path(DIR_RAW_AURORA, "educacao/IDEB/divulgacao_regioes_ufs_ideb_2025.xlsx")
caminho_brasil          <- file.path(DIR_RAW_AURORA, "educacao/IDEB/divulgacao_brasil_ideb_2025.xlsx")
caminho_creches         <- file.path(DIR_RAW_AURORA, "educacao/Infantil/Taxa bruta de matrículas em creches.xlsx")
caminho_pre_escola      <- file.path(DIR_RAW_AURORA, "educacao/Infantil/Taxa bruta de matrículas em pré-escola.xlsx")
caminho_liq_creches     <- file.path(DIR_RAW_AURORA, "educacao/Infantil/Taxa líquida de matrículas em creches.xlsx")
caminho_liq_pre_escola  <- file.path(DIR_RAW_AURORA, "educacao/Infantil/Taxa líquida de matrículas em pré-escola.xlsx")
caminho_abandono        <- file.path(DIR_RAW_AURORA, "educacao/Indicadores Educacionais/Taxa de abandono no ensino médio segundo dependência administrativa.xlsx")
caminho_distorcao       <- file.path(DIR_RAW_AURORA, "educacao/Indicadores Educacionais/Taxa de distorção idade-série no ensino médio segundo dependência administrativa.xlsx")
caminho_escolarizacao   <- file.path(DIR_RAW_AURORA, "educacao/EM/Taxa de escolarização bruta e líquida no ensino médio.xlsx")
caminho_cnuc            <- file.path(DIR_RAW_AURORA, "meio ambiente/cnuc_2026_07.csv")
caminho_mapbiomas       <- file.path(DIR_RAW_AURORA, "meio ambiente/indicadores_mapbiomas.parquet")
caminho_ibge            <- file.path(DIR_RAW_AURORA, "ibge/df_indicadores_ibge_ufs_br.parquet")

caminho_consultas       <- file.path(DIR_RAW_AURORA, "saude/Consultas Médicas por tipo de gestão.csv")
caminho_exames          <- file.path(DIR_RAW_AURORA, "saude/Exames (Procedimentos com Finalidade Diagnóstica) por tipo de gestão.csv")
caminho_cirurgias       <- file.path(DIR_RAW_AURORA, "saude/Procedimentos Cirúrgicos por tipo de gestão.csv")

caminho_indicador       <- file.path(DIR_RAW_AURORA, "indicadores/lista de indicadores.xlsx")


caminho_agua            <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_agua_sinisa.xlsx")
caminho_esgoto          <- file.path(DIR_RAW_AURORA, "vulnerabilidade/relatorio_brasil_esgoto_sinisa.xlsx")

# 2. Dicionários e Metadados --------------------------------------------------
tb_regioes <- read_excel(caminho_indicador, sheet = "dic_Regioes") %>%
  mutate(cod_regiao = as.character(cod_regiao))

tb_ufs <- read_excel(caminho_indicador, sheet = "dic_UFs") %>%
  mutate(cod_uf = as.character(cod_uf))

meta_indicadores <- read_excel(caminho_indicador, sheet = "UFs")

padronizar_rede <- function(x) {
  case_when(
    str_detect(x, regex("^Total|^Taxa de", ignore_case = TRUE)) ~ "total",
    str_detect(x, regex("^P[uú]blica", ignore_case = TRUE))     ~ "publica",
    str_detect(x, regex("^Estadual", ignore_case = TRUE))       ~ "estadual",
    str_detect(x, regex("^Municipal", ignore_case = TRUE))      ~ "municipal",
    str_detect(x, regex("^Privada", ignore_case = TRUE))        ~ "privada",
    str_detect(x, regex("^Federal", ignore_case = TRUE))        ~ "federal",
    TRUE ~ NA_character_
  )
}

# 3. Extração do IDEB (Regiões e Brasil) --------------------------------------
extrair_ideb_regioes <- function(nome_aba, etapa) {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_regioes, sheet = nome_aba, skip = 9, .name_repair = "unique")
    )
  )
  names(df_raw)[1:2] <- c("nm_regiao_orig", "rede")
  
  df_raw %>%
    inner_join(tb_regioes, by = "nm_regiao_orig") %>%
    mutate(rede_limpa = padronizar_rede(rede)) %>%
    filter(!is.na(rede_limpa)) %>%
    mutate(
      sigla_indicador = paste0("ideb_", etapa, "_", rede_limpa),
      mes_referencia  = NA_character_
    ) %>%
    select(cod_regiao, sg_regiao, nm_regiao, sigla_indicador, mes_referencia, starts_with("VL_OBSERVADO_")) %>%
    mutate(across(starts_with("VL_OBSERVADO_"), as.character)) %>%
    pivot_longer(
      cols = starts_with("VL_OBSERVADO_"), 
      names_to = "ano", 
      names_prefix = "VL_OBSERVADO_", 
      values_to = "indicador"
    ) %>%
    mutate(ano = as.numeric(ano)) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA))
}

extrair_ideb_brasil <- function(nome_aba, etapa) {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_brasil, sheet = nome_aba, skip = 9, .name_repair = "unique")
    )
  )
  names(df_raw)[1:2] <- c("nm_regiao_orig", "rede")
  
  if (nome_aba == "Brasil (Anos Finais)") {
    col_idx <- which(names(df_raw) == "VL_OBSERVADO_2021")
    if (length(col_idx) > 0 && names(df_raw)[col_idx + 1] != "VL_OBSERVADO_2025") {
      names(df_raw)[col_idx + 1] <- "VL_OBSERVADO_2023"
    }
  }
  
  df_raw %>%
    mutate(rede_limpa = padronizar_rede(rede)) %>%
    filter(!is.na(rede_limpa)) %>%
    mutate(nm_regiao_orig = "Brasil") %>%
    inner_join(tb_regioes, by = "nm_regiao_orig") %>%
    mutate(
      sigla_indicador = paste0("ideb_", etapa, "_", rede_limpa),
      mes_referencia  = NA_character_
    ) %>%
    select(cod_regiao, sg_regiao, nm_regiao, sigla_indicador, mes_referencia, starts_with("VL_OBSERVADO_")) %>%
    mutate(across(starts_with("VL_OBSERVADO_"), as.character)) %>%
    pivot_longer(
      cols = starts_with("VL_OBSERVADO_"), 
      names_to = "ano", 
      names_prefix = "VL_OBSERVADO_", 
      values_to = "indicador"
    ) %>%
    mutate(ano = as.numeric(ano)) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA))
}

df_ideb_regioes <- bind_rows(
  extrair_ideb_regioes("UF e Regiões (AI)", "5ano"), extrair_ideb_brasil("Brasil (Anos Iniciais)", "5ano"),
  extrair_ideb_regioes("UF e Regiões (AF)", "9ano"), extrair_ideb_brasil("Brasil (Anos Finais)", "9ano"),
  extrair_ideb_regioes("UF e Regiões (EM)", "3ano"), extrair_ideb_brasil("Brasil (EM)", "3ano")
)

# 4. Extração de Educação Infantil (Regiões e Brasil) ------------------------
extrair_infantil_regioes <- function(caminho_arquivo, prefixo_sigla) {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_arquivo, sheet = 2, skip = 2, col_types = "text")
    )
  )
  
  df_raw %>%
    rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
    rename(Localidade_Orig = Localidade, cod_regiao = `Código IBGE`) %>%
    mutate(
      cod_regiao = as.character(cod_regiao),
      cod_regiao = if_else(Localidade_Orig == "Brasil", "76", cod_regiao)
    ) %>%
    inner_join(tb_regioes, by = "cod_regiao") %>%
    select(cod_regiao, sg_regiao, nm_regiao, matches("^[0-9]{4}")) %>%
    mutate(across(matches("^[0-9]{4}"), as.character)) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}"), 
      names_to = "col_orig", 
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(str_extract(col_orig, "^[0-9]{4}")),
      metrica = if_else(str_detect(col_orig, regex("absoluto", ignore_case = TRUE)), "abs", "pct"),
      sigla_indicador = paste0(prefixo_sigla, "_", metrica),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA)) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_infantil_reg <- bind_rows(
  extrair_infantil_regioes(caminho_creches, "tx_creche"),
  extrair_infantil_regioes(caminho_pre_escola, "tx_pre_escola"),
  extrair_infantil_regioes(caminho_liq_creches, "tx_liq_creche"),
  extrair_infantil_regioes(caminho_liq_pre_escola, "tx_liq_pre_escola")
)

# 5. Extração por Dependência Administrativa (Regiões e Brasil) ---------------
extrair_dependencia_regioes <- function(caminho_arquivo, prefixo_sigla) {
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
      rename(Localidade_Orig = Localidade, cod_regiao = `Código IBGE`) %>%
      mutate(
        cod_regiao = as.character(cod_regiao),
        cod_regiao = if_else(Localidade_Orig == "Brasil", "76", cod_regiao)
      ) %>%
      inner_join(tb_regioes, by = "cod_regiao") %>%
      select(cod_regiao, sg_regiao, nm_regiao, matches("^[0-9]{4}$")) %>%
      mutate(across(matches("^[0-9]{4}$"), as.character)) %>%
      pivot_longer(
        cols = matches("^[0-9]{4}$"), 
        names_to = "ano", 
        values_to = "indicador"
      ) %>%
      mutate(
        ano = as.numeric(ano),
        sigla_indicador = paste0(prefixo_sigla, "_", rede_limpa),
        mes_referencia = NA_character_,
        indicador = as.character(indicador)
      ) %>%
      filter(!indicador %in% c("-", "ND", "nan", NA)) %>%
      select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
  })
}

df_dependencia_reg <- bind_rows(
  extrair_dependencia_regioes(caminho_abandono, "aba_em"),
  extrair_dependencia_regioes(caminho_distorcao, "tdi_em")
)

# 6. Extração de Unidades de Conservação (Regiões e Brasil) -------------------
extrair_cnuc_regioes <- function() {
  df_raw <- suppressWarnings(
    read_delim(
      caminho_cnuc, 
      delim = ";", 
      locale = locale(encoding = "latin1"),
      col_types = cols(.default = col_character())
    )
  )
  
  df_base <- df_raw %>%
    select(
      sg_uf = UF,
      ano_criacao = `Ano de Criação`,
      esfera = `Esfera Administrativa`,
      grupo = Grupo,
      area_raw = `Área soma biomas`
    ) %>%
    separate_rows(sg_uf, sep = ",\\s*") %>%
    mutate(
      sg_uf = str_trim(sg_uf),
      ano_criacao = as.numeric(ano_criacao),
      area_ha = area_raw %>% 
        str_remove_all("\\.") %>% 
        str_replace(",", ".") %>% 
        as.numeric(),
      area_ha = coalesce(area_ha, 0)
    ) %>%
    filter(!is.na(ano_criacao)) %>%
    mutate(
      cod_regiao = case_when(
        sg_uf %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "1",
        sg_uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "2",
        sg_uf %in% c("MG", "ES", "RJ", "SP") ~ "3",
        sg_uf %in% c("PR", "SC", "RS") ~ "4",
        sg_uf %in% c("MS", "MT", "GO", "DF") ~ "5",
        TRUE ~ NA_character_
      ),
      esfera_cat = case_when(
        str_detect(esfera, regex("^Fed", ignore_case = TRUE)) ~ "fed",
        str_detect(esfera, regex("^Est", ignore_case = TRUE)) ~ "est",
        str_detect(esfera, regex("^Mun", ignore_case = TRUE)) ~ "mun",
        TRUE ~ NA_character_
      ),
      grupo_cat = case_when(
        str_detect(grupo, regex("Integral", ignore_case = TRUE)) ~ "pi",
        str_detect(grupo, regex("Sustent", ignore_case = TRUE))  ~ "us",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(cod_regiao))
  
  base_regioes <- df_base
  base_brasil  <- df_base %>% mutate(cod_regiao = "76")
  df_total     <- bind_rows(base_regioes, base_brasil)
  
  b_tot <- df_total %>% mutate(recorte = "total")
  b_esf <- df_total %>% filter(!is.na(esfera_cat)) %>% mutate(recorte = esfera_cat)
  b_grp <- df_total %>% filter(!is.na(grupo_cat)) %>% mutate(recorte = grupo_cat)
  
  bind_rows(b_tot, b_esf, b_grp) %>%
    inner_join(tb_regioes, by = "cod_regiao") %>%
    group_by(cod_regiao, sg_regiao, nm_regiao, recorte, ano = ano_criacao) %>%
    summarise(
      novas_ucs = n(),
      nova_area = sum(area_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      nesting(cod_regiao, sg_regiao, nm_regiao, recorte),
      ano = min(ano):2026,
      fill = list(novas_ucs = 0, nova_area = 0)
    ) %>%
    arrange(cod_regiao, recorte, ano) %>%
    group_by(cod_regiao, sg_regiao, nm_regiao, recorte) %>%
    mutate(
      qtd_uc = cumsum(novas_ucs),
      area_uc = cumsum(nova_area)
    ) %>%
    ungroup() %>%
    pivot_longer(
      cols = c(qtd_uc, area_uc),
      names_to = "metrica",
      values_to = "indicador"
    ) %>%
    mutate(
      sigla_indicador = paste0(metrica, "_", recorte),
      mes_referencia = NA_character_,
      indicador = as.character(round(indicador, 2))
    ) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_cnuc_reg <- extrair_cnuc_regioes()

# 7. Extração de Indicadores Demográficos e Econômicos (IBGE - Regiões e Brasil)
extrair_ibge_regioes <- function() {
  df_raw <- if (str_ends(caminho_ibge, "\\.parquet$")) {
    read_parquet(caminho_ibge)
  } else {
    read_excel(caminho_ibge)
  }
  
  df_raw %>%
    filter(nivel_geo %in% c("Brasil", "Grande Região")) %>%
    mutate(
      cod_regiao = case_when(
        nome_local == "Brasil"                                      ~ "76",
        nome_local == "Norte"                                       ~ "1",
        nome_local == "Nordeste"                                    ~ "2",
        nome_local == "Sudeste"                                     ~ "3",
        nome_local == "Sul"                                         ~ "4",
        str_detect(nome_local, regex("^Centro", ignore_case = TRUE)) ~ "5",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(cod_regiao)) %>%
    inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
    select(-any_of(c("cod_ibge", "nivel_geo", "nome_local"))) %>%
    distinct() %>%
    pivot_longer(
      cols = -c(cod_regiao, sg_regiao, nm_regiao, ano),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_ibge_reg <- extrair_ibge_regioes()

# 8. Extração de Bases de Saúde por Tipo de Gestão (Regiões e Brasil) ----------
extrair_saude_gestao_regioes <- function(caminho_arquivo, prefixo_sigla) {
  df_raw <- suppressWarnings(
    read_csv(
      caminho_arquivo,
      col_types = cols(.default = col_character())
    )
  ) %>%
    rename_with(~ str_remove_all(., "[\u200B-\u200D\uFEFF]") %>% str_trim()) %>%
    clean_names()
  
  nome_col_valor <- names(df_raw)[ncol(df_raw)]
  
  df_base <- df_raw %>%
    rename(nm_uf = uf) %>%
    mutate(
      ano = as.numeric(ano),
      gestao = str_to_lower(str_trim(gestao)),
      valor_num = as.numeric(str_replace(str_trim(.data[[nome_col_valor]]), ",", "."))
    ) %>%
    filter(!is.na(valor_num)) %>%
    inner_join(tb_ufs %>% select(nm_uf, sg_uf) %>% distinct(), by = "nm_uf") %>%
    mutate(
      cod_regiao = case_when(
        sg_uf %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "1",
        sg_uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "2",
        sg_uf %in% c("MG", "ES", "RJ", "SP") ~ "3",
        sg_uf %in% c("PR", "SC", "RS") ~ "4",
        sg_uf %in% c("MS", "MT", "GO", "DF") ~ "5",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(cod_regiao))
  
  base_reg <- df_base
  base_br  <- df_base %>% mutate(cod_regiao = "76")
  df_total_reg <- bind_rows(base_reg, base_br)
  
  df_gestao <- df_total_reg %>%
    group_by(cod_regiao, gestao, ano) %>%
    summarise(valor_num = sum(valor_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(sigla_indicador = paste0(prefixo_sigla, "_", gestao))
  
  df_tot <- df_total_reg %>%
    group_by(cod_regiao, ano) %>%
    summarise(valor_num = sum(valor_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(sigla_indicador = paste0(prefixo_sigla, "_total"))
  
  bind_rows(df_gestao, df_tot) %>%
    inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
    mutate(
      mes_referencia = NA_character_,
      indicador = as.character(valor_num)
    ) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_saude_gestao_reg <- bind_rows(
  extrair_saude_gestao_regioes(caminho_consultas, "consultas_med"),
  extrair_saude_gestao_regioes(caminho_exames, "exames"),
  extrair_saude_gestao_regioes(caminho_cirurgias, "proc_cirurgicos")
)

# 9. Extração de Taxa de Escolarização no Ensino Médio (Regiões e Brasil) ------
extrair_escolarizacao_em_regioes <- function() {
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
        rename(Localidade_Orig = Localidade, cod_regiao = `Código IBGE`) %>%
        mutate(
          cod_regiao = as.character(cod_regiao),
          cod_regiao = if_else(Localidade_Orig == "Brasil", "76", cod_regiao)
        ) %>%
        inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
        select(cod_regiao, sg_regiao, nm_regiao, matches("^[0-9]{4}$")) %>%
        pivot_longer(
          cols = matches("^[0-9]{4}$"),
          names_to = "ano",
          values_to = "indicador"
        ) %>%
        mutate(
          ano = as.numeric(ano),
          sigla_indicador = sigla,
          mes_referencia = NA_character_,
          indicador = as.character(indicador)
        ) %>%
        filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
        select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
    }
  )
}

df_escolarizacao_reg <- extrair_escolarizacao_em_regioes()

# 10. Extração de MapBiomas (Regiões e Brasil - 6 Indicadores) -----------------
extrair_mapbiomas_regioes <- function() {
  df_raw <- if (file.exists(caminho_mapbiomas)) {
    read_parquet(caminho_mapbiomas)
  } else {
    read_excel(str_replace(caminho_mapbiomas, "\\.parquet$", ".xlsx"))
  }
  
  df_base <- df_raw %>%
    clean_names() %>%
    rename(nm_uf = state) %>%
    inner_join(tb_ufs %>% select(nm_uf, sg_uf) %>% distinct(), by = "nm_uf") %>%
    mutate(
      cod_regiao = case_when(
        sg_uf %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "1",
        sg_uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "2",
        sg_uf %in% c("MG", "ES", "RJ", "SP") ~ "3",
        sg_uf %in% c("PR", "SC", "RS") ~ "4",
        sg_uf %in% c("MS", "MT", "GO", "DF") ~ "5",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(cod_regiao))
  
  base_reg <- df_base
  base_br  <- df_base %>% mutate(cod_regiao = "76")
  df_total_reg <- bind_rows(base_reg, base_br)
  
  df_total_reg %>%
    group_by(cod_regiao, ano) %>%
    summarise(
      desmatamento_ha = sum(desmatamento_ha, na.rm = TRUE),
      recuperacao_ha  = sum(recuperacao_ha, na.rm = TRUE),
      area_total_ha   = sum(area_total_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(cod_regiao, ano) %>%
    group_by(cod_regiao) %>%
    mutate(
      desmatamento_pct     = (desmatamento_ha / area_total_ha) * 100,
      recuperacao_pct      = (recuperacao_ha / area_total_ha) * 100,
      desm_lag             = lag(desmatamento_ha),
      var_desmatamento_pct = ((desmatamento_ha - desm_lag) / desm_lag) * 100
    ) %>%
    select(-desm_lag) %>%
    ungroup() %>%
    inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
    select(
      cod_regiao,
      sg_regiao,
      nm_regiao,
      ano,
      desmatamento_ha,
      recuperacao_ha,
      area_total_ha,
      desmatamento_pct,
      var_desmatamento_pct,
      recuperacao_pct
    ) %>%
    pivot_longer(
      cols = -c(cod_regiao, sg_regiao, nm_regiao, ano),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_mapbiomas_reg <- extrair_mapbiomas_regioes()

# 11. Abastecimento de Água (SINISA - Regiões e Brasil) -----------------------
extrair_agua_sinisa_regioes <- function() {
  if (!file.exists(caminho_agua)) {
    cli_alert_warning("Arquivo do SINISA Água ausente em {.path {caminho_agua}}")
    return(tibble())
  }
  
  abas <- excel_sheets(caminho_agua)
  
  # --- 11.1 Aba Regiões (formato wide) ---
  aba_reg <- abas[str_detect(abas, regex("^regi[oõ]es?$", ignore_case = TRUE))]
  if (length(aba_reg) == 0) aba_reg <- abas[1]
  
  df_reg <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_agua, sheet = aba_reg, col_types = "text")
    )
  ) %>%
    clean_names() %>%
    rename(
      cod_regiao = cod_ibge,
      ano        = ano_referencia
    ) %>%
    mutate(
      cod_regiao = as.character(str_trim(cod_regiao)),
      ano        = as.numeric(ano)
    ) %>%
    inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, everything()) %>%
    pivot_longer(
      cols = c(
        iag0001, iag0002, iag0003, iag0004, iag0005, iag0006,
        iag2006, iag2013, ifa1001, ifa2002
      ),
      names_to  = "metrica_orig",
      values_to = "indicador_raw"
    )
  
  # --- 11.2 Aba Brasil (formato long: codigo | indicador | valor) ---
  aba_br <- abas[str_detect(abas, regex("^brasil$", ignore_case = TRUE))]
  
  df_br <- if (length(aba_br) > 0) {
    raw_br <- suppressWarnings(
      suppressMessages(
        read_excel(caminho_agua, sheet = aba_br[1], col_types = "text")
      )
    ) %>%
      clean_names()
    
    # Verifica se há coluna de ano na aba Brasil; se não houver, herda da aba Regiões
    col_ano <- names(raw_br)[str_detect(names(raw_br), "^ano")]
    ano_fallback <- if (nrow(df_reg) > 0) max(df_reg$ano, na.rm = TRUE) else NA_real_
    
    raw_br %>%
      mutate(
        cod_regiao   = "76",
        metrica_orig = str_to_lower(str_trim(codigo)),
        indicador_raw = valor,
        ano = if (length(col_ano) > 0) as.numeric(.data[[col_ano[1]]]) else ano_fallback
      ) %>%
      inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
      select(cod_regiao, sg_regiao, nm_regiao, ano, metrica_orig, indicador_raw)
  } else {
    tibble()
  }
  
  # --- 11.3 Padronização Unificada (Regiões + Brasil) ---
  bind_rows(df_reg, df_br) %>%
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
    filter(!is.na(sigla_indicador), !is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_agua_reg <- extrair_agua_sinisa_regioes()
cli_alert_success("Abastecimento de Água (Regiões e Brasil) processado.")

# 12. Esgoto (SINISA - Regiões e Brasil) --------------------------------------
extrair_esgoto_sinisa_regioes <- function() {
  if (!file.exists(caminho_esgoto)) {
    cli_alert_warning("Arquivo do SINISA Esgoto ausente em {.path {caminho_esgoto}}")
    return(tibble())
  }
  
  abas <- excel_sheets(caminho_esgoto)
  
  # --- 12.1 Aba Regiões (formato wide) ---
  aba_reg <- abas[str_detect(abas, regex("^regi[oõ]es?$", ignore_case = TRUE))]
  if (length(aba_reg) == 0) aba_reg <- abas[1]
  
  df_reg <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_esgoto, sheet = aba_reg, col_types = "text")
    )
  ) %>%
    clean_names() %>%
    rename(
      cod_regiao = cod_ibge,
      ano        = ano_referencia
    ) %>%
    mutate(
      cod_regiao = as.character(str_trim(cod_regiao)),
      ano        = as.numeric(ano)
    ) %>%
    inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, everything()) %>%
    pivot_longer(
      cols = c(
        ies0001, ies0002, ies0003, ies0004, ies0005, ies0006,
        ife1001, ife2002, ies2003, ies2004
      ),
      names_to  = "metrica_orig",
      values_to = "indicador_raw"
    )
  
  # --- 12.2 Aba Brasil (formato long: codigo | indicador | valor) ---
  aba_br <- abas[str_detect(abas, regex("^brasil$", ignore_case = TRUE))]
  
  df_br <- if (length(aba_br) > 0) {
    raw_br <- suppressWarnings(
      suppressMessages(
        read_excel(caminho_esgoto, sheet = aba_br[1], col_types = "text")
      )
    ) %>%
      clean_names()
    
    col_ano <- names(raw_br)[str_detect(names(raw_br), "^ano")]
    ano_fallback <- if (nrow(df_reg) > 0) max(df_reg$ano, na.rm = TRUE) else NA_real_
    
    raw_br %>%
      mutate(
        cod_regiao    = "76",
        metrica_orig  = str_to_lower(str_trim(codigo)),
        indicador_raw = valor,
        ano = if (length(col_ano) > 0) as.numeric(.data[[col_ano[1]]]) else ano_fallback
      ) %>%
      inner_join(tb_regioes %>% select(cod_regiao, sg_regiao, nm_regiao) %>% distinct(), by = "cod_regiao") %>%
      select(cod_regiao, sg_regiao, nm_regiao, ano, metrica_orig, indicador_raw)
  } else {
    tibble()
  }
  
  # --- 12.3 Padronização Unificada (Regiões + Brasil) ---
  bind_rows(df_reg, df_br) %>%
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
    filter(!is.na(sigla_indicador), !is.na(valor_num), !indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_regiao, sg_regiao, nm_regiao, ano, indicador, mes_referencia, sigla_indicador)
}

df_esgoto_reg <- extrair_esgoto_sinisa_regioes()
cli_alert_success("Esgoto (Regiões e Brasil) processado.")


# 13. Consolidação Final com nm_fantasia e polaridade -------------------------
df_indicadores_regioes <- bind_rows(
  df_ideb_regioes, 
  df_infantil_reg, 
  df_dependencia_reg, 
  df_cnuc_reg,
  df_ibge_reg,
  df_saude_gestao_reg,
  df_escolarizacao_reg,
  df_mapbiomas_reg,
  df_agua_reg,
  df_esgoto_reg
) %>%
  left_join(meta_indicadores, by = "sigla_indicador") %>%
  select(
    cod_regiao,
    sg_regiao,
    nm_regiao,
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
  arrange(sigla_indicador, cod_regiao, ano)

df_indicadores_regioes %>% glimpse()

# Salvar unificado ----
dir.create(file.path(DIR_OUT_AURORA, "parquet"), recursive = TRUE, showWarnings = FALSE)
df_indicadores_regioes %>%
  write_parquet(
    file.path(DIR_OUT_AURORA, "parquet/df_indicadores_regioes.parquet")
  )

# 
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
# sheet_id_destino <- "1668WVwrTafj_aycva5uHAgiBSYp1sPA0NrxwIBwAlSQ"
# 
# # 3. Escrita da base consolidada na primeira aba (gid = 0)
# googlesheets4::sheet_write(
#   data  = df_indicadores_regioes,
#   ss    = sheet_id_destino,
#   sheet = 1
# )
# 
# cli::cli_alert_success("Base {.val df_indicadores_ufs} exportada com sucesso para o Google Sheets!")

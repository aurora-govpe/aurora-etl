library(arrow)
library(readxl)
library(janitor)
library(tidyverse)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# 1. Caminhos -----------------------------------------------------------------
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

# 2. Dicionários e Metadados --------------------------------------------------
tb_ufs           <- read_excel(caminho_indicador, sheet = "dic_UFs")
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

# 3. Extração do IDEB (Estados) -----------------------------------------------
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
      cols = starts_with("VL_OBSERVADO_"), 
      names_to = "ano", 
      names_prefix = "VL_OBSERVADO_", 
      values_to = "indicador"
    ) %>%
    mutate(ano = as.numeric(ano)) %>%
    filter(!indicador %in% c("-", "ND", "nan", NA))
}

df_ideb_ufs <- bind_rows(
  extrair_ideb_uf("UF e Regiões (AI)", "5ano"),
  extrair_ideb_uf("UF e Regiões (AF)", "9ano"),
  extrair_ideb_uf("UF e Regiões (EM)", "3ano")
)

# 4. Extração de Educação Infantil (Estados) ----------------------------------
extrair_infantil_uf <- function(caminho_arquivo, prefixo_sigla) {
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
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_infantil_ufs <- bind_rows(
  extrair_infantil_uf(caminho_creches, "tx_creche"),
  extrair_infantil_uf(caminho_pre_escola, "tx_pre_escola"),
  extrair_infantil_uf(caminho_liq_creches, "tx_liq_creche"),
  extrair_infantil_uf(caminho_liq_pre_escola, "tx_liq_pre_escola")
)

# 5. Extração por Dependência Administrativa (Estados) ------------------------
extrair_dependencia_uf <- function(caminho_arquivo, prefixo_sigla) {
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
      select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
  })
}

df_dependencia_ufs <- bind_rows(
  extrair_dependencia_uf(caminho_abandono, "aba_em"),
  extrair_dependencia_uf(caminho_distorcao, "tdi_em")
)

# 6. Extração de Unidades de Conservação (Estados) ----------------------------
extrair_cnuc_uf <- function() {
  df_raw <- suppressWarnings(
    read_delim(
      caminho_cnuc, 
      delim = ";", 
      locale = locale(encoding = "latin1"),
      col_types = cols(.default = col_character())
    )
  )
  
  df_limpo <- df_raw %>%
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
      grupo_cat = case_when(
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
      .groups = "drop"
    ) %>%
    complete(
      nesting(cod_uf, sg_uf, nm_uf, recorte),
      ano = min(ano):2026,
      fill = list(novas_ucs = 0, nova_area = 0)
    ) %>%
    arrange(cod_uf, recorte, ano) %>%
    group_by(cod_uf, sg_uf, nm_uf, recorte) %>%
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
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_cnuc_ufs <- extrair_cnuc_uf()

# 7. Extração de Indicadores Demográficos e Econômicos (IBGE - Estados) -------
extrair_ibge_uf <- function() {
  # Lê parquet ou excel de forma transparente
  df_raw <- if (str_ends(caminho_ibge, "\\.parquet$")) {
    read_parquet(caminho_ibge)
  } else {
    read_excel(caminho_ibge)
  }
  
  df_raw %>%
    # 1. Cria cod_uf explicitamente antes do join
    mutate(cod_uf = as.character(cod_ibge)) %>%
    # 2. Cruza por cod_uf (garantindo que cod_uf permaneça no data frame)
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "cod_uf") %>%
    # 3. Remove apenas as colunas de identificação original
    select(-any_of(c("nivel_geo", "nome_local", "cod_ibge"))) %>%
    distinct() %>%
    # 4. Pivota dinamicamente todas as 26 variáveis numéricas
    pivot_longer(
      cols = -c(cod_uf, sg_uf, nm_uf, ano),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    # 5. Remove anos/células sem dados observados
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_ibge_ufs <- extrair_ibge_uf()

# 8. Extração de Segurança Pública (Estados) ----------------------------------

# 8.1 Mortes Violentas Intencionais (MVI)
extrair_mvi_uf <- function() {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_seguranca, sheet = "mvi")
    )
  )
  
  df_raw %>%
    rename(
      sg_uf = sigla_uf,
      mvi   = n_ab_mvi
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      mvi    = as.character(mvi) %>% str_remove_all("\\."),
      tx_mvi = as.character(tx_mvi) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(
      cols = c(mvi, tx_mvi),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

# 8.2 Roubo e Furto de Veículos (RFV)
extrair_rfv_uf <- function() {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_seguranca, sheet = "rfv")
    )
  )
  
  df_raw %>%
    rename(
      sg_uf = sigla_uf,
      rfv   = n_ab_rfv
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      rfv    = as.character(rfv) %>% str_remove_all("\\."),
      tx_rfv = as.character(tx_rfv) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(
      cols = c(rfv, tx_rfv),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

# 8.3 Roubo e Furto de Celular (Celulares)
extrair_celulares_uf <- function() {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_seguranca, sheet = "celulares")
    )
  )
  
  df_raw %>%
    rename(
      sg_uf        = sigla_uf,
      rf_celular   = rf_celulares,
      tx_rfcelular = taxa_rf_celulares
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      rf_celular   = as.character(rf_celular) %>% str_remove_all("\\."),
      tx_rfcelular = as.character(tx_rfcelular) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(
      cols = c(rf_celular, tx_rfcelular),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(ano >= 2020) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

# 8.4 Crimes Contra o Patrimônio (CVP)
extrair_cvp_uf <- function() {
  df_raw <- suppressWarnings(
    suppressMessages(
      read_excel(caminho_seguranca, sheet = "cvp")
    )
  )
  
  df_raw %>%
    rename(
      sg_uf  = sigla_uf,
      cvp    = n_ab_roubo_tot,
      tx_cvp = tx_roubo_tot
    ) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "sg_uf") %>%
    mutate(
      cvp    = as.character(cvp) %>% str_remove_all("\\."),
      tx_cvp = as.character(tx_cvp) %>% str_replace_all(",", ".")
    ) %>%
    pivot_longer(
      cols = c(cvp, tx_cvp),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_mvi_ufs       <- extrair_mvi_uf()
df_rfv_ufs       <- extrair_rfv_uf()
df_celulares_ufs <- extrair_celulares_uf()
df_cvp_ufs       <- extrair_cvp_uf()

# 9. Extração de Bases de Saúde por Tipo de Gestão (Estados) ------------------
extrair_saude_gestao_uf <- function(caminho_arquivo, prefixo_sigla) {
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
      indicador = as.character(valor_num)
    ) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_saude_gestao_ufs <- bind_rows(
  extrair_saude_gestao_uf(caminho_consultas, "consultas_med"),
  extrair_saude_gestao_uf(caminho_exames, "exames"),
  extrair_saude_gestao_uf(caminho_cirurgias, "proc_cirurgicos")
)

# 9. Extração de Taxa de Escolarização no Ensino Médio (Estados) --------------
extrair_escolarizacao_em_uf <- function() {
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
        select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
    }
  )
}

df_escolarizacao_ufs <- extrair_escolarizacao_em_uf()

caminho_mapbiomas <- "data/meio ambiente/indicadores_mapbiomas.parquet"

extrair_mapbiomas_uf <- function() {
  df_raw <- if (file.exists(caminho_mapbiomas)) {
    read_parquet(caminho_mapbiomas)
  } else {
    read_excel(str_replace(caminho_mapbiomas, "\\.parquet$", ".xlsx"))
  }
  
  df_raw %>%
    clean_names() %>%
    rename(nm_uf = state) %>%
    inner_join(tb_ufs %>% select(cod_uf, sg_uf, nm_uf) %>% distinct(), by = "nm_uf") %>%
    # Seleciona apenas as chaves territoriais, ano e os 6 indicadores desejados
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
      cols = -c(cod_uf, sg_uf, nm_uf, ano),
      names_to = "sigla_indicador",
      values_to = "indicador"
    ) %>%
    mutate(
      ano = as.numeric(ano),
      mes_referencia = NA_character_,
      indicador = as.character(indicador)
    ) %>%
    filter(!indicador %in% c("-", "ND", "nan", "NA", NA)) %>%
    select(cod_uf, sg_uf, nm_uf, ano, indicador, mes_referencia, sigla_indicador)
}

df_mapbiomas_ufs <- extrair_mapbiomas_uf()

# 10. Consolidação Final com nm_fantasia e polaridade -------------------------
df_indicadores_ufs <- bind_rows(
  df_ideb_ufs, 
  df_infantil_ufs, 
  df_dependencia_ufs, 
  df_cnuc_ufs,
  df_ibge_ufs,
  df_mvi_ufs,
  df_rfv_ufs,
  df_celulares_ufs,
  df_cvp_ufs,
  df_saude_gestao_ufs,
  df_escolarizacao_ufs,
  df_mapbiomas_ufs
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

df_indicadores_ufs %>% glimpse()

# Salvar unificado ------------------------------------------------------------
dir.create(file.path(DIR_OUT_AURORA, "parquet"), recursive = TRUE, showWarnings = FALSE)
df_indicadores_ufs %>%
  write_parquet(
    file.path(DIR_OUT_AURORA, 'parquet/df_indicadores_ufs.parquet')
  )

df_indicadores_ufs %>% View()

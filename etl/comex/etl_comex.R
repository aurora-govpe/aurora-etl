library(arrow)
library(comexr)
library(geobr)
library(sf)
library(stringi)
library(tidyverse)

# 1. Função com Tratamento de Erro HTTP 429 e Pausas Preventivas
consultar_comex_safe <- function(ano, flow, max_tentativas = 5, pausa_base = 11) {
  tentativa <- 1
  sucesso <- FALSE
  resultado <- NULL
  
  while (!sucesso && tentativa <= max_tentativas) {
    message(sprintf("--> Consultando %s | Ano: %d | Tentativa: %d/%d", flow, ano, tentativa, max_tentativas))
    
    resultado <- tryCatch({
      out <- comexr::comex_query_city(
        flow         = flow,
        start_period = sprintf("%d-01", ano),
        end_period   = sprintf("%d-12", ano),
        filters      = list(state = "26"), # 26 = Pernambuco
        details      = c("city", "heading", "section"),
        month_detail = FALSE,
        language     = "pt",
        verbose      = FALSE
      )
      sucesso <- TRUE
      out
    }, error = function(e) {
      if (grepl("429", e$message)) {
        tempo_espera <- pausa_base * tentativa
        message(sprintf("   ⚠️ Limite de API excedido (HTTP 429). Aguardando %d segundos...", tempo_espera))
        Sys.sleep(tempo_espera)
      } else {
        message(sprintf("   ⚠️ Erro na requisição: %s. Aguardando 5s...", e$message))
        Sys.sleep(5)
      }
      return(NULL)
    })
    
    tentativa <- tentativa + 1
  }
  
  Sys.sleep(3)
  
  if (is.null(resultado)) {
    warning(sprintf("Falha para %s no ano %d após %d tentativas.", flow, ano, max_tentativas))
    return(tibble())
  }
  
  return(resultado %>% mutate(ano = ano, flow = flow))
}

# 2. Execução Iterativa Cadenciada (2022 a 2025)
anos <- 2022:2026

df_exp_pe <- map_dfr(anos, ~consultar_comex_safe(ano = .x, flow = "export"))
Sys.sleep(10) # Pausa entre fluxos
df_imp_pe <- map_dfr(anos, ~consultar_comex_safe(ano = .x, flow = "import"))

# 3. Tabela De-Para de Nomes de Municípios para Código IBGE de PE (geobr)
malha_pe <- geobr::read_municipality(code_muni = "PE", year = 2022, showProgress = FALSE) %>%
  st_drop_geometry() %>%
  mutate(
    nm_clean = stri_trans_general(name_muni, "Latin-ASCII") %>% str_to_upper() %>% str_trim()
  ) %>%
  select(id_municipio = code_muni, nm_clean, name_muni)

# 4. Taxonomia OCDE / MDIC (Capítulos NCM - SH2)
sh2_high_tech <- c("29", "30", "38", "84", "85", "86", "87", "88", "90")

# 5. Processamento de Exportações e Concentração HH
exp_indicadores <- df_exp_pe %>%
  mutate(
    nm_clean = str_remove(noMunMinsgUf, " - PE") %>% 
      stri_trans_general("Latin-ASCII") %>% 
      str_to_upper() %>% 
      str_trim(),
    sh2 = substr(headingCode, 1, 2),
    is_high_tech = sh2 %in% sh2_high_tech,
    valor_fob = as.numeric(metricFOB)
  ) %>%
  # Consolidação por Posição NCM (4 dígitos) para cálculo preciso do HH
  group_by(noMunMinsgUf, nm_clean, ano, headingCode, is_high_tech) %>%
  summarise(valor_fob_ncm = sum(valor_fob, na.rm = TRUE), .groups = "drop") %>%
  group_by(noMunMinsgUf, nm_clean, ano) %>%
  summarise(
    exp_total = sum(valor_fob_ncm, na.rm = TRUE),
    exp_alta_tec = sum(valor_fob_ncm[is_high_tech], na.rm = TRUE),
    hh_exp_alta_tec = if_else(
      exp_alta_tec > 0,
      sum((valor_fob_ncm[is_high_tech] / exp_alta_tec)^2, na.rm = TRUE),
      0
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pct_exp_alta_tec = if_else(exp_total > 0, (exp_alta_tec / exp_total) * 100, 0),
    div_exp_alta_tec = if_else(exp_alta_tec > 0, 1 - hh_exp_alta_tec, 0)
  )

# 6. Processamento de Importações
imp_indicadores <- df_imp_pe %>%
  mutate(
    nm_clean = str_remove(noMunMinsgUf, " - PE") %>% 
      stri_trans_general("Latin-ASCII") %>% 
      str_to_upper() %>% 
      str_trim(),
    sh2 = substr(headingCode, 1, 2),
    is_high_tech = sh2 %in% sh2_high_tech,
    valor_fob = as.numeric(metricFOB)
  ) %>%
  group_by(noMunMinsgUf, nm_clean, ano) %>%
  summarise(
    imp_total = sum(valor_fob, na.rm = TRUE),
    imp_alta_tec = sum(valor_fob[is_high_tech], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_imp_alta_tec = if_else(imp_total > 0, (imp_alta_tec / imp_total) * 100, 0)
  )

# 7. Consolidação e Atribuição do Código IBGE de Pernambuco
painel_comex_pe <- exp_indicadores %>%
  full_join(imp_indicadores, by = c("noMunMinsgUf", "nm_clean", "ano")) %>%
  left_join(malha_pe, by = "nm_clean") %>%
  replace_na(list(
    exp_total = 0, exp_alta_tec = 0, pct_exp_alta_tec = 0, hh_exp_alta_tec = 0, div_exp_alta_tec = 0,
    imp_total = 0, imp_alta_tec = 0, pct_imp_alta_tec = 0
  )) %>%
  select(
    id_municipio, name_muni, noMunMinsgUf, ano,
    exp_total, exp_alta_tec, pct_exp_alta_tec, hh_exp_alta_tec, div_exp_alta_tec,
    imp_total, imp_alta_tec, pct_imp_alta_tec
  )

painel_comex_long <- painel_comex_pe %>%
  # Padroniza as chaves de identificação territorial e temporal
  mutate(
    cod_municipio  = as.character(id_municipio),
    nm_municipio   = name_muni,
    ano            = as.numeric(ano),
    mes_referencia = NA_character_
  ) %>%
  # Converte todas as métricas para <chr> antes de empilhar (evita incompatibilidade de tipo)
  mutate(across(
    c(exp_total, exp_alta_tec, pct_exp_alta_tec, 
      hh_exp_alta_tec, div_exp_alta_tec, 
      imp_total, imp_alta_tec, pct_imp_alta_tec), 
    as.character
  )) %>%
  # Pivotagem para o formato Long (Tidy Data)
  pivot_longer(
    cols = c(exp_total, exp_alta_tec, pct_exp_alta_tec, 
             hh_exp_alta_tec, div_exp_alta_tec, 
             imp_total, imp_alta_tec, pct_imp_alta_tec),
    names_to  = "sigla_indicador",
    values_to = "indicador"
  ) %>%
  select(cod_municipio, nm_municipio, ano, sigla_indicador, indicador, mes_referencia)

# 2. Junção com a Planilha de Metadados / Dicionário (opcional)
lista_de_indicadores <- readxl::read_excel("data/indicadores/lista de indicadores.xlsx")

df_comex_long <- painel_comex_long %>%
  left_join(
    lista_de_indicadores %>% select(sigla_indicador, nm_indicador, grupo, tipo),
    by = "sigla_indicador"
  )

df_comex_long %>%
  write_parquet(
    'data/comex/df_comex_long.parquet'
  )

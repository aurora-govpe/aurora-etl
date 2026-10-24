# ---- 1. SETUP & EXTRAÇÃO ----
# Instale o pacote se necessário: install.packages("jsonlite")
library(tidyverse)
library(readxl)
library(janitor)
library(jsonlite)

df_compesa_bruto <- read_excel("data/compesa/portfolio_projetos_compesa.xlsx")

# ---- 2. TRANSFORMAÇÃO ----

## ---- 2.1. Limpeza Base Principal ----
df_compesa_projetos <- df_compesa_bruto %>%
  clean_names() %>% 
  mutate(
    id_projeto = row_number(),
    valor_total_m = as.numeric(str_replace(str_replace_all(valor_loa_r_m, "\\.", ""), ",", ".")),
    populacao_beneficiada = as.numeric(str_replace_all(populacao_beneficiada, "\\.", "")),
    percentual_executado = as.numeric(str_remove(executado_percent, "%")) / 100,
    ano_termino = str_extract(previsao_termino, "\\d{2}$"),
    ano_termino = if_else(!is.na(ano_termino), paste0("20", ano_termino), NA_character_)
  )

## ---- 2.2. Tabela Relacional: Municípios ----
df_compesa_municipios <- df_compesa_projetos %>%
  select(id_projeto, municipios_atendidos, valor_total_m) %>%
  mutate(muns_limpo = str_replace_all(municipios_atendidos, "\\(.*?\\)| e ", ",")) %>%
  separate_rows(muns_limpo, sep = ",") %>%
  mutate(municipio = str_trim(muns_limpo)) %>%
  filter(municipio != "") %>%
  group_by(id_projeto) %>%
  mutate(valor_rateado_m = valor_total_m / n()) %>%
  ungroup() %>%
  select(id_projeto, municipio, valor_rateado_m)

## ---- 2.3. Tabela Relacional: Desembolso ----
df_compesa_desembolso <- df_compesa_projetos %>%
  select(id_projeto, detalhamento_desembolso) %>%
  drop_na(detalhamento_desembolso) %>%
  separate_rows(detalhamento_desembolso, sep = "\\|") %>%
  mutate(linha = str_trim(detalhamento_desembolso)) %>%
  filter(str_detect(linha, "^20\\d{2}")) %>%
  mutate(
    ano_desembolso = as.integer(str_extract(linha, "^20\\d{2}")),
    valor_desembolso_m = as.numeric(str_replace(str_extract(linha, "(?<=R\\$\\s?)[0-9,]+"), ",", "."))
  ) %>%
  select(id_projeto, ano_desembolso, valor_desembolso_m)

## ---- 2.4. Finalização da Base Principal ----
df_compesa_projetos <- df_compesa_projetos %>%
  select(
    id_projeto, diretoria, nome_do_projeto, status, fonte_de_recursos, 
    carteira, objetivo, valor_total_m, populacao_beneficiada, 
    percentual_executado, ano_termino
  )

# ---- 3. CARREGAMENTO (JSON) ----
# Salvando em formato JSON (pretty = TRUE facilita a leitura e o debug, mas pode ser FALSE para produção)
write_json(df_compesa_projetos, "app_aurora_municipios/data/compesa_projetos_clean.json", pretty = TRUE)
write_json(df_compesa_municipios, "app_aurora_municipios/data/compesa_municipios_rel.json", pretty = TRUE)
write_json(df_compesa_desembolso, "app_aurora_municipios/data/compesa_desembolso_rel.json", pretty = TRUE)
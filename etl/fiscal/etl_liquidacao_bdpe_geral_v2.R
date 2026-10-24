# versão jdbc ----
library(arrow)
library(BigDataPE)
library(DBI)
library(glue)
library(janitor)
library(stringr) 
library(tidyverse)

## 1. Carga dos Parquets de Liquidação ----
liquidacao_2020_bdpe <- read_parquet('data/efisco/liquidacao_2020_bdpe.parquet') %>%
  mutate(ano = 2020, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2021_bdpe <- read_parquet('data/efisco/liquidacao_2021_bdpe.parquet') %>%
  mutate(ano = 2021, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2022_bdpe <- read_parquet('data/efisco/liquidacao_2022_bdpe.parquet') %>%
  mutate(ano = 2022, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2023_bdpe <- read_parquet('data/efisco/liquidacao_2023_bdpe.parquet') %>%
  mutate(ano = 2023, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2024_bdpe <- read_parquet('data/efisco/liquidacao_2024_bdpe.parquet') %>%
  mutate(ano = 2024, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2025_bdpe <- read_parquet('data/efisco/liquidacao_2025_bdpe.parquet') %>%
  mutate(ano = 2025, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

liquidacao_2026_bdpe <- read_parquet('data/efisco/liquidacao_2026_bdpe.parquet') %>%
  mutate(ano = 2026, chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd))

## 2. Consolidação da Base Fato ----
liquidacao_bdpe <- liquidacao_2026_bdpe %>%
  bind_rows(
    liquidacao_2025_bdpe, 
    liquidacao_2024_bdpe,
    liquidacao_2023_bdpe,
    liquidacao_2022_bdpe,
    liquidacao_2021_bdpe,
    liquidacao_2020_bdpe
  ) %>%
  select(
    ano, chave_contrato, gbplicit_cd, gbpcontr_nu_ano, gbpcontr_sq, gbpcontr_in_tac, sg_ugc, 
    ug_cd_coordenadora, ug_cd, ug_ds, despgern_nm, gpoacao_cd, gpoacaocad_nm, gposubacao_cd, 
    gposubcad_nm, fichafinan_in_dea, fonterecur_cd, fonterecur_nm, nu_documento_credor, 
    pessoa_nm_razao_social, gfuliqd_dt, mes_liquidacao, grdesp_cd, modalidade_aplicacao, 
    elemdesp_id, valor_liquido_le, ano_mes_documento_origem
  ) %>%
  mutate(
    valor_liquido_le_2 = as.numeric(str_replace(valor_liquido_le, ",", ".")),
    ano_mes_documento_origem = ym(ano_mes_documento_origem),
    ano_mes_liquidacao = ym(paste0(ano, mes_liquidacao))
  )

## 3. Carga das Bases Auxiliares ----
dados_contratos <- read.csv2(
  'data/efisco/EF_CONTRATO.csv',
  sep = '|',
  encoding = 'UTF-8'
) %>%
  clean_names() %>%
  mutate(contrato_limpo = str_remove(contrato, "^0+")) %>%
  arrange(contrato_limpo, numero_do_subcontrato_tac) %>%
  distinct(contrato_limpo, .keep_all = TRUE)

temas_gasto <- read.csv2(
  "data/efisco/orcamento_temas_subtemas.csv", 
  sep = ";",
  encoding = "UTF-8"
) %>%
  clean_names() %>%
  rename(
    gasto_tema = tema_orc,
    gasto_subtema = sub_tema_orc
  ) %>%
  mutate(across(c(gasto_tema, gasto_subtema), str_squish))

## 4. Junção, Agregação e Criação de Dimensões ----
liquidacao_consolidada <- liquidacao_bdpe %>%
  left_join(
    dados_contratos %>%
      select(
        contrato_limpo, contrato, tipo_de_contrato_tac, ano_do_contrato, 
        valor_do_contrato, objeto_da_licitacao, data_de_inclusao_do_contrato
      ), 
    by = c("chave_contrato" = "contrato_limpo")
  ) %>%
  mutate(
    contrato = tidyr::replace_na(contrato, "Sem contrato"),
    is_contrato = contrato != "Sem contrato",
    chave_2 = paste(
      gpoacao_cd, 
      if_else(gposubacao_cd == "0000", "0", as.character(gposubacao_cd)), 
      grdesp_cd, 
      sep = "-"
    )
  ) %>%
  select(-c(gbplicit_cd, gbpcontr_nu_ano, gbpcontr_sq, gbpcontr_in_tac, gfuliqd_dt, mes_liquidacao, valor_liquido_le)) %>%
  group_by(across(-valor_liquido_le_2)) %>%
  summarise(
    valor_liquido_le_2 = sum(valor_liquido_le_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    temas_gasto %>% select(chave_2, gasto_tema, gasto_subtema), 
    by = "chave_2"
  ) %>%
  mutate(
    across(c(gasto_tema, gasto_subtema), ~ tidyr::replace_na(.x, "Sem tema de gasto"))
  ) %>%
  
  # 5. OTIMIZAÇÕES PARA PERFORMANCE DA API ----
mutate(
  # Trata valores nulos de texto
  pessoa_nm_razao_social = coalesce(str_squish(pessoa_nm_razao_social), "NÃO INFORMADO"),
  nu_documento_credor = coalesce(str_squish(nu_documento_credor), ""),
  objeto_da_licitacao = coalesce(str_squish(objeto_da_licitacao), "Sem objeto"),
  
  # Rótulos pré-formatados de Ação e Subação (elimina paste() na API)
  acao_label = if_else(!is.na(gpoacao_cd), paste0(gpoacao_cd, " - ", gpoacaocad_nm), "Sem Ação"),
  subacao_label = if_else(!is.na(gposubacao_cd), paste0(gposubacao_cd, " - ", gposubcad_nm), "Sem Subação"),
  
  # Lista/Rótulo padronizado para Fornecedores
  credor_label = if_else(
    nu_documento_credor != "", 
    paste0(nu_documento_credor, " - ", pessoa_nm_razao_social), 
    pessoa_nm_razao_social
  ),
  
  # Lista/Rótulo padronizado para Contratos
  contrato_label = if_else(
    is_contrato == TRUE,
    paste0(chave_contrato, " - ", objeto_da_licitacao),
    "Sem contrato"
  ),
  
  # IDs inteiros sequenciais (Surrogate Keys) para agrupamentos rápidos em DuckDB
  credor_id = dense_rank(pessoa_nm_razao_social),
  contrato_id = if_else(is_contrato == TRUE, dense_rank(chave_contrato), 0L)
)

## 6. Exportação do Dataset Otimizado ----
write_parquet(liquidacao_consolidada, 'data/efisco/contratos_bdpe.parquet')
write_parquet(liquidacao_consolidada, 'app_aurora_orcamento/data/contratos_bdpe.parquet')

cat("[SUCCESS] ETL concluído com sucesso! Dataset gerado com labels e IDs numéricos.\n")
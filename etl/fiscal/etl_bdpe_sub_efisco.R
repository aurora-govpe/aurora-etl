# versão jdbc ----
library(BigDataPE)
library(DBI)
library(glue)
library(janitor)
library(stringr) 
library(tidyverse)

## final ----
liquidacao_2020_bdpe <- read_parquet('data/efisco/liquidacao_2020_bdpe.parquet') %>%
  mutate(
    ano  = 2020,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2020_bdpe %>% glimpse()

liquidacao_2021_bdpe <- read_parquet('data/efisco/liquidacao_2021_bdpe.parquet') %>%
  mutate(
    ano  = 2021,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2021_bdpe %>% glimpse()

liquidacao_2022_bdpe <- read_parquet('data/efisco/liquidacao_2022_bdpe.parquet') %>%
  mutate(
    ano  = 2022,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2022_bdpe %>% glimpse()

liquidacao_2023_bdpe <- read_parquet('data/efisco/liquidacao_2023_bdpe.parquet') %>%
  mutate(
    ano  = 2023,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2023_bdpe %>% glimpse()

liquidacao_2024_bdpe <- read_parquet('data/efisco/liquidacao_2024_bdpe.parquet') %>%
  mutate(
    ano  = 2024,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2024_bdpe %>% glimpse()

liquidacao_2025_bdpe <- read_parquet('data/efisco/liquidacao_2025_bdpe.parquet') %>%
  mutate(
    ano  = 2025,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2025_bdpe %>% glimpse()

liquidacao_2026_bdpe <- read_parquet('data/efisco/liquidacao_2026_bdpe.parquet') %>%
  mutate(
    ano  = 2026,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, sg_ugc, ug_cd)
  )

liquidacao_2026_bdpe %>% glimpse()

compare_df_cols(
  liquidacao_2026_bdpe, 
  liquidacao_2025_bdpe, 
  liquidacao_2024_bdpe,
  liquidacao_2023_bdpe,
  liquidacao_2022_bdpe,
  liquidacao_2021_bdpe,
  liquidacao_2020_bdpe)

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
    ano, chave_contrato, gbplicit_cd, gbpcontr_nu_ano, gbpcontr_sq, gbpcontr_in_tac, sg_ugc, ug_cd_coordenadora, ug_cd, ug_ds, despgern_nm, gpoacao_cd, gpoacaocad_nm, gposubacao_cd , gposubcad_nm, fichafinan_in_dea, fonterecur_cd, fonterecur_nm, nu_documento_credor, pessoa_nm_razao_social, gfuliqd_dt, mes_liquidacao, grdesp_cd, modalidade_aplicacao, elemdesp_id, elemdesp_id, valor_liquido_le, ano_mes_documento_origem
  ) %>%
  mutate(
    valor_liquido_le_2 = as.numeric(str_replace(valor_liquido_le, ",", ".")),
    ano_mes_documento_origem = ym(ano_mes_documento_origem),
    ano_mes_liquidacao = ym(paste0(ano, mes_liquidacao))
  )

liquidacao_bdpe %>% glimpse()

dados_contratos <- read.csv2(
  'data/efisco/EF_CONTRATO.csv',
  sep = '|',
  encoding = 'UTF-8'
) %>%
  clean_names() %>%
  mutate(
    contrato_limpo = str_remove(contrato, "^0+")
  ) %>%
  # Ordena para garantir que a linha principal do contrato venha primeiro
  arrange(contrato_limpo, numero_do_subcontrato_tac) %>%
  # Remove as duplicatas (subcontratos), mantendo todas as outras colunas
  distinct(contrato_limpo, .keep_all = TRUE)

dados_contratos %>% glimpse()

liquidacao_consolidada <- liquidacao_bdpe %>%
  # 1. Junta as bases indicando as chaves correspondentes
  left_join(
    dados_contratos %>%
      select(
        contrato_limpo, contrato, tipo_de_contrato_tac, ano_do_contrato, 
        valor_do_contrato, objeto_da_licitacao, data_de_inclusao_do_contrato
      ), 
    by = c("chave_contrato" = "contrato_limpo")
  ) %>%
  
  # 2. Preenche "Sem contrato" APENAS na coluna 'contrato' e cria a flag 'is_contrato'
  mutate(
    contrato = tidyr::replace_na(contrato, "Sem contrato"),
    is_contrato = contrato != "Sem contrato"
  ) %>%
  
  # 3. Agrupa por todas as colunas da base, EXCETO a coluna de valor
  group_by(across(-valor_liquido_le_2)) %>%
  
  # 4. Soma os valores, reduzindo múltiplas liquidações idênticas para uma única linha
  summarise(
    valor_liquido_le_2 = sum(valor_liquido_le_2, na.rm = TRUE),
    .groups = "drop"
  )

liquidacao_consolidada %>% glimpse()

liquidacao_consolidada %>% count(is_contrato)

liquidacao_consolidada %>% write_parquet('data/efisco/liquidacao_bdpe.parquet')

liquidacao_consolidada %>% write_parquet('app_aurora_orcamento/data/liquidacao_bdpe.parquet')
  
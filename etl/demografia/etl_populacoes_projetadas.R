library(arrow)
library(dplyr)
library(ibger)
library(purrr)
library(tidyr)

# 1. SETUP E VETORES DE CONTROLE ----

## 1.1 Carga de municípios ----
vetor_muni_pe <- readRDS('data/ibge/lista_muni_pe.rds')

## 1.2 Mapeamento de categorias etárias IBGE ----
# Tabela 200 (Censo 2010 - Classificação 58)
categorias_idade_200 <- c(
  '1140', '1141', '1142', '1143', '1144', '1145', '1146', 
  '1147', '1148', '1149', '1150', '1151', '1152', '1153', 
  '1154', '1155', '2503', '6802', '6803', '92963', '92964', '92965'
)

# Tabela 9514 (Censo 2022 - Classificação 287)
categorias_idade_9514 <- c(
  '93070', '93084', '93085', '93086', '93087', '93088', '93089', 
  '93090', '93091', '93092', '93093', '93094', '93095', '93096', 
  '93097', '93098', '49108', '49109', '60040', '60041', '6653'
)


# 2. INGESTÃO DOS DADOS VIA IBGER ----

## 2.1 Censo Demográfico 2022 (Tabela 9514) ----
message("--> Extraindo Censo 2022...")
dados_populacao_2022_pe <- map_dfr(categorias_idade_9514, function(cat_idade) {
  ibge_variables(
    aggregate = 9514,
    variable = 93,
    periods = 2022,
    localities = list(N6 = vetor_muni_pe),
    classification = list(
      "2"   = c('4', '5'),
      "287" = cat_idade
    )
  )
})

write_parquet(dados_populacao_2022_pe, 'data/ibge/dados_populacao_2022_pe.parquet')

## 2.2 Censo Demográfico 2010 (Tabela 200) ----
message("--> Extraindo Censo 2010...")
dados_populacao_2010_pe <- map_dfr(categorias_idade_200, function(cat_idade) {
  ibge_variables(
    aggregate = 200,
    variable = 93,
    periods = 2010,
    localities = list(N6 = vetor_muni_pe),
    classification = list(
      "2"  = c('4', '5'),
      "58" = cat_idade
    )
  )
})

write_parquet(dados_populacao_2010_pe, 'data/ibge/dados_populacao_2010_pe.parquet')

## 2.3 Estimativas Populacionais IBGE para Controle (Tabela 6579, Var 9324) ----
message("--> Extraindo Estimativas Municipais do IBGE (Tabela 6579, Var 9324)...")
estimativas_ibge_raw <- ibge_variables(
  aggregate = 6579,
  variable = 9324,
  periods = c(2023, 2024, 2025, 2026),
  localities = list(N6 = vetor_muni_pe)
)

df_controle_ibge <- estimativas_ibge_raw %>%
  mutate(
    locality_id  = as.character(locality_id),
    ano          = as.character(period),
    pop_controle = parse_ibge_value(value)
  ) %>%
  select(locality_id, ano, pop_controle)


# 3. TRATAMENTO E MODELAGEM DE TAXAS ----

## 3.1 Harmonização Censo 2022 ----
df_2022 <- dados_populacao_2022_pe %>%
  mutate(
    locality_id = as.character(locality_id),
    sexo        = classification_2,
    coorte      = classification_287,
    pop_2022    = parse_ibge_value(value)
  ) %>%
  group_by(locality_id, locality_name, sexo, coorte) %>%
  summarise(pop_2022 = sum(pop_2022, na.rm = TRUE), .groups = "drop")

## 3.2 Harmonização Censo 2010 ----
df_2010 <- dados_populacao_2010_pe %>%
  mutate(
    locality_id = as.character(locality_id),
    sexo        = classification_2,
    coorte      = classification_58,
    pop_2010    = parse_ibge_value(value)
  ) %>%
  group_by(locality_id, sexo, coorte) %>%
  summarise(pop_2010 = sum(pop_2010, na.rm = TRUE), .groups = "drop")

## 3.3 Matriz de taxas anuais (r_anual) ----
matriz_taxas <- df_2022 %>%
  inner_join(df_2010, by = c("locality_id", "sexo", "coorte")) %>%
  mutate(
    ccr_12_anos  = if_else(pop_2010 > 0, pop_2022 / pop_2010, 1.0),
    ccr_12_trava = case_when(
      ccr_12_anos > 1.6 ~ 1.6,
      ccr_12_anos < 0.4 ~ 0.4,
      TRUE ~ ccr_12_anos
    ),
    r_anual = (ccr_12_trava)^(1 / 12)
  )


# 4. PROJEÇÃO POPULACIONAL E CALIBRAÇÃO (2023 - 2027) ----

## 4.1 Projeção bruta por coorte (Sem Controle) ----
anos_projecao <- 2023:2027

projecoes_brutas <- map_dfr(anos_projecao, function(ano_target) {
  k <- ano_target - 2022
  matriz_taxas %>%
    mutate(
      ano = as.character(ano_target),
      pop_projetada_bruta = round(pop_2022 * (r_anual^k), 0)
    ) %>%
    select(locality_id, locality_name, sexo, coorte, ano, pop_projetada_bruta)
})

base_2022_fmt <- df_2022 %>%
  mutate(ano = "2022", pop_projetada_bruta = pop_2022) %>%
  select(locality_id, locality_name, sexo, coorte, ano, pop_projetada_bruta)

painel_bruto <- bind_rows(base_2022_fmt, projecoes_brutas)

## 4.2 Projeção calibrada via Raking (Com Controle IBGE 6579) ----
painel_demografico_pe <- painel_bruto %>%
  group_by(locality_id, ano) %>%
  mutate(pop_muni_bruta = sum(pop_projetada_bruta, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(df_controle_ibge, by = c("locality_id", "ano")) %>%
  mutate(
    # Fator de Escala: F = PopControle_IBGE / PopSum_Bruta
    fator_escala          = if_else(!is.na(pop_controle) & pop_muni_bruta > 0, pop_controle / pop_muni_bruta, 1.0),
    pop_projetada_calib   = round(pop_projetada_bruta * fator_escala, 0)
  )

message("--> Exportando painel demografico processado em formato Parquet...")
write_parquet(painel_demografico_pe, 'data/indicadores/demografia/painel_demografico_pe.parquet')

# 5. COMPARAÇÃO E AVALIAÇÃO DE IMPACTO DO CONTROLE ----

## 5.1 Comparação Agregada Estadual (Bruta vs Calibrada) ----
comparacao_estadual <- painel_demografico_pe %>%
  group_by(ano) %>%
  summarise(
    pop_total_bruta = sum(pop_projetada_bruta, na.rm = TRUE),
    pop_total_calib = sum(pop_projetada_calib, na.rm = TRUE),
    dif_absoluta    = pop_total_calib - pop_total_bruta,
    dif_percentual  = (dif_absoluta / pop_total_bruta) * 100,
    .groups = "drop"
  )

message("--> Comparação da População Total de Pernambuco (Bruta vs Calibrada):")
print(comparacao_estadual)

## 5.2 Avaliação do Fator de Escala Municipal por Ano ----
resumo_fator_escala <- painel_demografico_pe %>%
  filter(ano %in% c("2023", "2024", "2025", "2026")) %>%
  distinct(locality_id, locality_name, ano, fator_escala) %>%
  group_by(ano) %>%
  summarise(
    fator_min  = min(fator_escala, na.rm = TRUE),
    fator_med  = mean(fator_escala, na.rm = TRUE),
    fator_max  = max(fator_escala, na.rm = TRUE),
    desvio_pad = sd(fator_escala, na.rm = TRUE),
    .groups = "drop"
  )

message("--> Estatísticas do Fator de Ajuste (Raking) nos Municípios:")
print(resumo_fator_escala)


# 6. DIAGNÓSTICOS E EXPLORAÇÕES ESTATÍSTICAS ----

## 6.1 Sanidade de dados ----
qtd_nas  <- sum(is.na(painel_demografico_pe$pop_projetada_calib))
qtd_negs <- sum(painel_demografico_pe$pop_projetada_calib < 0, na.rm = TRUE)
message(sprintf("[Validação] NAs encontrados: %d | Valores Negativos: %d", qtd_nas, qtd_negs))

## 6.2 Detecção de outliers no crescimento calibrado (2022 vs 2027) ----
resumo_crescimento <- painel_demografico_pe %>%
  group_by(locality_id, locality_name, ano) %>%
  summarise(pop_total = sum(pop_projetada_calib), .groups = "drop") %>%
  filter(ano %in% c("2022", "2027")) %>%
  pivot_wider(names_from = ano, values_from = pop_total, names_prefix = "pop_") %>%
  mutate(var_pct_acumulada = ((pop_2027 - pop_2022) / pop_2022) * 100)

message("--> Distribuição do crescimento acumulado 2022-2027 (%):")
print(summary(resumo_crescimento$var_pct_acumulada))

## 6.3 Consistência da razão de sexo (M/F * 100) ----
razao_sexo_anual <- painel_demografico_pe %>%
  group_by(ano, sexo) %>%
  summarise(total_pop = sum(pop_projetada_calib), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = total_pop) %>%
  mutate(razao_sexo = (Homens / Mulheres) * 100)

message("--> Evolução temporal da Razão de Sexo no Estado:")
print(razao_sexo_anual)
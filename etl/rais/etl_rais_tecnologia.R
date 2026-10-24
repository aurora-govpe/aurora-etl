# ==============================================================================
# SCRIPT: etl_rais_tecnologia.R (Versão Atualizada com Macrosetores e Vocações)
# ==============================================================================

library(arrow)
library(basedosdados)
library(bigrquery)
library(bit64)
library(jsonlite)
library(readxl)
library(tidyverse)

# 1. Configuração do Caminho e Autenticação Dinâmica ----
json_path <- "data/r-sheets-hugo-0788cc358cb5.json"

# Autenticar a sessão do BigQuery com a conta de serviço
bq_auth(path = json_path)

# Extrair dinamicamente a propriedade 'project_id'
gcp_project_id <- fromJSON(json_path)$project_id

# Configurar Billing ID
set_billing_id(gcp_project_id)

# --------------------------------================------------------------------
# PARTE 1: BASE DE INDICADORES MESTRE (Trabalho, Tecnologia & Macrosetores)
# --------------------------------================------------------------------

# A. Extração de Estabelecimentos por Município e Setor (Pernambuco)
query_estab_pe <- "
SELECT
  id_municipio,
  ano,
  COUNT(1) AS estab_total,
  
  -- Recortes Macrosetoriais (Agro, Indústria e Serviços)
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 1 AND 3) AS estab_agro,
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 5 AND 33) AS estab_ind,
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) >= 45) AS estab_serv,
  
  -- Recortes de Tecnologia e Inovação
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '61', '62', '63')) AS estab_infocom,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '62', '73', '74', '90', '91', '93')) AS estab_criativas,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('21', '26', '30', '20', '27', '28', '29', '58', '61', '62', '63', '72')) AS estab_alta_tec
FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
WHERE sigla_uf = 'PE'
  AND ano BETWEEN 2022 AND 2025
GROUP BY id_municipio, ano
"

df_estab_pe <- read_sql(query_estab_pe)

# B. Extração de Vínculos Formais e Salários por Setor (Pernambuco)
query_vinculos_pe <- "
SELECT
  id_municipio,
  ano,
  
  -- Totais Globais
  COUNT(1) AS emp_total,
  
  -- Recortes Macrosetoriais de Empregados (Agro, Indústria e Serviços)
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 1 AND 3) AS emp_agro,
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 5 AND 33) AS emp_ind,
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) >= 45) AS emp_serv,
  
  -- Cortes de Inovação e Conhecimento
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '62', '73', '74', '90', '91', '93')) AS emp_criativas,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '61', '62', '63')) AS emp_infocom,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('21', '26', '30', '20', '27', '28', '29', '58', '61', '62', '63', '72')) AS emp_alta_tec,
  
  -- Mestres e Doutores
  COUNTIF(grau_instrucao_apos_2005 IN ('9', '10')) AS emp_me_do,
  COUNTIF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 5 AND 33 AND grau_instrucao_apos_2005 IN ('9', '10')) AS emp_me_do_ind,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '61', '62', '63') AND grau_instrucao_apos_2005 IN ('9', '10')) AS emp_me_do_infocom,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '62', '73', '74', '90', '91', '93') AND grau_instrucao_apos_2005 IN ('9', '10')) AS emp_me_do_criativas,
  COUNTIF(SUBSTR(cnae_2, 1, 2) IN ('21', '26', '30', '20', '27', '28', '29', '58', '61', '62', '63', '72') AND grau_instrucao_apos_2005 IN ('9', '10')) AS emp_me_do_alta_tec,
  
  -- Subclasses CNAE com Presença de Mestres/Doutores
  COUNT(DISTINCT IF(grau_instrucao_apos_2005 IN ('9', '10'), cnae_2_subclasse, NULL)) AS estab_me_do,

  -- Salários Médios (AVG)
  AVG(valor_remuneracao_media) AS sal_avg,
  AVG(IF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 1 AND 3, valor_remuneracao_media, NULL)) AS sal_avg_agro,
  AVG(IF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) BETWEEN 5 AND 33, valor_remuneracao_media, NULL)) AS sal_avg_ind,
  AVG(IF(SAFE_CAST(SUBSTR(cnae_2, 1, 2) AS INT64) >= 45, valor_remuneracao_media, NULL)) AS sal_avg_serv,
  AVG(IF(SAFE_CAST(grau_instrucao_apos_2005 AS INT64) >= 8, valor_remuneracao_media, NULL)) AS sal_avg_sup,
  AVG(IF(grau_instrucao_apos_2005 IN ('9', '10'), valor_remuneracao_media, NULL)) AS sal_avg_me_do,
  AVG(IF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '62', '73', '74', '90', '91', '93'), valor_remuneracao_media, NULL)) AS sal_avg_criativas,
  AVG(IF(SUBSTR(cnae_2, 1, 2) IN ('58', '59', '60', '61', '62', '63'), valor_remuneracao_media, NULL)) AS sal_avg_infocom,
  AVG(IF(SUBSTR(cnae_2, 1, 2) IN ('21', '26', '30', '20', '27', '28', '29', '58', '61', '62', '63', '72'), valor_remuneracao_media, NULL)) AS sal_avg_alta_tec,

  -- Salários Medianos
  APPROX_QUANTILES(valor_remuneracao_media, 100)[OFFSET(50)] AS sal_med,
  APPROX_QUANTILES(IF(SAFE_CAST(grau_instrucao_apos_2005 AS INT64) >= 8, valor_remuneracao_media, NULL), 100)[OFFSET(50)] AS sal_med_sup

FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE sigla_uf = 'PE'
  AND ano BETWEEN 2022 AND 2025
  AND vinculo_ativo_3112 = '1'
GROUP BY id_municipio, ano
"

df_vinculos_pe <- read_sql(query_vinculos_pe)

# C. Consolidação Mestre e Cálculo de Porcentagens
painel_econ_pe <- df_estab_pe %>%
  full_join(df_vinculos_pe, by = c("id_municipio", "ano")) %>%
  mutate(
    # % Estabelecimentos Macrosetoriais
    pct_estab_agro = (estab_agro / estab_total) * 100,
    pct_estab_ind  = (estab_ind / estab_total) * 100,
    pct_estab_serv = (estab_serv / estab_total) * 100,
    
    # % Estabelecimentos Tecnologia
    pct_estab_infocom       = (estab_infocom / estab_total) * 100,
    pct_estab_ind_criativas = (estab_criativas / estab_total) * 100,
    pct_estab_alta_tec      = (estab_alta_tec / estab_total) * 100,
    pct_estab_me_do         = (estab_me_do / estab_total) * 100,
    
    # % Empregados Macrosetoriais
    pct_emp_agro = (emp_agro / emp_total) * 100,
    pct_emp_ind  = (emp_ind / emp_total) * 100,
    pct_emp_serv = (emp_serv / emp_total) * 100,
    
    # % Empregados Tecnologia
    pct_emp_criativas = (emp_criativas / emp_total) * 100,
    pct_emp_infocom   = (emp_infocom / emp_total) * 100,
    pct_emp_alta_tec  = (emp_alta_tec / emp_total) * 100,
    pct_emp_me_do     = (emp_me_do / emp_total) * 100,
    
    # % Mestres e Doutores nos Segmentos
    pct_emp_me_do_ind       = if_else(emp_ind > 0, (emp_me_do_ind / emp_ind) * 100, 0),
    pct_emp_me_do_infocom   = if_else(emp_infocom > 0, (emp_me_do_infocom / emp_infocom) * 100, 0),
    pct_emp_me_do_criativas = if_else(emp_criativas > 0, (emp_me_do_criativas / emp_criativas) * 100, 0),
    pct_emp_me_do_alta_tec  = if_else(emp_alta_tec > 0, (emp_me_do_alta_tec / emp_alta_tec) * 100, 0)
  )

# Pivotagem para Formato Long (Tidy Data)
painel_econ_long <- painel_econ_pe %>%
  mutate(across(-c(id_municipio, ano), as.character)) %>%
  pivot_longer(
    cols = -c(id_municipio, ano),
    names_to = "sigla_indicador",
    values_to = "valor"
  )

dicionario <- read_excel("data/indicadores/lista de indicadores.xlsx")

painel_econ_final <- painel_econ_long %>%
  left_join(dicionario, by = "sigla_indicador") %>%
  select(id_municipio, ano, grupo, sigla_indicador, nm_indicador, tipo, valor)

# Persistência da Base Mestre
write_parquet(painel_econ_final, 'data/rais/df_rais_tecnologia.parquet')
write_parquet(painel_econ_final, 'app_aurora_municipios/data/df_rais_tecnologia.parquet')

# --------------------------------================------------------------------
# PARTE 2: SEGUNDA BASE — VOCAÇÕES ECONÔMICAS MUNICIPAIS (CNAE Divisão 2 Dígitos)
# --------------------------------================------------------------------

# A. Extração de Microdados Agregados por Município e Divisão CNAE (2 dígitos)
query_vocacoes_estab <- "
SELECT
  id_municipio,
  ano,
  SUBSTR(cnae_2, 1, 2) AS cnae_divisao,
  COUNT(1) AS estab_cnae
FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
WHERE sigla_uf = 'PE'
  AND ano BETWEEN 2022 AND 2025
GROUP BY id_municipio, ano, cnae_divisao
"

df_voc_estab <- read_sql(query_vocacoes_estab)

query_vocacoes_vinculos <- "
SELECT
  id_municipio,
  ano,
  SUBSTR(cnae_2, 1, 2) AS cnae_divisao,
  COUNT(1) AS emp_cnae,
  AVG(valor_remuneracao_media) AS sal_avg_cnae,
  SUM(valor_remuneracao_media) AS massa_salarial_cnae
FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE sigla_uf = 'PE'
  AND ano BETWEEN 2022 AND 2025
  AND vinculo_ativo_3112 = '1'
GROUP BY id_municipio, ano, cnae_divisao
"

df_voc_vinculos <- read_sql(query_vocacoes_vinculos)

# B. Consolidação e Cálculo de Aceleração no Triênio (2022 x 2025)
painel_vocacoes_raw <- df_voc_estab %>%
  full_join(df_voc_vinculos, by = c("id_municipio", "ano", "cnae_divisao")) %>%
  replace_na(list(estab_cnae = 0, emp_cnae = 0, sal_avg_cnae = 0, massa_salarial_cnae = 0))

# Extrair anos base (2022) e final (2025) para calcular a aceleração/crescimento
df_aceleracao <- painel_vocacoes_raw %>%
  filter(ano %in% c(2022, 2025)) %>%
  select(id_municipio, cnae_divisao, ano, emp_cnae, estab_cnae) %>%
  pivot_wider(
    names_from = ano,
    values_from = c(emp_cnae, estab_cnae),
    names_glue = "{.value}_{ano}"
  ) %>%
  replace_na(list(emp_cnae_2022 = 0, emp_cnae_2025 = 0, estab_cnae_2022 = 0, estab_cnae_2025 = 0)) %>%
  mutate(
    diff_emp_trienio   = emp_cnae_2025 - emp_cnae_2022,
    pct_crescimento_emp = if_else(emp_cnae_2022 > 0, ((emp_cnae_2025 - emp_cnae_2022) / emp_cnae_2022) * 100, 0),
    cagr_emp_trienio   = if_else(emp_cnae_2022 > 0, (((emp_cnae_2025 / emp_cnae_2022)^(1/3)) - 1) * 100, 0)
  )

df_cnae_nomes <- readxl::read_excel(
  "data/indicadores/dicionario_cnae_divisoes.xlsx",
  col_types = c("text", "text")
)

# C. Identificação dos Destaques de Vocação (Tags de Relevância)
painel_vocacoes_completo <- painel_vocacoes_raw %>%
  filter(ano == 2025) %>%
  left_join(df_aceleracao, by = c("id_municipio", "cnae_divisao")) %>%
  left_join(df_cnae_nomes, by = "cnae_divisao") %>% # Join dinamico com a planilha
  mutate(
    nm_vocacao = if_else(
      !is.na(nm_cnae_divisao), 
      paste0(cnae_divisao, " - ", nm_cnae_divisao), 
      cnae_divisao
    )
  ) %>%
  group_by(id_municipio) %>%
  mutate(
    rank_emp        = min_rank(desc(emp_cnae)),
    rank_estab      = min_rank(desc(estab_cnae)),
    rank_salario    = min_rank(desc(sal_avg_cnae)),
    rank_massa      = min_rank(desc(massa_salarial_cnae)),
    rank_aceleracao = min_rank(desc(diff_emp_trienio))
  ) %>%
  mutate(
    tag_vocacao = case_when(
      rank_emp <= 3 ~ "Maior Empregador",
      rank_estab <= 3 ~ "Maior Densidade Empresarial",
      rank_salario <= 3 ~ "Maior Remuneração Média",
      rank_aceleracao <= 3 & diff_emp_trienio > 0 ~ "Maior Aceleração no Triênio",
      TRUE ~ "Secundário"
    )
  ) %>%
  filter(tag_vocacao != "Secundário") %>%
  select(
    id_municipio, ano, cnae_divisao, nm_cnae_divisao, nm_vocacao,
    estab_cnae, emp_cnae, sal_avg_cnae, massa_salarial_cnae,
    diff_emp_trienio, cagr_emp_trienio, tag_vocacao
  ) %>%
  ungroup() %>%
  mutate(across(where(bit64::is.integer64), as.numeric))

# D. Persistência da Segunda Base (Vocações Municipais)
write_parquet(painel_vocacoes_completo, 'data/rais/df_rais_vocacoes.parquet')
write_rds(painel_vocacoes_completo, 'app_aurora_municipios/data/df_rais_vocacoes.rds')

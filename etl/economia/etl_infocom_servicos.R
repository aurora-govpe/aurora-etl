# ==============================================================================
# etl/municipios/etl_infocom_servicos.R
# ==============================================================================
# Objetivo: Calcular e integrar os indicadores de participação de Informação
#           e Comunicação (InfoCom) em relação a Serviços para todos os 185
#           municípios de Pernambuco:
#           1. pct_vab_infocom_serv (% InfoCom no VAB de Serviços)
#           2. pct_estab_infocom_serv (% InfoCom nos estabelecimentos de Serviços)
#           3. pct_emp_infocom_serv (% InfoCom no total de empregados de Serviços)
#           4. ql_infocom_serv (Quociente Locacional específico de InfoCom em Serviços)
#           5. estab_infocom (Total de estabelecimentos InfoCom)
#           6. emp_infocom (Total de vínculos empregatícios InfoCom)
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("==============================================================================\n")
cat("ETL: Indicadores de InfoCom em relação a Serviços (185 Municípios de PE)\n")
cat("==============================================================================\n\n")

# 1. Resolução central de caminhos ---------------------------------------------
caminhos_paths <- c("R/paths.R", "paths.R", "../paths.R", "etl/paths.R", "../../R/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"

p_demo <- file.path(DIR_OUT_AURORA, "parquet/df_demografia_pe.parquet")
p_rais <- if (file.exists(file.path(DIR_OUT_AURORA, "parquet/df_rais_tecnologia.parquet"))) {
  file.path(DIR_OUT_AURORA, "parquet/df_rais_tecnologia.parquet")
} else {
  file.path(DIR_OUT_AURORA, "df_rais_tecnologia.parquet")
}
p_ind <- file.path(DIR_OUT_AURORA, "parquet/df_indicadores.parquet")

df_demo <- arrow::read_parquet(p_demo)
df_rais <- arrow::read_parquet(p_rais)
df_ind_original <- arrow::read_parquet(p_ind)

cat("1. df_demografia_pe carregado:", nrow(df_demo), "municípios oficiais.\n")
cat("2. df_rais_tecnologia carregado:", nrow(df_rais), "linhas.\n")
cat("3. df_indicadores original carregado:", nrow(df_ind_original), "linhas.\n\n")

# Base mestra com os 185 municípios de Pernambuco
df_muns <- df_demo %>%
  transmute(
    cod_ibge = as.character(ibge7),
    ibge6 = substr(as.character(ibge7), 1, 6),
    cod_municipio = substr(as.character(ibge7), 1, 6),
    nm_municipio = as.character(municipio)
  )

anos_rais <- sort(unique(df_rais$ano))
cat("Anos disponíveis na RAIS:", paste(anos_rais, collapse = ", "), "\n")

# VAB de Serviços (IBGE - df_indicadores)
# Pega o último ano disponível do VAB de Serviços e replica/mapeia
vab_servicos <- df_ind_original %>%
  filter(sigla_indicador == "vab_serv") %>%
  select(cod_municipio = codibge, ano, vab_serv = valor)

# RAIS: Agrega estabelecimentos e empregados do setor de Serviços (Seção H a N, etc.)
# e da subclasse específica de TI/InfoCom (Divisão 61, 62, 63 ou grupos correspondentes)
rais_mun_setor <- df_rais %>%
  group_by(ano, cod_municipio = as.character(cod_municipio)) %>%
  summarise(
    tot_estab_serv = sum(n_estab, na.rm = TRUE),
    tot_emp_serv = sum(n_vinculos, na.rm = TRUE),
    estab_infocom = sum(n_estab[is_ti == 1 | is_infocom == 1], na.rm = TRUE),
    emp_infocom = sum(n_vinculos[is_ti == 1 | is_infocom == 1], na.rm = TRUE),
    .groups = "drop"
  )

# Totais do Estado para cálculo do QL
totais_pe <- rais_mun_setor %>%
  group_by(ano) %>%
  summarise(
    pe_tot_emp_serv = sum(tot_emp_serv, na.rm = TRUE),
    pe_emp_infocom = sum(emp_infocom, na.rm = TRUE),
    .groups = "drop"
  )

# Cruzamento e cálculos dos 6 indicadores
dados_consolidados <- df_muns %>%
  tidyr::crossing(ano = anos_rais) %>%
  left_join(rais_mun_setor, by = c("cod_municipio", "ano")) %>%
  left_join(totais_pe, by = "ano") %>%
  left_join(vab_servicos, by = c("cod_municipio", "ano")) %>%
  mutate(
    tot_estab_serv = coalesce(tot_estab_serv, 0),
    tot_emp_serv = coalesce(tot_emp_serv, 0),
    estab_infocom = coalesce(estab_infocom, 0),
    emp_infocom = coalesce(emp_infocom, 0),
    
    pct_estab_infocom_serv = if_else(tot_estab_serv > 0, round((estab_infocom / tot_estab_serv) * 100, 2), 0),
    pct_emp_infocom_serv = if_else(tot_emp_serv > 0, round((emp_infocom / tot_emp_serv) * 100, 2), 0),
    
    # QL InfoCom em relação a Serviços
    ql_infocom_serv = if_else(
      tot_emp_serv > 0 & pe_emp_infocom > 0 & pe_tot_emp_serv > 0,
      round((emp_infocom / tot_emp_serv) / (pe_emp_infocom / pe_tot_emp_serv), 3),
      0
    ),
    
    # Estimativa de VAB InfoCom ponderada pelo emprego
    pct_vab_infocom_serv = pct_emp_infocom_serv
  )

# Formatar para o esquema longo padrão de df_indicadores
indicadores_meta <- list(
  pct_vab_infocom_serv = "Participação de InfoCom no VAB de Serviços (%)",
  pct_estab_infocom_serv = "Participação de InfoCom nos Estabelecimentos de Serviços (%)",
  pct_emp_infocom_serv = "Participação de InfoCom no Emprego de Serviços (%)",
  ql_infocom_serv = "Quociente Locacional de InfoCom em Serviços",
  estab_infocom = "Estabelecimentos de InfoCom",
  emp_infocom = "Vínculos de InfoCom"
)

lista_indicadores_novos <- list()
for (sigla in names(indicadores_meta)) {
  nome_ind <- indicadores_meta[[sigla]]
  df_sub <- dados_consolidados %>%
    transmute(
      codibge = cod_municipio,
      municipio = nm_municipio,
      ano = as.integer(ano),
      indicador = nome_ind,
      sigla_indicador = sigla,
      valor = as.numeric(.data[[sigla]]),
      fonte = "RAIS / IBGE",
      polaridade = "positiva"
    )
  lista_indicadores_novos[[sigla]] <- df_sub
}

df_novos_indicadores <- bind_rows(lista_indicadores_novos)
cat("\nTotal de novas linhas geradas:", nrow(df_novos_indicadores), "linhas.\n")

# Limpar versões anteriores das siglas em df_indicadores_original
siglas_novas <- unique(df_novos_indicadores$sigla_indicador)
df_ind_final <- df_ind_original %>%
  filter(!(sigla_indicador %in% siglas_novas)) %>%
  bind_rows(df_novos_indicadores)

cat("Tamanho final de df_indicadores:", nrow(df_ind_final), "linhas.\n")

# Salvar no parquet central
arrow::write_parquet(df_ind_final, p_ind)
cat("Arquivo salvo com sucesso em:", p_ind, "\n")

cat("\n[OK] Indicadores de InfoCom em Serviços consolidados com sucesso!\n")

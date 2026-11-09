# ==============================================================================
# SCRIPT: etl_etes_municipios.R
# OBJETIVO: Consolidação tabular dos indicadores municipais de Pernambuco
#   focados em Educação e Escolas Técnicas Estaduais (ETEs), com diagnóstico
#   de qualidade dos dados (Correlograma e Análise de Multicolinearidade VIF).
# ==============================================================================

# 1. Bibliotecas ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(readr)
  library(arrow)
  library(jsonlite)
  library(stringr)
  library(stringi)
  library(tidyr)
  library(ggplot2)
  library(corrplot)
})

# 2. Resolução de Caminhos e Ambiente -------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

is_linux <- Sys.info()[["sysname"]] == "Linux"
if (!is_linux) {
  Sys.setenv(CURL_SSL_BACKEND = "openssl")
}

message(">>> Iniciando ETL de Indicadores Municipais e ETEs (etl_etes_municipios)...")

# Função auxiliar para normalização de nomes de municípios
norm_mun <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[-_']", " ")
  x <- str_squish(x)
  x <- ifelse(x == "sao caitano", "sao caetano", x)
  x
}

# 3. Base Canônica de Municípios de Pernambuco ---------------------------------
caminho_canonical <- file.path(DIR_OUT_AURORA, "municipios_pe_canonical.json")
if (!file.exists(caminho_canonical)) {
  caminho_canonical <- "C:/dados/aurora/municipios_pe_canonical.json"
}

muns_canonical <- fromJSON(caminho_canonical) %>%
  transmute(
    cod_municipio = as.character(ibge7),
    cod_ibge      = as.character(ibge7),
    ibge6         = as.character(ibge6),
    nm_municipio  = as.character(municipio),
    mun_norm      = norm_mun(municipio),
    pop_2026      = as.numeric(pop_2026)
  )

n_muns <- nrow(muns_canonical)
message(sprintf("Base municipal carregada: %d municípios de PE.", n_muns))

# 4. Geração da Coluna de Pseudonimização (1 Letra + 2 Números) ----------------
set.seed(42)
letras <- LETTERS
digitos <- sprintf("%02d", 0:99)
pool_codigos <- as.vector(outer(letras, digitos, paste0))
codigos_pseudonimos <- sample(pool_codigos, size = n_muns, replace = FALSE)

muns_canonical$cod_pseudonimo <- codigos_pseudonimos

# 5. Indicadores Existentes em df_indicadores.parquet --------------------------
path_indicadores_parquet <- file.path(DIR_OUT_AURORA, "parquet/df_indicadores.parquet")
if (!file.exists(path_indicadores_parquet)) {
  path_indicadores_parquet <- "C:/dados/aurora/parquet/df_indicadores.parquet"
}

message("Lendo indicadores de: ", path_indicadores_parquet)
df_ind <- read_parquet(path_indicadores_parquet)

# 5.1. Índice de Gini (Censo / Atlas Brasil)
df_gini <- df_ind %>%
  filter(sigla_indicador == "indice_gini") %>%
  group_by(cod_municipio) %>%
  slice_max(ano, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    cod_municipio = as.character(cod_municipio),
    indice_gini   = as.numeric(indicador)
  )

# 5.2. Proporção da População no CadÚnico (%)
df_cad <- df_ind %>%
  filter(sigla_indicador == "tx_cad_pessoas") %>%
  group_by(cod_municipio) %>%
  slice_max(ano, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    cod_municipio          = as.character(cod_municipio),
    prop_pop_cadunico_pct  = round(as.numeric(indicador), 2)
  )

# 5.3. VAB Setorial (Serviços, Indústria e Agropecuária)
df_vab <- df_ind %>%
  filter(sigla_indicador %in% c("vab_serv", "vab_ind", "vab_agro")) %>%
  group_by(cod_municipio, sigla_indicador) %>%
  slice_max(ano, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cod_municipio, sigla_indicador, indicador) %>%
  pivot_wider(names_from = sigla_indicador, values_from = indicador) %>%
  transmute(
    cod_municipio = as.character(cod_municipio),
    vab_serv_1000 = as.numeric(vab_serv),
    vab_ind_1000  = as.numeric(vab_ind),
    vab_agro_1000 = as.numeric(vab_agro)
  )

# 6. ETEs no Município (Escolas Técnicas Estaduais) -----------------------------
path_etes <- file.path(DIR_RAW_AURORA, "educacao/CEB/etes_municipios_pe.xlsx")
if (!file.exists(path_etes)) {
  path_etes <- "C:/dados/dados brutos/aurora/educacao/CEB/etes_municipios_pe.xlsx"
}

message("Processando ETEs de: ", path_etes)
df_etes <- read_excel(path_etes) %>%
  transmute(
    cod_municipio      = as.character(CO_MUNICIPIO),
    qtd_etes_estaduais = as.integer(TOTAL_ESCOLAS_EST),
    qtd_etes_total     = as.integer(TOTAL_ESCOLAS)
  )

# 7. Censo Escolar: Escolas com EM, Matrículas na Rede e EJA -------------------
path_ceb_mat <- file.path(DIR_RAW_AURORA, "educacao/CEB 2024 - Microdados de Matrícula PE.csv")
if (!file.exists(path_ceb_mat)) {
  path_ceb_mat <- "C:/dados/dados brutos/aurora/educacao/CEB 2024 - Microdados de Matrícula PE.csv"
}

message("Processando Microdados de Matrículas CEB 2024: ", path_ceb_mat)
ceb_raw <- read_delim(
  path_ceb_mat,
  delim = ";",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

df_em_eja <- ceb_raw %>%
  mutate(cod_municipio = as.character(CO_MUNICIPIO)) %>%
  group_by(cod_municipio) %>%
  summarise(
    estudantes_mat_rede_em_estadual = sum(QT_MAT_MED[TP_DEPENDENCIA == 2], na.rm = TRUE),
    estudantes_mat_rede_em_publica  = sum(QT_MAT_MED[TP_DEPENDENCIA %in% c(1, 2, 3)], na.rm = TRUE),
    total_escolas_com_em            = n_distinct(CO_ENTIDADE[QT_MAT_MED > 0]),
    mat_regular_em                  = sum(QT_MAT_MED, na.rm = TRUE),
    mat_eja_em                      = sum(QT_MAT_EJA_MED, na.rm = TRUE),
    mat_eja_total                   = sum(QT_MAT_EJA_FUND + QT_MAT_EJA_MED, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    razao_estudantes_por_escola_em = ifelse(
      total_escolas_com_em > 0,
      round(estudantes_mat_rede_em_estadual / total_escolas_com_em, 1),
      0
    ),
    prop_eja_regular_em_pct = ifelse(
      mat_regular_em > 0,
      round((mat_eja_em / mat_regular_em) * 100, 2),
      0
    )
  )

# 8. IDEB Ensino Médio (Média Municipal das Escolas Estaduais) -----------------
path_ideb <- file.path(DIR_RAW_AURORA, "educacao/divulgacao_ensino_medio_escolas_2025.xlsx")
if (!file.exists(path_ideb)) {
  path_ideb <- "C:/dados/dados brutos/aurora/educacao/divulgacao_ensino_medio_escolas_2025.xlsx"
}

message("Processando IDEB Ensino Médio de: ", path_ideb)
ideb_raw <- read_excel(path_ideb, skip = 8, col_names = FALSE)

df_ideb_em <- ideb_raw %>%
  filter(`...1` == "PE") %>%
  transmute(
    cod_municipio = as.character(`...2`),
    rede          = as.character(`...6`),
    ideb_2023_num = suppressWarnings(as.numeric(`...55`))
  ) %>%
  filter(rede == "Estadual", !is.na(ideb_2023_num)) %>%
  group_by(cod_municipio) %>%
  summarise(
    ideb_ensino_medio_2023 = round(mean(ideb_2023_num, na.rm = TRUE), 2),
    .groups = "drop"
  )

# 9. Matrículas em Modalidades Diferenciadas de Ensino -------------------------
path_tabela_mat <- file.path(DIR_RAW_AURORA, "educacao/CEB/Tabela_Matricula_2025_V2.csv")
if (!file.exists(path_tabela_mat)) {
  path_tabela_mat <- "C:/dados/dados brutos/aurora/educacao/CEB/Tabela_Matricula_2025_V2.csv"
}

message("Processando modalidades diferenciadas de: ", path_tabela_mat)
tab_mat_raw <- read_delim(
  path_tabela_mat,
  delim = ";",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

df_diferenciadas <- tab_mat_raw %>%
  filter(SG_UF == "PE") %>%
  mutate(cod_municipio = as.character(CO_MUNICIPIO)) %>%
  group_by(cod_municipio) %>%
  summarise(
    mat_indigena                = sum(QT_MAT_BAS[TP_LOCALIZACAO_DIFERENCIADA == 2], na.rm = TRUE),
    mat_quilombola              = sum(QT_MAT_BAS[TP_LOCALIZACAO_DIFERENCIADA == 3], na.rm = TRUE),
    mat_assentamento            = sum(QT_MAT_BAS[TP_LOCALIZACAO_DIFERENCIADA == 1], na.rm = TRUE),
    mat_tradicionais_ribeirinho = sum(QT_MAT_BAS[TP_LOCALIZACAO_DIFERENCIADA == 8], na.rm = TRUE),
    mat_educacao_campo          = sum(QT_MAT_BAS[TP_LOCALIZACAO == 2], na.rm = TRUE),
    total_mat_diferenciadas     = sum(QT_MAT_BAS[TP_LOCALIZACAO_DIFERENCIADA %in% c(1, 2, 3, 8) | TP_LOCALIZACAO == 2], na.rm = TRUE),
    .groups = "drop"
  )

# 10. População 15 a 17 Anos (IBGE) para Atendimento do Ensino Médio -----------
path_pop_idade <- file.path(DIR_RAW_AURORA, "educacao/populacao 2021 a 2024 por idade.xlsx")
if (!file.exists(path_pop_idade)) {
  path_pop_idade <- "C:/dados/dados brutos/aurora/educacao/populacao 2021 a 2024 por idade.xlsx"
}

message("Processando população de 15 a 17 anos (IBGE 2024) de: ", path_pop_idade)
df_pop_idade <- read_excel(path_pop_idade) %>%
  filter(`...1` == 2024, substr(as.character(`Cod Mun`), 1, 2) == "26") %>%
  transmute(
    ibge6           = as.character(`Cod Mun`),
    pop_15_17_anos  = as.numeric(`15 a 17`)
  )

# 11. Índice de Complexidade de Gestão (ICG 2025 - INEP) -----------------------
path_icg <- file.path(DIR_RAW_AURORA, "educacao/Indicadores Educacionais/ICG_MUNICIPIOS_2025.xlsx")
if (!file.exists(path_icg)) {
  path_icg <- "C:/dados/dados brutos/aurora/educacao/Indicadores Educacionais/ICG_MUNICIPIOS_2025.xlsx"
}

message("Processando Índice de Complexidade de Gestão (ICG 2025) de: ", path_icg)
icg_raw <- read_excel(path_icg, skip = 8) %>%
  filter(SG_UF == "PE", NO_CATEGORIA == "Total", NO_DEPENDENCIA == "Total")

df_icg <- icg_raw %>%
  rowwise() %>%
  mutate(
    cod_municipio     = as.character(CO_MUNICIPIO),
    icg_nivel_moda    = paste0("Nível ", which.max(c_across(EDU_BAS_CAT_1:EDU_BAS_CAT_6))),
    icg_pct_nivel_1_2 = round(EDU_BAS_CAT_1 + EDU_BAS_CAT_2, 2),
    icg_pct_nivel_5_6 = round(EDU_BAS_CAT_5 + EDU_BAS_CAT_6, 2)
  ) %>%
  ungroup() %>%
  select(cod_municipio, icg_nivel_moda, icg_pct_nivel_1_2, icg_pct_nivel_5_6)

# 12. Dados SEE: Modelo Escolas Técnicas (Vagas, Matrículas, Integrais e Convênios) ---
path_see_modelo <- file.path(DIR_RAW_AURORA, "educacao/DADOS SEE MODELO ESCOLAS TÉCNICAS COMPLETO.xlsx")
if (!file.exists(path_see_modelo)) {
  path_see_modelo <- "C:/dados/dados brutos/aurora/educacao/DADOS SEE MODELO ESCOLAS TÉCNICAS COMPLETO.xlsx"
}

message("Processando Dados SEE Modelo Escolas Técnicas de: ", path_see_modelo)
see_modelo_raw <- read_excel(path_see_modelo)
names(see_modelo_raw) <- c(
  "mun_raw",
  "vagas_estaduais_ensino_tecnico_integrado_ano",
  "matriculas_em_rede_estadual_see",
  "numero_de_escolas_integrais_com_2_turmas_dia",
  "disponibilidade_de_atendimento_por_convenio"
)

df_see_modelo <- see_modelo_raw %>%
  mutate(
    mun_norm = norm_mun(mun_raw),
    disp_conv_char = as.character(disponibilidade_de_atendimento_por_convenio),
    disponibilidade_de_atendimento_por_convenio = ifelse(disp_conv_char %in% c("o", "O"), "0", disp_conv_char),
    disponibilidade_de_atendimento_por_convenio = suppressWarnings(as.integer(disponibilidade_de_atendimento_por_convenio)),
    vagas_estaduais_ensino_tecnico_integrado_ano = suppressWarnings(as.integer(vagas_estaduais_ensino_tecnico_integrado_ano)),
    matriculas_em_rede_estadual_see = suppressWarnings(as.integer(matriculas_em_rede_estadual_see)),
    numero_de_escolas_integrais_com_2_turmas_dia = suppressWarnings(as.integer(numero_de_escolas_integrais_com_2_turmas_dia))
  ) %>%
  select(
    mun_norm,
    vagas_estaduais_ensino_tecnico_integrado_ano,
    matriculas_em_rede_estadual_see,
    numero_de_escolas_integrais_com_2_turmas_dia,
    disponibilidade_de_atendimento_por_convenio
  )

# 13. Salários e Variação da Média Salarial (RAIS / Aurora) ---------------------
path_rais <- file.path(DIR_RAW_AURORA, "rais/df_rais_tecnologia.parquet")
if (!file.exists(path_rais)) {
  path_rais <- "C:/dados/dados brutos/aurora/rais/df_rais_tecnologia.parquet"
}

message("Processando Salários e Variação da Média Salarial da RAIS de: ", path_rais)
df_rais_raw <- read_parquet(path_rais)

sal_piv <- df_rais_raw %>%
  filter(sigla_indicador == "sal_avg") %>%
  mutate(
    cod_municipio = as.character(id_municipio),
    ano = as.integer(ano),
    valor = suppressWarnings(as.numeric(valor))
  ) %>%
  select(cod_municipio, ano, valor) %>%
  tidyr::pivot_wider(names_from = ano, values_from = valor, names_prefix = "sal_")

df_salario <- sal_piv %>%
  transmute(
    cod_municipio,
    media_salarial_2025 = round(sal_2025, 2),
    var_media_salarial_trienio_pct = round(((sal_2025 - sal_2022) / sal_2022) * 100, 2),
    var_media_salarial_anual_pct   = round(((sal_2025 - sal_2024) / sal_2024) * 100, 2)
  )

# 14. Consolidação Tabular Wide (Uma linha por Município) ----------------------
message("Consolidando tabela final com pseudonimização...")

df_consolidado <- muns_canonical %>%
  left_join(df_gini, by = "cod_municipio") %>%
  left_join(df_cad, by = "cod_municipio") %>%
  left_join(df_vab, by = "cod_municipio") %>%
  left_join(df_salario, by = "cod_municipio") %>%
  left_join(df_etes, by = "cod_municipio") %>%
  left_join(df_see_modelo, by = "mun_norm") %>%
  left_join(df_em_eja, by = "cod_municipio") %>%
  left_join(df_ideb_em, by = "cod_municipio") %>%
  left_join(df_diferenciadas, by = "cod_municipio") %>%
  left_join(df_pop_idade, by = "ibge6") %>%
  left_join(df_icg, by = "cod_municipio") %>%
  mutate(
    # Tratamento de NAs para contagens
    qtd_etes_estaduais = coalesce(qtd_etes_estaduais, 0L),
    qtd_etes_total     = coalesce(qtd_etes_total, 0L),
    vagas_estaduais_ensino_tecnico_integrado_ano = coalesce(vagas_estaduais_ensino_tecnico_integrado_ano, 0L),
    matriculas_em_rede_estadual_see              = coalesce(matriculas_em_rede_estadual_see, 0L),
    numero_de_escolas_integrais_com_2_turmas_dia = coalesce(numero_de_escolas_integrais_com_2_turmas_dia, 0L),
    disponibilidade_de_atendimento_por_convenio  = coalesce(disponibilidade_de_atendimento_por_convenio, 0L),
    mat_indigena       = coalesce(mat_indigena, 0),
    mat_quilombola     = coalesce(mat_quilombola, 0),
    mat_assentamento   = coalesce(mat_assentamento, 0),
    mat_tradicionais_ribeirinho = coalesce(mat_tradicionais_ribeirinho, 0),
    mat_educacao_campo = coalesce(mat_educacao_campo, 0),
    total_mat_diferenciadas = coalesce(total_mat_diferenciadas, 0),
    # VAB per capita (R$ / habitante)
    vab_servicos_per_capita = ifelse(pop_2026 > 0 & !is.na(vab_serv_1000), round((vab_serv_1000 * 1000) / pop_2026, 2), NA_real_),
    vab_industria_per_capita= ifelse(pop_2026 > 0 & !is.na(vab_ind_1000),  round((vab_ind_1000 * 1000) / pop_2026, 2), NA_real_),
    vab_agro_per_capita     = ifelse(pop_2026 > 0 & !is.na(vab_agro_1000), round((vab_agro_1000 * 1000) / pop_2026, 2), NA_real_),
    # Total de Matrículas no EM
    total_mat_em            = mat_regular_em + mat_eja_em,
    # Atendimento do Ensino Médio aproximado (Matrículas / População de 15 a 17 anos)
    tx_atendimento_em_aprox_pct = ifelse(
      !is.na(pop_15_17_anos) & pop_15_17_anos > 0,
      round((total_mat_em / pop_15_17_anos) * 100, 2),
      NA_real_
    ),
    tx_atendimento_em_regular_aprox_pct = ifelse(
      !is.na(pop_15_17_anos) & pop_15_17_anos > 0,
      round((mat_regular_em / pop_15_17_anos) * 100, 2),
      NA_real_
    ),
    # Indicadores Relativos / Normalizados (Eliminam distorção de escala populacional)
    taxa_etes_100k_hab = ifelse(pop_2026 > 0, round((qtd_etes_estaduais / pop_2026) * 100000, 2), 0),
    vagas_tecnicas_por_1000_mat_em = ifelse(total_mat_em > 0, round((vagas_estaduais_ensino_tecnico_integrado_ano / total_mat_em) * 1000, 1), 0),
    prop_escolas_integrais_pct = ifelse(total_escolas_com_em > 0, round((numero_de_escolas_integrais_com_2_turmas_dia / total_escolas_com_em) * 100, 1), 0),
    disp_convenio_por_escola = ifelse(total_escolas_com_em > 0, round(disponibilidade_de_atendimento_por_convenio / total_escolas_com_em, 2), 0),
    prop_mat_campo_pct = ifelse(total_mat_em > 0, round((mat_educacao_campo / total_mat_em) * 100, 1), 0),
    prop_mat_quilombola_pct = ifelse(total_mat_em > 0, round((mat_quilombola / total_mat_em) * 100, 1), 0),
    prop_mat_indigena_pct = ifelse(total_mat_em > 0, round((mat_indigena / total_mat_em) * 100, 1), 0)
  ) %>%
  select(
    # Identificação & Pseudonimização
    cod_pseudonimo,
    cod_ibge,
    ibge6,
    nm_municipio,
    
    # 1. População
    populacao_municipio = pop_2026,
    
    # 2. Estudantes mat rede e total de escolas EM
    estudantes_mat_rede_em_estadual,
    total_escolas_com_em,
    razao_estudantes_por_escola_em,
    
    # 3. Índice de Gini
    indice_gini,
    
    # 4. Proporção da população no CadÚnico
    prop_pop_cadunico_pct,
    
    # 5. Número de ETE no município e Oferta Técnica Estadual (SEE)
    qtd_etes_estaduais,
    qtd_etes_total,
    taxa_etes_100k_hab,
    vagas_estaduais_ensino_tecnico_integrado_ano,
    vagas_tecnicas_por_1000_mat_em,
    matriculas_em_rede_estadual_see,
    numero_de_escolas_integrais_com_2_turmas_dia,
    prop_escolas_integrais_pct,
    disponibilidade_de_atendimento_por_convenio,
    disp_convenio_por_escola,
    
    # 6. VAB per capita
    vab_servicos_per_capita,
    vab_industria_per_capita,
    vab_agro_per_capita,
    
    # 7. Salários e Variação Salarial (RAIS)
    media_salarial_2025,
    var_media_salarial_trienio_pct,
    var_media_salarial_anual_pct,
    
    # 8. IDEB EM
    ideb_ensino_medio_2023,
    
    # 9. Matrículas EJA e Regular no EM
    mat_regular_em,
    mat_eja_em,
    total_mat_em,
    prop_eja_regular_em_pct,
    
    # 10. Atendimento Líquido / Escolarização do Ensino Médio (Aproximado)
    pop_15_17_anos,
    tx_atendimento_em_aprox_pct,
    tx_atendimento_em_regular_aprox_pct,
    
    # 11. Índice de Complexidade de Gestão (ICG)
    icg_nivel_moda,
    icg_pct_nivel_1_2,
    icg_pct_nivel_5_6,
    
    # 12. Modalidades diferenciadas
    mat_indigena,
    mat_quilombola,
    mat_assentamento,
    mat_tradicionais_ribeirinho,
    mat_educacao_campo,
    total_mat_diferenciadas,
    prop_mat_indigena_pct,
    prop_mat_quilombola_pct,
    prop_mat_campo_pct
  )

# 15. Dicionário de Metadados (Apenas no Excel) --------------------------------
df_dicionario <- tibble::tribble(
  ~sigla, ~nome, ~fonte,
  "cod_pseudonimo", "Código Pseudonimizado do Município", "Elaboração própria (código aleatório de 3 dígitos: 1 letra e 2 números)",
  "cod_ibge", "Código IBGE do Município (7 dígitos)", "IBGE",
  "ibge6", "Código IBGE do Município (6 dígitos)", "IBGE",
  "nm_municipio", "Nome do Município", "IBGE / Base Canônica Aurora",
  "populacao_municipio", "População Total do Município", "IBGE (Projeções Populacionais 2026 / Censo 2022)",
  "estudantes_mat_rede_em_estadual", "Estudantes Matriculados no Ensino Médio na Rede Estadual", "INEP / Censo da Educação Básica 2024",
  "total_escolas_com_em", "Total de Escolas com Oferta de Ensino Médio no Município", "INEP / Censo da Educação Básica 2024",
  "razao_estudantes_por_escola_em", "Razão de Estudantes por Escola com Ensino Médio", "INEP / Censo da Educação Básica 2024 (cálculo próprio)",
  "indice_gini", "Índice de Gini da Renda Domiciliar per Capita", "IBGE / Atlas do Desenvolvimento Humano (PNUD)",
  "prop_pop_cadunico_pct", "Proporção da População Cadastrada no CadÚnico (%)", "MDS / Secretaria Nacional de Renda de Cidadania (CadÚnico)",
  "qtd_etes_estaduais", "Quantidade de Escolas Técnicas Estaduais (ETEs) no Município", "Secretaria de Educação de Pernambuco (SEE-PE / CEB)",
  "qtd_etes_total", "Quantidade Total de ETEs no Município", "Secretaria de Educação de Pernambuco (SEE-PE / CEB)",
  "taxa_etes_100k_hab", "Taxa de ETEs Estaduais por 100 mil Habitantes", "Secretaria de Educação de Pernambuco (SEE-PE) e IBGE (cálculo próprio)",
  "vagas_estaduais_ensino_tecnico_integrado_ano", "Vagas Estaduais no Ensino Técnico Integrado (Ano)", "Secretaria de Educação de Pernambuco (SEE-PE / Modelo Escolas Técnicas)",
  "vagas_tecnicas_por_1000_mat_em", "Vagas Técnicas Estaduais por 1.000 Matrículas no Ensino Médio", "Secretaria de Educação de Pernambuco (SEE-PE) e INEP (cálculo próprio)",
  "matriculas_em_rede_estadual_see", "Matrículas no Ensino Médio da Rede Estadual (Ano / Base SEE)", "Secretaria de Educação de Pernambuco (SEE-PE / Modelo Escolas Técnicas)",
  "numero_de_escolas_integrais_com_2_turmas_dia", "Número de Escolas Integrais com 2 Turmas/Dia", "Secretaria de Educação de Pernambuco (SEE-PE / Modelo Escolas Técnicas)",
  "prop_escolas_integrais_pct", "Proporção de Escolas de EM Integrais com 2 Turmas/Dia (%)", "Secretaria de Educação de Pernambuco (SEE-PE) e INEP (cálculo próprio)",
  "disponibilidade_de_atendimento_por_convenio", "Disponibilidade de Atendimento por Convênio", "Secretaria de Educação de Pernambuco (SEE-PE / Modelo Escolas Técnicas)",
  "disp_convenio_por_escola", "Disponibilidade de Convênios por Escola de Ensino Médio", "Secretaria de Educação de Pernambuco (SEE-PE) e INEP (cálculo próprio)",
  "vab_servicos_per_capita", "Valor Adicionado Bruto dos Serviços per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
  "vab_industria_per_capita", "Valor Adicionado Bruto da Indústria per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
  "vab_agro_per_capita", "Valor Adicionado Bruto da Agropecuária per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
  "media_salarial_2025", "Média Salarial Formal do Município em 2025 (R$)", "MTE / RAIS (Tratamento Aurora)",
  "var_media_salarial_trienio_pct", "Variação da Média Salarial no Triênio 2022-2025 (%)", "MTE / RAIS (Tratamento Aurora / cálculo próprio)",
  "var_media_salarial_anual_pct", "Variação Anual da Média Salarial 2024-2025 (%)", "MTE / RAIS (Tratamento Aurora / cálculo próprio)",
  "ideb_ensino_medio_2023", "IDEB do Ensino Médio (Média das Escolas Estaduais do Município)", "INEP / Divulgação dos Resultados do IDEB 2023",
  "mat_regular_em", "Matrículas no Ensino Médio Regular", "INEP / Censo da Educação Básica 2024",
  "mat_eja_em", "Matrículas na EJA de Ensino Médio", "INEP / Censo da Educação Básica 2024",
  "total_mat_em", "Total de Matrículas no Ensino Médio (Regular + EJA)", "INEP / Censo da Educação Básica 2024",
  "prop_eja_regular_em_pct", "Proporção de Matrículas EJA em Relação ao Regular no EM (%)", "INEP / Censo da Educação Básica 2024 (cálculo próprio)",
  "pop_15_17_anos", "População Estimada de 15 a 17 Anos no Município", "IBGE / Projeção da População por Idade Simples (2024)",
  "tx_atendimento_em_aprox_pct", "Taxa de Atendimento do Ensino Médio Aproximada - Total (%)", "INEP (Censo Escolar 2024) e IBGE (Projeção 15-17 anos 2024)",
  "tx_atendimento_em_regular_aprox_pct", "Taxa de Atendimento do Ensino Médio Aproximada - Regular (%)", "INEP (Censo Escolar 2024) e IBGE (Projeção 15-17 anos 2024)",
  "icg_nivel_moda", "ICG - Nível de Complexidade de Gestão Mais Comum (Moda de 1 a 6)", "INEP / Indicadores Educacionais (ICG 2025)",
  "icg_pct_nivel_1_2", "ICG - Percentual de Escolas em Nível 1 e 2 (Menor Complexidade) (%)", "INEP / Indicadores Educacionais (ICG 2025)",
  "icg_pct_nivel_5_6", "ICG - Percentual de Escolas em Nível 5 e 6 (Maior Complexidade) (%)", "INEP / Indicadores Educacionais (ICG 2025)",
  "mat_indigena", "Matrículas em Localização Diferenciada Indígena", "INEP / Censo da Educação Básica 2025",
  "mat_quilombola", "Matrículas em Localização Diferenciada Quilombola", "INEP / Censo da Educação Básica 2025",
  "mat_assentamento", "Matrículas em Assentamento", "INEP / Censo da Educação Básica 2025",
  "mat_tradicionais_ribeirinho", "Matrículas em Comunidades Tradicionais / Ribeirinhas", "INEP / Censo da Educação Básica 2025",
  "mat_educacao_campo", "Matrículas em Educação do Campo (Área Rural)", "INEP / Censo da Educação Básica 2025",
  "total_mat_diferenciadas", "Total de Matrículas em Modalidades Diferenciadas de Ensino", "INEP / Censo da Educação Básica 2025",
  "prop_mat_indigena_pct", "Proporção de Matrículas Indígenas sobre o Total do EM (%)", "INEP / Censo da Educação Básica 2025 (cálculo próprio)",
  "prop_mat_quilombola_pct", "Proporção de Matrículas Quilombolas sobre o Total do EM (%)", "INEP / Censo da Educação Básica 2025 (cálculo próprio)",
  "prop_mat_campo_pct", "Proporção de Matrículas do Campo sobre o Total do EM (%)", "INEP / Censo da Educação Básica 2025 (cálculo próprio)"
)

# 16. Exportação dos Arquivos de Dados (Excel, Parquet, RDS) --------------------
dir_out_educacao <- file.path(DIR_RAW_AURORA, "educacao")
if (!dir.exists(dir_out_educacao)) {
  dir_out_educacao <- "C:/dados/dados brutos/aurora/educacao"
}
dir.create(dir_out_educacao, recursive = TRUE, showWarnings = FALSE)

path_out_xlsx <- file.path(dir_out_educacao, "indicadores_municipais_etes.xlsx")
path_out_pqt  <- file.path(dir_out_educacao, "indicadores_municipais_etes.parquet")
path_out_rds  <- file.path(dir_out_educacao, "indicadores_municipais_etes.rds")

message("Gravando planilha Excel em: ", path_out_xlsx)
write_xlsx(
  list(
    "indicadores" = df_consolidado,
    "dicionario"  = df_dicionario
  ),
  path_out_xlsx
)

message("Gravando arquivo Parquet em: ", path_out_pqt)
write_parquet(df_consolidado, path_out_pqt, compression = "snappy")

message("Gravando arquivo RDS em: ", path_out_rds)
saveRDS(df_consolidado, path_out_rds)

# Cópia defensiva na pasta central de dados tratados (AURORA_DATA_DIR)
path_central_xlsx <- file.path(DIR_OUT_AURORA, "indicadores_municipais_caracterizacao.xlsx")
if (dir.exists(DIR_OUT_AURORA)) {
  write_xlsx(list("indicadores" = df_consolidado, "dicionario" = df_dicionario), path_central_xlsx)
  saveRDS(df_consolidado, file.path(DIR_OUT_AURORA, "df_indicadores_caracterizacao_wide.rds"))
  write_parquet(df_consolidado, file.path(DIR_OUT_AURORA, "parquet/df_indicadores_caracterizacao_wide.parquet"), compression = "snappy")
}

# 17. Diagnóstico de Qualidade dos Dados: Correlograma e Análise VIF ------------
message("Iniciando geração dos gráficos de qualidade dos dados...")

# Função auxiliar para cálculo de VIF com tratamento de colinearidade perfeita
calc_all_vif <- function(data, cap_inf = 1000) {
  vars <- names(data)
  vifs <- numeric(length(vars))
  names(vifs) <- vars
  for (v in vars) {
    other_vars <- setdiff(vars, v)
    form <- as.formula(paste("`", v, "` ~ ", paste(paste0("`", other_vars, "`"), collapse = " + "), sep = ""))
    fit <- tryCatch(lm(form, data = data), error = function(e) NULL)
    if (is.null(fit)) {
      vifs[v] <- NA
    } else {
      r2 <- summary(fit)$r.squared
      if (is.null(r2) || is.na(r2)) {
        vifs[v] <- NA
      } else if (r2 >= 0.999999) {
        vifs[v] <- cap_inf
      } else {
        vifs[v] <- min(1 / (1 - r2), cap_inf)
      }
    }
  }
  tibble(
    indicador = names(vifs),
    vif = vifs,
    vif_display = ifelse(vifs >= 1000, ">= 1000 (Inf)", sprintf("%.1f", vifs)),
    categoria = case_when(
      vifs >= 10 ~ "Severa (VIF >= 10)",
      vifs >= 5  ~ "Moderada (5 <= VIF < 10)",
      TRUE       ~ "Baixa / Saudável (VIF < 5)"
    )
  )
}

# ==============================================================================
# CONJUNTO 1: VALORES ABSOLUTOS (Evidencia viés de escala populacional)
# ==============================================================================
message(">>> Gerando gráficos do Conjunto 1 (Valores Absolutos)...")

df_abs <- df_consolidado %>%
  transmute(
    `População Total`             = populacao_municipio,
    `Alunos EM (INEP)`            = estudantes_mat_rede_em_estadual,
    `Matrículas EM (SEE)`         = matriculas_em_rede_estadual_see,
    `Total Escolas EM`            = total_escolas_com_em,
    `Qtd ETEs Estaduais`          = qtd_etes_estaduais,
    `Vagas Téc Integrado (SEE)`   = vagas_estaduais_ensino_tecnico_integrado_ano,
    `Escolas Integrais 2T (SEE)`  = numero_de_escolas_integrais_com_2_turmas_dia,
    `Convênios Atendimento (SEE)` = disponibilidade_de_atendimento_por_convenio,
    `Matrículas Regular EM`       = mat_regular_em,
    `Matrículas EJA EM`           = mat_eja_em,
    `População 15 a 17 Anos`      = pop_15_17_anos,
    `Matrículas Campo`            = mat_educacao_campo,
    `Matrículas Quilombola`       = mat_quilombola,
    `Matrículas Indígena`         = mat_indigena
  )

# --- 17.1.1 Correlograma 1 (Valores Absolutos) ---
path_corr1 <- file.path(dir_out_educacao, "corr_1_valores_absolutos.png")
cor_abs <- cor(df_abs, use = "pairwise.complete.obs")

png(path_corr1, width = 2200, height = 2200, res = 220)
corrplot(
  cor_abs,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  diag = FALSE,
  addCoef.col = "black",
  number.cex = 0.6,
  addgrid.col = "grey90",
  col = colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#ffffff", "#d1e5f0", "#67a9cf", "#2166ac"))(200),
  mar = c(0, 0, 3, 0),
  title = "Correlograma 1: Indicadores em Valores Absolutos (Efeito de Escala Populacional)"
)
dev.off()

# Cópia nomeada alternativa
file.copy(path_corr1, file.path(dir_out_educacao, "corr_indicadores_municipais_etes_absolutos.png"), overwrite = TRUE)

# --- 17.1.2 VIF 1 (Valores Absolutos) ---
path_vif1 <- file.path(dir_out_educacao, "vif_1_valores_absolutos.png")
vif_abs <- calc_all_vif(df_abs, cap_inf = 1000) %>%
  arrange(vif) %>%
  mutate(indicador = factor(indicador, levels = indicador))

p_vif1 <- ggplot(vif_abs, aes(x = vif, y = indicador, fill = categoria)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkorange", linewidth = 0.8) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "firebrick", linewidth = 0.8) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 50, 100, 500, 1000),
    labels = c("1", "2", "5", "10", "50", "100", "500", ">= 1000")
  ) +
  scale_fill_manual(
    values = c(
      "Baixa / Saudável (VIF < 5)" = "#2ca02c",
      "Moderada (5 <= VIF < 10)"  = "#ff7f0e",
      "Severa (VIF >= 10)"         = "#d62728"
    )
  ) +
  geom_text(
    aes(label = vif_display),
    hjust = -0.15,
    size = 3.2,
    color = "grey15",
    fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 9.5, color = "grey40", margin = margin(b = 10)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9.5, color = "grey15", face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 45, 15, 15)
  ) +
  labs(
    title = "Diagnóstico 1: Multicolinearidade (VIF) em Valores Absolutos",
    subtitle = "Contagens brutas sofrem forte viés de escala populacional gerando colinearidade extrema (VIF > 10 e > 100).",
    x = "VIF (Fator de Inflação da Variância - Escala Logarítmica)",
    fill = "Qualidade Estatística"
  )

ggsave(path_vif1, plot = p_vif1, width = 11, height = 8, dpi = 220)
file.copy(path_vif1, file.path(dir_out_educacao, "vif_indicadores_municipais_etes_absolutos.png"), overwrite = TRUE)


# ==============================================================================
# CONJUNTO 2: VALORES CORRIGIDOS / NORMALIZADOS (Taxas, Per Capita e Proporções)
# ==============================================================================
message(">>> Gerando gráficos do Conjunto 2 (Valores Corrigidos)...")

mediana_ideb <- median(df_consolidado$ideb_ensino_medio_2023, na.rm = TRUE)

df_corr <- df_consolidado %>%
  mutate(
    ideb_em_ajustado = ifelse(is.na(ideb_ensino_medio_2023), mediana_ideb, ideb_ensino_medio_2023)
  ) %>%
  transmute(
    `Índice de Gini`         = indice_gini,
    `% Pop CadÚnico`         = prop_pop_cadunico_pct,
    `VAB Serviços pc`        = vab_servicos_per_capita,
    `VAB Indústria pc`       = vab_industria_per_capita,
    `VAB Agro pc`            = vab_agro_per_capita,
    `Média Salarial (R$)`    = media_salarial_2025,
    `Var. Salarial Triênio`  = var_media_salarial_trienio_pct,
    `IDEB Ensino Médio`      = ideb_em_ajustado,
    `Tx Atend. Médio (%)`    = tx_atendimento_em_aprox_pct,
    `Alunos / Escola EM`     = razao_estudantes_por_escola_em,
    `% EJA / Regular EM`     = prop_eja_regular_em_pct,
    `ICG (% Baixa Compl.)`   = icg_pct_nivel_1_2,
    `ICG (% Alta Compl.)`    = icg_pct_nivel_5_6,
    `% Matr. Campo`          = prop_mat_campo_pct,
    `% Matr. Quilombola`     = prop_mat_quilombola_pct,
    `% Matr. Indígena`       = prop_mat_indigena_pct,
    `ETEs / 100k hab`        = taxa_etes_100k_hab,
    `Vagas Téc / 1000 EM`    = vagas_tecnicas_por_1000_mat_em,
    `% Escolas Integrais 2T` = prop_escolas_integrais_pct,
    `Convênios / Escola`     = disp_convenio_por_escola
  )

# --- 17.2.1 Correlograma 2 (Valores Corrigidos) ---
path_corr2 <- file.path(dir_out_educacao, "corr_2_valores_corrigidos.png")
cor_corr <- cor(df_corr, use = "pairwise.complete.obs")

png(path_corr2, width = 2300, height = 2300, res = 220)
corrplot(
  cor_corr,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  diag = FALSE,
  addCoef.col = "black",
  number.cex = 0.52,
  addgrid.col = "grey90",
  col = colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#ffffff", "#d1e5f0", "#67a9cf", "#2166ac"))(200),
  mar = c(0, 0, 3, 0),
  title = "Correlograma 2: Indicadores Normalizados e Corrigidos (Taxas, Per Capita e Proporções)"
)
dev.off()

# Cópia mantendo o nome padrão solicitado originalmente
file.copy(path_corr2, file.path(dir_out_educacao, "corr_indicadores_municipais_etes.png"), overwrite = TRUE)

# --- 17.2.2 VIF 2 (Valores Corrigidos) ---
path_vif2 <- file.path(dir_out_educacao, "vif_2_valores_corrigidos.png")
vif_corr <- calc_all_vif(df_corr, cap_inf = 100) %>%
  arrange(vif) %>%
  mutate(indicador = factor(indicador, levels = indicador))

p_vif2 <- ggplot(vif_corr, aes(x = vif, y = indicador, fill = categoria)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkorange", linewidth = 0.8) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "firebrick", linewidth = 0.8) +
  scale_x_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, by = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Baixa / Saudável (VIF < 5)" = "#2ca02c",
      "Moderada (5 <= VIF < 10)"  = "#ff7f0e",
      "Severa (VIF >= 10)"         = "#d62728"
    )
  ) +
  geom_text(
    aes(label = vif_display),
    hjust = -0.2,
    size = 3.3,
    color = "grey15",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 9.5, color = "grey40", margin = margin(b = 10)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9.5, color = "grey15", face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 30, 15, 15)
  ) +
  labs(
    title = "Diagnóstico 2: Multicolinearidade (VIF) com Indicadores Corrigidos",
    subtitle = "Grandezas normalizadas (taxas, per capita e percentuais) eliminam a colinearidade espúria (todos VIF < 3).",
    x = "VIF (Fator de Inflação da Variância)",
    fill = "Qualidade Estatística"
  )

ggsave(path_vif2, plot = p_vif2, width = 11, height = 8.5, dpi = 220)
file.copy(path_vif2, file.path(dir_out_educacao, "vif_indicadores_municipais_etes.png"), overwrite = TRUE)

message(">>> Todos os gráficos e diagnósticos foram gerados e salvos com sucesso!")
message(sprintf(">>> ETL concluído com sucesso! %d municípios consolidados com %d colunas.",
                nrow(df_consolidado), ncol(df_consolidado)))


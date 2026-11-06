# ==============================================================================
# SCRIPT: etl_indicadores_caracterizacao_consolidada.R
# OBJETIVO: Consolidação tabular (Wide) dos indicadores municipais de Pernambuco
#   com pseudonimização alfanumérica de 3 dígitos (1 letra + 2 números).
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
  library(tidyr)
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

message(">>> Iniciando ETL de Consolidação Tabular de Indicadores Municipais...")

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
    pop_2026      = as.numeric(pop_2026)
  )

n_muns <- nrow(muns_canonical)
message(sprintf("Base municipal carregada: %d municípios de PE.", n_muns))

# 4. Geração da Coluna de Pseudonimização (1 Letra + 2 Números) ----------------
# Gera 185 códigos únicos, determinísticos e aleatórios (ex: A01, B42, X89)
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
# (Indígenas, Quilombolas, Assentamento, Tradicionais/Ribeirinhos e Campo)
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
# Como a escala de níveis do ICG é ordinal/não-linear, calculam-se 3 indicadores:
# 1) Moda (nível mais frequente de 1 a 6)
# 2) % em Nível 1 e 2 (menor complexidade)
# 3) % em Nível 5 e 6 (maior complexidade)
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

# 12. Consolidação Tabular Wide (Uma linha por Município) ----------------------
message("Consolidando tabela final com pseudonimização...")

df_consolidado <- muns_canonical %>%
  left_join(df_gini, by = "cod_municipio") %>%
  left_join(df_cad, by = "cod_municipio") %>%
  left_join(df_vab, by = "cod_municipio") %>%
  left_join(df_etes, by = "cod_municipio") %>%
  left_join(df_em_eja, by = "cod_municipio") %>%
  left_join(df_ideb_em, by = "cod_municipio") %>%
  left_join(df_diferenciadas, by = "cod_municipio") %>%
  left_join(df_pop_idade, by = "ibge6") %>%
  left_join(df_icg, by = "cod_municipio") %>%
  mutate(
    # Tratamento de NAs para contagens
    qtd_etes_estaduais = coalesce(qtd_etes_estaduais, 0L),
    qtd_etes_total     = coalesce(qtd_etes_total, 0L),
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
    )
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
    
    # 5. Número de ETE no município
    qtd_etes_estaduais,
    qtd_etes_total,
    
    # 6. VAB per capita
    vab_servicos_per_capita,
    vab_industria_per_capita,
    vab_agro_per_capita,
    
    # 7. IDEB EM
    ideb_ensino_medio_2023,
    
    # 8. Matrículas EJA e Regular no EM
    mat_regular_em,
    mat_eja_em,
    total_mat_em,
    prop_eja_regular_em_pct,
    
    # 9. Atendimento Líquido / Escolarização do Ensino Médio (Aproximado)
    pop_15_17_anos,
    tx_atendimento_em_aprox_pct,
    tx_atendimento_em_regular_aprox_pct,
    
    # 10. Índice de Complexidade de Gestão (ICG)
    icg_nivel_moda,
    icg_pct_nivel_1_2,
    icg_pct_nivel_5_6,
    
    # 11. Modalidades diferenciadas
    mat_indigena,
    mat_quilombola,
    mat_assentamento,
    mat_tradicionais_ribeirinho,
    mat_educacao_campo,
    total_mat_diferenciadas
  )

# 13. Dicionário de Metadados (Apenas no Excel) --------------------------------
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
  "vab_servicos_per_capita", "Valor Adicionado Bruto dos Serviços per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
  "vab_industria_per_capita", "Valor Adicionado Bruto da Indústria per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
  "vab_agro_per_capita", "Valor Adicionado Bruto da Agropecuária per Capita (R$/habitante)", "IBGE / Contas Regionais do Brasil",
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
  "total_mat_diferenciadas", "Total de Matrículas em Modalidades Diferenciadas de Ensino", "INEP / Censo da Educação Básica 2025"
)

# 14. Exportação dos Arquivos --------------------------------------------------
dir_out <- DIR_OUT_AURORA
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dir_out, "parquet"), recursive = TRUE, showWarnings = FALSE)

path_out_xlsx <- file.path(dir_out, "indicadores_municipais_caracterizacao.xlsx")
path_out_pqt  <- file.path(dir_out, "parquet/df_indicadores_caracterizacao_wide.parquet")
path_out_rds  <- file.path(dir_out, "df_indicadores_caracterizacao_wide.rds")

message("Gravando planilha Excel com abas 'indicadores' e 'dicionario' em: ", path_out_xlsx)
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

message(sprintf(">>> ETL concluído com sucesso! %d municípios consolidados com %d colunas.",
                nrow(df_consolidado), ncol(df_consolidado)))

message(sprintf(">>> ETL concluído com sucesso! %d municípios consolidados com %d colunas.",
                nrow(df_consolidado), ncol(df_consolidado)))

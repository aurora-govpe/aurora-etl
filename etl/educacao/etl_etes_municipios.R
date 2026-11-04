# ==============================================================================
# ETL: Indicadores Municipais para Análise de ETEs (Escolas Técnicas Estaduais)
# ==============================================================================
# Este pipeline realiza:
# 1. Leitura de C:/dados/aurora/parquet/df_indicadores.parquet.
# 2. Filtro dos indicadores solicitados nos seus respectivos últimos anos:
#    - Educação: todos os indicadores do grupo (IDEB, Aprovação, Reprovação,
#      Abandono, TDI, Matrículas integrais, Alfabetização, Creche, etc.).
#    - População: População Total (estimativa mais recente), Urbana, Rural,
#      População em Idade Ativa (PIA) e Razão de Dependência.
#    - PIB: PIB Total (R$ 1.000), PIB per capita e Participação no PIB PE (%).
#    - VAB: VAB Administração Pública, Agropecuária, Indústria, Serviços e InfoCom.
# 3. Enriquecimento com dados municipais canônicos (GRE, Região de Desenvolvimento).
# 4. Cruzamento com base de ETEs (C:/dados/dados brutos/aurora/educacao/CEB/etes_municipios_pe.xlsx),
#    sinalizando municípios atendidos por ETEs e métricas de matrículas profissionais.
# 5. Exportação:
#    - Formato Wide / Tabular em Excel (.xlsx) com uma linha por município.
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(writexl)
  library(janitor)
})

cat("🚀 Iniciando etl_etes_municipios...\n\n")

# 2. Resolução de Caminhos -----------------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else if (file.exists("../../R/paths.R")) {
  source("../../R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else {
  is_linux <- Sys.info()[["sysname"]] == "Linux"
  DIR_RAW_AURORA <- Sys.getenv("AURORA_RAW_DIR", if (is_linux) "/srv/dados_brutos/aurora" else "C:/dados/dados brutos/aurora")
  DIR_OUT_AURORA <- Sys.getenv("AURORA_DATA_DIR", if (is_linux) "/srv/dados/aurora" else "C:/dados/aurora")
}

path_indicadores_parquet <- file.path(DIR_OUT_AURORA, "parquet/df_indicadores.parquet")
path_demografia_parquet  <- file.path(DIR_OUT_AURORA, "parquet/df_demografia_pe.parquet")
path_populacao_rds       <- file.path(DIR_OUT_AURORA, "populacao_mun.rds")
path_etes_xlsx           <- file.path(DIR_RAW_AURORA, "educacao/CEB/etes_municipios_pe.xlsx")

# Fallback se arquivos estiverem em diretórios alternativos
if (!file.exists(path_indicadores_parquet)) {
  path_indicadores_parquet <- "C:/dados/aurora/parquet/df_indicadores.parquet"
}

dir_out_educacao <- file.path(DIR_OUT_AURORA, "educacao")
dir_out_parquet  <- file.path(DIR_OUT_AURORA, "parquet")
dir.create(dir_out_educacao, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out_parquet,  recursive = TRUE, showWarnings = FALSE)

# 3. Referência Municipal Canônica de Pernambuco ------------------------------
cat("📍 1. Carregando malha e referência municipal de Pernambuco...\n")
if (file.exists(path_demografia_parquet)) {
  ref_mun <- arrow::read_parquet(path_demografia_parquet) %>%
    select(
      ibge6,
      cod_ibge = ibge7,
      nm_municipio = municipio,
      regiao_de_desenvolvimento,
      gerencia_regional_de_educacao,
      pop_2026
    ) %>%
    mutate(
      ibge6    = as.character(ibge6),
      cod_ibge = as.character(cod_ibge)
    )
} else {
  # Fallback caso não encontre df_demografia_pe
  ref_mun <- tibble::tibble(
    ibge6 = character(),
    cod_ibge = character(),
    nm_municipio = character(),
    regiao_de_desenvolvimento = character(),
    gerencia_regional_de_educacao = character(),
    pop_2026 = numeric()
  )
}
cat("   ->", nrow(ref_mun), "municípios de referência carregados.\n\n")

# 4. Leitura e Filtragem de df_indicadores.parquet -----------------------------
cat("📊 2. Lendo", path_indicadores_parquet, "...\n")
if (!file.exists(path_indicadores_parquet)) {
  stop("Arquivo de indicadores não encontrado em: ", path_indicadores_parquet)
}

df_ind_raw <- arrow::read_parquet(path_indicadores_parquet) %>%
  mutate(
    ibge6 = substr(as.character(cod_municipio), 1, 6)
  )

cat("   -> Total de registros na base bruta:", nrow(df_ind_raw), "\n")

# Mapear siglas de indicadores solicitados
# a) Educação: todos do grupo 'educação'
siglas_educacao <- df_ind_raw %>%
  filter(str_detect(tolower(grupo), "educa")) %>%
  pull(sigla_indicador) %>%
  unique()

# b) PIB
siglas_pib <- c("pib_1000", "pibpc", "pib_pct")

# c) VAB
siglas_vab <- c("vab_agro", "vab_ind", "vab_serv", "vab_adm", "pct_vab_infocom_serv")

# d) População
siglas_pop <- c("perc_pop_urb", "perc_pop_rur", "pct_pia", "rd_pop")

todas_siglas <- unique(c(siglas_educacao, siglas_pib, siglas_vab, siglas_pop))

cat("   -> Indicadores selecionados (", length(todas_siglas), "):\n")
cat("      * Educação (", length(siglas_educacao), "): ", paste(siglas_educacao, collapse = ", "), "\n")
cat("      * População: ", paste(siglas_pop, collapse = ", "), "\n")
cat("      * PIB: ", paste(siglas_pib, collapse = ", "), "\n")
cat("      * VAB: ", paste(siglas_vab, collapse = ", "), "\n\n")

# 5. Filtrar os respectivos últimos anos por município e indicador --------------
cat("⏳ 3. Filtrando o último ano disponível para cada indicador...\n")
df_ultimos_anos <- df_ind_raw %>%
  filter(sigla_indicador %in% todas_siglas) %>%
  group_by(ibge6, sigla_indicador) %>%
  slice_max(order_by = ano, n = 1, with_ties = FALSE) %>%
  ungroup()

# 6. Complementar com População Total ------------------------------------------
# Se populacao_mun.rds ou pop_2026 estiver disponível, adicionamos como indicador
cat("👥 4. Consolidando indicador de População Total...\n")
pop_adicional <- if (file.exists(path_populacao_rds)) {
  readRDS(path_populacao_rds) %>%
    mutate(
      ibge6 = substr(as.character(cod_ibge), 1, 6),
      exercicio = as.numeric(exercicio)
    ) %>%
    group_by(ibge6) %>%
    slice_max(order_by = exercicio, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(ref_mun %>% select(ibge6, cod_ibge_ref = cod_ibge, nm_municipio), by = "ibge6") %>%
    transmute(
      cod_municipio  = coalesce(cod_ibge_ref, cod_ibge),
      nm_municipio,
      ano            = exercicio,
      nm_indicador   = "População Total",
      nm_fantasia    = "População Total",
      indicador      = as.character(populacao),
      mes_referencia = NA_character_,
      sigla_indicador= "populacao_total",
      grupo          = "demografia",
      tipo           = "absoluto",
      polaridade     = "positiva",
      cod_ibge       = coalesce(cod_ibge_ref, cod_ibge),
      ibge6,
      rank_pe        = rank(-populacao, ties.method = "min")
    )
} else if (nrow(ref_mun) > 0 && any(!is.na(ref_mun$pop_2026))) {
  ref_mun %>%
    transmute(
      cod_municipio  = cod_ibge,
      nm_municipio,
      ano            = 2026,
      nm_indicador   = "População Total",
      nm_fantasia    = "População Total",
      indicador      = as.character(pop_2026),
      mes_referencia = NA_character_,
      sigla_indicador= "populacao_total",
      grupo          = "demografia",
      tipo           = "absoluto",
      polaridade     = "positiva",
      cod_ibge,
      ibge6,
      rank_pe        = rank(-pop_2026, ties.method = "min")
    )
} else {
  NULL
}

# 7. Unificação do Formato Long Canônico ----------------------------------------
if (nrow(ref_mun) > 0) {
  df_long_final <- df_ultimos_anos %>%
    select(-any_of(c("cod_ibge", "nm_municipio"))) %>%
    inner_join(
      ref_mun %>% select(ibge6, cod_ibge, nm_municipio),
      by = "ibge6"
    ) %>%
    mutate(cod_municipio = cod_ibge)
} else {
  df_long_final <- df_ultimos_anos
}

if (!is.null(pop_adicional)) {
  df_long_final <- bind_rows(df_long_final, pop_adicional)
}

df_long_final <- df_long_final %>%
  arrange(cod_municipio, sigla_indicador)

cat("   -> Total de linhas filtradas (Long):", nrow(df_long_final), "\n")
cat("   -> Total de municípios únicos:", n_distinct(df_long_final$ibge6), "\n")
cat("   -> Total de indicadores únicos:", n_distinct(df_long_final$sigla_indicador), "\n\n")

# 8. Cruzamento com Informações de ETEs -----------------------------------------
cat("🏫 5. Cruzando com dados de ETEs (Escolas Técnicas)...\n")
df_etes_info <- if (file.exists(path_etes_xlsx)) {
  read_excel(path_etes_xlsx) %>%
    janitor::clean_names() %>%
    mutate(
      ibge6 = substr(as.character(co_municipio), 1, 6),
      tem_ete = 1L
    ) %>%
    select(
      ibge6,
      tem_ete,
      total_escolas_ete_fed = total_escolas_fed,
      total_escolas_ete_est = total_escolas_est,
      total_escolas_ete     = total_escolas,
      qt_mat_prof,
      qt_mat_prof_tec,
      qt_mat_prof_tec_subs
    )
} else {
  cat("   ⚠️ Arquivo etes_municipios_pe.xlsx não localizado em:", path_etes_xlsx, "\n")
  NULL
}

# Adicionar flag de ETE também no formato long
if (!is.null(df_etes_info)) {
  df_long_final <- df_long_final %>%
    left_join(df_etes_info %>% select(ibge6, tem_ete), by = "ibge6") %>%
    mutate(tem_ete = coalesce(tem_ete, 0L))
}

# 9. Geração do Formato Wide (Tabular / 1 Linha por Município) ------------------
cat("📐 6. Gerando formato Wide (uma linha por município)...\n")
df_wide_valores <- df_long_final %>%
  mutate(indicador_num = suppressWarnings(as.numeric(indicador))) %>%
  select(ibge6, sigla_indicador, indicador_num) %>%
  pivot_wider(
    names_from = sigla_indicador,
    values_from = indicador_num
  )

# Base com metadados do município
df_wide_final <- if (nrow(ref_mun) > 0) {
  ref_mun %>%
    select(
      cod_ibge,
      ibge6,
      nm_municipio,
      gerencia_regional_de_educacao,
      regiao_de_desenvolvimento
    ) %>%
    left_join(df_wide_valores, by = "ibge6")
} else {
  df_long_final %>%
    distinct(cod_municipio, ibge6, nm_municipio) %>%
    left_join(df_wide_valores, by = "ibge6")
}

# Incorporar dados de ETEs no Wide
if (!is.null(df_etes_info)) {
  df_wide_final <- df_wide_final %>%
    left_join(df_etes_info, by = "ibge6") %>%
    mutate(
      tem_ete               = coalesce(tem_ete, 0L),
      total_escolas_ete_fed = coalesce(total_escolas_ete_fed, 0),
      total_escolas_ete_est = coalesce(total_escolas_ete_est, 0),
      total_escolas_ete     = coalesce(total_escolas_ete, 0),
      qt_mat_prof           = coalesce(qt_mat_prof, 0),
      qt_mat_prof_tec       = coalesce(qt_mat_prof_tec, 0),
      qt_mat_prof_tec_subs  = coalesce(qt_mat_prof_tec_subs, 0)
    ) %>%
    relocate(tem_ete, total_escolas_ete, qt_mat_prof_tec, .after = nm_municipio)
}

# Criar recorte exclusivo para os municípios com ETEs
df_apenas_etes <- if (!is.null(df_etes_info)) {
  df_wide_final %>% filter(tem_ete == 1)
} else {
  NULL
}

# 10. Gravação da Saída (Apenas Wide Excel) -----------------------------------
cat("💾 7. Gravando saída Wide (Excel)...\n")

file_wide_xlsx <- file.path(dir_out_educacao, "df_etes_municipios_indicadores.xlsx")
writexl::write_xlsx(df_wide_final, file_wide_xlsx)
cat("   [OK] Wide Excel gravado em:", file_wide_xlsx, "\n")

cat("\n==============================================================================\n")
cat("✅ ETL etl_etes_municipios concluído com sucesso!\n")
cat("   * Total de municípios processados:", nrow(df_wide_final), "\n")
if (!is.null(df_etes_info)) {
  cat("   * Municípios com ETE:            ", sum(df_wide_final$tem_ete == 1), "\n")
  cat("   * Municípios sem ETE:            ", sum(df_wide_final$tem_ete == 0), "\n")
}
cat("   * Total de colunas:              ", ncol(df_wide_final), "\n")
cat("   * Arquivo de saída:              ", file_wide_xlsx, "\n")
cat("==============================================================================\n")

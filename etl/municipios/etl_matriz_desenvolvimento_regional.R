# ==============================================================================
# etl/municipios/etl_matriz_desenvolvimento_regional.R
# ==============================================================================
# Objetivo: Construir a Matriz de Indicadores de Planejamento e Desenvolvimento
#           Regional para todos os 185 municípios de Pernambuco (184 municípios
#           + Fernando de Noronha), consolidando os 17 indicadores viáveis da
#           Matriz Territorial v2.
#
# Entradas:
#   - DIR_OUT_AURORA/parquet/df_demografia_pe.parquet
#   - DIR_OUT_AURORA/parquet/regic_pe_main.parquet
#   - DIR_OUT_AURORA/parquet/arranjos_populacionais_pe.parquet
#   - DIR_RAW_AURORA/lista_municipios_Semiarido_2022.xlsx (ou DIR_OUT_AURORA)
#   - DIR_OUT_AURORA/parquet/df_indicadores.parquet
#   - DIR_OUT_AURORA/df_rais_tecnologia.parquet (ou parquet/)
#   - DIR_RAW_AURORA/rodovias_extensoes_sf.rds (ou DIR_OUT_AURORA)
#
# Saídas:
#   - DIR_OUT_AURORA/parquet/matriz_desenvolvimento_regional.parquet
#   - DIR_OUT_AURORA/matriz_desenvolvimento_regional.parquet
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
})

cat("==============================================================================\n")
cat("Iniciando ETL: Matriz de Desenvolvimento Regional de Pernambuco\n")
cat("==============================================================================\n\n")

# 1. Resolução centralizada de caminhos ----
caminhos_paths <- c("R/paths.R", "paths.R", "../paths.R", "etl/paths.R", "../../R/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"

parquet_aurora <- file.path(DIR_OUT_AURORA, "parquet")

# ------------------------------------------------------------------------------
# 1. Base Cadastral Demográfica Municipal
# ------------------------------------------------------------------------------
cat("1. Carregando base cadastral demografica (df_demografia_pe)...\n")
demo_path <- file.path(parquet_aurora, "df_demografia_pe.parquet")
df_demo <- arrow::read_parquet(demo_path)

matriz <- df_demo %>%
  transmute(
    cod_ibge = as.character(ibge7),
    ibge6    = substr(as.character(ibge7), 1, 6),
    municipio = as.character(municipio),
    rd_pe    = as.character(regiao_de_desenvolvimento),
    pop_total = as.numeric(pop_2026)
  )

cat("   -> Municípios carregados:", nrow(matriz), "\n")

# ------------------------------------------------------------------------------
# 2. Tipologia Urbana REGIC (com fallback de arranjos conurbados)
# ------------------------------------------------------------------------------
cat("2. Mapeando tipologia REGIC (IBGE)...\n")
regic_main <- arrow::read_parquet(file.path(parquet_aurora, "regic_pe_main.parquet")) %>%
  transmute(cod_ibge = as.character(name), hierarquia = as.character(hierarquia))

arranjos <- arrow::read_parquet(file.path(parquet_aurora, "arranjos_populacionais_pe.parquet")) %>%
  transmute(cod_ibge = as.character(cod_ibge), cod_arranjo = as.character(cod_arranjo))

matriz <- matriz %>%
  left_join(regic_main, by = "cod_ibge") %>%
  left_join(arranjos, by = "cod_ibge") %>%
  left_join(regic_main %>% rename(hierarquia_arranjo = hierarquia), by = c("cod_arranjo" = "cod_ibge")) %>%
  mutate(
    tipologia_regic = coalesce(hierarquia, hierarquia_arranjo, "Centro Local")
  ) %>%
  select(-hierarquia, -cod_arranjo, -hierarquia_arranjo)

cat("   -> Tipologias mapeadas com sucesso.\n")

# ------------------------------------------------------------------------------
# 3. Delimitação Oficial do Semiárido 2022
# ------------------------------------------------------------------------------
cat("3. Integrando delimitacao oficial do Semiarido (SUDENE/MDR 2022)...\n")
semi_candidatos <- c(
  file.path(DIR_RAW_AURORA, "lista_municipios_Semiarido_2022.xlsx"),
  file.path(DIR_OUT_AURORA, "lista_municipios_Semiarido_2022.xlsx"),
  "data/lista_municipios_Semiarido_2022.xlsx"
)

semi_path <- NULL
for (sp in semi_candidatos) {
  if (file.exists(sp)) { semi_path <- sp; break }
}

if (!is.null(semi_path)) {
  df_semi <- readxl::read_excel(semi_path, sheet = "1477 mun", col_types = "text")
  semi_pe_codes <- df_semi %>%
    filter(str_starts(CD_MUN, "26")) %>%
    pull(CD_MUN) %>%
    str_trim()
  matriz <- matriz %>%
    mutate(semiarido = if_else(cod_ibge %in% semi_pe_codes, 1L, 0L))
  cat("   -> Semiárido integrado:", sum(matriz$semiarido), "municípios no Semiárido.\n")
} else {
  warning("Arquivo lista_municipios_Semiarido_2022.xlsx não encontrado; mantendo semiárido zerado.")
  matriz <- matriz %>% mutate(semiarido = 0L)
}

# ------------------------------------------------------------------------------
# 4. Indicadores Econômicos e Demográficos (df_indicadores)
# ------------------------------------------------------------------------------
cat("4. Extraindo indicadores de df_indicadores.parquet...\n")
df_ind <- arrow::read_parquet(file.path(parquet_aurora, "df_indicadores.parquet"))

# Último ano para cada indicador desejado
ind_recente <- df_ind %>%
  filter(sigla_indicador %in% c("pct_vab_serv", "pct_vab_adm", "pct_vab_ind", "pct_vab_agro", 
                                "reposicao_pia", "rd_jovem", "ivcad_taxa", "deficit_esgoto")) %>%
  group_by(codibge, sigla_indicador) %>%
  filter(ano == max(ano, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  select(cod_ibge = codibge, sigla_indicador, valor) %>%
  pivot_wider(names_from = sigla_indicador, values_from = valor)

matriz <- matriz %>% left_join(ind_recente, by = "cod_ibge")
cat("   -> Indicadores de df_indicadores integrados.\n")

# ------------------------------------------------------------------------------
# 5. Empregos de Serviços e TI (RAIS)
# ------------------------------------------------------------------------------
cat("5. Integrando empregos da RAIS Tecnologia...\n")
p_rais <- if (file.exists(file.path(parquet_aurora, "df_rais_tecnologia.parquet"))) {
  file.path(parquet_aurora, "df_rais_tecnologia.parquet")
} else {
  file.path(DIR_OUT_AURORA, "df_rais_tecnologia.parquet")
}

if (file.exists(p_rais)) {
  df_rais <- arrow::read_parquet(p_rais)
  ultimo_ano_rais <- max(df_rais$ano, na.rm = TRUE)
  rais_mun <- df_rais %>%
    filter(ano == ultimo_ano_rais) %>%
    group_by(cod_municipio = as.character(cod_municipio)) %>%
    summarise(
      total_emp_serv = sum(n_vinculos, na.rm = TRUE),
      total_emp_infocom = sum(n_vinculos[is_ti == 1 | is_infocom == 1], na.rm = TRUE),
      .groups = "drop"
    )
  matriz <- matriz %>%
    left_join(rais_mun, by = c("ibge6" = "cod_municipio")) %>%
    mutate(
      total_emp_serv = coalesce(total_emp_serv, 0),
      total_emp_infocom = coalesce(total_emp_infocom, 0)
    )
  cat("   -> Empregos da RAIS integrados.\n")
} else {
  matriz <- matriz %>%
    mutate(total_emp_serv = 0, total_emp_infocom = 0)
}

# ------------------------------------------------------------------------------
# 6. Eixos Rodoviários Principais
# ------------------------------------------------------------------------------
cat("6. Mapeando eixos rodoviários...\n")
p_roads <- file.path(DIR_OUT_AURORA, "rodovias_extensoes_sf.rds")
if (!file.exists(p_roads)) p_roads <- file.path(DIR_RAW_AURORA, "rodovias_extensoes_sf.rds")

if (file.exists(p_roads)) {
  roads_sf <- readRDS(p_roads)
  df_mun_roads <- as.data.frame(roads_sf) %>%
    mutate(
      mun = stringr::str_trim(as.character(municipio)),
      rodovia = stringr::str_trim(as.character(rodovia)),
      ext = as.numeric(extensao_km),
      is_br = stringr::str_detect(rodovia, "^BR")
    ) %>%
    filter(mun != "")
  
  df_principal_road <- df_mun_roads %>%
    arrange(mun, desc(is_br), desc(ext)) %>%
    group_by(mun) %>%
    slice(1) %>%
    ungroup() %>%
    select(municipio = mun, eixo_rodoviario = rodovia)
  
  matriz <- matriz %>% left_join(df_principal_road, by = "municipio")
} else {
  matriz <- matriz %>% mutate(eixo_rodoviario = "Não identificado")
}

# Preenchimento de NAs residuais
matriz <- matriz %>%
  mutate(
    pct_vab_serv = coalesce(pct_vab_serv, 0),
    pct_vab_adm = coalesce(pct_vab_adm, 0),
    pct_vab_ind = coalesce(pct_vab_ind, 0),
    pct_vab_agro = coalesce(pct_vab_agro, 0),
    total_emp_serv = coalesce(total_emp_serv, 0),
    total_emp_infocom = coalesce(total_emp_infocom, 0),
    reposicao_pia = coalesce(reposicao_pia, 0),
    rd_jovem = coalesce(rd_jovem, 0),
    ivcad_taxa = coalesce(ivcad_taxa, 0),
    deficit_esgoto = coalesce(deficit_esgoto, 0),
    eixo_rodoviario = coalesce(eixo_rodoviario, "Não identificado")
  )

# ------------------------------------------------------------------------------
# 7. Formatação Final e Verificação de Integridade
# ------------------------------------------------------------------------------
cols_17 <- c(
  "cod_ibge", "municipio", "rd_pe", "tipologia_regic", "semiarido", "pop_total",
  "pct_vab_serv", "pct_vab_adm", "pct_vab_ind", "pct_vab_agro",
  "total_emp_serv", "total_emp_infocom", "reposicao_pia", "rd_jovem",
  "ivcad_taxa", "deficit_esgoto", "eixo_rodoviario"
)

matriz_final <- matriz %>% select(all_of(cols_17))

cat("\nVerificação de integridade da Matriz:\n")
cat("   - Linhas:", nrow(matriz_final), "x Colunas:", ncol(matriz_final), "\n")

# ------------------------------------------------------------------------------
# 8. Exportação dos Arquivos Parquet
# ------------------------------------------------------------------------------
out_paths <- c(
  file.path(parquet_aurora, "matriz_desenvolvimento_regional.parquet"),
  file.path(DIR_OUT_AURORA, "matriz_desenvolvimento_regional.parquet")
)

for (op in out_paths) {
  dir.create(dirname(op), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(matriz_final, op)
  cat("   -> Salvo com sucesso em:", op, "\n")
}

cat("\n==============================================================================\n")
cat("ETL Matriz de Desenvolvimento Regional concluído com sucesso!\n")
cat("==============================================================================\n")

# ==============================================================================
# etl/municipios/etl_matriz_desenvolvimento_regional.R
# ==============================================================================
# Objetivo: Construir a Matriz de Indicadores de Planejamento e Desenvolvimento
#           Regional para todos os 185 municípios de Pernambuco (184 municípios
#           + Fernando de Noronha), consolidando os 17 indicadores viáveis da
#           Matriz Territorial v2.
#
# Entradas:
#   - app_aurora_municipios/data/parquet/df_demografia_pe.parquet
#   - app_aurora_municipios/data/parquet/regic_pe_main.parquet
#   - app_aurora_municipios/data/parquet/arranjos_populacionais_pe.parquet
#   - app_aurora_municipios/data/lista_municipios_Semiarido_2022.xlsx
#   - app_aurora_municipios/data/parquet/df_indicadores.parquet
#   - app_aurora_municipios/data/df_rais_tecnologia.parquet
#   - app_aurora_municipios/data/rodovias_extensoes_sf.rds
#
# Saídas:
#   - data/indicadores/matriz_desenvolvimento_regional.parquet
#   - app_aurora_municipios/data/matriz_desenvolvimento_regional.parquet
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

# Caminhos base
proj_dir <- "C:/Users/hugo.vasconcelos/Documents/projetos/segpr_ndgr/auroraprime"
data_aurora <- file.path(proj_dir, "app_aurora_municipios/data")
parquet_aurora <- file.path(data_aurora, "parquet")

out_dir_1 <- file.path(proj_dir, "data/indicadores")
out_dir_2 <- file.path(proj_dir, "app_aurora_municipios/data")

if (!dir.exists(out_dir_1)) dir.create(out_dir_1, recursive = TRUE)
if (!dir.exists(out_dir_2)) dir.create(out_dir_2, recursive = TRUE)

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
semi_path <- file.path(data_aurora, "lista_municipios_Semiarido_2022.xlsx")
df_semi <- readxl::read_excel(semi_path, sheet = "1477 mun", col_types = "text")

semi_pe_codes <- df_semi %>%
  filter(str_starts(CD_MUN, "26")) %>%
  pull(CD_MUN) %>%
  str_trim()

matriz <- matriz %>%
  mutate(semiarido = if_else(cod_ibge %in% semi_pe_codes, 1L, 0L))

cat("   -> Municípios no Semiárido (valor 1):", sum(matriz$semiarido == 1), "\n")
cat("   -> Municípios fora do Semiárido (valor 0):", sum(matriz$semiarido == 0), "\n")

# ------------------------------------------------------------------------------
# 4. Indicadores do df_indicadores (VAB, IR_PIA, RD_Jovem, IVCAD_DR, Esgoto)
# ------------------------------------------------------------------------------
cat("4. Processando indicadores setoriais (VAB, Demografia, IVCAD, Saneamento)...\n")
df_ind <- arrow::read_parquet(file.path(parquet_aurora, "df_indicadores.parquet")) %>%
  mutate(
    ibge6 = substr(as.character(cod_ibge), 1, 6),
    indicador_num = suppressWarnings(as.numeric(indicador))
  )

# 4.1. VAB Setorial (%) - Ano 2021
vab_piv <- df_ind %>%
  filter(sigla_indicador %in% c("vab_serv", "vab_adm", "vab_ind", "vab_agro"), ano == 2021) %>%
  select(ibge6, sigla_indicador, indicador_num) %>%
  pivot_wider(names_from = sigla_indicador, values_from = indicador_num) %>%
  mutate(
    vab_total = vab_serv + vab_adm + vab_ind + vab_agro,
    pct_vab_serv = (vab_serv / vab_total) * 100,
    pct_vab_adm  = (vab_adm / vab_total) * 100,
    pct_vab_ind  = (vab_ind / vab_total) * 100,
    pct_vab_agro = (vab_agro / vab_total) * 100
  ) %>%
  select(ibge6, pct_vab_serv, pct_vab_adm, pct_vab_ind, pct_vab_agro)

matriz <- matriz %>% left_join(vab_piv, by = "ibge6")

# 4.2. Demografia: IR_PIA e RD_Jovem - Ano 2026
demo_piv <- df_ind %>%
  filter(sigla_indicador %in% c("ind_ir_pia", "rd_jovem"), ano == 2026) %>%
  select(ibge6, sigla_indicador, indicador_num) %>%
  pivot_wider(names_from = sigla_indicador, values_from = indicador_num) %>%
  rename(reposicao_pia = ind_ir_pia)

matriz <- matriz %>% left_join(demo_piv, by = "ibge6")

# 4.3. IVCAD - Disponibilidade de Recursos (DR) - Ano 2026
dr_df <- df_ind %>%
  filter(sigla_indicador == "ivcad_dr", ano == 2026) %>%
  select(ibge6, ivcad_taxa = indicador_num)

matriz <- matriz %>% left_join(dr_df, by = "ibge6")

# 4.4. Saneamento: Déficit de Esgotamento Sanitário (Censo)
esg_df <- df_ind %>%
  filter(sigla_indicador == "pct_esgoto_rede") %>%
  select(ibge6, pct_esgoto_rede = indicador_num) %>%
  mutate(deficit_esgoto = 100 - pct_esgoto_rede) %>%
  select(ibge6, deficit_esgoto)

matriz <- matriz %>% left_join(esg_df, by = "ibge6")

cat("   -> Indicadores do painel vinculados com sucesso.\n")

# ------------------------------------------------------------------------------
# 5. Mercado de Trabalho / Empresas (RAIS Tecnologia 2025)
# ------------------------------------------------------------------------------
cat("5. Vinculando estoque de empresas de Servicos e Infocom (RAIS 2025)...\n")
df_rais <- arrow::read_parquet(file.path(data_aurora, "df_rais_tecnologia.parquet"))

rais_piv <- df_rais %>%
  filter(sigla_indicador %in% c("estab_serv", "estab_infocom"), ano == 2025) %>%
  transmute(
    cod_ibge = as.character(id_municipio),
    sigla_indicador,
    valor = as.integer(valor)
  ) %>%
  pivot_wider(names_from = sigla_indicador, values_from = valor) %>%
  rename(
    total_emp_serv = estab_serv,
    total_emp_infocom = estab_infocom
  )

matriz <- matriz %>% left_join(rais_piv, by = "cod_ibge")

cat("   -> Empresas RAIS vinculadas.\n")

# ------------------------------------------------------------------------------
# 6. Eixo Rodoviário Principal (rodovias_extensoes_sf.rds)
# ------------------------------------------------------------------------------
cat("6. Extraindo eixo rodoviario principal por municipio (rodovias_extensoes_sf)...\n")
p_roads <- file.path(data_aurora, "rodovias_extensoes_sf.rds")
df_roads <- readRDS(p_roads)

if (inherits(df_roads, "sf")) {
  df_roads <- sf::st_drop_geometry(df_roads)
}

# Desaninha a lista de municípios para criar tabela relacional: rodovia <-> município
df_mun_roads <- df_roads %>%
  mutate(
    rodovia = str_trim(as.character(rodovia)),
    ext = as.numeric(ext),
    is_br = str_starts(rodovia, "BR-")
  ) %>%
  separate_rows(mun, sep = ",") %>%
  mutate(mun = str_trim(mun)) %>%
  filter(mun != "")

# Regra hierárquica:
# 1. Preferência para rodovias federais (is_br == TRUE), ordenadas por extensão decrescente
# 2. Em seguida, rodovias estaduais, ordenadas por extensão decrescente
df_principal_road <- df_mun_roads %>%
  arrange(mun, desc(is_br), desc(ext)) %>%
  group_by(mun) %>%
  slice(1) %>%
  ungroup() %>%
  select(municipio = mun, eixo_rodoviario = rodovia)

matriz <- matriz %>% left_join(df_principal_road, by = "municipio")

cat("   -> Eixos rodoviários identificados para todos os municípios.\n")

# ------------------------------------------------------------------------------
# 7. Formatação Final e Verificação de Integridade
# ------------------------------------------------------------------------------
cols_17 <- c(
  "cod_ibge", "municipio", "rd_pe", "tipologia_regic", "semiarido", "pop_total",
  "pct_vab_serv", "pct_vab_adm", "pct_vab_ind", "pct_vab_agro",
  "total_emp_serv", "total_emp_infocom", "reposicao_pia", "rd_jovem",
  "ivcad_taxa", "deficit_esgoto", "eixo_rodoviario"
)

matriz_final <- matriz %>%
  select(all_of(cols_17))

cat("\n------------------------------------------------------------------------------\n")
cat("Verificacao de Integridade da Matriz Consolidada:\n")
cat("   - Dimensoes:", nrow(matriz_final), "linhas por", ncol(matriz_final), "colunas\n")
cat("   - Total de valores NA por coluna:\n")
na_counts <- colSums(is.na(matriz_final))
print(na_counts)

if (any(na_counts > 0)) {
  warning("Atencao: Existem colunas com valores NA!")
} else {
  cat("\n   [OK] Todos os 17 indicadores estao 100% preenchidos sem nenhum valor nulo!\n")
}

# ------------------------------------------------------------------------------
# 8. Exportação dos Arquivos Parquet
# ------------------------------------------------------------------------------
file_name <- "matriz_desenvolvimento_regional.parquet"
file_name_2 <- "matriz_desenvolvimento_regional.csv"
path_out_1 <- file.path(out_dir_1, file_name)
path_out_2 <- file.path(out_dir_2, file_name)
path_out_3 <- file.path(out_dir_1, file_name_2)

cat("\nExportando arquivos Parquet...\n")
arrow::write_parquet(matriz_final, path_out_1)
write.csv2(matriz_final, path_out_3)
cat("   -> Salvo com sucesso em:", path_out_1, "\n")

arrow::write_parquet(matriz_final, path_out_2)
cat("   -> Salvo com sucesso em:", path_out_2, "\n")

cat("\n==============================================================================\n")
cat("ETL concluido com sucesso!\n")
cat("==============================================================================\n")

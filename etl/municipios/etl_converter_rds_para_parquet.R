# etl/municipios/etl_converter_rds_para_parquet.R
# Script de conversão das bases legadas RDS para Parquet otimizado
# Aurora Municípios

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(jsonlite)
})

cat("Iniciando conversao e sincronizacao de dados para Parquet (Aurora Municipios)...\n")
dir_origem <- "C:/Users/hugo.vasconcelos/Documents/projetos/segpr_ndgr/auroraprime/app_aurora_municipios/data"
dir_dest <- file.path(dir_origem, "parquet")
if (!dir.exists(dir_dest)) dir.create(dir_dest, recursive = TRUE)

# 1. Demografia
cat("1. Convertendo df_demografia_pe...\n")
demo <- readRDS(file.path(dir_origem, "df_demografia_pe.rds"))
if (inherits(demo, "sf")) demo <- sf::st_drop_geometry(demo)
if ("ibge6" %in% names(demo)) demo$ibge6 <- as.character(demo$ibge6)
if ("ibge7" %in% names(demo)) demo$ibge7 <- as.character(demo$ibge7)
arrow::write_parquet(demo, file.path(dir_dest, "df_demografia_pe.parquet"))
cat("   -> df_demografia_pe.parquet salvo com", nrow(demo), "linhas.\n")

# 2. Emendas
cat("2. Convertendo dados_emendas_atual...\n")
eme <- readRDS(file.path(dir_origem, "dados_emendas_atual.rds"))
if (inherits(eme, "sf")) eme <- sf::st_drop_geometry(eme)
if ("ibge6" %in% names(eme)) eme$ibge6 <- as.character(eme$ibge6)
if ("ibge7" %in% names(eme)) eme$ibge7 <- as.character(eme$ibge7)
arrow::write_parquet(eme, file.path(dir_dest, "dados_emendas.parquet"))
cat("   -> dados_emendas.parquet salvo com", nrow(eme), "linhas.\n")

# 3. PCP (Chapéu de Palha)
cat("3. Convertendo df_pcp...\n")
pcp <- readRDS(file.path(dir_origem, "df_pcp.rds"))
if (inherits(pcp, "sf")) pcp <- sf::st_drop_geometry(pcp)
if ("ibge6" %in% names(pcp)) pcp$ibge6 <- as.character(pcp$ibge6)
if ("ibge7" %in% names(pcp)) pcp$ibge7 <- as.character(pcp$ibge7)
if ("cod_ibge" %in% names(pcp)) pcp$cod_ibge <- as.character(pcp$cod_ibge)
arrow::write_parquet(pcp, file.path(dir_dest, "df_pcp.parquet"))
cat("   -> df_pcp.parquet salvo com", nrow(pcp), "linhas.\n")

# 4. Dados Fiscais com calculo de rank_pe
cat("4. Convertendo base_painel_mun com rank_pe...\n")
fisc <- readRDS(file.path(dir_origem, "base_painel_mun.rds"))
if (is.list(fisc) && "dados" %in% names(fisc)) fisc <- fisc$dados
if (inherits(fisc, "sf")) fisc <- sf::st_drop_geometry(fisc)

fisc <- fisc %>%
  group_by(grupo, indicador, exercicio) %>%
  mutate(
    rank_pe = ifelse(is.na(valor), NA_integer_, min_rank(desc(valor)))
  ) %>%
  ungroup()

if ("ibge6" %in% names(fisc)) fisc$ibge6 <- as.character(fisc$ibge6)
if ("cod_ibge" %in% names(fisc)) fisc$cod_ibge <- as.character(fisc$cod_ibge)
arrow::write_parquet(fisc, file.path(dir_dest, "dados_fiscais.parquet"))
cat("   -> dados_fiscais.parquet salvo com", nrow(fisc), "linhas.\n")

# 5. RAIS Vocações
cat("5. Convertendo df_rais_vocacoes...\n")
voc <- readRDS(file.path(dir_origem, "df_rais_vocacoes.rds"))
if (inherits(voc, "sf")) voc <- sf::st_drop_geometry(voc)
if ("id_municipio" %in% names(voc)) voc$id_municipio <- as.character(voc$id_municipio)
arrow::write_parquet(voc, file.path(dir_dest, "df_rais_vocacoes.parquet"))
cat("   -> df_rais_vocacoes.parquet salvo com", nrow(voc), "linhas.\n")

cat("ETL concluido com sucesso!\n")

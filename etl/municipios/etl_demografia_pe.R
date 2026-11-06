# ==============================================================================
# SCRIPT: etl_demografia_pe.R
# Objetivo: Enriquecimento demográfico e territorial dos municípios de PE,
#           conversão em formato plano com geojson_feature, exportação para o
#           Aurora (XLSX, RDS, Parquet, GeoJSON) e sincronização com Google Drive.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(dplyr)
library(googledrive)
library(googlesheets4)
library(ibger)
library(janitor)
library(readxl)
library(rmapshaper)
library(sf)
library(cli)
library(jsonlite)
library(geojsonsf)
library(arrow)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

# Busca hierárquica pelo centralizador paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))                DIR_ETL                <- getwd()
if (!exists("DIR_RAW_AURORA"))         DIR_RAW_AURORA         <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA"))         DIR_OUT_AURORA         <- file.path(DIR_ETL, "data")
if (!exists("DIR_DADOS_AURORA_MUNI"))  DIR_DADOS_AURORA_MUNI  <- DIR_OUT_AURORA
if (!exists("DIR_DADOS_AURORA"))       DIR_DADOS_AURORA       <- DIR_OUT_AURORA

# Autenticação Google via DIR_RAW_AURORA
json_auth <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(json_auth)) {
  drive_auth(path = json_auth)
  gs4_auth(path = json_auth)
  cli_alert_success("Autenticação Google realizada com sucesso via {.path {json_auth}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {json_auth}}")
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO E TRATAMENTO
# ------------------------------------------------------------------------------
## 3.1 População (IBGE API)
dados_populacao_mun <- ibge_variables(6579, localities = "N6")

dados_populacao_mun_pe <- dados_populacao_mun %>% 
  filter(substr(as.character(locality_id), 1, 2) == "26")

## 3.2 Área Territorial (IBGE Local via DIR_RAW_AURORA)
caminho_area <- file.path(DIR_RAW_AURORA, "ibge/AR_BR_RG_UF_RGINT_RGI_MUN_2025.xls")

if (!file.exists(caminho_area)) {
  cli_abort("Arquivo de áreas territoriais não encontrado: {.path {caminho_area}}")
}

dados_area_pe <- read_excel(caminho_area) %>%
  clean_names() %>%
  filter(cd_uf == "26")

## 3.3 Malha Geométrica (Shapefile RDS Local via DIR_RAW_AURORA)
caminho_sf <- file.path(DIR_RAW_AURORA, "pernambuco_sf.rds")

if (!file.exists(caminho_sf)) {
  cli_abort("Arquivo de limites geográficos não encontrado: {.path {caminho_sf}}")
}

dados_geo_pe <- readr::read_rds(caminho_sf) %>%
  select(-População) # Remove a população antiga para atualizar com a nova

## 3.4 Arranjos e Cadeias Produtivas (Google Drive)
url_arranjos <- "https://docs.google.com/spreadsheets/d/1ryN0_2wGp2mUqBQeSuKkHiT7SuCSclFE/edit?gid=1616214224#gid=1616214224"
caminho_temp_arranjos <- tempfile(fileext = ".xlsx")

drive_download(
  file      = googledrive::as_id(url_arranjos),
  path      = caminho_temp_arranjos,
  overwrite = TRUE
)

df_arranjos <- readxl::read_excel(caminho_temp_arranjos) %>%
  select(
    `Região de Desenvolvimento`,
    arranjos_produtivos_locais = `Arranjos produtivos locais`,
    cadeias_produtivas         = `Cadeias produtivas`
  )

# ------------------------------------------------------------------------------
# 4. ENRIQUECIMENTO DA MALHA
# ------------------------------------------------------------------------------
## 4.1 Prepara a última população disponível
ultimo_ano <- max(dados_populacao_mun_pe$period, na.rm = TRUE)
nome_coluna_pop <- paste0("pop_", ultimo_ano)

df_pop_recente <- dados_populacao_mun_pe %>%
  filter(period == ultimo_ano) %>%
  mutate(valor_num = as.numeric(value)) %>% 
  select(ibge7 = locality_id, !!sym(nome_coluna_pop) := valor_num)

## 4.2 Prepara a área
df_area_simples <- dados_area_pe %>%
  select(ibge7 = cd_mun, area_km2 = ar_mun_2025)

## 4.3 Consolidação dos atributos na base geoespacial
df_demografia_pe <- dados_geo_pe %>%
  left_join(df_pop_recente, by = "ibge7") %>%
  left_join(df_area_simples, by = "ibge7") %>%
  left_join(df_arranjos, by = "Região de Desenvolvimento") %>%
  rename(
    municipio                     = `Município`,
    regiao_de_desenvolvimento     = `Região de Desenvolvimento`,
    regiao_de_saude               = `Região de Saúde`,
    gerencia_regional_de_educacao = `Gerência Regional de Educação`,
    regiao_fiscal                 = `Região Fiscal`
  )

cli_alert_success("Base demográfica consolidada ({nrow(df_demografia_pe)} municípios).")

# ------------------------------------------------------------------------------
# 5. ESTRUTURAÇÃO PLANA E LEVEZA PARA O FRONTEND
# ------------------------------------------------------------------------------
# Simplifica malha com preservação de topologia
df_demografia_pe_simples <- df_demografia_pe %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Gera GeoJSON individual de cada município como texto puro
geojson_features <- sapply(1:nrow(df_demografia_pe_simples), function(i) {
  gj_raw <- geojsonsf::sf_geojson(df_demografia_pe_simples[i, ])
  parsed <- jsonlite::fromJSON(gj_raw, simplifyVector = FALSE)
  as.character(jsonlite::toJSON(parsed$features[[1]], auto_unbox = TRUE))
})

# Converte em data frame puro sem geometria sf
df_demografia_pe_plano <- sf::st_drop_geometry(df_demografia_pe_simples)
df_demografia_pe_plano$geojson_feature <- geojson_features
df_demografia_pe_plano$cod_ibge        <- df_demografia_pe_plano$ibge6
class(df_demografia_pe_plano)          <- "data.frame"

# ------------------------------------------------------------------------------
# 6. EXPORTAÇÃO LOCAL UNIFICADA (DIR_OUT_AURORA & DIR_RAW_AURORA)
# ------------------------------------------------------------------------------
# Estrutura de pastas
dir_raw_ibge         <- file.path(DIR_RAW_AURORA, "ibge")
dir_parquet_out      <- file.path(DIR_OUT_AURORA, "parquet")
dir_parquet_raw_ibge <- file.path(dir_raw_ibge, "parquet")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_raw_ibge, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_raw_ibge, recursive = TRUE, showWarnings = FALSE)

## 6.1 Tabelas auxiliares em DIR_OUT_AURORA (Excel, RDS e Parquet)
writexl::write_xlsx(dados_populacao_mun_pe, file.path(DIR_OUT_AURORA, "dados_populacao_mun_pe.xlsx"))
writexl::write_xlsx(dados_area_pe,          file.path(DIR_OUT_AURORA, "dados_area_mun_pe.xlsx"))
readr::write_rds(dados_populacao_mun_pe,   file.path(DIR_OUT_AURORA, "dados_populacao_mun_pe.rds"))
readr::write_rds(dados_area_pe,             file.path(DIR_OUT_AURORA, "dados_area_mun_pe.rds"))
arrow::write_parquet(dados_populacao_mun_pe, file.path(dir_parquet_out, "dados_populacao_mun_pe.parquet"))
arrow::write_parquet(dados_area_pe,          file.path(dir_parquet_out, "dados_area_mun_pe.parquet"))

## Backup das tabelas auxiliares em DIR_RAW_AURORA/ibge
writexl::write_xlsx(dados_populacao_mun_pe, file.path(dir_raw_ibge, "dados_populacao_mun_pe.xlsx"))
writexl::write_xlsx(dados_area_pe,          file.path(dir_raw_ibge, "dados_area_mun_pe.xlsx"))
readr::write_rds(dados_populacao_mun_pe,   file.path(dir_raw_ibge, "dados_populacao_mun_pe.rds"))
readr::write_rds(dados_area_pe,             file.path(dir_raw_ibge, "dados_area_mun_pe.rds"))
arrow::write_parquet(dados_populacao_mun_pe, file.path(dir_parquet_raw_ibge, "dados_populacao_mun_pe.parquet"))
arrow::write_parquet(dados_area_pe,          file.path(dir_parquet_raw_ibge, "dados_area_mun_pe.parquet"))

## 6.2 Base Demográfica Principal (RDS, Parquet e GeoJSON)
caminho_rds_out     <- file.path(DIR_OUT_AURORA, "df_demografia_pe.rds")
caminho_parquet_out <- file.path(dir_parquet_out, "df_demografia_pe.parquet")
caminho_geo_out     <- file.path(DIR_OUT_AURORA, "df_demografia_pe.geojson")

readr::write_rds(df_demografia_pe_plano, caminho_rds_out)
arrow::write_parquet(df_demografia_pe_plano, caminho_parquet_out)
geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(caminho_geo_out)

# Cópia de redundância em DIR_RAW_AURORA/ibge
readr::write_rds(df_demografia_pe_plano, file.path(dir_raw_ibge, "df_demografia_pe.rds"))
arrow::write_parquet(df_demografia_pe_plano, file.path(dir_parquet_raw_ibge, "df_demografia_pe.parquet"))
geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(file.path(dir_raw_ibge, "df_demografia_pe.geojson"))

cli_alert_success("Bases locais exportadas com sucesso (Excel, RDS, Parquet e GeoJSON).")

# ------------------------------------------------------------------------------
# 7. EXPORTAÇÃO E SINCRONIZAÇÃO NO GOOGLE DRIVE
# ------------------------------------------------------------------------------
pasta_id <- as_id("1ucHI_N3q1vi3lMCe9G_N_rUnGdqqyvS9")

## 7.1 Planilhas de tabelas auxiliares
nome_planilha <- "Bases_IBGE_PE" 
arquivos_planilha <- drive_ls(path = pasta_id, pattern = paste0("^", nome_planilha, "$"))

if (nrow(arquivos_planilha) == 0) {
  arquivos_planilha <- drive_create(nome_planilha, path = pasta_id, type = "spreadsheet")
}

tryCatch({
  sheet_write(data = dados_populacao_mun_pe, ss = arquivos_planilha[1, ], sheet = "Populacao")
  sheet_write(data = dados_area_pe,          ss = arquivos_planilha[1, ], sheet = "Area_Territorial")
  sheet_write(data = df_arranjos,            ss = arquivos_planilha[1, ], sheet = "Arranjos_Cadeias")
  cli_alert_success("Abas atualizadas com sucesso na planilha {.file {nome_planilha}}!")
}, error = function(e) {
  cli_alert_danger("Erro ao gravar abas no Google Sheets: {.val {e$message}}")
})

## 7.2 Malha enriquecida (GeoJSON)
nome_geojson <- "df_demografia_pe.geojson"

if (file.exists(caminho_geo_out)) {
  arquivos_geo <- drive_ls(path = pasta_id, pattern = paste0("^", nome_geojson, "$"))
  
  tryCatch({
    if (nrow(arquivos_geo) == 0) {
      drive_upload(
        media = caminho_geo_out, 
        path  = pasta_id, 
        name  = nome_geojson
      )
      cli_alert_success("Arquivo {.file {nome_geojson}} criado no Google Drive!")
    } else {
      drive_update(
        file  = arquivos_geo[1, ], 
        media = caminho_geo_out
      )
      cli_alert_success("Arquivo {.file {nome_geojson}} atualizado no Google Drive!")
    }
  }, error = function(e) {
    cli_alert_danger("Erro na sincronização do arquivo GeoJSON: {.val {e$message}}")
  })
} else {
  cli_alert_danger("Arquivo local não encontrado para upload: {.path {caminho_geo_out}}")
}

cli_alert_success("Sincronização no Google Drive finalizada!")
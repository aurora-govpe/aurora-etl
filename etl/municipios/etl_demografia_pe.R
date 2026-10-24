# ==============================================================================
# SCRIPT: etl_demografia_pe.R
# ==============================================================================

# bibliotecas ----
library(dplyr)
library(googledrive)
library(googlesheets4)
library(ibger)
library(janitor)
library(readxl)
library(rmapshaper)
library(sf)

is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  # Configuração de encoding/locale para tratamento correto de acentuação no Linux
  Sys.setlocale(locale = "pt_BR.utf8")
}

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# 1. autenticação ----
json_auth <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(json_auth)) {
  drive_auth(path = json_auth)
  gs4_auth(path = json_auth)
}

DIR_DADOS_AURORA_MUNI <- DIR_OUT_AURORA
DIR_DADOS_AURORA      <- DIR_OUT_AURORA


# 2. extração e tratamento ----
## população (ibge) ----
dados_populacao_mun <- ibge_variables(6579, localities = "N6")

dados_populacao_mun_pe <- dados_populacao_mun %>% 
  filter(substr(as.character(locality_id), 1, 2) == "26")

## área territorial (ibge local) ----
caminho_area <- file.path(DIR_RAW_AURORA, "ibge/AR_BR_RG_UF_RGINT_RGI_MUN_2025.xls")

dados_area_pe <- read_excel(caminho_area) %>%
  clean_names() %>%
  filter(cd_uf == "26")

## malha geométrica (local) ----
caminho_sf <- file.path(DIR_RAW_AURORA, "pernambuco_sf.rds")

dados_geo_pe <- readr::read_rds(caminho_sf) %>%
  select(-População) # Remove a população antiga para atualizar com a nova

## arranjos e cadeias produtivas (drive) ----
url_arranjos <- "https://docs.google.com/spreadsheets/d/1ryN0_2wGp2mUqBQeSuKkHiT7SuCSclFE/edit?gid=1616214224#gid=1616214224"
caminho_temp_arranjos <- tempfile(fileext = ".xlsx")

drive_download(
  file = googledrive::as_id(url_arranjos),
  path = caminho_temp_arranjos,
  overwrite = TRUE
)

df_arranjos <- readxl::read_excel(caminho_temp_arranjos) %>%
  # Seleciona as colunas mantendo o nome exato da chave de join ("Região de Desenvolvimento")
  select(
    `Região de Desenvolvimento`,
    arranjos_produtivos_locais = `Arranjos produtivos locais`,
    cadeias_produtivas = `Cadeias produtivas`
  )


# 3. enriquecimento da malha ----
## prepara a última população disponível
ultimo_ano <- max(dados_populacao_mun_pe$period, na.rm = TRUE)
nome_coluna_pop <- paste0("pop_", ultimo_ano)

df_pop_recente <- dados_populacao_mun_pe %>%
  filter(period == ultimo_ano) %>%
  mutate(valor_num = as.numeric(value)) %>% 
  select(ibge7 = locality_id, !!sym(nome_coluna_pop) := valor_num)

## prepara a área
df_area_simples <- dados_area_pe %>%
  select(ibge7 = cd_mun, area_km2 = ar_mun_2025)

## junta tudo na base sf
df_demografia_pe <- dados_geo_pe %>%
  left_join(df_pop_recente, by = "ibge7") %>%
  left_join(df_area_simples, by = "ibge7") %>%
  left_join(df_arranjos, by = "Região de Desenvolvimento") %>%
  rename(
    municipio = `Município`,
    regiao_de_desenvolvimento = `Região de Desenvolvimento`,
    regiao_de_saude = `Região de Saúde`,
    gerencia_regional_de_educacao = `Gerência Regional de Educação`,
    regiao_fiscal = `Região Fiscal`
  )

df_demografia_pe %>% glimpse()
#df_demografia_pe %>% View()

# 4. exportação disco local ----
caminho_local_pop <- file.path("data", "ibge", "dados_populacao_mun_pe.xlsx")
caminho_rds_pop   <- file.path("data", "ibge", "dados_populacao_mun_pe.rds")

caminho_local_area <- file.path("data", "ibge", "dados_area_mun_pe.xlsx")
caminho_rds_area   <- file.path("data", "ibge", "dados_area_mun_pe.rds")

caminho_rds_demo <- file.path("data", "ibge", "df_demografia_pe.rds")

caminho_geo_demo <- file.path("data", "ibge", "df_demografia_pe.geojson")

caminho_rds_aurora_novo <- file.path(DIR_DADOS_AURORA, "df_demografia_pe.rds")

# salvando tabelas auxiliares
writexl::write_xlsx(dados_populacao_mun_pe, caminho_local_pop)
writexl::write_xlsx(dados_area_pe, caminho_local_area)
readr::write_rds(dados_populacao_mun_pe, caminho_rds_pop)
readr::write_rds(dados_area_pe, caminho_rds_area)

readr::write_rds(df_demografia_pe, caminho_rds_aurora_novo)


# 4. exportação aurora_municipios ----
caminho_local_pop_muni <- file.path(DIR_DADOS_AURORA_MUNI, "dados_populacao_mun_pe.xlsx")
caminho_rds_pop_muni   <- file.path(DIR_DADOS_AURORA_MUNI, "dados_populacao_mun_pe.rds")
caminho_local_area_muni <- file.path(DIR_DADOS_AURORA_MUNI, "dados_area_mun_pe.xlsx")
caminho_rds_area_muni   <- file.path(DIR_DADOS_AURORA_MUNI, "dados_area_mun_pe.rds")
caminho_rds_demo_muni <- file.path(DIR_DADOS_AURORA_MUNI, "df_demografia_pe.rds")
caminho_geo_demo_muni <- file.path(DIR_DADOS_AURORA_MUNI, "df_demografia_pe.geojson")


# salvando tabelas auxiliares
writexl::write_xlsx(dados_populacao_mun_pe, caminho_local_pop_muni)
writexl::write_xlsx(dados_area_pe, caminho_local_area_muni)
readr::write_rds(dados_populacao_mun_pe, caminho_rds_pop_muni)
readr::write_rds(dados_area_pe, caminho_rds_area_muni)

# salvando a malha enriquecida
readr::write_rds(df_demografia_pe, caminho_rds_demo)
geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(caminho_geo_demo)

df_demografia_pe_mun <- df_demografia_pe %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

geojson_features <- sapply(1:nrow(df_demografia_pe_mun), function(i) {
  gj_raw <- geojsonsf::sf_geojson(df_demografia_pe_mun[i, ])
  parsed <- jsonlite::fromJSON(gj_raw, simplifyVector = FALSE)
  # Converte apenas o objeto Feature interno para texto JSON
  as.character(jsonlite::toJSON(parsed$features[[1]], auto_unbox = TRUE))
})

df_demografia_pe_mun <- sf::st_drop_geometry(df_demografia_pe_mun)

df_demografia_pe_mun$geojson_feature <- geojson_features

df_demografia_pe_mun %>%
  mutate(
    cod_ibge = ibge6
  ) %>%
  readr::write_rds(caminho_rds_demo_muni)

geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(caminho_geo_demo_muni)

# 5. exportação google drive ----
pasta_id <- as_id("1ucHI_N3q1vi3lMCe9G_N_rUnGdqqyvS9")

## planilhas de tabelas auxiliares
nome_planilha <- "Bases_IBGE_PE" 
arquivos_planilha <- drive_ls(path = pasta_id, pattern = paste0("^", nome_planilha, "$"))

if (nrow(arquivos_planilha) == 0) {
  arquivos_planilha <- drive_create(nome_planilha, path = pasta_id, type = "spreadsheet")
}

sheet_write(data = dados_populacao_mun_pe, ss = arquivos_planilha[1, ], sheet = "Populacao")
sheet_write(data = dados_area_pe, ss = arquivos_planilha[1, ], sheet = "Area_Territorial")
sheet_write(data = df_arranjos, ss = arquivos_planilha[1, ], sheet = "Arranjos_Cadeias")

## malha enriquecida (geojson)
nome_geojson <- "df_demografia_pe.geojson"
arquivos_geo <- drive_ls(path = pasta_id, pattern = paste0("^", nome_geojson, "$"))

if (nrow(arquivos_geo) == 0) {
  drive_upload(
    media = caminho_geo_demo, 
    path = pasta_id, 
    name = nome_geojson
  )
} else {
  drive_update(
    file = arquivos_geo[1, ], 
    media = caminho_geo_demo
  )
}




## TESTE PARA TIRAR O SF

# 4. exportação unificada ----
# Salvando tabelas auxiliares no destino central
writexl::write_xlsx(dados_populacao_mun_pe, file.path(DIR_OUT_AURORA, "dados_populacao_mun_pe.xlsx"))
writexl::write_xlsx(dados_area_pe, file.path(DIR_OUT_AURORA, "dados_area_mun_pe.xlsx"))
readr::write_rds(dados_populacao_mun_pe, file.path(DIR_OUT_AURORA, "dados_populacao_mun_pe.rds"))
readr::write_rds(dados_area_pe, file.path(DIR_OUT_AURORA, "dados_area_mun_pe.rds"))

# Cópia em raw/ibge se existir
dir_raw_ibge <- file.path(DIR_RAW_AURORA, "ibge")
if (dir.exists(dir_raw_ibge)) {
  writexl::write_xlsx(dados_populacao_mun_pe, file.path(dir_raw_ibge, "dados_populacao_mun_pe.xlsx"))
  writexl::write_xlsx(dados_area_pe, file.path(dir_raw_ibge, "dados_area_mun_pe.xlsx"))
  readr::write_rds(dados_populacao_mun_pe, file.path(dir_raw_ibge, "dados_populacao_mun_pe.rds"))
  readr::write_rds(dados_area_pe, file.path(dir_raw_ibge, "dados_area_mun_pe.rds"))
}

# ------------------------------------------------------------------------------
# CONVERSÃO PARA TABELA PLANA (Sem classe 'sf' e sem colunas de geometria)
# ------------------------------------------------------------------------------
# Simplifica malha para leveza no frontend
df_demografia_pe_simples <- df_demografia_pe %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Gera GeoJSON individual de cada município como texto simples (string)
geojson_features <- sapply(1:nrow(df_demografia_pe_simples), function(i) {
  gj_raw <- geojsonsf::sf_geojson(df_demografia_pe_simples[i, ])
  parsed <- jsonlite::fromJSON(gj_raw, simplifyVector = FALSE)
  as.character(jsonlite::toJSON(parsed$features[[1]], auto_unbox = TRUE))
})

# Exporta GeoJSON completo do estado para o destino unificado
geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(file.path(DIR_OUT_AURORA, "df_demografia_pe.geojson"))
if (dir.exists(dir_raw_ibge)) {
  geojsonsf::sf_geojson(df_demografia_pe) %>% writeLines(file.path(dir_raw_ibge, "df_demografia_pe.geojson"))
}

# Remove geometria e transforma o data frame em tabela nativa simples
df_demografia_pe_plano <- sf::st_drop_geometry(df_demografia_pe_simples)
df_demografia_pe_plano$geojson_feature <- geojson_features
df_demografia_pe_plano$cod_ibge         <- df_demografia_pe_plano$ibge6
class(df_demografia_pe_plano)          <- "data.frame"

# Salva arquivo RDS unificado
readr::write_rds(df_demografia_pe_plano, file.path(DIR_OUT_AURORA, "df_demografia_pe.rds"))
if (dir.exists(dir_raw_ibge)) {
  readr::write_rds(df_demografia_pe_plano, file.path(dir_raw_ibge, "df_demografia_pe.rds"))
}



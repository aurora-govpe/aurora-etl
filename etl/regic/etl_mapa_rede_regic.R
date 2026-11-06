# ==============================================================================
# ETL - GERAÇÃO DO MAPA E MALHA ESPACIAL DA REDE REGIC PE EXPANDIDA
# Abrange os 185 municípios de Pernambuco + 214 municípios/arranjos externos
# ==============================================================================

library(dplyr)
library(readxl)
library(janitor)
library(arrow)
library(sf)

# 1. Configuração de Caminhos --------------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

dir_out <- DIR_OUT_AURORA
dir_raw <- file.path(DIR_RAW_AURORA, "ibge/regic")

dir.create(file.path(dir_out, "geojson"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dir_out, "parquet"), recursive = TRUE, showWarnings = FALSE)

# 2. Identificar todos os nós da Rede REGIC conectados a PE -------------------
path_ligacoes <- file.path(dir_raw, "REGIC2018_Ligacoes_entre_Cidades.xlsx")
if (!file.exists(path_ligacoes)) {
  stop("Arquivo de ligacoes REGIC nao encontrado em: ", path_ligacoes)
}

message("Lendo conexoes REGIC...")
df_arestas_brutas <- read_excel(path_ligacoes) %>% clean_names()

df_pe <- df_arestas_brutas %>%
  filter(coduf_ori == 26 | coduf_dest == 26)

cods_nos_rede <- unique(as.character(c(df_pe$cod_ori, df_pe$cod_dest)))
message(sprintf("Nós únicos na rede conectada a PE: %d", length(cods_nos_rede)))

# 3. Carregar Malha Municipal Simplificada do Brasil ---------------------------
path_muni_br <- file.path(dir_out, "municipalities_2022_simplified.parquet")
if (!file.exists(path_muni_br)) {
  stop("Arquivo de malha simplificada nao encontrado: ", path_muni_br)
}

message("Carregando malha municipal simplificada...")
muni_df <- read_parquet(path_muni_br)
muni_sf <- st_as_sf(muni_df, crs = 4674)

# Filtrar: ou pertence à rede completa REGIC de PE ou é município de PE
rede_sf <- muni_sf %>%
  mutate(
    cod_ibge = as.character(code_muni),
    ibge6    = substr(cod_ibge, 1, 6),
    uf       = as.character(abbrev_state),
    nome_mun = as.character(name_muni)
  ) %>%
  filter(cod_ibge %in% cods_nos_rede | uf == "PE") %>%
  mutate(
    pertence_pe = (uf == "PE"),
    tipo_no     = if_else(pertence_pe, "Pernambuco", "Conexão Externa")
  )

message(sprintf("Total de municípios na malha expandida: %d (PE: %d | Outros Estados: %d)",
                nrow(rede_sf),
                sum(rede_sf$pertence_pe),
                sum(!rede_sf$pertence_pe)))

# 4. Calcular Centroides (Lon/Lat) para cada Município -------------------------
coords_centroides <- suppressWarnings(st_coordinates(st_centroid(rede_sf)))
rede_sf$longitude <- round(coords_centroides[, 1], 6)
rede_sf$latitude  <- round(coords_centroides[, 2], 6)

# Organizar colunas
rede_sf <- rede_sf %>%
  select(
    cod_ibge,
    ibge6,
    nome_mun,
    uf,
    name_state,
    name_region,
    pertence_pe,
    tipo_no,
    longitude,
    latitude,
    geometry
  ) %>%
  arrange(desc(pertence_pe), uf, nome_mun)

# 5. Exportar Arquivos de Malha Espacial ---------------------------------------
path_geojson <- file.path(dir_out, "geojson/rede_regic_municipios.geojson")
path_parquet <- file.path(dir_out, "parquet/rede_regic_municipios.parquet")
path_rds     <- file.path(dir_out, "rede_regic_municipios.rds")

message("Gravando GeoJSON em: ", path_geojson)
st_write(rede_sf, path_geojson, delete_dsn = TRUE, quiet = TRUE)

message("Gravando Parquet tabular (com lon/lat) em: ", path_parquet)
df_rede_tab <- st_drop_geometry(rede_sf)
write_parquet(df_rede_tab, path_parquet, compression = "snappy")

message("Gravando RDS espacial (objeto sf com geometrias) em: ", path_rds)
saveRDS(rede_sf, path_rds)

# 6. Atualizar Tabela de Fluxos com Coordenadas de Origem e Destino ------------
path_fluxos <- file.path(dir_out, "parquet/regic_pe_fluxos.parquet")
if (file.exists(path_fluxos)) {
  message("Enriquecendo tabela de fluxos com coordenadas...")
  df_fluxos <- read_parquet(path_fluxos)
  
  df_coords <- rede_sf %>%
    st_drop_geometry() %>%
    select(cod_ibge, uf, lon = longitude, lat = latitude)
  
  df_fluxos_geo <- df_fluxos %>%
    left_join(
      df_coords %>% rename(origem_uf = uf, origem_lon = lon, origem_lat = lat),
      by = c("origem" = "cod_ibge")
    ) %>%
    left_join(
      df_coords %>% rename(destino_uf = uf, destino_lon = lon, destino_lat = lat),
      by = c("destino" = "cod_ibge")
    ) %>%
    select(
      origem, origem_nome, origem_uf, origem_lon, origem_lat,
      destino, destino_nome, destino_uf, destino_lon, destino_lat,
      tipo_fluxo,
      peso_geral,
      starts_with("peso_")
    )
  
  temp_pqt <- file.path(dir_out, "parquet/regic_pe_fluxos_temp.parquet")
  write_parquet(df_fluxos_geo, temp_pqt, compression = "snappy")
  rm(df_fluxos)
  invisible(gc())
  file.copy(temp_pqt, path_fluxos, overwrite = TRUE)
  unlink(temp_pqt)
  message("Tabela de fluxos regic_pe_fluxos.parquet enriquecida com coordenadas de origem e destino!")
}

message(">>> ETL da malha geográfica da rede REGIC concluído com sucesso!")

# bibliotecas ----
library(tidyverse)
library(readxl)
library(fs)
library(googledrive)
library(glue)
library(sf)
library(vialactea)
library(bslib)
library(BigDataPE)
library(leaflet)

# setup ----
source('scripts/utils.R')

### SEDUH SEHAB ----
drive_auth(email = "hugo.vasconcelos@gd.seplag.pe.gov.br")

drive_user()

url_seduh_sehab <- "https://docs.google.com/spreadsheets/d/190eQuQx5qe7Ofjl6BzQqnRg4zxm_rLB5/edit?gid=1847341728#gid=1847341728"

arquivo_seduh_sehab <- drive_download(
  as_id(url_seduh_sehab),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

dados_seduh_sehab <- arquivo_seduh_sehab$local_path |>
  excel_sheets() |>
  set_names() |>
  map(\(sheet_name) read_excel(arquivo_seduh_sehab$local_path, sheet = sheet_name, col_types = "text"))

seduh_sehab_df_poligono <- dados_seduh_sehab$`Meta com Vários Produtos` %>%
  janitor::clean_names() %>% 
  select(!starts_with("x")) %>%
  filter(
    iniciativa_estrategica %in% c('REFORMA NO LAR I')
  ) %>%
  mutate(
    secretaria_unidade = 'SEDUH - SEHAB',
    
    secretaria = str_remove(secretaria, " - .*")
  )

### shapes ---
library(sf)
library(fs)
library(purrr) # Para manipulação de listas

caminho_shp <- "data/mapbox/seduh"

CRS_DESTINO <- 4326

diretorios_lotes <- fs::dir_ls(caminho_shp, type = "directory")

lotes_sf_list <- purrr::map_dfr(diretorios_lotes, function(dir_path) {
  
  shp_path <- fs::dir_ls(dir_path, glob = "*.shp")
  
  if (length(shp_path) == 0) {
    message(paste("Nenhum arquivo .shp encontrado em:", dir_path))
    return(NULL)
  }

  dados_shp <- st_read(shp_path[1], quiet = TRUE)
  
  if (st_crs(dados_shp)$epsg != CRS_DESTINO) {
    # 
    dados_shp <- st_transform(dados_shp, CRS_DESTINO)
    message(paste("CRS do lote", basename(dir_path), "transformado para EPSG:", CRS_DESTINO))
  }
  
  nome_lote <- basename(dir_path)
  
  dados_shp$ID_LOTE_PASTA <- nome_lote
  
  return(dados_shp)
})

seduh_sehab_shp_poligono <- lotes_sf_list %>%
  # 1. Substitui "RNL I" por "REFORMA NO LAR I" na coluna temporária
  mutate(
    temp_col = str_replace(ID_LOTE_PASTA, "RNL I", "REFORMA NO LAR I")
  ) %>%
  
  # 2. Separa a coluna temporária em duas novas colunas (usando o separador " - ")
  separate(
    col = temp_col, 
    into = c("iniciativa_estrategica", "produto"), 
    sep = " - ", 
    extra = "merge", # Para garantir que se houver mais de um '-', só o primeiro é usado como separador
    remove = TRUE  # Remove a coluna temporária
  ) %>%
  
  # 3. Limpeza final: remove espaços em branco extras (whitespaces)
  mutate(
    iniciativa_estrategica = str_trim(iniciativa_estrategica),
    produto = str_trim(produto)
  ) %>%
  
  select(iniciativa_estrategica, produto, everything())

# Inspecione o resultado
seduh_sehab_shp_poligono %>% glimpse()

seduh_sehab_shp_poligono_2 <- seduh_sehab_shp_poligono %>%
  
  filter(produto == "LOTE 3") %>%
  
  mutate(
    geometry = st_make_valid(geometry),
    geometry = st_buffer(geometry, dist = 0)
  ) %>%
  
  group_by(iniciativa_estrategica, produto) %>% 
  
  summarise(
    
    geometria_agregada = st_union(geometry),
    
    nomes_areas_concatenados = paste(sort(unique(NM_AREA)), collapse = "; "),
    
    ID_LOTE_PASTA_ORIGINAL = first(ID_LOTE_PASTA),
    
    .groups = 'drop'
  ) %>%
  
  st_set_geometry("geometria_agregada") %>%
  rename(geom = geometria_agregada) %>%
  ungroup()

#### teste
library(mapgl) # Pacote MapLibre / MapGL
library(sf)
library(dplyr)

seduh_sehab_shp_poligono_2 <- seduh_sehab_shp_poligono %>%
  # filter(
  #   LOTE != 3
  # ) %>%
  mutate(
    # 
    geometry = st_make_valid(geometry),
    geometry = st_buffer(geometry, dist = 0)
  ) %>%
  
  st_transform(4326) %>%
  mutate(
    geometry = st_cast(geometry, "MULTIPOLYGON")
  )

centro_mapa <- st_bbox(seduh_sehab_shp_poligono_2) %>%
  st_as_sfc() %>%
  st_centroid() %>%
  st_coordinates()

centro_mapa

mapgl::maplibre() %>%
  fit_bounds(seduh_sehab_shp_poligono_2) %>%
  add_fill_layer(
    id = "lotes_restantes",
    source = seduh_sehab_shp_poligono_2, 
    fill_color = 'pink',
    fill_opacity = 0.6
  ) %>%
  add_line_layer(
    id = "lotes_borda",
    source = seduh_sehab_shp_poligono_2,
    line_color = "red",
    line_width = 2,
    line_opacity = 1
  )

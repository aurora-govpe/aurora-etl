# pacotes ----
library(dplyr)
library(googledrive)
library(mapgl)
library(sf)
library(stringr)

# setup ----
FOLDER_ID <- "1sYxZ4HRCjB8QZtvbjpJ1cymRh7WPyDMt"
AUTH_PATH <- "data/r-sheets-hugo-0788cc358cb5.json"
LOCAL_PATH <- "data/periferias"

# auth ----
drive_auth(path = AUTH_PATH)

# Peixinhos, roda de fogo, bode e jardim monte verde
# fcu polígonos ----
mapa_FCUs_poligonos <- read_sf("data/periferias/fcu_poligonos/qg_2022_670_fcu_agreg.shp") %>%
  st_zm(drop = TRUE) %>%          # REMOVE as dimensões Z e M (Crucial para MultiPolygon)
  st_transform(4326) %>%          # Garante WGS84
  st_make_valid() %>%
  mutate(
    popup_info = paste0("<b>Código:</b> ", cd_fcu, "<br><b>Nome:</b> ", nm_fcu)
  )

mapa_FCUs_nao_setorizadas <- read_sf("data/periferias/fcu_nao_setorizadas/geopackages_n_setorizadas.shp") %>%
  st_zm(drop = TRUE) %>%          # REMOVE as dimensões Z e M (Crucial para MultiPolygon)
  st_transform(4326) %>%          # Garante WGS84
  st_make_valid()

# reforma no lar ----
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

mapa_reforma_no_lar <- lotes_sf_list %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  st_transform(4326) %>%
  relocate(
    NM_AREA, .before = 'LOTE'
  )

lista_cod_fcu <- c(
  "26079010121", 
  "26079010145", 
  "26116060685",
  '26116060844',
  '26116060764',
  '26079010144',
  '26079010132',
  '26116060761',
  '26116060533',
  '26116060737',
  '26116060811',
  '26116060775',
  '26116060627',
  '26096000123',
  '26096000089',
  '26096000090',
  '26116060716', # sem políticas
  '26116060882' # sem políticas
  )

mapa_FCUs_poligonos_2 <- mapa_FCUs_poligonos %>%
  filter(
    sigla_uf == 'PE'
    , cd_fcu %in% lista_cod_fcu
    )

# visualização ----
# 3. Gerar o mapa
maplibre(
  center = c(-34.88, -8.05),
  zoom   = 11
) %>%
  # Adicionamos a fonte de dados primeiro
  add_source(
    id = "fcu_data",
    data = mapa_FCUs_poligonos_2
  ) %>%
  # Referenciamos a fonte pelo ID no preenchimento
  add_fill_layer(
    id = "fcu_layer",
    source = "fcu_data",
    fill_color = "blue",
    fill_opacity = 0.6,
    popup = "popup_info"
  ) %>%
  add_source(
    id = "reforma_no_lar_data",
    data = mapa_reforma_no_lar
  ) %>%
  add_fill_layer(
    id = "reforma_no_lar",
    source = "reforma_no_lar_data",
    fill_color = "orange",
    fill_opacity = 0.6,
    popup = "NM_AREA"
  )

# censo ----
library(geobr)

# 1. Baixar setores censitários de PE (Censo 2022)
mapa_setores_2022_pe <- read_census_tract(
  code_tract = "PE", year = 2022) %>%
  st_transform(4326) %>%
  st_make_valid()

library(censobr)

#data_dictionary(year = 2022, dataset = "tracts")

dados_setores_2022_basico <- read_tracts(
  year = 2022, 
  dataset = "Basico", 
  as_data_frame = TRUE
)

dados_setores_2022_basico_pe <- dados_setores_2022_basico %>%
  filter(code_state == 26) %>%
  select(
    code_tract, 
    code_favela, 
    name_favela,
    #V0001
    pop_total            = V0001,
    dom_total            = V0002,
    dom_part_total       = V0003,
    dom_col_total        = V0004,
    medi_pop_dom_part_oc = V0005, #V0001/V0007
    perc_dom_part_oc_imp = V0006,
    dom_part_oc          = V0007
    ) %>% 
  collect()

dados_setores_2022_basico_pe_2 <- mapa_setores_2022_pe %>%
  mutate(
    code_tract = as.character(code_tract)
  ) %>%
  left_join(dados_setores_2022_basico_pe, by = "code_tract") %>%
  mutate(area_total_setor = st_area(.))

estimar_pop <- function(shape_alvo, setores_base) {
  st_intersection(shape_alvo, setores_base) %>%
    mutate(
      area_fragmento = st_area(.),
      proporcao = as.numeric(area_fragmento / area_total_setor),
      pop_total_estimada   = pop_total * proporcao,
      dom_part_oc_estimado = dom_part_oc * proporcao
    ) %>%
    group_by(
      across(1) # pegar 1ª coluna
      ) %>%
    summarise(
      pop_total_estimada = sum(pop_total_estimada, na.rm = TRUE),
      dom_part_oc_estimado = sum(dom_part_oc_estimado, na.rm = TRUE) 
      ) %>%
    mutate(
      perc_dom_part_oc_imp_estimado = round(pop_total_estimada/dom_part_oc_estimado, 2)
    )
}

# EXECUTAR PARA OS SEUS DOIS MAPAS
pop_fcu_ibge <- estimar_pop(
  mapa_FCUs_poligonos_2, 
  dados_setores_2022_basico_pe_2)

pop_reforma_lar <- estimar_pop(
  mapa_reforma_no_lar, 
  dados_setores_2022_basico_pe_2)

pop_fcu_ibge_2 <- pop_fcu_ibge %>%
  left_join(
    mapa_FCUs_poligonos_2 %>%
      st_drop_geometry() %>%
      select(
        cd_fcu, nm_fcu
      )
  ) %>%
  mutate(
    pop_total_estimada = ceiling(pop_total_estimada),
    dom_part_oc_estimado = ceiling(dom_part_oc_estimado),
    popup_info = paste0(
      "<b>Código </b> ", cd_fcu, "<br>",
      "<b>Nome </b> ", nm_fcu, "<br>", 
      "<b>População </b> ", pop_total_estimada, "<br>",
      "<b>Domicílios </b> ", dom_part_oc_estimado, "<br>",
      "<b>% </b> ", perc_dom_part_oc_imp_estimado, "<br>"
      )
  )

pop_reforma_lar_2 <- pop_reforma_lar %>%
  mutate(
    pop_total_estimada = ceiling(pop_total_estimada),
    dom_part_oc_estimado = ceiling(dom_part_oc_estimado),
    popup_info = paste0(
      "<b>Nome </b> ", NM_AREA, "<br>", 
      "<b>População </b> ", pop_total_estimada, "<br>",
      "<b>Domicílios </b> ", dom_part_oc_estimado, "<br>",
      "<b>% </b> ", perc_dom_part_oc_imp_estimado, "<br>"
    )
  )

maplibre(
  center = c(-34.88, -8.05),
  zoom   = 11
) %>%
  # Adicionamos a fonte de dados primeiro
  add_source(
    id = "fcu_data",
    data = pop_fcu_ibge_2
  ) %>%
  # Referenciamos a fonte pelo ID no preenchimento
  add_fill_layer(
    id = "FCU IBGE",
    source = "fcu_data",
    fill_color = "blue",
    fill_outline_color = 'white',
    fill_opacity = 0.6,
    popup = "popup_info"
  ) %>%
  add_source(
    id = "reforma_no_lar_data",
    data = pop_reforma_lar_2
  ) %>%
  add_fill_layer(
    id = "Reforma no Lar",
    source = "reforma_no_lar_data",
    fill_color = "orange",
    fill_opacity = 0.6,
    fill_outline_color = 'black',
    popup = "popup_info"
  ) %>%
  add_layers_control(
    position = "bottom-left",
    layers = c(
      "FCU IBGE", 
      "Reforma no Lar"),
    collapsible = FALSE
  )

# envio ----
## população fcu ibge ----
pop_fcu_ibge_2 %>% 
  saveRDS(
    'data/periferias/pop_fcu_ibge.rds'
  )

drive_upload(
  "data/periferias/pop_fcu_ibge.rds",
  path = as_id(FOLDER_ID),
  overwrite = TRUE
)

## população reforma no lar seduh-pe ----
pop_reforma_lar_2 %>% 
  saveRDS(
    'data/periferias/pop_reforma_lar.rds'
  )

drive_upload(
  "data/periferias/pop_reforma_lar.rds",
  path = as_id(FOLDER_ID),
  overwrite = TRUE
)

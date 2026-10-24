# bibliotecas ----
library(dplyr)
library(glue)
library(httr2)
library(jsonlite)
library(mapgl)
library(purrr)
library(readr)
library(rlang)
library(sf)
library(stringr)
library(tibble)

# setup ----
## tokens ----
mapbox_key <- 'pk.eyJ1IjoibWlsa3dheSIsImEiOiJjaW1naDljNmswMXpvdGlsdjA5d3BnZ3E5In0.SGw2jcn16RRNffusFgWzWg'
mapbox_style <- 'mapbox://styles/milkway/cmb3pmayy00p901qy796r3ejd'

## utils ----
# helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Função que resolve o style p/ JSON (list)
fetch_style_json <- function(style, access_token) {
  if (is.list(style)) return(style)                     # já é JSON
  if (is.character(style) && startsWith(style, "mapbox://styles/")) {
    # mapbox://styles/user/styleid  ->  https://api.mapbox.com/styles/v1/user/styleid
    api_url <- sub("^mapbox://styles/", "https://api.mapbox.com/styles/v1/", style)
    req <- request(api_url) |> req_url_query(access_token = access_token)
    resp <- req_perform(req)
    return(resp_body_json(resp, simplifyVector = FALSE))
  }
  if (is.character(style) && grepl("^https?://", style)) {
    # style hospedado em URL HTTP(S). Se precisa token, anexamos.
    req <- request(style)
    if (!grepl("access_token=", style, fixed = TRUE))
      req <- req |> req_url_query(access_token = access_token)
    resp <- req_perform(req)
    return(resp_body_json(resp, simplifyVector = FALSE))
  }
  stop("Não consegui resolver o style a partir do objeto do mapa.")
}

to_wgs84 <- function(x) {
  if (is.na(st_crs(x))) st_set_crs(x, 4326) else st_transform(x, 4326)
}

# etl ----
map <- mapboxgl(
  style = mapbox_style,
  preserveDrawingBuffer = TRUE, # zoom = 7, center = centro,
  access_token = mapbox_key
)

map %>% class()
map %>% glimpse()
map %>% typeof()

str(map$x, max.level = 2)

style_json <- fetch_style_json(mapbox_style, access_token = mapbox_key)

layers_tbl <- map(style_json$layers, function(ly) {
  tibble(
    id           = ly$id %||% NA_character_,
    type         = ly$type %||% NA_character_,
    source       = ly$source %||% NA_character_,
    source_layer = ly[["source-layer"]] %||% NA_character_,
    minzoom      = ly$minzoom %||% NA_real_,
    maxzoom      = ly$maxzoom %||% NA_real_,
    layout_has   = if (!is.null(ly$layout)) paste(names(ly$layout), collapse = ",") else NA_character_,
    paint_has    = if (!is.null(ly$paint))  paste(names(ly$paint),  collapse = ",") else NA_character_
  )
}) |> list_rbind()

layers_tbl |> print(n = 30)

# salvar ----
library(htmlwidgets)
saveWidget(map, "map_inspect.html", selfcontained = TRUE) 

# recriar mapa ----

pe_limits <- read_rds("data/pe_limits.rds") %>%
  st_as_sf() %>%
  st_transform(4326)

obras_geo    <- st_read("data/mapbox/geometria_obras.geojson", quiet = TRUE)
projetos_geo <- st_read("data/mapbox/geometria_projetos.geojson", quiet = TRUE)
inic_geo     <- st_read("data/mapbox/iniciativas.geojson", quiet = TRUE)

obras_geo    <- to_wgs84(obras_geo)    |> st_make_valid()
projetos_geo <- to_wgs84(projetos_geo) |> st_make_valid()
inic_geo     <- to_wgs84(inic_geo)     |> st_make_valid()

obras_geo    <- obras_geo[st_geometry_type(obras_geo) %in% c("LINESTRING","MULTILINESTRING"), ]
projetos_geo <- projetos_geo[st_geometry_type(projetos_geo) %in% c("LINESTRING","MULTILINESTRING"), ]
inic_geo     <- inic_geo[st_geometry_type(inic_geo) %in% c("POINT","MULTIPOINT"), ]

m <- maplibre(style = carto_style("voyager")) |>
  mapgl::fit_bounds(pe_limits) %>%
  add_navigation_control(
    show_compass = TRUE,
    show_zoom = TRUE,
    visualize_pitch = TRUE,
    position = "top-right",
    orientation = "vertical"
  ) |>
  add_line_layer(
    id         = "geometria-obras",
    source     = obras_geo,
    line_color = list("get", "palheta_cor"),
    line_width = 2
  ) |>
  # PROJETOS: multilinhas, cor fixa
  add_line_layer(
    id         = "geometria-projetos",
    source     = projetos_geo,
    line_color = "#00695C",
    line_width = 2,
    line_opacity = 0.9,
    min_zoom   = 5
  ) |>
  # INICIATIVAS: pontos, cor vinda de 'avanco_cor'
  add_circle_layer(
    id                  = "iniciativas",
    source              = inic_geo,
    circle_radius       = 4,
    circle_color        = get_column("avanco_cor"),
    circle_stroke_color = "#222",
    circle_stroke_width = 0.8,
    min_zoom            = 5,
    tooltip = "label",
    popup   = "label"
  )

m

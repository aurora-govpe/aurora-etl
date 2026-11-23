# ==============================================================================
# ETL ADUTORAS DE PERNAMBUCO
# ==============================================================================

library(sf)
library(dplyr)

# 1. Resolução centralizada de caminhos ----
caminhos_paths <- c("R/paths.R", "paths.R", "../paths.R", "etl/paths.R", "../../R/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"

# 2. Localização do arquivo de entrada ----
candidatos_raw <- c(
  file.path(DIR_RAW_AURORA, "compesa", "Estágio_do_Sistema_de_Adutoras_d_Água.geojson"),
  file.path(DIR_RAW_AURORA, "Estágio_do_Sistema_de_Adutoras_d_Água.geojson"),
  "data/compesa/Estágio_do_Sistema_de_Adutoras_d_Água.geojson"
)

src_geo <- NULL
for (c_path in candidatos_raw) {
  if (file.exists(c_path)) {
    src_geo <- c_path
    break
  }
}

if (is.null(src_geo)) {
  stop("Arquivo de adutoras não encontrado em DIR_RAW_AURORA/compesa nem data/compesa.")
}

message(glue::glue("Lendo adutoras de: {src_geo}"))
dados <- st_read(src_geo, quiet = TRUE)

# 3. Filtrar apenas as adutoras de Pernambuco ("PE") ----
dados_pe <- dados %>%
  filter(ESTADO == "PE")

# 4. Salvar o resultado filtrado no repositório central do Aurora ----
out_destinos <- c(
  file.path(DIR_OUT_AURORA, "adutoras_pernambuco.geojson"),
  file.path(DIR_OUT_AURORA, "geojson", "adutoras_pernambuco.geojson")
)

for (out_file in out_destinos) {
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  st_write(
    obj = dados_pe,
    dsn = out_file,
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  message(glue::glue("✅ Adutoras salvas com sucesso em: {out_file}"))
}

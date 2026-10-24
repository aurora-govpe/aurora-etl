library(sf)
library(dplyr)

# 1. Carregar o arquivo GeoJSON/JSON
# (Se o arquivo estiver salvo em disco)
dados <- st_read("data/compesa/Estágio_do_Sistema_de_Adutoras_d_Água.geojson")

# 2. Filtrar apenas as adutoras de Pernambuco ("PE")
dados_pe <- dados %>%
  filter(ESTADO == "PE")

# 3. Salvar o resultado filtrado em um novo arquivo GeoJSON
st_write(
  obj = dados_pe,
  dsn = "data/compesa/adutoras_pernambuco.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)

st_write(
  obj = dados_pe,
  dsn = "app_aurora_municipios/data/adutoras_pernambuco.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)

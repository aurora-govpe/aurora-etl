# ---- 1. Pacotes ----
# install.packages(c("arrow", "censobr", "cnefetools", "dplyr", "geobr", "purrr", "sf", "stringr"))

library(arrow)
library(censobr)
library(cnefetools)
library(dplyr)
library(geobr)
library(purrr)
library(sf)
library(stringr)

# ---- 2. Configuração de Diretório ----
pasta_ibge <- "C:/dados/dados brutos/aurora/ibge"
dir.create(pasta_ibge, recursive = TRUE, showWarnings = FALSE)

# ---- 3. Extração dos Dados Brutos (IBGE) ----
municipios_raw <- read_municipality(code_muni = "PE", year = 2022, simplified = FALSE)

pop_raw        <- read_tracts(year = 2022, dataset = "Basico", as_data_frame = TRUE)

setores_raw    <- read_census_tract(code_tract = "PE", year = 2022, simplified = FALSE)

# ---- 4. Transformação e Junção ----
municipios_pe <- municipios_raw |>
  st_transform(31984) |>
  transmute(code_muni = as.character(code_muni), name_muni = as.character(name_muni))

pop_2022 <- pop_raw |>
  rename_with(tolower) |>
  filter(str_starts(code_tract, "26")) |>
  transmute(code_tract = as.character(code_tract), pop_rural_ibge = coalesce(v0001, 0))

setores_rurais_pe <- setores_raw |>
  filter(zone == 'rural') |>
  st_transform(31984) |>
  transmute(code_tract = as.character(code_tract), code_muni = as.character(code_muni)) |>
  left_join(pop_2022, by = "code_tract") |>
  mutate(pop_rural_ibge = coalesce(pop_rural_ibge, 0))

setores_urbanos_pe <- setores_raw |>
  filter(zone == 'urbano') |>
  st_transform(31984) |>
  transmute(code_tract = as.character(code_tract), code_muni = as.character(code_muni)) |>
  left_join(pop_2022, by = "code_tract") |>
  mutate(pop_rural_ibge = coalesce(pop_rural_ibge, 0))

# ---- 5. Extração do CNEFE 2022 (Mantido no ETL) ----
setores_ids <- unique(setores_rurais_pe$code_tract)

extrair_cnefe <- function(cod_muni) {
  tryCatch({
    read_cnefe(code_muni = as.integer(cod_muni), cache = TRUE) |>
      filter(COD_ESPECIE == 1, !is.na(LATITUDE), !is.na(LONGITUDE)) |>
      select(COD_SETOR, LATITUDE, LONGITUDE) |>
      collect() |>
      transmute(code_muni = cod_muni, code_tract = as.character(COD_SETOR), lat = LATITUDE, lon = LONGITUDE) |>
      filter(code_tract %in% setores_ids)
  }, error = function(e) tibble())
}

cnefe_rural_pe <- map_dfr(municipios_pe$code_muni, extrair_cnefe)

# ---- 6. Exportação Local ----
st_write(municipios_pe, file.path(pasta_ibge, "municipios_pe_2022.gpkg"), delete_dsn = TRUE, quiet = TRUE)

st_write(setores_rurais_pe, file.path(pasta_ibge, "setores_rurais_pe_2022.gpkg"), delete_dsn = TRUE, quiet = TRUE)

st_write(setores_urbanos_pe, file.path(pasta_ibge, "setores_urbanos_pe_2022.gpkg"), delete_dsn = TRUE, quiet = TRUE)

write_parquet(cnefe_rural_pe, file.path(pasta_ibge, "cnefe_domicilios_rurais_pe_2022.parquet"), compression = "zstd")
# pacotes ----
library(BigDataPE)
library(data.table)
library(DBI)
library(dplyr)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)
library(purrr)
library(qs)
library(RPostgres)
library(sf)
library(stringi)
library(stringr)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# setup ----
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(auth_path)) {
  gs4_auth(
    path = auth_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
}

config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) file.path(DIR_RAW_AURORA, "config.yml") else "config.yml"
config_tokens <- if (file.exists(config_path)) config::get('tokens', file = config_path) else NULL

# utils ----
normalize_ascii <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# etl ----
## mapa ----
shp_pe_mun_limites <- readRDS(file.path(DIR_RAW_AURORA, 'pernambuco_sf.rds')) %>%
  clean_names() %>%
  mutate(aux_municipio = normalize_ascii(municipio))

## equipamentos_combate_fome ----
equipamentos_combate_fome_bdpe <- tryCatch(
  {
    dados <- BigDataPE::bdpe_fetch_data(
      base_name = "equipamentos_combate_fome",
      verbosity = 1
    ) %>% 
      clean_names()
    
    cli::cli_alert_success("Dados de equipamentos_combate_fome baixados via BDPE API.")
    dados
  },
  error = function(e) {
    cli::cli_alert_warning("Falha na API BDPE ({e$message}). Carregando base local de backup...")
    
    # Carrega a base local de backup
    readRDS(file.path(DIR_OUT_AURORA, 'df_cozinhas.rds'))
  }
)

equipamentos_combate_fome_bdpe %>% glimpse()

equipamentos_combate_fome_bdpe_2 <- equipamentos_combate_fome_bdpe %>%
  select(
    -c(municipio,
       regiao_de_desenvolvimento
       
       )
  ) %>%
  left_join(
    shp_pe_mun_limites %>%
      st_drop_geometry() %>%
      select(ibge7, municipio, regiao_de_desenvolvimento) %>%
      mutate(ibge7 = as.integer(ibge7)),
    by = c('cod_ibge' = 'ibge7')
  )

equipamentos_combate_fome_bdpe_2 %>% glimpse()

equipamentos_combate_fome_bdpe_2 %>%
  saveRDS(
    file.path(DIR_OUT_AURORA, 'df_cozinhas.rds')
  )

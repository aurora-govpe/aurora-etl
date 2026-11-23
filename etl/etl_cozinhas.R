# java options (obrigatoriamente na primeira linha) ----
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-21.0.5.11-hotspot") # Ajuste para a sua pasta instalada
options(java.parameters = "-Xmx4g")

# pacotes ----
library(DBI)
library(dplyr)
library(glue)
library(janitor)
library(jsonlite)
library(RJDBC)
library(rJava)
library(sf)
library(stringr)
library(tidygeocoder)

# paths e diretorios base ----
caminhos_paths <- c("R/paths.R", "paths.R", "../paths.R", "etl/paths.R", "../../R/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"

dir_aurora     <- DIR_OUT_AURORA
cache_geo_path <- file.path(DIR_OUT_AURORA, "cache_geocodificacao_cozinhas.json")
mapa_pe_path   <- if (file.exists(file.path(dir_aurora, "mapa_pe.json"))) {
  file.path(dir_aurora, "mapa_pe.json")
} else if (file.exists(file.path(DIR_RAW_AURORA, "mapa_pe.json"))) {
  file.path(DIR_RAW_AURORA, "mapa_pe.json")
} else {
  file.path(dir_aurora, "mapa_pe.json")
}

out_rds_path     <- file.path(DIR_OUT_AURORA, "df_cozinhas.rds")
out_parquet_path <- file.path(DIR_OUT_AURORA, "parquet/df_cozinhas.parquet")
out_json_path    <- file.path(DIR_OUT_AURORA, "df_cozinhas.json")

# setup conexao trino ----
config_path <- file.path(dir_aurora, "config.yml")
if (!file.exists(config_path)) config_path <- file.path(DIR_RAW_AURORA, "config.yml")

con <- NULL
if (file.exists(config_path)) {
  tryCatch({
    config_bdpe <- config::get("bdpe", file = config_path)
    drv <- JDBC(
      driverClass = "io.trino.jdbc.TrinoDriver",
      classPath   = file.path(dir_aurora, "trino-jdbc-468.jar")
    )
    url <- glue(
      "jdbc:trino://{config_bdpe$db_host}:{config_bdpe$db_port}?SSL=true&user={config_bdpe$db_user}&password={config_bdpe$db_pass}"
    )
    con <- dbConnect(drv, url)
    cli::cli_alert_success("Conexão Trino estabelecida!")
  }, error = function(e) {
    cli::cli_alert_warning("Não foi possível conectar ao Trino: {e$message}")
  })
}

# 1. extracao via sql (com fallback para rds local) ----
p_shp_sf <- if (file.exists(file.path(DIR_RAW_AURORA, "pernambuco_sf.rds"))) {
  file.path(DIR_RAW_AURORA, "pernambuco_sf.rds")
} else {
  file.path(DIR_OUT_AURORA, "pernambuco_sf.rds")
}

shp_pe_mun_limites <- readRDS(p_shp_sf) %>% clean_names()

equipamentos_bdpe <- if (!is.null(con)) {
  tryCatch(
    {
      dados <- dbGetQuery(con, "select * from servico_hive.ingestedata.equipamentos_combate_fome") %>%
        clean_names()
      cli::cli_alert_success("Dados de equipamentos_combate_fome extraídos via Trino SQL.")
      dados
    },
    error = function(e) {
      cli::cli_alert_warning("Falha na consulta Trino ({e$message}). Carregando backup local...")
      readRDS(out_rds_path)
    }
  )
} else {
  cli::cli_alert_info("Carregando base de cozinhas do backup local ({out_rds_path})...")
  readRDS(out_rds_path)
}

# 2. transformacao e padronizacao cadastral ----
df_cozinhas <- equipamentos_bdpe %>%
  # Descarta coordenadas e validações prévias se os dados vieram do backup RDS
  select(any_of(c("cod_ibge", "cod_servico", "nome_equipamento", "status_funcionamento", "endereco"))) %>%
  left_join(
    shp_pe_mun_limites %>%
      st_drop_geometry() %>%
      select(ibge7, municipio, regiao_de_desenvolvimento) %>%
      mutate(ibge7 = as.integer(ibge7)),
    by = c("cod_ibge" = "ibge7")
  ) %>%
  mutate(
    endereco_limpo = endereco %>%
      str_remove_all("(?i)\\b(s/n|sn|sem n[uú]mero)\\b") %>%
      str_replace_all("[,\\s]+", " ") %>%
      str_squish(),
    query_geocoding = glue("{endereco_limpo}, {municipio}, Pernambuco, Brasil")
  )

# 3. carregar cache do disco (fonte da verdade) ----
cache_geo <- if (file.exists(cache_geo_path)) {
  fromJSON(cache_geo_path) %>%
    as_tibble() %>%
    mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
    # Garante que qualquer alteração manual salva no fim do JSON sobreponha valores antigos
    group_by(query_geocoding) %>%
    slice_tail(n = 1) %>%
    ungroup()
} else {
  tibble(query_geocoding = character(), lat = numeric(), long = numeric())
}

# 4. geocodificacao apenas para novos enderecos ----
enderecos_novos <- df_cozinhas %>%
  filter(!query_geocoding %in% cache_geo$query_geocoding) %>%
  distinct(query_geocoding)

if (nrow(enderecos_novos) > 0) {
  cli::cli_alert_info("Buscando {nrow(enderecos_novos)} novos endereços via ArcGIS...")
  
  novas_coordenadas <- enderecos_novos %>%
    geocode(
      address = query_geocoding,
      method = "arcgis",
      lat = lat,
      long = long
    )
  
  cache_geo <- bind_rows(cache_geo, novas_coordenadas) %>%
    group_by(query_geocoding) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  write_json(cache_geo, cache_geo_path, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success("Novos endereços salvos no cache JSON!")
} else {
  cli::cli_alert_success("Todos os endereços já estavam presentes no cache.")
}

# 5. acopla coordenadas do cache ----
df_cozinhas <- df_cozinhas %>%
  left_join(
    cache_geo %>% select(query_geocoding, lat, long),
    by = "query_geocoding"
  )

# 6. validacao espacial direta com limites municipais ----
if (file.exists(mapa_pe_path)) {
  mapa_pe <- read_sf(mapa_pe_path) %>%
    clean_names() %>%
    st_transform(4326)
  
  col_ibge_mapa <- intersect(
    names(mapa_pe),
    c("cod_ibge", "cd_mun", "ibge7", "cd_geocmu", "codigo_ibge", "codigo")
  )[1]
  
  mapa_valida <- mapa_pe %>%
    select(ibge_mun = all_of(col_ibge_mapa)) %>%
    mutate(ibge_mun = as.integer(ibge_mun))
  
  pontos_sf <- df_cozinhas %>%
    filter(!is.na(lat), !is.na(long)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
  
  validacao_pontos <- st_join(pontos_sf, mapa_valida, join = st_intersects) %>%
    st_drop_geometry() %>%
    group_by(query_geocoding) %>%
    summarise(coord_valida_municipio = any(cod_ibge == ibge_mun, na.rm = TRUE))
  
  df_cozinhas <- df_cozinhas %>%
    left_join(validacao_pontos, by = "query_geocoding")
} else {
  df_cozinhas <- df_cozinhas %>%
    mutate(coord_valida_municipio = !is.na(lat) & !is.na(long))
}

df_cozinhas <- df_cozinhas %>%
  select(-any_of(c("endereco_limpo", "query_geocoding")))

# 7. gravacao final e encerramento ----
saveRDS(df_cozinhas, out_rds_path)
write_json(df_cozinhas, out_json_path, pretty = TRUE, auto_unbox = TRUE)
if (dir.exists(dirname(out_parquet_path))) {
  arrow::write_parquet(df_cozinhas, out_parquet_path)
}

if (!is.null(con)) {
  tryCatch(dbDisconnect(con), error = function(e) NULL)
}

cli::cli_alert_success("ETL Cozinhas finalizado com sucesso!")

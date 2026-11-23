# ==============================================================================
# ETL CONSÓRCIOS PÚBLICOS DE PERNAMBUCO
# ==============================================================================

library(googledrive)
library(readxl)
library(dplyr)
library(jsonlite)

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

# 2. Autenticação via Google Drive ----
AUTH_PATH <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
} else if (file.exists("data/r-sheets-hugo-0788cc358cb5.json")) {
  "data/r-sheets-hugo-0788cc358cb5.json"
} else {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
}

drive_auth(path = AUTH_PATH)

url_consorcios <- "https://docs.google.com/spreadsheets/d/1TMp2yjI18LRP0bAeOmNHWZbVTW7YmOiE/edit?gid=390795986#gid=390795986"

caminho_temp_consorcios <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_consorcios), path = caminho_temp_consorcios, overwrite = TRUE)

aba_municipios <- readxl::read_excel(caminho_temp_consorcios, sheet = 'municipios_e_consorcios') %>%
  janitor::clean_names()

aba_descricao <- readxl::read_excel(caminho_temp_consorcios, sheet = 'descricao_consorcios') %>%
  janitor::clean_names()

# 3. Cruza as bases ----
base_consolidada <- aba_municipios |>
  left_join(aba_descricao, by = "consorcio")

# 4. Exporta para JSON central do Aurora ----
out_json <- file.path(DIR_OUT_AURORA, "consorcios_pe.json")
write_json(base_consolidada, out_json, pretty = TRUE)

# Backup secundário na pasta json/ se existir
if (dir.exists(file.path(DIR_OUT_AURORA, "json"))) {
  write_json(base_consolidada, file.path(DIR_OUT_AURORA, "json", "consorcios_pe.json"), pretty = TRUE)
}

cat(glue::glue("Arquivo 'consorcios_pe.json' gerado com sucesso em: {out_json}\n"))

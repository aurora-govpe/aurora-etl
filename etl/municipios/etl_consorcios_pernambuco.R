library(googledrive)
library(readxl)
library(dplyr)
library(jsonlite)

# 1. Autenticação via googledrive ----
drive_auth(
  path = file.path("data", "r-sheets-hugo-0788cc358cb5.json")
)

url_consorcios <- "https://docs.google.com/spreadsheets/d/1TMp2yjI18LRP0bAeOmNHWZbVTW7YmOiE/edit?gid=390795986#gid=390795986"

caminho_temp_consorcios <- tempfile(fileext = ".xlsx")

drive_download(file = googledrive::as_id(url_consorcios), path = caminho_temp_consorcios, overwrite = TRUE)

aba_municipios <- readxl::read_excel(caminho_temp_consorcios, sheet = 'municipios_e_consorcios') %>%
  janitor::clean_names()

aba_descricao <- readxl::read_excel(caminho_temp_consorcios, sheet = 'descricao_consorcios') %>%
  janitor::clean_names()

# 5. Cruza as bases
base_consolidada <- aba_municipios |>
  left_join(aba_descricao, by = "consorcio")

# 6. Exporta para JSON
write_json(base_consolidada, "app_aurora_municipios/data/consorcios_pe.json", pretty = TRUE)

cat("Arquivo 'consorcios_pe.json' gerado com sucesso!\n")
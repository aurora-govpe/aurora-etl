# bibliotecas ----
library(BigDataPE)
library(bslib)
library(cli)
library(fs)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(purrr)
library(readxl)
library(rmapshaper)
library(sf)
library(stringi)
library(stringr)
library(tidyverse)
library(vialactea)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# setup ----
options(scipen = 999)

auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(auth_path)) {
  drive_auth(cache = FALSE, path = auth_path)
}

DIR_DADOS <- DIR_OUT_AURORA

url_pac <- "https://docs.google.com/spreadsheets/d/1IPmAHCes6ujcJHqHOQ-f6AdlU-Okw5oEGbLYID9YRac/edit?gid=1575105298#gid=1575105298"


# extrair ----
dados_pac <- read_sheet(
  ss = url_pac
) %>%
  janitor::clean_names() %>%
  rename(
    estimativa_de_valor_total = estimativa_de_valor_total_do_empreendimento_2023_2030_r
  ) %>%
  mutate(
    estimativa_de_valor_total = parse_number(
      estimativa_de_valor_total, 
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  )

dados_pac %>% glimpse()

# Gravação unificada
path_out <- file.path(DIR_OUT_AURORA, "dados_pac.rds")
write_rds(dados_pac, path_out)

## subindo no drive
drive_upload(
  media     = path_out,
  path      = googledrive::as_id("1IU7unR4LOscOCXB92xGufCxX4z2kKEjf"), # id da pasta
  name      = "dados_pac.rds",
  overwrite = TRUE
)


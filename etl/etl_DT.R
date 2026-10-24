# bibliotecas ----
library(BigDataPE)
library(bslib)
library(googledrive)
library(glue)
library(reactable)
library(readxl)
library(sf)
library(tidyverse)
library(vialactea)

# setup ----
source('scripts/utils.R')

valid_status <- c(
  "Em formulação", "Em elaboração de projeto", "Concluída", "A licitar",
  "Em licitação", "A iniciar", "Em execução", "Paralisada", "Cancelada"
)

valid_eixo <- c("Conhecimento e inovação", "Desenvolvimento sustentável", "Saúde e qualidade de vida", "Segurança e cidadania" )

drive_auth(path = "data/token_eco-condition-406519-adac7b9fb8e5.json", cache = TRUE)

bdpe_store_token(base_name = "maespe_beneficiarias_municipio", token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjb2xlY2FvIjoiNjg0MDgzY2I2NTQzZjUyMmNlZmFiZTA1IiwidXN1YXJpbyI6IjY0NjM3NTQ4OTJiZDg2NGUwMzkxNjJhYSIsImlhdCI6MTc0OTE0OTU1M30.RK_LrYuwIPi7nLO808ypf3rICeC252tD-MDwCSBgMv8")

# extrair ----
source('scripts/esquemas de dados.R')

shared_iniciativas <- read_rds("data/shared_iniciativas.rds")

# transformar ----
## reactable ----
library(reactable)
tbl_base <- shared_iniciativas %>% 
  mutate(detalhes = map(
    id_produto, ~ as.character(
      tags$div(
        class = "mlk-details-btn-list",
        tags$button(
          class = "mlk-details-btn ",
          onclick = paste0("mlkRowButtonDetails('", .x, "', '", "main-detalhes_mapa", "');"),
          tooltip(
            fontawesome::fa("map", a11y = "sem", title = "Teste"),
            "Abrir mapa do trecho",
            options = list(trigger = "hover")
          )
        )
      )
    )
  ), .after = ordem) %>% unnest(detalhes) %>% 
  mutate(status_html = case_when(
    status == "Em formulação" ~ '<span class="mlk-hex-status-ef"></span>',
    status == "Em elaboração de projeto" ~ '<span class="mlk-hex-status-ep"></span>',
    status == "A licitar" ~ '<span class="mlk-hex-status-al"></span>',
    status == "Em licitação" ~ '<span class="mlk-hex-status-el"></span>',
    status == "A iniciar" ~ '<span class="mlk-hex-status-ai"></span>',
    status == "Em execução" ~ '<span class="mlk-hex-status-ee"></span>',
    status == "Concluída" ~ '<span class="mlk-hex-status-c"></span>',
    status == "Não informado" ~ '<span class="mlk-hex-status-ni"></span>',
    status == "Cancelada" ~ '<span class="mlk-hex-status-cl"></span>',
    status == "Paralisada" ~ '<span class="mlk-hex-status-p"></span>',
    .default = as.character(vialactea::status_badge(width = "1rem", height = "1rem", color = "#ecececF0", style = ""))
  )) %>%
  mutate(eixo_html = case_when(
    eixo_estrategico == "Conhecimento e inovação" ~ '<span class="mlk-hex-icon-ci"></span>',
    # leaf (sustentável)
    eixo_estrategico == "Desenvolvimento sustentável" ~ '<span class="mlk-hex-icon-ds"></span>',
    # heart (saúde e qualidade de vida)
    eixo_estrategico == "Saúde e qualidade de vida" ~ '<span class="mlk-hex-icon-sqv"></span>',
    # shield-check (segurança e cidadania)
    eixo_estrategico == "Segurança e cidadania" ~ '<span class="mlk-hex-icon-sc"></span>'
  )) %>%
  rowwise() %>% 
  mutate(
    eixo_html = as.character(tooltip(HTML(eixo_html), eixo_estrategico, options = list(html = TRUE, trigger = "hover"))),
    status_html = as.character(tooltip(HTML(status_html), status, options = list(html = TRUE, trigger = "hover")))
  ) %>% 
  ungroup() %>%
  relocate(fonte, .after = secretaria)

# salvar ----
# Salva APENAS a base de dados em formato tibble
write_rds(tbl_base, "app_aurora/data/tbl_base_2.rds")


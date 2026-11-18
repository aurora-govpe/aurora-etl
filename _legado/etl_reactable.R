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
    eixo_estrategico == "Desenvolvimento sustentável" ~ '<span class="mlk-hex-icon-ds"></span>',
    eixo_estrategico == "Saúde e qualidade de vida" ~ '<span class="mlk-hex-icon-sqv"></span>',
    eixo_estrategico == "Segurança e cidadania" ~ '<span class="mlk-hex-icon-sc"></span>'
  )) %>%
  rowwise() %>% 
  mutate(
    eixo_html = as.character(tooltip(HTML(eixo_html), eixo_estrategico, options = list(html = TRUE, trigger = "hover"))),
    status_html = as.character(tooltip(HTML(status_html), status, options = list(html = TRUE, trigger = "hover")))
  ) %>% 
  ungroup() %>%
  select(
    -any_of(
      c(
        "eixo_icon", 
        "status_icon",
        'endereco',
        'construtora',
        'obras_incidentes',
        'proponentes',
        'entidades',
        'uh',
        'tipologia_das_uh',
        'status_da_contratacao',
        'valor_de_contrapartida_do_estado_far_fds_e_retomadas_valor_contratual_aditivado_reforma_no_lar_i',
        'valor_avaliado_do_terreno_doado',
        'comunidade',
        'shapefile',
        'area_total_m2',
        'status_de_ocupacao_pcx',
        'situacao_de_transferencia_de_propriedade_pc',
        'situacao_de_transferencia_de_propriedade_pcx',
        'tipo_de_uh_producao_ou_melhoria',
        'cadastrados',
        'a_iniciar',
        'em_andamento',
        'concluido',
        'doacao_de_terreno',
        'obras_nao_incidentes',
        'processo_seletivo',
        'financiamento'
      )
    )
  )

tbl <- tbl_base %>% 
  relocate(fonte, .after = secretaria) %>% 
  reactable(
    language = reactable::reactableLang(
      searchPlaceholder = "Pesquisar...",
      noData = "Nenhum item encontrado.",
      pageSizeOptions = "Mostrar {rows}",
      pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
      pagePrevious = "\u276e",
      pageNext = "\u276f",
      pagePreviousLabel = "Pág. anterior",
      pageNextLabel = "Pág. seguinte"
    ),
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    ),
    wrap = TRUE,
    fullWidth = TRUE,
    striped = TRUE,
    filterable = TRUE,
    searchable = FALSE,
    outlined = FALSE,
    bordered = FALSE,
    highlight = TRUE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 20, 50, 100),
    showSortable = TRUE,
    defaultSorted = "ordem",
    defaultSortOrder = "asc"
  )

write_rds(tbl_base, "app_aurora/data/tbl_base_2.rds")

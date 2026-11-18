# bibliotecas ----
library(tidyverse)
library(reactable)
library(htmltools)
library(vialactea)

# extrair ----
shared_iniciativas <- read_rds("data/shared_iniciativas.rds")

# transformar ----

# 1. Preparação dos dados
tbl_base <- shared_iniciativas %>% 
  mutate(
    status_class = case_when(
      status == "Em formulação" ~ "ef",
      status == "Em elaboração de projeto" ~ "ep",
      status == "A licitar" ~ "al",
      status == "Em licitação" ~ "el",
      status == "A iniciar" ~ "ai",
      status == "Em execução" ~ "ee",
      status == "Concluída" ~ "c",
      .default = "ni"
    ),
    eixo_class = case_when(
      eixo_estrategico == "Conhecimento e inovação" ~ "ci",
      eixo_estrategico == "Desenvolvimento sustentável" ~ "ds",
      eixo_estrategico == "Saúde e qualidade de vida" ~ "sqv",
      eixo_estrategico == "Segurança e cidadania" ~ "sc",
      .default = "default"
    )
  )

# 2. Definição da Reactable com SEU estilo original
tbl <- tbl_base %>% 
  relocate(fonte, .after = secretaria) %>% 
  reactable(
    wrap = TRUE, fullWidth = TRUE, striped = TRUE, filterable = TRUE,
    highlight = TRUE, compact = TRUE, showSortable = TRUE,
    defaultPageSize = 10
  )

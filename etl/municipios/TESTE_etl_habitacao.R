library(dplyr)

DIR_EXPORT_DATA    <- "C:/dados/aurora"
DIR_EXPORT_PARQUET <- file.path(DIR_EXPORT_DATA, "parquet")

# ------------------------------------------------------------------------------
# Base Gerada pelo Etl seduh sehab do Bot
# ------------------------------------------------------------------------------
url_seduh_sehab <- "https://docs.google.com/spreadsheets/d/1MI6maAX1nLvcJZFq3QmxYoz3QK2A2Nz5QsDdUTtDm4g/edit?gid=199486837#gid=199486837"

arquivo_seduh_sehab <- drive_download(
  as_id(url_seduh_sehab),
  path      = tempfile(fileext = ".xlsx"),
  overwrite = TRUE
)

dados_seduh_sehab <- excel_sheets(arquivo_seduh_sehab$local_path) %>%
  set_names() %>%
  map(~ read_excel(arquivo_seduh_sehab$local_path, sheet = .x, col_types = "text"))

chat <- dados_seduh_sehab$`Sheet1` %>%
  rename(
    valor_total         = valor_global_orcado2,
    valor_medido_rnl    = valor_medido,
    valor_contrapartida = custo2,
    data_de_termino     = previsao_de_entrega
  ) %>%
  filter(!is.na(iniciativa_estrategica)) %>%
  mutate(
    secretaria_unidade   = 'SEDUH - SEHAB',
    secretaria           = "SEDUH",
    indicador_secundario = NA_character_,
    executor             = NA_character_,
    status = case_when(
      !is.na(concluido) ~ "Concluída",
      !is.na(cadastrados) ~ "A iniciar",
      status_de_obra_projeto %in% c("A iniciar", "A INICIAR", "FASE DE CONTRATAÇÃO", "Projeto em andamento") ~ "A iniciar",
      status_de_obra_projeto %in% c("Concluído", "CONCLUÍDA", "CONCLUÍDA COM PENDÊNCIAS") ~ "Concluída",
      status_de_obra_projeto %in% c("À LICITAR", "A LICITAR") ~ "A licitar",
      status_de_obra_projeto == "Obra em andamento" ~ "Em execução",
      status_de_obra_projeto == "EM PROCESSO DE RETOMADA" ~ "Paralisada",
      
      is.na(status_de_obra_projeto) ~ "Em formulação",
      
      .default = NA_character_  
    ),
    indicador = case_when(
      str_detect(iniciativa_estrategica, "REFORMA NO LAR") & status == "Concluída" ~ concluido,
      str_detect(iniciativa_estrategica, "REFORMA NO LAR") & status == "A iniciar" ~ cadastrados,
      TRUE ~ uh
    )
  ) %>% 
  mutate(produto = empreendimento, 
         valor_global_orcado2 = valor_total) %>% 
  select(quantidade_de_empreendimentos, indicador, everything())


# Base tabular principal sem geometria
chat %>% arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "df_habitacao.parquet"))

# Exportação de df_iniciativas em formato JSON
jsonlite::write_json(
  x = chat,
  path = file.path(DIR_EXPORT_DATA, "df_habitacao.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

# ------------------------------------------------------------------------------
# REGULARIZAÇÕES
# ------------------------------------------------------------------------------

url_dados_perpart <- "https://docs.google.com/spreadsheets/d/1N-y4OfG9OQLspGGKwlFQD2DD1OhLU6zJ1n9oThQxeC0/edit?gid=467727708#gid=467727708"

perpart_df_multi <- read_sheet(ss = url_dados_perpart, sheet = "BASE ANA_NT") %>%
  clean_names() %>%
  select(!starts_with("x")) %>%
  rename(status_detalhado = status) %>%
  mutate(across(c(lat, long), ~ map_chr(.x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x)))) %>%
  mutate(
    eixo_estrategico       = "DESENVOLVIMENTO SUSTENTÁVEL",
    iniciativa_estrategica = "PROGRAMA MORAR BEM",
    secretaria             = "SEDUH",
    secretaria_unidade     = orgao,
    produto                = "REGULARIZAÇÃO FUNDIÁRIA PERPART REURB",
    status                 = case_when(situacao == "Em Andamento" ~ "Em execução", situacao == "Realizados" ~ "Concluída", TRUE ~ NA_character_),
    percent_execucao       = case_when(situacao == "Em Andamento" ~ "0", situacao == "Realizados" ~ "100.0", TRUE ~ NA_character_),
    data_de_inicio         = NA_character_,
    data_de_termino        = NA_character_,
    valor_total            = NA_character_,
    latitude               = as.character(as.numeric(lat)),
    longitude              = as.character(as.numeric(long)),
    local                  = nc,
    observacoes            = paste0(regularizacoes, " Títulos - ", status_detalhado),
    endereco_target        = NA_character_,
    indicador              = as.character(regularizacoes),
    indicador_nome         = "Regularizações",
    indicador_secundario   = NA_character_,
    executor               = NA_character_
  )


# Base tabular principal sem geometria
perpart_df_multi %>% arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "df_regularizacoes.parquet"))

# Exportação de df_iniciativas em formato JSON
jsonlite::write_json(
  x = perpart_df_multi,
  path = file.path(DIR_EXPORT_DATA, "df_regularizacoes.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)
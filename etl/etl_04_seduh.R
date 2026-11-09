# ==============================================================================
# SCRIPT 04: HABITAÇÃO E DESENVOLVIMENTO URBANO (SEDUH)
# Órgãos Atendidos: CEHAB, SEDU, PERPART e SEHAB
# Objetivo: Unificar a carteira de habitação e obras urbanas em formato tabular.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. FUNÇÃO DE NORMALIZAÇÃO DE NOMES DE MUNICÍPIO
# ------------------------------------------------------------------------------
padronizar_municipio <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("(?i)\\s*-\\s*PE$", "") %>% # Remove ' - PE' ou '-PE'
    stri_trans_general("Latin-ASCII") %>%      # Remove acentuações (Água Preta -> Agua Preta)
    str_squish() %>%                           # Remove múltiplos espaços em branco
    toupper()                                  # Padroniza em maiúsculas
}

# ------------------------------------------------------------------------------
# 1. PREPARAÇÃO DO DICIONÁRIO DE-PARA (IBGE7)
# ------------------------------------------------------------------------------
de_para_ibge <- ficha_municipal %>%
  select(ibge7, municipio) %>%
  distinct(ibge7, .keep_all = TRUE) %>%
  mutate(municipio_norm = padronizar_municipio(municipio)) %>%
  select(ibge7, municipio_norm)

# ------------------------------------------------------------------------------
# 1. CEHAB (COMPANHIA ESTADUAL DE HABITAÇÃO E OBRAS)
# ------------------------------------------------------------------------------
url_dados_cehab <- "https://docs.google.com/spreadsheets/d/1iE3Sl2CTIDH2cbEp4fI4Tqk-dS92pze83RgTmNC5NRI/edit?gid=1983646507#gid=1983646507"

df_cehab_raw <- read_sheet(
  ss    = url_dados_cehab,
  sheet = "Principal"
) %>%
  clean_names()

df_cehab <- df_cehab_raw %>%
  mutate(
    across(
      where(is.list),
      ~ map_chr(.x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x))
    )
  ) %>%
  mutate(
    latitude  = str_replace_all(str_squish(as.character(latitude)), ",", "."),
    longitude = str_replace_all(str_squish(as.character(longitude)), ",", "."),
    latitude  = if_else(!str_detect(latitude, "\\."), str_replace(latitude, "^([-+]?\\d)(\\d+)$", "\\1.\\2"), latitude),
    longitude = if_else(!str_detect(longitude, "\\."), str_replace(longitude, "^([-+]?\\d{2})(\\d+)$", "\\1.\\2"), longitude)
  ) %>%
  rename(secretaria_unidade_orig = secretaria_diretoria) %>%
  mutate(
    secretaria         = "SEDUH",
    fonte              = "CEHAB",
    secretaria_unidade = if_else(
      secretaria_unidade_orig == fonte,
      str_c(fonte, diretoria, sep = " - "),
      str_c(fonte, diretoria, secretaria_unidade_orig, sep = " - ")
    ),
    data_de_inicio   = format(as.Date(data_de_inicio), "%d/%m/%Y"),
    data_de_termino  = format(as.Date(data_de_termino), "%d/%m/%Y"),
    percent_execucao = str_replace(str_squish(as.character(as.numeric(percent_execucao_fisica) / 100)), ",", "."),
    valor_total      = as.character(valor_total)
  ) %>%
  select(-secretaria_unidade_orig)

df_cehab_final <- df_cehab %>%
  mutate(municipio = case_when(
    municipio == "ITAMARACÁ" ~ "ILHA DE ITAMARACÁ",
    municipio == "BELÉM DE SÃO FRANCISCO"  ~ "BELÉM DO SÃO FRANCISCO",
    municipio == "SÃO CAETANO"  ~ "SÃO CAITANO",
    TRUE ~ municipio
  )) %>%
  mutate(municipio_norm = padronizar_municipio(municipio)) %>%
  left_join(de_para_ibge, by = "municipio_norm") %>%
  select(ibge7, everything(), -municipio_norm)

df_cehab_final %>% filter(is.na(ibge7) & !is.na(municipio)) %>% distinct(municipio)

# Exportação Parquet
arrow::write_parquet(
  x = df_cehab_final,
  sink = file.path(DIR_EXPORT_PARQUET, "df_obras.parquet")
)

# Exportação JSON
jsonlite::write_json(
  x = df_cehab_final,
  path = file.path(DIR_EXPORT_JSON, "df_obras.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cli_alert_success("Bases da CEHAB processadas ({nrow(df_cehab)} registros).")


# ------------------------------------------------------------------------------
# 2. SEDUH / SEDU (DESENVOLVIMENTO URBANO)
# ------------------------------------------------------------------------------
file_id_sedu <- "1ASShW1mUDfefciQEGth57ZfAdb-UXGqU"
auth_hugo_path <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(auth_hugo_path)) {
  googledrive::drive_auth(path = auth_hugo_path)
} else {
  cli_alert_warning("Credencial do Hugo para acesso ao arquivo da SEDU não encontrada em: {.path {auth_hugo_path}}")
}

seduh_file_source <- file.path(
  DIR_RAW_AURORA,
  glue::glue("seduh_base_geral_aurora_{format(datatime_file_source, '%Y_%m_%d_T%Hh%Mm')}.xlsx")
)

tryCatch({
  drive_download(file = as_id(file_id_sedu), path = seduh_file_source, overwrite = TRUE)
}, error = function(e) {
  drive_download(
    file      = as_id(file_id_sedu),
    path      = seduh_file_source,
    type      = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    overwrite = TRUE
  )
})

seduh_df_multi <- readxl::read_excel(seduh_file_source, col_types = "text") %>%
  clean_names() %>%
  rename(
    data_de_inicio       = data_inicio,
    data_de_termino      = data_termino,
    produto              = objeto_resumido,
    percent_execucao     = percent_obra,
    observacoes          = observacoes_aurora,
    empresa_contratada   = empresa, 
    indicador_secundario = indicador_previsto
  ) %>%
  # filter(!fonte %in% c("[SEDUH] Novos Projetos", "[SEDUH] NOVOS PROJETOS", "[SCI] Ilumina Ata", "[SCI] ILUMINA ATA")) %>%
  filter(!fonte %in% c("[SEDUH] Novos Projetos", "[SEDUH] NOVOS PROJETOS")) %>%
  mutate(
    secretaria_unidade = "SEDUH - SEDU",
    acao               = acao, 
    subacao            = subacao,
    secretaria         = "SEDUH",
    executor           = if_else(empresa_contratada == "NEOENERGIA", "NEOENERGIA", "SEDUH"),
    endereco_target    = NA_character_,
    municipio          = case_when(
      produto == "PAVIMENTAÇÃO ASFÁLTICA DE DIVERSAS RUAS NO MUNICÍPIO DE ÁGUA PRETA - PE" ~ "Água Preta",
      produto == "PAVIMENTAÇÃO ASFÁLTICA DE DIVERSAS RUAS DO MUNICÍPIO DE EXÚ-PE" ~ "Exú",
      TRUE ~ municipio
    ),
    status = case_when(
      fonte == "[SCI] NOVOS CONVÊNIOS AUTORIZADOS" ~ "Em formulação",
      TRUE ~ status
    )
  )

df_ilumina_final <- seduh_df_multi %>%
  mutate(municipio = case_when(
    municipio == "SÃO CAETANO" ~ "SÃO CAITANO",
    TRUE ~ municipio
  )) %>%
  mutate(municipio_norm = padronizar_municipio(municipio)) %>%
  left_join(de_para_ibge, by = "municipio_norm") %>%
  select(ibge7, everything(), -municipio_norm) %>%
  filter(iniciativa_estrategica == "ILUMINA PERNAMBUCO") 


df_ilumina_final %>% filter(is.na(ibge7) & !is.na(municipio)) %>% distinct(municipio)

# Exportação Parquet
arrow::write_parquet(
  x = df_ilumina_final,
  sink = file.path(DIR_EXPORT_PARQUET, "df_iluminacao.parquet")
)

# Exportação JSON
jsonlite::write_json(
  x = df_ilumina_final,
  path = file.path(DIR_EXPORT_JSON, "df_iluminacao.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

df_ilumina_final %>% View()

cli_alert_success("Bases da SEDU processadas ({nrow(seduh_df_multi)} registros).")


# ------------------------------------------------------------------------------
# 3. PERPART (PERNAMBUCO PARTICIPAÇÕES E INVESTIMENTOS S/A)
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

# # Base tabular principal sem geometria
# perpart_df_multi %>% arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "df_regularizacoes.parquet"))
# 
# # Exportação de df_iniciativas em formato JSON
# jsonlite::write_json(
#   x = perpart_df_multi,
#   path = file.path(DIR_EXPORT_JSON, "df_regularizacoes.json"),
#   pretty = TRUE,
#   auto_unbox = TRUE,
#   na = "null"
# )

df_regularizacoes_final <- perpart_df_multi %>%
  mutate(municipio_norm = padronizar_municipio(municipio)) %>%
  left_join(de_para_ibge, by = "municipio_norm") %>%
  select(ibge7, everything(), -municipio_norm)

# Exportação Parquet
arrow::write_parquet(
  x = df_regularizacoes_final,
  sink = file.path(DIR_EXPORT_PARQUET, "df_regularizacoes.parquet")
)

# Exportação JSON
jsonlite::write_json(
  x = df_regularizacoes_final,
  path = file.path(DIR_EXPORT_JSON, "df_regularizacoes.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cli_alert_success(
  "df_regularizacoes exportado com ibge7 ({sum(!is.na(df_regularizacoes_final$ibge7))}/{nrow(df_regularizacoes_final)} casados)."
)

# ------------------------------------------------------------------------------
# 4. SEDUH / SEHAB (SECRETARIA EXECUTIVA DE HABITAÇÃO)
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

seduh_sehab_df_multi <- dados_seduh_sehab$`Sheet1` %>%
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
  mutate(valor_global_orcado3 = valor_total)

# Base tabular principal sem geometria
seduh_sehab_df_multi %>% arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "df_habitacao.parquet"))

# Exportação de df_iniciativas em formato JSON
jsonlite::write_json(
  x = seduh_sehab_df_multi,
  path = file.path(DIR_EXPORT_JSON, "df_habitacao.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

# df_habitacao_final <- seduh_sehab_df_multi %>%
#   mutate(municipio_norm = padronizar_municipio(municipio)) %>%
#   left_join(de_para_ibge, by = "municipio_norm") %>%
#   select(ibge7, everything(), -municipio_norm)
# 
# # Exportação Parquet
# arrow::write_parquet(
#   x = df_habitacao_final,
#   sink = file.path(DIR_EXPORT_PARQUET, "df_habitacao.parquet")
# )
# 
# # Exportação JSON
# jsonlite::write_json(
#   x = df_habitacao_final,
#   path = file.path(DIR_EXPORT_JSON, "df_habitacao.json"),
#   pretty = TRUE,
#   auto_unbox = TRUE,
#   na = "null"
# )

cli_alert_success(
  "df_habitacao exportado com ibge7 ({sum(!is.na(df_habitacao_final$ibge7))}/{nrow(df_habitacao_final)} casados)."
)


df_regularizacoes_final %>% filter(is.na(ibge7) & !is.na(municipio)) %>% distinct(municipio)
seduh_sehab_df_multi %>% filter(is.na(ibge7) & !is.na(municipio)) %>% distinct(municipio)

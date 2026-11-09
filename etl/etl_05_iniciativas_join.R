# utils ----
norm_mun_ini <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    trimws()
}

# ==============================================================================
# SCRIPT 05: UNIFICAÇÃO, CONSOLIDADO ESPACIAL E EXPORTAÇÕES (AURORA ETL)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. EMPILHAMENTO DAS BASES SETORIAIS (CONVERSÃO DE PREVENÇÃO PARA CHARACTER)
# ------------------------------------------------------------------------------
locais_iniciativas <- bind_rows(
  if (exists("dados_convenios_secult") && !is.null(dados_convenios_secult)) 
    dados_convenios_secult %>% mutate(across(everything(), as.character), fonte = "SECULT"),
  
  if (exists("dados_convenios_educacao") && !is.null(dados_convenios_educacao)) 
    dados_convenios_educacao %>% mutate(across(everything(), as.character), fonte = "SEE"),
  
  if (exists("dados_compesa_processed") && !is.null(dados_compesa_processed)) 
    dados_compesa_processed %>% mutate(across(everything(), as.character), fonte = "COMPESA"),
  
  if (exists("df_cehab") && !is.null(df_cehab)) 
    df_cehab %>% mutate(across(everything(), as.character), fonte = "CEHAB"),
  
  if (exists("seduh_df_multi") && !is.null(seduh_df_multi)) 
    seduh_df_multi %>% mutate(across(everything(), as.character), fonte = "SEDUH"),
  
  if (exists("df_sepe") && !is.null(df_sepe)) 
    df_sepe %>% mutate(across(everything(), as.character), fonte = "SEPE"),
  
  if (exists("seduh_sehab_df_multi") && !is.null(seduh_sehab_df_multi)) 
    seduh_sehab_df_multi %>% mutate(across(everything(), as.character), fonte = "SEDUH"),
  
  if (exists("perpart_df_multi") && !is.null(perpart_df_multi)) 
    perpart_df_multi %>% mutate(across(everything(), as.character), fonte = "PERPART"),
  
  if (exists("df_transporte_final") && !is.null(df_transporte_final)) 
    df_transporte_final %>% mutate(across(everything(), as.character), fonte = "SEE")
  
) %>%
  mutate(
    # Normalização dos eixos estratégicos
    eixo_estrategico = case_when(
      str_detect(eixo_estrategico, regex("SEGURANÇ", ignore_case = TRUE))                 ~ "SEGURANÇA E CIDADANIA",
      str_detect(eixo_estrategico, regex("SAÚDE", ignore_case = TRUE))                    ~ "SAÚDE E QUALIDADE DE VIDA",
      str_detect(eixo_estrategico, regex("HABITAÇÃO|INFRAESTRUTURA", ignore_case = TRUE)) ~ "DESENVOLVIMENTO SUSTENTÁVEL",
      str_detect(eixo_estrategico, regex("EDUCAÇÃO|CULTURA|INOVAÇÃO", ignore_case = TRUE)) ~ "CONHECIMENTO E INOVAÇÃO",
      TRUE ~ eixo_estrategico
    ),
    municipio  = if_else(municipio == "Jaboatão dos Guararapes e Recife", "Jaboatão dos Guararapes", municipio),
    secretaria = if_else(secretaria == "CEHAB/SEDUH", "CEHAB", secretaria)
  )

cli_alert_success("Setoriais empilhados com sucesso sem conflitos de tipo ({nrow(locais_iniciativas)} registros).")

locais_iniciativas %>% filter(fonte == "SECULT")

locais_iniciativas %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

locais_iniciativas %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))


# ------------------------------------------------------------------------------
# 2. CARREGAMENTO DE BASES TRANSVERSAIS E DERIVADAS
# ------------------------------------------------------------------------------

# 2.1 DER / Mulungu e Geometrias de Obras/Projetos via DIR_RAW_AURORA e DIR_DADOS
mulungu_path <- file.path(DIR_RAW_AURORA, "base_mulungu_aurora.rds")
base_mulungu_aurora <- if (file.exists(mulungu_path)) read_rds(mulungu_path) %>% mutate(fonte = "DER") else NULL

geom_obras_path     <- file.path(DIR_DADOS, "obras_notion_geometrias2.rds")
geometrias_obras    <- if (file.exists(geom_obras_path)) read_rds(geom_obras_path) else NULL

geom_proj_path      <- file.path(DIR_DADOS, "projetos_geometrias.rds")
geometrias_projetos <- if (file.exists(geom_proj_path)) read_rds(geom_proj_path) else NULL


# 2.2 Programas Sociais: Mães de Pernambuco (BDPE)
maes_raw <- tryCatch(
  {
    dados <- bdpe_fetch_data("maespe_beneficiarias_municipio", verbosity = 1) %>% as_tibble()
    cli_alert_success("Dados do Mães de Pernambuco baixados via BDPE API.")
    dados
  },
  error = function(e) {
    cli_alert_warning("Falha na API BDPE ({e$message}). Carregando base local de backup...")
    read_rds(file.path(DIR_RAW_AURORA, "maes_raw.rds"))
  }
)

# Salva cópia de segurança local no diretório de dados brutos
write_rds(maes_raw, file.path(DIR_RAW_AURORA, "maes_raw.rds"))

maes_pe <- maes_raw %>%
  rename(ibge7 = cod_mun_ibge) %>%
  filter(periodicidade == "p") %>%
  left_join(
    pe_sf %>% st_drop_geometry() %>% select(ibge7, municipio = Município),
    by = "ibge7"
  ) %>%
  mutate(
    eixo_estrategico       = "Saúde e qualidade de vida",
    iniciativa_estrategica = "Mães de Pernambuco",
    secretaria             = "SAS",
    status                 = "Em execução",
    data_de_inicio         = NA_character_,
    data_de_termino        = NA_character_,
    percent_execucao       = NA_character_,
    latitude               = NA_character_,
    longitude              = NA_character_,
    observacoes            = NA_character_,
    endereco_target        = NA_character_,
    local                  = municipio,
    valor_total            = as.character(valor_total),
    produto                = str_trim(paste(format(qtd_beneficiarias_total, decimal.mark = ",", big.mark = "."), "mães beneficiadas")),
    indicador              = as.character(qtd_beneficiarias_total),
    indicador_nome         = "Mães Beneficiadas",
    indicador_secundario   = NA_character_,
    executor               = NA_character_
  ) %>%
  select(any_of(colunas_processamento))


# 2.3 Cofinanciamento da Assistência Social (SAS)
cofinanciamento_path <- file.path(DIR_RAW_AURORA, "cofinanciamento.rds")

cofinanciamento <- if (file.exists(cofinanciamento_path)) {
  read_rds(cofinanciamento_path) %>% 
    mutate(
      data_de_inicio   = as.character(data_de_inicio),
      data_de_termino  = as.character(data_de_termino),
      latitude         = as.character(latitude),
      longitude        = as.character(longitude),
      local            = NA_character_,
      endereco_target  = as.character(endereco_target),
      percent_execucao = "100%",
      valor_total      = format(valor_total, scientific = FALSE),
      produto          = iniciativa_estrategica
    ) %>%
    select(any_of(colunas_processamento))
} else NULL


# ------------------------------------------------------------------------------
# 3. UNIFICAÇÃO, LIMPEZA E FORMATAÇÃO DAS INICIATIVAS
# ------------------------------------------------------------------------------
df_iniciativas <- locais_iniciativas %>% 
  bind_rows(maes_pe %>% mutate(fonte = "SAS")) %>% 
  bind_rows(cofinanciamento %>% mutate(fonte = "SAS")) %>%
  mutate(
    secretaria = case_when(
      secretaria %in% c("ADEPE", "COPERGÁS", "PORTO DO RECIFE") ~ "SDEC",
      secretaria %in% c("IPA")                                  ~ "SEAGRI",
      secretaria %in% c("FUNDARPE")                             ~ "SECULT",
      TRUE ~ secretaria 
    )
  )

df_iniciativas %>% filter(fonte == "SECULT")

df_iniciativas %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

df_iniciativas %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- df_iniciativas %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 
unique(teste$iniciativa_estrategica)

library(tidyverse)
library(lubridate)

df_iniciativas2 <- df_iniciativas %>% 
  filter(str_to_upper(status) %in% str_to_upper(valid_status)) %>% 
  mutate(status = str_to_sentence(status)) %>% 
  filter(str_to_upper(eixo_estrategico) %in% str_to_upper(valid_eixo)) %>% 
  mutate(
    eixo_estrategico = str_to_sentence(eixo_estrategico),
    
    # Normalização prévia de strings de datas
    across(c(data_de_inicio, data_de_termino), ~ str_remove(.x, "\\.0$")),
    
    # Parsing flexível de datas (valores inválidos viram NA silenciosamente)
    data_de_inicio = coalesce(
      suppressWarnings(as_date(as.numeric(data_de_inicio), origin = "1899-12-30")),
      suppressWarnings(ymd(data_de_inicio)),
      suppressWarnings(dmy(data_de_inicio))
    ),
    data_de_termino = coalesce(
      suppressWarnings(as_date(as.numeric(data_de_termino), origin = "1899-12-30")),
      suppressWarnings(ymd(data_de_termino)),
      suppressWarnings(dmy(data_de_termino))
    ),
    
    # Tratamento de conflito cronológico (início > término)
    data_de_termino = if_else(!is.na(data_de_inicio) & !is.na(data_de_termino) & data_de_termino < data_de_inicio, NA_Date_, data_de_termino),
    
    # Normalização de Percentuais
    percent_execucao = parse_number(str_replace(str_replace(percent_execucao, ",", "."), "%", "E-2")),
    percent_execucao = if_else(percent_execucao > 1.01, percent_execucao / 100, percent_execucao),
    
    # Cleaning e Parsing Numérico dos Valores Financeiros sem warnings
    valor_total = if_else(str_detect(valor_total, "-") | !str_detect(valor_total, "\\d"), NA_character_, valor_total),
    valor_total = str_remove_all(valor_total, "(\\.|,)\\d+$"),
    valor_total = if_else(str_detect(valor_total, "E\\d+$"), valor_total, str_remove_all(valor_total, "\\.")),
    valor_total = suppressWarnings(
      parse_number(if_else(str_detect(valor_total, "R\\$"), str_replace(str_remove_all(valor_total, "(R\\$\\s*)"), ",", "."), valor_total))
    ),
    
    latitude  = suppressWarnings(parse_number(latitude)),
    longitude = suppressWarnings(parse_number(longitude)),
    id        = row_number(), 
    .before   = eixo_estrategico
  )

df_iniciativas2 %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

df_iniciativas2 %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- df_iniciativas2 %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 
unique(teste$iniciativa_estrategica)

# ------------------------------------------------------------------------------
# 4. NORMALIZAÇÃO GEOGRÁFICA DE MUNICÍPIOS E JITTERING DE COORDENADAS
# ------------------------------------------------------------------------------
iniciativas_cidades_limpas <- df_iniciativas2 %>% 
  mutate(municipio = str_split(municipio, ",")) %>% 
  unnest(municipio) %>% 
  mutate(municipio = str_trim(municipio)) %>% 
  mutate(
    municipio = case_match(
      municipio, 
      "BELÉM DE SÃO FRANCISCO" ~ "BELÉM DO SÃO FRANCISCO",
      "Belém de São Francisco" ~ "BELÉM DO SÃO FRANCISCO",
      "BELÉM DE S. FRANCISCO" ~ "BELÉM DO SÃO FRANCISCO",
      "EXÚ" ~ "EXU",
      "Exú" ~ "EXU",
      "QUIXABÁ" ~ "QUIXABA",
      "SÃO CAITANO" ~ "SÃO CAETANO",
      "São Caitano" ~ "SÃO CAETANO",
      "TACAIMBO" ~ "TACAIMBÓ",
      "IGUARACI" ~ "IGUARACY", 
      "OURUCURI" ~ "OURICURI",
      "TUPARATEMA" ~ "TUPARETAMA",
      "AFOGADOS DA IGAZEIRA" ~ "AFOGADOS DA INGAZEIRA",
      "GRANTO" ~ "GRANITO",
      "ITAMARACÁ" ~ "ILHA DE ITAMARACÁ",       
      "LAGOA DO ITAENGA" ~ "LAGOA DE ITAENGA",
      "STA. CRUZ DO CAPIBARIBE" ~ "SANTA CRUZ DO CAPIBARIBE",
      "STA. MARIA DO CAMBUCÁ" ~ "SANTA MARIA DO CAMBUCÁ",
      "STA. CRUZ DA BAIXA VERDE" ~ "SANTA CRUZ DA BAIXA VERDE",
      .default = municipio
    )) %>% 
  mutate(municipio = str_to_lower(municipio)) %>% 
  left_join(pe_sf %>% st_drop_geometry() %>% 
              select(Município, ibge7, `Região de Desenvolvimento`, longitude_sede = longitude, latitude_sede = latitude, População) %>% 
              mutate(municipio = str_to_lower(Município)),
            by = join_by(municipio)) %>% 
  select(-municipio) %>% 
  mutate(
    latitude = if_else(is.na(latitude), if_else(is.na(latitude_sede), -8.3826, latitude_sede), latitude),
    longitude = if_else(is.na(longitude), if_else(is.na(longitude_sede), -37.8647, longitude_sede), longitude)
  ) %>% 
  janitor::clean_names() %>% 
  mutate(regiao_de_desenvolvimento = coalesce(regiao_de_desenvolvimento_2, regiao_de_desenvolvimento)) %>% 
  ungroup() %>% 
  group_by(id) %>% 
  mutate(fator_de_divisao_municipio = 1/n()) %>% 
  mutate(valor_total = valor_total * fator_de_divisao_municipio) %>% 
  mutate(fator_de_divisao_rd = 1/length(na.omit(unique(regiao_de_desenvolvimento)))) %>% 
  ungroup() %>% 
  group_by(latitude) %>% 
  mutate(latitude = round(latitude + (n() > 1)*(.0001/2)*runif(n(), -1, 1), 5)) %>% 
  ungroup() %>% 
  group_by(longitude) %>% 
  mutate(longitude = round(longitude + (n() > 1)*(.0001/4)*runif(n(), -1, 1), 5)) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326) %>% 
  ungroup() %>% 
  select(-starts_with(match = "x"))

iniciativas_cidades_limpas %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

iniciativas_cidades_limpas %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- iniciativas_cidades_limpas %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 
unique(teste$iniciativa_estrategica)

# ------------------------------------------------------------------------------
# 5. CONSTRUÇÃO DA BASE DO MAPA (AURORA GEOJSON & TABLES)
# ------------------------------------------------------------------------------
base_aurora <- iniciativas_cidades_limpas %>% 
  mutate(
    inicio     = if_else(is.na(data_de_inicio), "S/I", format(data_de_inicio, "%b/%Y")),
    termino    = if_else(is.na(data_de_termino), "S/I", format(data_de_termino, "%b/%Y")),
    ordem      = row_number(),
    avanco_cor = as.character(vialactea::get_score_color(percent_execucao)),
    label      = "not a label"
  ) %>% 
  ungroup() %>% 
  mutate(
    id_produto             = row_number(),
    secretaria_unidade     = if_else(is.na(secretaria_unidade) | secretaria_unidade == "", str_remove(secretaria, " - .*"), secretaria_unidade),
    secretaria             = str_remove(secretaria, " - .*"),
    iniciativa_estrategica = str_squish(iniciativa_estrategica)
  )

base_aurora %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

base_aurora %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- base_aurora %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 

# Incorporação de Ícones
base_aurora_2 <- base_aurora 

base_aurora_2 %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

base_aurora_2 %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- base_aurora_2 %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 

# Estruturação dos códigos para o Mapbox
base_aurora_3 <- base_aurora_2 %>% 
  mutate(
    cd_status = case_when(
      status == "Em formulação"            ~ "ef",
      status == "Em elaboração de projeto" ~ "ep",
      status == "A licitar"                ~ "al",
      status == "Em licitação"              ~ "el",
      status == "A iniciar"                ~ "ai",
      status == "Em execução"              ~ "ee",
      status == "Concluída"                ~ "c",
      status == "Não informado"            ~ "ni",
      status == "Cancelada"                ~ "cl",
      status == "Paralisada"               ~ "p",
      TRUE ~ NA_character_
    ),
    cd_eixo = case_when(
      eixo_estrategico == "Conhecimento e inovação"    ~ "ci",
      eixo_estrategico == "Desenvolvimento sustentável" ~ "ds",
      eixo_estrategico == "Saúde e qualidade de vida"  ~ "sqv",
      eixo_estrategico == "Segurança e cidadania"      ~ "sc",
      TRUE ~ NA_character_
    )
  )

base_aurora_geojson <- geojsonsf::sf_geojson(base_aurora_3)

base_aurora_3 %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

base_aurora_3 %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- base_aurora_3 %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 

# ------------------------------------------------------------------------------
# 6. UNIFICAÇÃO FINAL COMPLETA (AURORA + MULUNGU)
# ------------------------------------------------------------------------------
shared_iniciativas_temp <- base_aurora_2 %>% 
  mutate(percent_execucao = round(100 * percent_execucao)) %>% 
  select(any_of(c(colunas_aurora, "latitude", "longitude"))) %>% 
  rename(ordem = id)

shared_iniciativas_temp %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

shared_iniciativas_temp %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- shared_iniciativas_temp %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 

max_id_prod <- max(shared_iniciativas_temp$id_produto, na.rm = TRUE)

shared_iniciativas <- shared_iniciativas_temp %>% 
  bind_rows(
    if (!is.null(base_mulungu_aurora)) {
      base_mulungu_aurora %>%
        mutate(
          ordem              = id_mulungu + max_id_prod,
          secretaria_unidade = secretaria,
          id_produto         = row_number() + max_id_prod
        ) %>%
        select(-id_mulungu)
    } else NULL
  ) %>% 
  # Limpeza final de encodings e padronização de textos
  mutate(
    municipio = if_else(municipio == "Jaboatão dos Guararapes e Recife", "Recife", municipio),
    municipio = if_else(municipio == "Jaboatão dos Guararapes, Recife", "Recife", municipio),
    status    = case_match(
      status,
      "ConcluÃ­da"                  ~ "Concluída",
      "Em execuÃ§Ã£o"              ~ "Em execução",
      "Em elaboraÃ§Ã£o de projeto" ~ "Em elaboração de projeto",
      "Em formulaÃ§Ã£o"            ~ "Em formulação",
      "Em licitaÃ§Ã£o"              ~ "Em licitação",
      "Aprovada"                   ~ "A iniciar",
      .default                     = status
    ),
    eixo_estrategico = case_match(
      eixo_estrategico,
      "Desenvolvimento sustentÃ¡vel" ~ "Desenvolvimento sustentável",
      "SaÃºde e qualidade de vida"    ~ "Saúde e qualidade de vida",
      "SeguranÃ§a e cidadania"        ~ "Segurança e cidadania",
      "Conhecimento e inovaÃ§Ã£o"    ~ "Conhecimento e inovação",
      .default                     = eixo_estrategico
    ),
    across(c(iniciativa_estrategica, produto), ~ str_replace_all(.x, dicionario_siglas))
  ) %>% 
  # Remove as linhas que possuem a combinação de iniciativa e fonte informadas
  filter(
    !(iniciativa_estrategica == "CRIAÇÃO DE 60 MIL VAGAS EM CRECHES E PRÉ-ESCOLA" & fonte == "SEPE")
  )

shared_iniciativas %>% 
  filter(iniciativa_estrategica == "REFORMA NO LAR I") %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

shared_iniciativas %>% 
  filter(
    str_detect(produto, "REGULARIZA"),
    status == "Concluída"
  ) %>% 
  mutate(indicador = as.numeric(indicador)) %>% 
  summarise(total = sum(indicador))

teste <- shared_iniciativas %>% 
  filter(secretaria_unidade == "SEDUH - SEHAB") 

caminho_demo <- file.path(DIR_DADOS_AURORA_MUNI, "df_demografia_pe.rds")

if (file.exists(caminho_demo)) {
  df_demo <- readRDS(caminho_demo)
  
  # Cria o dicionário
  dicionario_ibge <- df_demo %>% 
    st_drop_geometry() %>% 
    select(municipio, ibge7) %>% 
    mutate(
      municipio_norm = norm_mun_ini(municipio),
      cod_ibge = substr(as.character(ibge7), 1, 6),
      ibge6 = substr(as.character(ibge7), 1, 6)
    ) %>% 
    select(municipio_norm, cod_ibge, ibge6) %>% 
    distinct()
  
  # Cruza com a base final de iniciativas
  shared_iniciativas <- shared_iniciativas %>% 
    mutate(municipio_norm = norm_mun_ini(municipio)) %>% 
    left_join(dicionario_ibge, by = "municipio_norm") %>% 
    select(-municipio_norm)
  
  cli_alert_success("Código IBGE de 6 dígitos cruzado com sucesso na base shared_iniciativas.")
} else {
  cli_alert_warning("df_demografia_pe.rds não encontrado. Base shared_iniciativas continuará sem cod_ibge.")
}


# ------------------------------------------------------------------------------
# 7. EXPORTAÇÕES DE ARQUIVOS (PARQUET, GEOJSON, GERAIS)
# ------------------------------------------------------------------------------

# 7.0 Definição e criação das pastas de destino
DIR_EXPORT_DATA    <- "C:/dados/aurora"
DIR_EXPORT_PARQUET <- file.path(DIR_EXPORT_DATA, "parquet")
DIR_EXPORT_GEOJSON <- file.path(DIR_EXPORT_DATA, "geojson")

fs::dir_create(c(DIR_EXPORT_DATA, DIR_EXPORT_PARQUET, DIR_EXPORT_GEOJSON))

# ------------------------------------------------------------------------------
# 7.1 EXPORTAÇÕES PARQUET (data/parquet/)
# ------------------------------------------------------------------------------
col_ibge <- if ("ibge6" %in% names(shared_iniciativas)) "ibge6" else "ibge7"

# Base tabular principal sem geometria
shared_iniciativas %>% 
  sf::st_drop_geometry() %>% 
  mutate(
    ibge6 = substr(as.character(.data[[col_ibge]]), 1, 6)
  ) %>% 
  arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "df_iniciativas.parquet"))


df_iniciativas <- shared_iniciativas %>% 
  sf::st_drop_geometry() %>% 
  mutate(
    ibge6 = substr(as.character(.data[[col_ibge]]), 1, 6)
  )

# Exportação de df_iniciativas em formato JSON
jsonlite::write_json(
  x = df_iniciativas,
  path = file.path(DIR_EXPORT_DATA, "df_iniciativas.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)
cli_alert_success("Arquivo 'df_iniciativas.json' gravado em: {DIR_EXPORT_DATA}")

# Exportação parquet da base de iniciativas municipais
base_aurora %>% 
  sf::st_drop_geometry() %>% 
  arrow::write_parquet(file.path(DIR_EXPORT_PARQUET, "base_aurora.parquet"))

cli_alert_success("Arquivos Parquet gravados em: {DIR_EXPORT_PARQUET}")

# ------------------------------------------------------------------------------
# 7.2 EXPORTAÇÕES GEOJSON (data/geojson/)
# ------------------------------------------------------------------------------
# Base unificada do mapa
readr::write_lines(base_aurora_geojson, file.path(DIR_EXPORT_GEOJSON, "iniciativas.geojson"))

# Polígonos e geometrias auxiliares
if (exists("seduh_sehab_shp_poligono_3") && !is.null(seduh_sehab_shp_poligono_3)) {
  geojsonsf::sf_geojson(seduh_sehab_shp_poligono_3) %>% 
    readr::write_lines(file.path(DIR_EXPORT_GEOJSON, "df_reforma_no_lar.geojson"))
}

if (exists("geometrias_obras") && !is.null(geometrias_obras)) {
  geojsonsf::sf_geojson(geometrias_obras) %>% 
    readr::write_lines(file.path(DIR_EXPORT_GEOJSON, "geometrias_obras.geojson"))
}

if (exists("geometrias_projetos") && !is.null(geometrias_projetos)) {
  geojsonsf::sf_geojson(geometrias_projetos) %>% 
    readr::write_lines(file.path(DIR_EXPORT_GEOJSON, "geometrias_projetos.geojson"))
}

cli_alert_success("Camadas GeoJSON gravadas em: {DIR_EXPORT_GEOJSON}")

# ------------------------------------------------------------------------------
# 7.3 DEMAIS ARQUIVOS (PASTA GERAL: data/)
# ------------------------------------------------------------------------------
# Arquivos RDS locais de uso operacional
write_rds(
  shared_iniciativas %>% distinct(secretaria, secretaria_unidade) %>% arrange(secretaria_unidade),
  file.path(DIR_EXPORT_DATA, "secretarias_unidades.rds")
)

write_rds(base_aurora, file.path(DIR_EXPORT_DATA, "iniciativas.rds"))
write_rds(shared_iniciativas, file.path(DIR_EXPORT_DATA, "shared_iniciativas.rds"))

if (exists("geometrias_obras") && !is.null(geometrias_obras)) {
  write_rds(geometrias_obras, file.path(DIR_EXPORT_DATA, "geometrias_obras.rds"))
}

# Planilha consolidada de conferência
shared_iniciativas %>% 
  select(secretaria, secretaria_unidade, eixo_estrategico, iniciativa_estrategica, produto) %>% 
  writexl::write_xlsx(file.path(DIR_EXPORT_DATA, "iniciativas_resumo.xlsx"))

# Atualização de Metadata do config.yml
# config_yml_path <- file.path(DIR_EXPORT_DATA, "config.yml")
# if (file.exists(config_yml_path)) {
#   config_file <- readr::read_lines(config_yml_path)
#   config_file[4] <- glue::glue("  update: \"{ format(Sys.time(), '%Y/%m/%d %H:%M') }\"")
#   readr::write_lines(config_file, config_yml_path, append = FALSE)
#   cli_alert_success("Metadata de atualização gravada no config.yml com sucesso!")
# }

# Localização do config: busca em 'data/' ou na raiz como fallback
config_yml_path  <- file.path(DIR_EXPORT_DATA, "config.yml")
config_json_path <- file.path(DIR_EXPORT_JSON, "config.json")

# Se não estiver em data/, mas existir na raiz, copia para data/
if (!file.exists(config_yml_path) && file.exists("config.yml")) {
  file.copy("config.yml", config_yml_path)
}

data_hora_atual <- format(Sys.time(), "%Y/%m/%d %H:%M")

if (file.exists(config_yml_path)) {
  config_data <- yaml::read_yaml(config_yml_path)
  
  # Se o config tiver estrutura aninhada (ex: config_data$default), atualiza no nó correto
  if ("default" %in% names(config_data)) {
    config_data$default$update <- data_hora_atual
  } else {
    config_data$update <- data_hora_atual
  }
  
  # Grava config.yml
  yaml::write_yaml(config_data, config_yml_path)
  cli_alert_success("Metadata gravada em {config_yml_path}: {data_hora_atual}")
  
  # Grava config.json
  jsonlite::write_json(
    x = config_data,
    path = config_json_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli_alert_success("Configuração exportada para {config_json_path} com sucesso!")
  
} else {
  # Cria um arquivo novo caso não exista previamente
  config_data <- list(update = data_hora_atual)
  yaml::write_yaml(config_data, config_yml_path)
  jsonlite::write_json(config_data, config_json_path, pretty = TRUE, auto_unbox = TRUE)
  cli_alert_warning("Arquivo {config_yml_path} não existia; foi inicializado com a data atual.")
}

cli_alert_success("Bases gerais gravadas em: {DIR_EXPORT_DATA}")

# ------------------------------------------------------------------------------
# 7.4 SINCRONIZAÇÃO EM NUVEM (GOOGLE DRIVE)
# ------------------------------------------------------------------------------
caminho_temp_upload <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(shared_iniciativas, caminho_temp_upload)

googledrive::drive_upload(
  media     = caminho_temp_upload,
  path      = as_id("1nc_x9z1sX6pUXKNBBku9hXCImjr5fKn4"),
  name      = "Iniciativas de Pernambuco",
  type      = "spreadsheet",
  overwrite = TRUE
)

cli_alert_success("Base 'shared_iniciativas' enviada para o Google Drive com sucesso!")
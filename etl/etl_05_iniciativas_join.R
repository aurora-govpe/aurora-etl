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
    df_transporte_final %>% mutate(across(everything(), as.character), fonte = "SEE"),
  
) %>%
  mutate(
    # Normalização dos eixos estratégicos
    eixo_estrategico = case_when(
      str_detect(eixo_estrategico, regex("SEGURANÇ", ignore_case = TRUE))            ~ "SEGURANÇA E CIDADANIA",
      str_detect(eixo_estrategico, regex("SAÚDE", ignore_case = TRUE))               ~ "SAÚDE E QUALIDADE DE VIDA",
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

# 2.1 DER / Mulungu e Geometrias de Obras/Projetos
mulungu_path_parquet <- file.path(DIR_OUT_AURORA, "parquet/base_mulungu_aurora.parquet")
mulungu_path_rds <- if (file.exists(file.path(DIR_OUT_AURORA, "base_mulungu_aurora.rds"))) {
  file.path(DIR_OUT_AURORA, "base_mulungu_aurora.rds")
} else {
  file.path(DIR_RAW_AURORA, "base_mulungu_aurora.rds")
}
base_mulungu_aurora <- if (file.exists(mulungu_path_parquet)) {
  arrow::read_parquet(mulungu_path_parquet) %>% mutate(fonte = "DER")
} else if (file.exists(mulungu_path_rds)) {
  read_rds(mulungu_path_rds) %>% mutate(fonte = "DER")
} else NULL

geom_obras_path     <- if (file.exists(file.path(DIR_OUT_AURORA, "obras_notion_geometrias2.rds"))) file.path(DIR_OUT_AURORA, "obras_notion_geometrias2.rds") else file.path(DIR_RAW_AURORA, "obras_notion_geometrias2.rds")
geometrias_obras    <- if (file.exists(geom_obras_path)) read_rds(geom_obras_path) else NULL

geom_proj_path      <- if (file.exists(file.path(DIR_OUT_AURORA, "projetos_geometrias.rds"))) file.path(DIR_OUT_AURORA, "projetos_geometrias.rds") else file.path(DIR_RAW_AURORA, "projetos_geometrias.rds")
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

# Salva cópia de segurança local
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
  # mutate(latitude = if_else((is.na(latitude)) & (!is.na(latitude_sede)), latitude_sede, latitude)) %>% 
  # mutate(longitude = if_else((is.na(longitude)) & (!is.na(longitude_sede)), longitude_sede, longitude)) %>% 
  mutate(
    latitude = if_else(is.na(latitude), if_else(is.na(latitude_sede), -8.3826, latitude_sede), latitude),
    longitude = if_else(is.na(longitude), if_else(is.na(longitude_sede), -37.8647, longitude_sede), longitude)
  ) %>%
  janitor::clean_names() %>% 
  mutate(regiao_de_desenvolvimento = coalesce(regiao_de_desenvolvimento_2, regiao_de_desenvolvimento)) %>%
  ungroup() %>% 
  group_by(id) %>% 
  mutate(fator_de_divisao_municipio = 1/n()) %>%
  mutate(valor_total = valor_total*fator_de_divisao_municipio) %>% 
  mutate(fator_de_divisao_rd = 1/length(na.omit(unique(regiao_de_desenvolvimento)))) %>%
  # select(indicador_nome, indicador, latitude, longitude, latitude_sede, longitude_sede, everything()) %>%
  # filter(!is.na(latitude), !is.na(longitude)) %>% 
  # filter(
  #   !(longitude < (-180 - .Machine$double.eps) |
  #       longitude > (360 + .Machine$double.eps) |
  #       latitude < (-90 - .Machine$double.eps) |
  #       latitude > (90 + .Machine$double.eps))) %>%
  ungroup() %>% 
  group_by(latitude) %>% 
  mutate(latitude = round(latitude + (n() > 1)*(.0001/2)*runif(n(), -1, 1), 5)) %>%
  ungroup() %>% 
  group_by(longitude) %>% 
  mutate(longitude = round(longitude + (n() > 1)*(.0001/4)*runif(n(), -1, 1), 5)) %>%
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)  %>% 
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
    inicio  = if_else(is.na(data_de_inicio), "S/I", format(data_de_inicio, "%b/%Y")),
    termino = if_else(is.na(data_de_termino), "S/I", format(data_de_termino, "%b/%Y")),
    ordem   = row_number(),
    avanco_cor = as.character(vialactea::get_score_color(percent_execucao)),
    label      = "not a label"
  ) %>%
  ungroup() %>%
  mutate(
    id_produto         = row_number(),
    secretaria_unidade = if_else(is.na(secretaria_unidade) | secretaria_unidade == "", str_remove(secretaria, " - .*"), secretaria_unidade),
    secretaria         = str_remove(secretaria, " - .*"),
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
# %>%
#   relocate(secretaria_unidade, .after = secretaria) %>%
#   left_join(base_aurora_icones, by = c("iniciativa_estrategica", "secretaria"))

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

# Exporta tabela de controle para conferência de ícones ausentes
# base_aurora_2 %>%
#   filter(is.na(icone_tipo)) %>%
#   select(iniciativa_estrategica, avanco_cor, label, id_produto, icone_tipo)  %>%
#   writexl::write_xlsx(file.path(DIR_ETL, "data", "mapbox", "base_aurora_icones_aux.xlsx"))

# Estruturação dos códigos para o Mapbox
base_aurora_3 <- base_aurora_2 %>% 
  mutate(
    cd_status = case_when(
      status == "Em formulação"            ~ "ef",
      status == "Em elaboração de projeto" ~ "ep",
      status == "A licitar"                ~ "al",
      status == "Em licitação"             ~ "el",
      status == "A iniciar"                ~ "ai",
      status == "Em execução"              ~ "ee",
      status == "Concluída"                ~ "c",
      status == "Não informado"            ~ "ni",
      status == "Cancelada"                ~ "cl",
      status == "Paralisada"               ~ "p",
      TRUE ~ NA_character_
    ),
    cd_eixo = case_when(
      eixo_estrategico == "Conhecimento e inovação"   ~ "ci",
      eixo_estrategico == "Desenvolvimento sustentável"~ "ds",
      eixo_estrategico == "Saúde e qualidade de vida" ~ "sqv",
      eixo_estrategico == "Segurança e cidadania"     ~ "sc",
      TRUE ~ NA_character_
    ),
    # icon_status = paste0(cd_status, "-", str_to_lower(icone_tipo)),
    # icon_eixo   = paste0("base-eixo-", cd_eixo, "-", str_to_lower(secretaria))
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
      "ConcluÃ­da"                 ~ "Concluída",
      "Em execuÃ§Ã£o"             ~ "Em execução",
      "Em elaboraÃ§Ã£o de projeto"~ "Em elaboração de projeto",
      "Em formulaÃ§Ã£o"            ~ "Em formulação",
      "Em licitaÃ§Ã£o"             ~ "Em licitação",
      "Aprovada"                  ~ "A iniciar",
      .default                    = status
    ),
    eixo_estrategico = case_match(
      eixo_estrategico,
      "Desenvolvimento sustentÃ¡vel" ~ "Desenvolvimento sustentável",
      "SaÃºde e qualidade de vida"    ~ "Saúde e qualidade de vida",
      "SeguranÃ§a e cidadania"        ~ "Segurança e cidadania",
      "Conhecimento e inovaÃ§Ã£o"    ~ "Conhecimento e inovação",
      .default                    = eixo_estrategico
    ),
    across(c(iniciativa_estrategica, produto), ~ str_replace_all(.x, dicionario_siglas))
    
    # latitude  = as.numeric(latitude),
    # longitude = as.numeric(longitude)
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

caminho_demo <- if (file.exists(file.path(DIR_OUT_AURORA, "df_demografia_pe.rds"))) file.path(DIR_OUT_AURORA, "df_demografia_pe.rds") else file.path(DIR_RAW_AURORA, "df_demografia_pe.rds")

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
# 7. EXPORTAÇÕES DE ARQUIVOS (RDS, GEOJSON, YML, DRIVE)
# ------------------------------------------------------------------------------

# 7.1 Exportação para arquivos RDS locais e do App
write_rds(
  shared_iniciativas %>% distinct(secretaria, secretaria_unidade) %>% arrange(secretaria_unidade),
  file.path(DIR_OUT_AURORA, "secretarias_unidades.rds")
)

write_rds(base_aurora, file.path(DIR_OUT_AURORA, "iniciativas.rds"))
write_rds(shared_iniciativas, file.path(DIR_OUT_AURORA, "shared_iniciativas.rds"))

col_ibge <- if ("ibge6" %in% names(shared_iniciativas)) "ibge6" else "ibge7"

dir.create(file.path(DIR_OUT_AURORA, "parquet"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_OUT_AURORA, "geojson"), recursive = TRUE, showWarnings = FALSE)

shared_iniciativas %>%
  sf::st_drop_geometry() %>%
  mutate(
    ibge6 = substr(as.character(.data[[col_ibge]]), 1, 6)
  ) %>%
arrow::write_parquet(file.path(DIR_OUT_AURORA, "parquet/df_iniciativas.parquet"))

if (exists("geometrias_obras") && !is.null(geometrias_obras)) {
  write_rds(geometrias_obras, file.path(DIR_OUT_AURORA, "geometrias_obras.rds"))
}

# 7.2 Exportação de GeoJSONs
write_lines(base_aurora_geojson, file.path(DIR_OUT_AURORA, "iniciativas.geojson"))

if (exists("seduh_sehab_shp_poligono_3")) {
  geojsonsf::sf_geojson(seduh_sehab_shp_poligono_3) %>% write_lines(file.path(DIR_OUT_AURORA, "geojson/df_reforma_no_lar.geojson"))
}
if (!is.null(geometrias_obras)) {
  geojsonsf::sf_geojson(geometrias_obras) %>% write_lines(file.path(DIR_OUT_AURORA, "geojson/geometrias_obras.geojson"))
}
if (!is.null(geometrias_projetos)) {
  geojsonsf::sf_geojson(geometrias_projetos) %>% write_lines(file.path(DIR_OUT_AURORA, "geojson/geometrias_projetos.geojson"))
}

# 7.3 Atualização de Metadata do config.yml
config_yml_path <- file.path(DIR_OUT_AURORA, "config.yml")
if (!file.exists(config_yml_path) && file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) {
  file.copy(file.path(DIR_RAW_AURORA, "config.yml"), config_yml_path)
}
if (file.exists(config_yml_path)) {
  config_file <- readr::read_lines(config_yml_path)
  config_file[4] <- glue::glue("  update: \"{ format(Sys.time(), '%Y/%m/%d %H:%M') }\"")
  write_lines(config_file, config_yml_path, append = FALSE)
  cli_alert_success("Metadata de atualização gravada no config.yml com sucesso!")
}

# 7.4 Upload do Relatório Consolidado para o Google Drive
caminho_temp_upload <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(shared_iniciativas, caminho_temp_upload)

googledrive::drive_upload(
  media     = caminho_temp_upload,
  path      = as_id("1nc_x9z1sX6pUXKNBBku9hXCImjr5fKn4"),
  name      = "Iniciativas de Pernambuco",
  type      = "spreadsheet",
  overwrite = TRUE
)

shared_iniciativas %>%
  select(
    secretaria, secretaria_unidade, eixo_estrategico, iniciativa_estrategica, produto
  ) %>%
  writexl::write_xlsx(
    'data/obras/iniciativas_aurora.xlsx'
  )

cli_alert_success("Base 'shared_iniciativas' enviada para o Google Drive com sucesso!")


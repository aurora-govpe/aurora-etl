# ==============================================================================
# SCRIPT 02: TRATAMENTO E PROCESSAMENTO DE CONVÊNIOS - SECULT E SEE
# Objetivo: Importação e padronização dos dados de convênios das secretarias
#           de Cultura (SECULT) e Educação (SEE) provenientes de Excel e Google Sheets.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SECULT (SECRETARIA DE CULTURA)
# ------------------------------------------------------------------------------
# secult_file_path <- file.path(DIR_ETL, "data/secult/convênios_municípios_secult.xlsx")
# 
# if (!file.exists(secult_file_path)) {
#   cli_abort("Arquivo de convênios da SECULT não encontrado: {.path {secult_file_path}}")
# }


url_dados_secult <- "https://docs.google.com/spreadsheets/d/17eD-4yesZGL6Kcy9eOWjQTMgnh_ywkHQD99tW_xqEFQ/edit?gid=1221686350#gid=1221686350"

# 2.2 Importação da Planilha Google Sheets
df_secult_raw <- read_sheet(
  ss    = url_dados_secult,
  sheet = "CONVÊNIOS - 2026", 
  skip = 2
) %>%
  clean_names()

dados_convenios_secult <- df_secult_raw %>%
  mutate(
    # Normalização dos Municípios (remove acentos e espaços extras)
    municipio = trimws(
      gsub(
        "\\bDE\\b",
        "",
        sub("^MUNIC[ÍI]PIO(\\s+DE)?\\s*", "", convenente_2, ignore.case = TRUE),
        ignore.case = TRUE
      )
    ),
    eixo_estrategico       = "DESENVOLVIMENTO SUSTENTÁVEL",
    iniciativa_estrategica = "CONVÊNIOS COM MUNICÍPIOS",
    secretaria             = "SECULT",
    secretaria_unidade     = "SECULT",
    objeto                = objeto_4,
    produto = objeto,
    no_contrato = no_do_instrumento_1,
    # Valores e Execução
    # valor_total            = as.character(valor_govpe),
    # valor_pago             = as.character(valor_pago),
    # percent_execucao       = as.character(percent_execucao_fisica),
    valor_total            = valor_repassado_secult_5,
    valor_pago             = valor_repassado_secult_5,
    percent_execucao       = valor_total/valor_pago,
    valor_total            = as.character(valor_total),
    valor_pago             = as.character(valor_pago),
    valor_total_convenio = valor_global_7,
    
    # Datas (Garante parsing correto para formato DD/MM/YYYY)
    data_de_inicio = as.Date(
      sapply(vigencia_inicial_10, function(x) {
        if (length(x) == 0 || is.na(x)) return(NA)
        sub(" UTC$", "", x)
      }),
      format = "%Y-%m-%d"
    ),
    data_de_termino = as.Date(
      sapply(vigencia_final_11, function(x) {
        if (length(x) == 0 || is.na(x)) return(NA)
        sub(" UTC$", "", x)
      }),
      format = "%Y-%m-%d"
    ),
    
    # Atributos Complementares
    latitude               = NA_character_,
    longitude              = NA_character_,
    local                  = NA_character_,
    observacoes            = NA_character_,
    endereco_target        = NA_character_,
    fonte                  = "SECULT", 
    subacao = subacao_cod, 
    acao = NA_character_  
    ) %>%
  mutate(
    status = case_when(
      Sys.Date() < data_de_inicio ~ "Em Execução",
      Sys.Date() > data_de_termino ~ "Concluída",
      TRUE ~ "Em Execução"
    )
  ) %>%
  filter(!is.na(no_1)) %>%
  select(-any_of("valor_total_do_convenio"))

cli_alert_success("Convênios da SECULT processados com sucesso ({nrow(dados_convenios_secult)} registros).")
glimpse(dados_convenios_secult)


# ------------------------------------------------------------------------------
# 2. SEE (SECRETARIA DE EDUCAÇÃO)
# ------------------------------------------------------------------------------

# 2.1 Autenticação Google Sheets
googlesheets_token_path <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(googlesheets_token_path)) {
  gs4_auth(
    path   = googlesheets_token_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
} else {
  cli_alert_warning("Credencial do Google Sheets não encontrada em: {.path {googlesheets_token_path}}")
}

url_dados_convenios_educacao <- "https://docs.google.com/spreadsheets/d/18E5lQtemh7-iepWQlWP7AJUAWaPAaogFGaPzH7Ml-mo/edit?gid=1878243986#gid=1878243986"

# 2.2 Importação da Planilha Google Sheets
dados_convenios_educacao_raw <- read_sheet(
  ss    = url_dados_convenios_educacao,
  sheet = "PAINEL COM REPASSE",
  skip  = 4
) %>%
  clean_names() %>%
  filter(!is.na(municipio))

# 2.3 Processamento e Padronização
dados_convenios_educacao <- dados_convenios_educacao_raw %>%
  # Trata colunas do tipo 'list' que o read_sheet pode retornar para células vazias/mistas
  mutate(
    across(
      where(is.list),
      ~ map_chr(.x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x))
    )
  ) %>%
  
  # Conversão adequada das colunas de aditivos para Datetime/Date
  mutate(
    across(
      any_of(c("inicio", "termino", "x1o_aditivo_inicio", "x1o_aditivo_termino", 
               "x2o_aditivo_termino", "x3o_aditivo_termino")),
      ~ suppressWarnings(as_datetime(.x))
    )
  ) %>%
  
  # Cálculo da Maior Data de Término considerando os aditivos válidos
  mutate(
    data_de_inicio_dt  = inicio,
    data_de_termino_dt = pmax(
      termino,
      x1o_aditivo_termino,
      x2o_aditivo_termino,
      x3o_aditivo_termino,
      na.rm = TRUE
    ),
    
    # Conversão de Valores Financeiros
    valor_total_num = suppressWarnings(as.numeric(parte_estadual)),
    valor_pago_num  = suppressWarnings(as.numeric(valor_38)),
    
    # Cálculo do Percentual Financeiro de Execução
    percent_num = case_when(
      is.na(valor_total_num) | valor_total_num == 0 ~ 0,
      TRUE ~ valor_pago_num / valor_total_num
    ),
    
    # Regra de Negócio para o Status
    status = case_when(
      is.na(data_da_publicacao) ~ "Em formulação",
      percent_num >= 1          ~ "Concluída",
      percent_num > 0           ~ "Em execução",
      TRUE                      ~ "Em formulação"
    ),
    
    # Definição e Padronização dos Campos Finais
    eixo_estrategico       = "CONHECIMENTO E INOVAÇÃO",
    iniciativa_estrategica = "CONVÊNIOS COM MUNICÍPIOS",
    secretaria             = "SEE",
    secretaria_unidade     = "SEE",
    produto                = objeto,
    municipio              = normalize_ascii(municipio),
    
    # Formatação de Datas em Texto (DD/MM/YYYY)
    data_de_inicio         = format(as.Date(data_de_inicio_dt), "%d/%m/%Y"),
    data_de_termino        = format(as.Date(data_de_termino_dt), "%d/%m/%Y"),
    
    # Formatação de Numéricos em Texto
    percent_execucao       = as.character(round(percent_num, 4)),
    valor_total            = as.character(valor_total_num),
    valor_pago             = as.character(valor_pago_num),
    
    # Colunas Nulas Padrão
    latitude               = NA_character_,
    longitude              = NA_character_,
    local                  = NA_character_,
    observacoes            = NA_character_,
    endereco_target        = NA_character_,
    indicador              = NA_character_,
    indicador_nome         = NA_character_,
    indicador_secundario   = NA_character_,
    executor               = NA_character_,
    fonte                  = "SEE"
  ) %>%
  
  # Remove atributos temporários de apoio
  select(
    -any_of(c("data_de_inicio_dt", "data_de_termino_dt", 
              "valor_total_num", "valor_pago_num", "percent_num"))
  ) %>% 
  mutate(acao = acao, 
         subacao = subacao)

url_dados_onibus <- "https://docs.google.com/spreadsheets/d/1ddJpe3vTVUoIGNThNNE5lSS3tJ6K5upTmMLiTrsSqtc/edit?gid=41250456#gid=41250456"

df_onibus <- read_sheet(
  ss = url_dados_onibus,
  sheet = "Ônibus"
) %>%
  janitor::clean_names()

df_transporte_final <- df_onibus %>%
  mutate(
    # 1. Limpeza do Valor (R$ 485.000,00 -> 485000.00)
    valor_limpo = valor_veiculo %>%
      stringr::str_remove_all("R\\$\\s*") %>%
      stringr::str_replace_all("\\.", "") %>%
      stringr::str_replace(",", ".") %>%
      as.numeric()
  ) %>%
  # 2. Agrupar por Município e RD
  group_by(
    cod_ibge = codigo_ibge
  ) %>%
  filter(
    !is.na(codigo_ibge)
  ) %>%
  summarise(
    qtd_onibus = sum(stringr::str_detect(tipo_de_veiculo, stringr::regex("ÔNIBUS", ignore_case = TRUE)), na.rm = TRUE),
    qtd_vans   = sum(stringr::str_detect(tipo_de_veiculo, stringr::regex("VAN", ignore_case = TRUE)), na.rm = TRUE),
    valor_total = sum(valor_limpo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3. Criar a coluna 'produto' no padrão para o Regex do JS pescar
  mutate(
    produto = case_when(
      qtd_onibus > 0 & qtd_vans > 0 ~ paste0(qtd_onibus, " ônibus e ", qtd_vans, " vans"),
      qtd_onibus > 0 ~ paste0(qtd_onibus, " ônibus"),
      qtd_vans > 0   ~ paste0(qtd_vans, " vans"),
      TRUE ~ "0 veículos"
    )
  ) %>% 
  left_join(
    shp_pe_mun_limites %>%
      st_drop_geometry() %>%
      select(ibge7, municipio, regiao_de_desenvolvimento) %>%
      mutate(ibge7 = as.integer(ibge7)),
    by = c('cod_ibge' = 'ibge7')
  ) %>%
  mutate(
    eixo_estrategico = 'CONHECIMENTO E INOVAÇÃO',
    iniciativa_estrategica = 'AQUISIÇÃO DE VANS E ÔNIBUS ESCOLARES',
    secretaria = 'SEE',
    status = 'CONCLUÍDA',
    data_de_inicio = NA_character_,
    data_de_termino = NA_character_,
    percent_execucao = '100.0',
    municipio = toupper(municipio),
    latitude = NA_character_,
    longitude = NA_character_,
    local = NA_character_,
    observacoes = NA_character_,
    endereco_target = NA_character_,
    valor_total = as.character(valor_total),
    fonte = "SEE"
  ) 


cli_alert_success("Convênios da Educação (SEE) processados com sucesso ({nrow(dados_convenios_educacao)} registros).")
glimpse(dados_convenios_educacao)
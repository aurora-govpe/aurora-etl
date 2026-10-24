# ==============================================================================
# SCRIPT 01: TRATAMENTO E PROCESSAMENTO DE DADOS - COMPESA
# Objetivo: Leitura do plano de investimentos da COMPESA, mapeamento/rateio de
#           valores por municípios, padronização de campos e status.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LEITURA DOS DADOS BRUTOS DA COMPESA E TABELA DE TRADUÇÃO
# ------------------------------------------------------------------------------
# Leitura do plano de investimentos mais recente da COMPESA
compesa_file_path <- file.path(DIR_RAW_AURORA, "compesa/Plano de Investimentos - 29jul26 SAESPRI.xlsx")

if (!file.exists(compesa_file_path)) {
  cli_abort("Arquivo do plano de investimentos não localizado: {.path {compesa_file_path}}")
}

dados_compesa_raw <- read_excel(compesa_file_path) %>%
  clean_names() %>%
  filter(!is.na(nome_da_acao))

# Leitura da tabela auxiliar de de-para / mapeamento de municípios
muni_aux_path <- file.path(DIR_RAW_AURORA, "compesa/lista_compesa_municipios.xlsx")

if (!file.exists(muni_aux_path)) {
  cli_abort("Arquivo de mapeamento de municípios da COMPESA não encontrado: {.path {muni_aux_path}}")
}

base_auxiliar_municipios <- read_excel(muni_aux_path)

# ------------------------------------------------------------------------------
# 2. VERIFICAÇÃO E AUDITORIA DE MUNICÍPIOS FALTANTES (LOG/DIAGNÓSTICO)
# ------------------------------------------------------------------------------
# Identifica quais registros possuem grafias de municípios não cobertas na base auxiliar
dados_compesa_muni_faltantes <- dados_compesa_raw %>%
  left_join(base_auxiliar_municipios, by = "municipios_beneficiados") %>%
  filter(is.na(municipios_clean)) %>%
  select(municipios_beneficiados, municipios_clean) %>%
  distinct()

# Exporta arquivo para revisão manual se houver divergências
if (nrow(dados_compesa_muni_faltantes) > 0) {
  cli_alert_warning("Existem {nrow(dados_compesa_muni_faltantes)} combinações de municípios sem mapeamento!")
  writexl::write_xlsx(
    dados_compesa_muni_faltantes, 
    file.path(DIR_RAW_AURORA, "compesa/muni_faltante.xlsx")
  )
} else {
  cli_alert_success("Todos os municípios foram mapeados com sucesso!")
}

# ------------------------------------------------------------------------------
# 3. TRATAMENTO, RATEIO E PADRONIZAÇÃO DOS DADOS
# ------------------------------------------------------------------------------
dados_compesa_processed <- dados_compesa_raw %>%
  left_join(base_auxiliar_municipios, by = "municipios_beneficiados") %>%
  
  # --- Passo A: Rateio de Valor Financeiro entre Municípios ---
  mutate(
    municipio = municipios_clean,
    # Garante parsing numérico mesmo se a coluna vier formatada como texto
    valor_bruto = suppressWarnings(parse_number(as.character(valor_divulgado_r))),
    
    # Conta quantidade de municípios concatenados por vírgula
    qtd_municipios = str_count(municipios_clean, ",") + 1,
    qtd_municipios = if_else(is.na(qtd_municipios) | qtd_municipios == 0, 1L, as.integer(qtd_municipios)),
    
    # Rateio proporcional igualitário
    valor_total_rateado = round(valor_bruto / qtd_municipios)
  ) %>%
  
  # --- Passo B: Parsing de Datas Flexível ---
  mutate(
    data_de_inicio_parsed  = parse_date_time(data_de_inicio, orders = c("ymd", "dmy", "YmdHMS", "dmY")),
    termino_previsto_parsed = parse_date_time(termino_previsto, orders = c("ymd", "dmy", "YmdHMS", "dmY"))
  ) %>%
  
  # --- Passo C: Desmembramento de Linhas Multi-município ---
  separate_rows(municipio, sep = ",\\s*") %>%
  mutate(municipio = str_squish(municipio)) %>%
  filter(!is.na(municipio) & municipio != "") %>%
  
  # --- Passo D: Normalização de Atributos e Categorias ---
  mutate(
    eixo_estrategico       = "DESENVOLVIMENTO SUSTENTÁVEL",
    iniciativa_estrategica = paste0(eixo, " - ", subeixo),
    secretaria             = "SRHS",
    produto                = nome_da_acao,
    
    # Padronização de domínios do Status
    status = case_when(
      str_to_lower(status) %in% c("a contratar", "a iniciar", "a fazer") ~ "A iniciar",
      str_to_lower(status) %in% c("a elaborar projeto", "em elaboração de projeto", "projeto em elaboração") ~ "Em elaboração de projeto",
      str_to_lower(status) %in% c("a licitar") ~ "A licitar",
      str_to_lower(status) %in% c("a retomar", "paralisada") ~ "Paralisada",
      str_to_lower(status) %in% c("concluída", "concluído") ~ "Concluída",
      str_to_lower(status) %in% c("em andamento", "em execução") ~ "Em execução",
      str_to_lower(status) %in% c("em licitação") ~ "Em licitação",
      is.na(status) ~ "A iniciar",
      TRUE ~ status
    ),
    
    # Formatação Final de Campos para exportação/compatibilidade
    valor_total          = as.character(valor_total_rateado),
    data_de_inicio       = format(data_de_inicio_parsed, "%d/%m/%Y"),
    data_de_termino      = format(termino_previsto_parsed, "%d/%m/%Y"),
    percent_execucao     = as.character(percent_execucao_realizado),
    latitude             = NA_character_,
    longitude            = NA_character_,
    local                = NA_character_,
    observacoes          = NA_character_,
    endereco_target      = NA_character_,
    fonte                = "COMPESA",
    secretaria_unidade   = "COMPESA",
    indicador_secundario = as.character(populacao_beneficiada)
  ) %>%
  # Limpeza de colunas intermediárias
  select(
    -c(municipios_clean, valor_divulgado_r, valor_bruto, qtd_municipios, 
       valor_total_rateado, data_de_inicio_parsed, termino_previsto_parsed)
  )

# Visualização de verificação
glimpse(dados_compesa_processed)

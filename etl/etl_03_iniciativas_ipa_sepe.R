# ==============================================================================
# SCRIPT 03: TRATAMENTO E PROCESSAMENTO DE DADOS - IPA E SEPE
# Objetivo: Download e estruturação das bases de metas do IPA (download via 
#           Google Drive) e projetos de engenharia da SEPE (Google Sheets).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. IPA (INSTITUTO DE INOVAÇÃO AGROPECUÁRIA DE PERNAMBUCO)
# ------------------------------------------------------------------------------

# 1.1 Verificação do objeto auxiliar de downloads (prevenção de falha por variável inexistente)
if (!exists("planilhas_locais")) {
  cli_abort("Objeto {.var planilhas_locais} não localizado na memória. Certifique-se de carregar o script de listagem do Google Drive prévio.")
}

# 1.2 Identificação do ID da planilha no Google Drive
ipa_id <- planilhas_locais %>% 
  filter(str_detect(name, "ipa_base_geral_aurora")) %>% 
  pluck("id", 1)

if (is.null(ipa_id) || is.na(ipa_id)) {
  cli_abort("Arquivo 'ipa_base_geral_aurora' não encontrado no mapeamento de planilhas do Google Drive!")
}

# 1.3 Download do arquivo Excel do Google Drive
ipa_file_source <- file.path(
  DIR_RAW_AURORA, 
 
  glue::glue("ipa_base_geral_aurora_{format(datatime_file_source, '%Y_%m_%d_T%Hh%Mm')}.xlsx")
)

drive_download(ipa_id, path = ipa_file_source, overwrite = TRUE)

# 1.4 Processamento da Aba: Meta com Produto Único
ipa_df_uni <- readxl::read_excel(
  ipa_file_source, 
  sheet = "Meta com Produto Único",
  col_types = "text"
) %>%
  clean_names() %>% 
  select(!starts_with("x")) %>%
  mutate(
    secretaria_unidade = secretaria,
    secretaria         = str_remove(secretaria, " - .*"),
    fonte              = "IPA"
  )

# Garante existência da coluna produto se não for mapeada na planilha de origem
if (!"produto" %in% names(ipa_df_uni)) {
  ipa_df_uni <- ipa_df_uni %>% 
    mutate(produto = iniciativa_estrategica)
}

# 1.5 Processamento da Aba: Meta com Vários Produtos
ipa_df_multi <- readxl::read_excel(
  ipa_file_source, 
  sheet = "Meta com Vários Produtos",
  col_types = "text"
) %>%
  clean_names() %>% 
  select(!starts_with("x")) %>%
  mutate(
    secretaria_unidade = secretaria,
    secretaria         = str_remove(secretaria, " - .*"),
    fonte              = "IPA"
  )

cli_alert_success("Bases do IPA baixadas e carregadas com sucesso.")


# ------------------------------------------------------------------------------
# 2. SEPE (SECRETARIA EXECUTIVA DE PROJETOS EESTRATÉGICOS)
# ------------------------------------------------------------------------------

# 2.1 Autenticação Google Sheets
googlesheets_token_path <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(googlesheets_token_path)) {
  gs4_auth(
    path   = googlesheets_token_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
} else {
  cli_alert_warning("Credencial do Google Sheets não localizada em: {.path {googlesheets_token_path}}")
}

url_dados_sepe <- "https://docs.google.com/spreadsheets/d/13vwP-s5nAje9ZAI4TSYKliBWry1-HE8_taKYjtx3la8/edit?gid=2004918492#gid=2004918492"

# 2.2 Importação da Planilha Google Sheets
df_sepe_raw <- read_sheet(
  ss    = url_dados_sepe,
  sheet = "Carteira SEPE-CEHAB"
) %>%
  clean_names()

# 2.3 Tratamento e Normalização das Informações
df_sepe <- df_sepe_raw %>%
  # Trata células compostas por listas/nulos vindos do read_sheet
  mutate(
    across(
      where(is.list),
      ~ map_chr(.x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x))
    )
  ) %>%
  
  # Filtro inicial de processo
  filter(!status_processo %in% c("OS emitida")) %>%
  
  # Mapeamento e Normalização do Órgão/Secretaria
  mutate(
    secretaria = str_squish(secretaria),
    secretaria = case_when(
      secretaria %in% c("ADEPE") ~ "SDEC",
      secretaria %in% c("DER") ~ "SEMOBI",
      secretaria %in% c("FAB") ~ "EXTERNO",
      secretaria %in% c("IPA") ~ "SEAGRI",
      secretaria %in% c("PALÁCIO") ~ "PALÁCIO",
      secretaria %in% c("SEE", "EDUCAÇÃO", "SAS/SEE") ~ "SEE",
      secretaria %in% c("SDS", "SDS - PC", "SDS - PM", "SDS - CB", "SDS - PM / SES") ~ "SDS",
      secretaria == "SECTI/SES" ~ "SECTI",
      secretaria == "NA" | is.na(secretaria) ~ "Não Informado",
      TRUE ~ secretaria
    ),
    
    # Classificação do Eixo Estratégico
    eixo_estrategico = case_when(
      secretaria %in% c("SECTI", "SEE") ~ "Conhecimento e inovação",
      secretaria %in% c("SES") ~ "Saúde e qualidade de vida",
      secretaria %in% c("SDS", "SEAP") ~ "Segurança e cidadania",
      TRUE ~ "Desenvolvimento sustentável"
    ),
    
    # Normalização de Status de Engenharia / Projeto
    status = case_when(
      status_processo %in% c("Enviado à CEHAB", "OS emitida", "Publicado") ~ "Concluída",
      status_processo %in% c("A iniciar", "Em análise", "Em revisão") ~ "A iniciar",
      status_processo %in% c("Em elaboração do projeto") ~ "Em execução",
      TRUE ~ "A iniciar"
    ),
    
    # Padronização da Iniciativa Estratégica (Ex: Creches)
    iniciativa_estrategica = case_when(
      str_detect(iniciativa_estrategica, regex("Creche", ignore_case = TRUE)) ~ "CRIAÇÃO DE 60 MIL VAGAS EM CRECHES E PRÉ-ESCOLA",
      TRUE ~ iniciativa_estrategica
    )
  ) %>%
  
  # Regra de Negócio: Exclui creches com OS emitida ou Publicado para evitar dupla contagem com outras bases
  filter(
    !(iniciativa_estrategica == "CRIAÇÃO DE 60 MIL VAGAS EM CRECHES E PRÉ-ESCOLA" & 
        status_processo %in% c("OS emitida", "Publicado"))
  ) %>%
  
  # Construção dos Campos Finais de Interface
  mutate(
    # municipio          = normalize_ascii(municipio),
    data_de_inicio     = format(as.Date(inicio_real), "%d/%m/%Y"),
    data_de_termino    = format(as.Date(fim_real), "%d/%m/%Y"),
    percent_execucao   = NA_character_,
    valor_total        = as.character(valor_estimado),
    local              = NA_character_,
    observacoes        = NA_character_,
    endereco_target    = NA_character_,
    secretaria_unidade = "SEPE",
    produto            = paste0("Projeto - ", iniciativa_estrategica, " - ", municipio),
    fonte              = "SEPE"
  )

cli_alert_success("Dados da SEPE processados com sucesso ({nrow(df_sepe)} registros).")
glimpse(df_sepe)
# ==============================================================================
# SCRIPT 00: CONFIGURAÇÕES GLOBAIS E INICIALIZAÇÃO DE AMBIENTE (ETL AURORA)
# Objetivo: Carregar bibliotecas, definir variáveis de ambiente/diretórios,
#           funções auxiliares, esquemas de dados e efetuar autenticações de API.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(BigDataPE)
library(bslib)
library(cli)
library(fs)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(purrr)
library(readxl)
library(rmapshaper)
library(sf)
library(stringi)
library(stringr)
library(tidyverse)
library(vialactea)

# ------------------------------------------------------------------------------
# 2. AMBIENTE E DIRETÓRIOS DINÂMICOS
# ------------------------------------------------------------------------------
# Evita notação científica na exibição de números grandes (ex: códigos IBGE)
options(scipen = 999)

# Validação do Sistema Operacional
is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  # Configuração de encoding/locale para tratamento correto de acentuação no Linux
  Sys.setlocale(locale = "pt_BR.utf8")
}

# Definição dos diretórios base via paths.R com busca hierárquica (fallback)
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
}

DIR_ETL               <- getwd()
DIR_DADOS             <- DIR_OUT_AURORA
DIR_DADOS_AURORA_MUNI <- DIR_OUT_AURORA
DIR_MULUNGU           <- ifelse(is_linux, "/srv/dados/mulungu", file.path(getwd(), "../Mulungu/app_mulungu/data"))

# ------------------------------------------------------------------------------
# 3. FUNÇÕES AUXILIARES (UTILS)
# ------------------------------------------------------------------------------
#' Normaliza strings removendo acentos, convertendo para caixa alta e removendo espaços extras
#' @param x Vetor de caracteres
#' @return Vetor normalizado
normalize_ascii <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# ------------------------------------------------------------------------------
# 4. LEITURA DE CONFIGURAÇÕES E TOKENS
# ------------------------------------------------------------------------------
# Carrega script utilitário complementar com resolução dinâmica
utils_path <- if (file.exists("R/utils.R")) "R/utils.R" else if (file.exists("../R/utils.R")) "../R/utils.R" else file.path(DIR_ETL, "R/utils.R")
if (file.exists(utils_path)) source(utils_path)

# Resolução dinâmica do config.yml priorizando DIR_RAW_AURORA
config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) {
  file.path(DIR_RAW_AURORA, "config.yml")
} else {
  file.path(DIR_ETL, "config.yml")
}

# Validação da existência do arquivo YAML de configuração
if (!file.exists(config_path)) {
  cli_abort("Arquivo de configuração não encontrado no caminho: {.path {config_path}}")
}

# Leitura das chaves do arquivo YAML
config_tokens <- config::get("tokens", file = config_path)
cli_alert_success("Arquivo {.file config.yml} lido com sucesso!")

# Validação de token crítico de integração
if (is.null(config_tokens$maespe_beneficiarias_municipio)) {
  cli_alert_warning("Atenção: O token 'maespe_beneficiarias_municipio' está NULL ou não existe no YAML!")
} else {
  cli_alert_info("Token 'maespe_beneficiarias_municipio' carregado com sucesso na memória.")
}

# ------------------------------------------------------------------------------
# 5. CONSTANTES, DICIONÁRIOS E DOMÍNIOS VÁLIDOS
# ------------------------------------------------------------------------------
valid_status <- c(
  "Em formulação", "Em elaboração de projeto", "Concluída", "A licitar",
  "Em licitação", "A iniciar", "Em execução", "Paralisada", "Cancelada"
)

valid_eixo <- c(
  "Conhecimento e inovação", "Desenvolvimento sustentável",
  "Saúde e qualidade de vida", "Segurança e cidadania"
)

# Colunas padrão utilizadas na etapa de processamento
colunas_processamento <- c(
  "eixo_estrategico", "iniciativa_estrategica", "no_contrato", "tipologia",
  "secretaria", "status", "data_de_inicio", "data_de_termino",
  "percent_execucao", "valor_total", "municipio", "latitude", "longitude",
  "local", "observacoes", "endereco_target", "produto", "indicador",
  "indicador_nome", "indicador_secundario", "executor", "latitude", "longitude", "acao", "subacao"
)

# Colunas padrão exigidas pela estrutura final do app Aurora
colunas_aurora <- c(
  "id", "id_produto", "eixo_estrategico", "secretaria", "secretaria_unidade",
  "status", "percent_execucao", "indicador", "indicador_nome",
  "iniciativa_estrategica", "no_contrato", "tipologia", "produto", "ibge7",
  "municipio", "regiao_de_desenvolvimento", "local", "valor_total", "valor_pago",
  "avanco_cor", "data_de_inicio", "data_de_termino", "fator_de_divisao_municipio",
  "fonte", "indicador_secundario", "executor", "latitude", "longitude", "acao", "subacao"
)

# Mapeamento de siglas hospitalares padrão
dicionario_siglas <- c(
  "\\bHAM\\b" = "HOSPITAL AGAMENON MAGALHÃES - HAM",
  "\\bHGV\\b" = "HOSPITAL GETÚLIO VARGAS - HGV",
  "\\bHR\\b"  = "HOSPITAL DA RESTAURAÇÃO - HR",
  "\\bHBL\\b" = "HOSPITAL BARÃO DE LUCENA - HBL"
)

# ------------------------------------------------------------------------------
# 6. INICIALIZAÇÃO DE OBJETOS VAZIOS / PLACEHOLDERS
# ------------------------------------------------------------------------------
cehab_df_uni         <- NULL
cehab_df_multi       <- NULL
seduh_df_uni         <- NULL
seduh_df_multi       <- NULL
sepe_df_uni          <- NULL
sepe_df_multi        <- NULL
seduh_sehab_df_multi <- NULL
perpart_df_multi     <- NULL

# ------------------------------------------------------------------------------
# 7. AUTENTICAÇÕES GLOBAIS DE APIS E SERVIÇOS (GOOGLE & BIGDATAPE)
# ------------------------------------------------------------------------------

# 7.1. Credencial Principal do Google (Service Account ECO-CONDITION)
google_token_eco <- file.path(DIR_RAW_AURORA, "token_eco-condition-406519-adac7b9fb8e5.json")

if (file.exists(google_token_eco)) {
  # Autentica no Google Drive e Google Sheets para o projeto Eco-Condition
  googledrive::drive_auth(cache = FALSE, path = google_token_eco)
  cli_alert_success("Autenticação Google Drive (Eco-Condition) realizada com sucesso.")
} else {
  cli_alert_warning("Credencial Eco-Condition não localizada em: {.path {google_token_eco}}")
}

# 7.2. Credencial Secundária do Google (Conta Hugo - Responsável por bases restritas)
google_token_hugo <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(google_token_hugo)) {
  # Autentica no Google Sheets para a conta do Hugo
  googlesheets4::gs4_auth(
    path   = google_token_hugo,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
  cli_alert_success("Autenticação Google Sheets (Hugo) realizada com sucesso.")
} else {
  cli_alert_warning("Credencial do Hugo não localizada em: {.path {google_token_hugo}}")
}

# 7.3. Armazenamento do Token Interno (BigDataPE)
if (!is.null(config_tokens$maespe_beneficiarias_municipio)) {
  bdpe_store_token(
    "maespe_beneficiarias_municipio",
    config_tokens$maespe_beneficiarias_municipio
  )
}

# ------------------------------------------------------------------------------
# 8. EXTRAÇÃO E CARREGAMENTO DE BASES DE SUPORTE
# ------------------------------------------------------------------------------
# Carrega esquemas de dados externos com resolução dinâmica
esquemas_path <- if (file.exists("R/esquemas_de_dados.R")) "R/esquemas_de_dados.R" else if (file.exists("../R/esquemas_de_dados.R")) "../R/esquemas_de_dados.R" else file.path(DIR_ETL, "R/esquemas_de_dados.R")
if (file.exists(esquemas_path)) {
  source(esquemas_path)
} else {
  cli_alert_danger("Arquivo {.file esquemas_de_dados.R} não localizado!")
}

# Carregamento do arquivo de fronteiras geográficas (Shapefile/sf object)
sf_path <- file.path(DIR_RAW_AURORA, "pernambuco_sf.rds")

if (file.exists(sf_path)) {
  pe_sf <- read_rds(sf_path)
  
  # Cria base de limites com nome do município padronizado
  shp_pe_mun_limites <- pe_sf %>%
    clean_names() %>%
    mutate(aux_municipio = normalize_ascii(municipio))
} else {
  cli_abort("Arquivo de limites geográficos não encontrado: {.path {sf_path}}")
}

planilhas_locais <- drive_ls(pattern = "_base_geral_aurora")

# Base com os ícones do Mapbox
icones_path <- file.path(DIR_RAW_AURORA, "mapbox/base_aurora_icones.xlsx")

if (file.exists(icones_path)) {
  base_aurora_icones <- readxl::read_xlsx(icones_path) %>%
    # Remove coluna desnecessária de conferência, se existir
    select(-any_of(c("conferência", "conferencia"))) %>%
    mutate(iniciativa_estrategica = str_squish(iniciativa_estrategica))
} else {
  cli_alert_warning("Arquivo de ícones do Mapbox não encontrado: {.path {icones_path}}")
}

# Registro da data e hora da execução
datatime_file_source <- Sys.time()
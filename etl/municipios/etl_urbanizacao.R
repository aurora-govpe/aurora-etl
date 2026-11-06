# ==============================================================================
# SCRIPT: etl_urbanizacao.R
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(sidrar)
library(janitor)
library(dplyr)
library(stringr)
library(googledrive)
library(googlesheets4)
library(cli)

# Validação do Sistema Operacional
is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  # Configuração de encoding/locale para tratamento correto de acentuação no Linux
  Sys.setlocale(locale = "pt_BR.utf8")
}

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
options(scipen = 999)

# Busca hierárquica pelo paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_DADOS"))      DIR_DADOS      <- DIR_OUT_AURORA

# Compatibilidade com diretório de municípios (se não definido pelo paths.R)
if (!exists("DIR_DADOS_AURORA_MUNI")) {
  DIR_DADOS_AURORA_MUNI <- ifelse(
    is_linux, 
    "/srv/dados/aurora", 
    file.path(DIR_ETL, "app_aurora_municipios", "data")
  )
}

# Autenticação Google Drive / Sheets ancorada em DIR_RAW_AURORA
google_token_hugo <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(google_token_hugo)) {
  drive_auth(cache = FALSE, path = google_token_hugo)
  gs4_auth(path = google_token_hugo, scopes = "https://www.googleapis.com/auth/spreadsheets")
  cli_alert_success("Autenticação Google realizada com sucesso via {.path {google_token_hugo}}.")
} else {
  cli_alert_warning("Credencial do Hugo não encontrada em: {.path {google_token_hugo}}")
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO E PROCESSAMENTO DOS DADOS (SIDRA)
# ------------------------------------------------------------------------------
# Dica: 'info_sidra' costuma falhar se a API web do IBGE oscilar. 
# Recomenda-se comentar em produção:
# info_sidra(9923)

cli_alert_info("Consultando SIDRA (Tabela 9923 - Censo 2022)...")

# Configuração de timeout maior para o R em requisições web
options(timeout = 300)

# Tentativa com get_sidra:
# Na tabela 9923, a variável principal de pessoas é 93. 
# Para garantir que não quebre por URL inválida, baixe a variável 93:
dados_brutos <- tryCatch({
  get_sidra(
    x          = 9923,
    variable   = 93,                     # População residente (pessoas)
    period     = "2022",
    geo        = "City",
    geo.filter = list("State" = 26)      # Pernambuco
  )
}, error = function(e) {
  cli_alert_warning("Falha via get_sidra: {e$message}. Tentando requisição direta via API...")
  
  # Alternativa direta via API REST oficial do SIDRA caso o pacote falhe na conexão
  url_sidra <- "https://apisidra.ibge.gov.br/values/t/9923/n6/in%20n3%2026/v/93/p/2022/c1/all"
  res <- httr::GET(url_sidra, httr::user_agent("Mozilla/5.0 (R ETL Aurora)"))
  
  if (httr::status_code(res) != 200) {
    stop("Erro ao conectar à API do SIDRA. Código: ", httr::status_code(res))
  }
  
  conteudo <- httr::content(res, as = "text", encoding = "UTF-8")
  tab <- jsonlite::fromJSON(conteudo)
  colnames(tab) <- tab[1, ]
  tab <- tab[-1, ]
  as.data.frame(tab)
})

# Tratamento dos dados e cálculo do percentual de urbanização
dados_limpos <- dados_brutos |> 
  clean_names()

# Normalização de colunas dependendo se veio do get_sidra ou direto da API
if (!"situacao_do_domicilio" %in% names(dados_limpos) && "situacao_do_domicilio_codigo" %in% names(dados_limpos)) {
  # Mapeamento caso venha da API crua
  dados_limpos <- dados_limpos |> 
    rename(
      valor = v,
      municipio_codigo = d2n,
      municipio = d2c,
      ano = d3c,
      situacao_do_domicilio = d4n
    )
}

dados_limpos <- dados_limpos |> 
  select(
    valor, 
    municipio_codigo, 
    municipio, 
    ano, 
    situacao_do_domicilio
  ) |> 
  mutate(
    municipio = str_remove(municipio, " - [A-Z]{2}$"),
    valor = as.numeric(valor)
  ) |> 
  filter(
    !is.na(valor),
    situacao_do_domicilio %in% c("Total", "Urbana", "Rural")
  ) |> 
  # Calcula o percentual dividindo a população urbana/rural pelo total municipal
  group_by(municipio_codigo, municipio, ano) |> 
  mutate(
    total_mun = valor[situacao_do_domicilio == "Total"][1],
    indicador = valor / total_mun
  ) |> 
  ungroup() |> 
  filter(situacao_do_domicilio %in% c("Urbana", "Rural")) |> 
  mutate(
    nm_indicador = case_when(
      situacao_do_domicilio == "Urbana" ~ "População urbana",
      situacao_do_domicilio == "Rural"  ~ "População rural"
    ),
    sigla_indicador = case_when(
      situacao_do_domicilio == "Urbana" ~ "pop_urb",
      situacao_do_domicilio == "Rural"  ~ "pop_rur"
    ),
    cod_municipio = municipio_codigo,
    nm_municipio  = municipio
  ) |> 
  select(cod_municipio, nm_municipio, indicador, ano, nm_indicador, sigla_indicador)

cli_alert_success("Dados processados com sucesso! Linhas extraídas: {nrow(dados_limpos)}")



# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO LOCAL E GOOGLE SHEETS
# ------------------------------------------------------------------------------
# Garante que o diretório base existe
if (!dir.exists(DIR_DADOS_AURORA_MUNI)) {
  dir.create(DIR_DADOS_AURORA_MUNI, recursive = TRUE)
}

# --- Exportação Excel ---
caminho_local_pop_muni <- file.path(DIR_DADOS_AURORA_MUNI, "dados_urbanizacao_mun_pe.xlsx")
writexl::write_xlsx(dados_limpos, caminho_local_pop_muni)
cli_alert_success("Arquivo local Excel gravado em: {.path {caminho_local_pop_muni}}")

# --- Exportação RDS ---
caminho_rds_pop_muni <- file.path(DIR_DADOS_AURORA_MUNI, "dados_urbanizacao_mun_pe.rds")
readr::write_rds(dados_limpos, caminho_rds_pop_muni)
cli_alert_success("Arquivo local RDS gravado em: {.path {caminho_rds_pop_muni}}")

# --- Exportação Parquet ---
dir_parquet <- file.path(DIR_DADOS_AURORA_MUNI, "parquet")
if (!dir.exists(dir_parquet)) {
  dir.create(dir_parquet, recursive = TRUE)
}

caminho_parquet_pop_muni <- file.path(dir_parquet, "dados_urbanizacao_mun_pe.parquet")
arrow::write_parquet(dados_limpos, caminho_parquet_pop_muni)
cli_alert_success("Arquivo local Parquet gravado em: {.path {caminho_parquet_pop_muni}}")

# --- Google Sheets ---
sheet_id <- "1eoH5dTmx5xipoV_plL7jIPB5yvQNHx1rOqIk3_UZM9o"

tryCatch({
  googlesheets4::sheet_write(
    data  = dados_limpos,
    ss    = sheet_id,
    sheet = "censo_2022"
  )
  cli_alert_success("Dados gravados no Google Sheets com sucesso na aba {.val censo_2022}!")
}, error = function(e) {
  cli_alert_danger("Falha ao gravar no Google Sheets: {.val {e$message}}")
})
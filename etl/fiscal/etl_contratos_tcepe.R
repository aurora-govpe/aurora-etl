# pacotes ----
library(BigDataPE)
library(data.table)
library(DBI)
library(dplyr)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)
library(purrr)
library(qs)
library(readr)
library(RPostgres)
library(stringr)
library(tceper)

# setup ----
## gd ----
folder_id <- "1YqT6F8X0HR46ChKtPi2JnplAkw4YxM-y"

drive_auth(
  path = "data/r-sheets-hugo-0788cc358cb5.json",
  scopes = "https://www.googleapis.com/auth/drive"
)

## tokens ----
config_path <- file.path("data", "config.yml")

config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'execucao',
  config_tokens$execucao_despesa_para_sepe)

# extrações ----
## contratos ----
## Configurações ----
ano_atual_num <- year(Sys.Date())
ano_atual_str <- as.character(ano_atual_num)
diretorio_data <- "data/efisco/contratos"
caminho_parquet <- file.path(diretorio_data, "dados_tce_contracts.parquet")

# Garante que a pasta existe
if (!dir.exists(diretorio_data)) dir.create(diretorio_data, recursive = TRUE)

## 1. Estratégia de Captura ----
# Garante que a pasta existe
if (!dir.exists(diretorio_data)) dir.create(diretorio_data, recursive = TRUE)

# 1. Flag de Controle (Garante que o script saiba se é Carga Total ou Incremental)
existe_historico <- file.exists(caminho_parquet)

## 2. Estratégia de Captura (Retorna apenas os dados) ----
dados_tce_contracts <- tryCatch(
  {
    if (!existe_historico) {
      cli::cli_alert_warning("PARQUET não encontrado. Iniciando CARGA TOTAL (Histórico)...")
      
      anos_historico <- 2020:ano_atual_num
      
      map_df(anos_historico, function(a) {
        cli::cli_alert_info("Baixando contratos de {a}...")
        tce_contracts(AnoContrato = a)
      }) %>% 
        clean_names()
      
    } else {
      cli::cli_alert_info("PARQUET encontrado. Atualizando ano atual ({ano_atual_str})...")
      
      #historico <- readRDS(caminho_parquet)
      historico <- read_parquet(caminho_parquet)
      
      dados_novos <- tce_contracts(AnoContrato = ano_atual_num) %>% 
        clean_names()
      
      historico %>%
        filter(as.character(ano_contrato) != ano_atual_str) %>%
        bind_rows(dados_novos)
    }
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao processar contratos TCE: {.val {e$message}}")
    #if (file.exists(caminho_parquet)) readRDS(caminho_parquet) else tibble()
    if (file.exists(caminho_parquet)) read_parquet(caminho_parquet) else tibble()
  }
)

## 3. Definição estrita de quais anos enviar para o Drive ----
# Usa a flag definida no início para decidir o que sobe
if (!existe_historico) {
  anos_para_processar <- unique(as.character(dados_tce_contracts$ano_contrato))
} else {
  anos_para_processar <- ano_atual_str
}

## 4. CSV e Google Drive ----
walk(anos_para_processar, function(ano_ref) {
  
  nome_arquivo <- paste0("tce_contracts_", ano_ref, ".csv")
  caminho_local <- file.path(diretorio_data, nome_arquivo)
  
  # Salva o CSV do ano específico
  dados_tce_contracts %>%
    filter(as.character(ano_contrato) == ano_ref) %>%
    write_csv(caminho_local)
  
  # Envia apenas o que está na lista 'anos_para_processar'
  drive_upload(
    caminho_local,
    path = as_id(folder_id),
    name = nome_arquivo,
    overwrite = TRUE
  )
  
  cli::cli_alert_success("Contratos TCE {ano_ref} processado e enviado para o Drive!")
})

dados_tce_contracts %>% glimpse()

dados_tce_contracts %>% distinct(ano_contrato)

## 5. Salva PARQUET Final ----
#write_rds(dados_tce_contracts, caminho_parquet)
write_parquet(dados_tce_contracts, caminho_parquet)
cli::cli_alert_success("PARQUET de contratos TCE consolidado com sucesso!")

dados_tce_contracts %>%
  mutate(
    sigla_ug = str_to_upper(str_trim(sigla_ug)),
    cpf_cnpj = str_replace_all(cpf_cnpj, "\\D", ""),
    ano_contrato = as.integer(ano_contrato)
  ) %>%
  write_parquet('app_aurora_orcamento/data/dados_tce_contracts.parquet')

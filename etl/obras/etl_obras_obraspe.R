# versão jdbc
# pacotes ----
library(arrow)
library(BigDataPE)
library(DBI)
library(glue)
library(janitor)
library(lubridate)
library(readr)
library(stringr)

# utils ----
parse_br <- function(x) {
  parse_number(x, locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# setup ----
options(java.parameters = "-Xmx4g")
library(rJava)
library(RJDBC)

config_path <- file.path("data", "config.yml")

config_bdpe <- config::get('bdpe', file = config_path)

drv <- JDBC(driverClass = "io.trino.jdbc.TrinoDriver", classPath = "data/trino-jdbc-468.jar")

url <- glue("jdbc:trino://{config_bdpe$db_host}:{config_bdpe$db_port}?SSL=true&user={config_bdpe$db_user}&password={config_bdpe$db_pass}")

con <- dbConnect(drv, url)

# dados ----
## obras_obraspe ----
tictoc::tic("tempo_obras_obraspe_jdbc")

obras_obraspe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.obras_obraspe_v01"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup..."
    )
    
    obras_obraspe <- read_parquet("data/obras/obras_obraspe.parquet")
  }
)

obras_obraspe %>% glimpse()

## contratos_obraspe ----
tictoc::tic("tempo_contratos_obraspe_jdbc")

contratos_obraspe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.contratos_obraspe_v01"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup..."
    )
    
    contratos_obraspe <- read_parquet("data/obras/contratos_obraspe.parquet")
  }
)

contratos_obraspe %>% glimpse()

contratos_obraspe %>% 
  write_parquet('data/obras/contratos_obraspe.parquet')

## servicos_obraspe ----
tictoc::tic("tempo_servicos_obraspe_jdbc")

servicos_obraspe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.servicos_obraspe_v01"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup..."
    )
    
    servicos_obraspe <- read_parquet("data/obras/servicos_obraspe.parquet")
  }
)

servicos_obraspe %>% glimpse()

servicos_obraspe %>% 
  write_parquet('data/obras/servicos_obraspe.parquet')

# enriquecimento ----
obras_obraspe_2 <- obras_obraspe %>%
  left_join(
    contratos_obraspe %>%
      select(
        id_contrato, numero_contrato, sigla_orgao, nome_orgao, nome_fornecedor, cnpj_fornecedor, situacao_contrato, obra_paralisada, motivo_paralizacao
      ),
    by = 'id_contrato'
  ) %>%
  # 1. Tratar textos vazios para NA
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  
  # 2. Converter datas (formato DD/MM/AAAA)
  mutate(across(starts_with("data_"), dmy)) %>%
  
  # 3. Converter valores monetários e percentuais para numérico
  mutate(across(c(starts_with("valor_"), percentual_medido_obra), parse_br)) %>%
  
  # 4. Converter colunas lógicas ("SIM"/"NÃO" para TRUE/FALSE)
  mutate(
    obra_paralisada = case_when(
      obra_paralisada == "SIM" ~ TRUE,
      obra_paralisada == "NÃO" ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  
  # 5. Garantir que CNPJ mantenha os 14 dígitos (preservando zeros à esquerda)
  mutate(cnpj_fornecedor = str_pad(cnpj_fornecedor, width = 14, side = "left", pad = "0")) %>%
  
  # 6. Padronizar textos em caixa alta (opcional)
  mutate(across(c(descricao_obra, municipio_obra, gestor_obra, fiscal_obra, nome_orgao, nome_fornecedor), str_to_upper))

obras_obraspe_2 %>% glimpse()

obras_obraspe_2 %>% count(sigla_orgao)

obras_obraspe_2 %>% filter(
  sigla_orgao %in% c(
    'SDS', 'SEAP', 'SEMOBI', 'SEPE', 'SES', 'SRHS'
  )
    ) %>%
  View()

obras_obraspe_2 %>%
  select(
    id_obra, id_contrato, sigla_orgao, descricao_obra, municipio_obra
  ) %>%
  writexl::write_xlsx(
  'data/obras/obras_obraspe.xlsx'
)

obras_obraspe_2 %>% 
  write_parquet('data/obras/obras_obraspe.parquet')

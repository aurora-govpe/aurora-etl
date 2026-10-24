# versão jdbc ----
library(arrow)
library(BigDataPE)
library(DBI)
library(glue)
library(janitor)
library(stringr) 
library(tidyverse)

options(java.parameters = "-Xmx4g")
library(rJava)
library(RJDBC)

config_path <- file.path("data", "config.yml")

config_bdpe <- config::get('bdpe', file = config_path)

drv <- JDBC(driverClass = "io.trino.jdbc.TrinoDriver", classPath = "data/trino-jdbc-468.jar")

url <- glue("jdbc:trino://{config_bdpe$db_host}:{config_bdpe$db_port}?SSL=true&user={config_bdpe$db_user}&password={config_bdpe$db_pass}")

con <- dbConnect(drv, url)

## 2026 ----
tictoc::tic("tempo_liquidacao_2026_bdpe_jdbc")
liquidacao_2026_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.liquidacaototalizadorpei"
    ) %>%
      janitor::clean_names() %>%
      mutate(
        ano  = 2026
      )
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2026_bdpe.rds}..."
    )
    
    liquidacao_2026_bdpe <- read_parquet("data/efisco/liquidacao_2026_bdpe.parquet")
  }
)

liquidacao_2026_bdpe %>% glimpse()

liquidacao_2026_bdpe %>% 
  write_parquet('data/efisco/liquidacao_2026_bdpe.parquet')
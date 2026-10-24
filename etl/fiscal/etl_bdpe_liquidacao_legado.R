# versão jdbc ----
library(BigDataPE)
library(DBI)
library(glue)
library(janitor)
library(tidyverse)

options(java.parameters = "-Xmx4g")
library(rJava)
library(RJDBC)

config_path <- file.path("data", "config.yml")

config_bdpe <- config::get('bdpe', file = config_path)

drv <- JDBC(driverClass = "io.trino.jdbc.TrinoDriver", classPath = "data/trino-jdbc-468.jar")

url <- glue("jdbc:trino://{config_bdpe$db_host}:{config_bdpe$db_port}?SSL=true&user={config_bdpe$db_user}&password={config_bdpe$db_pass}")

con <- dbConnect(drv, url)

## contratos ----
tictoc::tic("tempo_contratos_bdpe_jdbc")
contratos_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.ef_contrato"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/contratos_bdpe.parquet")
  }
)

contratos_bdpe %>% glimpse()

contratos_bdpe %>% write_parquet('data/efisco/contratos_bdpe.parquet')

tictoc::toc()

## empenhos ----
tictoc::tic("tempo_empenhos_bdpe_jdbc")
empenhos_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.empenho_ne"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/empenhos_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/empenhos_bdpe.parquet")
  }
)

empenhos_bdpe %>% glimpse()

empenhos_bdpe %>% count(exercicio)

empenhos_bdpe %>% write_parquet('data/efisco/empenhos_bdpe.parquet')

tictoc::toc()

## 2020 ----
### empenho ----
tictoc::tic("tempo_empenho_2020_bdpe_jdbc")
empenho_2020_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.empenhototalizadorpei_2020"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/empenho_2020_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/empenho_2020_bdpe.parquet")
  }
)

empenho_2020_bdpe %>% glimpse()

empenho_2020_bdpe %>% write_parquet('data/efisco/empenho_2020_bdpe.parquet')

tictoc::toc()

### liquidação ----
tictoc::tic("tempo_liquidacao_2020_bdpe_jdbc")
liquidacao_2020_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.liquidacaototalizadorpei_2020"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2020_bdpe.parquet")
  }
)

liquidacao_2020_bdpe %>% glimpse()

liquidacao_2020_bdpe %>% write_parquet('data/efisco/liquidacao_2020_bdpe.parquet')

tictoc::toc()

## 2021 ----
### empenho ----
tictoc::tic("tempo_empenho_2021_bdpe_jdbc")
empenho_2021_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.empenhototalizadorpei_2021"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/empenho_2021_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/empenho_2021_bdpe.parquet")
  }
)

empenho_2021_bdpe %>% glimpse()

empenho_2021_bdpe %>% write_parquet('data/efisco/empenho_2021_bdpe.parquet')

tictoc::toc()

### liquidação ----
tictoc::tic("tempo_liquidacao_2021_bdpe_jdbc")
liquidacao_2021_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.liquidacaototalizadorpei_2021"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2021_bdpe.parquet")
  }
)

liquidacao_2021_bdpe %>% write_parquet('data/efisco/liquidacao_2021_bdpe.parquet')

tictoc::toc()

## 2022 ----
tictoc::tic("tempo_empenho_2022_bdpe_jdbc")
empenho_2022_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.empenhototalizadorpei_2022"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/empenho_2022_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/empenho_2022_bdpe.parquet")
  }
)

empenho_2022_bdpe %>% glimpse()

empenho_2022_bdpe %>% write_parquet('data/efisco/empenho_2022_bdpe.parquet')

### liquidação ----
tictoc::tic("tempo_liquidacao_2022_bdpe_jdbc")
liquidacao_2022_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select GBPLICIT_CD, GBPCROELIC_NU_ANO, GBPCROELIC_SQ, SOLICEMPN_NU_PROT_SEI, SOLICEMPN_NU_PROT_EXTERNO, EXECORC_IN_TEMPORAL_FONTE, GBPCONTR_NU_ANO, GBPCONTR_SQ, GBPCONTR_IN_TAC, SG_UGC, UG_CD_COORDENADORA, UG_CD, UG_DS, DESPGERN_NM, DESPGERN_TX_DET, GPOACAO_CD, GPOACAOCAD_NM, GPOSUBACAO_CD, GPOSUBCAD_NM, GPOPRIGOV_NM, FICHAFINAN_IN_DEA, FONTERECUR_CD, FONTERECUR_NM, NU_EMPENHO, NU_EMPENHO_ALT, EMPENHO_DH_LANCAMENTO, TPDOCIDNT_CD, NU_DOCUMENTO_CREDOR, PESSOA_NM_RAZAO_SOCIAL, GFUSUPD_TP_SUPRIMENTO, INDICADOR_REPFIN, PESSOA_NM_FANTASIA, UG_CD_CREDOR, UG_SG, NU_LIQUIDACAO, NU_LIQUIDACAO_ALT, GFULIQD_DT, MES_LIQUIDACAO, GRDESP_CD, MODALIDADE_APLICACAO, ELEMDESP_ID, ELEMDESP_NM, CODIGO_ITEM_GASTO, DS_ITEM_GASTO, DS_ITEM_EXTERNO, CODIGO_ITEM_GBP, DS_ITEM_GBP, MATSRVGR_TP, GFULIQD_TX_OBS, MARCITEMAT_NM, ITEM_VALOR_LIQUIDADO, VALOR_ESTORNADO, VALOR_LIQUIDO_LE, NU_DOC_HABIL, ANO_MES_DOCUMENTO_ORIGEM, DOCORIGEM_NU_SEI, GBPLICIT_NU_PROCESSO_PEI, GBPLICIT_IN_EXTERNO_PEI 
       from servico_hive.ingestedata.liquidacaototalizadorpei_2022"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2022_bdpe.parquet")
  }
)

liquidacao_2022_bdpe %>% write_parquet('data/efisco/liquidacao_2022_bdpe.parquet')

tictoc::toc()

## 2023 ----
tictoc::tic("tempo_liquidacao_2023_bdpe_jdbc")
liquidacao_2023_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select GBPLICIT_CD, GBPCROELIC_NU_ANO, GBPCROELIC_SQ, SOLICEMPN_NU_PROT_SEI, SOLICEMPN_NU_PROT_EXTERNO, EXECORC_IN_TEMPORAL_FONTE, GBPCONTR_NU_ANO, GBPCONTR_SQ, GBPCONTR_IN_TAC, SG_UGC, UG_CD_COORDENADORA, UG_CD, UG_DS, DESPGERN_NM, DESPGERN_TX_DET, GPOACAO_CD, GPOACAOCAD_NM, GPOSUBACAO_CD, GPOSUBCAD_NM, GPOPRIGOV_NM, FICHAFINAN_IN_DEA, FONTERECUR_CD, FONTERECUR_NM, NU_EMPENHO, NU_EMPENHO_ALT, EMPENHO_DH_LANCAMENTO, TPDOCIDNT_CD, NU_DOCUMENTO_CREDOR, PESSOA_NM_RAZAO_SOCIAL, GFUSUPD_TP_SUPRIMENTO, INDICADOR_REPFIN, PESSOA_NM_FANTASIA, UG_CD_CREDOR, UG_SG, NU_LIQUIDACAO, NU_LIQUIDACAO_ALT, GFULIQD_DT, MES_LIQUIDACAO, GRDESP_CD, MODALIDADE_APLICACAO, ELEMDESP_ID, ELEMDESP_NM, CODIGO_ITEM_GASTO, DS_ITEM_GASTO, DS_ITEM_EXTERNO, CODIGO_ITEM_GBP, DS_ITEM_GBP, MATSRVGR_TP, GFULIQD_TX_OBS, MARCITEMAT_NM, ITEM_VALOR_LIQUIDADO, VALOR_ESTORNADO, VALOR_LIQUIDO_LE, NU_DOC_HABIL, ANO_MES_DOCUMENTO_ORIGEM, DOCORIGEM_NU_SEI, GBPLICIT_NU_PROCESSO_PEI, GBPLICIT_IN_EXTERNO_PEI 
       from servico_hive.ingestedata.liquidacaototalizadorpei_2023"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2023_bdpe.parquet")
  }
)

liquidacao_2023_bdpe %>% write_parquet('data/efisco/liquidacao_2023_bdpe.parquet')

tictoc::toc()

## 2024 ----
tictoc::tic("tempo_liquidacao_2024_bdpe_jdbc")
liquidacao_2024_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.liquidacaototalizadorpei_2024"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2024_bdpe.parquet")
  }
)

liquidacao_2024_bdpe %>% write_parquet('data/efisco/liquidacao_2024_bdpe.parquet')

tictoc::toc()

## 2025 ----
tictoc::tic("tempo_liquidacao_2025_bdpe_jdbc")
liquidacao_2025_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via consulta ao banco...")
    
    dados <- dbGetQuery(
      con,
      "select * from servico_hive.ingestedata.liquidacaototalizadorpei_2025"
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do banco baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    cli::cli_alert_danger("Erro ao buscar dados no banco: {.val {e$message}}")
    
    cli::cli_alert_warning(
      "Carregando versão local de backup em {.path data/efisco/liquidacao_2025_bdpe.rds}..."
    )
    
    read_parquet("data/efisco/liquidacao_2025_bdpe.parquet")
  }
)

tictoc::toc()

liquidacao_2025_bdpe %>% glimpse()

liquidacao_2025_bdpe %>% write_parquet('data/efisco/liquidacao_2025_bdpe.parquet')

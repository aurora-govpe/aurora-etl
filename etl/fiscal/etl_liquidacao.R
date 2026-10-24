# pacotes ----
library(BigDataPE)
library(data.table)
library(DBI)
library(dplyr)
library(glue)
library(janitor)
library(lubridate)
library(qs)
library(readr)
library(RPostgres)

# setup ----
config_path <- file.path("data", "config.yml")

config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'liquidacao_2025',
  config_tokens$liquidacao_2025)

# etl ----
## liquidação 2023-24 ----
liquidacao_23_24_qs <- qs::qread(
  'data/efisco/liquidacao_23_24.qs'
)

## liquidação 2025 ----
# liquidacao_2025_bdpe <-BigDataPE::bdpe_fetch_data(
#   base_name = "liquidacao_2025",
#   verbosity = 1)  %>%
#   clean_names()

liquidacao_2025_bdpe <- tryCatch(
  {
    BigDataPE::bdpe_fetch_data(
      base_name = "liquidacao_2025",
      verbosity = 1
    ) %>%
      janitor::clean_names()
  },
  error = function(e) {
    message("⚠️ Erro ao buscar dados do BigDataPE: carregando versão local.")
    readRDS("data/efisco/liquidacao_2025_bdpe.rds")
  }
)

liquidacao_2025_bdpe %>% 
  write_rds('data/efisco/liquidacao_2025_bdpe.rds')

df_liquidacao <- liquidacao_2025_bdpe %>%
  mutate(
    
    # inteiros
    ano_da_liquidacao = as.integer(ano_da_liquidacao),
    mes_da_liquidacao = as.integer(mes_da_liquidacao),
    uge_emp = as.integer(uge_emp),
    codigo_grupo_da_despesa = as.integer(codigo_grupo_da_despesa),
    natureza_da_despesa_codigo = as.integer(natureza_da_despesa_codigo),
    unidade_fornecimento = as.integer(unidade_fornecimento),
    codigo_da_classe_do_material_de_servico = as.integer(codigo_da_classe_do_material_de_servico),
    codigo_do_grupo_de_material_de_servico = as.integer(codigo_do_grupo_de_material_de_servico),
    codigo_da_licitacao = as.integer(codigo_da_licitacao),
    
    # datas
    data_de_lancamento_do_empenho = as.Date(data_de_lancamento_do_empenho),
    data_do_doc_liq = as.IDate(data_do_doc_liq),
  ) %>%
  bind_rows(liquidacao_23_24_qs) %>%
  mutate(
    ano = ano_da_liquidacao,
    qtd_de_itens_liquidados   = as.numeric(
      gsub(",", ".", qtd_de_itens_liquidados)),
    valor_unitario_do_item_liquidado   = as.numeric(
      gsub(",", ".", valor_unitario_do_item_liquidado)),
    valor_total_liquidado   = as.numeric(
      gsub(",", ".", valor_total_liquidado)),
    valor_liquidado_final   = as.numeric(
      gsub(",", ".", valor_liquidado_final)),
    valor_descontado        = as.numeric(
      gsub(",", ".", valor_descontado)),
    valor_estornado         = as.numeric(
      gsub(",", ".", valor_estornado))
  )

# salvar ----
## liquidação ----
df_liquidacao %>% 
  qs::qsave('data/efisco/df_liquidacao.qs')

# pacotes ----
library(arrow)
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

# setup ----
## gd ----
folder_id <- "1kcNqyfhaSQR3Tf5VpyuTb6R0g3LfM-3Q"

drive_auth(
  path = "data/r-sheets-hugo-0788cc358cb5.json",
  scopes = "https://www.googleapis.com/auth/drive"
)

## tokens ----
config_path <- file.path("data", "config.yml")

config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'contratos',
  config_tokens$ef_contrato)

bdpe_store_token(
  'empenhos',
  config_tokens$empenho_ne)

bdpe_store_token(
  'execucao',
  config_tokens$execucao_despesa_para_sepe)

bdpe_store_token(
  'liquidacao_2025',
  config_tokens$liquidacao_2025)

# extrair ----
## contratos ----
contratos_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via {.pkg BigDataPE}...")
    
    dados <- BigDataPE::bdpe_fetch_data(
      base_name = "contratos",
      verbosity = 1
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do BigDataPE baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    
    cli::cli_alert_danger("Erro ao buscar dados online: {.val {e$message}}")
    
    cli::cli_alert_warning("Carregando versão local de backup em {.path data/efisco/contratos_bdpe.rds}...")
    
    readRDS("data/efisco/contratos_bdpe.rds")
  }
)

contratos_bdpe %>% glimpse()

contratos_bdpe %>% 
  write_rds('data/efisco/contratos_bdpe.rds')

## empenhos ----
empenhos_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via {.pkg BigDataPE}...")
    
    dados <- BigDataPE::bdpe_fetch_data(
      base_name = "empenhos",
      verbosity = 1
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do BigDataPE baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    
    cli::cli_alert_danger("Erro ao buscar dados online: {.val {e$message}}")
    
    cli::cli_alert_warning("Carregando versão local de backup em {.path data/efisco/empenhos_bdpe.rds}...")
    
    readRDS("data/efisco/empenhos_bdpe.rds")
  }
)

empenhos_bdpe %>% 
  write_rds('data/efisco/empenhos_bdpe.rds')

empenhos_bdpe %>% glimpse()

## execucao ----
execucao_bdpe <- tryCatch(
  {
    cli::cli_alert_info("Iniciando busca de contratos via {.pkg BigDataPE}...")
    
    dados <- BigDataPE::bdpe_fetch_data(
      base_name = "execucao",
      verbosity = 1
    ) %>%
      janitor::clean_names()
    
    cli::cli_alert_success("Dados do BigDataPE baixados e tratados com sucesso!")
    
    dados
  },
  error = function(e) {
    
    cli::cli_alert_danger("Erro ao buscar dados online: {.val {e$message}}")
    
    cli::cli_alert_warning("Carregando versão local de backup em {.path data/efisco/execucao_bdpe.rds}...")
    
    readRDS("data/efisco/execucao_bdpe.rds")
  }
)

execucao_bdpe %>% 
  write_rds('data/efisco/execucao_bdpe.rds')

execucao_bdpe %>% glimpse()

anos <- unique(execucao_bdpe$ano)

# 2. Função para salvar e subir cada arquivo
walk(anos, function(ano_ref) {
  
  # Define o nome do arquivo baseado no ano
  nome_arquivo <- paste0("execucao_bdpe_", ano_ref, ".csv")
  caminho_local <- file.path("data/efisco", nome_arquivo)
  
  # Filtra os dados e salva localmente
  execucao_bdpe %>%
    filter(ano == ano_ref) %>%
    write_csv(caminho_local)
  
  # Faz o upload para a pasta específica no Google Drive
  drive_upload(
    caminho_local,
    path = as_id(folder_id),
    name = nome_arquivo, # Garante que o nome no Drive seja por ano
    overwrite = TRUE
  )
  
  message(paste("Arquivo do ano", ano_ref, "enviado com sucesso!"))
})

## liquidação 2023-24 ----
liquidacao_23_24_qs <- qs::qread(
  'data/efisco/liquidacao_23_24.qs'
)

## liquidação 2025 ----

source('etl/etl_sub_efisco.R')

liquidacao_2025_bdpe %>% glimpse()

liquidacao_23_24_qs %>% glimpse()

liquidacao_2025_bdpe %>% count(ano_da_liquidacao, mes_da_liquidacao)

# liquidacao_2025_bdpe %>% 
#   write_rds('data/efisco/liquidacao_2025_bdpe.rds')

liquidacao_2025_bdpe %>% write_parquet('app_aurora_orcamento/data/liquidacao_2025_bdpe.parquet')

liquidacao_2025_bdpe %>% write_parquet('data/efisco/liquidacao_2025_bdpe.parquet')

# transformar ----
## contratos ----
df_contratos <- contratos_bdpe %>%
  select(
    uge_sigla, uge_cod, uge_contrato, ano_da_licitacao, status_da_licitacao, 
    codigo_licitacao = licitacao,
    codigo_contrato = contrato,
    pessoa_cd, credor, cnpj_ou_cpf, 
    tipo_contrato = tipo_de_contrato_tac
  ) %>%
  distinct()

## liquidação ----
df_liquidacao <- liquidacao_2025_bdpe %>%
  mutate(
    
    ano_da_liquidacao = as.integer(ano_da_liquidacao),
    mes_da_liquidacao = as.integer(mes_da_liquidacao),
    uge_emp = as.integer(uge_emp),
    
    natureza_da_despesa_codigo = as.integer(natureza_da_despesa_codigo),
    unidade_fornecimento = as.integer(unidade_fornecimento),
    codigo_da_classe_do_material_de_servico = as.integer(codigo_da_classe_do_material_de_servico),
    codigo_do_grupo_de_material_de_servico = as.integer(codigo_do_grupo_de_material_de_servico),
    codigo_da_licitacao = as.integer(codigo_da_licitacao),
    
    # datas
    data_de_lancamento_do_empenho = as.Date(data_de_lancamento_do_empenho),
    data_do_doc_liq = as.IDate(data_do_doc_liq),
  ) %>%
  bind_rows(
    liquidacao_23_24_qs %>%
      mutate(
        codigo_grupo_da_despesa = as.character(codigo_grupo_da_despesa),
      )
    ) %>%
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

df_liquidacao_2 <- df_liquidacao %>%
  left_join(
    df_contratos,
    by = c('codigo_contrato')
  )

## credores ----
dic_acao_subacao <- df_liquidacao_2 %>%
  filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(acao, sub_acao) %>%
  separate(acao, into = c("acao_cod", "acao_nm"), sep = "\\s?-\\s?", extra = "merge", remove = FALSE) %>%
  separate(sub_acao, into = c("sub_acao_cod", "sub_acao_nm"), sep = "\\s?-\\s?", extra = "merge", remove = FALSE) %>%
  mutate(across(everything(), str_trim)) %>%
  arrange(acao, sub_acao)

df_acao_ord <- dic_acao_subacao %>%
  dplyr::arrange(acao_cod) %>%
  dplyr::distinct(acao_cod, .keep_all = TRUE)

choices_acao_full <- stats::setNames(df_acao_ord$acao_cod, df_acao_ord$acao)

df_sub_ord <- dic_acao_subacao %>%
  dplyr::arrange(sub_acao_cod) %>%
  dplyr::distinct(sub_acao_cod, .keep_all = TRUE)

choices_sub_full <- stats::setNames(df_sub_ord$sub_acao_cod, df_sub_ord$sub_acao)

dic_credores <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(credor_razao_social) %>%
  arrange(credor_razao_social) %>%
  mutate(
    credor_id = dplyr::row_number()
  )

dic_despesas_gerenciais_nome <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(despesa_gerencial_nome) %>%
  arrange(despesa_gerencial_nome) %>%
  mutate(
    despesa_gerencial_nome_id = dplyr::row_number()
  )

dic_item_de_gasto <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(item_de_gasto) %>%
  arrange(item_de_gasto) %>%
  mutate(
    item_de_gasto_id = dplyr::row_number()
  )

dic_ugcs <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(ugc_emp_sigla) %>%
  arrange(ugc_emp_sigla) %>%
  mutate(
    ugc_emp_sigla_id = dplyr::row_number()
  )

dic_uges <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(uge_emp, uge_nome_emp) %>%
  arrange(uge_emp) %>%
  mutate(uge_nome_emp = na_if(uge_nome_emp, "")) %>%
  fill(uge_nome_emp, .direction = "down") %>%
  mutate(
    uge_nome_emp = case_match(
      uge_emp,
      150111 ~ "NUCLEO DE APOIO ADMINISTRATIVO - I REGIAO FISCAL",
      .default = uge_nome_emp
    )
  ) %>%
  distinct(uge_emp, uge_nome_emp, .keep_all = TRUE) 

dic_gds <- df_liquidacao_2 %>%
  #filter(codigo_grupo_da_despesa %in% c(3, 4)) %>%
  distinct(codigo_grupo_da_despesa) %>%
  arrange(codigo_grupo_da_despesa)

df_credores <- df_liquidacao_2 %>%
  filter(
    #codigo_grupo_da_despesa %in% c(3, 4)
  ) %>%
  group_by(
    ano_da_liquidacao, 
    mes_da_liquidacao, 
    ugc_emp_sigla, 
    acao,
    codigo_grupo_da_despesa,
    sub_acao, 
    despesa_gerencial_nome, 
    dea, 
    item_de_gasto, 
    item_liquidado_nome, 
    credor_documento, 
    #cnpj_ou_cpf, 
    credor_razao_social, 
    codigo_contrato
  ) %>%
  summarise(
    valor_liquidado_final = sum(valor_liquidado_final, na.rm = TRUE), .groups = 'drop'
  ) %>%
  mutate(
    data_competencia = make_date(year = ano_da_liquidacao, month = mes_da_liquidacao, day = 1)
  ) %>%
  #filter(!is.na(codigo_contrato)) %>%
  filter(!is.na(valor_liquidado_final)) %>%
  filter(!is.na(ugc_emp_sigla)) %>%
  #filter(!is.na(credor_razao_social)) %>%
  left_join(
    dic_credores,
    by = c("credor_razao_social")
  ) %>%
  left_join(
    dic_ugcs,
    by = c("ugc_emp_sigla")
  ) %>%
  left_join(
    dic_despesas_gerenciais_nome,
    by = c("despesa_gerencial_nome")
  ) %>%
  left_join(
    dic_item_de_gasto,
    by = c("item_de_gasto")
  ) %>%
  left_join(
    dic_acao_subacao %>%
      select(
        -c(acao_nm, sub_acao_nm)
      ),
    by = c("acao", "sub_acao")
  )

df_credores_bot <- df_credores %>%
  filter(
    ano_da_liquidacao == 2025
  ) %>%
  group_by(
    ugc = ugc_emp_sigla, 
    credor = credor_razao_social
  ) %>%
  summarise(
    valor_liquidado = sum(valor_liquidado_final, na.rm = TRUE), .groups = 'drop'
  )

# salvar ----
## dicionários ----

dic_acao_subacao %>% 
  qs::qsave('app_contratos/data/dic_acao_subacao.qs')

choices_acao_full %>% 
  saveRDS('app_contratos/data/choices_acao_full.rds')

choices_sub_full  %>% 
  saveRDS('app_contratos/data/choices_sub_full.rds')

dic_credores %>% 
  qs::qsave('app_contratos/data/dic_credores.qs')

dic_despesas_gerenciais_nome %>% 
  qs::qsave('app_contratos/data/dic_despesas_gerenciais_nome.qs')

dic_gds %>% 
  pull(1) %>%
  qs::qsave('app_contratos/data/dic_gds.qs')

dic_item_de_gasto %>% 
  qs::qsave('app_contratos/data/dic_item_de_gasto.qs')

dic_ugcs %>% 
  qs::qsave('app_contratos/data/dic_ugcs.qs')

dic_uges %>% 
  qs::qsave('app_contratos/data/dic_uges.qs')

## credores ----
df_credores %>%
  select(
    -c(
      acao, sub_acao,
      credor_razao_social, 
      despesa_gerencial_nome, 
      item_de_gasto, 
      ugc_emp_sigla)
  ) %>%
  qs::qsave('app_contratos/data/df_credores.qs')

df_credores_bot %>% 
  saveRDS('../bots/bot_segpr/data/credores.rds')

## liquidação ----
df_liquidacao_2 %>% 
  qs::qsave('app_contratos/data/df_liquidacao.qs')

# envio para servidor ----
library(ssh)

## setup ----

# Caminho Local (onde estão os arquivos no seu PC)
dir_local <- "app_contratos/data"

# Caminho Remoto (onde eles vão ser salvos)
dir_remoto <- "/srv/dados/contratos"

# Lista de arquivos
arquivos <- c(
  "choices_acao_full.rds",
  "choices_sub_full.rds",
  "df_credores.qs",
  "dic_acao_subacao.qs",
  "dic_credores.qs",
  "dic_despesas_gerenciais_nome.qs",
  'dic_gds.qs',
  "dic_item_de_gasto.qs",
  'dic_ugcs.qs'
)

# Prepara os caminhos completos locais
arquivos_completos <- file.path(dir_local, arquivos)

# --- 2. Conexão ---

message("Conectando ao servidor na porta 7654...")

# A sintaxe user@host:port funciona perfeitamente no ssh_connect
# O R vai usar sua chave SSH padrão (id_rsa) automaticamente.
session <- ssh_connect("hugo.vasconcelos@10.238.113.49:7654")

# --- 3. Envio (Upload) ---

message("Iniciando upload dos arquivos...")

# O scp_upload envia a lista toda de uma vez
scp_upload(session, files = arquivos_completos, to = dir_remoto)

message("Upload concluído com sucesso!")

# --- 4. Desconectar ---
ssh_disconnect(session)


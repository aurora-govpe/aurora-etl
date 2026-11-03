library(tidyverse)
library(arrow)
library(furrr)
library(pool)

# Carregamento centralizado de caminhos
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../../R/paths.R")) source("../../R/paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

contratos_plug <- if (file.exists(file.path(DIR_RAW_MULUNGU, "contratos_plug.rds"))) {
  read_rds(file.path(DIR_RAW_MULUNGU, "contratos_plug.rds"))
} else if (file.exists(file.path(DIR_RAW_MULUNGU, "constratos_plug.rds"))) {
  read_rds(file.path(DIR_RAW_MULUNGU, "constratos_plug.rds"))
} else {
  read_rds("data/constratos_plug.rds")
}

contratos_efisco_plug <- contratos_plug %>% 
  select(contrato_plug, contratos_efisco) %>% 
  filter(!is.na(contratos_efisco)) %>% 
  mutate(contratos = str_split(contratos_efisco, ",")) %>% 
  unnest(contratos) %>% 
  mutate(contratos = str_trim(contratos)) %>% 
  mutate(contratos = str_remove(contratos, "^0+")) %>% 
  select(-contratos_efisco, contrato_plug, contrato_efisco = contratos) %>% 
  distinct()

write_rds(contratos_efisco_plug, file.path(DIR_RAW_MULUNGU, "contratos_efisco_plug.rds"))

empenhos_efisco <- read_parquet("../contratos/data/ETL/efisco_empenhos.pqt") %>%
  filter(
    str_detect(ug_sigla, '(DER)|(SEMOBI)|(SEINFRA)') # DER-PE
  )

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "sepe",
  host = "10.238.107.182",
  port = "5432",
  user = "sepe_adm",
  password = "!sepe_adm!",
  minSize = 4,
  idleTimeout = 60 * 60
)

DBI::dbWriteTable(
  conn = pool,
  name = "efisco_empenhos_der_semobi",
  value = empenhos_efisco,
  overwrite = TRUE
)

poolClose(pool)

#DBI::dbRemoveTable(pool, "empenhos_der_semobi")

empenhos_efisco %>% 
  select(acao_codigo, acao_nome, subacao_codigo, subacao_nome) %>% 
  distinct() %>% 
  write_rds(file.path(DIR_MULUNGU, "base_acoes_subacoes_der.rds"))

empenhos_plug <- empenhos_efisco %>% filter(contrato_codigo %in% unique(contratos_efisco_plug$contrato_efisco))


contratos_acoes_fontes_efisco <- empenhos_plug %>% 
  mutate(
    pago = valor_total_pag_empenho - valor_total_devolvido_ob_empenho,
    liquidado = valor_liquidado_final_empenho) %>% 
  group_by(
    contrato_efisco = contrato_codigo, 
    acao_codigo, acao_nome, subacao_codigo, subacao_nome,
    fonte_de_recurso_sintetica_codigo, fonte_de_recurso_detalhada_codigo, 
    fonte_de_recurso_detalhada_nome) %>% 
  summarise(
    pago = sum(pago, na.rm = TRUE), 
    liquidado = sum(liquidado, na.rm = TRUE),
    .groups = "drop") 
#select(subacao_nome) %>% 
#mutate(rodovia_subacao = str_to_upper(str_extract(str_to_lower(subacao_nome), "(([v,a]*pe|br)-\\d{3})|(\\d. dod)"))) %>% 

write_rds(contratos_acoes_fontes_efisco, file.path(DIR_RAW_MULUNGU, "contratos_acoes_fontes_efisco.rds"))
  
financeiro_plug <- contratos_efisco_plug %>% 
  left_join(
    contratos_acoes_fontes_efisco, 
    by = join_by(contrato_efisco),
    relationship = "many-to-many") %>% 
  select(-contrato_efisco) %>% 
  distinct() %>% #glimpse()
  group_by(contrato_plug) %>% 
  mutate(acao_subacao = paste0(acao_codigo, ".", subacao_codigo)) %>% 
  summarise(
    acao_codigo  = paste0(na.omit(sort(unique(acao_codigo))), collapse = ", "),
    acao_subacao  = paste0(na.omit(sort(unique(acao_subacao))), collapse = ", "),
    #subacao_codigo  = paste0(sort(unique(subacao_codigo)), collapse = ", "),
    fonte_de_recurso_sintetica_codigo = paste0(na.omit(sort(unique(fonte_de_recurso_sintetica_codigo))), collapse = ", "),
    fonte_de_recurso_detalhada_codigo = paste0(na.omit(sort(unique(fonte_de_recurso_detalhada_codigo))), collapse = ", "),
    pago = sum(pago, na.rm = TRUE), 
    liquidado = sum(liquidado, na.rm = TRUE),
    .groups = "drop"
  )

write_rds(financeiro_plug, file.path(DIR_RAW_MULUNGU, "financeiro_plug.rds"))


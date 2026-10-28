# 1. Pacotes ----
library(arrow)
library(cli)
library(DBI)
library(dplyr)
library(glue)
library(janitor)
library(lubridate)
library(readr)
library(readxl)
library(rJava)
library(RJDBC)
library(sf)
library(stringr)
library(tidyr)

# Resolução de caminhos centralizados
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

dir_out <- if (exists("DIR_OUT_AURORA")) DIR_OUT_AURORA else "C:/dados/aurora"

# Leitura dos programas finalísticos da planilha de cadastro
cadastro_programas <- readxl::read_excel(
  'data/efisco/_2026082704_Produto - Unidade - Cadastro.xls',
  sheet = 'PLN_PLO_EXP_20260827_1416'
) %>%
  janitor::clean_names()

# Vetor com os códigos numéricos/texto dos programas finalísticos
programas_finalisticos <- cadastro_programas %>%
  filter(programa_tipo == 'Finalístico') %>%
  pull(programa_codigo) %>%
  unique() %>%
  as.character()

# Alternativa: vetor com as ações finalísticas (caso sua base de execução não tenha a coluna do programa)
acoes_finalisticas <- cadastro_programas %>%
  filter(programa_tipo == 'Finalístico') %>%
  pull(acao_codigo) %>%
  unique() %>%
  as.character()

dicionario_uo <-
  readxl::read_excel(
    'data/efisco/execucao/dicionario_uo.xlsx'
  )

dados_convenios_despesa <- read_parquet(
  'data/efisco/execucao/execucao_bdpe.parquet'
) %>%
  filter(
    modalidade_despesa_cod %in% c('40', '41', '50', '70', '71', '72', '73', '74', '75', '76') |
      (
        modalidade_despesa_cod == '90' & 
          as.character(programa_cod) %in% programas_finalisticos
      )
  ) %>%
  mutate(
    # Unifica todas as 70-76 em '70'
    modalidade_despesa_cod = if_else(
      modalidade_despesa_cod %in% c('70', '71', '72', '73', '74', '75', '76'),
      '70',
      modalidade_despesa_cod
    ),
    # Atribui a descrição da modalidade
    modalidade_despesa_nm = case_match(
      modalidade_despesa_cod,
      '40' ~ 'TRANSFERÊNCIAS A MUNICÍPIOS',
      '41' ~ 'TRANSFERÊNCIAS A MUNICÍPIOS – FUNDO A FUNDO',
      '50' ~ 'TRANSFERÊNCIAS A INSTITUIÇÕES PRIVADAS SEM FINS LUCRATIVOS',
      '70' ~ 'CONSÓRCIOS',
      '90' ~ 'APLICAÇÕES DIRETAS',
      .default = modalidade_despesa_cod
    )
  )

dados_convenios_despesa %>%
  filter(
    ano %in% c(2026)
  ) %>%
  group_by(
    ano
  ) %>%
  summarise(
    dotacao_inicial = sum(dotacao_inicial, na.rm = TRUE),
    dotacao_autorizada = sum(dotacao_autorizada, na.rm = TRUE),
    prog_financeira = sum(prog_financeira, na.rm = TRUE),
    empenhado = sum(empenhado, na.rm = TRUE),
    liquidado = sum(liquidado, na.rm = TRUE),
    pago = sum(pago, na.rm = TRUE)
  ) %>% 
  print(n=100)
  
dados_convenios_despesa %>% glimpse()

dados_emendas <- read_parquet(
  'data/efisco/dados_emendas.parquet'
)

dados_emendas %>% glimpse()

dados_convenios_despesa_2 <- dados_convenios_despesa %>%
  mutate(
    chave_convenio = paste0(acao_cod, "-", subacao_cod),
    data_ref = make_date(ano, mes, 1),
    # Proteção: Garante que NAs em 'liquidado' sejam lidos como 0 e não quebrem a condição
    data_liq = if_else(coalesce(liquidado, 0) > 0, data_ref, NA_Date_)
  ) %>%
  group_by(
    chave_convenio, 
    ano, 
    uo_cod_acao, uo_nome_acao, 
    acao_cod, acao_nome, subacao_cod, subacao_nome, modalidade_despesa_cod, modalidade_despesa_nm
  ) %>%
  summarise(
    # suppressWarnings evita que grupos contendo apenas NAs inundem o console com mensagens de Inf e -Inf
    data_min = suppressWarnings(coalesce(min(data_liq, na.rm = TRUE), min(data_ref, na.rm = TRUE))),
    data_max = suppressWarnings(coalesce(max(data_liq, na.rm = TRUE), max(data_ref, na.rm = TRUE))),
    
    # format() processa NAs nativamente sem gerar lixo de string como "NA/NA"
    periodo = paste0(format(data_min, "%m/%Y"), " a ", format(data_max, "%m/%Y")),
    
    dotacao_inicial    = sum(dotacao_inicial, na.rm = TRUE),
    dotacao_autorizada = sum(dotacao_autorizada, na.rm = TRUE),
    
    prog_financeira    = sum(prog_financeira, na.rm = TRUE),
    empenhado          = sum(empenhado, na.rm = TRUE),
    liquidado          = sum(liquidado, na.rm = TRUE),
    pago               = sum(pago, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  select(-data_min, -data_max) %>%
  mutate(
    # Substitui a checagem manual de 'NA/NA a NA/NA' por uma detecção segura de NA propagado
    periodo = if_else(str_detect(periodo, "NA"), NA_character_, periodo),
    
    # Reverte retornos de infinito (-Inf) gerados pelo max() em grupos sem dados de dotação
    across(c(dotacao_inicial, dotacao_autorizada), ~ if_else(is.infinite(.), 0, .))
  ) %>%
  left_join(
    dicionario_uo,
    by = c('uo_cod_acao', 'uo_nome_acao')
  )

dados_convenios_despesa_2 %>% glimpse()

dados_convenios_despesa_2 %>% 
  filter(
    ano %in% c(2026)
  ) %>%
  group_by(
    ano
  ) %>%
  summarise(
    dotacao_inicial = sum(dotacao_inicial, na.rm = TRUE),
    dotacao_autorizada = sum(dotacao_autorizada, na.rm = TRUE),
    prog_financeira = sum(prog_financeira, na.rm = TRUE),
    empenhado = sum(empenhado, na.rm = TRUE),
    liquidado = sum(liquidado, na.rm = TRUE),
    pago = sum(pago, na.rm = TRUE)
  ) %>% 
  print(n=100)

# 2. Agregação e remoção de duplicatas por chave_convenio
dados_convenios_despesa_consolidado <- dados_convenios_despesa_2 %>%
  left_join(
    dados_emendas %>%
      rename(
        empenhado_emenda = empenhado,
        pago_emenda = pago
      ) %>% 
      select(-c(ano, acao_nome)),
    by = c('acao_cod', 'subacao_cod' = 'subacao')
  ) %>%
  mutate(
    is_emenda = if_else(is.na(chave_01), 0, 1),
    periodo = if_else(
      periodo == "Inf a -Inf", NA_character_, periodo
    )
  )

dados_convenios_despesa_consolidado %>% 
  get_dupes(chave_convenio)

dados_convenios_despesa_consolidado %>% glimpse()

# iniciativas ----
dados_iniciativas <- readRDS(
  'data/efisco/execucao/shared_iniciativas.rds'
) %>%
  mutate(
    subacao = ifelse(
      subacao == '-', NA, subacao
    ),
    long = st_coordinates(.)[, 1],
    lat  = st_coordinates(.)[, 2]
  ) %>%
  filter(
    !is.na(acao),
    !is.na(subacao)
  )  %>%
  st_drop_geometry()

dados_iniciativas_2 <- dados_iniciativas %>%
  separate_rows(acao, subacao, sep = "[;,]\\s*") %>%
  mutate(across(c(acao, subacao), str_trim)) %>%
  mutate(
    chave_iniciativa = paste0(acao, "-", subacao)
  )

dim_iniciativas <- dados_iniciativas %>%
  select(-acao, -subacao) %>%
  distinct()

bridge_iniciativa_convenio <- dados_iniciativas_2 %>%
  select(
    id_produto,
    produto,
    chave_convenio = chave_iniciativa
  ) %>%
  distinct()

# 3. Exportação dos arquivos finais
dados_convenios_despesa_consolidado %>%
  write_parquet(file.path(dir_out, 'dados_convenios_despesa.parquet'))

dados_emendas %>% 
  write_parquet(file.path(dir_out, 'dados_emendas.parquet'))

# Exporta a Dimensão Iniciativas
dim_iniciativas %>%
  write_parquet(file.path(dir_out, 'dim_iniciativas.parquet'))

# Exporta a Tabela Ponte
bridge_iniciativa_convenio %>%
  write_parquet(file.path(dir_out, 'bridge_iniciativa_convenio.parquet'))

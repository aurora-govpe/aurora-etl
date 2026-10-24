# bibliotecas ----
library(dplyr)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(purrr)
library(stringr)
library(tidyr)

# setup ----
source('scripts/utils.R')

FOLDER_ID <- "1BOKygrvTgeM8UXGVWG-RnWB8esCXZDQU"

AUTH_PATH <- "data/r-sheets-hugo-0788cc358cb5.json"

gs4_auth(
  path = AUTH_PATH,
  scopes = "https://www.googleapis.com/auth/spreadsheets"
)

drive_auth(
  path = AUTH_PATH)

# listas ----
lista_campos <- c(
  'municipio',
  'no_contrato',
  'objeto_resumido',
  'status',
  'percent_obra',
  'data_inicio',
  'data_termino',
  'indicador',
  'indicador_nome',
  'valor_total',
  'valor_pago',
  'prox_parc',
  'liberacao_de_desembolso',
  'empresa',
  'vigencia_contrato',
  'gestor',
  'fiscal',
  'situacao_aditivos',
  'licenca_ambiental',
  'observacoes_aurora',
  'latitude',
  'longitude',
  'local',
  'tipologia',
  'fonte'
)

# urls ----
url_sedu <- "https://docs.google.com/spreadsheets/d/1JM9DXKrPBOJMQFnxMxBgMzyvmsQarev2FVMH2NQThMs/edit?gid=1578385382#gid=1578385382&fvid=423291797"

id_planilha <- as_sheets_id(url_sedu)

# convênios ----
## vigentes ----
convênios_vigentes_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SCI] CONVÊNIOS',
  skip = 5
) %>%
  janitor::clean_names() %>%
  
  mutate(across(
    c(autorizacao_ambiental, area, latitude, longitude),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  
  # 2. Converte para os tipos corretos
  mutate(
    area                  = as.numeric(area),
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude)
  ) %>%
  mutate(
    municipio = municipio_1,
    no_contrato = convenio,
    data_inicio = data_inicio %>% as.character(),
    data_termino = data_vencimento_vigencia,
    indicador = area,
    indicador_nome = 'Área',
    valor_total = r_total,
    valor_pago = r_pago_pelo_estado,
    empresa = '',
    vigencia_contrato = vigencia %>% as.character(),
    gestor = '',
    fiscal = fiscal_do_convenio,
    situacao_aditivos = aditivos %>% as.character(),
    licenca_ambiental = validade,
    local = '',
    fonte = '[SCI] CONVÊNIOS'
  )

convênios_vigentes_dados_brutos %>% glimpse()

## encerrados ----
convênios_encerrados_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SCI] CONV. TCESP/ENC',
  skip = 6
) %>%
  janitor::clean_names() %>%
  mutate(across(
    c(vigencia, autorizacao_ambiental, validade_licenca, area_pavimentada),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  mutate(
    area_pavimentada      = as.numeric(area_pavimentada),
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude)
  ) %>%
  mutate(
    no_contrato = convenio,
    data_inicio = data_inicio %>% as.character(),
    data_termino = data_vencimento_vigencia,
    indicador = area_pavimentada,
    indicador_nome = 'Área Pavimentada',
    valor_total = r_total,
    valor_pago = r_pago_pelo_estado,
    empresa = '',
    vigencia_contrato = vigencia %>% as.character(),
    gestor = '',
    fiscal = fiscal_do_convenio,
    situacao_aditivos = aditivos %>% as.character(),
    licenca_ambiental = validade,
    local = '',
    fonte = '[SCI] CONV. TCESP/ENC'
  )

convênios_encerrados_dados_brutos %>% glimpse()

# contratos ----
## ilumina pe ----
iluminape_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SCI] ILUMINA PE',
  skip = 6
) %>%
  janitor::clean_names() %>%
  mutate(across(
    c(data_de_inicio_ordem_de_servico, autorizacao_ambiental, validade, status_licenca_ambiental, latitude, longitude),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  mutate(
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude)
  ) %>%
  mutate(
    data_inicio = data_de_inicio_ordem_de_servico %>% as.character(),
    data_termino = data_de_termino,
    indicador = no_de_pontos,
    indicador_nome = 'Número de Pontos',
    valor_total = valor_atualizado_do_contrato,
    valor_pago = total_pago,
    vigencia_contrato = vigenc_execucao_original %>% as.character(),
    situacao_aditivos = aditivo %>% as.character(),
    licenca_ambiental = status_licenca_ambiental,
    local = '',
    fonte = '[SCI] ILUMINA PE'
  )

iluminape_dados_brutos %>% glimpse()

## contratos encerrados ----
pares_para_remover <- tibble(
  municipio = c("CAPOEIRAS", "MIRANDIBA", "CALUMBI", "BELÉM DE MARIA", "PASSIRA"),
  no_contrato = c("001/2022", "012/2022", "022/2022", "030/2022", "031/2022")
)

contratos_encerrados_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SCI] ENCERRADOS',
  skip = 7
) %>%
  janitor::clean_names() %>%
  anti_join(pares_para_remover, by = c("municipio", "no_contrato")) %>%
  mutate(
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude)
  ) %>%
  mutate(
    status = status_6,
    data_inicio = data_de_inicio_ordem_de_servico %>% as.character(),
    data_termino = data_de_termino,
    indicador = NA,
    indicador_nome = 'Não há',
    valor_total = valor_atualizado_do_contrato,
    valor_pago = total_pago,
    vigencia_contrato = vigenc_execucao_original %>% as.character(),
    situacao_aditivos = NA_character_,
    licenca_ambiental = status_81,
    local = '',
    fonte = '[SCI] ENCERRADOS'
  )

contratos_encerrados_dados_brutos %>% glimpse()

# encostas ----
encostas_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SCE] ENCOSTAS',
  skip = 6
) %>%
  janitor::clean_names() %>%
  mutate(across(
    c(no_contrato, data_de_inicio_ordem_de_servico, prazo_de_vigencia, prazo_de_execucao, autorizacao_ambiental, validade, area, latitude, longitude),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  mutate(
    area                  = as.numeric(area),
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude)
  ) %>%
  mutate(
    data_inicio = data_de_inicio_ordem_de_servico %>% as.character(),
    data_termino = data_de_termino,
    indicador = area,
    indicador_nome = 'Área',
    valor_total = valor_atualizado_do_contrato,
    valor_pago = total_pago,
    vigencia_contrato = prazo_de_vigencia %>% as.character(),
    situacao_aditivos = aditivo %>% as.character(),
    licenca_ambiental = status_da_licenca,
    local = '',
    fonte = '[SCE] ENCOSTAS'
  )

encostas_dados_brutos %>% glimpse()

# mobilidade ----
mobilidade_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SUMOB] MOBILIDADE',
  skip = 6
) %>%
  janitor::clean_names() %>%
  mutate(across(
    c(no_contrato, data_de_termino, valor_atualizado_do_contrato, total_pago, data_de_termino, vigenc_execucao_original, longitude),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  mutate(
    area                  = as.numeric(area),
    latitude              = as.numeric(latitude),
    longitude             = as.numeric(longitude),
    valor_atualizado_do_contrato = as.numeric(valor_atualizado_do_contrato),
    total_pago = as.numeric(total_pago),
    data_de_termino = ymd(data_de_termino)
  ) %>%
  mutate(
    data_inicio = data_de_inicio %>% as.character(),
    data_termino = data_de_termino,
    indicador = area,
    indicador_nome = 'Área',
    valor_total = valor_atualizado_do_contrato,
    valor_pago = total_pago,
    vigencia_contrato = vigenc_execucao_original %>% as.character(),
    situacao_aditivos = situacao_atual_aditivo %>% as.character(),
    licenca_ambiental = validade,
    fonte = '[SUMOB] MOBILIDADE'
  )

mobilidade_dados_brutos %>% glimpse()

# periferias ----
periferias_dados_brutos <- read_sheet(
  ss = id_planilha,
  sheet = '[SUPER] PERIFERIAS',
  skip = 6
) %>%
  janitor::clean_names() %>%
  mutate(across(
    c(no_contrato, data_de_inicio, vigenc_contrato_original, vigenc_execucao_original, validade),
    ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
  )) %>%
  mutate(
    area = as.numeric(area)
  ) %>%
  mutate(
    data_inicio = data_de_inicio %>% as.character(),
    data_termino = data_de_termino,
    indicador = area,
    indicador_nome = 'Área',
    valor_total = valor_atualizado_do_contrato,
    valor_pago = total_pago,
    vigencia_contrato = vigenc_execucao_original %>% as.character(),
    situacao_aditivos = situacao_atual_aditivo %>% as.character(),
    licenca_ambiental = status_155,
    fonte = '[SUPER] PERIFERIAS'
  )

periferias_dados_brutos %>% glimpse()

# base completa ----
sedu_completa <- bind_rows(
  convênios_vigentes_dados_brutos %>% select(any_of(lista_campos)),
  convênios_encerrados_dados_brutos %>% select(any_of(lista_campos)), 
  iluminape_dados_brutos %>% select(any_of(lista_campos)),
  contratos_encerrados_dados_brutos %>% select(any_of(lista_campos)),
  encostas_dados_brutos %>% select(any_of(lista_campos)),
  mobilidade_dados_brutos %>% select(any_of(lista_campos)),
  periferias_dados_brutos %>% select(any_of(lista_campos))
)

# Verifique como ficou
glimpse(sedu_completa)

sedu_completa %>% count(fonte)

sedu_completa %>% writexl::write_xlsx(
  'data/sedu_completa.xlsx'
)

drive_upload(
  "data/sedu_completa.xlsx",
  path = as_id(FOLDER_ID),
  overwrite = TRUE
)

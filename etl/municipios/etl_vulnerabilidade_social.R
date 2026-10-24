# bibliotecas ----
library(data.table)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)

# ------------------------------------------------------------------------------
# lINK DE ACESSO AO POWER BI DE CADUNICO 
# https://paineis.mds.gov.br/public/extensions/observatorio-do-cadastro-unico/index.html#tabela_familias_pessoas
# ------------------------------------------------------------------------------


# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# 1. Autenticação via googledrive ----
json_auth <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(json_auth)) {
  drive_auth(path = json_auth)
  gs4_auth(path = json_auth)
}

# extrair e tratar ----
## população (ibge) ----
dados_populacao_mun <- ibge_variables(6579, localities = "N6")

dados_populacao_mun_pe <- dados_populacao_mun %>% 
  mutate(
    period = as.integer(period),
    value = as.integer(value)
  ) %>%
  filter(
    substr(as.character(locality_id), 1, 2) == "26",
    period == max(period)
    ) %>% 
  mutate(codigo = as.integer(locality_id))

dados_populacao_mun_pe %>% glimpse()
dados_populacao_mun_pe %>% View()

## pessoas no cad único ----
# df_cad_pessoas <- fread(
#   'data/vulnerabilidade/cad_pessoas.csv',
#   encoding = 'Latin-1'
# ) %>%
#   clean_names() %>%
#   filter(
#     uf == 'PE'
#   ) %>%
#   rename(
#     #pessoas_cad_unico = pessoas_cad_unico
#     pessoas_cad_unico = numero_total_de_pessoas_cadastradas_no_cadastro_unico
#   )

df_cad_pessoas <- readxl::read_xlsx(file.path(DIR_RAW_AURORA, 'vulnerabilidade/cad_unico.xlsx')) %>%
  clean_names() %>%
  mutate(codigo = as.integer(cod_ibge), 
         pessoas_cad_unico = pessoas) %>%
  filter(str_starts(as.character(codigo), "26")) %>%
  select(-cod_ibge, -pessoas)

df_cad_pessoas %>% glimpse()

df_cad_pessoas_2 <- df_cad_pessoas %>%
  left_join(
    dados_populacao_mun_pe,
      # mutate(codigo = as.integer(str_sub(locality_id, 1, 6))),
    by = "codigo"
  ) %>%
  mutate(
    tx_pessoas_cad_unico = pessoas_cad_unico / value, 
    unidade_territorial = toupper(municipio)
  ) %>%
  mutate(uf = "PE") %>%
  select(
    codigo, unidade_territorial, uf, referencia, pessoas_cad_unico, tx_pessoas_cad_unico
  )

df_cad_pessoas_2 %>% glimpse()

## rede de água ----
df_agua_rede <- fread(
  file.path(DIR_RAW_AURORA, 'vulnerabilidade/cad_agua_rede.csv'),
  encoding = 'Latin-1'
) %>% 
  clean_names() %>%
  rename(
    'agua_distribuicao' = 'quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_rede_geral_de_distribuicao',
    'agua_cisterna' = 'quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_cisterna',
    'agua_poco' = 'quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_poco_ou_nascente',
    'agua_outros' = 'quantidade_de_familias_cadastradas_em_domicilios_com_abastecimento_de_agua_por_outras_formas',
    'agua_NA' = 'quantidade_de_familias_cadastradas_em_domicilios_sem_informacao_sobre_a_forma_de_abastecimento_de_agua'
  ) %>%
  mutate(
    total_domicilios_agua = agua_distribuicao + agua_poco + agua_cisterna + agua_outros + agua_NA,
    pct_agua_distribuicao = round((agua_distribuicao / total_domicilios_agua) * 100, 2)
  ) %>%
  filter(
    uf == 'PE'
  )

# Conferindo os dados gerados
df_agua_rede %>% glimpse()
df_agua_rede %>% View()

## esgoto ----
df_agua_esgoto <- fread(
  file.path(DIR_RAW_AURORA, 'vulnerabilidade/cad_agua_esgoto.csv'),
  encoding = 'Latin-1'
) %>% 
  clean_names() %>%
  rename(
    'esgoto_distribuicao' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_rede_coletora_de_esgoto_ou_pluvial',
    'esgoto_fossa_septica' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_fossa_septica',
    'esgoto_fossa_rudimentar' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_fossa_rudimentar',
    'esgoto_vala' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_vala_a_ceu_aberto',
    'esgoto_rio_lago_mar' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_direto_para_rio_lago_ou_mar',
    'esgoto_outros' = 'quantidade_de_familias_cadastradas_em_domicilios_com_escoamento_sanitario_por_outras_formas',
    'esgoto_NA' = 'quantidade_de_familias_cadastradas_em_domicilios_sem_informacao_sobre_a_forma_de_escoamento_sanitario'
  ) %>%
  # Calculando o indicador de esgoto
  mutate(
    total_domicilios_esgoto = esgoto_distribuicao + esgoto_fossa_septica + esgoto_fossa_rudimentar + esgoto_vala + esgoto_rio_lago_mar + esgoto_outros + esgoto_NA,
    pct_esgoto_rede = round((esgoto_distribuicao / total_domicilios_esgoto) * 100, 2)
  ) %>%
  filter(
    uf == 'PE'
  )

df_agua_esgoto %>% glimpse()

# salvar ----
# ID da pasta do Google Drive
id_pasta_drive <- as_id("11SbyKS9ZFN5mCDpIKorUmWEePIaMlQA-")

## subir pessoas no cad unico ----
arquivo_temp_cad_pessoas <- tempfile(fileext = ".csv")
write.csv(df_cad_pessoas_2, arquivo_temp_cad_pessoas, row.names = FALSE)

drive_upload(
  media = arquivo_temp_cad_pessoas,
  path = id_pasta_drive,
  name = "indicador_cad_pessoas_pe.csv",
  overwrite = TRUE
)

## subir dados de água ----
arquivo_temp_agua <- tempfile(fileext = ".csv")
write.csv(df_agua_rede, arquivo_temp_agua, row.names = FALSE)

drive_upload(
  media = arquivo_temp_agua,
  path = id_pasta_drive,
  name = "indicador_agua_pe.csv",
  overwrite = TRUE
)

## subir dados de esgoto ----
arquivo_temp_esgoto <- tempfile(fileext = ".csv")
write.csv(df_agua_esgoto, arquivo_temp_esgoto, row.names = FALSE)

drive_upload(
  media = arquivo_temp_esgoto,
  path = id_pasta_drive,
  name = "indicador_esgoto_pe.csv",
  overwrite = TRUE
)

print("Arquivos de vulnerabilidade social enviados com sucesso para o Drive!")
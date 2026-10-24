# bibliotecas ----
library(data.table)
library(dplyr)
library(googledrive)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)

# 1. Autenticação via googledrive ----
drive_auth(
  path = file.path("data", "r-sheets-hugo-0788cc358cb5.json")
)

# extrair e tratar ----
## rede de água ----
df_agua_rede <- fread(
  'data/vulnerabilidade/cad_agua_rede.csv',
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

## esgoto ----
df_agua_esgoto <- fread(
  'data/vulnerabilidade/cad_agua_esgoto.csv',
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

print("Arquivos de água e esgoto enviados com sucesso para o Drive!")
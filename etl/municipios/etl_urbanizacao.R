# ==============================================================================
# SCRIPT: etl_urganizacao.R
# ==============================================================================

library(sidrar)
library(janitor)
library(dplyr)
library(googledrive)
library(googlesheets4)

# Validação do Sistema Operacional
is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  # Configuração de encoding/locale para tratamento correto de acentuação no Linux
  Sys.setlocale(locale = "pt_BR.utf8")
}

# 1. autenticação ----
json_auth <- file.path("data", "r-sheets-hugo-0788cc358cb5.json")
drive_auth(path = json_auth)
gs4_auth(path = json_auth)

DIR_DADOS_AURORA_MUNI <- ifelse(is_linux, "/srv/dados/aurora", file.path(getwd(), "app_aurora_municipios/data"))

# Montando Requisição  ----------------------------------------------------

#Exibindo informações sobre a tabela
info_sidra(9923)

#Captura as variáveis necessárias pelo código: var c(93, 1000093)
codigos_variavel <- c(93, 1000093)

#Faz a requisição pro Sidra com os parâmetros 
dados_brutos <- get_sidra(
  x = 9923,                             #Número da tabela
  variable = codigos_variavel,          #Código das variáveis 
  period = "2022",                      #Período 
  geo = "City",                         #Nível Geográfico (Municipio)
  geo.filter = list("State" = 26)       #Filtro para Pernambuco        
)

#Faz a limpeza dos nomes (retira acentos,espaços e letras maiusculas)
dados_limpos <- dados_brutos |> 
  clean_names()

#Aplica os filtros para Limpeza da tabela
dados_limpos <- dados_limpos |> 
  select(
    unidade_de_medida,        
    valor,       
    municipio_codigo,              
    municipio,
    ano,
    variavel,
    situacao_do_domicilio
  ) |> 
  mutate(
    # Limpa a sigla ' - PE' (ou qualquer outra ' - UF') do nome do município
    municipio = str_remove(municipio, " - [A-Z]{2}$"),
    
    # Converte valor para numérico e ajusta percentual se necessário
    valor = as.numeric(valor),
    valor = ifelse(unidade_de_medida == "Percentual", valor / 100, valor),
    
    orgao_fonte = "IBGE/SIDRA",
    
    # Cria nm_indicador e sigla_indicador com base na situação do domicílio
    nm_indicador = case_when(
      situacao_do_domicilio == "Urbana" ~ "População urbana",
      situacao_do_domicilio == "Rural"  ~ "População rural",
      TRUE ~ NA_character_
    ),
    sigla_indicador = case_when(
      situacao_do_domicilio == "Urbana" ~ "pop_urb",
      situacao_do_domicilio == "Rural"  ~ "pop_rur",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(
    !is.na(valor),
    unidade_de_medida == "Percentual"
  ) |> 
  mutate(
    cod_municipio = municipio_codigo, 
    nm_municipio = municipio, 
    indicador = valor
  )  |> 
  filter(!is.na(nm_indicador)) |> 
  select(cod_municipio,nm_municipio, indicador, ano, nm_indicador, sigla_indicador)

# 4. exportação aurora_municipios ----
caminho_local_pop_muni <- file.path(DIR_DADOS_AURORA_MUNI, "dados_urbanizacao_mun_pe.xlsx")
caminho_rds_pop_muni   <- file.path(DIR_DADOS_AURORA_MUNI, "dados_urbanizacao_mun_pe.rds")

writexl::write_xlsx(dados_limpos, caminho_local_pop_muni)
readr::write_rds(dados_limpos, caminho_rds_pop_muni)

# ID da sua planilha Google Sheets
sheet_id <- "1eoH5dTmx5xipoV_plL7jIPB5yvQNHx1rOqIk3_UZM9o"

# Escreve o data frame limpo na planilha
sheet_write(
  data = dados_limpos,
  ss = sheet_id,
  sheet = "censo_2022"  # Nome da aba/página onde os dados serão salvos
)

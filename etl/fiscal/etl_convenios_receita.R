# bibliotecas ----
library(BigDataPE)
library(bslib)
library(fs)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(purrr)
library(readxl)
library(sf)
library(stringi)
library(stringr)
library(tidyverse)
library(vialactea)

options(scipen = 999)

# utils ----
normalize_ascii <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# setup ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

utils_path <- if (file.exists("R/utils.R")) "R/utils.R" else if (file.exists("../R/utils.R")) "../R/utils.R" else "utils.R"
if (file.exists(utils_path)) source(utils_path)

config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) file.path(DIR_RAW_AURORA, "config.yml") else "config.yml"
config_tokens <- if (file.exists(config_path)) config::get('tokens', file = config_path) else NULL

valid_status <- c(
  "Em formulação", "Em elaboração de projeto", "Concluída", "A licitar",
  "Em licitação", "A iniciar", "Em execução", "Paralisada", "Cancelada"
)

valid_eixo <- c("Conhecimento e inovação", "Desenvolvimento sustentável", "Saúde e qualidade de vida", "Segurança e cidadania")

token_eco_path <- file.path(DIR_RAW_AURORA, "token_eco-condition-406519-adac7b9fb8e5.json")
if (file.exists(token_eco_path)) {
  drive_auth(cache = FALSE, path = token_eco_path)
}

url_convenios_receita <- "https://docs.google.com/spreadsheets/d/1b3Z1m80-o-L1hpUqX7kP5fEprIeCHyHy7jwBE_ZVPGc/edit?gid=739325674#gid=739325674"

df_convenios_receita <- read_sheet(
  ss = url_convenios_receita
) %>%
  janitor::clean_names() %>%
  filter(!is.na(modalidade_do_instrumento))  %>%
  mutate(
    across(
      where(is.list),
      ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
    )) %>%
  mutate(
    # modalidade 
    modalidade_do_instrumento = coalesce(toupper(modalidade_do_instrumento), "OUTRAS TRANSFERÊNCIAS"),
    modalidade_do_instrumento = case_when(
      modalidade_do_instrumento %in% c('CONTRATO DE REPASSE', 'REPASSE') ~ 'REPASSE',
      modalidade_do_instrumento %in% c('CONTRATO DE FINANCIAMENTO') ~ 'FINANCIAMENTO',
      .default = modalidade_do_instrumento
    ),
    # ministério
    ministerio = orgao_concedente,
    ministerio = case_when(
      ministerio %in% c('CIDADES', 'MINISTÉRIO DAS CIDADES', 'MCID') ~ 'MCID',
      ministerio %in% c('CULTURA', 'MINISTÉRIO DA CULTURA') ~ 'MinC',
      ministerio %in% c('MINISTÉRIO DE PORTOS E AEROPORTOS - MPOR', 'SECRETARIA DE AVIAÇÃO CIVIL - SAC') ~ 'MPOR',
      ministerio %in% c('MEC', 'INEP') ~ 'MEC',
      ministerio %in% c('MINISTÉRIO DA SAÚDE', 'FUNASA') ~ 'MS',
      ministerio %in% c('MINISTÉRIO DO TURISMO') ~ 'MTur',
      ministerio %in% c('JUSTIÇA') ~ 'MJSP',
      ministerio %in% c('MDR/CAIXA', 'MIDR') ~ 'MIDR',
      ministerio %in% c('MINISTÉRIOS DOS TRANSPORTES-DNIT') ~ 'MTR',
      is.na(ministerio) ~ "NI",
      .default = ministerio
    ),
    # situação
    situacao = toupper(situacao),
    status = case_when(
      situacao %in% c('CLÁUSULA SUSPENSIVA', 'CONDIÇÃO SUSPENSIVA', 'MONITORAMENTO SUSPENSO') ~ 'SUSPENSO',
      situacao %in% c('CONVÊNIO ANULADO') ~ 'ANULADO',
      situacao %in% c('EM LICITAÇÃO', 'EM LICITAÇÃO/ EM CONTRATAÇÃO') ~ 'EM LICITAÇÃO',
      situacao %in% c('EM PRESTAÇÃO DE CONTAS', 'PREST. CONTAS EM ANÁLISE', 'PRESTAÇÃO DE CONTAS EM COMPLEMENTAÇÃO', 'PRESTAÇÃO DE CONTAS ENVIADA PARA ANÁLISE', 'PRESTAÇÃO DE CONTAS', 'PRESTAÇÃO DE CONTAS EM ANÁLISE') ~ 'EM PRESTAÇÃO DE CONTAS',
      situacao %in% c('AGUARDANDO PRESTAÇÃO DE CONTAS', 'PREST. CONTAS EM ABERTO', 'OBRA CONCLUÍDA AGUARDANDO RAE FINAL') ~ 'PRESTAÇÃO DE CONTAS EM ABERTO',
      situacao %in% c('PRESTAÇÃO DE CONTAS CONCLUÍDA') ~ 'PRESTAÇÃO DE CONTAS CONCLUÍDA',
      situacao %in% c('EM REPROGRAMAÇÃO NA CAIXA', 'REPROGRAMAÇÃO APROVADA C.E. 1621/2026') ~ 'REPROGRAMAÇÃO',
      situacao %in% c('EDITAL PUBLICADO NO DOE EM 16/01/2026 ABERTURA EM 02/04/2026 P.L. Nº 003/2026', 'HOMOLOGADO EM 09/02/2026') ~ 'ASSINADO',
      situacao %in% c('LICITADO', 'EM EXECUÇÃO') ~ 'EM EXECUÇÃO',
      situacao %in% c('NÃO INICIADA') ~ 'A INICIAR',
      situacao %in% c('PROPOSTA E PLANO DE TRABALHO APROVADO') ~ 'APROVADO',
      is.na(situacao) ~ "NÃO INFORMADO",
      .default = situacao
    )
  )

df_convenios_receita %>% distinct(orgao_executor) %>% arrange(orgao_executor)

df_convenios_receita %>% glimpse()

# Exportação unificada
write_rds(df_convenios_receita, file.path(DIR_OUT_AURORA, "df_convenios_receita.rds"))


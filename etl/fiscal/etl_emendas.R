# ==============================================================================
# SCRIPT: etl_emendas_pe.R
# Objetivo: Extração, tratamento e enriquecimento de dados de emendas parlamentares
#           estaduais de Pernambuco. Tradução/resumo de objetos, integração com
#           código IBGE e exportação unificada (RDS e Parquet).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(arrow)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)
library(readxl)
library(sf)
library(stringr)
library(stringi)
library(writexl)
library(cli)
library(readr)

is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

# ------------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
norm_mun <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    trimws()
}

# Busca hierárquica pelo centralizador paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))                DIR_ETL                <- getwd()
if (!exists("DIR_RAW_AURORA"))         DIR_RAW_AURORA         <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA"))         DIR_OUT_AURORA         <- file.path(DIR_ETL, "data")
if (!exists("DIR_DADOS_AURORA_MUNI"))  DIR_DADOS_AURORA_MUNI  <- DIR_OUT_AURORA

# Criação de diretórios de saída locais
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir_efisco_raw  <- file.path(DIR_RAW_AURORA, "efisco")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_efisco_raw, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. AUTENTICAÇÃO GOOGLE
# ------------------------------------------------------------------------------
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
} else {
  "data/r-sheets-hugo-0788cc358cb5.json"
}

if (file.exists(auth_path)) {
  drive_auth(path = auth_path)
  cli_alert_success("Autenticação Google realizada via {.path {auth_path}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {auth_path}}")
}

# ------------------------------------------------------------------------------
# 4. EXTRAÇÃO DOS DADOS BRUTOS
# ------------------------------------------------------------------------------
id_arquivo <- "1lanBtudgOXobGDRLjtCM6s4kXbe-eXvX"
caminho_temporario <- tempfile(fileext = ".xlsx")

tryCatch({
  drive_download(
    file      = googledrive::as_id(id_arquivo),
    path      = caminho_temporario,
    overwrite = TRUE
  )
  cli_alert_success("Download da base bruta de emendas concluído.")
}, error = function(e) {
  cli_abort("Falha ao baixar arquivo bruto de emendas do Drive: {.val {e$message}}")
})

dados_emendas_raw <- read_excel(
  path      = caminho_temporario,
  sheet     = "BD Geral",
  skip      = 1,
  col_types = "text"
) %>%
  clean_names() %>%
  select(-c(starts_with("x"))) %>%
  mutate(ano = as.numeric(str_extract(categoria, "\\d{4}")))

# ------------------------------------------------------------------------------
# 5. CARREGAR OU RECONSTRUIR DICIONÁRIO DE OBJETOS
# ------------------------------------------------------------------------------
caminho_traducao <- file.path(DIR_RAW_AURORA, "efisco/objetos_emendas.xlsx")

if (file.exists(caminho_traducao) && file.size(caminho_traducao) > 100) {
  df_traducao <- read_excel(caminho_traducao, col_types = "text") %>%
    mutate(ano = as.numeric(ano))
  
  if (!"ano" %in% names(df_traducao) || all(is.na(df_traducao$ano))) {
    anos_para_atualizar <- dados_emendas_raw %>%
      group_by(objeto) %>%
      summarise(ano = max(ano, na.rm = TRUE), .groups = "drop")
    
    df_traducao <- df_traducao %>%
      select(-any_of("ano")) %>%
      left_join(anos_para_atualizar, by = "objeto") %>%
      relocate(ano, .before = objeto)
    
    write_xlsx(df_traducao, caminho_traducao)
  }
} else {
  df_traducao <- dados_emendas_raw %>%
    group_by(objeto) %>%
    summarise(ano = max(ano, na.rm = TRUE), .groups = "drop") %>%
    select(ano, objeto) %>%
    mutate(
      objeto_simples = NA_character_,
      classe         = "Não"
    ) %>%
    arrange(desc(ano))
  
  write_xlsx(df_traducao, caminho_traducao)
  cli_alert_success("Planilha de tradução reconstruída com sucesso em {.path {caminho_traducao}}.")
}

# Identificação e inclusão de novos objetos
novos_objetos <- dados_emendas_raw %>%
  anti_join(df_traducao, by = "objeto") %>%
  group_by(objeto) %>%
  summarise(ano = max(ano, na.rm = TRUE), .groups = "drop") %>%
  select(ano, objeto) %>% 
  mutate(
    objeto_simples = NA_character_, 
    classe         = "Não"                  
  )

if (nrow(novos_objetos) > 0) {
  df_traducao <- bind_rows(df_traducao, novos_objetos) %>%
    arrange(desc(ano))
  
  write_xlsx(df_traducao, caminho_traducao)
  cli_alert_info("{nrow(novos_objetos)} novos objetos adicionados à planilha de tradução.")
}

# ------------------------------------------------------------------------------
# 6. DICIONÁRIO DE CÓDIGOS IBGE
# ------------------------------------------------------------------------------
caminho_demo <- if (file.exists(file.path(DIR_OUT_AURORA, "df_demografia_pe.rds"))) {
  file.path(DIR_OUT_AURORA, "df_demografia_pe.rds")
} else if (file.exists(file.path(DIR_RAW_AURORA, "ibge/df_demografia_pe.rds"))) {
  file.path(DIR_RAW_AURORA, "ibge/df_demografia_pe.rds")
} else {
  file.path(DIR_RAW_AURORA, "df_demografia_pe.rds")
}

if (file.exists(caminho_demo)) {
  df_demo <- readr::read_rds(caminho_demo)
  
  # Remove geometria caso venha como sf
  if (inherits(df_demo, "sf")) {
    df_demo <- sf::st_drop_geometry(df_demo)
  }
  
  dicionario_ibge <- df_demo %>%
    select(municipio, ibge7) %>%
    mutate(
      municipio_norm = norm_mun(municipio),
      cod_ibge       = substr(as.character(ibge7), 1, 6),
      ibge6          = substr(as.character(ibge7), 1, 6)
    ) %>%
    select(municipio_norm, cod_ibge, ibge6) %>%
    distinct()
  cli_alert_success("Dicionário IBGE carregado a partir de demografia.")
} else {
  cli_alert_warning("Arquivo demográfico não encontrado. O {.var cod_ibge} ficará vazio.")
  dicionario_ibge <- tibble::tibble(
    municipio_norm = character(), 
    cod_ibge       = character(), 
    ibge6          = character()
  )
}

# ------------------------------------------------------------------------------
# 7. TRATAMENTO E ENRIQUECIMENTO DOS DADOS
# ------------------------------------------------------------------------------
dados_emendas <- dados_emendas_raw %>%
  left_join(
    df_traducao %>% 
      distinct(objeto, .keep_all = TRUE) %>% 
      select(objeto, objeto_simples), 
    by = "objeto"
  ) %>%
  mutate(
    parlamentar = parlamentar %>%
      toupper() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_squish(),
    municipio = case_when(
      municipio == "DISTRITO ESTADUAL DE FERNANDO DE NORONHA" ~ "FERNANDO DE NORONHA",
      municipio == "LAGOA DO ITAENGA"                         ~ "LAGOA DE ITAENGA",
      municipio == "BELEM DE SAO FRANCISCO"                   ~ "BELEM DO SAO FRANCISCO",
      municipio == "CALCADOS"                                 ~ "CALCADO",
      municipio == "IGUARACI"                                 ~ "IGUARACY",
      municipio == "ITAMARACA"                                ~ "ILHA DE ITAMARACA",
      .default = municipio
    ),
    municipio_norm  = norm_mun(municipio), 
    objeto_original = objeto,
    objeto          = if_else(is.na(objeto_simples) | objeto_simples == "", objeto_original, objeto_simples)
  ) %>%
  select(-objeto_simples) %>%
  left_join(dicionario_ibge, by = "municipio_norm") %>%
  select(-municipio_norm)

ano_vigente <- lubridate::year(lubridate::today())
dados_emendas_atual <- dados_emendas %>% filter(ano == ano_vigente)

cli_alert_success("Base de emendas consolidada ({nrow(dados_emendas)} registros totais; {nrow(dados_emendas_atual)} em {ano_vigente}).")

# ------------------------------------------------------------------------------
# 8. EXPORTAÇÃO LOCAL (RDS & PARQUET)
# ------------------------------------------------------------------------------
# Base histórica consolidada
readr::write_rds(dados_emendas, file.path(DIR_OUT_AURORA, "dados_emendas.rds"))
arrow::write_parquet(dados_emendas, file.path(dir_parquet_out, "dados_emendas.parquet"), compression = "snappy")
arrow::write_parquet(dados_emendas, file.path(DIR_OUT_AURORA, "dados_emendas.parquet"), compression = "snappy")

# Base do ano vigente
readr::write_rds(dados_emendas_atual, file.path(DIR_OUT_AURORA, "dados_emendas_atual.rds"))
arrow::write_parquet(dados_emendas_atual, file.path(dir_parquet_out, "dados_emendas_atual.parquet"), compression = "snappy")
arrow::write_parquet(dados_emendas_atual, file.path(DIR_OUT_AURORA, "dados_emendas_atual.parquet"), compression = "snappy")

cli_alert_success("Arquivos locais (RDS e Parquet) gravados em {.path {DIR_OUT_AURORA}}.")

# ------------------------------------------------------------------------------
# 9. CONTROLE DE QUALIDADE E AUDITORIA
# ------------------------------------------------------------------------------
total_registros    <- nrow(dados_emendas)
resumidos_qtd      <- sum(dados_emendas$objeto_original != dados_emendas$objeto, na.rm = TRUE)
resumidos_pct      <- round((resumidos_qtd / total_registros) * 100, 1)

chars_antes        <- sum(nchar(dados_emendas$objeto_original), na.rm = TRUE)
chars_depois       <- sum(nchar(dados_emendas$objeto), na.rm = TRUE)
red_absoluta_chars <- chars_antes - chars_depois
red_pct_chars      <- round((red_absoluta_chars / chars_antes) * 100, 1)

cli_rule("Controle de Qualidade de Texto")
cli_alert_info("Objetos resumidos: {resumidos_qtd} de {total_registros} ({resumidos_pct}%)")
cli_alert_info("Redução absoluta: {format(red_absoluta_chars, big.mark = '.', decimal.mark = ',')} caracteres a menos")
cli_alert_info("Redução percentual: {red_pct_chars}% de enxugamento textual")

emendas_sem_ibge <- dados_emendas %>% 
  filter(is.na(cod_ibge) & !is.na(municipio) & !municipio %in% c("MÚLTIPLOS", "-", "MULTlPLOS")) %>% 
  count(municipio, sort = TRUE)

if (nrow(emendas_sem_ibge) > 0) {
  cli_alert_danger("Municípios sem mapeamento de código IBGE identificados:")
  print(emendas_sem_ibge)
} else {
  cli_alert_success("Todos os municípios válidos foram mapeados com sucesso ao código IBGE.")
}

cli_alert_success("ETL de Emendas concluído com sucesso!")
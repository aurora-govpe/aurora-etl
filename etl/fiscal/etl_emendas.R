# bibliotecas ----
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

is_linux <- Sys.info()["sysname"] == "Linux"

if (is_linux) Sys.setlocale(locale = "pt_BR.utf8")

# utils ----
norm_mun <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    trimws()
}

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# Autenticação ----
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(auth_path)) drive_auth(path = auth_path)

DIR_DADOS_AURORA_MUNI <- DIR_OUT_AURORA

# 1. EXTRAIR DADOS BRUTOS ----
id_arquivo <- "1lanBtudgOXobGDRLjtCM6s4kXbe-eXvX"
caminho_temporario <- tempfile(fileext = ".xlsx")

drive_download(
  file = as_id(id_arquivo),
  path = caminho_temporario,
  overwrite = TRUE
)

dados_emendas_raw <- read_excel(
  path = caminho_temporario,
  sheet = 'BD Geral',
  skip = 1,
  col_types = "text"
) %>%
  clean_names() %>%
  select(-c(starts_with('x'))) %>%
  mutate(ano = as.numeric(str_extract(categoria, "\\d{4}")))

# 2. CARREGAR OU RECONSTRUIR DICIONÁRIO ----
caminho_traducao <- file.path(DIR_RAW_AURORA, "efisco/objetos_emendas.xlsx")

if (file.exists(caminho_traducao) && file.size(caminho_traducao) > 100) {
  df_traducao <- read_excel(caminho_traducao, col_types = "text") %>%
    mutate(ano = as.numeric(ano))
  
  # AJUSTE 1: Se o arquivo antigo não tinha 'ano', preenche com os anos reais e grava no disco
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
  dir.create(dirname(caminho_traducao), recursive = TRUE, showWarnings = FALSE)
  
  df_traducao <- dados_emendas_raw %>%
    group_by(objeto) %>%
    summarise(ano = max(ano, na.rm = TRUE), .groups = "drop") %>%
    select(ano, objeto) %>%
    mutate(
      objeto_simples = NA_character_,
      classe = "Não"
    ) %>%
    arrange(desc(ano))
  
  write_xlsx(df_traducao, caminho_traducao)
  message(">>> PLANILHA RECONSTRUÍDA COM SUCESSO A PARTIR DA BASE BRUTA!")
}

# 3. IDENTIFICAR E ANEXAR NOVOS OBJETOS ----
novos_objetos <- dados_emendas_raw %>%
  anti_join(df_traducao, by = "objeto") %>%
  group_by(objeto) %>%
  summarise(ano = max(ano, na.rm = TRUE), .groups = "drop") %>%
  select(ano, objeto) %>% 
  mutate(
    objeto_simples = NA_character_, 
    classe = "Não"                  
  )

if (nrow(novos_objetos) > 0) {
  df_traducao <- bind_rows(df_traducao, novos_objetos) %>%
    arrange(desc(ano))
  
  write_xlsx(df_traducao, caminho_traducao)
  message(paste(">>>", nrow(novos_objetos), "novos objetos adicionados com segurança à planilha!"))
}

# 3.5 CRIAR DICIONÁRIO DE IBGE A PARTIR DA DEMOGRAFIA ----
caminho_demo <- if (file.exists(file.path(DIR_OUT_AURORA, "df_demografia_pe.rds"))) file.path(DIR_OUT_AURORA, "df_demografia_pe.rds") else file.path(DIR_RAW_AURORA, "df_demografia_pe.rds")

if (file.exists(caminho_demo)) {
  df_demo <- readRDS(caminho_demo)
  
  dicionario_ibge <- df_demo %>%
    # Se for objeto sf (mapa), dropa a geometria para não pesar na memória
    st_drop_geometry() %>% 
    select(municipio, ibge7) %>%
    mutate(
      municipio_norm = norm_mun(municipio),
      # Extrai os 6 primeiros dígitos para o cod_ibge
      cod_ibge = substr(as.character(ibge7), 1, 6),
      ibge6 = substr(as.character(ibge7), 1, 6)
    ) %>%
    select(municipio_norm, cod_ibge, ibge6) %>%
    distinct()
} else {
  warning("Arquivo de demografia não encontrado. O cod_ibge ficará vazio.")
  dicionario_ibge <- tibble(municipio_norm = character(), cod_ibge = character())
}

# 4. PIPELINE PRINCIPAL ----
dados_emendas <- dados_emendas_raw %>%
  # Previne duplicidade de linhas caso o Excel tenha entradas repetidas
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
      municipio == 'DISTRITO ESTADUAL DE FERNANDO DE NORONHA' ~ 'FERNANDO DE NORONHA',
      municipio == 'LAGOA DO ITAENGA' ~ 'LAGOA DE ITAENGA',
      municipio == 'BELEM DE SAO FRANCISCO' ~ 'BELEM DO SAO FRANCISCO',
      municipio == 'CALCADOS' ~ 'CALCADO',
      municipio == 'IGUARACI' ~ 'IGUARACY',
      municipio == 'ITAMARACA' ~ 'ILHA DE ITAMARACA',
      .default = municipio
    ),
    # 1) Normaliza o nome do município no momento da limpeza
    #municipio_norm = norm_mun(municipio), 
    municipio_norm = municipio, 
    objeto_original = objeto,
    objeto = if_else(is.na(objeto_simples) | objeto_simples == "", objeto_original, objeto_simples)
  ) %>%
  select(-objeto_simples) %>%
  # 2) Faz o cruzamento (Join) com o dicionário do IBGE
  left_join(dicionario_ibge, by = "municipio_norm") %>%
  # 3) Remove a coluna auxiliar para não sujar a base final
  select(-municipio_norm)

# Exportação dos arquivos finais
saveRDS(dados_emendas, file.path(DIR_OUT_AURORA, 'dados_emendas.rds'))
write_parquet(dados_emendas, file.path(DIR_OUT_AURORA, "dados_emendas.parquet"))

dados_emendas %>%
  filter(ano == year(today())) %>%
  saveRDS(file.path(DIR_OUT_AURORA, 'dados_emendas_atual.rds'))

# controle de qualidade ----
total_registros   <- nrow(dados_emendas)
resumidos_qtd     <- sum(dados_emendas$objeto_original != dados_emendas$objeto, na.rm = TRUE)
resumidos_pct     <- round((resumidos_qtd / total_registros) * 100, 1)

chars_antes       <- sum(nchar(dados_emendas$objeto_original), na.rm = TRUE)
chars_depois      <- sum(nchar(dados_emendas$objeto), na.rm = TRUE)
red_absoluta_chars<- chars_antes - chars_depois
red_pct_chars     <- round((red_absoluta_chars / chars_antes) * 100, 1)

message("==================================================")
message(paste0(">>> OBJETOS RESUMIDOS: ", resumidos_qtd, " de ", total_registros, " (", resumidos_pct, "%)"))
message(paste0(">>> REDUÇÃO ABSOLUTA DE TEXTO: ", format(red_absoluta_chars, big.mark = ".", decimal.mark = ','), " caracteres a menos"))
message(paste0(">>> REDUÇÃO PERCENTUAL DE TEXTO: ", red_pct_chars, "% de enxugamento total"))
message("==================================================")

emendas_sem_ibge <- dados_emendas %>% 
  filter(is.na(cod_ibge) & !is.na(municipio) & municipio != "MÚLTIPLOS" & municipio != "-") %>% 
  count(municipio)

if(nrow(emendas_sem_ibge) > 0) {
  message(">>> ALERTA: Alguns municípios não cruzaram o código IBGE:")
  print(emendas_sem_ibge)
} else {
  message(">>> IBGE: Todos os municípios válidos foram mapeados com sucesso (6 dígitos).")
}

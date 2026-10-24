# pacotes ----
library(DBI)
library(dplyr)
library(googledrive)
library(RMariaDB)
library(sf)
library(stringi)
library(janitor)
library(writexl)
library(config)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# setup ----
config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) file.path(DIR_RAW_AURORA, "config.yml") else "config.yml"
config_pcp  <- if (file.exists(config_path)) config::get("pcp", file = config_path) else NULL

FOLDER_ID  <- "18L56UQcsM5PIzHA_WJMYhexRFctjNwCM"
AUTH_PATH  <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
LOCAL_PATH <- file.path(DIR_RAW_AURORA, "pcp")
RAW_CACHE  <- file.path(LOCAL_PATH, "dados_pcp_2026_raw.rds")

# Cria pasta local se não existir
if (!dir.exists(LOCAL_PATH)) {
  dir.create(LOCAL_PATH, recursive = TRUE)
}

if (file.exists(AUTH_PATH)) drive_auth(path = AUTH_PATH)

# mapa ----
pernambuco_sf <- readRDS(file.path(DIR_RAW_AURORA, 'pernambuco_sf.rds')) %>%
  sf::st_drop_geometry() %>%
  janitor::clean_names()

# --- 1. Leitura do Banco com Fallback em Disco ---
dados_pcp_2026 <- tryCatch({
  message("🔌 Conectando ao banco de dados MariaDB...")
  
  con <- dbConnect(
    RMariaDB::MariaDB(),
    host     = config_pcp$host,
    port     = 3306,
    user     = config_pcp$usr,
    password = config_pcp$pwd,
    dbname   = config_pcp$dbname
  )
  
  query <- "SELECT * FROM beneficiario2026"
  raw_db <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  message("✓ Consulta concluída. Processando codificação...")
  
  df_proc <- raw_db %>%
    mutate(across(where(is.character), ~stri_conv(., from = "UTF-8", to = "UTF-8"))) %>%
    mutate(
      across(
        .cols = starts_with("pergunta"),
        .fns = ~ case_when(
          .x == "MÃ©dio" ~ "Médio",
          .x == "Ã“timo" ~ "Ótimo",
          TRUE           ~ .x
        )
      ),
      segmento = case_when(
        programa == "CANA"  ~ "ZONA CANAVIEIRA",
        programa == "FRUTA" ~ "FRUTICULTURA",
        programa == "PESCA" ~ "PESCA ARTESANAL",
        TRUE                ~ programa
      )
    )
  
  # Salva cópia bruta/tratada no disco local
  saveRDS(df_proc, RAW_CACHE)
  message("💾 Backup dos dados brutos salvo em disco: ", RAW_CACHE)
  
  df_proc
  
}, error = function(e) {
  warning("⚠️ Falha na conexão/consulta ao banco de dados: ", e$message)
  
  if (file.exists(RAW_CACHE)) {
    message("⚡ Carregando última versão salva em disco: ", RAW_CACHE)
    readRDS(RAW_CACHE)
  } else {
    stop("❌ Erro fatal: Conexão falhou e nenhum backup local foi encontrado em ", RAW_CACHE)
  }
})

# dados_pcp_2026_teste <- dados_pcp_2026 %>%
#   filter(situacao == 'APROVADO') %>%
#   # filter(municipio == 2606903) %>%
#   select(cpf, everything()) %>%
#   select(cpf, segmento, municipio, valorparcela, dataatualizacao, datasincronizacao, datacadastro) %>%
#   distinct(cpf, .keep_all = T)

# dados_pcp_2026_teste <- raw_db %>%
#   filter(situacao == 'APROVADO') %>%
#   filter(municipio == 2600054) %>%
#   select(cpf, everything()) %>%
#   select(cpf, programa, municipio, valorparcela, dataatualizacao, datasincronizacao, datacadastro) %>%
#   distinct(cpf, .keep_all = T)
# 
# 
# 
#   group_by(segmento, municipio) %>%
#   summarise(
#     `VALOR INVESTIDO NO MUNICÍPIO` = sum(valorparcela, na.rm = TRUE),
#     `NÚMERO DE BENEFICIÁRIOS`      = n_distinct(cpf), 
#     .groups = 'drop'
#   ) %>%
#   mutate(
#     `% DO VALOR TOTAL`            = (`VALOR INVESTIDO NO MUNICÍPIO` / sum(`VALOR INVESTIDO NO MUNICÍPIO`)) * 100,
#     `PROPORÇÃO DO TOTAL DE CASOS` = (`NÚMERO DE BENEFICIÁRIOS` / sum(`NÚMERO DE BENEFICIÁRIOS`)) * 100,
#     # Ajuste de código IBGE incorreto e padronização para character
#     cod_ibge                      = as.character(ifelse(municipio == 4111100, 2607653, municipio))
#   )

# --- 2. Agrupamento Sintético e Georreferenciamento ---
dados_pcp_2026_sintetico <- dados_pcp_2026 %>%
  filter(situacao == 'APROVADO') %>%
  mutate(
    parcelas_totais   = 5,
    parcelas_pagas    = case_when(
      segmento == "ZONA CANAVIEIRA" ~ 5, 
      TRUE ~ 5
    ),
    valor_total       = valorparcela * parcelas_totais, 
    valor_pago        = valorparcela * parcelas_pagas,
    percent_executado = valor_pago / valor_total
  ) %>%
  select(segmento, parcelas_totais, parcelas_pagas, valor_total, valor_pago, percent_executado, everything()) %>%
  group_by(segmento, municipio) %>%
  summarise(
    `VALOR INVESTIDO NO MUNICÍPIO` = sum(valor_total, na.rm = TRUE),
    `VALOR PAGO NO MUNICÍPIO`      = sum(valor_pago, na.rm = TRUE),
    `% EXECUÇÃO`                   = (sum(valor_pago, na.rm = TRUE) / sum(valor_total, na.rm = TRUE)) * 100,
    `NÚMERO DE BENEFICIÁRIOS`      = n_distinct(cpf), 
    .groups = 'drop'
  ) %>%
  mutate(
    `% DO VALOR TOTAL`             = (`VALOR INVESTIDO NO MUNICÍPIO` / sum(`VALOR INVESTIDO NO MUNICÍPIO`)) * 100,
    `PROPORÇÃO DO TOTAL DE CASOS` = (`NÚMERO DE BENEFICIÁRIOS` / sum(`NÚMERO DE BENEFICIÁRIOS`)) * 100,
    # Ajuste de código IBGE incorreto e padronização para character
    cod_ibge                       = as.character(ifelse(municipio == 4111100, 2607653, municipio))
  )

# Cruzamento com a malha municipal para injetar o NOME e RD
df_pcp_final <- dados_pcp_2026_sintetico %>%
  left_join(
    pernambuco_sf %>%
      select(ibge7, nome_municipio = municipio, regiao_de_desenvolvimento),
    by = c('cod_ibge' = 'ibge7')
  ) %>%
  mutate(
    municipio = nome_municipio,
    rd        = regiao_de_desenvolvimento
  ) %>% 
  clean_names() %>% 
  mutate(status = case_when(percent_execucao == 100 ~ "Concluída", 
                            percent_execucao == 80 ~ "Em Execução"))

# --- 3. Exportação e Envio ---
planilha_pcp_path <- file.path(LOCAL_PATH, "SEPLAG_Beneficiários do Chapeú de Palha 2026.xlsx")
writexl::write_xlsx(df_pcp_final, planilha_pcp_path)

# Salva cópia na pasta raw
saveRDS(df_pcp_final, file.path(LOCAL_PATH, "df_pcp.rds"))

# Salva na pasta de saída única lida pelos apps
saveRDS(df_pcp_final, file.path(DIR_OUT_AURORA, "df_pcp.rds"))

# Upload para o Google Drive
if (file.exists(AUTH_PATH) && file.exists(planilha_pcp_path)) {
  drive_upload(
    planilha_pcp_path,
    path = as_id(FOLDER_ID),
    overwrite = TRUE
  )
}

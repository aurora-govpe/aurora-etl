# ==============================================================================
# SCRIPT: etl_pcp_2026.R
# Objetivo: Extração dos dados do Chapéu de Palha (MariaDB com fallback em RDS),
#           tratamento de codificação, cálculo sintético de execução orçamentária,
#           georreferenciamento com a malha municipal de PE, exportação local
#           (XLSX, RDS e Parquet) e sincronização com o Google Drive.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(DBI)
library(dplyr)
library(googledrive)
library(RMariaDB)
library(sf)
library(stringi)
library(janitor)
library(writexl)
library(config)
library(arrow)
library(readr)
library(cli)

is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Estrutura de pastas locais
LOCAL_PATH      <- file.path(DIR_RAW_AURORA, "pcp")
RAW_CACHE       <- file.path(LOCAL_PATH, "dados_pcp_2026_raw.rds")
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir_parquet_pcp <- file.path(LOCAL_PATH, "parquet")

dir.create(LOCAL_PATH,      recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_AURORA,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_pcp, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. AUTENTICAÇÃO E CONFIGURAÇÃO
# ------------------------------------------------------------------------------
config_path <- if (file.exists(file.path(DIR_RAW_AURORA, "config.yml"))) {
  file.path(DIR_RAW_AURORA, "config.yml")
} else if (file.exists("config.yml")) {
  "config.yml"
} else {
  NULL
}

config_pcp <- if (!is.null(config_path)) config::get("pcp", file = config_path) else NULL

FOLDER_ID <- "18L56UQcsM5PIzHA_WJMYhexRFctjNwCM"
AUTH_PATH <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
} else {
  "data/r-sheets-hugo-0788cc358cb5.json"
}

if (file.exists(AUTH_PATH)) {
  drive_auth(path = AUTH_PATH)
  cli_alert_success("Autenticação Google Drive realizada via {.path {AUTH_PATH}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {AUTH_PATH}}")
}

# ------------------------------------------------------------------------------
# 4. CARGA DA MALHA TERRITORIAL
# ------------------------------------------------------------------------------
caminho_sf <- if (file.exists(file.path(DIR_RAW_AURORA, "pernambuco_sf.rds"))) {
  file.path(DIR_RAW_AURORA, "pernambuco_sf.rds")
} else if (file.exists(file.path(DIR_OUT_AURORA, "df_demografia_pe.rds"))) {
  file.path(DIR_OUT_AURORA, "df_demografia_pe.rds")
} else {
  file.path(DIR_RAW_AURORA, "ibge/df_demografia_pe.rds")
}

if (!file.exists(caminho_sf)) {
  cli_abort("Arquivo com malha de Pernambuco não localizado: {.path {caminho_sf}}")
}

pernambuco_sf <- readr::read_rds(caminho_sf)
if (inherits(pernambuco_sf, "sf")) {
  pernambuco_sf <- sf::st_drop_geometry(pernambuco_sf)
}
pernambuco_sf <- pernambuco_sf %>% janitor::clean_names()

# ------------------------------------------------------------------------------
# 5. EXTRAÇÃO DO BANCO MARIADB (COM FALLBACK EM DISCO)
# ------------------------------------------------------------------------------
cli_alert_info("Iniciando extração do banco MariaDB (PCP 2026)...")

dados_pcp_2026 <- tryCatch({
  if (is.null(config_pcp)) {
    stop("Configuração 'pcp' não encontrada no arquivo config.yml")
  }
  
  con <- dbConnect(
    RMariaDB::MariaDB(),
    host     = config_pcp$host,
    port     = 3306,
    user     = config_pcp$usr,
    password = config_pcp$pwd,
    dbname   = config_pcp$dbname
  )
  on.exit(dbDisconnect(con), add = TRUE)
  
  query <- "SELECT * FROM beneficiario2026"
  raw_db <- dbGetQuery(con, query)
  
  cli_alert_success("Consulta MariaDB concluída ({nrow(raw_db)} registros). Tratando codificação...")
  
  df_proc <- raw_db %>%
    mutate(across(where(is.character), ~stri_conv(., from = "UTF-8", to = "UTF-8"))) %>%
    mutate(
      across(
        .cols = starts_with("pergunta"),
        .fns  = ~ case_when(
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
  
  # Grava backup dos dados brutos/tratados
  readr::write_rds(df_proc, RAW_CACHE)
  arrow::write_parquet(df_proc, file.path(LOCAL_PATH, "dados_pcp_2026_raw.parquet"), compression = "snappy")
  cli_alert_success("Backup local bruto gravado em: {.path {RAW_CACHE}}")
  
  df_proc
  
}, error = function(e) {
  cli_alert_warning("Falha na consulta MariaDB ({e$message}).")
  
  if (file.exists(RAW_CACHE)) {
    cli_alert_info("Carregando último backup salvo em disco: {.path {RAW_CACHE}}")
    readr::read_rds(RAW_CACHE)
  } else {
    cli_abort("Conexão falhou e nenhum backup local foi encontrado em {.path {RAW_CACHE}}")
  }
})

# ------------------------------------------------------------------------------
# 6. AGRUPAMENTO SINTÉTICO E GEORREFERENCIAMENTO
# ------------------------------------------------------------------------------
dados_pcp_2026_sintetico <- dados_pcp_2026 %>%
  filter(situacao == "APROVADO") %>%
  mutate(
    parcelas_totais   = 5,
    parcelas_pagas    = case_when(
      segmento == "ZONA CANAVIEIRA" ~ 5,
      TRUE                          ~ 5
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
    .groups                        = "drop"
  ) %>%
  mutate(
    `% DO VALOR TOTAL`            = (`VALOR INVESTIDO NO MUNICÍPIO` / sum(`VALOR INVESTIDO NO MUNICÍPIO`)) * 100,
    `PROPORÇÃO DO TOTAL DE CASOS` = (`NÚMERO DE BENEFICIÁRIOS` / sum(`NÚMERO DE BENEFICIÁRIOS`)) * 100,
    cod_ibge                      = as.character(ifelse(municipio == 4111100, 2607653, municipio))
  )

# Cruzamento com a malha municipal (injetando NOME e Região de Desenvolvimento)
col_ibge_sf <- intersect(names(pernambuco_sf), c("ibge7", "cd_mun", "cod_ibge"))[1]
col_mun_sf  <- intersect(names(pernambuco_sf), c("municipio", "nome_municipio", "nm_mun"))[1]
col_rd_sf   <- intersect(names(pernambuco_sf), c("regiao_de_desenvolvimento", "rd"))[1]

df_pcp_final <- dados_pcp_2026_sintetico %>%
  left_join(
    pernambuco_sf %>%
      select(
        ibge_join                 = all_of(col_ibge_sf),
        nome_municipio_ref        = all_of(col_mun_sf),
        regiao_desenvolvimento_ref = any_of(col_rd_sf)
      ) %>%
      mutate(ibge_join = as.character(ibge_join)),
    by = c("cod_ibge" = "ibge_join")
  ) %>%
  mutate(
    municipio = coalesce(nome_municipio_ref, as.character(municipio)),
    rd        = regiao_desenvolvimento_ref
  ) %>%
  select(-any_of(c("nome_municipio_ref", "regiao_desenvolvimento_ref"))) %>%
  clean_names() %>%
  mutate(
    status = case_when(
      percent_execucao == 100 ~ "Concluída",
      percent_execucao >= 80  ~ "Em Execução",
      TRUE                    ~ "Em Planejamento"
    )
  )

cli_alert_success("Base sintética do Chapéu de Palha consolidada ({nrow(df_pcp_final)} registros).")

# ------------------------------------------------------------------------------
# 7. EXPORTAÇÕES LOCAIS (XLSX, RDS, PARQUET)
# ------------------------------------------------------------------------------
planilha_pcp_path <- file.path(LOCAL_PATH, "SEPLAG_Beneficiários do Chapeú de Palha 2026.xlsx")
writexl::write_xlsx(df_pcp_final, planilha_pcp_path)

# Gravações RDS
readr::write_rds(df_pcp_final, file.path(LOCAL_PATH, "df_pcp.rds"))
readr::write_rds(df_pcp_final, file.path(DIR_OUT_AURORA, "df_pcp.rds"))

# Gravações Parquet
arrow::write_parquet(df_pcp_final, file.path(DIR_OUT_AURORA, "df_pcp.parquet"), compression = "snappy")
arrow::write_parquet(df_pcp_final, file.path(dir_parquet_out, "df_pcp.parquet"), compression = "snappy")
arrow::write_parquet(df_pcp_final, file.path(LOCAL_PATH, "df_pcp.parquet"), compression = "snappy")
arrow::write_parquet(df_pcp_final, file.path(dir_parquet_pcp, "df_pcp.parquet"), compression = "snappy")

df_pcp_final2 <- df_pcp_final %>%
  mutate(municipio_norm = padronizar_municipio(municipio)) %>%
  left_join(de_para_ibge, by = "municipio_norm") %>%
  select(ibge7, everything(), -municipio_norm)

df_pcp_final2 %>% filter(is.na(ibge7) & !is.na(municipio)) %>% distinct(municipio)

# Exportação JSON
jsonlite::write_json(
  x = df_pcp_final2,
  path = file.path(DIR_EXPORT_JSON, "df_pcp.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cli_alert_success("Bases locais salvas em XLSX, RDS e Parquet em {.path {DIR_OUT_AURORA}}.")

# ------------------------------------------------------------------------------
# 8. SINCRONIZAÇÃO GOOGLE DRIVE
# ------------------------------------------------------------------------------
if (file.exists(AUTH_PATH) && file.exists(planilha_pcp_path)) {
  tryCatch({
    drive_upload(
      media     = planilha_pcp_path,
      path      = googledrive::as_id(FOLDER_ID),
      name      = "SEPLAG_Beneficiários do Chapeú de Palha 2026.xlsx",
      overwrite = TRUE
    )
    cli_alert_success("Planilha enviada com sucesso ao Google Drive!")
  }, error = function(e) {
    cli_alert_danger("Falha ao subir planilha para o Google Drive: {.val {e$message}}")
  })
}

cli_alert_success("ETL Chapéu de Palha 2026 finalizado!")
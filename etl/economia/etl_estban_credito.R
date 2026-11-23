# ==============================================================================
# PIPELINE ETL - CRÉDITO E DEPÓSITOS MUNICIPAIS (ESTBAN / BANCO CENTRAL DO BRASIL)
# ==============================================================================
# Extrai as estatísticas bancárias mensais por município (ESTBAN) para Pernambuco,
# consolidando as principais rubricas do balanço (Plano Contábil COSIF):
# - Crédito Total (Operações de Crédito)
# - Financiamentos
# - Financiamento Imobiliário / Habitacional
# - Depósitos à Vista
# - Depósitos de Poupança
# - Depósitos a Prazo
#
# Utiliza o pacote {estbanr} (StrategicProjects):
# https://strategicprojects.github.io/estbanr/
#
# Camada Bruta (Raw/Bronze):  C:/dados/dados brutos/aurora/economia/
# Camada Curada (Gold/Out):   C:/dados/aurora/parquet/df_estban_credito_municipios_pe.parquet
# ==============================================================================

# 1. Carregamento de Pacotes ---------------------------------------------------
garantir_pacote <- function(pkg, github_repo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Instalando pacote {%s}...", pkg))
    if (!is.null(github_repo)) {
      if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", quiet = TRUE)
      remotes::install_github(github_repo, quiet = TRUE)
    } else {
      install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
    }
  }
}

garantir_pacote("estbanr", "StrategicProjects/estbanr")

suppressPackageStartupMessages({
  library(estbanr)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
})

# 2. Resolução de Caminhos -----------------------------------------------------
caminhos_paths <- c("R/paths.R", "paths.R", "../R/paths.R", "../../R/paths.R", "etl/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"

dir_raw_econ <- file.path(DIR_RAW_AURORA, "economia")
dir_out_pqt  <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_raw_econ)) dir.create(dir_raw_econ, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_out_pqt))  dir.create(dir_out_pqt, recursive = TRUE, showWarnings = FALSE)

cat("🏦 [ESTBAN] Iniciando extração e consolidação de Crédito e Depósitos (BCB - Pernambuco)...\n")

# Configura cache local do estbanr
options(estbanr.cache_dir = file.path(dir_raw_econ, "estban_cache"))
if (!dir.exists(getOption("estbanr.cache_dir"))) {
  dir.create(getOption("estbanr.cache_dir"), recursive = TRUE, showWarnings = FALSE)
}

# 3. Definição da Janela Temporal (Últimos 36 meses) --------------------------
ano_atual <- lubridate::year(Sys.Date())
mes_atual <- lubridate::month(Sys.Date())

start_period <- as.integer(sprintf("%d01", ano_atual - 3))
end_period   <- as.integer(sprintf("%d%02d", ano_atual, max(1, mes_atual - 1)))

cat(glue::glue("📥 Período ESTBAN: {start_period} a {end_period} (UF = PE)\n"))

# 4. Extração via estbanr ------------------------------------------------------
df_estban_raw <- tryCatch({
  estbanr::estban_fetch(
    start_period = start_period,
    end_period   = end_period,
    uf           = "PE"
  )
}, error = function(e) {
  message("⚠️ Erro no estban_fetch direto: ", e$message)
  NULL
})

if (is.null(df_estban_raw) || nrow(df_estban_raw) == 0) {
  warning("⚠️ Não foi possível obter novos dados da ESTBAN. Mantendo base anterior se existir.")
} else {
  cat(glue::glue("📊 Registros brutos ESTBAN obtidos: {nrow(df_estban_raw)}\n"))
  
  # Salva camada bruta
  saveRDS(df_estban_raw, file.path(dir_raw_econ, "estban_pe_bruto.rds"))
  
  # 5. Mapeamento de Rubricas COSIF / Verbetes ----------------------------------
  # O leiaute do BCB padroniza verbetes numéricos no ESTBAN:
  # - 160: Operações de Crédito (Total)
  # - 161: Empréstimos e Títulos Descontados
  # - 162/163: Financiamentos
  # - 169: Financiamentos Imobiliários / Habitacionais
  # - 411: Depósitos à Vista
  # - 420: Depósitos de Poupança
  # - 432: Depósitos a Prazo
  
  df_norm <- df_estban_raw %>% clean_names()
  
  # Identifica nomes de colunas presentes para mapear com segurança
  cols_atuais <- names(df_norm)
  
  col_ano_mes <- cols_atuais[str_detect(cols_atuais, "ano_mes|data_base|dt_base|anomes")][1]
  col_mun_cod <- cols_atuais[str_detect(cols_atuais, "cod_ibge|codigo_ibge|cod_mun|municipio_ibge")][1]
  col_mun_nom <- cols_atuais[str_detect(cols_atuais, "nome_municipio|municipio|mun")][1]
  
  # Seleciona ou identifica colunas de verbetes
  # Procura padrões de colunas como verbe_160, verbete_160, v160, etc.
  achar_col_verbete <- function(padrao) {
    c_match <- cols_atuais[str_detect(cols_atuais, padrao)]
    if (length(c_match) > 0) c_match[1] else NULL
  }
  
  col_cred_total <- achar_col_verbete("160|operacoes_de_credito")
  col_financ     <- achar_col_verbete("162|163|financiamento")
  col_imob       <- achar_col_verbete("169|habitac|imobili")
  col_vista      <- achar_col_verbete("411|410|a_vista")
  col_poupanca   <- achar_col_verbete("420|poupanca")
  col_prazo      <- achar_col_verbete("432|430|a_prazo")
  
  # Constrói agregação consolidada por município e mês
  df_estban_mun <- df_norm %>%
    mutate(
      ano_mes = as.character(.data[[col_ano_mes]]),
      ano = as.integer(substr(ano_mes, 1, 4)),
      mes = as.integer(substr(ano_mes, 5, 6)),
      cod_mun_ibge = as.character(.data[[col_mun_cod]]),
      nome_municipio = if (!is.null(col_mun_nom)) as.character(.data[[col_mun_nom]]) else NA_character_,
      # Extrai ou computa valores das rubricas
      credito_total = if (!is.null(col_cred_total)) as.numeric(.data[[col_cred_total]]) else 0,
      financiamentos = if (!is.null(col_financ)) as.numeric(.data[[col_financ]]) else 0,
      financiamento_imobiliario = if (!is.null(col_imob)) as.numeric(.data[[col_imob]]) else 0,
      depositos_a_vista = if (!is.null(col_vista)) as.numeric(.data[[col_vista]]) else 0,
      depositos_poupanca = if (!is.null(col_poupanca)) as.numeric(.data[[col_poupanca]]) else 0,
      depositos_a_prazo = if (!is.null(col_prazo)) as.numeric(.data[[col_prazo]]) else 0
    ) %>%
    group_by(ano_mes, ano, mes, cod_mun_ibge, nome_municipio) %>%
    summarise(
      credito_total = sum(credito_total, na.rm = TRUE),
      financiamentos = sum(financiamentos, na.rm = TRUE),
      financiamento_imobiliario = sum(financiamento_imobiliario, na.rm = TRUE),
      depositos_a_vista = sum(depositos_a_vista, na.rm = TRUE),
      depositos_poupanca = sum(depositos_poupanca, na.rm = TRUE),
      depositos_a_prazo = sum(depositos_a_prazo, na.rm = TRUE),
      total_depositos = depositos_a_vista + depositos_poupanca + depositos_a_prazo,
      .groups = "drop"
    )
  
  # 6. Gravação da Camada Curada (Ouro / Parquet) -------------------------------
  out_pqt_estban <- file.path(dir_out_pqt, "df_estban_credito_municipios_pe.parquet")
  arrow::write_parquet(df_estban_mun, out_pqt_estban)
  cat(glue::glue("💾 [ESTBAN] Base consolidada salva em: {out_pqt_estban} ({nrow(df_estban_mun)} linhas)\n"))
}

cat("✅ [ESTBAN] Pipeline finalizado com sucesso!\n")

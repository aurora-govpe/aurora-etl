# ==============================================================================
# PIPELINE ETL - EXPORTAÇÃO E IMPORTAÇÃO COMEX STAT (MDIC / COMEXR)
# ==============================================================================
# Extrai dados mensais de comércio exterior (Exportações e Importações) para PE.
#
# Utiliza o pacote {comexr} (StrategicProjects):
# https://strategicprojects.github.io/comexr/
#
# ⚠️ SINALIZAÇÃO TÉCNICA DE GRANULARIDADE (NCM vs MUNICÍPIO):
# A API oficial ComexStat / MDIC NÃO disponibiliza o código NCM completo (8 dígitos)
# na granularidade municipal para preservar o sigilo estatístico e fiscal das empresas
# exportadoras locais. No nível municipal, a maior desagregação de produto permitida
# é o SH4 / Heading (4 dígitos do Sistema Harmonizado).
# Para atender à demanda de forma integral:
# 1. Camada Municipal Mensal: agregada por Município x Mês x SH4 (Heading).
# 2. Camada Estadual Mensal: agregada por NCM (8 dígitos) x Mês para Pernambuco.
#
# Camada Bruta (Raw/Bronze):  C:/dados/dados brutos/aurora/economia/
# Camada Curada (Gold/Out):   C:/dados/aurora/parquet/
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

garantir_pacote("comexr", "StrategicProjects/comexr")

suppressPackageStartupMessages({
  library(comexr)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(purrr)
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

cat("🚢 [COMEX] Iniciando extração de Comércio Exterior (ComexStat / MDIC - Pernambuco)...\n")

# 3. Função com Retentativas e Tratamento de Rate-Limit (HTTP 429) -------------
consultar_comex_safe <- function(consulta_fn, max_tentativas = 5, pausa_base = 10) {
  tentativa <- 1
  sucesso <- FALSE
  resultado <- NULL
  
  while (!sucesso && tentativa <= max_tentativas) {
    resultado <- tryCatch({
      out <- consulta_fn()
      sucesso <- TRUE
      out
    }, error = function(e) {
      if (grepl("429", e$message)) {
        espera <- pausa_base * tentativa
        message(sprintf("   ⚠️ Limite ComexStat excedido (HTTP 429). Aguardando %d segundos...", espera))
        Sys.sleep(espera)
      } else {
        message(sprintf("   ⚠️ Erro na requisição: %s. Aguardando 5s...", e$message))
        Sys.sleep(5)
      }
      return(NULL)
    })
    tentativa <- tentativa + 1
  }
  Sys.sleep(3) # Pausa preventiva
  return(resultado)
}

# 4. Definição do Período (Últimos 3 anos) ------------------------------------
ano_atual <- lubridate::year(Sys.Date())
anos_alvo <- (ano_atual - 2):ano_atual

# 5. Extração Municipal Mensal (SH4 / Heading) --------------------------------
cat("📥 Consultando base MUNICIPAL MENSAL (SH4 Heading) para Pernambuco...\n")

extrair_muni_mes <- function(ano, fluxo) {
  message(sprintf("   --> Municípios | Fluxo: %s | Ano: %d", fluxo, ano))
  consultar_comex_safe(function() {
    comexr::comex_query_city(
      flow         = fluxo,
      start_period = sprintf("%d-01", ano),
      end_period   = sprintf("%d-12", ano),
      filters      = list(state = "26"), # 26 = Pernambuco
      details      = c("city", "heading"),
      month_detail = TRUE,
      language     = "pt",
      verbose      = FALSE
    )
  })
}

lista_muni_exp <- map(anos_alvo, ~extrair_muni_mes(.x, "export"))
Sys.sleep(5)
lista_muni_imp <- map(anos_alvo, ~extrair_muni_mes(.x, "import"))

df_muni_exp <- bind_rows(compact(lista_muni_exp)) %>% mutate(tipo_fluxo = "Exportação")
df_muni_imp <- bind_rows(compact(lista_muni_imp)) %>% mutate(tipo_fluxo = "Importação")

df_comex_muni_raw <- bind_rows(df_muni_exp, df_muni_imp)

# 6. Extração Estadual Mensal Detalhada por NCM (8 Dígitos) --------------------
cat("📥 Consultando base ESTADUAL MENSAL por NCM (8 dígitos) para Pernambuco...\n")

extrair_ncm_uf <- function(ano, fluxo) {
  message(sprintf("   --> NCM Estadual | Fluxo: %s | Ano: %d", fluxo, ano))
  consultar_comex_safe(function() {
    comexr::comex_query(
      flow         = fluxo,
      start_period = sprintf("%d-01", ano),
      end_period   = sprintf("%d-12", ano),
      filters      = list(state = "26"), # Pernambuco
      details      = c("ncm"),
      month_detail = TRUE,
      language     = "pt",
      verbose      = FALSE
    )
  })
}

lista_ncm_exp <- map(anos_alvo, ~extrair_ncm_uf(.x, "export"))
Sys.sleep(5)
lista_ncm_imp <- map(anos_alvo, ~extrair_ncm_uf(.x, "import"))

df_ncm_exp <- bind_rows(compact(lista_ncm_exp)) %>% mutate(tipo_fluxo = "Exportação")
df_ncm_imp <- bind_rows(compact(lista_ncm_imp)) %>% mutate(tipo_fluxo = "Importação")

df_comex_ncm_raw <- bind_rows(df_ncm_exp, df_ncm_imp)

# 7. Gravação da Camada Bruta (Raw / Bronze) ----------------------------------
saveRDS(
  list(municipal_sh4 = df_comex_muni_raw, estadual_ncm = df_comex_ncm_raw),
  file.path(dir_raw_econ, "comex_pe_bruto.rds")
)

# 8. Tratamento e Gravação da Camada Curada (Ouro / Parquet) -------------------
if (nrow(df_comex_muni_raw) > 0) {
  df_muni_ouro <- df_comex_muni_raw %>%
    clean_names() %>%
    mutate(
      cod_mun_ibge = as.character(coalesce(
        if ("city_code" %in% names(.)) as.character(city_code) else NULL,
        if ("codigo_municipio" %in% names(.)) as.character(codigo_municipio) else NULL,
        NA_character_
      )),
      municipio = coalesce(
        if ("city" %in% names(.)) as.character(city) else NULL,
        if ("nome_municipio" %in% names(.)) as.character(nome_municipio) else NULL,
        NA_character_
      ),
      ano = as.integer(year),
      mes = as.integer(coalesce(
        if ("month_number" %in% names(.)) as.integer(month_number) else NULL,
        if ("month" %in% names(.)) as.integer(month) else NULL,
        1L
      )),
      ano_mes = sprintf("%d%02d", ano, mes),
      sh4_codigo = coalesce(
        if ("heading_code" %in% names(.)) as.character(heading_code) else NULL,
        NA_character_
      ),
      sh4_descricao = coalesce(
        if ("heading" %in% names(.)) as.character(heading) else NULL,
        NA_character_
      ),
      valor_fob_usd = as.numeric(coalesce(
        if ("metric_fob" %in% names(.)) as.numeric(metric_fob) else NULL,
        if ("valor_fob" %in% names(.)) as.numeric(valor_fob) else NULL,
        0
      )),
      peso_kg = as.numeric(coalesce(
        if ("metric_kg" %in% names(.)) as.numeric(metric_kg) else NULL,
        if ("peso_liquido_kg" %in% names(.)) as.numeric(peso_liquido_kg) else NULL,
        0
      ))
    )
  
  pqt_muni <- file.path(dir_out_pqt, "df_comex_municipios_sh4_pe.parquet")
  arrow::write_parquet(df_muni_ouro, pqt_muni)
  cat(glue::glue("💾 [COMEX] Salvo municipal SH4: {pqt_muni} ({nrow(df_muni_ouro)} linhas)\n"))
}

if (nrow(df_comex_ncm_raw) > 0) {
  df_ncm_ouro <- df_comex_ncm_raw %>%
    clean_names() %>%
    mutate(
      ano = as.integer(year),
      mes = as.integer(coalesce(
        if ("month_number" %in% names(.)) as.integer(month_number) else NULL,
        if ("month" %in% names(.)) as.integer(month) else NULL,
        1L
      )),
      ano_mes = sprintf("%d%02d", ano, mes),
      ncm_codigo = coalesce(
        if ("ncm_code" %in% names(.)) as.character(ncm_code) else NULL,
        if ("ncm" %in% names(.)) as.character(ncm) else NULL,
        NA_character_
      ),
      ncm_descricao = coalesce(
        if ("ncm" %in% names(.)) as.character(ncm) else NULL,
        NA_character_
      ),
      valor_fob_usd = as.numeric(coalesce(
        if ("metric_fob" %in% names(.)) as.numeric(metric_fob) else NULL,
        0
      )),
      peso_kg = as.numeric(coalesce(
        if ("metric_kg" %in% names(.)) as.numeric(metric_kg) else NULL,
        0
      ))
    )
  
  pqt_ncm <- file.path(dir_out_pqt, "df_comex_estadual_ncm_pe.parquet")
  arrow::write_parquet(df_ncm_ouro, pqt_ncm)
  cat(glue::glue("💾 [COMEX] Salvo estadual NCM: {pqt_ncm} ({nrow(df_ncm_ouro)} linhas)\n"))
}

cat("✅ [COMEX] Pipeline finalizado com sucesso!\n")

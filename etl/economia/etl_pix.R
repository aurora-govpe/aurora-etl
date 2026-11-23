# ==============================================================================
# PIPELINE ETL - PIX BANCO CENTRAL DO BRASIL (ESTATÍSTICAS MUNICIPAIS)
# ==============================================================================
# Extrai as estatísticas mensais de transações Pix desagregadas por município
# para Pernambuco a partir da API de Dados Abertos do BCB (Olinda).
#
# Utiliza o pacote {pixr} (StrategicProjects):
# https://strategicprojects.github.io/pixr/
#
# Indicadores gerados:
# - Pagador (PF e PJ): Valor e Quantidade
# - Receptor (PF e PJ): Valor e Quantidade
# - Ticket Médio PF: Valor Pago PF / Quantidade Pagador PF
# - Ticket Médio PJ: Valor Pago PJ / Quantidade Pagador PJ
# - Razão PJ / PF (Volume financeiro e transações)
# - Penetração PF: Pessoas Físicas pagadoras distintas / População
# - Penetração PJ: Pessoas Jurídicas pagadoras distintas / Empresas Ativas
# - Pix por Habitante (PF): Valor Pago PF / População
#
# Camada Bruta (Raw/Bronze):  C:/dados/dados brutos/aurora/economia/
# Camada Curada (Gold/Out):   C:/dados/aurora/parquet/df_pix_municipios_pe.parquet
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

garantir_pacote("pixr", "StrategicProjects/pixr")

suppressPackageStartupMessages({
  library(pixr)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(httr2)
  library(jsonlite)
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

cat("⚡ [PIX] Iniciando extração dos indicadores de transações Pix (BCB - Pernambuco)...\n")

# 3. Definição da Janela Temporal (Últimos 24 meses) --------------------------
data_ref <- Sys.Date()
meses_seq <- seq(from = data_ref - months(24), to = data_ref - months(1), by = "1 month")
meses_banco <- format(meses_seq, "%Y%m")

# 4. Extração Cadenciada por Mês -----------------------------------------------
consultar_pix_pe_mes <- function(db) {
  message(sprintf("   --> Consultando Pix para a competência: %s", db))
  
  # Tentativa 1: Via pacote {pixr}
  df <- tryCatch({
    pixr::get_pix_transactions_by_municipality(
      database = db,
      filter = "Estado eq 'PERNAMBUCO'",
      verbose = FALSE
    )
  }, error = function(e) {
    # Fallback: Consulta direta à API OData Olinda do Banco Central
    tryCatch({
      url_olinda <- sprintf(
        "https://olinda.bcb.gov.br/olinda/servico/Pix_DadosAbertos/versao/v1/odata/TransacoesPixPorMunicipio(DataBase=@DataBase)?@DataBase='%s'&$filter=Estado%%20eq%%20'PERNAMBUCO'&$format=json",
        db
      )
      resp <- request(url_olinda) %>%
        req_timeout(30) %>%
        req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") %>%
        req_perform()
      conteudo <- fromJSON(resp_body_string(resp))
      if (!is.null(conteudo$value)) as_tibble(conteudo$value) else tibble()
    }, error = function(err) {
      message(sprintf("   ⚠️ Falha ao obter competência %s: %s", db, err$message))
      NULL
    })
  })
  
  Sys.sleep(1) # Pausa cortês para evitar HTTP 429
  return(df)
}

lista_pix <- list()
for (m in meses_banco) {
  res <- consultar_pix_pe_mes(m)
  if (!is.null(res) && nrow(res) > 0) {
    lista_pix[[m]] <- res
  }
}

if (length(lista_pix) == 0) {
  warning("⚠️ Nenhuma competência de Pix foi obtida nesta execução. Mantendo base anterior se existir.")
} else {
  df_pix_raw <- bind_rows(lista_pix) %>% clean_names()
  
  # Salva camada bruta
  saveRDS(df_pix_raw, file.path(dir_raw_econ, "pix_municipios_pe_bruto.rds"))
  cat(glue::glue("💾 [Prata] Registros brutos do Pix gravados: {nrow(df_pix_raw)}\n"))
  
  # 5. Cruzamento com População e Empresas Ativas -------------------------------
  # Carrega dados demográficos para cálculo per capita
  p_demo <- file.path(dir_out_pqt, "df_demografia_pe.parquet")
  df_pop <- if (file.exists(p_demo)) {
    arrow::read_parquet(p_demo) %>%
      clean_names() %>%
      transmute(
        cod_mun_ibge = as.character(substr(cod_mun, 1, 6)),
        populacao = as.numeric(populacao)
      ) %>%
      distinct()
  } else {
    tibble(cod_mun_ibge = character(), populacao = numeric())
  }
  
  # Carrega total de empresas ativas por município para cálculo de penetração PJ
  p_emp <- file.path(dir_out_pqt, "df_empresas_resumo_municipal.parquet")
  df_emp_ativas <- if (file.exists(p_emp)) {
    arrow::read_parquet(p_emp) %>%
      clean_names() %>%
      transmute(
        cod_mun_ibge = as.character(substr(municipio_codigo, 1, 6)),
        total_empresas_ativas = as.numeric(total_ativas)
      ) %>%
      distinct()
  } else {
    tibble(cod_mun_ibge = character(), total_empresas_ativas = numeric())
  }
  
  # 6. Cálculo dos Indicadores Analíticos ---------------------------------------
  df_pix_tratado <- df_pix_raw %>%
    mutate(
      cod_mun_ibge = as.character(municipio_ibge),
      ano_mes = as.character(ano_mes),
      ano = as.integer(substr(ano_mes, 1, 4)),
      mes = as.integer(substr(ano_mes, 5, 6)),
      # Garante tipagem numérica das métricas
      vl_pagador_pf = as.numeric(vl_pagador_pf),
      qt_pagador_pf = as.numeric(qt_pagador_pf),
      vl_pagador_pj = as.numeric(vl_pagador_pj),
      qt_pagador_pj = as.numeric(qt_pagador_pj),
      vl_recebedor_pf = as.numeric(vl_recebedor_pf),
      qt_recebedor_pf = as.numeric(qt_recebedor_pf),
      vl_recebedor_pj = as.numeric(vl_recebedor_pj),
      qt_recebedor_pj = as.numeric(qt_recebedor_pj),
      qt_pes_pagador_pf = as.numeric(qt_pes_pagador_pf),
      qt_pes_pagador_pj = as.numeric(qt_pes_pagador_pj),
      qt_pes_recebedor_pf = as.numeric(qt_pes_recebedor_pf),
      qt_pes_recebedor_pj = as.numeric(qt_pes_recebedor_pj),
      
      # Ticket Médio
      ticket_medio_pf = if_else(qt_pagador_pf > 0, round(vl_pagador_pf / qt_pagador_pf, 2), NA_real_),
      ticket_medio_pj = if_else(qt_pagador_pj > 0, round(vl_pagador_pj / qt_pagador_pj, 2), NA_real_),
      
      # Razão PJ / PF
      razao_valor_pj_pf = if_else(vl_pagador_pf > 0, round(vl_pagador_pj / vl_pagador_pf, 4), NA_real_),
      razao_qtd_pj_pf   = if_else(qt_pagador_pf > 0, round(qt_pagador_pj / qt_pagador_pf, 4), NA_real_)
    ) %>%
    # Incorpora dados populacionais
    left_join(df_pop, by = "cod_mun_ibge") %>%
    # Incorpora estoque de empresas
    left_join(df_emp_ativas, by = "cod_mun_ibge") %>%
    mutate(
      # Pix por Habitante (PF)
      pix_por_habitante_pf = if_else(!is.na(populacao) & populacao > 0, round(vl_pagador_pf / populacao, 2), NA_real_),
      # Penetração PF (% da população que pagou Pix no mês)
      penetracao_pf = if_else(!is.na(populacao) & populacao > 0, round((qt_pes_pagador_pf / populacao) * 100, 2), NA_real_),
      # Penetração PJ (% das empresas ativas que pagaram Pix no mês)
      penetracao_pj = if_else(!is.na(total_empresas_ativas) & total_empresas_ativas > 0, 
                              round((qt_pes_pagador_pj / total_empresas_ativas) * 100, 2), NA_real_)
    ) %>%
    select(
      ano_mes,
      ano,
      mes,
      cod_mun_ibge,
      municipio,
      estado,
      # Métricas Pagador
      vl_pagador_pf,
      qt_pagador_pf,
      vl_pagador_pj,
      qt_pagador_pj,
      # Métricas Receptor
      vl_recebedor_pf,
      qt_recebedor_pf,
      vl_recebedor_pj,
      qt_recebedor_pj,
      # Pessoas distintas
      qt_pes_pagador_pf,
      qt_pes_pagador_pj,
      qt_pes_recebedor_pf,
      qt_pes_recebedor_pj,
      # Indicadores de Intensidade e Penetração
      ticket_medio_pf,
      ticket_medio_pj,
      razao_valor_pj_pf,
      razao_qtd_pj_pf,
      populacao,
      pix_por_habitante_pf,
      penetracao_pf,
      total_empresas_ativas,
      penetracao_pj
    )
  
  # 7. Gravação da Camada Curada (Ouro / Parquet) -------------------------------
  out_pqt_pix <- file.path(dir_out_pqt, "df_pix_municipios_pe.parquet")
  arrow::write_parquet(df_pix_tratado, out_pqt_pix)
  cat(glue::glue("💾 [PIX] Camada Ouro salva em: {out_pqt_pix} ({nrow(df_pix_tratado)} registros)\n"))
}

cat("✅ [PIX] Pipeline finalizado com sucesso!\n")

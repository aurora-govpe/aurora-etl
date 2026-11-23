# ==============================================================================
# PIPELINE ETL - PESQUISA MENSAL DE COMÉRCIO (PMC - IBGE / SIDRA)
# ==============================================================================
# Extrai os índices mensais e taxas de variação do comércio varejista (PE e BR).
#
# ⚠️ SINALIZAÇÃO TÉCNICA DE GRANULARIDADE (PMC):
# A Pesquisa Mensal de Comércio do IBGE é produzida EXCLUSIVAMENTE para os âmbitos
# Estadual (Unidades da Federação) e Nacional (Brasil). Não há produção municipal
# da PMC pelo IBGE.
#
# O pipeline contempla:
# 1. Comércio Varejista Geral e Varejo Ampliado
# 2. Recorte Prioritário: "Tecidos, vestuário e calçados"
#
# Tabelas SIDRA:
# - Tabela 8880: PMC (Variações percentuais - Base 2022 = 100)
# - Tabela 8881: PMC (Número-índice - Base 2022 = 100)
#
# Camada Bruta (Raw/Bronze):  C:/dados/dados brutos/aurora/economia/
# Camada Curada (Gold/Out):   C:/dados/aurora/parquet/pmc_comercio_varejista.parquet
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(arrow)
  library(stringr)
  library(lubridate)
})

# 1. Resolução Dinâmica de Caminhos --------------------------------------------
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

cat("🛍️ [PMC] Iniciando extração da Pesquisa Mensal de Comércio (IBGE/SIDRA)...\n")

# 2. Requisições à API SIDRA ---------------------------------------------------
# Tabela 8880 (Variações %):
# /v/11707,11708,11709,11710 (Volume de vendas)
#    11707 = Variação mês / mesmo mês do ano anterior (%)
#    11708 = Variação acumulada no ano (%)
#    11709 = Variação mês / mês anterior com ajuste sazonal (%)
#    11710 = Variação acumulada em 12 meses (%)
# /c11046/all : Atividades do comércio (incluindo Varejo Geral e Tecidos, vestuário e calçados)
# /n3/26, n1/1 : Pernambuco (26) e Brasil (1)
# /p/last 36   : Últimos 36 meses

urls_pmc <- c(
  # Pernambuco (26)
  "https://apisidra.ibge.gov.br/values/t/8880/n3/26/v/11707,11708,11709,11710/p/last%2036/c11046/all",
  # Brasil (1)
  "https://apisidra.ibge.gov.br/values/t/8880/n1/1/v/11707,11708,11709,11710/p/last%2036/c11046/all"
)

cat("📥 Baixando séries da PMC via API SIDRA (PE e BR)...\n")
lista_pmc <- lapply(urls_pmc, function(u) {
  tryCatch({
    resp <- request(u) |>
      req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
      req_timeout(60) |>
      req_perform()
    d <- fromJSON(resp_body_string(resp))
    colnames(d) <- d[1, ]
    as_tibble(d[-1, ])
  }, error = function(e) {
    message("⚠️ Erro na URL PMC: ", u, " - ", e$message)
    tibble()
  })
})

df_pmc_bruto <- bind_rows(lista_pmc)

if (nrow(df_pmc_bruto) == 0) {
  warning("⚠️ Não foi possível obter novos dados da PMC. Mantendo base anterior se existir.")
} else {
  # 3. Gravação da Camada Bruta ------------------------------------------------
  arquivo_raw_rds <- file.path(dir_raw_econ, "pmc_comercio_bruto.rds")
  saveRDS(df_pmc_bruto, arquivo_raw_rds)
  cat(glue::glue("💾 [Prata] Arquivo salvo em: {arquivo_raw_rds} ({nrow(df_pmc_bruto)} linhas)\n"))
  
  # 4. Tratamento e Segmentação Setorial (Camada Ouro) -------------------------
  df_pmc_ouro <- df_pmc_bruto %>%
    clean_names() %>%
    mutate(
      localidade_codigo = coalesce(
        if ("unidade_da_federacao_codigo" %in% names(.)) as.character(unidade_da_federacao_codigo) else NULL,
        if ("brasil_codigo" %in% names(.)) as.character(brasil_codigo) else NULL,
        NA_character_
      ),
      localidade = coalesce(
        if ("unidade_da_federacao" %in% names(.)) as.character(unidade_da_federacao) else NULL,
        if ("brasil" %in% names(.)) as.character(brasil) else NULL,
        NA_character_
      ),
      ano_mes = as.character(mes_codigo),
      ano = as.integer(substr(ano_mes, 1, 4)),
      mes = as.integer(substr(ano_mes, 5, 6)),
      atividade_codigo = as.character(atividades_codigo),
      atividade_nome   = as.character(atividades),
      variavel_nome    = as.character(variavel),
      valor_num        = suppressWarnings(as.numeric(valor)),
      
      # Flag de foco temático
      flag_tecidos_vestuario = str_detect(
        atividade_nome, 
        regex("tecidos|vestu[aá]rio|cal[cç]ados", ignore_case = TRUE)
      )
    ) %>%
    select(
      nivel_territorial_codigo,
      localidade_codigo,
      localidade,
      ano_mes,
      ano,
      mes,
      atividade_codigo,
      atividade_nome,
      flag_tecidos_vestuario,
      variavel_codigo,
      variavel_nome,
      valor = valor_num
    )
  
  # 5. Gravação da Camada Curada (Ouro / Parquet) -------------------------------
  arquivo_ouro_pqt <- file.path(dir_out_pqt, "pmc_comercio_varejista.parquet")
  arrow::write_parquet(df_pmc_ouro, arquivo_ouro_pqt)
  cat(glue::glue("💾 [Ouro] PMC consolidada salva em: {arquivo_ouro_pqt} ({nrow(df_pmc_ouro)} linhas)\n"))
}

cat("✅ [PMC] Pipeline finalizado com sucesso!\n")

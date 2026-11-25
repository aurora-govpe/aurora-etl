# ==============================================================================
# PIPELINE ETL - CADASTRO ÚNICO (MDS / SAGICAD / DADOS ABERTOS)
# ==============================================================================
# Extrai as famílias inscritas no Cadastro Único por faixa de renda per capita:
# - Famílias em situação de pobreza (cadun_qtd_familias_cadastradas_pobreza_pbf_i)
# - Famílias de baixa renda (cadun_qtd_familias_cadastradas_baixa_renda_i)
# - Famílias com renda familiar per capita acima de 1/2 salário mínimo (cadun_qtd_familias_cadastradas_rfpc_acima_meio_sm_i)
#
# Fonte: API Solr de Dados Abertos do MDS:
# https://dadosabertos.mds.gov.br/misocial/
#
# Cobertura Temporal: 2019 até o ano corrente (execução anual consolidada)
# Cobertura Espacial: Todos os municípios brasileiros (com destaque para PE)
#
# Camada Bruta (Raw):
#   - C:/dados/dados brutos/aurora/cadunico/cadunico_familias_faixa_renda_consolidado.parquet
#   - C:/dados/dados brutos/aurora/cadunico/cadunico_familias_faixa_renda_consolidado.csv
#   - C:/dados/dados brutos/aurora/cadunico/cadunico_familias_faixa_renda_consolidado.rds
# Camada Curada (Ouro):
#   - C:/dados/aurora/parquet/df_cadunico_familias_renda_br.parquet (Brasil)
#   - C:/dados/aurora/parquet/df_cadunico_familias_renda_pe.parquet (Pernambuco - UF 26)
# ==============================================================================

# 1. Carregamento e Garantia de Pacotes -----------------------------------------
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

suppressPackageStartupMessages({
  garantir_pacote("readr")
  garantir_pacote("dplyr")
  garantir_pacote("tidyr")
  garantir_pacote("stringr")
  garantir_pacote("arrow")
  garantir_pacote("janitor")
  garantir_pacote("glue")
  
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(janitor)
  library(glue)
})

# 2. Resolução Centralizada de Caminhos ----------------------------------------
caminhos_paths <- c(
  "R/paths.R", "paths.R", "../R/paths.R", "../../R/paths.R", 
  "etl/paths.R", "../paths.R", "../../etl/paths.R"
)
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"

dir_raw_cadunico <- file.path(DIR_RAW_AURORA, "cadunico")
dir_out_pqt      <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_raw_cadunico)) dir.create(dir_raw_cadunico, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_out_pqt))      dir.create(dir_out_pqt, recursive = TRUE, showWarnings = FALSE)

cat("📋 [CadÚnico] Iniciando pipeline de extração: Famílias por Faixa de Renda...\n")

# 3. Configuração de Parâmetros e URL Base --------------------------------------
ano_inicial <- 2019
ano_atual   <- as.integer(format(Sys.Date(), "%Y"))
anos_alvo   <- seq(from = ano_inicial, to = ano_atual, by = 1)

base_url <- paste0(
  "https://dadosabertos.mds.gov.br/misocial/?",
  "fl=codigo_ibge%2Canomes_s%20",
  "qtd_fam_pob:cadun_qtd_familias_cadastradas_pobreza_pbf_i%20",
  "qtd_fam_baixa_renda:cadun_qtd_familias_cadastradas_baixa_renda_i%20",
  "qtd_fam_acima_meio_sm:cadun_qtd_familias_cadastradas_rfpc_acima_meio_sm_i&",
  "fq=cadun_qtd_familias_cadastradas_baixa_renda_i%3A*&",
  "q=*%3A*&rows=100000&",
  "sort=anomes_s%20desc%2C%20codigo_ibge%20asc&",
  "wt=csv&fq=anomes_s:"
)

# 4. Extração Iterativa por Ano ------------------------------------------------
lista_dados_anos <- list()

for (ano in anos_alvo) {
  url_ano <- paste0(base_url, ano, "*")
  cat(glue("📥 Baixando competências do ano {ano}... "))
  
  df_ano <- tryCatch({
    # Leitura direta do stream CSV do MDS Solr
    suppressWarnings(
      readr::read_csv(
        url_ano,
        col_types = cols(
          codigo_ibge = col_character(),
          anomes_s = col_character(),
          qtd_fam_pob = col_double(),
          qtd_fam_baixa_renda = col_double(),
          qtd_fam_acima_meio_sm = col_double()
        ),
        show_col_types = FALSE
      )
    )
  }, error = function(e) {
    message(glue("\n⚠️ Falha ao obter dados do ano {ano}: {e$message}"))
    NULL
  })
  
  if (!is.null(df_ano) && nrow(df_ano) > 0) {
    meses_ano <- length(unique(df_ano$anomes_s))
    cat(glue("OK! ({nrow(df_ano)} registros, {meses_ano} competências)\n"))
    lista_dados_anos[[as.character(ano)]] <- df_ano
  } else {
    cat("⚠️ Nenhum registro retornado.\n")
  }
}

if (length(lista_dados_anos) == 0) {
  stop("❌ Erro: Nenhum dado foi baixado da API do MDS. Verifique a conexão com a internet.")
}

# 5. Consolidação e Tratamento -------------------------------------------------
cat("🔄 Consolidando todos os anos desde 2019...\n")
df_cadunico_bruto <- bind_rows(lista_dados_anos)

cat(glue("📊 Total de registros brutos consolidados: {nrow(df_cadunico_bruto)}\n"))

df_cadunico_tratado <- df_cadunico_bruto %>%
  clean_names() %>%
  mutate(
    cod_mun_ibge = as.character(codigo_ibge),
    cod_uf = substr(cod_mun_ibge, 1, 2),
    ano_mes = as.character(anomes_s),
    ano = as.integer(substr(ano_mes, 1, 4)),
    mes = as.integer(substr(ano_mes, 5, 6)),
    qtd_fam_pobreza = as.numeric(qtd_fam_pob),
    qtd_fam_baixa_renda = as.numeric(qtd_fam_baixa_renda),
    qtd_fam_acima_meio_sm = as.numeric(qtd_fam_acima_meio_sm),
    # Total somando as 3 faixas extraídas
    total_familias_cadastradas_faixas = coalesce(qtd_fam_pobreza, 0) + 
                                       coalesce(qtd_fam_baixa_renda, 0) + 
                                       coalesce(qtd_fam_acima_meio_sm, 0)
  ) %>%
  select(
    cod_mun_ibge, cod_uf, ano_mes, ano, mes,
    qtd_fam_pobreza, qtd_fam_baixa_renda, qtd_fam_acima_meio_sm,
    total_familias_cadastradas_faixas
  ) %>%
  arrange(cod_mun_ibge, ano_mes)

# 6. Gravação na Camada Bruta (Raw / Bronze) -----------------------------------
# Conforme solicitado: 'C:\dados\dados brutos\aurora\cadunico'
raw_pqt_path <- file.path(dir_raw_cadunico, "cadunico_familias_faixa_renda_consolidado.parquet")
raw_csv_path <- file.path(dir_raw_cadunico, "cadunico_familias_faixa_renda_consolidado.csv")
raw_rds_path <- file.path(dir_raw_cadunico, "cadunico_familias_faixa_renda_consolidado.rds")

arrow::write_parquet(df_cadunico_tratado, raw_pqt_path)
readr::write_csv(df_cadunico_tratado, raw_csv_path)
saveRDS(df_cadunico_tratado, raw_rds_path)

cat(glue("💾 [Raw/Bruto] Salvo Parquet: {raw_pqt_path} ({nrow(df_cadunico_tratado)} linhas)\n"))
cat(glue("💾 [Raw/Bruto] Salvo CSV:     {raw_csv_path}\n"))
cat(glue("💾 [Raw/Bruto] Salvo RDS:     {raw_rds_path}\n"))

# 7. Gravação na Camada Curada (Ouro / Parquet) --------------------------------
# Brasil Completo
out_br_path <- file.path(dir_out_pqt, "df_cadunico_familias_renda_br.parquet")
arrow::write_parquet(df_cadunico_tratado, out_br_path)
cat(glue("💾 [Ouro/BR]   Salvo Curado Brasil: {out_br_path} ({nrow(df_cadunico_tratado)} linhas)\n"))

# Recorte Específico de Pernambuco (UF 26)
df_cadunico_pe <- df_cadunico_tratado %>%
  filter(cod_uf == "26")

out_pe_path <- file.path(dir_out_pqt, "df_cadunico_familias_renda_pe.parquet")
arrow::write_parquet(df_cadunico_pe, out_pe_path)
cat(glue("💾 [Ouro/PE]   Salvo Curado Pernambuco: {out_pe_path} ({nrow(df_cadunico_pe)} linhas)\n"))

cat("✅ [CadÚnico] Pipeline finalizado com sucesso!\n")

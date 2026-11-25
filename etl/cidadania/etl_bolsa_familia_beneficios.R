# ==============================================================================
# PIPELINE ETL - BOLSA FAMÍLIA (MDS / SAGICAD / DADOS ABERTOS)
# MODALIDADES E TIPOS DE BENEFÍCIOS MUNICIPAIS (NOVO BOLSA FAMÍLIA)
# ==============================================================================
# Extrai as quantidades de benefícios concedidos por modalidade e perfil:
# - BRC: Benefício Renda de Cidadania (R$ 142 por indivíduo da família)
# - BCO: Benefício Complementar (garante o piso de R$ 600 por família)
# - BPI: Benefício Primeira Infância (R$ 150 para crianças de 0 a 6 anos)
# - BVG: Benefício Variável Familiar para Gestantes (R$ 50)
# - BVN: Benefício Variável Familiar para Nutrizes (R$ 50 para bebês até 6 meses)
# - BVA: Benefício Variável Familiar para Adolescentes (12 a 17 anos) (R$ 50)
# - BV:  Benefício Variável Familiar para Crianças (7 a 11 anos) (R$ 50)
# - BF:  Benefício Familiar consolidado
# - BVBVA: Benefício Criança e Adolescente consolidado
# - BET: Benefício Extraordinário de Transição
# - SUS: Famílias com suspensão parcial em virtude do Seguro Defeso
#
# Fonte: API Solr de Dados Abertos do MDS:
# https://dadosabertos.mds.gov.br/misocial/
#
# ⚠️ NOTA HISTÓRICA SOBRE A SÉRIE TEMPORAL:
# As novas modalidades de benefícios (BRC, BCO, BPI, BVG, BVN, etc.) foram criadas
# pela Lei nº 14.601/2023 (Novo Bolsa Família), passando a ser registradas na folha
# de pagamentos a partir de 2023. O script busca dinamicamente desde 2019 até o ano
# corrente, consolidando todas as competências disponíveis.
#
# Camada Bruta (Raw / Bronze):
#   - C:/dados/dados brutos/aurora/cadunico/bolsa_familia_beneficios_tipo_consolidado.parquet
#   - C:/dados/dados brutos/aurora/cadunico/bolsa_familia_beneficios_tipo_consolidado.csv
#   - C:/dados/dados brutos/aurora/cadunico/bolsa_familia_beneficios_tipo_consolidado.rds
#   (replicado em C:/dados/dados brutos/aurora/bolsa_familia/)
# Camada Curada (Ouro / Parquet):
#   - C:/dados/aurora/parquet/df_bolsa_familia_beneficios_tipo_br.parquet (Brasil)
#   - C:/dados/aurora/parquet/df_bolsa_familia_beneficios_tipo_pe.parquet (Pernambuco - UF 26)
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
dir_raw_bf       <- file.path(DIR_RAW_AURORA, "bolsa_familia")
dir_out_pqt      <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_raw_cadunico)) dir.create(dir_raw_cadunico, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_raw_bf))       dir.create(dir_raw_bf, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_out_pqt))      dir.create(dir_out_pqt, recursive = TRUE, showWarnings = FALSE)

cat("🛡️ [Bolsa Família] Iniciando extração: Benefícios por Modalidade e Perfil...\n")

# 3. Configuração de Parâmetros e URL Base --------------------------------------
ano_inicial <- 2019
ano_atual   <- as.integer(format(Sys.Date(), "%Y"))
anos_alvo   <- seq(from = ano_inicial, to = ano_atual, by = 1)

base_url <- paste0(
  "https://dadosabertos.mds.gov.br/misocial/?",
  "fl=codigo_ibge%2Canomes_s%20",
  "qtd_ben_brc:pbf_qtd_benef_renda_cidadania_i%20",
  "qtd_ben_bco:pbf_qtd_benef_complementares_i%20",
  "qtd_ben_bpi:pbf_qtd_benef_primeira_infancia_bpi_i%20",
  "qtd_ben_bvg:pbf_qtd_benef_gestantes_i%20",
  "qtd_ben_bvn:pbf_2023_qtd_benef_bvn_i%20",
  "qtd_ben_bva:pbf_qtd_benef_familiares_adolescente_i%20",
  "qtd_ben_bv:pbf_qtd_benef_familiares_crianca_i%20",
  "qtd_ben_bf:pbf_qtd_benef_familiares_i%20",
  "qtd_ben_bvbva:pbf_qtd_benef_familiares_crianca_adolescente_i%20",
  "qtd_ben_bet:pbf_qtd_benef_extraordinarios_transicao_i%20",
  "qtd_ben_sus:pbf_qtd_familias_suspensao_parcial_seguro_defeso_i&",
  "q=*%3A*&rows=100000&",
  "sort=anomes_s%20desc%2C%20codigo_ibge%20asc&",
  "wt=csv&fq=pbf_qtd_benef_renda_cidadania_i%3A*&fq=anomes_s:"
)

# 4. Extração Iterativa por Ano ------------------------------------------------
lista_dados_anos <- list()

# Esquema estrito de tipos para evitar inconsistências em colunas com muitos NAs
tipos_colunas <- cols(
  codigo_ibge = col_character(),
  anomes_s = col_character(),
  qtd_ben_brc = col_double(),
  qtd_ben_bco = col_double(),
  qtd_ben_bpi = col_double(),
  qtd_ben_bvg = col_double(),
  qtd_ben_bvn = col_double(),
  qtd_ben_bva = col_double(),
  qtd_ben_bv = col_double(),
  qtd_ben_bf = col_double(),
  qtd_ben_bvbva = col_double(),
  qtd_ben_bet = col_double(),
  qtd_ben_sus = col_double()
)

for (ano in anos_alvo) {
  url_ano <- paste0(base_url, ano, "*")
  cat(glue("📥 Verificando e baixando competências do ano {ano}... "))
  
  df_ano <- tryCatch({
    suppressWarnings(
      readr::read_csv(
        url_ano,
        col_types = tipos_colunas,
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
    cat("ℹ️ Sem registros (modalidades do Novo PBF vigentes a partir de 2023).\n")
  }
}

if (length(lista_dados_anos) == 0) {
  stop("❌ Erro: Nenhum dado foi baixado da API do MDS. Verifique a conexão com a internet.")
}

# 5. Consolidação e Tratamento dos Dados ----------------------------------------
cat("🔄 Consolidando todos os anos...\n")
df_bf_bruto <- bind_rows(lista_dados_anos)

cat(glue("📊 Total de registros consolidados: {nrow(df_bf_bruto)}\n"))

df_bf_tratado <- df_bf_bruto %>%
  clean_names() %>%
  mutate(
    cod_mun_ibge = as.character(codigo_ibge),
    cod_uf = substr(cod_mun_ibge, 1, 2),
    ano_mes = as.character(anomes_s),
    ano = as.integer(substr(ano_mes, 1, 4)),
    mes = as.integer(substr(ano_mes, 5, 6)),
    
    # Modalidades Principais
    ben_renda_cidadania_brc = as.numeric(qtd_ben_brc),
    ben_complementar_bco    = as.numeric(qtd_ben_bco),
    ben_primeira_infancia_bpi = as.numeric(qtd_ben_bpi),
    
    # Benefícios Variáveis Familiares
    ben_variavel_gestante_bvg = as.numeric(qtd_ben_bvg),
    ben_variavel_nutriz_bvn   = as.numeric(qtd_ben_bvn),
    ben_variavel_crianca_bv   = as.numeric(qtd_ben_bv),
    ben_variavel_adolescente_bva = as.numeric(qtd_ben_bva),
    
    # Agrupadores e Especiais
    ben_familiar_bf           = as.numeric(qtd_ben_bf),
    ben_crianca_adolescente_bvbva = as.numeric(qtd_ben_bvbva),
    ben_extraordinario_transicao_bet = as.numeric(qtd_ben_bet),
    familias_suspensao_seguro_defeso = as.numeric(qtd_ben_sus),
    
    # Cálculos analíticos derivados
    total_beneficios_primeira_infancia = coalesce(ben_primeira_infancia_bpi, 0),
    total_beneficios_variaveis = coalesce(ben_primeira_infancia_bpi, 0) +
                                 coalesce(ben_variavel_gestante_bvg, 0) +
                                 coalesce(ben_variavel_nutriz_bvn, 0) +
                                 coalesce(ben_variavel_crianca_bv, 0) +
                                 coalesce(ben_variavel_adolescente_bva, 0)
  ) %>%
  select(
    cod_mun_ibge, cod_uf, ano_mes, ano, mes,
    ben_renda_cidadania_brc, ben_complementar_bco, ben_primeira_infancia_bpi,
    ben_variavel_gestante_bvg, ben_variavel_nutriz_bvn, ben_variavel_crianca_bv,
    ben_variavel_adolescente_bva, ben_familiar_bf, ben_crianca_adolescente_bvbva,
    ben_extraordinario_transicao_bet, familias_suspensao_seguro_defeso,
    total_beneficios_primeira_infancia, total_beneficios_variaveis
  ) %>%
  arrange(cod_mun_ibge, ano_mes)

# 6. Gravação na Camada Bruta (Raw / Bronze) -----------------------------------
# Gravação nas pastas de destino solicitadas
caminhos_raw <- list(
  cadunico = list(
    pqt = file.path(dir_raw_cadunico, "bolsa_familia_beneficios_tipo_consolidado.parquet"),
    csv = file.path(dir_raw_cadunico, "bolsa_familia_beneficios_tipo_consolidado.csv"),
    rds = file.path(dir_raw_cadunico, "bolsa_familia_beneficios_tipo_consolidado.rds")
  ),
  bolsa_familia = list(
    pqt = file.path(dir_raw_bf, "bolsa_familia_beneficios_tipo_consolidado.parquet"),
    csv = file.path(dir_raw_bf, "bolsa_familia_beneficios_tipo_consolidado.csv"),
    rds = file.path(dir_raw_bf, "bolsa_familia_beneficios_tipo_consolidado.rds")
  )
)

for (alvo in names(caminhos_raw)) {
  arrow::write_parquet(df_bf_tratado, caminhos_raw[[alvo]]$pqt)
  readr::write_csv(df_bf_tratado, caminhos_raw[[alvo]]$csv)
  saveRDS(df_bf_tratado, caminhos_raw[[alvo]]$rds)
  cat(glue("💾 [Raw/{alvo}] Salvo: {caminhos_raw[[alvo]]$pqt}\n"))
}

# 7. Gravação na Camada Curada (Ouro / Parquet) --------------------------------
# Brasil Completo
out_br_path <- file.path(dir_out_pqt, "df_bolsa_familia_beneficios_tipo_br.parquet")
arrow::write_parquet(df_bf_tratado, out_br_path)
cat(glue("💾 [Ouro/BR] Salvo Curado Brasil: {out_br_path} ({nrow(df_bf_tratado)} linhas)\n"))

# Recorte Específico de Pernambuco (UF 26)
df_bf_pe <- df_bf_tratado %>%
  filter(cod_uf == "26")

out_pe_path <- file.path(dir_out_pqt, "df_bolsa_familia_beneficios_tipo_pe.parquet")
arrow::write_parquet(df_bf_pe, out_pe_path)
cat(glue("💾 [Ouro/PE] Salvo Curado Pernambuco: {out_pe_path} ({nrow(df_bf_pe)} linhas)\n"))

cat("✅ [Bolsa Família] Pipeline de Benefícios finalizado com sucesso!\n")

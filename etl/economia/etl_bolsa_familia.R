# ==============================================================================
# PIPELINE ETL - BOLSA FAMÍLIA MUNICIPAL (MDS / SAGICAD / MI SOCIAL)
# ==============================================================================
# Extrai os indicadores municipais mensais do Programa Bolsa Família
# para todos os municípios de Pernambuco (UF 26) e Brasil.
#
# Utiliza o pacote {misocialr} (StrategicProjects):
# https://strategicprojects.github.io/misocial/
#
# Indicadores extraídos (MI Social / MDS):
# - Famílias beneficiárias do Bolsa Família
# - Pessoas beneficiárias do Bolsa Família
# - Valor repassado (R$) às famílias
# - Valor médio do benefício (R$)
# - Famílias beneficiárias em situação de pobreza e extrema pobreza
# - Benefícios Primeira Infância (BPI)
# - Benefícios a gestantes
# - Benefícios a crianças e adolescentes (7 a 17 anos)
# - Percentual de famílias beneficiárias com responsável mulher (%)
#
# ⚠️ OBSERVAÇÃO TÉCNICA SOBRE A API MI SOCIAL (MDS/SAGI):
# O pacote conecta-se ao endpoint oficial da Matriz de Informação Social:
# https://aplicacoes.mds.gov.br/sagi/servicos/misocial
# Caso os servidores federais estejam temporariamente em manutenção preventiva
# de segurança (retornando HTTP 302 para www.gov.br/mds/pt-br/avisos), o script
# detecta a condição graciosamente e preserva a base curada existente.
#
# Camada Bruta (Prata / RDS):
#   - C:/dados/dados brutos/aurora/economia/bolsa_familia_pe_bruto.rds
# Camada Curada (Ouro / Parquet):
#   - C:/dados/aurora/parquet/df_bolsa_familia_municipios_pe.parquet
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

garantir_pacote("misocialr", "StrategicProjects/misocialr")

suppressPackageStartupMessages({
  library(misocialr)
  library(httr2)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(glue)
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

cat("🛡️ [Bolsa Família] Iniciando extração municipal via pacote {misocialr}...\n")

# 3. Verificação Prévia de Disponibilidade da API MI Social --------------------
mds_api_url <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial"
cat(glue("🔍 Verificando status do servidor MDS ({mds_api_url})...\n"))

api_online <- tryCatch({
  # Executa probe rápido sem seguir redirecionamentos para avisos de manutenção
  resp <- request(mds_api_url) |>
    req_url_query(q = "*:*", wt = "json", rows = 1) |>
    req_timeout(15) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  status <- resp_status(resp)
  c_type <- resp_header(resp, "content-type")
  
  if (status == 200 && str_detect(tolower(c_type), "json")) {
    TRUE
  } else if (status %in% c(301, 302, 307, 308)) {
    loc <- resp_header(resp, "location")
    message(glue("⚠️ API do MDS retornou redirecionamento ({status}) para: {loc}."))
    message("⚠️ O portal SAGI/MDS pode estar em janela de manutenção de segurança.")
    FALSE
  } else {
    message(glue("⚠️ API do MDS respondeu com status HTTP {status} ({c_type})."))
    FALSE
  }
}, error = function(e) {
  message("⚠️ Falha de rede ao conectar à API MI Social do MDS: ", e$message)
  FALSE
})

# 4. Definição de Parâmetros de Extração ---------------------------------------
campos_bf <- c(
  "qtd_familias_beneficiarias_bolsa_familia_i",
  "qtd_pessoas_beneficiarias_bolsa_familia_i",
  "valor_repassado_bolsa_familia_f",
  "pbf_vlr_medio_benef_f",
  "familias_pbf_pobreza_i",
  "familias_pbf_extrema_pobreza_i",
  "pbf_qtd_benef_primeira_infancia_bpi_i",
  "pbf_qtd_benef_gestantes_i",
  "pbf_qtd_benef_7_a_17_anos_i",
  "pbf_perc_familias_benef_rf_mulher_d"
)

# Janela temporal: últimos 24 meses
data_ref <- Sys.Date()
periodos_alvo <- format(seq(from = data_ref - months(24), to = data_ref - months(1), by = "1 month"), "%Y%m")

df_bf_bruto <- NULL

if (api_online) {
  cat(glue("📥 Extraindo dados para {length(periodos_alvo)} competências ({min(periodos_alvo)} a {max(periodos_alvo)}) em Pernambuco (UF 26)...\n"))
  
  df_bf_bruto <- tryCatch({
    misocialr::mds_bolsa_familia(
      periods = periodos_alvo,
      localities = "26", # UF 26 = Pernambuco (e seus 185 municípios)
      fields = campos_bf
    )
  }, error = function(e) {
    message("⚠️ Erro durante chamada mds_bolsa_familia(): ", e$message)
    NULL
  })
} else {
  cat("ℹ️ Operação em modo resiliente: servidor MDS indisponível na verificação prévia.\n")
}

# 5. Gravação da Camada Bruta e Tratamento Ouro --------------------------------
arquivo_prata <- file.path(dir_raw_econ, "bolsa_familia_pe_bruto.rds")
out_pqt_bf    <- file.path(dir_out_pqt, "df_bolsa_familia_municipios_pe.parquet")

if (!is.null(df_bf_bruto) && nrow(df_bf_bruto) > 0) {
  # Salva camada bruta em RDS
  saveRDS(df_bf_bruto, arquivo_prata)
  cat(glue("💾 [Prata] Camada bruta salva: {arquivo_prata} ({nrow(df_bf_bruto)} registros)\n"))
  
  # Tratamento e Padronização
  df_bf_tratado <- df_bf_bruto %>%
    clean_names() %>%
    mutate(
      cod_mun_ibge = as.character(ibge),
      nome_municipio = as.character(municipio),
      ano_mes = as.character(anomes_s),
      ano = as.integer(substr(ano_mes, 1, 4)),
      mes = as.integer(substr(ano_mes, 5, 6)),
      uf = as.character(uf),
      familias_beneficiarias = as.integer(qtd_familias_beneficiarias_bolsa_familia_i),
      pessoas_beneficiarias = as.integer(qtd_pessoas_beneficiarias_bolsa_familia_i),
      valor_total_repassado = as.numeric(valor_repassado_bolsa_familia_f),
      beneficio_medio = as.numeric(pbf_vlr_medio_benef_f),
      familias_pobreza = as.integer(familias_pbf_pobreza_i),
      familias_extrema_pobreza = as.integer(familias_pbf_extrema_pobreza_i),
      beneficios_primeira_infancia = as.integer(pbf_qtd_benef_primeira_infancia_bpi_i),
      beneficios_gestantes = as.integer(pbf_qtd_benef_gestantes_i),
      beneficios_7_a_17_anos = as.integer(pbf_qtd_benef_7_a_17_anos_i),
      perc_responsavel_mulher = as.numeric(pbf_perc_familias_benef_rf_mulher_d)
    ) %>%
    select(
      cod_mun_ibge, nome_municipio, uf, ano_mes, ano, mes,
      familias_beneficiarias, pessoas_beneficiarias, valor_total_repassado,
      beneficio_medio, familias_pobreza, familias_extrema_pobreza,
      beneficios_primeira_infancia, beneficios_gestantes, beneficios_7_a_17_anos,
      perc_responsavel_mulher
    ) %>%
    arrange(cod_mun_ibge, ano, mes)
  
  arrow::write_parquet(df_bf_tratado, out_pqt_bf)
  cat(glue("💾 [Ouro] Salvo Parquet consolidado: {out_pqt_bf} ({nrow(df_bf_tratado)} linhas)\n"))
} else {
  if (file.exists(out_pqt_bf)) {
    cat(glue("ℹ️ Mantendo base Parquet existente prévia em: {out_pqt_bf}\n"))
  } else {
    cat("ℹ️ Base Parquet ainda não existente. Uma nova consulta será efetuada assim que a API do MDS for restabelecida.\n")
  }
}

cat("✅ [Bolsa Família] Pipeline finalizado!\n")

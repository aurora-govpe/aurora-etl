# ==============================================================================
# PIPELINE ETL - PNAD CONTÍNUA (IBGE) - MERCADO DE TRABALHO E RENDA
# ==============================================================================
# Extrai dados agregados da Pesquisa Nacional por Amostra de Domicílios Contínua
# (PNAD Contínua Trimestral) para Pernambuco (UF 26) e Brasil (1).
#
# Indicadores extraídos:
# 1. Taxa de Desocupação e Taxa de Subutilização da Força de Trabalho (Tabela 4099)
# 2. Condição de Força de Trabalho e Ocupação (Tabela 4092):
#    - Pessoas em idade ativa (14+ anos)
#    - Força de trabalho total
#    - Força de trabalho ocupada
#    - Força de trabalho desocupada
#    - Pessoas fora da força de trabalho
# 3. Rendimento Médio Mensal Real (Tabela 5437):
#    - Habitualmente recebido no trabalho principal e em todos os trabalhos
#    - Efetivamente recebido no trabalho principal e em todos os trabalhos
# 4. Rendimento Médio Real por Grupamento de Atividade (Tabela 5442)
#
# Utiliza o pacote {ibger} (rOpenSci):
# https://docs.ropensci.org/ibger/
#
# ⚠️ NOTA TÉCNICA DE GRANULARIDADE TERRITORIAL:
# A PNAD Contínua é uma pesquisa amostral probabilística complexa do IBGE.
# Suas estimativas trimestrais de mercado de trabalho e renda possuem representatividade
# estatística para o Brasil (N1), Grandes Regiões (N2) e Unidades da Federação (N3, PE=26).
# O IBGE NÃO disponibiliza estimativas municipais gerais na PNAD Contínua.
#
# Camada Bruta (Prata / RDS):
#   - C:/dados/dados brutos/aurora/economia/pnadc_mercado_trabalho_bruto.rds
# Camada Curada (Ouro / Parquet):
#   - C:/dados/aurora/parquet/df_pnadc_mercado_trabalho_pe.parquet
#   - C:/dados/aurora/parquet/df_pnadc_rendimento_atividades_pe.parquet
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

garantir_pacote("ibger", "ropensci/ibger")

suppressPackageStartupMessages({
  library(ibger)
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

cat("👥 [PNADC] Iniciando extração da PNAD Contínua via pacote {ibger}...\n")

# Janela temporal: últimos 20 trimestres (5 anos)
num_trimestres <- -20
localidades_alvo <- "N1|N3[26]" # Brasil (1) e Pernambuco (26)

# 3. Extração das Tabelas da PNAD Contínua --------------------------------------

# A. Taxa de Desocupação e Subutilização (Tabela 4099)
cat("📥 [1/4] Extraindo Taxas de Desocupação e Subutilização (Tabela 4099)...\n")
df_raw_taxas <- tryCatch({
  ibger::ibge_variables(
    aggregate = 4099,
    variable = c(4099, 4118), # 4099: Desocupação (%), 4118: Subutilização (%)
    periods = num_trimestres,
    localities = localidades_alvo
  )
}, error = function(e) {
  message("⚠️ Erro na Tabela 4099: ", e$message)
  NULL
})

# B. Condição da Força de Trabalho e Ocupação (Tabela 4092)
# Categorias C629:
# 32385: Total 14+ anos
# 32386: Força de trabalho
# 32387: Força de trabalho - ocupada
# 32446: Força de trabalho - desocupada
# 32447: Fora da força de trabalho
cat("📥 [2/4] Extraindo População e Força de Trabalho em Mil Pessoas (Tabela 4092)...\n")
df_raw_ft <- tryCatch({
  ibger::ibge_variables(
    aggregate = 4092,
    variable = 1641, # Pessoas de 14 anos ou mais de idade (Mil pessoas)
    periods = num_trimestres,
    localities = localidades_alvo,
    classification = list("629" = c(32385, 32386, 32387, 32446, 32447))
  )
}, error = function(e) {
  message("⚠️ Erro na Tabela 4092: ", e$message)
  NULL
})

# C. Rendimento Médio Mensal Real (Tabela 5437)
# Variáveis:
# 5932: Rendimento médio real habitualmente recebido no trabalho principal (R$)
# 5933: Rendimento médio real habitualmente recebido em todos os trabalhos (R$)
# 5934: Rendimento médio real efetivamente recebido no trabalho principal (R$)
# 5935: Rendimento médio real efetivamente recebido em todos os trabalhos (R$)
cat("📥 [3/4] Extraindo Rendimento Médio Real Habitual e Efetivo (Tabela 5437)...\n")
df_raw_rend <- tryCatch({
  ibger::ibge_variables(
    aggregate = 5437,
    variable = c(5932, 5933, 5934, 5935),
    periods = num_trimestres,
    localities = localidades_alvo,
    classification = list("58" = 95253) # Total de idades
  )
}, error = function(e) {
  message("⚠️ Erro na Tabela 5437: ", e$message)
  NULL
})

# D. Rendimento Médio por Grupamento de Atividades Econômicas (Tabela 5442)
cat("📥 [4/4] Extraindo Rendimento Médio Real por Atividade Econômica (Tabela 5442)...\n")
df_raw_atividades <- tryCatch({
  ibger::ibge_variables(
    aggregate = 5442,
    variable = c(5932, 5934), # Habitual e Efetivo no trabalho principal
    periods = num_trimestres,
    localities = localidades_alvo,
    classification = list("888" = "all") # Todas as atividades
  )
}, error = function(e) {
  message("⚠️ Erro na Tabela 5442: ", e$message)
  NULL
})

# 4. Gravação da Camada Bruta (Prata / RDS) -------------------------------------
lista_bruto <- list(
  taxas = df_raw_taxas,
  forca_trabalho = df_raw_ft,
  rendimento_geral = df_raw_rend,
  rendimento_atividades = df_raw_atividades,
  extracao_data = Sys.time()
)

raw_rds_path <- file.path(dir_raw_econ, "pnadc_mercado_trabalho_bruto.rds")
saveRDS(lista_bruto, raw_rds_path)
cat(glue("💾 [Prata] Camada bruta salva em: {raw_rds_path}\n"))

# 5. Tratamento, Limpeza e Padronização -----------------------------------------
parse_periodo_pnad <- function(periodo_str) {
  # Exemplo de entrada: "202401" -> Ano = 2024, Trimestre = 1
  ano <- as.integer(substr(periodo_str, 1, 4))
  tri <- as.integer(substr(periodo_str, 5, 6))
  tibble(
    periodo = periodo_str,
    ano = ano,
    trimestre = tri,
    ano_trimestre = sprintf("%dT%d", ano, tri)
  )
}

# Tratamento 1: Indicadores Gerais Consolidados (Taxas, Força de Trabalho e Rendimento)
df_consol_list <- list()

if (!is.null(df_raw_taxas) && nrow(df_raw_taxas) > 0) {
  df_t <- df_raw_taxas %>%
    clean_names() %>%
    mutate(
      valor = suppressWarnings(as.numeric(value)),
      cod_variavel = case_when(
        variable_id == "4099" ~ "taxa_desocupacao",
        variable_id == "4118" ~ "taxa_subutilizacao",
        TRUE ~ paste0("var_", variable_id)
      )
    ) %>%
    select(locality_id, locality_name, locality_level, period, cod_variavel, valor)
  df_consol_list$taxas <- df_t
}

if (!is.null(df_raw_ft) && nrow(df_raw_ft) > 0) {
  df_f <- df_raw_ft %>%
    clean_names() %>%
    mutate(
      valor = suppressWarnings(as.numeric(value)) * 1000, # Converter de mil pessoas para pessoas
      cod_variavel = case_when(
        classification_629 == "Total" ~ "populacao_14_anos_ou_mais",
        classification_629 == "Força de trabalho" ~ "forca_trabalho_total",
        classification_629 == "Força de trabalho - ocupada" ~ "pessoas_ocupadas",
        classification_629 == "Força de trabalho - desocupada" ~ "pessoas_desocupadas",
        classification_629 == "Fora da força de trabalho" ~ "fora_da_forca_trabalho",
        TRUE ~ "outra_condicao_ft"
      )
    ) %>%
    select(locality_id, locality_name, locality_level, period, cod_variavel, valor)
  df_consol_list$ft <- df_f
}

if (!is.null(df_raw_rend) && nrow(df_raw_rend) > 0) {
  df_r <- df_raw_rend %>%
    clean_names() %>%
    mutate(
      valor = suppressWarnings(as.numeric(value)),
      cod_variavel = case_when(
        variable_id == "5932" ~ "rendimento_real_habitual_principal",
        variable_id == "5933" ~ "rendimento_real_habitual_todos",
        variable_id == "5934" ~ "rendimento_real_efetivo_principal",
        variable_id == "5935" ~ "rendimento_real_efetivo_todos",
        TRUE ~ paste0("rend_var_", variable_id)
      )
    ) %>%
    select(locality_id, locality_name, locality_level, period, cod_variavel, valor)
  df_consol_list$rend <- df_r
}

# Consolidação no formato Wide analítico
df_indicadores_long <- bind_rows(df_consol_list)

if (nrow(df_indicadores_long) > 0) {
  df_pnadc_mercado_trabalho <- df_indicadores_long %>%
    pivot_wider(names_from = cod_variavel, values_from = valor) %>%
    mutate(
      ano = as.integer(substr(period, 1, 4)),
      trimestre = as.integer(substr(period, 5, 6)),
      ano_trimestre = sprintf("%dT%d", ano, trimestre),
      cod_localidade = as.character(locality_id),
      localidade = locality_name,
      tipo_localidade = if_else(cod_localidade == "26", "Estado (PE)", "Brasil"),
      # Cálculos derivados úteis
      taxa_participacao_ft = round(forca_trabalho_total / populacao_14_anos_ou_mais * 100, 2),
      nivel_ocupacao = round(pessoas_ocupadas / populacao_14_anos_ou_mais * 100, 2)
    ) %>%
    arrange(cod_localidade, ano, trimestre)
  
  # 6. Gravação da Camada Curada (Ouro / Parquet) -------------------------------
  out_pqt_principal <- file.path(dir_out_pqt, "df_pnadc_mercado_trabalho_pe.parquet")
  arrow::write_parquet(df_pnadc_mercado_trabalho, out_pqt_principal)
  cat(glue("💾 [Ouro] Salvo resumo PNADC Mercado de Trabalho e Renda: {out_pqt_principal} ({nrow(df_pnadc_mercado_trabalho)} linhas)\n"))
}

# Tratamento 2: Rendimento Médio Real por Atividade Econômica
if (!is.null(df_raw_atividades) && nrow(df_raw_atividades) > 0) {
  df_atividades_tratadas <- df_raw_atividades %>%
    clean_names() %>%
    mutate(
      valor = suppressWarnings(as.numeric(value)),
      tipo_rendimento = if_else(variable_id == "5932", "habitual_principal", "efetivo_principal"),
      atividade = classification_888,
      ano = as.integer(substr(period, 1, 4)),
      trimestre = as.integer(substr(period, 5, 6)),
      ano_trimestre = sprintf("%dT%d", ano, trimestre),
      cod_localidade = as.character(locality_id),
      localidade = locality_name,
      # Flag setorial têxtil / confecção / indústria de transformação
      flag_industria_transformacao = str_detect(atividade, regex("transformação", ignore_case = TRUE))
    ) %>%
    select(
      cod_localidade, localidade, ano, trimestre, ano_trimestre,
      atividade, tipo_rendimento, valor, flag_industria_transformacao
    ) %>%
    pivot_wider(names_from = tipo_rendimento, values_from = valor) %>%
    arrange(cod_localidade, atividade, ano, trimestre)
  
  out_pqt_atividades <- file.path(dir_out_pqt, "df_pnadc_rendimento_atividades_pe.parquet")
  arrow::write_parquet(df_atividades_tratadas, out_pqt_atividades)
  cat(glue("💾 [Ouro] Salvo recorte por atividade econômica: {out_pqt_atividades} ({nrow(df_atividades_tratadas)} linhas)\n"))
}

cat("✅ [PNADC] Pipeline da PNAD Contínua finalizado com sucesso!\n")

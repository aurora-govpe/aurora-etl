# ==============================================================================
# PIPELINE ETL - PRODUÇÃO INDUSTRIAL MENSAL (PIM-PF - IBGE / SIDRA)
# ==============================================================================
# Extrai os índices de produção física industrial mensal (Pernambuco e Brasil)
# Tabela SIDRA 8888: PIM-PF (Nova série - Base 2022 = 100)
# Camada Prata: C:\dados\dados brutos\aurora\industrias
# Camada Ouro:  C:\dados\aurora\parquet\pim_producao_industrial.parquet
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(arrow)
})

# 1. Resolução dinâmica de caminhos ---------------------------------------------
caminhos_paths <- c("R/paths.R", "paths.R", "../R/paths.R", "../../R/paths.R", "etl/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

# 2. Definição dos diretórios Prata e Ouro ---------------------------------------
# Prata: C:\dados\dados brutos\aurora\industrias
dir_prata <- file.path(DIR_RAW_AURORA, "industrias")
# Ouro:  C:\dados\aurora\parquet
dir_ouro  <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_prata)) dir.create(dir_prata, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_ouro))  dir.create(dir_ouro, recursive = TRUE, showWarnings = FALSE)

cat("🏭 [PIM-PF] Iniciando extração da Pesquisa Industrial Mensal (IBGE/SIDRA)...\n")

# 3. Requisição à API SIDRA ----------------------------------------------------
# Tabela 8888:
# /n3/26, n1/1 : Pernambuco (26) e Brasil (1)
# /v/11601,11602,11603,11604,12606,12607:
#    11601 = Variação mês / mesmo mês do ano anterior (%)
#    11602 = Variação acumulada no ano (%)
#    11603 = Variação mês / mês anterior com ajuste sazonal (%)
#    11604 = Variação acumulada em 12 meses (%)
#    12606 = Índice de base fixa sem ajuste sazonal (2022=100)
#    12607 = Índice de base fixa com ajuste sazonal (2022=100)
# /p/last 36  : Últimos 36 meses disponíveis
# /c544/129314: Indústria geral

urls_pim <- c(
  "https://apisidra.ibge.gov.br/values/t/8888/n3/26/v/11601,11602,11603,11604,12606,12607/p/last%2036/c544/129314",
  "https://apisidra.ibge.gov.br/values/t/8888/n1/1/v/11601,11602,11603,11604,12606,12607/p/last%2036/c544/129314"
)

cat("📥 Baixando indicadores industriais da API SIDRA (PE e BR)...\n")
lista_pim <- lapply(urls_pim, function(u) {
  resp <- request(u) |>
    req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
    req_perform()
  d <- fromJSON(resp_body_string(resp))
  colnames(d) <- d[1, ]
  as_tibble(d[-1, ])
})
df_pim_bruto <- bind_rows(lista_pim)

# 4. Gravação da Camada Prata (Raw / Dados Brutos) ------------------------------
arquivo_prata_rds <- file.path(dir_prata, "pim_pf_bruto.rds")
saveRDS(df_pim_bruto, arquivo_prata_rds)
cat(glue::glue("💾 [Prata] Arquivo salvo em: {arquivo_prata_rds}\n"))

# 5. Tratamento, limpeza e estruturação (Camada Ouro) -------------------------
cat("⚙️ Processando séries temporais da indústria para a camada Ouro...\n")

df_pim_ouro <- df_pim_bruto %>%
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
    )
  ) %>%
  select(
    nivel_territorial_codigo,
    localidade_codigo,
    localidade,
    mes_codigo,
    mes,
    variavel,
    valor
  ) %>%
  mutate(
    # Extrai ano e mês numéricos a partir do código do mês (formato AAAAMM)
    ano = as.integer(substr(mes_codigo, 1, 4)),
    mes_num = as.integer(substr(mes_codigo, 5, 6)),
    data_referencia = as.Date(paste0(ano, "-", sprintf("%02d", mes_num), "-01")),
    localidade_tipo = if_else(nivel_territorial_codigo == "1", "Brasil", "Pernambuco"),
    uf = if_else(nivel_territorial_codigo == "3", "PE", "BR"),
    valor_num = case_when(
      valor %in% c("-", "..", "...", "X", "x") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(valor))
    )
  ) %>%
  select(-valor) %>%
  pivot_wider(
    names_from = variavel,
    values_from = valor_num
  ) %>%
  clean_names() %>%
  rename_with(
    ~ case_when(
      grepl("mesmo_mes_do_ano", .x) ~ "var_interanual_pct",
      grepl("acumulada_no_ano", .x) ~ "var_acumulada_ano_pct",
      grepl("12_meses", .x) ~ "var_acumulada_12m_pct",
      grepl("imediatamente_anterior", .x) ~ "var_margem_sazonal_pct",
      grepl("indice_com_ajuste", .x) ~ "indice_com_ajuste_2022",
      grepl("numero_indice_2022", .x) ~ "indice_sem_ajuste_2022",
      TRUE ~ .x
    )
  ) %>%
  arrange(localidade_tipo, data_referencia)

# 6. Gravação da Camada Ouro (Parquet) ------------------------------------------
arquivo_ouro_parquet <- file.path(dir_ouro, "pim_producao_industrial.parquet")
write_parquet(df_pim_ouro, arquivo_ouro_parquet)

cat(glue::glue("✅ [Ouro] Base Parquet da indústria salva: {arquivo_ouro_parquet} ({nrow(df_pim_ouro)} linhas)\n\n"))

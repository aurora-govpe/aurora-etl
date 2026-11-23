# ==============================================================================
# PIPELINE ETL - PRODUÇÃO DA AQUICULTURA (PPM - IBGE / SIDRA)
# ==============================================================================
# Extrai dados da Aquicultura para Pernambuco (Tilápia, Camarão, Alevinos, etc.)
# Tabela SIDRA 3940: Produção da aquicultura por tipo de produto
# Camada Prata: C:\dados\dados brutos\aurora\agricultura
# Camada Ouro:  C:\dados\aurora\parquet\ppm_aquicultura.parquet
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
dir_prata <- file.path(DIR_RAW_AURORA, "agricultura")
dir_ouro  <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_prata)) dir.create(dir_prata, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_ouro))  dir.create(dir_ouro, recursive = TRUE, showWarnings = FALSE)

cat("🐟 [Aquicultura] Iniciando extração da Pesquisa da Aquicultura (IBGE/SIDRA)...\n")

# 3. Requisição à API SIDRA ----------------------------------------------------
# Tabela 3940:
# /n6/in n3 26 : Todos os municípios de Pernambuco
# /n3/26       : Total do estado de Pernambuco
# /v/214,215   : 214 = Quantidade produzida (Quilos / Milheiros de alevinos)
#                215 = Valor da produção (Mil Reais)
# /p/last 5    : Últimos 5 anos disponíveis
# /c11248/all  : Todos os tipos de produtos da aquicultura

urls_aqui <- c(
  "https://apisidra.ibge.gov.br/values/t/3940/n6/in%20n3%2026/v/215,4146/p/last%205/c654/all",
  "https://apisidra.ibge.gov.br/values/t/3940/n3/26/v/215,4146/p/last%205/c654/all"
)

cat("📥 Baixando dados de aquicultura da API SIDRA...\n")
lista_aqui <- lapply(urls_aqui, function(u) {
  resp <- request(u) |>
    req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
    req_perform()
  d <- fromJSON(resp_body_string(resp))
  colnames(d) <- d[1, ]
  as_tibble(d[-1, ])
})
df_aqui_bruto <- bind_rows(lista_aqui)

# 4. Gravação da Camada Prata (Raw / Dados Brutos) ------------------------------
arquivo_prata_rds <- file.path(dir_prata, "aquicultura_pe_bruto.rds")
saveRDS(df_aqui_bruto, arquivo_prata_rds)
cat(glue::glue("💾 [Prata] Arquivo salvo em: {arquivo_prata_rds}\n"))

# 5. Tratamento, limpeza e estruturação (Camada Ouro) -------------------------
cat("⚙️ Processando dados de piscicultura e carcinicultura para a camada Ouro...\n")

df_aqui_ouro <- df_aqui_bruto %>%
  clean_names() %>%
  mutate(
    unidade_geografica_codigo = coalesce(
      if ("municipio_codigo" %in% names(.)) as.character(municipio_codigo) else NULL,
      if ("unidade_da_federacao_codigo" %in% names(.)) as.character(unidade_da_federacao_codigo) else NULL,
      NA_character_
    ),
    unidade_geografica = coalesce(
      if ("municipio" %in% names(.)) as.character(municipio) else NULL,
      if ("unidade_da_federacao" %in% names(.)) as.character(unidade_da_federacao) else NULL,
      NA_character_
    )
  ) %>%
  select(
    nivel_territorial_codigo,
    unidade_geografica_codigo,
    unidade_geografica,
    ano,
    produto_aquicultura = tipo_de_produto_da_aquicultura,
    variavel,
    valor
  ) %>%
  mutate(
    ibge6 = if_else(nivel_territorial_codigo == "6", substr(unidade_geografica_codigo, 1, 6), NA_character_),
    ibge7 = if_else(nivel_territorial_codigo == "6", unidade_geografica_codigo, NA_character_),
    uf = "PE",
    ano = as.integer(ano),
    valor_num = case_when(
      valor %in% c("-", "..", "...", "X", "x") ~ 0,
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
      grepl("producao_da_aquicultura|quantidade", .x) ~ "quantidade_produzida",
      grepl("valor_da_producao", .x) ~ "valor_producao_mil_reais",
      TRUE ~ .x
    )
  ) %>%
  mutate(
    valor_producao_reais = if ("valor_producao_mil_reais" %in% names(.)) valor_producao_mil_reais * 1000 else 0,
    preco_medio_kg = if (all(c("valor_producao_reais", "quantidade_produzida") %in% names(.))) {
      if_else(quantidade_produzida > 0, valor_producao_reais / quantidade_produzida, NA_real_)
    } else {
      NA_real_
    }
  ) %>%
  arrange(nivel_territorial_codigo, unidade_geografica, ano, produto_aquicultura)

# 6. Gravação da Camada Ouro (Parquet) ------------------------------------------
arquivo_ouro_parquet <- file.path(dir_ouro, "ppm_aquicultura.parquet")
write_parquet(df_aqui_ouro, arquivo_ouro_parquet)

cat(glue::glue("✅ [Ouro] Base Parquet de aquicultura salva: {arquivo_ouro_parquet} ({nrow(df_aqui_ouro)} linhas)\n\n"))

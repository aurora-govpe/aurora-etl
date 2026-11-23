# ==============================================================================
# PIPELINE ETL - PESQUISA DA PECUÁRIA MUNICIPAL (PPM - IBGE / SIDRA)
# ==============================================================================
# Extrai dados da PPM para Pernambuco (Rebanhos e Produtos de Origem Animal)
# Tabela SIDRA 3939: Efetivo dos rebanhos por tipo de rebanho
# Tabela SIDRA 74:   Produção de origem animal (Leite, Ovos, Mel)
# Camada Prata: C:\dados\dados brutos\aurora\agricultura
# Camada Ouro:  C:\dados\aurora\parquet\ppm_pecuaria.parquet
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
dir_prata <- file.path(DIR_RAW_AURORA, "economia")
dir_ouro  <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_prata)) dir.create(dir_prata, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_ouro))  dir.create(dir_ouro, recursive = TRUE, showWarnings = FALSE)

cat("🐄 [PPM] Iniciando extração da Pesquisa da Pecuária Municipal (IBGE/SIDRA)...\n")

# 3. Requisições à API SIDRA ----------------------------------------------------
# 3.1 Tabela 3939 - Efetivo dos rebanhos (v/105: Cabeças, c79/all: tipos de rebanho)
urls_rebanhos <- c(
  "https://apisidra.ibge.gov.br/values/t/3939/n6/in%20n3%2026/v/105/p/last%205/c79/all",
  "https://apisidra.ibge.gov.br/values/t/3939/n3/26/v/105/p/last%205/c79/all"
)

cat("📥 Baixando efetivo dos rebanhos (Tabela 3939 - Municípios e Total PE)...\n")
lista_reb <- lapply(urls_rebanhos, function(u) {
  resp <- request(u) |>
    req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
    req_perform()
  d <- fromJSON(resp_body_string(resp))
  colnames(d) <- d[1, ]
  as_tibble(d[-1, ])
})
df_reb_bruto <- bind_rows(lista_reb)

# 3.2 Tabela 74 - Produção de Origem Animal (v/106: Qtd produzida, v/215: Valor da produção)
urls_produtos <- c(
  "https://apisidra.ibge.gov.br/values/t/74/n6/in%20n3%2026/v/106,215/p/last%205/c80/all",
  "https://apisidra.ibge.gov.br/values/t/74/n3/26/v/106,215/p/last%205/c80/all"
)

cat("📥 Baixando produtos de origem animal (Tabela 74 - Leite, Ovos, Mel)...\n")
lista_prod <- lapply(urls_produtos, function(u) {
  resp <- request(u) |>
    req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
    req_perform()
  d <- fromJSON(resp_body_string(resp))
  colnames(d) <- d[1, ]
  as_tibble(d[-1, ])
})
df_prod_bruto <- bind_rows(lista_prod)

# 4. Gravação da Camada Prata (Raw / Dados Brutos) ------------------------------
saveRDS(df_reb_bruto, file.path(dir_prata, "ppm_rebanhos_bruto.rds"))
saveRDS(df_prod_bruto, file.path(dir_prata, "ppm_origem_animal_bruto.rds"))
cat(glue::glue("💾 [Prata] Arquivos brutos salvos em: {dir_prata}\n"))

# 5. Tratamento e Estruturação (Camada Ouro) ------------------------------------
cat("⚙️ Processando e harmonizando dados de pecuária...\n")

# Tratamento Rebanhos
df_reb_clean <- df_reb_bruto %>%
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
    tipo_de_rebanho,
    valor
  ) %>%
  mutate(
    ibge6 = if_else(nivel_territorial_codigo == "6", substr(unidade_geografica_codigo, 1, 6), NA_character_),
    ibge7 = if_else(nivel_territorial_codigo == "6", unidade_geografica_codigo, NA_character_),
    ano = as.integer(ano),
    efetivo_cabecas = case_when(
      valor %in% c("-", "..", "...", "X", "x") ~ 0,
      TRUE ~ suppressWarnings(as.numeric(valor))
    )
  ) %>%
  select(-valor)

# Tratamento Origem Animal
df_prod_clean <- df_prod_bruto %>%
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
    tipo_de_produto_de_origem_animal,
    variavel,
    valor
  ) %>%
  mutate(
    ibge6 = if_else(nivel_territorial_codigo == "6", substr(unidade_geografica_codigo, 1, 6), NA_character_),
    ibge7 = if_else(nivel_territorial_codigo == "6", unidade_geografica_codigo, NA_character_),
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
      grepl("producao_de_origem_animal|quantidade", .x) ~ "quantidade_produzida",
      grepl("valor_da_producao", .x) ~ "valor_producao_mil_reais",
      TRUE ~ .x
    )
  ) %>%
  mutate(
    valor_producao_reais = if ("valor_producao_mil_reais" %in% names(.)) valor_producao_mil_reais * 1000 else 0
  )

# Consolida Rebanhos e Produtos em um formato estruturado
# Para facilitar consumo analítico no Aurora, salvamos uma tabela unificada e clara
df_ppm_ouro <- bind_rows(
  df_reb_clean %>%
    mutate(
      categoria_agro = "Efetivo de Rebanho",
      item = tipo_de_rebanho,
      unidade_medida = "Cabeças",
      quantidade = efetivo_cabecas,
      valor_reais = NA_real_
    ) %>%
    select(nivel_territorial_codigo, ibge6, ibge7, unidade_geografica, ano, categoria_agro, item, unidade_medida, quantidade, valor_reais),
  
  df_prod_clean %>%
    mutate(
      categoria_agro = "Produto de Origem Animal",
      item = tipo_de_produto_de_origem_animal,
      unidade_medida = "Unidade do Produto (Litros/Dúzias/Kg)",
      quantidade = quantidade_produzida,
      valor_reais = valor_producao_reais
    ) %>%
    select(nivel_territorial_codigo, ibge6, ibge7, unidade_geografica, ano, categoria_agro, item, unidade_medida, quantidade, valor_reais)
) %>%
  mutate(uf = "PE") %>%
  arrange(nivel_territorial_codigo, unidade_geografica, ano, categoria_agro, item)

# 6. Gravação da Camada Ouro (Parquet) ------------------------------------------
arquivo_ouro_parquet <- file.path(dir_ouro, "ppm_pecuaria.parquet")
write_parquet(df_ppm_ouro, arquivo_ouro_parquet)

cat(glue::glue("✅ [Ouro] Base Parquet de pecuária salva: {arquivo_ouro_parquet} ({nrow(df_ppm_ouro)} linhas)\n\n"))

# ==============================================================================
# PIPELINE ETL - PRODUÇÃO AGRÍCOLA MUNICIPAL (PAM - IBGE / SIDRA)
# ==============================================================================
# Extrai dados da PAM para Pernambuco (por município e total UF)
# Tabela SIDRA 5457: Lavouras temporárias e permanentes
# Camada Prata: C:\dados\dados brutos\aurora\agricultura
# Camada Ouro:  C:\dados\aurora\parquet\pam_agricultura.parquet
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

cat("🚜 [PAM] Iniciando extração da Produção Agrícola Municipal (IBGE/SIDRA)...\n")

# 3. Requisição à API SIDRA ----------------------------------------------------
# Tabela 5457:
# /n6/in n3 26 : Todos os municípios de Pernambuco
# /n3/26       : Total do estado de Pernambuco
# /v/214,215,216,1000216 :
#    214      = Quantidade produzida
#    215      = Valor da produção (Mil Reais)
#    216      = Área plantada (Hectares)
#    1000216  = Área colhida (Hectares)
# /p/last 5   : Últimos 5 anos disponíveis
# /c782/0     : Total das lavouras

urls_sidra_pam <- c(
  "https://apisidra.ibge.gov.br/values/t/5457/n6/in%20n3%2026/v/214,215,216,1000216/p/last%205/c782/0",
  "https://apisidra.ibge.gov.br/values/t/5457/n3/26/v/214,215,216,1000216/p/last%205/c782/0"
)

cat("📥 Baixando dados da API SIDRA (Municípios e Total PE)...\n")
lista_pam <- lapply(urls_sidra_pam, function(u) {
  resp <- request(u) |>
    req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
    req_perform()
  d <- fromJSON(resp_body_string(resp))
  colnames(d) <- d[1, ]
  as_tibble(d[-1, ])
})
df_pam_bruto <- bind_rows(lista_pam)

# 4. Gravação da Camada Prata (Raw / Dados Brutos) ------------------------------
arquivo_prata_rds <- file.path(dir_prata, "pam_pe_bruto.rds")
saveRDS(df_pam_bruto, arquivo_prata_rds)
cat(glue::glue("💾 [Prata] Arquivo salvo em: {arquivo_prata_rds}\n"))

# 5. Tratamento, limpeza e enriquecimento (Camada Ouro) -------------------------
cat("⚙️ Processando e estruturando dados para a camada Ouro...\n")

df_pam_ouro <- df_pam_bruto %>%
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
    nivel_territorial,
    unidade_geografica_codigo,
    unidade_geografica,
    ano,
    variavel,
    valor
  ) %>%
  mutate(
    ibge6 = if_else(nivel_territorial_codigo == "6", substr(unidade_geografica_codigo, 1, 6), NA_character_),
    ibge7 = if_else(nivel_territorial_codigo == "6", unidade_geografica_codigo, NA_character_),
    cod_uf = if_else(nivel_territorial_codigo == "3", unidade_geografica_codigo, "26"),
    uf = "PE",
    ano = as.integer(ano),
    # Substitui caracteres especiais de ausência do IBGE por NA
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
  clean_names()

# Padroniza nomes das colunas de métricas
df_pam_ouro <- df_pam_ouro %>%
  rename_with(
    ~ case_when(
      .x == "quantidade_produzida" ~ "quantidade_produzida",
      .x == "valor_da_producao" ~ "valor_producao_mil_reais",
      .x == "area_plantada" ~ "area_plantada_ha",
      .x == "area_colhida" ~ "area_colhida_ha",
      TRUE ~ .x
    )
  )

if (!"area_plantada_ha" %in% names(df_pam_ouro) && "area_colhida_ha" %in% names(df_pam_ouro)) {
  df_pam_ouro$area_plantada_ha <- df_pam_ouro$area_colhida_ha
}

df_pam_ouro <- df_pam_ouro %>%
  mutate(
    # Valor total da produção em Reais (R$)
    valor_producao_reais = if ("valor_producao_mil_reais" %in% names(.)) valor_producao_mil_reais * 1000 else NA_real_,
    # Indicador de produtividade econômica: R$/hectare colhido
    rendimento_economico_ha = if (all(c("valor_producao_reais", "area_colhida_ha") %in% names(.))) {
      if_else(area_colhida_ha > 0, valor_producao_reais / area_colhida_ha, NA_real_)
    } else {
      NA_real_
    }
  ) %>%
  arrange(nivel_territorial_codigo, unidade_geografica, ano)

# 6. Gravação da Camada Ouro (Parquet) ------------------------------------------
arquivo_ouro_parquet <- file.path(dir_ouro, "pam_agricultura.parquet")
write_parquet(df_pam_ouro, arquivo_ouro_parquet)

cat(glue::glue("✅ [Ouro] Base Parquet salva com sucesso: {arquivo_ouro_parquet} ({nrow(df_pam_ouro)} linhas)\n\n"))

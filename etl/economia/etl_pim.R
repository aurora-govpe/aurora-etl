# ==============================================================================
# PIPELINE ETL - PRODUÇÃO INDUSTRIAL MENSAL (PIM-PF - IBGE / SIDRA)
# ==============================================================================
# Extrai os índices de produção física industrial mensal (Pernambuco e Brasil)
# Tabela SIDRA 8888: PIM-PF (Nova série - Base 2022 = 100)
#
# ⚠️ SINALIZAÇÃO TÉCNICA DE GRANULARIDADE (PIM-PF):
# A Pesquisa Industrial Mensal do IBGE é produzida EXCLUSIVAMENTE para os âmbitos
# Estadual (Pernambuco) e Nacional (Brasil). Não há produção municipal pelo IBGE.
#
# O pipeline contempla:
# 1. Indústria Geral
# 2. Recorte Prioritário: "Confecção de artigos do vestuário e acessórios" (CNAE 14)
#    e demais seções/atividades industriais.
#
# Camada Prata (Raw): C:\dados\dados brutos\aurora\economia\pim_pf_bruto.rds
# Camada Ouro (Out):  C:\dados\aurora\parquet\pim_producao_industrial.parquet
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

# 1. Resolução dinâmica de caminhos ---------------------------------------------
caminhos_paths <- c("R/paths.R", "paths.R", "../R/paths.R", "../../R/paths.R", "etl/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- "C:/dados/aurora"

# 2. Definição dos diretórios Prata e Ouro ---------------------------------------
dir_prata <- file.path(DIR_RAW_AURORA, "economia")
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
# /c544/all   : Todas as seções e atividades industriais (incluindo Indústria Geral e Confecção)

urls_pim <- c(
  # Pernambuco (26)
  "https://apisidra.ibge.gov.br/values/t/8888/n3/26/v/11601,11602,11603,11604,12606,12607/p/last%2036/c544/all",
  # Brasil (1)
  "https://apisidra.ibge.gov.br/values/t/8888/n1/1/v/11601,11602,11603,11604,12606,12607/p/last%2036/c544/all"
)

cat("📥 Baixando indicadores industriais da API SIDRA (PE e BR com recorte de confecção e vestuário)...\n")
lista_pim <- lapply(urls_pim, function(u) {
  tryCatch({
    resp <- request(u) |>
      req_headers("User-Agent" = "Aurora-ETL/1.0 (Governo de Pernambuco)") |>
      req_timeout(90) |>
      req_perform()
    d <- fromJSON(resp_body_string(resp))
    colnames(d) <- d[1, ]
    as_tibble(d[-1, ])
  }, error = function(e) {
    message("⚠️ Erro na URL PIM: ", u, " - ", e$message)
    tibble()
  })
})
df_pim_bruto <- bind_rows(lista_pim)

if (nrow(df_pim_bruto) == 0) {
  warning("⚠️ Não foi possível obter novos dados da PIM-PF. Mantendo base anterior se existir.")
} else {
  # 4. Gravação da Camada Prata (Raw / Dados Brutos) ------------------------------
  arquivo_prata_rds <- file.path(dir_prata, "pim_pf_bruto.rds")
  saveRDS(df_pim_bruto, arquivo_prata_rds)
  cat(glue::glue("💾 [Prata] Arquivo salvo em: {arquivo_prata_rds} ({nrow(df_pim_bruto)} linhas)\n"))
  
  # 5. Tratamento, limpeza e estruturação (Camada Ouro) -------------------------
  cat("⚙️ Processando séries temporais da indústria para a camada Ouro...\n")
  
  df_norm <- df_pim_bruto %>% clean_names()
  
  col_atividade_cod <- names(df_norm)[str_detect(names(df_norm), "secoes_e_atividades_industriais_cnae_2_0_codigo|atividades_codigo")][1]
  col_atividade_nom <- names(df_norm)[str_detect(names(df_norm), "secoes_e_atividades_industriais_cnae_2_0|atividades")][1]
  
  df_pim_ouro <- df_norm %>%
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
      atividade_codigo = if (!is.null(col_atividade_cod)) as.character(.data[[col_atividade_cod]]) else "129314",
      atividade_nome   = if (!is.null(col_atividade_nom)) as.character(.data[[col_atividade_nom]]) else "Indústria geral",
      flag_confeccao_vestuario = str_detect(
        atividade_nome, 
        regex("vestu[aá]rio|confec[cç]", ignore_case = TRUE)
      ),
      flag_textil = str_detect(
        atividade_nome, 
        regex("t[eê]xtil|t[eê]xteis", ignore_case = TRUE)
      ),
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
    select(
      nivel_territorial_codigo,
      localidade_codigo,
      localidade,
      localidade_tipo,
      uf,
      data_referencia,
      ano,
      mes = mes_num,
      mes_codigo,
      atividade_codigo,
      atividade_nome,
      flag_confeccao_vestuario,
      flag_textil,
      variavel,
      valor_num
    ) %>%
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
    arrange(localidade_tipo, atividade_nome, data_referencia)
  
  # 6. Gravação da Camada Ouro (Parquet) ------------------------------------------
  arquivo_ouro_parquet <- file.path(dir_ouro, "pim_producao_industrial.parquet")
  write_parquet(df_pim_ouro, arquivo_ouro_parquet)
  
  cat(glue::glue("✅ [Ouro] Base Parquet da indústria salva: {arquivo_ouro_parquet} ({nrow(df_pim_ouro)} linhas)\n\n"))
}

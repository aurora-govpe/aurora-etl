# ==============================================================================
# PIPELINE ETL: MONITORAMENTO DA ESTIAGEM AGRÍCOLA (LSPA + PAM - IBGE)
# ==============================================================================
# Plataforma: Aurora ETL
# Objetivo  : Extrair a série mensal atualizada do LSPA (Tabela 6588) e o histórico
#             municipal do PAM (Tabela 5457) via API do SIDRA / IBGE, calculando:
#             - Taxa de quebra/perda de safra recente (Área Plantada vs Área Colhida)
#             - Evolução mensal durante o ciclo de estiagem
#             - Base consolidada para cruzamento com os 185 municípios do Caderno Técnico
# Saídas    :
#   - Nível Ouro (Gold / Consumo no App): C:/dados/aurora/parquet/*.parquet
#   - Nível Prata (Silver / Armazenamento): C:/dados/dados brutos/aurora/agricultura/*.rds e *.xlsx
# Autor     : Hugo Vasconcelos / Aurora Project
# Data      : 2026-11-20
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(readr)
  library(writexl)
  library(arrow)
})

# ------------------------------------------------------------------------------
# 1. RESOLUÇÃO DE CAMINHOS DO PROJETO AURORA
# ------------------------------------------------------------------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else {
  DIR_RAW_AURORA <- "C:/dados/dados brutos/aurora"
  DIR_OUT_AURORA <- "C:/dados/aurora"
}

# Nível Prata: RDS e Excel em dados brutos
dir_dest_prata <- file.path(DIR_RAW_AURORA, "economia")

# Nível Ouro: Parquet para consumo no App
dir_dest_parquet <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_dest_prata)) dir.create(dir_dest_prata, recursive = TRUE)
if (!dir.exists(dir_dest_parquet)) dir.create(dir_dest_parquet, recursive = TRUE)

message("====================================================================")
message("Iniciando ingestão da Estiagem Agrícola (LSPA / SIDRA)...")
message("Destino PRATA   (RDS / Excel): ", dir_dest_prata)
message("Destino PARQUET (Gold / App) : ", dir_dest_parquet)
message("====================================================================")

# ------------------------------------------------------------------------------
# 2. EXTRAÇÃO LSPA (TABELA 6588): MENSAL E ATUALIZADO PARA O ANO CORRENTE
# ------------------------------------------------------------------------------
# Variáveis: 109 (Área plantada - ha), 216 (Área colhida - ha), 35 (Produção - t)
# Produtos : 0 (Total), 39428 (Cereais/Leguminosas), 39439 (Feijão), 39443 (Milho), 39442 (Mandioca)
# Períodos : últimos 36 meses (/p/last%2036) para capturar o ciclo prévio e atual
# ------------------------------------------------------------------------------
produtos_lspa <- c(
  "0",      # Total das Lavouras
  "39428",  # Cereais, leguminosas e oleaginosas
  "39439",  # Feijão (1ª Safra)
  "39443",  # Milho (1ª Safra)
  "39442"   # Mandioca
)
cod_produtos <- paste(produtos_lspa, collapse = ",")

url_lspa <- sprintf(
  "https://apisidra.ibge.gov.br/values/t/6588/n3/26/v/109,216,35,36/p/last%%2036/c48/%s",
  cod_produtos
)

message("Consultando API SIDRA para LSPA de Pernambuco (últimos 36 meses)...")

req_lspa <- request(url_lspa) |>
  req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AuroraETL/1.0") |>
  req_timeout(30)

res_lspa <- tryCatch({
  req_perform(req_lspa)
}, error = function(e) {
  message("[ERRO] Falha na conexão com API SIDRA (LSPA): ", e$message)
  NULL
})

if (!is.null(res_lspa) && resp_status(res_lspa) == 200) {
  raw_lspa <- fromJSON(resp_body_string(res_lspa))
  colnames(raw_lspa) <- raw_lspa[1, ]
  
  df_lspa <- as_tibble(raw_lspa[-1, ]) |>
    clean_names() |>
    transmute(
      nivel_geografico = nivel_territorial,
      uf = unidade_da_federacao,
      cod_periodo = mes_codigo,
      mes_ano = mes,
      ano = as.integer(str_sub(mes_codigo, 1, 4)),
      mes_num = as.integer(str_sub(mes_codigo, 5, 6)),
      produto = produto_das_lavouras,
      variavel = variavel,
      unidade = unidade_de_medida,
      valor = suppressWarnings(as.numeric(na_if(valor, "-")))
    ) |>
    pivot_wider(
      names_from = variavel,
      values_from = valor
    ) |>
    clean_names()
  
  # Calcula indicadores de quebra de safra
  if ("area_plantada" %in% colnames(df_lspa) && "area_colhida" %in% colnames(df_lspa)) {
    df_lspa <- df_lspa |>
      mutate(
        area_perdida_ha = pmax(0, area_plantada - coalesce(area_colhida, area_plantada)),
        taxa_perda_pct = if_else(area_plantada > 0, round((area_perdida_ha / area_plantada) * 100, 2), 0)
      )
  }
  
  message("[OK] LSPA processado com sucesso: ", nrow(df_lspa), " registros mensais.")
  
  # 1. Nível Prata: Salva em RDS e Excel em dados brutos
  saveRDS(df_lspa, file.path(dir_dest_prata, "lspa_pernambuco_serie_mensal.rds"))
  write_xlsx(df_lspa, file.path(dir_dest_prata, "lspa_pernambuco_serie_mensal.xlsx"))
  message("  -> Salvo Prata: ", file.path(dir_dest_prata, "lspa_pernambuco_serie_mensal.rds / .xlsx"))
  
  # 2. Nível Ouro: Salva em Parquet no diretório central de parquets
  write_parquet(df_lspa, file.path(dir_dest_parquet, "lspa_pernambuco_serie_mensal.parquet"))
  message("  -> Salvo Ouro : ", file.path(dir_dest_parquet, "lspa_pernambuco_serie_mensal.parquet"))
} else {
  df_lspa <- NULL
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO PAM HISTÓRICA MUNICIPAL (TABELA 5457) PARA OS 185 MUNICÍPIOS
# ------------------------------------------------------------------------------
# Tabela 5457: Área plantada (214), Área colhida (215), Quantidade produzida (216)
# Nível 6: Todos os municípios de Pernambuco (n6/in%20n3%2026)
# Anos recentes: últimos 5 anos consolidados
# ------------------------------------------------------------------------------
message("\nConsultando API SIDRA para PAM Municipal (185 municípios de PE)...")

url_pam <- "https://apisidra.ibge.gov.br/values/t/5457/n6/in%20n3%2026/v/214,215,216/p/last%205/c782/0"

req_pam <- request(url_pam) |>
  req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AuroraETL/1.0") |>
  req_timeout(45)

res_pam <- tryCatch({
  req_perform(req_pam)
}, error = function(e) {
  message("[ERRO] Falha na conexão com API SIDRA (PAM): ", e$message)
  NULL
})

if (!is.null(res_pam) && resp_status(res_pam) == 200) {
  raw_pam <- fromJSON(resp_body_string(res_pam))
  colnames(raw_pam) <- raw_pam[1, ]
  
  df_pam <- as_tibble(raw_pam[-1, ]) |>
    clean_names() |>
    transmute(
      cod_ibge = municipio_codigo,
      municipio = str_replace(municipio, " - PE", ""),
      ano = as.integer(ano),
      variavel = variavel,
      valor = suppressWarnings(as.numeric(na_if(valor, "...")))
    ) |>
    pivot_wider(
      names_from = variavel,
      values_from = valor
    ) |>
    clean_names()
  
  # Calcula taxa de perda histórica municipal
  if ("area_plantada" %in% colnames(df_pam) && "area_colhida" %in% colnames(df_pam)) {
    df_pam <- df_pam |>
      mutate(
        area_perdida_ha = pmax(0, coalesce(area_plantada, 0) - coalesce(area_colhida, 0)),
        taxa_perda_pct = if_else(area_plantada > 0, round((area_perdida_ha / area_plantada) * 100, 2), 0)
      )
  }
  
  message("[OK] PAM Municipal processado: ", nrow(df_pam), " linhas (185 municípios).")
  
  # 1. Nível Prata: Salva em RDS e Excel em dados brutos
  saveRDS(df_pam, file.path(dir_dest_prata, "pam_municipios_pe_historico.rds"))
  write_xlsx(df_pam, file.path(dir_dest_prata, "pam_municipios_pe_historico.xlsx"))
  message("  -> Salvo Prata: ", file.path(dir_dest_prata, "pam_municipios_pe_historico.rds / .xlsx"))
  
  # 2. Nível Ouro: Salva em Parquet no diretório central de parquets
  write_parquet(df_pam, file.path(dir_dest_parquet, "pam_municipios_pe_historico.parquet"))
  message("  -> Salvo Ouro : ", file.path(dir_dest_parquet, "pam_municipios_pe_historico.parquet"))
} else {
  df_pam <- NULL
}

# ------------------------------------------------------------------------------
# 4. RESUMO EXECUTIVO NO CONSOLE
# ------------------------------------------------------------------------------
if (!is.null(df_lspa)) {
  message("\n--- RESUMO RECENTE DA ESTIAGEM AGRÍCOLA (ÚLTIMOS MESES EM PE) ---")
  resumo_recente <- df_lspa |>
    filter(produto %in% c("Total", "1 Cereais, leguminosas e oleaginosas")) |>
    select(mes_ano, produto, area_plantada, area_colhida, area_perdida_ha, taxa_perda_pct) |>
    arrange(desc(mes_ano)) |>
    head(6)
  
  print(as.data.frame(resumo_recente))
}

message("====================================================================")
message("ETL Agrícola concluído com sucesso!")
message("====================================================================")

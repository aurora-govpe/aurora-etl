# ==============================================================================
# etl/municipios/etl_infocom_servicos.R
# ==============================================================================
# Objetivo: Calcular e integrar os indicadores de participação de Informação
#           e Comunicação (InfoCom) em relação a Serviços para todos os 185
#           municípios de Pernambuco:
#           1. pct_vab_infocom_serv (% InfoCom no VAB de Serviços)
#           2. pct_estab_infocom_serv (% InfoCom nos estabelecimentos de Serviços)
#           3. pct_emp_infocom_serv (% InfoCom no total de empregados de Serviços)
#           4. ql_infocom_serv (Quociente Locacional específico de InfoCom em Serviços)
#           5. estab_infocom (Total de estabelecimentos InfoCom)
#           6. emp_infocom (Total de vínculos empregatícios InfoCom)
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cat("==============================================================================\n")
cat("ETL: Indicadores de InfoCom em relação a Serviços (185 Municípios de PE)\n")
cat("==============================================================================\n\n")

proj_dir <- "C:/Users/hugo.vasconcelos/Documents/projetos/segpr_ndgr/auroraprime"
data_aurora <- file.path(proj_dir, "app_aurora_municipios/data")
p_demo <- file.path(data_aurora, "parquet/df_demografia_pe.parquet")
p_rais <- file.path(data_aurora, "df_rais_tecnologia.parquet")
p_ind <- file.path(data_aurora, "parquet/df_indicadores.parquet")

df_demo <- arrow::read_parquet(p_demo)
df_rais <- arrow::read_parquet(p_rais)
df_ind_original <- arrow::read_parquet(p_ind)

cat("1. df_demografia_pe carregado:", nrow(df_demo), "municípios oficiais.\n")
cat("2. df_rais_tecnologia carregado:", nrow(df_rais), "linhas.\n")
cat("3. df_indicadores original carregado:", nrow(df_ind_original), "linhas.\n\n")

# Base mestra com os 185 municípios de Pernambuco
df_muns <- df_demo %>%
  transmute(
    cod_ibge = as.character(ibge7),
    ibge6 = substr(as.character(ibge7), 1, 6),
    cod_municipio = substr(as.character(ibge7), 1, 6),
    nm_municipio = as.character(municipio)
  )

anos_rais <- sort(unique(df_rais$ano))
cat("Anos disponíveis na RAIS:", paste(anos_rais, collapse = ", "), "\n")

lista_indicadores_novos <- list()

for (ano_corrente in anos_rais) {
  cat(sprintf("\n--- Processando Ano: %d ---\n", ano_corrente))
  
  df_ano <- df_rais %>%
    filter(ano == ano_corrente) %>%
    mutate(
      valor_num = suppressWarnings(as.numeric(valor)),
      ibge6 = substr(as.character(id_municipio), 1, 6)
    )
  
  piv <- df_ano %>%
    filter(sigla_indicador %in% c("estab_serv", "estab_infocom", "emp_serv", "emp_infocom", "sal_avg_serv", "sal_avg_infocom")) %>%
    select(ibge6, sigla_indicador, valor_num) %>%
    pivot_wider(names_from = sigla_indicador, values_from = valor_num)
  
  # Totais estaduais PE no ano
  pe_estab_serv <- sum(piv$estab_serv, na.rm = TRUE)
  pe_estab_infocom <- sum(piv$estab_infocom, na.rm = TRUE)
  pe_pct_estab <- (pe_estab_infocom / pe_estab_serv) * 100
  
  pe_emp_serv <- sum(piv$emp_serv, na.rm = TRUE)
  pe_emp_infocom <- sum(piv$emp_infocom, na.rm = TRUE)
  pe_pct_emp <- (pe_emp_infocom / pe_emp_serv) * 100
  
  massa_serv_pe <- sum(piv$emp_serv * coalesce(piv$sal_avg_serv, 0), na.rm = TRUE)
  massa_infocom_pe <- sum(piv$emp_infocom * coalesce(piv$sal_avg_infocom, piv$sal_avg_serv, 0), na.rm = TRUE)
  pe_pct_vab <- (massa_infocom_pe / massa_serv_pe) * 100
  
  cat(sprintf("   PE Totais -> %% Estab InfoCom/Serv: %.3f%% | %% Emp InfoCom/Serv: %.3f%% | %% VAB (Massa): %.3f%%\n",
              pe_pct_estab, pe_pct_emp, pe_pct_vab))
  
  calc_mun <- df_muns %>%
    left_join(piv, by = "ibge6") %>%
    mutate(
      estab_infocom_val = coalesce(estab_infocom, 0),
      emp_infocom_val   = coalesce(emp_infocom, 0),
      
      # 1. % Estabelecimentos InfoCom em relação a Serviços
      pct_estab_infocom_serv = if_else(coalesce(estab_serv, 0) > 0, (estab_infocom_val / estab_serv) * 100, 0),
      
      # 2. % Empregados InfoCom em relação a Serviços
      pct_emp_infocom_serv = if_else(coalesce(emp_serv, 0) > 0, (emp_infocom_val / emp_serv) * 100, 0),
      
      # 3. % VAB InfoCom em Serviços (via massa salarial)
      massa_infocom = emp_infocom_val * coalesce(sal_avg_infocom, sal_avg_serv, 0),
      massa_serv = coalesce(emp_serv, 0) * coalesce(sal_avg_serv, 0),
      pct_vab_infocom_serv = case_when(
        massa_serv > 0 ~ (massa_infocom / massa_serv) * 100,
        coalesce(emp_serv, 0) > 0 ~ (emp_infocom_val / emp_serv) * 100,
        TRUE ~ 0
      ),
      
      # 4. Quociente Locacional de InfoCom em Serviços
      ql_infocom_serv = if (pe_pct_emp > 0) (pct_emp_infocom_serv / pe_pct_emp) else 0
    )
  
  gerar_indicador <- function(df, coluna_val, sigla, nome, tipo) {
    df %>%
      transmute(
        cod_municipio,
        nm_municipio,
        ano = as.numeric(ano_corrente),
        nm_indicador = nome,
        indicador = if (tipo == "absoluto") as.character(round(.data[[coluna_val]])) else as.character(round(.data[[coluna_val]], 2)),
        mes_referencia = as.character(NA),
        sigla_indicador = sigla,
        grupo = "economia da inovação",
        tipo = tipo,
        cod_ibge,
        ibge6,
        valor_rank = .data[[coluna_val]]
      ) %>%
      arrange(desc(valor_rank)) %>%
      mutate(rank_pe = as.integer(row_number())) %>%
      select(-valor_rank)
  }
  
  i1 <- gerar_indicador(calc_mun, "pct_vab_infocom_serv", "pct_vab_infocom_serv", "% InfoCom em relação a Serviços no VAB", "percentual")
  i2 <- gerar_indicador(calc_mun, "pct_estab_infocom_serv", "pct_estab_infocom_serv", "% InfoCom nos Estabelecimentos de Serviços", "percentual")
  i3 <- gerar_indicador(calc_mun, "pct_emp_infocom_serv", "pct_emp_infocom_serv", "% InfoCom nos Empregados de Serviços", "percentual")
  i4 <- gerar_indicador(calc_mun, "ql_infocom_serv", "ql_infocom_serv", "QL InfoCom em Serviços", "indice")
  i5 <- gerar_indicador(calc_mun, "estab_infocom_val", "estab_infocom", "Estabelecimentos de Informação e Comunicação", "absoluto")
  i6 <- gerar_indicador(calc_mun, "emp_infocom_val", "emp_infocom", "Empregados em Informação e Comunicação", "absoluto")
  
  lista_indicadores_novos[[as.character(ano_corrente)]] <- bind_rows(i1, i2, i3, i4, i5, i6)
}

df_novos_indicadores <- bind_rows(lista_indicadores_novos)
cat("\nTotal de novas linhas geradas:", nrow(df_novos_indicadores), "linhas (exatamente 185 muns x 6 indicadores x 4 anos).\n")

# Limpar versões anteriores das siglas em df_indicadores_original
siglas_novas <- unique(df_novos_indicadores$sigla_indicador)
df_ind_final <- df_ind_original %>%
  filter(!(sigla_indicador %in% siglas_novas)) %>%
  bind_rows(df_novos_indicadores)

cat("Tamanho final de df_indicadores:", nrow(df_ind_final), "linhas.\n")

# Salvar no parquet
arrow::write_parquet(df_ind_final, p_ind)
cat("Arquivo salvo com sucesso em:", p_ind, "\n")

p_ind_raiz <- file.path(proj_dir, "data/indicadores/df_indicadores.parquet")
if (file.exists(dirname(p_ind_raiz))) {
  arrow::write_parquet(df_ind_final, p_ind_raiz)
  cat("Arquivo espelho salvo em:", p_ind_raiz, "\n")
}

cat("\n[OK] Indicadores de InfoCom em Serviços consolidados com sucesso!\n")

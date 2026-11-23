# ==============================================================================
# PIPELINE ETL - NOVO CAGED (MINISTÉRIO DO TRABALHO E EMPREGO)
# ==============================================================================
# Extrai os microdados mensais do Novo CAGED para Pernambuco (UF 26),
# agregando admissões, desligamentos e saldo líquido por município e CNAE.
#
# Utiliza o pacote {cagedr} (StrategicProjects):
# https://strategicprojects.github.io/cagedr/
#
# Camada Bruta (Raw/Bronze):  C:/dados/dados brutos/aurora/economia/
# Camada Curada (Gold/Out):   C:/dados/aurora/parquet/df_caged_municipios_pe.parquet
# ==============================================================================

# 1. Carregamento de Pacotes ---------------------------------------------------
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

garantir_pacote("cagedr", "StrategicProjects/cagedr")

suppressPackageStartupMessages({
  library(cagedr)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
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
dir_raw_textil <- file.path(dir_raw_econ, "textil")
dir_out_pqt  <- file.path(DIR_OUT_AURORA, "parquet")

if (!dir.exists(dir_raw_econ)) dir.create(dir_raw_econ, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_raw_textil)) dir.create(dir_raw_textil, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_out_pqt))  dir.create(dir_out_pqt, recursive = TRUE, showWarnings = FALSE)

cat("👔 [CAGED] Iniciando extração e processamento do Novo CAGED (Pernambuco)...\n")

# Configura cache local do cagedr para o diretório de dados brutos
options(cagedr.cache_dir = file.path(dir_raw_econ, "caged_cache"))
if (!dir.exists(getOption("cagedr.cache_dir"))) {
  dir.create(getOption("cagedr.cache_dir"), recursive = TRUE, showWarnings = FALSE)
}

# 3. Identificação dos Meses Disponíveis ----------------------------------------
meses_disp <- tryCatch({
  cagedr::caged_available()
}, error = function(e) {
  message("⚠️ Aviso ao listar meses do CAGED no servidor FTP: ", e$message)
  NULL
})

# Se disponível, filtra os últimos 24 meses; caso contrário, define janela padrão
if (!is.null(meses_disp) && length(meses_disp) > 0) {
  # Ordena e seleciona competências recentes
  meses_alvo <- tail(sort(meses_disp), 24)
} else {
  # Fallback: meses recentes até o mês anterior
  mes_atual <- Sys.Date()
  meses_seq <- seq(from = mes_atual - months(24), to = mes_atual - months(1), by = "1 month")
  meses_alvo <- format(meses_seq, "%Y%m")
}

cat(glue::glue("📥 Competências alvo para extração: {min(meses_alvo)} a {max(meses_alvo)} ({length(meses_alvo)} meses)\n"))

# 4. Leitura e Agregação por Mês ----------------------------------------------
# Microdados: filtra UF = 26 (Pernambuco)
colunas_interesse <- c(
  "competenciamov", "uf", "municipio", "secao", "subclasse", 
  "saldomovimentacao", "tipomovimentacao", "graudeinstrucao", "idade", "sexo"
)

dados_caged_pe <- tryCatch({
  # Executa download e leitura em stream com filtro direto de UF
  cagedr::caged_fetch(
    months = meses_alvo,
    uf = 26, # Pernambuco
    keep_cols = colunas_interesse
  )
}, error = function(e) {
  message("⚠️ Erro no caged_fetch direto: ", e$message)
  message("Tentando leitura mês a mês com fallback...")
  
  lista_meses <- list()
  for (m in meses_alvo) {
    res_m <- tryCatch({
      cagedr::caged_fetch(months = m, uf = 26)
    }, error = function(err) NULL)
    if (!is.null(res_m) && nrow(res_m) > 0) {
      lista_meses[[m]] <- res_m
    }
  }
  bind_rows(lista_meses)
})

if (is.null(dados_caged_pe) || nrow(dados_caged_pe) == 0) {
  warning("⚠️ Não foi possível obter novos microdados do CAGED na execução atual. Mantendo base anterior se existir.")
} else {
  cat(glue::glue("📊 Registros brutos de movimentação carregados para PE: {nrow(dados_caged_pe)}\n"))
  
  # Salva camada bruta em RDS (Geral e Recorte Têxtil)
  saveRDS(dados_caged_pe, file.path(dir_raw_econ, "caged_pe_movimentacoes_bruto.rds"))
  cat(glue::glue("💾 [Prata] Arquivo geral salvo: {file.path(dir_raw_econ, 'caged_pe_movimentacoes_bruto.rds')}\n"))

  # Filtro e salvamento na subpasta textil
  cnae_clean <- str_pad(as.character(dados_caged_pe$subclasse), width = 7, side = "left", pad = "0")
  cnae_div_raw <- substr(cnae_clean, 1, 2)
  mask_textil_raw <- cnae_div_raw %in% c("13", "14") | str_starts(cnae_clean, "478")
  dados_caged_textil <- dados_caged_pe[mask_textil_raw, ]
  
  saveRDS(dados_caged_textil, file.path(dir_raw_textil, "caged_pe_textil_bruto.rds"))
  cat(glue::glue("💾 [Prata - Têxtil] Salvo recorte têxtil: {file.path(dir_raw_textil, 'caged_pe_textil_bruto.rds')} ({nrow(dados_caged_textil)} linhas)\n"))
  
  # 5. Tratamento e Agregações Analíticas ---------------------------------------
  # Identifica divisão CNAE (primeiros 2 dígitos da subclasse)
  dados_tratados <- dados_caged_pe %>%
    clean_names() %>%
    mutate(
      cod_mun = as.character(municipio),
      # Padroniza código do município para 7 dígitos (ou 6 conforme padrão CAGED)
      cod_mun_ibge = case_when(
        nchar(cod_mun) == 6 ~ cod_mun,
        nchar(cod_mun) == 7 ~ substr(cod_mun, 1, 6),
        TRUE ~ cod_mun
      ),
      ano_mes = as.character(competenciamov),
      ano = as.integer(substr(ano_mes, 1, 4)),
      mes = as.integer(substr(ano_mes, 5, 6)),
      cnae_subclasse = as.character(subclasse),
      cnae_divisao = substr(str_pad(cnae_subclasse, width = 7, side = "left", pad = "0"), 1, 2),
      saldo = as.numeric(saldomovimentacao),
      admissao = if_else(saldo > 0, 1, 0),
      desligamento = if_else(saldo < 0, 1, 0),
      # Flags temáticas setoriais
      flag_textil = cnae_divisao %in% c("13", "14"),
      flag_confeccao = cnae_divisao == "14",
      flag_comercio_vestuario = str_starts(cnae_subclasse, "478")
    )
  
  # A. Agregação Municipal x Mensal x CNAE Divisão
  df_caged_muni_cnae <- dados_tratados %>%
    group_by(cod_mun_ibge, ano_mes, ano, mes, secao, cnae_divisao) %>%
    summarise(
      admissoes = sum(admissao, na.rm = TRUE),
      desligamentos = sum(desligamento, na.rm = TRUE),
      saldo_liquido = sum(saldo, na.rm = TRUE),
      .groups = "drop"
    )
  
  # B. Agregação Municipal x Mensal Geral (com destaque polo têxtil/vestuário)
  df_caged_muni_mes <- dados_tratados %>%
    group_by(cod_mun_ibge, ano_mes, ano, mes) %>%
    summarise(
      total_admissoes = sum(admissao, na.rm = TRUE),
      total_desligamentos = sum(desligamento, na.rm = TRUE),
      saldo_liquido_total = sum(saldo, na.rm = TRUE),
      # Recorte Polo Têxtil / Confecção (CNAE 13 e 14)
      admissoes_textil_confeccao = sum(admissao[flag_textil], na.rm = TRUE),
      desligamentos_textil_confeccao = sum(desligamento[flag_textil], na.rm = TRUE),
      saldo_textil_confeccao = sum(saldo[flag_textil], na.rm = TRUE),
      # Recorte Varejo de Vestuário (CNAE 47.8)
      admissoes_varejo_vestuario = sum(admissao[flag_comercio_vestuario], na.rm = TRUE),
      desligamentos_varejo_vestuario = sum(desligamento[flag_comercio_vestuario], na.rm = TRUE),
      saldo_varejo_vestuario = sum(saldo[flag_comercio_vestuario], na.rm = TRUE),
      .groups = "drop"
    )
  
  # 6. Gravação da Camada Curada (Ouro / Parquet) -------------------------------
  out_pqt_detalhe <- file.path(dir_out_pqt, "df_caged_municipios_cnae_pe.parquet")
  out_pqt_resumo  <- file.path(dir_out_pqt, "df_caged_municipios_pe.parquet")
  
  arrow::write_parquet(df_caged_muni_cnae, out_pqt_detalhe)
  arrow::write_parquet(df_caged_muni_mes, out_pqt_resumo)
  
  cat(glue::glue("💾 [CAGED] Salvo detalhado por CNAE: {out_pqt_detalhe} ({nrow(df_caged_muni_cnae)} linhas)\n"))
  cat(glue::glue("💾 [CAGED] Salvo resumo municipal mensal: {out_pqt_resumo} ({nrow(df_caged_muni_mes)} linhas)\n"))
}

cat("✅ [CAGED] Pipeline finalizado com sucesso!\n")

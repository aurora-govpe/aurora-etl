# ==============================================================================
# PIPELINE ETL - ORQUESTRADOR CENTRAL
# ==============================================================================

# 1. Resolução central de caminhos ---------------------------------------------
caminhos_paths <- c("R/paths.R", "paths.R", "../R/paths.R", "etl/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

pasta_scripts <- if (dir.exists("etl")) "etl" else "."

# 2. Definição do escopo de execução -------------------------------------------
# Opções: "diario", "semanal", "mensal", "bimestral", "anual", "legado", "servidor"
# Para rodar mais de um tipo, combine: c("diario", "semanal")
periodicidade <- "diario" 

# 3. Dicionário de rotinas por periodicidade -----------------------------------
rotinas <- list(
  
  # ROTINA PADRÃO (DIA A DIA)
  diario = c(
    'etl_00_iniciativas_global.R',
    'etl_01_iniciativas_compesa.R',
    'etl_02_iniciativas_secult_see.R',
    'etl_03_iniciativas_ipa_sepe.R',
    'etl_04_seduh.R',
    'etl_05_iniciativas_join.R',

    'fiscal/etl_emendas.R',
    'fiscal/etl_op_cred.R',
    'fical/etl_execucao_bdpe.R'
    # 'fiscal/etl_execucao_IGPE.R',
    # 'fiscal/etl_execucao_bdpe_IGPE.R'
    
    # 'etl_carga_servidor.R'
  ),
  
  # ROTINAS SEMANAIS
  semanal = c(
    'R/gerador_pdfs.R',
    'mulungu/ETL_AUTO_MULUNGU.R',
    'RPA/rpa_emendas.py', 
    'RPA/rpa_cadunico.py',
    'etl_cozinhas.R'
  ),
  
  # ROTINAS MENSAIS
  mensal = c(
    'RPA/rpa_compesa.py',
    'etl_pac.R',
    'municipios/etl_mvi_cvp_muni.R',
    'municipios/etl_caracter_mun.R',
    'municipios/etl_ficha_municipal.R',
    'R/gerador_pdfs.R'
  ),
  
  # ROTINAS BIMESTRAIS (Jan, Mar, Mai, Jul, Set, Nov)
  bimestral = c(
    'fiscal/etl_rreo_mun_pe.R',
    'fiscal/etl_rreo_ufs.R',
    'fiscal/etl_predicoes_ufs_coleta.R',
    'fiscal/etl_predicoes_ufs_modelos.R',
    'fiscal/etl_prescricoes_ufs_governanca.R',
    'etl_PCP.R'
  ),
  
  # ROTINAS ANUAIS / SOB DEMANDA
  anual = c(
    'etl_agricultura_sidra.R',
    'etl_movimentacoes_aeroportuarias.R',
    'municipios/etl_indicadores_ufs.R',
    'municipios/etl_urbanizacao.R',
    'municipios/etl_indicadores_regioes_br.R',
    'municipios/etl_demografia_pe.R',
    'municipios/etl_vulnerabilidade_social.R'
  ),
  
  # ROTINAS DE SERVIDOR / ESPECIAIS
  servidor = c(
    'fiscal/etl_transferencias_voluntarias.R'
  )
  
)

# 4. Consolidação da fila de execução ------------------------------------------
fila_execucao <- unlist(rotinas[periodicidade], use.names = FALSE)

if (length(fila_execucao) == 0) {
  stop("Nenhum script configurado para a(s) periodicidade(s) selecionada(s).")
}

cat("🚀 Iniciando pipeline ETL...\n")
cat(glue::glue("📅 Periodicidade(s) selecionada(s): {paste(periodicidade, collapse = ', ')}\n"))
cat(glue::glue("📋 Total de scripts na fila: {length(fila_execucao)}\n\n"))

tictoc::tic('Tempo ETL total')

# 5. Execução em lote ----------------------------------------------------------
for (script in fila_execucao) {
  caminho <- file.path(pasta_scripts, script)
  
  if (!file.exists(caminho)) {
    warning(glue::glue("⚠️ Arquivo não localizado: {caminho}. Pulando..."))
    next
  }
  
  cat(glue::glue("\n🔧 Executando: {script}...\n"))
  
  # Trata scripts R e scripts Python (.py)
  if (grepl("\\.R$", script, ignore.case = TRUE)) {
    source(caminho, local = TRUE)
  } else if (grepl("\\.py$", script, ignore.case = TRUE)) {
    system2("python", args = shQuote(caminho))
  } else {
    warning(glue::glue("Formato não suportado para o arquivo: {script}"))
  }
}

tictoc::toc()
cat("\n✅ Todos os scripts selecionados foram executados.\n")
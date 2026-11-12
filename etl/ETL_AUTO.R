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
periodicidade <- "diario"  # Altere conforme necessário

# 3. Dicionário de rotinas por periodicidade -----------------------------------
# DICA: Use caminhos relativos à pasta_scripts (ou à raiz do projeto, se fora de etl/)
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
    'fiscal/etl_execucao_bdpe.R'
    # 'fiscal/etl_execucao_IGPE.R',
    # 'fiscal/etl_execucao_bdpe_IGPE.R'
    
    # 'etl_carga_servidor.R'
  ),
  
  # ROTINAS SEMANAIS
  semanal = c(
    'rpa/rpa_emendas.py',
    'rpa/rpa_cadunico.py',
    'etl_cozinhas.R',
    'R/gerador_pdfs.R',
    'mulungu/ETL_AUTO_MULUNGU.R'
  ),
  
  # ROTINAS MENSAIS
  mensal = c(
    'rpa/rpa_compesa.py',
    'etl/rpa/rpa_anac_dados_estatisticos.py',
    'etl_pac.R',
    'municipios/etl_mvi_cvp_muni.R',
    'municipios/etl_caracter_mun.R',
    'municipios/etl_ficha_municipal.R',
    'R/gerador_pdfs.R'
  ),
  
  # ROTINAS BIMESTRAIS
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
cat(glue::glue("📁 Diretório base de scripts: {pasta_scripts}\n"))
cat(glue::glue("📅 Periodicidade(s) selecionada(s): {paste(periodicidade, collapse = ', ')}\n"))
cat(glue::glue("📋 Total de scripts na fila: {length(fila_execucao)}\n\n"))

tictoc::tic('Tempo ETL total')

# ==============================================================================
# 5. Execução em lote com interrupção em caso de erro --------------------------
# ==============================================================================

# ⚠️ DEFINA AQUI O SEU INTERPRETADOR PYTHON CORRETO:
# Se usar ambiente virtual (venv), aponte para ele. Exemplos:
# Windows: "C:/Users/juliabarreto/Desktop/1. seplag/aurora-etl/venv/Scripts/python.exe"
# Ou se estiver no PATH global mas precisar do python certo: "python"
python_exec <- "python" # Altere para o caminho do seu venv se necessário

for (script in fila_execucao) {
  # Prioriza o caminho dentro de pasta_scripts; faz fallback para a raiz (ex: R/gerador_pdfs.R)
  caminho_candidato <- if (pasta_scripts == ".") script else file.path(pasta_scripts, script)
  caminho <- if (file.exists(caminho_candidato)) caminho_candidato else script
  
  if (!file.exists(caminho)) {
    stop(glue::glue("❌ Arquivo não localizado: {caminho_candidato} (nem em {script}). Abortando pipeline."))
  }
  
  cat(glue::glue("\n🔧 Executando: {caminho}...\n"))
  
  # 1. Tratamento para scripts R
  if (grepl("\\.R$", caminho, ignore.case = TRUE)) {
    # source local captura o erro e para se houver stop() dentro do script R
    resultado_R <- tryCatch({
      source(caminho, local = TRUE)
      TRUE
    }, error = function(e) {
      cat(glue::glue("\n❌ ERRO CRÍTICO no script R: {caminho}\n"))
      cat(paste("Detalhes:", e$message, "\n"))
      FALSE
    })
    
    if (!resultado_R) {
      stop(glue::glue("Pipeline interrompido devido a falha em: {caminho}"))
    }
    
    # 2. Tratamento para scripts Python (.py)
  } else if (grepl("\\.py$", caminho, ignore.case = TRUE)) {
    # Executa o python capturando o código de saída (status)
    # stderr = "" faz com que os erros do Python apareçam direto no console do R
    status_code <- system2(python_exec, args = shQuote(caminho), stderr = "")
    
    # Se o status for diferente de 0, o Python encontrou um erro (ex: ModuleNotFoundError)
    if (!is.null(status_code) && status_code != 0) {
      stop(glue::glue("❌ Pipeline interrompido! O script Python '{caminho}' falhou com código de saída {status_code}."))
    }
    
  } else {
    warning(glue::glue("Formato não suportado para o arquivo: {caminho}"))
  }
}

tictoc::toc()
cat("\n✅ Todos os scripts selecionados foram executados com sucesso!\n")
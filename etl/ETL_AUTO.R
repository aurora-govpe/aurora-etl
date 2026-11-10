# Carrega resolucao central de caminhos
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../R/paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

cat("🚀 Iniciando execução dos scripts...\n")

tictoc::tic('Tempo ETL total')

# Definir caminho da pasta dos scripts de forma dinamica
pasta_scripts <- if (dir.exists("etl")) "etl" else "."

# Lista os scripts que você quer executar
scripts <- c(
  'municipios/etl_ficha_municipal.R',
  
  'etl_00_iniciativas_global.R',
  'etl_01_iniciativas_compesa.R',
  'etl_02_iniciativas_secult_see.R',
  'etl_03_iniciativas_ipa_sepe.R',
  'etl_04_seduh.R',
  'etl_05_iniciativas_join.R',
  'etl_pac.R', # pegar todo mês do site da casa civil
  'municipios/etl_urbanizacao.R', 
  'municipios/etl_demografia_pe.R',
  'municipios/etl_vulnerabilidade_social.R',
  'municipios/etl_mvi_cvp_muni.R',
  'municipios/etl_caracter_mun.R',
  'fiscal/etl_emendas.R',
  'fiscal/etl_op_cred.R',
  #'fiscal/etl_execucao_bdpe.R', # esperar ate regularizar no bdpe
  'fiscal/etl_execucao_IGPE.R',
  'fiscal/etl_execucao_bdpe_IGPE.R',
  # 'etl_cozinhas.R', # nao ta puxando os dados
  'etl_PCP.R',
  'municipios/etl_indicadores_ufs.R', ## falta dados brutos
  'municipios/etl_indicadores_regioes_br.R' ## falta dados brutos
  
  #'fiscal/etl_transferencias_voluntarias.R' ## Roda no servidor
  
  
  ### CONFERIR ANTES DE RODAS ---------------
  # 'etl_carga_servidor.R'
  ## ----------------------------------------
  
  ### ROTINAS SEMANAIS ----------------------
  #, 'R/gerador_pdfs.R', # uma vez na semana
  #, 'mulungu/ETL_AUTO_MULUNGU.R' # semanal
  #'RPA/rpa_emendas.py ## Semanal 
  #'RPA/rpa_cadunico.py ## Semanal 
  ## ---------------------------------------
  
  ### ROTINAS MENSAIS ----------------------
  #'RPA/rpa_compesa.py ## Mensal
  ## ---------------------------------------
  
  ### ROTINAS BIMESTRAL --------------------
  #' fiscal/etl_predicoes_ufs_coleta.R',
  #' fiscal/etl_predicoes_ufs_modelos.R',
  #' fiscal/etl_prescricoes_ufs_governanca.R',
  #' fiscal/etl_rreo_ufs.R' #roda uma vez bimestralmente jan, mar, mai, jul, set, nov
  ## ---------------------------------------
  
)


# Executa cada script em ordem, com log local (logs/etl_AAAA-MM-DD.log)
# Por padrao, para no primeiro erro (como antes). Para seguir mesmo com falhas:
#   Sys.setenv(AURORA_ETL_CONTINUE_ON_ERROR = "true")
if (file.exists("R/logger.R")) source("R/logger.R") else if (file.exists("../R/logger.R")) source("../R/logger.R")
continuar_no_erro <- tolower(Sys.getenv("AURORA_ETL_CONTINUE_ON_ERROR", "false")) == "true"
falhas <- character(0)

log_capture_start()  # grava no log tudo o que os scripts imprimem (cat/print/message)
log_info("===== INICIO DA EXECUCAO ETL (", length(scripts), " scripts) =====")

tryCatch({
  for (script in scripts) {
    caminho <- file.path(pasta_scripts, script)
    ok <- log_step(script, source(caminho, local = TRUE), stop_on_error = !continuar_no_erro)
    if (!ok) falhas <- c(falhas, script)
  }
}, finally = log_capture_stop())

tictoc::toc()

if (length(falhas) == 0) {
  log_info("===== FIM: todos os scripts executados com sucesso =====")
  cat("✅ Todos os scripts foram executados com sucesso.\n")
} else {
  log_error("===== FIM com ", length(falhas), " falha(s): ", paste(falhas, collapse = ", "), " =====")
  cat("⚠️ Execucao concluida com falhas. Veja o log em logs/.\n")
}

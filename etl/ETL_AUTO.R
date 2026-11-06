# Carrega resolucao central de caminhos
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../R/paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

cat("🚀 Iniciando execução dos scripts...\n")

tictoc::tic('Tempo ETL total')

# Definir caminho da pasta dos scripts de forma dinamica
pasta_scripts <- if (dir.exists("etl")) "etl" else "."

# Lista os scripts que você quer executar
scripts <- c(
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
  'municipios/etl_ficha_municipal.R',
  'municipios/etl_indicadores_ufs.R', ## falta dados brutos
  'municipios/etl_indicadores_regioes_br.R' ## falta dados brutos
  
  
  ### CONFERIR ANTES DE RODAS ---------------
  # 'etl_carga_servidor.R'
  ## ---------------------------------------
  
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
  #' fiscal/etl_rreo_mun_pe.R' #roda uma vez bimestralmente jan, mar, mai, jul, set, nov
  #' fiscal/etl_rreo_ufs.R' #roda uma vez bimestralmente jan, mar, mai, jul, set, nov
  ## ---------------------------------------
  
)


# Executa cada script em ordem
for (script in scripts) {
  caminho <- file.path(pasta_scripts, script)
  cat(glue::glue("\n\n🔧 Executando {script}...\n\n"))
  source(caminho, local = TRUE)
}

tictoc::toc()

cat("✅ Todos os scripts foram executados com sucesso.\n")

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
  'fiscal/etl_convenios_receita.R',
  'etl_pac.R',
  #'municipios/etl_urbanizacao.R', 
  'municipios/etl_demografia_pe.R',
  'municipios/etl_vulnerabilidade_social.R',
  'etl_mvi_cvp_muni.R',
  'municipios/etl_caracter_mun.R',
  'fiscal/etl_emendas.R',
  'fiscal/etl_op_cred.R',
  'fiscal/etl_execucao_bdpe.R',
  'etl_cozinhas.R',
  'etl_PCP.R'
  
  #'etl_carga_servidor.R'
  #'
  #, 'R/gerador_pdfs.R', # uma vez na semana
  #, 'fiscal/etl_rreo_mun_pe.R' #roda uma vez bimestralmente jan, mar, mai, jul, set, nov
  #, 'fiscal/etl_rreo_ufs.R' #roda uma vez bimestralmente jan, mar, mai, jul, set, nov
  #, 'mulungu/ETL_AUTO_MULUNGU.R' # semanal
  )


# Executa cada script em ordem
for (script in scripts) {
  caminho <- file.path(pasta_scripts, script)
  cat(glue::glue("\n\n🔧 Executando {script}...\n\n"))
  source(caminho, local = TRUE)
}

tictoc::toc()

cat("✅ Todos os scripts foram executados com sucesso.\n")

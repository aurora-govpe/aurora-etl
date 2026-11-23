# ==============================================================================
# PIPELINE ETL DE EMPRESAS - PLATAFORMA AURORA (R ENTRYPOINT)
# ==============================================================================
# Processa as bases de empresas da SEPLAG (Ativas e Inativas), gerando os indicadores
# demográficos, setoriais, de longevidade, porte (MEI) e espacialização geográfica.
#
# Entradas:
#   - C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_ATIVAS.parquet
#   - C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_INATIVAS.parquet
#   - C:/dados/aurora/geojson/pernambuco_municipios.geojson
#
# Saídas:
#   - C:/dados/aurora/parquet/df_empresas_*.parquet
#   - C:/dados/aurora/geojson/empresas_municipios_pe.geojson
# ==============================================================================

# 1. Carrega caminhos padronizados do projeto
caminhos_paths <- c("R/paths.R", "paths.R", "../paths.R", "../../R/paths.R", "etl/paths.R")
for (p in caminhos_paths) {
  if (file.exists(p)) {
    source(p)
    break
  }
}

message(">>> [ETL EMPRESAS] Iniciando processamento unificado...")

# Caminho do motor em Python (alta performance para bases de ~2 milhões de registros)
candidatos_py <- c(
  file.path("etl", "empresas", "etl_empresas.py"),
  file.path("empresas", "etl_empresas.py"),
  file.path(getwd(), "etl_empresas.py"),
  file.path(getwd(), "empresas", "etl_empresas.py")
)
caminho_script_py <- candidatos_py[file.exists(candidatos_py)][1]

# Tenta executar o motor de alta performance em Python
python_bin <- Sys.which("python")
if (python_bin == "") python_bin <- "C:/Python314/python.exe"

if (file.exists(caminho_script_py) && file.exists(python_bin)) {
  message(">>> Executando motor de ETL via: ", python_bin, " ", caminho_script_py)
  status <- system2(python_bin, args = c(shQuote(caminho_script_py)))
  
  if (status == 0) {
    message(">>> [ETL EMPRESAS] Finalizado com sucesso via motor otimizado!")
  } else {
    warning(">>> [ETL EMPRESAS] Erro na execucao do script Python. Codigo de saida: ", status)
  }
} else {
  message(">>> [ETL EMPRESAS] Executando rotina R nativa...")
  suppressPackageStartupMessages({
    library(arrow)
    library(dplyr)
    library(lubridate)
    library(jsonlite)
  })
  
  path_ativas   <- file.path(DIR_RAW_AURORA, "empresas", "df_SEPLAG_EMPRESAS_ATIVAS.parquet")
  path_inativas <- file.path(DIR_RAW_AURORA, "empresas", "df_SEPLAG_EMPRESAS_INATIVAS.parquet")
  
  df_ativas   <- arrow::read_parquet(path_ativas)
  df_inativas <- arrow::read_parquet(path_inativas)
  
  message(">>> Ativas: ", nrow(df_ativas), " | Inativas: ", nrow(df_inativas))
}

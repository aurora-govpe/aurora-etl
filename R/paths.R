# ==============================================================================
# RESOLUÇÃO CENTRALIZADA DE CAMINHOS - PIPELINES DE DADOS
# ==============================================================================
# Este módulo padroniza o acesso aos dados em desenvolvimento local (Windows)
# e no servidor de produção (Linux), permitindo expansão multi-plataforma (Aurora, Argos, etc.).
# ==============================================================================

is_linux <- Sys.info()[["sysname"]] == "Linux"

# No Windows, garante o uso do backend OpenSSL para requisições HTTPS do libcurl
if (!is_linux) {
  Sys.setenv(CURL_SSL_BACKEND = "openssl")
}

# ------------------------------------------------------------------------------
# 1. PLATAFORMA AURORA
# ------------------------------------------------------------------------------
# Entrada de dados brutos (Raw / Bronze): planilhas, shapefiles, arquivos fontes
DIR_RAW_AURORA <- Sys.getenv(
  "AURORA_RAW_DIR",
  if (is_linux) "/srv/dados_brutos/aurora" else "C:/dados/dados brutos/aurora"
)

# Saída de dados tratados (Curated / Gold): parquet, rds, geojson lidos pelos apps
DIR_OUT_AURORA <- Sys.getenv(
  "AURORA_DATA_DIR",
  if (is_linux) "/srv/dados/aurora" else "C:/dados/aurora"
)

# ------------------------------------------------------------------------------
# 2. PLATAFORMA ARGOS (Previsão de Expansão)
# ------------------------------------------------------------------------------
DIR_RAW_ARGOS <- Sys.getenv(
  "ARGOS_RAW_DIR",
  if (is_linux) "/srv/dados_brutos/argos" else "C:/dados/dados brutos/argos"
)

DIR_OUT_ARGOS <- Sys.getenv(
  "ARGOS_DATA_DIR",
  if (is_linux) "/srv/dados/argos" else "C:/dados/argos"
)

# ------------------------------------------------------------------------------
# 3. ALIASES PARA COMPATIBILIDADE RETROATIVA E OUTRAS PLATAFORMAS
# ------------------------------------------------------------------------------
# Scripts que antes gravavam em pastas internas dos apps agora apontam para o diretório central
DIR_DADOS             <- DIR_OUT_AURORA
DIR_DADOS_AURORA_MUNI <- DIR_OUT_AURORA
DIR_MULUNGU           <- Sys.getenv("MULUNGU_DATA_DIR", if (is_linux) "/srv/dados/mulungu" else "C:/dados/mulungu")
DIR_RAW_MULUNGU       <- Sys.getenv("MULUNGU_RAW_DIR", if (is_linux) "/srv/dados_brutos/aurora/mulungu" else "C:/dados/dados brutos/aurora/mulungu")

# ------------------------------------------------------------------------------
# 4. FUNÇÕES UTILITÁRIAS DE ACESSO A ARQUIVOS
# ------------------------------------------------------------------------------
#' Retorna caminho completo de um arquivo ou subpasta na camada de dados brutos
raw_path <- function(..., plataforma = "aurora") {
  base <- switch(
    tolower(plataforma),
    "aurora"  = DIR_RAW_AURORA,
    "argos"   = DIR_RAW_ARGOS,
    "mulungu" = DIR_RAW_MULUNGU,
    stop("Plataforma desconhecida: ", plataforma)
  )
  file.path(base, ...)
}

#' Retorna caminho completo de um arquivo ou subpasta na camada de dados de saída
out_path <- function(..., plataforma = "aurora") {
  base <- switch(
    tolower(plataforma),
    "aurora"  = DIR_OUT_AURORA,
    "argos"   = DIR_OUT_ARGOS,
    "mulungu" = DIR_MULUNGU,
    stop("Plataforma desconhecida: ", plataforma)
  )
  file.path(base, ...)
}

# Garante que os diretórios essenciais existam
if (!dir.exists(DIR_OUT_AURORA)) dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_MULUNGU)) dir.create(DIR_MULUNGU, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_RAW_MULUNGU)) dir.create(DIR_RAW_MULUNGU, recursive = TRUE, showWarnings = FALSE)

library(aws.s3)

readRenviron("~/.Renviron")

OPTS <- list(
  base_url = "s3.pe.gov.br", 
  region = "", 
  url_style = "path",
  key = Sys.getenv("SDS_ACCESS_KEY"), 
  secret = Sys.getenv("SDS_SECRET_KEY"),
  parse_response = FALSE
  )

# inspecioar objetos ----
objetos <- get_bucket_df(
  bucket    = "sds",
  base_url  = "s3.pe.gov.br",
  region    = "",
  url_style = "path",
  max       = Inf
)

# Exibir os arquivos encontrados
print(objetos[, c("Key", "Size", "LastModified")])

# escrever objetos -----
meus_dados <- data.frame(
  ultimo_mes = "2026-10",
  registros  = 1500,
  status     = "atualizado"
)

s3write_using(
  x           = meus_dados,
  FUN         = arrow::write_parquet,
  object      = "mvi/teste.parquet",
  bucket      = "sds",
  opts        = OPTS
)

# ler objetos ----
meta <- s3read_using(
  arrow::read_parquet, 
  object = "mvi/teste.parquet",
  bucket = "sds", 
  opts = OPTS)

# Visualizar a estrutura do conteúdo lido
str(meta)

library(aws.s3)
OPTS <- list(base_url = "s3.pe.gov.br", region = "", url_style = "path",
             key = Sys.getenv("IAEMPE_S3_KEY"), secret = Sys.getenv("IAEMPE_S3_SECRET"))

manifest <- s3read_using(jsonlite::fromJSON,   object = "etl/manifest.json",            bucket = "iaempe", opts
                         = OPTS)
painel   <- s3read_using(arrow::read_parquet,  object = "etl/painel_iaempe.parquet",    bucket = "iaempe", opts
                         = OPTS)


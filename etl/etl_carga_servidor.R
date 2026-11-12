cat("🚀 Iniciando carga iniciativas dos scripts...\n")

# envio para servidor ----
library(ssh)

## setup ----

### ---------------------------------------------------------------------------
# AURORA GERAL 
### ---------------------------------------------------------------------------

# Caminhos Local (onde estão os arquivos no seu PC) + Caminho Remoto (onde eles vão ser salvos)
dir_local <- "data"
dir_remoto <- "/srv/dados/aurora"
dir_remoto_parquet <- "/srv/dados/aurora/parquet"

# Lista de arquivos
arquivos <- c(
  # Aurora Fiscal
  "json",
  "geojson"
)

## PARQUET -------------------------------------------------------------------
# Tem arquivo parquet gerado direto do servidor, então é necessário subir apenas os atualizados localmente para não dar problema de desatualização ou conflito de dados
## ---------------------------------------------------------------------------
arquivos_parquet <- c(
  
# Aurora Orcamento -------------------------------------------------------------
"parquet/dados_op_cred.parquet",
"parquet/execucao_bdpe.parquet",

"parquet/fato_dashboard_agg.parquet",
"parquet/fato_contratos.parquet",
"parquet/dim_ugc.parquet",
"parquet/dim_acao.parquet",
"parquet/dim_tema.parquet",
"parquet/dim_fornecedor.parquet",
"parquet/dim_fonte.parquet",
"parquet/dim_contrato.parquet",
"parquet/dim_despesa_gerencial.parquet",

# Aurora Iniciativas -----------------------------------------------------------

"parquet/df_iniciativas.parquet",
"parquet/df_cozinhas.parquet",
"parquet/dados_pac.parquet",
"parquet/dados_emendas.parquet",
"parquet/df_pcp.parquet",
"parquet/ficha_municipal.parquet",

## Aurora Municípios - UFs ------------------------------------------------

"parquet/df_indicadores_ufs.parquet"

)

# Prepara os caminhos completos locais
arquivos_completos <- file.path(dir_local, arquivos)
arquivos_completos_parquet <- file.path(dir_local, arquivos_parquet)

# --- 3. Envio (Upload) ---

message("Iniciando upload dos arquivos para o AURORA INICIATIVAS...")

session <- ssh::ssh_connect(
  host = "julia.barreto@10.238.113.49:7654",
  passwd = Sys.getenv("PASS_RESULTADOS")
)

# O scp_upload envia a lista toda de uma vez
## CARGA AURORA UNIFICADO ----
scp_upload(session, files = arquivos_completos, to = dir_remoto)
scp_upload(session, files = arquivos_completos_parquet, to = dir_remoto_parquet)

message("Upload 'aurora' finalizado com sucesso!")

# --- 4. Desconectar ---
ssh_disconnect(session)

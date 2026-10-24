cat("🚀 Iniciando carga iniciativas dos scripts...\n")

# envio para servidor ----
library(ssh)

## setup ----

### ---------------------------------------------------------------------------
# AURORA INCIATIVAS 
### ---------------------------------------------------------------------------

# Caminhos Local (onde estão os arquivos no seu PC) + Caminho Remoto (onde eles vão ser salvos)
dir_local <- "app_aurora_novo/data"
dir_remoto <- "/srv/dados/aurora"

# Lista de arquivos
arquivos <- c(
  "config.yml",
  # 'dados_emendas.rds', 
  # "geometrias_obras.rds",
  
  'dados_emendas_atual.rds',
  "shared_iniciativas.rds",
  'df_convenios_receita.rds',
  'dados_pac.rds',
  'df_pcp.rds',
  'df_reforma_no_lar.rds',
  "dados_emendas.parquet",
  'ficha_municipal.rds',
  'geojson',
  'df_cozinhas.rds',
  "iniciativas.geojson",
  "iniciativas.rds",
  "secretarias_unidades.rds",
  "shared_iniciativas.rds",
  'df_indicadores_empilhados.rds',
  'df_demografia_pe.rds',
  
  # aurora fiscal
  'orcamento_bdpe.parquet',
  'execucao_bdpe.parquet',
  'dados_op_cred.rds'
  
  #legado 
  # "escolas.rds",
  # "listas.R",
  # "tabela_iniciativa.rds",
  # 'tbl_base_2.rds'
)

# Prepara os caminhos completos locais
arquivos_completos <- file.path(dir_local, arquivos)

### ---------------------------------------------------------------------------
# AURORA FISCAL 
### ---------------------------------------------------------------------------

# Caminhos Local (onde estão os arquivos no seu PC) + Caminho Remoto (onde eles vão ser salvos)
dir_remoto_fiscal <- "/srv/dados/aurora_fiscal"

# Lista de arquivos
arquivos_fiscal <- c(
  "orcamento_bdpe.parquet",
  "execucao_bdpe.parquet",
  'dados_op_cred.rds'
)

# Prepara os caminhos completos locais
arquivos_completos_fiscal <- file.path(dir_local, arquivos_fiscal)

# --- 2. Conexão ---

message("Conectando ao servidor na porta 7654...")

# A sintaxe user@host:port funciona perfeitamente no ssh_connect
# O R vai usar sua chave SSH padrão (id_rsa) automaticamente.

# session <- ssh_connect("hugo.vasconcelos@10.238.113.49:7654")

session <- ssh::ssh_connect(
  host = "julia.barreto@10.238.113.49:7654",
  passwd = Sys.getenv("PASS_RESULTADOS")
)

# --- 3. Envio (Upload) ---

message("Iniciando upload dos arquivos...")

# O scp_upload envia a lista toda de uma vez

## CARGA AURORA INICIATIVAS ----
scp_upload(session, files = arquivos_completos, to = dir_remoto)
message("Upload 'aurora' finalizado com sucesso!")

## CARGA AURORA FISCAL ----
scp_upload(session, files = arquivos_completos_fiscal, to = dir_remoto_fiscal)
message("Upload 'aurora_fiscal' finalizado com sucesso!")

message("Todos os uploads foram concluídos com sucesso!")

# --- 4. Desconectar ---
ssh_disconnect(session)

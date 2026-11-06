# Carregamento centralizado de caminhos
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../../R/paths.R")) {
  source("../../R/paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

cat("🚀 Iniciando execução dos scripts...\n")
tictoc::tic('Tempo ETL')

# Definir caminho da pasta dos scripts
pasta_scripts <- "etl/mulungu/"

# Lista os scripts que você quer executar
scripts <- c(
  '1_notion.R',
  '2_diario.R',
  '3_plug.R',
  #'4_efisco.R',
  '5_enriquecimento.R'
)

# Executa cada script em ordem e interrompe se houver erro
for (script in scripts) {
  caminho <- file.path(pasta_scripts, script)
  
  if (!file.exists(caminho)) {
    stop(glue::glue("❌ Arquivo não encontrado: {caminho}"), call. = FALSE)
  }
  
  cat(glue::glue("\n\n🔧 Executando {script}...\n\n"))
  
  # Executa o script. Se houver erro, captura a mensagem e aborta imediatamente
  tryCatch(
    {
      source(caminho, local = TRUE)
      cat(glue::glue("\n ✅ {script} finalizado com sucesso.\n"))
    },
    error = function(e) {
      cat(glue::glue("\n❌ ERRO DETECTADO EM: {script}\n"))
      # stop() encerra a execução do R e exibe o traceback/mensagem
      stop(glue::glue("A execução foi interrompida devido a uma falha no script '{script}':\n--> {conditionMessage(e)}"), call. = FALSE)
    }
  )
}

tictoc::toc()
cat("\n🎉 Todos os scripts foram executados com sucesso!\n")

# envio para servidor ----
library(ssh)

## setup ----

# Caminho Local (onde estão os arquivos no seu PC)
dir_local <- DIR_MULUNGU

# Caminho Remoto (onde eles vão ser salvos)
dir_remoto <- if (exists("DIR_MULUNGU_REMOTO")) DIR_MULUNGU_REMOTO else "/srv/dados/mulungu"

# Lista de arquivos (RDS, QS, Parquet e GeoJSON)
arquivos <- c(
  "tabela_timeline.rds",
  "tabela_timeline.parquet",
  "tabela_pe_na_estrada.rds",
  "tabela_pe_na_estrada.parquet",
  "tabela_cards.rds",
  "projetos_notion.rds",
  "projetos_notion.parquet",
  "obras_notion_enriquecido.rds",
  "obras_notion_enriquecido.parquet",
  "licitacoes_notion.rds",
  "licitacoes_notion.parquet",
  "df_relatorios_diario.qs",
  "df_medicoes_plug.qs",
  "df_contratos_plug.qs",
  "contratos_plug_enriquecidos.rds",
  "contratos_plug_enriquecidos.parquet",
  "tabela_cards2.rds",
  "tabela_ordem_plug.rds",
  "tabela_ordem_plug.parquet",
  "rodovias_mulungu.rds",
  "rodovias_mulungu.geojson",
  "obras_notion_geometrias2.rds",
  "obras_notion_geometrias2.geojson",
  "obras_notion_geometrias.rds",
  "obras_notion_geometrias.geojson",
  "projetos_geometrias.rds",
  "projetos_geometrias.geojson",
  "config.yml"
)

# Prepara os caminhos completos locais (filtrando os que realmente existem)
arquivos_completos <- file.path(dir_local, arquivos)
arquivos_completos <- arquivos_completos[file.exists(arquivos_completos)]

# --- 2. Conexão ---

message("Conectando ao servidor na porta 7654...")

# A sintaxe user@host:port funciona perfeitamente no ssh_connect
# O R vai usar sua chave SSH padrão (id_rsa) automaticamente.
session <- ssh_connect("hugo.vasconcelos@10.238.113.49:7654")

# --- 3. Envio (Upload) ---

message("Iniciando upload dos arquivos...")

# O scp_upload envia a lista toda de uma vez
scp_upload(session, files = arquivos_completos, to = dir_remoto)

message("Upload concluído com sucesso!")

# --- 4. Desconectar ---
ssh_disconnect(session)
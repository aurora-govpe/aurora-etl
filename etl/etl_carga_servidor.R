cat("🚀 Iniciando carga iniciativas dos scripts...\n")

# envio para servidor ----
library(ssh)

## setup ----

### ---------------------------------------------------------------------------
# AURORA GERAL 
### ---------------------------------------------------------------------------

# Caminhos Local (onde estão os arquivos no seu PC) + Caminho Remoto (onde eles vão ser salvos)
dir_local <- if (dir.exists("C:/dados/aurora")) "C:/dados/aurora" else if (dir.exists("data")) "data" else "."
dir_remoto <- "/srv/dados/aurora"
dir_remoto_parquet <- "/srv/dados/aurora/parquet"
dir_remoto_geojson <- "/srv/dados/aurora/geojson"

# Lista de arquivos gerais
arquivos <- c(
  # Pastas Gerais
  "json",
  "geojson",

  # Aurora Sistema & Metadados
  "aurora_system.sqlite",

  # Aurora Municípios - Dados Fiscais, Sociais e Painéis
  "base_painel.json",
  "consorcios_pe.json",
  "estradas_pe.json",
  "df_cozinhas.json",
  "df_maes.json",
  "ppcof_co_financiamento.json",
  "df_ivcad_pe.json",
  "capacidade_pagamento_capag.json",

  # Aurora Municípios - Recursos Hídricos & APAC
  "apac_resumo_estadual.json",
  "apac_reservatorios.geojson",
  "apac_bacias_hidrograficas.geojson",

  # Aurora Municípios - COMPESA & Segurança Hídrica
  "compesa_plano_investimentos.json",
  "compesa_projetos_clean.json",
  "compesa_desembolso_rel.json",
  "compesa_municipios_rel.json",
  "mapa_compesa_todas_camadas.geojson",
  "adutoras_pernambuco.geojson"
)

## GEOJSON ESPECÍFICOS --------------------------------------------------------
arquivos_geojson <- c(
  "geojson/empresas_municipios_pe.geojson",
  "geojson/mapa_compesa_todas_camadas.geojson"
)

## PARQUET -------------------------------------------------------------------
# Tem arquivo parquet gerado direto do servidor, então é necessário subir apenas os atualizados localmente para não dar problema de desatualização ou conflito de dados
## ---------------------------------------------------------------------------
arquivos_parquet <- c(
  
  # Aurora Orcamento -----------------------------------------------------------
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

  # Aurora Iniciativas ---------------------------------------------------------
  "parquet/df_iniciativas.parquet",
  "parquet/dados_emendas.parquet",
  "parquet/df_cozinhas.parquet",
  "parquet/df_transporte.parquet",
  # "parquet/dados_pac.parquet",
  # "parquet/df_pcp.parquet",
  # "parquet/ficha_municipal.parquet",

  ## Aurora Municípios - UFs, Regiões & Matriz Territorial ---------------------
  "parquet/df_indicadores_ufs.parquet",
  "parquet/df_indicadores.parquet",
  "parquet/df_indicadores_regioes.parquet",
  "parquet/matriz_desenvolvimento_regional.parquet",
  "parquet/regic_pe_main.parquet",
  "parquet/arranjos_populacionais_pe.parquet",
  "parquet/regic_pe_fluxos.parquet",

  ## Aurora Municípios - Dinâmica das Empresas (SEPLAG) -----------------------
  "parquet/df_empresas_resumo_municipal.parquet",
  "parquet/df_empresas_saldo_mensal_muni.parquet",
  "parquet/df_empresas_saldo_mensal_pe.parquet",
  "parquet/df_empresas_saldo_mensal_porte.parquet",
  "parquet/df_empresas_faixa_etaria.parquet",
  "parquet/df_empresas_cnae_setor.parquet",
  "parquet/df_empresas_bairros_recife.parquet",

  ## Aurora Municípios - COMPESA & Infraestrutura Hídrica ----------------------
  "parquet/compesa_iniciativas_srhs.parquet",
  "parquet/compesa_mapa_linhas_adutoras.parquet",
  "parquet/compesa_mapa_municipios_camadas.parquet",
  "parquet/compesa_mapa_sistemas_pontos.parquet",
  "parquet/compesa_municipios_deficit_bndes.parquet",
  "parquet/compesa_necessidade_recursos.parquet",
  "parquet/compesa_portfolio_projetos.parquet"
)

# Prepara os caminhos completos locais (enviando apenas os que existem)
arquivos_completos <- file.path(dir_local, arquivos)
arquivos_completos <- arquivos_completos[file.exists(arquivos_completos)]

arquivos_completos_parquet <- file.path(dir_local, arquivos_parquet)
arquivos_completos_parquet <- arquivos_completos_parquet[file.exists(arquivos_completos_parquet)]

arquivos_completos_geojson <- file.path(dir_local, arquivos_geojson)
arquivos_completos_geojson <- arquivos_completos_geojson[file.exists(arquivos_completos_geojson)]

# --- 3. Envio (Upload) ---

message("Iniciando upload dos arquivos para o AURORA...")

# session <- ssh::ssh_connect(
#   host = "julia.barreto@10.238.113.49:7654",
#   passwd = Sys.getenv("PASS_RESULTADOS")
# )

host_str <- sprintf("%s@%s:%s", 
                   Sys.getenv("AURORA_SSH_USER"),
                   Sys.getenv("AURORA_SSH_HOST"), 
                   Sys.getenv("AURORA_SSH_PORT"))

session <- ssh::ssh_connect(host = host_str, passwd = Sys.getenv("PASS_RESULTADOS"))

# O scp_upload envia a lista toda de uma vez
## CARGA AURORA UNIFICADO ----
if (length(arquivos_completos) > 0) {
  scp_upload(session, files = arquivos_completos, to = dir_remoto)
}
if (length(arquivos_completos_parquet) > 0) {
  scp_upload(session, files = arquivos_completos_parquet, to = dir_remoto_parquet)
}
if (length(arquivos_completos_geojson) > 0) {
  scp_upload(session, files = arquivos_completos_geojson, to = dir_remoto_geojson)
}

message("Upload 'aurora' finalizado com sucesso!")

# --- 4. Desconectar ---
ssh_disconnect(session)

# versão jdbc ----
library(arrow)
library(BigDataPE)
library(DBI)
library(googlesheets4)
library(googledrive)
library(glue)
library(janitor)
library(readxl)
library(stringr) 
library(tidyverse)

# setup ----
## google ----
drive_auth(path = "data/r-sheets-hugo-0788cc358cb5.json")

id_arquivo_gastos <- "1tQrprso7WxV-g7ITUv2QQFWNil4ORKcWh5ZTNAq-M9w" 

#caminho_local_gastos <- "data/planilha_temporaria.xlsm"
caminho_local_gastos <- "data/planilha_temporaria.xlsx"

# 3. Fazer o download do arquivo
drive_download(
  file = as_id(id_arquivo_gastos),
  path = caminho_local_gastos,
  overwrite = TRUE
)

# utils ----
jargoes <- c(
  # 1. Padrões de modalidades e atas (Tolerante a falta de espaços)
  "(?i)(forma[çc][ãa]o\\s+de\\s+)?(ata\\s+de\\s+)?registro\\s+de\\s+pre[çc]os?(\\,)?(\\s*corporativo)?(\\,)?\\s*(para|por|com validade de)(\\s*o)?(\\s*a)?(\\s*um)?",
  "(?i)preg[ãa]o\\s+eletr[ôo]nico(\\s+para)?(\\s+registro\\s+de\\s+pre[çc]os?)?",
  "(?i)processo\\s+n[ºo]?\\s*\\d+\\/\\d+\\,?\\s*p\\.e\\.\\s*\\d+\\/\\d+\\-?[a-z]*\\.?",
  
  # 2. Padrões temporais BLINDADOS contra erros de digitação (ex: "12doze", "períodode")
  # Pega: ", por um períodode 12dozemeses", "pelo periodo de 12 (doze) meses"
  "(?i)(\\,\\s*)?(pelo\\s+|por\\s+um\\s+)?per[íi]odo\\s*(de)?\\s*(at[ée])?\\s*\\d+\\s*(\\(?[a-zA-Zãõç]+\\)?)?\\s*meses(\\,)?(\\s*para)?",
  
  # 3. Padrões de eventuais (NÃO apaga mais o "material de consumo")
  # Pega: "eventual e futuro fornecimento de", "eventual aquisição de", "eventual de"
  "(?i)(\\,\\s*)?(para\\s+)?eventual(mente)?(\\s+e\\s+futur[oa])?\\s+(fornecimento|aquisi[çc][ãa]o)(\\s+pelo\\s+regime\\s+de\\s+consigna[çc][ãa]o)?(\\s+de)?",
  "(?i)(\\,\\s*)?(para\\s+)?eventual\\s+de",
  
  # 4. Padrões de contratação e aquisição puros
  "(?i)contrata[çc][ãa]o\\s+de\\s+empresa(\\s+especializada)?\\s*(para|na)(\\s*a)?(\\s*presta[çc][ãa]o\\s+de)?(\\s*servi[çc]os)?(\\s*t[ée]cnicos)?(\\s*cont[íi]nuos)?(\\s*de)?",
  "(?i)aquisi[çc][ãa]o(\\s+emergencial)?(\\s+dispensa\\s+para)?(\\s+de)?",
  "(?i)aq\\.?\\s*de",
  "(?i)fornecimento(\\s+de)?",
  
  # 5. Padrões residuais de ARP
  "(?i)mediante\\s+a\\s+arp\\s+\\d+\\/\\d+.*",
  
  # 6. CORTE FINAL: O "Ceifador" de demandas de órgãos (Tudo daqui pro final some)
  # Pega: "para atender a demanda do hospital...", "visando atender as necessidades de pacientes..."
  "(?i)(\\,\\s*)?(visando\\s+|objetivando\\s+)?(para\\s+)?atende\\s*r\\s*(a|as|à|às|o|os)?\\s*(necessidades?|demandas?).*$"
)

# liquidação ----
## 2019 ----
liquidacao_2019_bdpe <- read_parquet('data/efisco/liquidacao_2019_bdpe.parquet') %>%
  mutate(
    ano  = 2019,
    gbplicit_cd = as.character(gbplicit_cd),
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd),
    gbpcroelic_nu_ano = as.character(gbpcroelic_nu_ano),
    gbpcroelic_sq = as.character(gbpcroelic_sq),
    execorc_in_temporal_fonte = as.character(execorc_in_temporal_fonte),
    gbpcontr_nu_ano = as.character(gbpcontr_nu_ano),
    gbpcontr_sq = as.character(gbpcontr_sq),
    ug_cd_coordenadora = as.character(ug_cd_coordenadora),
    ug_cd = as.character(ug_cd),
    gpoacao_cd = as.character(gpoacao_cd),
    fonterecur_cd = as.character(fonterecur_cd),
    tpdocidnt_cd = as.character(tpdocidnt_cd),
    ug_cd_credor = as.character(ug_cd_credor),
    mes_liquidacao = as.character(mes_liquidacao),
    grdesp_cd = as.character(grdesp_cd),
    elemdesp_id = as.character(elemdesp_id),
    codigo_item_gasto = as.character(codigo_item_gasto),
    item_valor_liquidado = as.character(item_valor_liquidado),
    valor_estornado = as.character(valor_estornado),
    valor_liquido_le = as.character(valor_liquido_le),
    ano_mes_documento_origem = as.character(ano_mes_documento_origem)
  )

liquidacao_2019_bdpe %>% glimpse()

## 2020 ----
liquidacao_2020_bdpe <- read_parquet('data/efisco/liquidacao_2020_bdpe.parquet') %>%
  mutate(
    ano  = 2020,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2020_bdpe %>% glimpse()

## 2021 ----
liquidacao_2021_bdpe <- read_parquet('data/efisco/liquidacao_2021_bdpe.parquet') %>%
  mutate(
    ano  = 2021,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2021_bdpe %>% glimpse()

## 2022 ----
liquidacao_2022_bdpe <- read_parquet('data/efisco/liquidacao_2022_bdpe.parquet') %>%
  mutate(
    ano  = 2022,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2022_bdpe %>% glimpse()

## 2023 ----
liquidacao_2023_bdpe <- read_parquet('data/efisco/liquidacao_2023_bdpe.parquet') %>%
  mutate(
    ano  = 2023,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2023_bdpe %>% glimpse()

## 2024 ----
liquidacao_2024_bdpe <- read_parquet('data/efisco/liquidacao_2024_bdpe.parquet') %>%
  mutate(
    ano  = 2024,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2024_bdpe %>% glimpse()

## 2025 ----
liquidacao_2025_bdpe <- read_parquet('data/efisco/liquidacao_2025_bdpe.parquet') %>%
  mutate(
    ano  = 2025,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2025_bdpe %>% glimpse()

## 2026 ----
liquidacao_2026_bdpe <- read_parquet('data/efisco/liquidacao_2026_bdpe.parquet') %>%
  mutate(
    ano  = 2026,
    chave_contrato = paste0(gbpcontr_sq, gbpcontr_nu_ano, str_remove_all(sg_ugc, "-"), ug_cd)
  )

liquidacao_2026_bdpe %>% glimpse()

compare_df_cols(
  liquidacao_2026_bdpe, 
  liquidacao_2025_bdpe, 
  liquidacao_2024_bdpe,
  liquidacao_2023_bdpe,
  liquidacao_2022_bdpe,
  liquidacao_2021_bdpe,
  liquidacao_2020_bdpe)

liquidacao_bdpe <- liquidacao_2026_bdpe %>%
  bind_rows(
    liquidacao_2025_bdpe, 
    liquidacao_2024_bdpe,
    liquidacao_2023_bdpe,
    liquidacao_2022_bdpe,
    liquidacao_2021_bdpe,
    liquidacao_2020_bdpe,
    liquidacao_2019_bdpe
  ) %>%
  select(
    ano, chave_contrato, gbplicit_cd, gbpcontr_nu_ano, gbpcontr_sq, gbpcontr_in_tac, sg_ugc, gpoacao_cd, gpoacaocad_nm, gposubacao_cd , gposubcad_nm, fichafinan_in_dea, fonterecur_cd, fonterecur_nm, nu_documento_credor, pessoa_nm_razao_social, gfuliqd_dt, mes_liquidacao, grdesp_cd, modalidade_aplicacao, elemdesp_id, elemdesp_id, valor_liquido_le, ano_mes_documento_origem, despgern_tx_det
  ) %>%
  mutate(
    valor_liquido_le_2 = as.numeric(str_replace(valor_liquido_le, ",", ".")),
    ano_mes_documento_origem = ym(ano_mes_documento_origem),
    ano_mes_liquidacao = ym(paste0(ano, mes_liquidacao)),
    despgern_tx_det = coalesce(despgern_tx_det, "NÃO INFORMADO")
  )

liquidacao_bdpe %>% glimpse()

## contratos ----
dados_contratos <- read.csv2(
  'data/efisco/contratos/EF_CONTRATO.csv',
  sep = '|',
  encoding = 'UTF-8'
) %>%
  clean_names() %>%
  mutate(
    contrato_limpo = str_remove(contrato, "^0+")
  ) %>%
  # Ordena para garantir que a linha principal do contrato venha primeiro
  arrange(contrato_limpo, numero_do_subcontrato_tac) %>%
  # Remove as duplicatas (subcontratos), mantendo todas as outras colunas
  distinct(contrato_limpo, .keep_all = TRUE) %>%
  mutate(
    # Remove as expressões regulares do dicionário
    objeto_curto = str_remove_all(objeto_da_licitacao, paste(jargoes, collapse = "|")),
    
    # Remove palavras soltas e preposições que sobraram no começo (ex: " de limpeza...")
    objeto_curto = str_remove(objeto_curto, "^(?i)(de |para |com )"),
    
    # Limpa pontuações órfãs no início da string (ex: ", limpeza..." ou ". limpeza")
    objeto_curto = str_squish(str_remove(objeto_curto, "^[\\,\\.\\;\\-\\s]+")),
    
    # Coloca a primeira letra em maiúscula para ficar bonito na UI
    objeto_curto = str_to_sentence(objeto_curto)
  )

dados_contratos %>% glimpse()

dados_contratos %>%
  write_parquet(
    'data/efisco/contratos/contratos_bdpe.parquet'
  )

# dados_contratos %>% select(objeto_da_licitacao, objeto_curto) %>% View()
# 
# dados_contratos %>% distinct(contrato, objeto_da_licitacao, objeto_curto) %>% write.csv2('dados_contratos.csv')

## temas de gasto ----
# temas_gasto <- read.csv2(
#   "data/efisco/orcamento_temas_subtemas.csv", 
#   sep = ";",
#   encoding = "UTF-8"
# ) %>%
#   clean_names() %>%
#   rename(
#     gasto_tema = tema_orc,
#     gasto_subtema = sub_tema_orc
#   ) %>%
#   mutate(
#     across(c(gasto_tema, gasto_subtema), str_squish)
#   )

temas_gasto <- read_excel(
  path = caminho_local_gastos,
  sheet = "DIC_TEMA_SUBTEMA"
) %>%
  clean_names() %>%
  rename(
    gasto_tema = tema_orc,
    gasto_subtema = sub_tema_orc
  ) %>%
  mutate(
    across(c(gasto_tema, gasto_subtema), str_squish)
  ) %>%
  distinct(chave_2, .keep_all = TRUE)

temas_gasto %>% glimpse()

# =========================================================================
# MIGRAÇÃO PARA O DUCKDB (PROCESSAMENTO MASSIVO & STAR SCHEMA)
# =========================================================================
cat("[ETL] Conectando ao DuckDB para processamento em massa...\n")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 1. Registra as bases em memória no DuckDB
# O arrow_table garante que o R não duplique a liquidação na memória (Zero-Copy)
duckdb::duckdb_register_arrow(con, "liquidacao_db", arrow::arrow_table(liquidacao_bdpe))
duckdb::duckdb_register(con, "dados_contratos_db", dados_contratos)
duckdb::duckdb_register(con, "temas_gasto_db", temas_gasto)

# 2. Consolidar e Reduzir usando o motor analítico do DuckDB
liquidacao_consolidada_db <- tbl(con, "liquidacao_db") %>%
  left_join(
    tbl(con, "dados_contratos_db") %>%
      mutate(objeto_da_licitacao = objeto_curto) %>%
      select(contrato_limpo, contrato, tipo_de_contrato_tac, ano_do_contrato, valor_do_contrato, objeto_da_licitacao, data_de_inclusao_do_contrato), 
    by = c("chave_contrato" = "contrato_limpo")
  ) %>%
  mutate(
    contrato = coalesce(contrato, "Sem contrato"),
    is_contrato = contrato != "Sem contrato",
    chave_2 = paste(
      gpoacao_cd, 
      if_else(gposubacao_cd == "0000", "0", as.character(gposubacao_cd)), 
      grdesp_cd, 
      sep = "-"
    )
  ) %>%
  select(-c(ano_mes_documento_origem, contrato, tipo_de_contrato_tac)) %>%
  
  # O agrupamento primário que antes travava o R agora roda distribuído em C++
  group_by(across(-valor_liquido_le_2)) %>%
  summarise(
    valor_liquido_le_2 = sum(valor_liquido_le_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  left_join(
    tbl(con, "temas_gasto_db") %>% select(chave_2, gasto_tema, gasto_subtema), 
    by = "chave_2"
  ) %>%
  mutate(
    gasto_tema = coalesce(gasto_tema, "Sem tema de gasto"),
    gasto_subtema = coalesce(gasto_subtema, "Sem tema de gasto"),
    sg_ugc = toupper(trimws(sg_ugc)),
    nu_documento_credor = stringr::str_replace_all(nu_documento_credor, "\\D", ""),
    ano_do_contrato = as.integer(ano_do_contrato)
  ) %>%
  select(
    -c(gbplicit_cd, gbpcontr_nu_ano, gbpcontr_sq, gbpcontr_in_tac, gfuliqd_dt, mes_liquidacao, valor_liquido_le, elemdesp_id, data_de_inclusao_do_contrato, chave_2)
  ) %>%
  compute(name = "liquidacao_consolidada_tmp")

## dimensões ----

# 1. Fornecedor: Garante apenas 1 ID por CNPJ (nu_documento_credor)
dim_fornecedor <- liquidacao_consolidada_db %>%
  distinct(nu_documento_credor, pessoa_nm_razao_social, ano) %>%
  collect() %>%
  arrange(nu_documento_credor, desc(ano)) %>%
  distinct(nu_documento_credor, .keep_all = TRUE) %>%
  select(nu_documento_credor, pessoa_nm_razao_social) %>%
  arrange(nu_documento_credor) %>%
  mutate(id_fornecedor = row_number())

# 2. Ação e Subação: Garante apenas 1 ID por Código Numérico da Ação+Subação
dim_acao <- liquidacao_consolidada_db %>%
  distinct(gpoacao_cd, gpoacaocad_nm, gposubacao_cd, gposubcad_nm, ano) %>%
  collect() %>%
  arrange(gpoacao_cd, gposubacao_cd, desc(ano)) %>%
  distinct(gpoacao_cd, gposubacao_cd, .keep_all = TRUE) %>%
  select(gpoacao_cd, gpoacaocad_nm, gposubacao_cd, gposubcad_nm) %>%
  arrange(gpoacao_cd, gposubacao_cd) %>%
  mutate(id_acao = row_number())

# 3. Contrato: Garante apenas 1 ID por Chave do Contrato
dim_contrato <- liquidacao_consolidada_db %>%
  distinct(chave_contrato, objeto_da_licitacao, ano) %>%
  collect() %>%
  arrange(chave_contrato, desc(ano)) %>%
  distinct(chave_contrato, .keep_all = TRUE) %>%
  select(chave_contrato, objeto_da_licitacao) %>%
  arrange(chave_contrato) %>%
  mutate(id_contrato = row_number())

# 4. UGC: Apenas Sigla (Não muda ao longo do tempo)
dim_ugc <- liquidacao_consolidada_db %>%
  distinct(sg_ugc) %>%
  collect() %>%
  arrange(sg_ugc) %>%
  mutate(id_ugc = row_number())

# 5. Tema: Garante 1 ID por combinação única
dim_tema <- liquidacao_consolidada_db %>%
  distinct(gasto_tema, gasto_subtema, ano) %>%
  collect() %>%
  arrange(gasto_tema, gasto_subtema, desc(ano)) %>%
  distinct(gasto_tema, gasto_subtema, .keep_all = TRUE) %>%
  select(gasto_tema, gasto_subtema) %>%
  arrange(gasto_tema) %>%
  mutate(id_tema = row_number())

# 6. Fonte: Apenas Nome da Fonte
dim_fonte <- liquidacao_consolidada_db %>%
  distinct(fonterecur_nm) %>%
  collect() %>%
  arrange(fonterecur_nm) %>%
  mutate(id_fonte = row_number())

# 7. Despesa Gerencial
dim_despesa_gerencial <- liquidacao_consolidada_db %>%
  distinct(despgern_tx_det) %>%
  collect() %>%
  arrange(despgern_tx_det) %>%
  mutate(id_despesa_gerencial = row_number())

# =========================================================================
# MONTAGEM DA TABELA FATO & DASHBOARD (JOIN SEGURO)
# =========================================================================

# Devolvemos as dimensões clusterizadas para o DuckDB
duckdb::duckdb_register(con, "dim_forn_db", dim_fornecedor)
duckdb::duckdb_register(con, "dim_acao_db", dim_acao)
duckdb::duckdb_register(con, "dim_contr_db", dim_contrato)
duckdb::duckdb_register(con, "dim_ugc_db", dim_ugc)
duckdb::duckdb_register(con, "dim_tema_db", dim_tema)
duckdb::duckdb_register(con, "dim_fonte_db", dim_fonte)
duckdb::duckdb_register(con, "dim_desp_gern_db", dim_despesa_gerencial)

# O pulo do gato: Cruzamos os IDs usando APENAS AS CHAVES FORTES (códigos).
# Isso garante que um contrato antigo com erro de digitação vai encontrar o ID perfeito!
fato_contratos_db <- liquidacao_consolidada_db %>%
  left_join(tbl(con, "dim_forn_db") %>% select(id_fornecedor, nu_documento_credor), by = "nu_documento_credor") %>%
  left_join(tbl(con, "dim_acao_db") %>% select(id_acao, gpoacao_cd, gposubacao_cd), by = c("gpoacao_cd", "gposubacao_cd")) %>%
  left_join(tbl(con, "dim_contr_db") %>% select(id_contrato, chave_contrato), by = "chave_contrato") %>%
  left_join(tbl(con, "dim_ugc_db") %>% select(id_ugc, sg_ugc), by = "sg_ugc") %>%
  left_join(tbl(con, "dim_tema_db") %>% select(id_tema, gasto_tema, gasto_subtema), by = c("gasto_tema", "gasto_subtema")) %>%
  left_join(tbl(con, "dim_fonte_db") %>% select(id_fonte, fonterecur_nm), by = "fonterecur_nm") %>%
  left_join(tbl(con, "dim_desp_gern_db") %>% select(id_despesa_gerencial, despgern_tx_det), by = "despgern_tx_det") %>%
  select(
    id_fornecedor, id_acao, id_contrato, id_ugc, id_tema, id_fonte, id_despesa_gerencial,
    ano, ano_mes_liquidacao, grdesp_cd, is_contrato,
    valor_liquido_le_2, valor_do_contrato, ano_do_contrato
  ) %>%
  compute(name = "fato_contratos_tmp")

fato_contratos <- fato_contratos_db %>% collect()

# Criamos a redução extrema (Fato Dashboard Agg) para os gráficos da API
fato_dashboard_agg <- fato_contratos_db %>%
  group_by(
    ano, ano_mes_liquidacao, id_ugc, id_acao, id_fornecedor, 
    id_tema, id_contrato, id_fonte, id_despesa_gerencial,
    grdesp_cd, is_contrato
  ) %>%
  summarise(
    valor_liquido_le_2 = sum(valor_liquido_le_2, na.rm = TRUE),
    valor_do_contrato = max(valor_do_contrato, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()

DBI::dbDisconnect(con, shutdown = TRUE)

# =========================================================================
# EXPORTAÇÃO DOS ARQUIVOS PARQUET
# =========================================================================

caminho_exportacao <- "app_aurora_orcamento/data/"

# Dimensões
write_parquet(dim_fornecedor, paste0(caminho_exportacao, "dim_fornecedor.parquet"))
write_parquet(dim_acao, paste0(caminho_exportacao, "dim_acao.parquet"))
write_parquet(dim_contrato, paste0(caminho_exportacao, "dim_contrato.parquet"))
write_parquet(dim_ugc, paste0(caminho_exportacao, "dim_ugc.parquet"))
write_parquet(dim_tema, paste0(caminho_exportacao, "dim_tema.parquet"))
write_parquet(dim_fonte, paste0(caminho_exportacao, "dim_fonte.parquet"))
write_parquet(dim_despesa_gerencial, paste0(caminho_exportacao, "dim_despesa_gerencial.parquet"))

# Fatos
write_parquet(fato_contratos, paste0(caminho_exportacao, "fato_contratos.parquet"))
write_parquet(fato_dashboard_agg, paste0(caminho_exportacao, "fato_dashboard_agg.parquet"))

# exportação para o google drive ----

# 1. Enviar a base "liquidacao_bdpe"
arquivo_temp_liq <- tempfile(fileext = ".parquet")
write_parquet(liquidacao_bdpe, arquivo_temp_liq)

# drive_upload(
#   media = arquivo_temp_liq,
#   path = as_id("1Tb3wOCp3X-MwfsKv_QNCanngfvcZozep"),
#   name = "liquidacao_bdpe.parquet",
#   overwrite = TRUE
# )

# 2. Enviar a base "dados_contratos"
arquivo_temp_contratos <- tempfile(fileext = ".parquet")
write_parquet(dados_contratos, arquivo_temp_contratos)

drive_upload(
  media = arquivo_temp_contratos,
  path = as_id("1JEi6bIxURhY38mNrw6cZV2zayi0zg7Ta"),
  name = "dados_contratos.parquet",
  overwrite = TRUE
)
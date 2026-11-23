# ==============================================================================
# ETL: Pesquisa Mensal de Serviços (PMS) - Índice de Atividades Turísticas (IATUR)
# Fonte: API SIDRA / IBGE (Tabela 8694)
# ==============================================================================

# 1. Carregamento de Pacotes ---------------------------------------------------
library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)

# 2. Resolução Dinâmica de Caminhos (Estratégia Modular) -----------------------
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
}

DIR_ETL   <- getwd()
DIR_DADOS <- if (exists("DIR_OUT_AURORA")) DIR_OUT_AURORA else DIR_ETL

# Carregamento hierárquico de utilitários e esquemas (caso existam no projeto)
utils_path <- if (file.exists("R/utils.R")) {
  "R/utils.R"
} else if (file.exists("../R/utils.R")) {
  "../R/utils.R"
} else {
  file.path(DIR_ETL, "R/utils.R")
}
if (file.exists(utils_path)) source(utils_path)

esquemas_path <- if (file.exists("R/esquemas_de_dados.R")) {
  "R/esquemas_de_dados.R"
} else if (file.exists("../R/esquemas_de_dados.R")) {
  "../R/esquemas_de_dados.R"
} else {
  file.path(DIR_ETL, "R/esquemas_de_dados.R")
}
if (file.exists(esquemas_path)) source(esquemas_path)

# 3. Extração via API SIDRA ----------------------------------------------------
url_sidra_uf <- "https://apisidra.ibge.gov.br/values/t/8694/n3/all/v/all/p/all/c11046/all"

resp <- request(url_sidra_uf) %>%
  req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") %>%
  req_timeout(90) %>%
  req_perform()

raw_data <- resp %>%
  resp_body_string() %>%
  fromJSON()

# 4. Transformação e Limpeza ---------------------------------------------------
nomes_colunas <- as.character(raw_data[1, ])
dados <- raw_data[-1, ]
colnames(dados) <- make.unique(nomes_colunas)

iatur_uf_completo <- dados %>%
  as_tibble() %>%
  transmute(
    uf_codigo   = `Unidade da Federação (Código)`,
    uf_nome     = `Unidade da Federação`,
    var_codigo  = `Variável (Código)`,
    variavel    = `Variável`,
    tipo_codigo = `Tipos de índice (Código)`,
    tipo_indice = `Tipos de índice`,
    mes_codigo  = `Mês (Código)`,
    data        = ym(`Mês (Código)`),
    valor       = suppressWarnings(as.numeric(Valor))
  ) %>%
  filter(!is.na(valor)) %>%
  arrange(uf_codigo, tipo_codigo, var_codigo, data) %>% 
  filter(variavel == "PMS - Número-índice com ajuste sazonal (2022=100)")

iatur_uf_completo %>% View()

# 5. Exportação para Diretório de Saída Padronizado (DIR_OUT) ------------------
if (!dir.exists(DIR_DADOS)) {
  dir.create(DIR_DADOS, recursive = TRUE)
}

arquivo_saida <- file.path(DIR_DADOS, "parquet/pms_iatur_uf.parquet")
write_parquet(iatur_uf_completo, arquivo_saida)

message("ETL concluído. Arquivo salvo em: ", arquivo_saida)

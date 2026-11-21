library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(janitor)
library(writexl)

# 1. Carregamento dos caminhos com estratégia modular do Aurora
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
}

# 2. Definição e criação do diretório de destino
dir_destino <- file.path(DIR_RAW_AURORA, "agricultura")
if (!dir.exists(dir_destino)) {
  dir.create(dir_destino, recursive = TRUE)
}

# 3. Requisição à API SIDRA para os anos de 2020 a 2025 (/p/2020-2025)
url_sidra <- "https://apisidra.ibge.gov.br/values/t/5457/n3/all/v/all/p/2020-2025/c782/0"

resposta <- request(url_sidra) |>
  req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
  req_perform()

dados_brutos <- fromJSON(resp_body_string(resposta))

# Atribui a primeira linha como cabeçalho e descarta-a
colnames(dados_brutos) <- dados_brutos[1, ]
dados_pam_uf <- as_tibble(dados_brutos[-1, ])

# 4. Tratamento, limpeza e estruturação dos dados
dados_pam_uf_final <- dados_pam_uf %>%
  clean_names() %>%
  select(
    unidade_da_federacao_codigo,
    unidade_da_federacao,
    ano_codigo,
    ano,
    variavel,
    valor
  ) %>%
  rename(
    cod_uf = unidade_da_federacao_codigo,
    nm_uf = unidade_da_federacao,
    nm_indicador = variavel,
    indicador = valor
  ) %>%
  filter(nm_indicador %in% c("Valor da produção", "Área colhida")) %>%
  pivot_wider(
    names_from = nm_indicador,
    values_from = indicador
  ) %>%
  clean_names() %>%
  mutate(
    valor_da_producao = as.numeric(valor_da_producao),
    area_colhida = as.numeric(area_colhida),
    # R$/ha (no IBGE o valor vem expresso em Mil Reais)
    valor_producao_hec_colhido = (valor_da_producao * 1000) / area_colhida
  ) %>%
  arrange(cod_uf, ano)

dados_pam_uf_final %>% View()

# 5. Exportação para arquivo Excel
caminho_arquivo <- file.path(dir_destino, "dados_pam_uf_2020_2025.xlsx")
write_xlsx(dados_pam_uf_final, caminho_arquivo)

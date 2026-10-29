# ---- Setup ----
library(tidyverse)
library(janitor)
library(arrow)
library(httr)

# ---- Configurações e Diretórios ----
url_aneel <- "https://dadosabertos.aneel.gov.br/dataset/5a583f3e-1646-4f67-bf0f-69db4203e89e/resource/fcf2906c-7c32-4b9b-a637-054e7a5234f4/download/tarifas-homologadas-distribuidoras-energia-eletrica.csv"
dir_brutos <- "C:/dados/dados brutos/aurora/energia"
dir_tratados <- "C:/dados/aurora"
caminho_raw <- file.path(dir_brutos, "tarifas_aneel_raw.csv")

## ---- Criação de Diretórios ----
dir.create(dir_brutos, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tratados, recursive = TRUE, showWarnings = FALSE)

# ---- Extração (Download Robusto) ----
options(timeout = 3600)
resposta <- RETRY(
  verb = "GET",
  url = url_aneel,
  write_disk(caminho_raw, overwrite = TRUE),
  times = 5,
  pause_base = 5,
  timeout(3600)
)

if (http_error(resposta)) stop("Falha ao baixar dados da ANEEL.")

# ---- Transformação ----
## ---- Leitura e Limpeza Inicial ----
df_comercial <- read_csv2(caminho_raw, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  clean_names() %>%
  filter(
    dsc_sub_grupo == "B3",
    dsc_classe == "Não se aplica", # Isola o B3 comercial/serviços de outras exceções
    str_detect(dsc_unidade_terciaria, "(?i)MWh")
  ) %>%
  mutate(
    ano_mes = format(dat_inicio_vigencia, "%Y-%m"),
    vlr_tarifa = vlr_tusd + vlr_te
  ) %>%
  drop_na(vlr_tarifa, ano_mes)

## ---- Série Histórica por UF ----
de_para_uf <- tibble(
  sig_agente = c("CELPE", "NEOENERGIA PE", "ENEL CE", "COELBA", "NEOENERGIA BA", "CEMIG-D", "ENEL SP", "LIGHT"),
  uf = c("PE", "PE", "CE", "BA", "BA", "MG", "SP", "RJ")
)

df_serie_uf <- df_comercial %>%
  inner_join(de_para_uf, by = "sig_agente") %>%
  group_by(ano_mes, uf) %>%
  summarise(
    tarifa_comercial_mwh = mean(vlr_tarifa, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(uf, ano_mes)

## ---- Série Histórica por Município (PE) ----
df_mun_pe <- tibble(
  cod_ibge = c("2611606", "2607901", "2602902"), # Expandir com base completa do IBGE
  nome_municipio = c("Recife", "Jaboatão dos Guararapes", "Cabo de Santo Agostinho")
)

df_serie_mun_pe <- df_serie_uf %>%
  filter(uf == "PE") %>%
  cross_join(df_mun_pe) %>%
  select(ano_mes, cod_ibge, nome_municipio, tarifa_comercial_mwh) %>%
  arrange(nome_municipio, ano_mes)

# ---- Carga (Exportação Parquet) ----
write_parquet(df_serie_uf, file.path(dir_tratados, "serie_energia_uf.parquet"))
write_parquet(df_serie_mun_pe, file.path(dir_tratados, "serie_energia_municipios_pe.parquet"))

## ---- Limpeza de Memória ----
# rm(df_comercial, df_serie_uf, df_serie_mun_pe, de_para_uf, df_mun_pe)
# gc()
library(arrow)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(stringr)

# ==============================================================================
# 1. PARÂMETROS E DE-PARA OFICIAL DO IBGE
# ==============================================================================
caminho_arquivo <- file.path("data", "seguranca", "df_seguranca_publica_forum_nacional.xlsx")

# De-para de UFs para padronização com o painel master do IBGE
tb_aux_uf <- tibble::tribble(
  ~sigla_uf, ~cod_ibge, ~nome_local,
  "RO", "11", "Rondônia",
  "AC", "12", "Acre",
  "AM", "13", "Amazonas",
  "RR", "14", "Roraima",
  "PA", "15", "Pará",
  "AP", "16", "Amapá",
  "TO", "17", "Tocantins",
  "MA", "21", "Maranhão",
  "PI", "22", "Piauí",
  "CE", "23", "Ceará",
  "RN", "24", "Rio Grande do Norte",
  "PB", "25", "Paraíba",
  "PE", "26", "Pernambuco",
  "AL", "27", "Alagoas",
  "SE", "28", "Sergipe",
  "BA", "29", "Bahia",
  "MG", "31", "Minas Gerais",
  "ES", "32", "Espírito Santo",
  "RJ", "33", "Rio de Janeiro",
  "SP", "35", "São Paulo",
  "PR", "41", "Paraná",
  "SC", "42", "Santa Catarina",
  "RS", "43", "Rio Grande do Sul",
  "MS", "50", "Mato Grosso do Sul",
  "MT", "51", "Mato Grosso",
  "GO", "52", "Goiás",
  "DF", "53", "Distrito Federal"
)

# ==============================================================================
# 2. FUNÇÃO PARA LER E LIMPAR CADA ABA DO EXCEL
# ==============================================================================
ler_e_limpar_aba <- function(aba, path) {
  read_excel(path, sheet = aba, col_types = "text") |>
    clean_names() |>
    mutate(
      ano = as.integer(ano),
      # Converte marcadores especiais de omissão ("-", "...") para NA e força numérico
      across(
        !any_of(c("sigla_uf", "uf", "capital", "conceito")),
        ~ as.numeric(na_if(na_if(str_trim(.x), "-"), "..."))
      )
    )
}

# ==============================================================================
# 3. LEITURA E CONSOLIDAÇÃO DAS ABAS ESTADUAIS (UF)
# ==============================================================================
todas_abas <- excel_sheets(caminho_arquivo)
abas_uf <- setdiff(todas_abas, c("mvi_capitais", "dicionario"))

cat("Carregando e integrando as abas estaduais do Fórum de Segurança...\n")
lista_dfs_uf <- map(abas_uf, ~ ler_e_limpar_aba(.x, caminho_arquivo))

# A correção mágica: incluir 'uf' no by previne as duplicações de colunas
df_seguranca_uf <- lista_dfs_uf |>
  reduce(full_join, by = c("sigla_uf", "uf", "ano")) |>
  # Padronização do layout oficial Aurora/IBGE
  left_join(tb_aux_uf, by = "sigla_uf") |>
  mutate(nivel_geo = "Unidade da Federação")

# ==============================================================================
# 4. ABA DE CAPITAIS (Integrada como colunas adicionais de contexto)
# ==============================================================================
cat("Processando dados de Capitais...\n")
df_capitais <- ler_e_limpar_aba("mvi_capitais", caminho_arquivo) |>
  rename(
    sigla_uf = uf,
    nome_capital = capital,
    n_ab_mvi_capital = n_ab_mvi,
    tx_mvi_capital = tx_mvi
  )

# ==============================================================================
# 5. PAINEL FINAL CONSOLIDADO NO PADRÃO MASTER
# ==============================================================================
painel_seguranca_master <- df_seguranca_uf |>
  # Para cruzar com capitais, 'uf' não existe na aba de capitais original (só sigla e capital)
  full_join(df_capitais, by = c("sigla_uf", "ano")) |>
  relocate(
    ano,
    nivel_geo,
    cod_ibge,
    sigla_uf,
    nome_local,
    uf,             # Mantido caso precise da grafia original da tabela
    nome_capital
  ) |>
  arrange(cod_ibge, ano)

cat("Processamento concluído com sucesso!\n")
cat("Dimensões finais:", nrow(painel_seguranca_master), "linhas e", ncol(painel_seguranca_master), "colunas.\n")

# Salvando no formato solicitado
painel_seguranca_master |> 
  write_parquet("data/seguranca/df_forum_seguranca.parquet")

cat("Arquivo df_forum_seguranca.parquet salvo com sucesso!\n")
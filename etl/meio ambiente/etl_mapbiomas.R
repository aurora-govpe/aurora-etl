# Instale os pacotes caso não tenha: 
# install.packages(c("readxl", "dplyr", "tidyr", "stringr"))

library(arrow)
library(dplyr)
library(fs)
library(readxl)
library(stringr)
library(tidyr)
library(writexl)

# 1. Definir caminho do arquivo
caminho_arquivo <- "data/meio ambiente/MAPBIOMAS_BRAZIL-COL.11-BIOME_STATE.xlsx"

df_cov   <- read_excel(caminho_arquivo, sheet = "COVERAGE_11")
df_trans <- read_excel(caminho_arquivo, sheet = "TRANSITION_11")

# 2. Períodos anuais consecutivos estritos (1985-1986 até 2024-2025)
anos_de  <- 1985:2024
anos_ate <- 1986:2025
colunas_anuais <- paste0("p", anos_de, "_", anos_ate)

# 3. Área total geográfica por Estado (ha)
area_estados <- df_cov %>%
  group_by(state) %>%
  summarise(area_total_ha = sum(y2025, na.rm = TRUE), .groups = "drop")

# 4. Desmatamento Bruto anual (Nativa -> Agropecuária / Não Vegetado)
desmatamento <- df_trans %>%
  filter(
    class_level_1_from %in% c("1. Forest", "2. Herbaceous and Shrubby Vegetation"),
    class_level_1_to   %in% c("3. Farming", "4. Non vegetated area")
  ) %>%
  group_by(state) %>%
  summarise(across(all_of(colunas_anuais), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(colunas_anuais), names_to = "periodo", values_to = "desmatamento_ha") %>%
  mutate(ano = as.integer(sub("p.*_", "", periodo))) %>%
  select(state, ano, desmatamento_ha)

# 5. Recuperação anual (Agropecuária / Não Vegetado -> Nativa)
recuperacao <- df_trans %>%
  filter(
    class_level_1_from %in% c("3. Farming", "4. Non vegetated area"),
    class_level_1_to   %in% c("1. Forest", "2. Herbaceous and Shrubby Vegetation")
  ) %>%
  group_by(state) %>%
  summarise(across(all_of(colunas_anuais), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(colunas_anuais), names_to = "periodo", values_to = "recuperacao_ha") %>%
  mutate(ano = as.integer(sub("p.*_", "", periodo))) %>%
  select(state, ano, recuperacao_ha)

# 6. Cálculo dos Indicadores Brutos
indicadores_base <- desmatamento %>%
  left_join(recuperacao,  by = c("state", "ano")) %>%
  left_join(area_estados, by = "state") %>%
  arrange(state, ano) %>%
  group_by(state) %>%
  mutate(
    desmatamento_pct     = (desmatamento_ha / area_total_ha) * 100,
    var_desmatamento_pct = ((desmatamento_ha - lag(desmatamento_ha)) / lag(desmatamento_ha)) * 100,
    recuperacao_pct      = (recuperacao_ha / area_total_ha) * 100
  ) %>%
  ungroup()

# 7. Normalização Min-Max (0-100) e Ranking Anual (Estilo CLP)
# Função Min-Max (maior é melhor)
min_max_padrao <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(100, length(x)))
  ((x - min(x, na.rm = TRUE)) / rng) * 100
}

# Função Min-Max Invertida (menor é melhor)
min_max_invertido <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(100, length(x)))
  ((max(x, na.rm = TRUE) - x) / rng) * 100
}

indicadores_clp <- indicadores_base %>%
  group_by(ano) %>%
  mutate(
    # --- 1. Recuperação (Maior % = melhor) ---
    Score_Recuperacao = round(min_max_padrao(recuperacao_pct), 2),
    Rank_Recuperacao  = min_rank(desc(recuperacao_pct)),
    
    # --- 2. Desmatamento (Menor % = melhor) ---
    Score_Desmatamento = round(min_max_invertido(desmatamento_pct), 2),
    Rank_Desmatamento  = min_rank(desmatamento_pct),
    
    # --- 3. Variação do Desmatamento (Menor % / Maior Queda = melhor) ---
    Score_Var_Desmatamento = if (all(is.na(var_desmatamento_pct))) NA_real_ else round(min_max_invertido(var_desmatamento_pct), 2),
    Rank_Var_Desmatamento  = if (all(is.na(var_desmatamento_pct))) NA_integer_ else min_rank(var_desmatamento_pct)
  ) %>%
  ungroup() %>%
  arrange(ano, Rank_Desmatamento)

# 8. Conferência de Pernambuco nos anos recentes
indicadores_clp %>%
  filter(state == "Pernambuco", ano %in% c(2023, 2024, 2025)) %>%
  select(
    Estado = state, Ano = ano,
    Rank_Desm = Rank_Desmatamento, Score_Desm = Score_Desmatamento,
    Rank_Var  = Rank_Var_Desmatamento, Score_Var = Score_Var_Desmatamento,
    Rank_Rec  = Rank_Recuperacao, Score_Rec = Score_Recuperacao
  ) %>%
  print()

dir_path <- "data/meio ambiente"
if (!dir_exists(dir_path)) {
  dir_create(dir_path)
}

# 2. Salvar em XLSX e Parquet
write_xlsx(indicadores_clp, path = file.path(dir_path, "indicadores_mapbiomas.xlsx"))
write_parquet(indicadores_clp, sink = file.path(dir_path, "indicadores_mapbiomas.parquet"))

cat("Arquivos salvos com sucesso na pasta:", dir_path, "\n")
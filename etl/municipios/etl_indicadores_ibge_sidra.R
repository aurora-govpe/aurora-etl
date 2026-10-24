library(arrow)
library(ibger)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(janitor)

# Níveis geográficos: Brasil (N1), Regiões (N2) e UFs (N3)
niveis <- list(list(N1 = "all"), list(N2 = "all"), list(N3 = "all"))

# ==============================================================================
# 1. PIB TOTAL (Agregado 5938, Var 37: Mil R$)
# ==============================================================================
pib_total <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(5938, variable = 37, periods = "all", localities = geo)
}) |>
  transmute(
    ano             = as.integer(period),
    nivel_geo       = locality_level,
    cod_ibge        = locality_id,
    nome_local      = locality_name,
    pib_total_reais = parse_ibge_value(value) * 1000
  )

# ==============================================================================
# 2. POPULAÇÃO ESTIMADA (Agregado 6579, Var 9324)
# ==============================================================================
pop_total <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(6579, variable = 9324, periods = "all", localities = geo)
}) |>
  transmute(
    ano             = as.integer(period),
    nivel_geo       = locality_level,
    cod_ibge        = locality_id,
    populacao_total = parse_ibge_value(value)
  )

# ==============================================================================
# 3. MERCADO DE TRABALHO - PNAD CONTÍNUA TRIMESTRAL
# ==============================================================================
ocupados_trim <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(4092, variable = 1641, periods = "all", localities = geo, classification = list("629" = "32387"))
}) |>
  transmute(
    periodo   = period,
    ano       = as.integer(substr(period, 1, 4)),
    nivel_geo = locality_level,
    cod_ibge  = locality_id,
    ocupados  = parse_ibge_value(value) * 1000
  )

pia_trim <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(4092, variable = 1641, periods = "all", localities = geo, classification = list("629" = "32385"))
}) |>
  transmute(
    periodo   = period,
    nivel_geo = locality_level,
    cod_ibge  = locality_id,
    pia       = parse_ibge_value(value) * 1000
  )

desocup_trim <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(4099, variable = 4099, periods = "all", localities = geo)
}) |>
  transmute(
    periodo          = period,
    nivel_geo        = locality_level,
    cod_ibge         = locality_id,
    taxa_desocupacao = parse_ibge_value(value)
  )

trabalho_anual <- ocupados_trim |>
  inner_join(pia_trim,     by = c("periodo", "nivel_geo", "cod_ibge")) |>
  inner_join(desocup_trim, by = c("periodo", "nivel_geo", "cod_ibge")) |>
  group_by(ano, nivel_geo, cod_ibge) |>
  summarise(
    trimestres_observados  = n(),
    pop_ocupada_media      = mean(ocupados, na.rm = TRUE),
    pia_media              = mean(pia, na.rm = TRUE),
    taxa_desocupacao_media = mean(taxa_desocupacao, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 4. TAXA DE ANALFABETISMO 15+ (Agregado 7113, Var 10267)
# ==============================================================================
analfabetismo <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(
    7113,
    variable       = 10267,
    periods        = "all",
    localities     = geo,
    classification = list("2" = "6794", "58" = "2795")
  )
}) |>
  transmute(
    ano                       = as.integer(period),
    nivel_geo                 = locality_level,
    cod_ibge                  = locality_id,
    taxa_analfabetismo_15mais = parse_ibge_value(value)
  )

# ==============================================================================
# 5. TAXAS DE ESCOLARIZAÇÃO POR FAIXA ETÁRIA (Agregado 7138, Var 10276)
# ==============================================================================
categorias_idade <- c("47813", "31615", "2792", "100052", "108866")

escolarizacao <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(
    7138,
    variable       = 10276,
    periods        = "all",
    localities     = geo,
    classification = list("2" = "6794", "58" = categorias_idade)
  )
}) |>
  mutate(
    ano   = as.integer(period),
    valor = parse_ibge_value(value),
    faixa = case_when(
      str_detect(classification_58, "^4 a 5")   ~ "taxa_esc_4_a_5_anos",
      str_detect(classification_58, "^6 a 14")  ~ "taxa_esc_6_a_14_anos",
      str_detect(classification_58, "^15 a 17") ~ "taxa_esc_15_a_17_anos",
      str_detect(classification_58, "^18 a 24") ~ "taxa_esc_18_a_24_anos",
      str_detect(classification_58, "^25")      ~ "taxa_esc_25_mais"
    )
  ) |>
  filter(!is.na(faixa)) |>
  select(ano, nivel_geo = locality_level, cod_ibge = locality_id, faixa, valor) |>
  pivot_wider(names_from = faixa, values_from = valor)

categorias_etapa <- c("47815", "47816", "47817", "47818", "47819")

freq_liquida <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(
    7141,
    variable       = 10282,
    periods        = "all",
    localities     = geo,
    classification = list("2" = "6794", "871" = categorias_etapa)
  )
}) |>
  mutate(
    ano   = as.integer(period),
    valor = parse_ibge_value(value),
    etapa = case_when(
      str_detect(classification_871, "iniciais")    ~ "tx_liq_fund_iniciais",
      str_detect(classification_871, "finais")      ~ "tx_liq_fund_finais",
      str_detect(classification_871, "fundamental") ~ "tx_liq_fundamental",
      str_detect(classification_871, "médio|medio") ~ "tx_liq_medio",
      str_detect(classification_871, "superior")    ~ "tx_liq_superior"
    )
  ) |>
  filter(!is.na(etapa)) |>
  select(ano, nivel_geo = locality_level, cod_ibge = locality_id, etapa, valor) |>
  pivot_wider(names_from = etapa, values_from = valor)

categorias_anos_estudo <- c("2795", "108866", "3302")

anos_estudo <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(
    7126,
    variable       = 3593,
    periods        = "all",
    localities     = geo,
    classification = list("2" = "6794", "58" = categorias_anos_estudo)
  )
}) |>
  mutate(
    ano   = as.integer(period),
    valor = parse_ibge_value(value),
    faixa = case_when(
      str_detect(classification_58, "^15 anos") ~ "anos_estudo_15_mais",
      str_detect(classification_58, "^25 anos") ~ "anos_estudo_25_mais",
      str_detect(classification_58, "^60 anos") ~ "anos_estudo_60_mais"
    )
  ) |>
  filter(!is.na(faixa)) |>
  select(ano, nivel_geo = locality_level, cod_ibge = locality_id, faixa, valor) |>
  pivot_wider(names_from = faixa, values_from = valor)

categorias_jovens <- c("47978", "47979", "47980", "47981")

jovens_estudo_ocup <- map_dfr(niveis, function(geo) {
  Sys.sleep(0.3)
  ibge_variables(
    7206,
    variable       = 10315,
    periods        = "all",
    localities     = geo,
    classification = list("2" = "6794", "876" = categorias_jovens)
  )
}) |>
  mutate(
    ano      = as.integer(period),
    valor    = parse_ibge_value(value),
    condicao = case_when(
      str_detect(classification_876, "^Ocupadas e frequentando")     ~ "jovens_ocup_estuda",
      str_detect(classification_876, "^Ocupadas e não")             ~ "jovens_ocup_nao_estuda",
      str_detect(classification_876, "^Não ocupadas e frequentando") ~ "jovens_nao_ocup_estuda",
      str_detect(classification_876, "^Não ocupadas e não")         ~ "jovens_nem_nem"
    )
  ) |>
  filter(!is.na(condicao)) |>
  select(ano, nivel_geo = locality_level, cod_ibge = locality_id, condicao, valor) |>
  pivot_wider(names_from = condicao, values_from = valor)

# ==============================================================================
# 6. CONSOLIDAÇÃO FINAL
# ==============================================================================
painel_consolidado <- pib_total |>
  full_join(pop_total,      by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(trabalho_anual, by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(analfabetismo,  by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(escolarizacao,  by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(freq_liquida,   by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(anos_estudo,    by = c("ano", "nivel_geo", "cod_ibge")) |>
  full_join(jovens_estudo_ocup, by = c("ano", "nivel_geo", "cod_ibge")) |>
  group_by(nivel_geo, cod_ibge) |>
  fill(nome_local, .direction = "updown") |>
  ungroup() |>
  mutate(
    pib_per_capita      = pib_total_reais / populacao_total,
    pib_por_pop_ocupada = pib_total_reais / pop_ocupada_media
  ) |>
  relocate(
    ano,
    nivel_geo,
    cod_ibge,
    nome_local,
    populacao_total,
    pib_total_reais,
    pib_per_capita,
    pop_ocupada_media,
    pib_por_pop_ocupada,
    pia_media,
    taxa_desocupacao_media,
    trimestres_observados,
    taxa_analfabetismo_15mais,
    starts_with("taxa_esc_"),
    starts_with("tx_liq_"),
    starts_with("anos_estudo_"),
    starts_with("jovens_")
  ) |>
  arrange(nivel_geo, cod_ibge, ano)

# salvar ----
painel_consolidado %>%
  write_parquet(
    'data/ibge/df_indicadores_ibge_ufs_br.parquet'
  )

painel_consolidado %>%
  writexl::write_xlsx(
    'data/ibge/df_indicadores_ibge_ufs_br.xlsx'
  )

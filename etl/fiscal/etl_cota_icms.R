library(tidyverse)
library(readxl)
library(writexl)
library(googledrive)
library(stringi)
library(showtext)
library(ggrepel)
library(gt)

# ==============================================================================
# 0. CONFIGURAÇÃO DE FONTE (Jost) E AUTENTICAÇÃO VIA CONTA DE SERVIÇO
# ==============================================================================
font_add_google(name = "Jost", family = "Jost")
showtext_auto()

drive_auth(path = "C:/dados/dados brutos/aurora/r-sheets-hugo-0788cc358cb5.json")

# ==============================================================================
# 1. DOWNLOAD DOS DOIS ARQUIVOS .XLSX DO GOOGLE DRIVE
# ==============================================================================
id_distribuicao  <- "1xBrg9uMlAOpyDmV-c60W2StZkn-rjSj0"
arquivo_dist_tmp <- tempfile(fileext = ".xlsx")
drive_download(as_id(id_distribuicao), path = arquivo_dist_tmp, overwrite = TRUE)

id_simulacao    <- "1FtU9G292fuZ6QojGoF40O0mqmrxbTMof"
arquivo_sim_tmp <- tempfile(fileext = ".xlsx")
drive_download(as_id(id_simulacao), path = arquivo_sim_tmp, overwrite = TRUE)

# ==============================================================================
# 2. TABELA DE-PARA: NOME MUNICÍPIO <-> COD_IBGE + POPULAÇÃO CENSO 2022
# ==============================================================================
pop_ref <- read_excel(arquivo_sim_tmp, sheet = "populacao", range = "A1:E186") %>%
  transmute(
    cod_ibge       = as.integer(`Codigo _Municipio_Completo_IBGE`),
    municipio      = Nome_Municipio,
    regiao_pe      = `Nome_região_de_Desenvolvimento_PE`,
    populacao_2022 = as.integer(Populacao_censo_IBGE_2022),
    chave_ibge     = str_to_upper(stri_trans_general(str_squish(Nome_Municipio), "Latin-ASCII")),
    chave_sefaz    = str_to_upper(stri_trans_general(str_squish(Municipio_sefaz), "Latin-ASCII"))
  )

de_para_ibge <- bind_rows(
  pop_ref %>% select(cod_ibge, municipio, regiao_pe, populacao_2022, chave = chave_ibge),
  pop_ref %>% select(cod_ibge, municipio, regiao_pe, populacao_2022, chave = chave_sefaz)
) %>%
  distinct(chave, .keep_all = TRUE)

# ==============================================================================
# 3. EXTRAÇÃO DA ARRECADAÇÃO REAL E CÁLCULO DA SAZONALIDADE (2020–2026)
# ==============================================================================
path_arrecadacao <- "C:/dados/dados brutos/aurora/efisco/ARRECADAÇÃO DE ICMS POR MUNICÍPIO E REGIÕES DE DESENVOLVIMENTO DE PE.xlsx"

df_arr_raw <- read_excel(path_arrecadacao, sheet = "ICMS P MUNICÍPIO", skip = 1)
names(df_arr_raw)[1] <- "local"

subtotais_rd <- c(
  "AGRESTE CENTRAL", "AGRESTE MERIDIONAL", "AGRESTE SETENTRIONAL",
  "MATA NORTE", "MATA SUL", "REGIAO METROPOLITANA DO RECIFE",
  "SERTAO CENTRAL", "SERTAO DE ITAPARICA", "SERTAO DO ARARIPE",
  "SERTAO DO MOXOTO", "SERTAO DO PAJEU", "SERTAO DO SAO FRANCISCO",
  "FORA DE REGIAO", "TOTAL DE ICMS ARRECADADO"
)

df_arrecadacao <- df_arr_raw %>%
  mutate(chave = str_to_upper(stri_trans_general(str_squish(local), "Latin-ASCII"))) %>%
  filter(
    !is.na(chave),
    chave != "",
    !str_starts(chave, "\\*"),
    !chave %in% subtotais_rd,
    chave != "NAO CADASTRADO *"
  ) %>%
  mutate(
    chave = recode(
      chave,
      "LAGOA DO ITAENGA" = "LAGOA DE ITAENGA",
      "IGUARACI"         = "IGUARACY"
    )
  ) %>%
  pivot_longer(cols = -c(local, chave), names_to = "col_data", values_to = "arrecadacao_icms") %>%
  mutate(
    serial_excel     = suppressWarnings(as.numeric(col_data)),
    data_iso         = str_extract(col_data, "^\\d{4}-\\d{2}-\\d{2}"),
    competencia      = coalesce(as.Date(serial_excel, origin = "1899-12-30"), as.Date(data_iso)),
    ano              = year(competencia),
    mes              = month(competencia),
    arrecadacao_icms = as.numeric(arrecadacao_icms)
  ) %>%
  select(-serial_excel, -data_iso) %>%
  filter(!is.na(competencia), ano >= 2020L, ano <= 2026L) %>%
  left_join(de_para_ibge, by = "chave")

stopifnot(sum(is.na(df_arrecadacao$cod_ibge)) == 0L)

# Fator sazonal histórico estadual (2020–2025: Ano Cheio / Jan-Out ≈ 1,2279)
fator_sazonal_estado <- df_arrecadacao %>%
  filter(ano >= 2020L, ano <= 2025L) %>%
  summarise(
    fator = sum(arrecadacao_icms, na.rm = TRUE) / sum(arrecadacao_icms[mes <= 10L], na.rm = TRUE)
  ) %>%
  pull(fator)

# Fator sazonal histórico por município (2020–2025)
fatores_sazonais_mun <- df_arrecadacao %>%
  filter(ano >= 2020L, ano <= 2025L) %>%
  group_by(cod_ibge) %>%
  summarise(
    soma_total   = sum(arrecadacao_icms, na.rm = TRUE),
    soma_jan_out = sum(arrecadacao_icms[mes <= 10L], na.rm = TRUE),
    fator_mun    = if_else(soma_jan_out > 0, soma_total / soma_jan_out, fator_sazonal_estado),
    .groups      = "drop"
  ) %>%
  select(cod_ibge, fator_mun)

df_arrecadacao_anual <- df_arrecadacao %>%
  group_by(cod_ibge, municipio, ano) %>%
  summarise(arrecadacao_bruta = sum(arrecadacao_icms, na.rm = TRUE), .groups = "drop") %>%
  left_join(fatores_sazonais_mun, by = "cod_ibge") %>%
  mutate(
    arrecadacao_real = if_else(ano == 2026L, arrecadacao_bruta * fator_mun, arrecadacao_bruta)
  ) %>%
  select(cod_ibge, municipio, ano, arrecadacao_real)

# ==============================================================================
# 4. EXTRAÇÃO DA DISTRIBUIÇÃO REAL E PROJEÇÃO DE FECHAMENTO DE 2026
# ==============================================================================
df_dist_raw <- read_excel(
  arquivo_dist_tmp,
  sheet     = "Cons. Anual",
  skip      = 3,
  col_names = FALSE
) %>%
  select(1:16)

names(df_dist_raw) <- c(
  "municipio_raw", "rd_raw",
  "2019", "2020", "v20", "2021", "v21", "2022", "v22",
  "2023", "v23", "2024", "v24", "2025", "v25", "2026"
)

df_distribuicao_real <- df_dist_raw %>%
  select(municipio_raw, `2020`, `2021`, `2022`, `2023`, `2024`, `2025`, `2026`) %>%
  filter(
    !is.na(municipio_raw),
    !str_detect(str_to_upper(str_squish(municipio_raw)), "^T O T A L$|^TOTAL$")
  ) %>%
  mutate(
    chave = str_to_upper(stri_trans_general(str_squish(municipio_raw), "Latin-ASCII")),
    chave = recode(
      chave,
      "AMARAGI"                 = "AMARAJI",
      "JABOATAO"                = "JABOATAO DOS GUARARAPES",
      "CABO"                    = "CABO DE SANTO AGOSTINHO",
      "IGUARACI"                = "IGUARACY",
      "STA CRUZ DA BAIXA VERDE" = "SANTA CRUZ DA BAIXA VERDE",
      "LAGOA DO ITAENGA"        = "LAGOA DE ITAENGA"
    )
  ) %>%
  left_join(de_para_ibge, by = "chave") %>%
  filter(!is.na(cod_ibge)) %>%
  select(cod_ibge, municipio, regiao_pe, populacao_2022, `2020`:`2026`) %>%
  pivot_longer(
    cols      = `2020`:`2026`,
    names_to  = "ano",
    values_to = "distribuicao_acumulada"
  ) %>%
  mutate(
    ano                    = as.integer(ano),
    distribuicao_acumulada = as.numeric(distribuicao_acumulada),
    distribuicao_real      = if_else(
      ano == 2026L,
      distribuicao_acumulada * fator_sazonal_estado,
      distribuicao_acumulada
    )
  ) %>%
  select(-distribuicao_acumulada)

stopifnot(nrow(df_distribuicao_real) == 1288L)

# ==============================================================================
# 5. EXTRAÇÃO DOS ÍNDICES SIMULADOS (ABA 'por ano real e sim') E SIMULAÇÃO
# ==============================================================================
df_ipm_raw <- read_excel(arquivo_sim_tmp, sheet = "por ano real e sim") %>%
  select(1:12)

names(df_ipm_raw)[1] <- "cod_ibge"
names(df_ipm_raw)[2] <- "municipio_planilha"

df_ipm_long <- df_ipm_raw %>%
  mutate(cod_ibge = as.integer(cod_ibge)) %>%
  filter(!is.na(cod_ibge)) %>%
  pivot_longer(
    cols      = -c(cod_ibge, municipio_planilha),
    names_to  = "variavel",
    values_to = "ipm_valor"
  ) %>%
  mutate(
    ano       = as.integer(str_extract(variavel, "\\d{4}")),
    tipo      = if_else(str_detect(variavel, "sim"), "ipm_simulado", "ipm_real"),
    ipm_valor = as.numeric(ipm_valor)
  ) %>%
  select(-variavel) %>%
  pivot_wider(names_from = tipo, values_from = ipm_valor) %>%
  mutate(
    ipm_simulado = coalesce(ipm_simulado, ipm_real)
  ) %>%
  group_by(ano) %>%
  mutate(
    ipm_simulado_norm = ipm_simulado / sum(ipm_simulado, na.rm = TRUE)
  ) %>%
  ungroup()

df_distribuicao_completa <- df_distribuicao_real %>%
  left_join(
    df_ipm_long %>% select(cod_ibge, ano, ipm_real, ipm_simulado, ipm_simulado_norm),
    by = c("cod_ibge", "ano")
  ) %>%
  group_by(ano) %>%
  mutate(
    bolo_distribuido_pe   = sum(distribuicao_real, na.rm = TRUE),
    distribuicao_simulada = if_else(
      ano >= 2024L,
      bolo_distribuido_pe * ipm_simulado_norm,
      distribuicao_real
    ),
    diferenca_repasse = distribuicao_real - distribuicao_simulada
  ) %>%
  ungroup()

# Consolidação Final Longa
df_anual <- df_distribuicao_completa %>%
  left_join(df_arrecadacao_anual, by = c("cod_ibge", "municipio", "ano")) %>%
  mutate(
    faixa_populacional = case_when(
      populacao_2022 <= 35000L  ~ "Até 35.000",
      populacao_2022 <= 100000L ~ "Até 100.000",
      populacao_2022 <= 400000L ~ "Até 400.000",
      TRUE                      ~ "Maior que 400.000"
    ),
    faixa_populacional = factor(
      faixa_populacional,
      levels = c("Até 35.000", "Até 100.000", "Até 400.000", "Maior que 400.000")
    )
  ) %>%
  arrange(municipio, ano)

# ==============================================================================
# 6. EXPORTAÇÃO EM .XLSX NA RAIZ DO PROJETO (2020–23 REAL | 2024–26 REAL E SIM)
# ==============================================================================
df_export_dist <- df_anual %>%
  select(cod_ibge, municipio, regiao_pe, populacao_2022, faixa_populacional, ano, distribuicao_real, distribuicao_simulada) %>%
  pivot_wider(
    names_from  = ano,
    values_from = c(distribuicao_real, distribuicao_simulada),
    names_glue  = "dist_{ano}_{if_else(.value == 'distribuicao_real', 'real', 'sim')}"
  ) %>%
  select(
    cod_ibge, municipio, regiao_pe, populacao_2022, faixa_populacional,
    dist_2020_real, dist_2021_real, dist_2022_real, dist_2023_real,
    dist_2024_real, dist_2024_sim,
    dist_2025_real, dist_2025_sim,
    dist_2026_real, dist_2026_sim
  )

df_export_cotas <- df_anual %>%
  select(cod_ibge, ano, ipm_real, ipm_simulado) %>%
  pivot_wider(
    names_from  = ano,
    values_from = c(ipm_real, ipm_simulado),
    names_glue  = "cota_{ano}_{if_else(.value == 'ipm_real', 'real', 'sim')}"
  ) %>%
  select(
    cod_ibge,
    cota_2020_real, cota_2021_real, cota_2022_real, cota_2023_real,
    cota_2024_real, cota_2024_sim,
    cota_2025_real, cota_2025_sim,
    cota_2026_real, cota_2026_sim
  )

df_completo_xlsx <- df_export_dist %>%
  left_join(df_export_cotas, by = "cod_ibge")

write_xlsx(df_completo_xlsx, path = "icms_pe_2020_2026_completo.xlsx")

# ==============================================================================
# 7. TABELA ESTÁTICA 1: VARIAÇÃO ANO A ANO (AxA %) DOS MUNICÍPIOS DE INTERESSE
# ==============================================================================
municipios_plot <- c(
  "Recife", "Jaboatão dos Guararapes", "Olinda",
  "Caruaru", "Petrolina", "Abreu e Lima",
  "Ipojuca", "Vitória de Santo Antão", "Goiana"
)

# 1. Bloco de Valores Absolutos (R$ Milhões)
tab_valores_abs <- df_anual %>%
  filter(municipio %in% municipios_plot) %>%
  mutate(valor_mi = distribuicao_real / 1e6) %>%
  select(municipio, ano, valor_mi) %>%
  pivot_wider(
    names_from   = ano,
    values_from  = valor_mi,
    names_prefix = "abs_"
  )

# 2. Bloco de Variação Percentual Ano a Ano (AxA %)
tab_var_axa <- df_anual %>%
  filter(municipio %in% municipios_plot) %>%
  arrange(municipio, ano) %>%
  group_by(municipio) %>%
  mutate(
    var_real_pct = (distribuicao_real / lag(distribuicao_real) - 1)
  ) %>%
  ungroup() %>%
  filter(ano >= 2021L) %>%
  select(municipio, ano, var_real_pct) %>%
  pivot_wider(
    names_from   = ano,
    values_from  = var_real_pct,
    names_prefix = "axa_"
  )

# 3. Junção lado a lado sem repetir Município
tab_consolidada <- tab_valores_abs %>%
  left_join(tab_var_axa, by = "municipio") %>%
  mutate(Município = factor(municipio, levels = municipios_plot)) %>%
  arrange(Município) %>%
  select(
    Município,
    abs_2020, abs_2021, abs_2022, abs_2023, abs_2024, abs_2025, abs_2026,
    axa_2021, axa_2022, axa_2023, axa_2024, axa_2025, axa_2026
  )

# 4. Tabela gt unificada lado a lado
gt_icms_lado_a_lado <- tab_consolidada %>%
  gt() %>%
  tab_header(
    title    = md("**Repasse do ICMS (R$ Milhões) e Variação Ano a Ano (AxA %)**"),
    subtitle = "Municípios selecionados de Pernambuco (2020–2026)"
  ) %>%
  tab_spanner(
    label   = "Distribuição Real (R$ Milhões)",
    columns = starts_with("abs_")
  ) %>%
  tab_spanner(
    label   = "Variação Ano a Ano (AxA %)",
    columns = starts_with("axa_")
  ) %>%
  cols_label(
    abs_2020 = "2020",
    abs_2021 = "2021",
    abs_2022 = "2022",
    abs_2023 = "2023",
    abs_2024 = "2024",
    abs_2025 = "2025",
    abs_2026 = "2026*",
    axa_2021 = "2021",
    axa_2022 = "2022",
    axa_2023 = "2023",
    axa_2024 = "2024*",
    axa_2025 = "2025",
    axa_2026 = "2026**"
  ) %>%
  fmt_number(
    columns  = starts_with("abs_"),
    decimals = 1,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  fmt_percent(
    columns  = starts_with("axa_"),
    decimals = 1,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  # Linha divisória vertical separando o bloco Absoluto do bloco AxA %
  tab_style(
    style     = cell_borders(sides = "left", color = "#bdbdbd", weight = px(2)),
    locations = list(
      cells_body(columns = axa_2021),
      cells_column_labels(columns = axa_2021)
    )
  ) %>%
  # --- Destaques Absolutos (vs. ano anterior) ---
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2021, rows = abs_2021 > abs_2020)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2021, rows = abs_2021 < abs_2020)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2022, rows = abs_2022 > abs_2021)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2022, rows = abs_2022 < abs_2021)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2023, rows = abs_2023 > abs_2022)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2023, rows = abs_2023 < abs_2022)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2024, rows = abs_2024 > abs_2023)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2024, rows = abs_2024 < abs_2023)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2025, rows = abs_2025 > abs_2024)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2025, rows = abs_2025 < abs_2024)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2026, rows = abs_2026 > abs_2025)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = abs_2026, rows = abs_2026 < abs_2025)
  ) %>%
  # --- Destaques AxA % (+ / -) ---
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2021, rows = axa_2021 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2021, rows = axa_2021 < 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2022, rows = axa_2022 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2022, rows = axa_2022 < 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2023, rows = axa_2023 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2023, rows = axa_2023 < 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2024, rows = axa_2024 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2024, rows = axa_2024 < 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2025, rows = axa_2025 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2025, rows = axa_2025 < 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2026, rows = axa_2026 > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = axa_2026, rows = axa_2026 < 0)
  ) %>%
  tab_source_note(
    source_note = html(
      "<span style='background-color:#4575b4; color:white; padding:2px 6px; border-radius:3px; font-weight:bold;'>Azul</span> Aumento (+) vs. ano anterior &nbsp;|&nbsp; <span style='background-color:#d73027; color:white; padding:2px 6px; border-radius:3px; font-weight:bold;'>Vermelho</span> Redução (-) vs. ano anterior"
    )
  ) %>%
  tab_source_note(
    source_note = "*Em 2024, foi ativado para alguns municípios o mecanismo de Compensação Anual da Lei 18.425 de 22 de dezembro de 2023. Por isso, a variação ficou ao redor de 12%."
  ) %>%
  tab_source_note(
    source_note = "**2026 (Jan–Out) projetado para 12 meses usando Sazonalidade Histórica (2020-2025)."
  ) %>%
  opt_table_font(font = google_font("Jost")) %>%
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold",
    table.font.size           = px(14),
    data_row.padding          = px(5)
  )

print(gt_icms_lado_a_lado)

# ==============================================================================
# 8. TABELA ESTÁTICA 2: GANHADORES E PERDEDORES POR FAIXA POPULACIONAL
# ==============================================================================
df_faixa_resumo <- df_anual %>%
  filter(ano >= 2024L) %>%
  group_by(faixa_populacional, ano) %>%
  summarise(
    total_mun  = n(),
    aumentaram = sum(diferenca_repasse > 0, na.rm = TRUE),
    diminuiram = sum(diferenca_repasse < 0, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    names_from  = ano,
    values_from = c(aumentaram, diminuiram)
  ) %>%
  select(
    `Faixa Populacional (Censo 2022)` = faixa_populacional,
    `Total`           = total_mun,
    `Aumentaram 2024` = aumentaram_2024,
    `Diminuíram 2024` = diminuiram_2024,
    `Aumentaram 2025` = aumentaram_2025,
    `Diminuíram 2025` = diminuiram_2025,
    `Aumentaram 2026` = aumentaram_2026,
    `Diminuíram 2026` = diminuiram_2026
  )

linha_total_faixa <- df_faixa_resumo %>%
  summarise(
    `Faixa Populacional (Censo 2022)` = "Total Pernambuco",
    across(-`Faixa Populacional (Censo 2022)`, sum)
  )

tab_faixa_pop <- bind_rows(
  df_faixa_resumo %>% mutate(`Faixa Populacional (Censo 2022)` = as.character(`Faixa Populacional (Censo 2022)`)),
  linha_total_faixa
)

gt_faixa_pop <- tab_faixa_pop %>%
  gt() %>%
  tab_header(
    title    = md("**Impacto da Mudança do IPM por Porte Populacional (Real vs. Simulado)**"),
    subtitle = "Quantidade de municípios que ganharam ou perderam repasse (2024–2026)"
  ) %>%
  tab_spanner(label = "2024", columns = c(`Aumentaram 2024`, `Diminuíram 2024`)) %>%
  tab_spanner(label = "2025", columns = c(`Aumentaram 2025`, `Diminuíram 2025`)) %>%
  tab_spanner(label = "2026*", columns = c(`Aumentaram 2026`, `Diminuíram 2026`)) %>%
  cols_label(
    `Aumentaram 2024` = "Aumentaram",
    `Diminuíram 2024` = "Diminuíram",
    `Aumentaram 2025` = "Aumentaram",
    `Diminuíram 2025` = "Diminuíram",
    `Aumentaram 2026` = "Aumentaram",
    `Diminuíram 2026` = "Diminuíram"
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Aumentaram 2024`, rows = `Aumentaram 2024` > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Aumentaram 2025`, rows = `Aumentaram 2025` > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#4575b4"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Aumentaram 2026`, rows = `Aumentaram 2026` > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Diminuíram 2024`, rows = `Diminuíram 2024` > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Diminuíram 2025`, rows = `Diminuíram 2025` > 0)
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#d73027"), cell_text(color = "white", weight = "bold")),
    locations = cells_body(columns = `Diminuíram 2026`, rows = `Diminuíram 2026` > 0)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = c(`Faixa Populacional (Censo 2022)`, `Total`), rows = `Faixa Populacional (Censo 2022)` == "Total Pernambuco")
  ) %>%
  tab_source_note(
    source_note = html(
      "<span style='background-color:#4575b4; color:white; padding:2px 6px; border-radius:3px; font-weight:bold;'>Azul</span> Real melhor que Simulado (Aumentaram) &nbsp;|&nbsp; <span style='background-color:#d73027; color:white; padding:2px 6px; border-radius:3px; font-weight:bold;'>Vermelho</span> Real pior que Simulado (Diminuíram)"
    )
  ) %>%
  tab_source_note(
    source_note = "*2026 (Jan–Out) projetado para 12 meses usando Sazonalidade Histórica (2020-2025)."
  ) %>%
  opt_table_font(font = google_font("Jost")) %>%
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold"
  )

print(gt_faixa_pop)

# ==============================================================================
# 9. GRÁFICO 1: FACETAS — DISTRIBUIÇÃO REAL (AZUL) VS. SIMULADA (VERMELHO)
# ==============================================================================
plot_paineis <- df_anual %>%
  filter(municipio %in% municipios_plot) %>%
  mutate(municipio = factor(municipio, levels = municipios_plot)) %>%
  select(
    municipio,
    ano,
    `Distribuição Real`                            = distribuicao_real,
    `Distribuição Simulada (Sem Mudança de Regra)` = distribuicao_simulada
  ) %>%
  pivot_longer(
    cols      = c(`Distribuição Real`, `Distribuição Simulada (Sem Mudança de Regra)`),
    names_to  = "serie",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = ano, y = valor / 1e6, color = serie, linetype = serie)) +
  geom_vline(xintercept = 2023.5, linetype = "dotted", color = "gray40") +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.4) +
  facet_wrap(~municipio, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 2020:2026,
    labels = c("2020", "2021", "2022", "2023", "2024", "2025", "2026*")
  ) +
  scale_color_manual(values = c(
    "Distribuição Real"                            = "#4575b4",
    "Distribuição Simulada (Sem Mudança de Regra)" = "#d73027"
  )) +
  scale_linetype_manual(values = c(
    "Distribuição Real"                            = "solid",
    "Distribuição Simulada (Sem Mudança de Regra)" = "dashed"
  )) +
  labs(
    title    = "Repasse do ICMS em Pernambuco (2020–2026)",
    subtitle = "Distribuição Real vs. Simulada sem mudança de indicadores (escalas individuais)",
    caption  = "*2026 (Jan–Out) projetado usando Sazonalidade Histórica (2020-2025).",
    x        = NULL,
    y        = "Repasse Anual (R$ Milhões)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_family = "Jost", base_size = 15) +
  theme(
    legend.position      = "bottom",
    legend.justification = "center",
    legend.text          = element_text(size = 14),
    plot.title           = element_text(face = "bold", size = 20),
    plot.subtitle        = element_text(size = 14, color = "gray35", margin = margin(b = 14)),
    plot.caption         = element_text(size = 11.5, color = "gray40", hjust = 0, margin = margin(t = 12)),
    strip.text           = element_text(face = "bold", size = 14),
    axis.text            = element_text(size = 12.5),
    axis.title.y         = element_text(size = 14, margin = margin(r = 10)),
    panel.grid.minor     = element_blank()
  )

print(plot_paineis)

# ==============================================================================
# 10. GRÁFICO 2: ESPAGUETE — DISTRIBUIÇÃO REAL + LINHA 'IPOJUCA SIMULADO'
# ==============================================================================
municipios_destaque <- c("Recife", "Ipojuca", "Caruaru", "Petrolina")

# Base com os 184 municípios reais
df_base_espaguete <- df_anual %>%
  transmute(
    serie_id = municipio,
    ano,
    valor    = distribuicao_real,
    destaque = if_else(municipio %in% municipios_destaque, municipio, "Outros")
  )

# Linha adicional exclusivamente para 'Ipojuca Simulado' (2020–2026)
df_ipojuca_sim <- df_anual %>%
  filter(municipio == "Ipojuca") %>%
  transmute(
    serie_id = "Ipojuca Simulado",
    ano,
    valor    = distribuicao_simulada,
    destaque = "Ipojuca Simulado"
  )

df_plot_espaguete <- bind_rows(df_base_espaguete, df_ipojuca_sim)

df_labels_destaque <- df_plot_espaguete %>%
  filter(ano == max(ano), destaque != "Outros")

plot_espaguete <- df_plot_espaguete %>%
  ggplot(aes(x = ano, y = valor / 1e6, group = serie_id)) +
  geom_line(
    data      = filter(df_plot_espaguete, destaque == "Outros"),
    color     = "gray82",
    linewidth = 0.5,
    alpha     = 0.7
  ) +
  geom_line(
    data      = filter(df_plot_espaguete, destaque %in% municipios_destaque),
    aes(color = destaque),
    linewidth = 1.35,
    linetype  = "solid"
  ) +
  geom_line(
    data      = filter(df_plot_espaguete, destaque == "Ipojuca Simulado"),
    aes(color = destaque),
    linewidth = 1.35,
    linetype  = "dashed"
  ) +
  scale_color_manual(values = c(
    "Recife"           = "#084594",
    "Ipojuca"          = "#2171b5",
    "Petrolina"        = "#4292c6",
    "Caruaru"          = "#008080",
    "Ipojuca Simulado" = "#d73027"
  )) +
  scale_x_continuous(
    breaks = 2020:2026,
    labels = c("2020", "2021", "2022", "2023", "2024", "2025", "2026*"),
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  geom_text_repel(
    data               = df_labels_destaque,
    aes(x = ano, y = valor / 1e6, label = serie_id, color = destaque),
    family             = "Jost",
    fontface           = "bold",
    size               = 5.2,
    direction          = "y",
    hjust              = 0,
    nudge_x            = 0.15,
    segment.color      = "gray60",
    segment.size       = 0.35,
    min.segment.length = 0.1,
    show.legend        = FALSE
  ) +
  labs(
    title    = "Evolução do Repasse de ICMS por Município (2020–2026)",
    subtitle = "Destaque para Recife, Ipojuca (Real e Simulado), Caruaru e Petrolina",
    caption  = "*2026 (Jan–Out) projetado usando Sazonalidade Histórica (2020-2025).",
    x        = NULL,
    y        = "Repasse Anual (R$ Milhões)"
  ) +
  theme_minimal(base_family = "Jost", base_size = 15) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 20),
    plot.subtitle    = element_text(size = 14, color = "gray35", margin = margin(b = 14)),
    plot.caption     = element_text(size = 11.5, color = "gray40", hjust = 0, margin = margin(t = 12)),
    axis.text        = element_text(size = 13),
    axis.title.y     = element_text(size = 14, margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )

print(plot_espaguete)
library(tidyverse)
library(readxl)
library(googledrive)
library(stringi)
library(showtext)
library(ggrepel)

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
# 2. TABELA DE-PARA: NOME MUNICÍPIO <-> COD_IBGE (184 MUNICÍPIOS)
# ==============================================================================
pop_ref <- read_excel(arquivo_sim_tmp, sheet = "populacao", range = "A1:E186")

de_para_ibge <- bind_rows(
  pop_ref %>%
    transmute(
      cod_ibge  = as.integer(`Codigo _Municipio_Completo_IBGE`),
      municipio = Nome_Municipio,
      chave     = str_to_upper(stri_trans_general(str_squish(Nome_Municipio), "Latin-ASCII"))
    ),
  pop_ref %>%
    transmute(
      cod_ibge  = as.integer(`Codigo _Municipio_Completo_IBGE`),
      municipio = Nome_Municipio,
      chave     = str_to_upper(stri_trans_general(str_squish(Municipio_sefaz), "Latin-ASCII"))
    )
) %>%
  distinct(chave, .keep_all = TRUE)

# ==============================================================================
# 3. EXTRAÇÃO DA ARRECADAÇÃO REAL (MANTIDA NA BASE CONSOLIDADA, 2020–2026)
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
    arrecadacao_icms = as.numeric(arrecadacao_icms)
  ) %>%
  select(-serial_excel, -data_iso) %>%
  filter(!is.na(competencia), ano >= 2020L, ano <= 2026L) %>%
  left_join(de_para_ibge, by = "chave")

stopifnot(sum(is.na(df_arrecadacao$cod_ibge)) == 0L)

df_arrecadacao_anual <- df_arrecadacao %>%
  group_by(cod_ibge, municipio, ano) %>%
  summarise(arrecadacao_real = sum(arrecadacao_icms, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 4. EXTRAÇÃO DA DISTRIBUIÇÃO REAL (ABA 'Cons. Anual' DO MONITORAMENTO)
# ==============================================================================
df_dist_raw <- read_excel(arquivo_dist_tmp, sheet = "Cons. Anual", skip = 2)
names(df_dist_raw)[1] <- "municipio_raw"

cols_anos_dist <- names(df_dist_raw) %>%
  keep(~ str_detect(.x, "^20(20|21|22|23|24|25|26)$"))

df_distribuicao_real <- df_dist_raw %>%
  select(municipio_raw, all_of(cols_anos_dist)) %>%
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
  select(cod_ibge, municipio, all_of(cols_anos_dist)) %>%
  pivot_longer(
    cols      = all_of(cols_anos_dist),
    names_to  = "ano",
    values_to = "distribuicao_real"
  ) %>%
  mutate(
    ano               = as.integer(ano),
    distribuicao_real = as.numeric(distribuicao_real)
  )

stopifnot(nrow(df_distribuicao_real) == 184L * length(cols_anos_dist))

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
    ipm_simulado = ipm_simulado / sum(ipm_simulado, na.rm = TRUE)
  ) %>%
  ungroup()

df_distribuicao_completa <- df_distribuicao_real %>%
  left_join(df_ipm_long %>% select(cod_ibge, ano, ipm_real, ipm_simulado), by = c("cod_ibge", "ano")) %>%
  group_by(ano) %>%
  mutate(
    bolo_distribuido_pe   = sum(distribuicao_real, na.rm = TRUE),
    distribuicao_simulada = if_else(
      ano >= 2024L,
      bolo_distribuido_pe * ipm_simulado,
      distribuicao_real
    ),
    diferenca_repasse = distribuicao_real - distribuicao_simulada
  ) %>%
  ungroup()

# Consolidação Final (mantendo arrecadacao_real na tabela para consultas)
df_anual <- df_distribuicao_completa %>%
  left_join(df_arrecadacao_anual, by = c("cod_ibge", "municipio", "ano")) %>%
  arrange(municipio, ano)

# ==============================================================================
# 6. GRÁFICO 1: FACETAS — DISTRIBUIÇÃO REAL VS. DISTRIBUIÇÃO SIMULADA
# ==============================================================================
municipios_plot <- c(
  "Recife", "Jaboatão dos Guararapes", "Olinda",
  "Caruaru", "Petrolina", "Abreu e Lima",
  "Ipojuca", "Vitória de Santo Antão", "Goiana"
)

plot_paineis <- df_anual %>%
  filter(municipio %in% municipios_plot) %>%
  mutate(municipio = factor(municipio, levels = municipios_plot)) %>%
  select(
    municipio,
    ano,
    `Distribuição Real`                         = distribuicao_real,
    `Distribuição Simulada (Sem Mudança de Regra)` = distribuicao_simulada
  ) %>%
  pivot_longer(
    cols      = c(`Distribuição Real`, `Distribuição Simulada (Sem Mudança de Regra)`),
    names_to  = "serie",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = ano, y = valor / 1e6, color = serie, linetype = serie)) +
  geom_vline(xintercept = 2023.5, linetype = "dotted", color = "gray40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~municipio, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = 2020:2026) +
  scale_color_manual(values = c(
    "Distribuição Real"                            = "#d73027",
    "Distribuição Simulada (Sem Mudança de Regra)" = "#4575b4"
  )) +
  scale_linetype_manual(values = c(
    "Distribuição Real"                            = "solid",
    "Distribuição Simulada (Sem Mudança de Regra)" = "dashed"
  )) +
  labs(
    title    = "Cota-Parte do ICMS em Pernambuco (2020–2026)",
    subtitle = "Distribuição Real vs. Distribuição Simulada sem mudança de indicadores (escalas individuais)",
    x        = NULL,
    y        = "Repasse Anual (R$ Milhões)",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_family = "Jost", base_size = 12) +
  theme(
    legend.position      = "bottom",
    legend.justification = "center",
    plot.title           = element_text(face = "bold", size = 15),
    plot.subtitle        = element_text(color = "gray35", margin = margin(b = 12)),
    strip.text           = element_text(face = "bold", size = 11),
    panel.grid.minor     = element_blank()
  )

print(plot_paineis)

# ==============================================================================
# 7. GRÁFICO 2: ESPAGUETE — DISTRIBUIÇÃO REAL (RECIFE, IPOJUCA, CARUARU, PETROLINA)
# ==============================================================================
municipios_destaque <- c("Recife", "Ipojuca", "Caruaru", "Petrolina")

df_plot_espaguete <- df_anual %>%
  mutate(
    destaque = if_else(municipio %in% municipios_destaque, municipio, "Outros")
  )

df_labels_destaque <- df_plot_espaguete %>%
  filter(ano == max(ano), destaque != "Outros")

plot_espaguete <- df_plot_espaguete %>%
  ggplot(aes(x = ano, y = distribuicao_real / 1e6, group = municipio)) +
  geom_line(
    data      = filter(df_plot_espaguete, destaque == "Outros"),
    color     = "gray82",
    linewidth = 0.5,
    alpha     = 0.7
  ) +
  geom_line(
    data      = filter(df_plot_espaguete, destaque != "Outros"),
    aes(color = destaque),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c(
    "Recife"    = "#d73027",
    "Ipojuca"   = "#1f78b4",
    "Caruaru"   = "#33a02c",
    "Petrolina" = "#ff7f00"
  )) +
  scale_x_continuous(breaks = 2020:2026, expand = expansion(mult = c(0.02, 0.15))) +
  geom_text_repel(
    data               = df_labels_destaque,
    aes(x = ano, y = distribuicao_real / 1e6, label = municipio, color = destaque),
    family             = "Jost",
    fontface           = "bold",
    size               = 4.2,
    direction          = "y",
    hjust              = 0,
    nudge_x            = 0.15,
    segment.color      = "gray60",
    segment.size       = 0.3,
    min.segment.length = 0.1,
    show.legend        = FALSE
  ) +
  labs(
    title    = "Evolução da Distribuição Real de ICMS por Município (2020–2026)",
    subtitle = "Destaque para Recife, Ipojuca, Caruaru e Petrolina frente aos demais municípios de Pernambuco",
    x        = NULL,
    y        = "Repasse Anual Real (R$ Milhões)"
  ) +
  theme_minimal(base_family = "Jost", base_size = 12) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray35", margin = margin(b = 12)),
    panel.grid.minor = element_blank()
  )

print(plot_espaguete)
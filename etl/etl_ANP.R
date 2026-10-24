# pacotes ----
library(dplyr)
library(lubridate)
library(stringr)

# extrair ----
## ceps ----
df_ceps <- readxl::read_excel(
  'data/anp/ceps.xlsx'
) %>%
  mutate(
    CEP_INICIAL_num = trimws(CEP_INICIAL),
    CEP_FINAL_num = trimws(CEP_FINAL)
  )

## municípios ----
anp_municipios <- readxl::read_excel(
  'data/anp/anp_municipios.xlsx'
)

## preços combustíveis automotivos ----
precos_combustiveis_automotivos <- data.table::fread(
  'data/anp/Preços semestrais - AUTOMOTIVOS_2024.02.csv'
) %>%
  janitor::clean_names() %>%
  mutate(
    cep_limpo = str_remove_all(cep, "-")
  )

precos_combustiveis_automotivos %>% glimpse()

precos_combustiveis_automotivos_2 <- precos_combustiveis_automotivos %>%
  left_join(
    df_ceps %>% select(
      CEP_INICIAL_num, 
      CEP_FINAL_num, 
      COD_IBGE,
      LOCALIDADE),
    by = join_by(
      cep_limpo >= CEP_INICIAL_num, 
      cep_limpo <= CEP_FINAL_num
    )
  ) %>%
  mutate(
    COD_IBGE = case_when(
      municipio == "RIO BRANCO" & estado_sigla == "AC" ~ "1200401",
      municipio == "MANAUS" & estado_sigla == "AM" ~ "1302603",
      municipio == "BARREIRAS" & estado_sigla == "BA" ~ "2903201",
      municipio == "IRECE" & estado_sigla == "BA" ~ "2914604",
      municipio == "JUAZEIRO" & estado_sigla == "BA" ~ "2918407",
      municipio == "SANTO ANTONIO DE JESUS" & estado_sigla == "BA" ~ "2928703",
      municipio == "BETIM" & estado_sigla == "MG" ~ "3106705",
      municipio == "CONTAGEM" & estado_sigla == "MG" ~ "3118601",
      municipio == "GUAXUPE" & estado_sigla == "MG" ~ "3128709",
      municipio == "POCOS DE CALDAS" & estado_sigla == "MG" ~ "3151800",
      municipio == "CUIABA" & estado_sigla == "MT" ~ "5103403",
      municipio == "RONDONOPOLIS" & estado_sigla == "MT" ~ "5107602",
      municipio == "BAYEUX" & estado_sigla == "PB" ~ "2501807",
      municipio == "CABEDELO" & estado_sigla == "PB" ~ "2503209",
      municipio == "CAMPINA GRANDE" & estado_sigla == "PB" ~ "2504009",
      municipio == "UMUARAMA" & estado_sigla == "PR" ~ "4128104",
      municipio == "SAQUAREMA" & estado_sigla == "RJ" ~ "3305505",
      municipio == "VOLTA REDONDA" & estado_sigla == "RJ" ~ "3306305",
      municipio == "MOSSORO" & estado_sigla == "RN" ~ "2408003",
      municipio == "ARIQUEMES" & estado_sigla == "RO" ~ "1100023",
      municipio == "CACOAL" & estado_sigla == "RO" ~ "1100049",
      municipio == "JI-PARANA" & estado_sigla == "RO" ~ "1100122",
      municipio == "PORTO VELHO" & estado_sigla == "RO" ~ "1100205",
      municipio == "GUAIBA" & estado_sigla == "RS" ~ "4309308",
      municipio == "SANTA ROSA" & estado_sigla == "RS" ~ "4317202",
      municipio == "SANTANA DO LIVRAMENTO" & estado_sigla == "RS" ~ "4317103",
      municipio == "SANTO ANGELO" & estado_sigla == "RS" ~ "4317509",
      municipio == "COTIA" & estado_sigla == "SP" ~ "3513009",
      municipio == "EMBU DAS ARTES" & estado_sigla == "SP" ~ "3515004",
      municipio == "HORTOLANDIA" & estado_sigla == "SP" ~ "3519071",
      municipio == "PAULINIA" & estado_sigla == "SP" ~ "3536505",
      municipio == "PIRACICABA" & estado_sigla == "SP" ~ "3538709",
      municipio == "SUZANO" & estado_sigla == "SP" ~ "3552502",
      TRUE ~ COD_IBGE 
    )
  )

precos_combustiveis_automotivos_3 <- precos_combustiveis_automotivos_2 %>%
  group_by(
    estado_sigla, 
    COD_IBGE,
    LOCALIDADE,
    municipio, 
    produto
  ) %>%
  summarise(
    valor_de_venda = mean(valor_de_venda, na.rm = TRUE),
    .groups = 'drop'
  )

precos_combustiveis_automotivos_3 %>% 
  qs::qsave(
    'data/anp/precos_combustiveis_automotivos_2024_02.qs'
  )

library(tidyverse)
library(geobr)
library(sf)

# --- 1. TABELA: Média por Estado e Produto ---
tabela_estado <- precos_combustiveis_automotivos_3 %>%
  group_by(estado_sigla, produto) %>%
  summarise(preco_medio = mean(valor_de_venda, na.rm = TRUE), .groups = 'drop')

print(tabela_estado)

# --- 2. GRÁFICO: Comparativo de Estados por Produto ---
# Ordenado do menor para o maior preço médio
ggplot(tabela_estado, aes(x = reorder(estado_sigla, preco_medio), y = preco_medio, fill = produto)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~produto, scales = "free_x") +
  theme_minimal() +
  labs(title = "Preço Médio por Estado e Tipo de Combustível",
       x = "Estado", y = "Preço Médio (R$)", fill = "Combustível")

# --- 3. MAPA: Facetado por Combustível ---
# Baixar mapa do Brasil
mapa_br <- read_state(year = 2020)

# Unir dados de preços com a geometria do mapa
mapa_dados <- mapa_br %>%
  left_join(tabela_estado, by = c("abbrev_state" = "estado_sigla"))

# Gerar o mapa com facetas (um mapa para cada combustível)
ggplot(mapa_dados) +
  geom_sf(aes(fill = preco_medio), color = "white", size = 0.1) +
  facet_wrap(~produto) +
  scale_fill_viridis_c(option = "viridis", name = "Preço (R$)") +
  theme_void() +
  labs(title = "Distribuição Geográfica de Preços por Combustível",
       subtitle = "Análise por Estado Brasileiro")

library(tidyverse)
library(sf)
library(patchwork) # Para juntar os mapas

# 1. Criar uma função para gerar o mapa individual de cada produto
gerar_mapa_combustivel <- function(nome_produto, dados) {
  dados %>%
    filter(produto == nome_produto) %>%
    ggplot() +
    geom_sf(aes(fill = preco_medio), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "viridis", name = paste("R$ (", nome_produto, ")")) +
    theme_void() +
    labs(subtitle = nome_produto) +
    theme(legend.position = "bottom", 
          legend.key.width = unit(0.8, "cm"))
}

# 2. Listar os produtos únicos
produtos <- unique(mapa_dados$produto)

# 3. Gerar a lista de mapas usando o purrr::map
lista_de_mapas <- map(produtos, ~gerar_mapa_combustivel(.x, mapa_dados))

# 4. Juntar tudo com o patchwork
# O plot_layout define quantas colunas você quer (ex: 2 colunas)
mapa_final <- wrap_plots(lista_de_mapas) + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Distribuição Geográfica de Preços por Combustível",
    subtitle = "Escalas independentes para cada tipo de produto"
  )

print(mapa_final)

library(tidyverse)
library(gt)

# 1. Agrupar e calcular a média
tabela_ordenada <- precos_combustiveis_automotivos_3 %>%
  group_by(estado_sigla, produto) %>%
  summarise(preco_medio = mean(valor_de_venda, na.rm = TRUE), .groups = 'drop') %>%
  # 2. Transformar produtos em colunas para facilitar a ordenação
  pivot_wider(names_from = produto, values_from = preco_medio) %>%
  # 3. Ordenar pelo maior valor de GASOLINA
  arrange(desc(GASOLINA))

# 4. Exibir com formatação pt-BR
tabela_ordenada %>%
  gt() %>%
  tab_header(
    title = "Ranking de Preços: Foco em Gasolina",
    subtitle = "Estados ordenados do maior para o menor preço médio de Gasolina"
  ) %>%
  fmt_currency(
    columns = -estado_sigla, # Formata todas as colunas exceto a UF
    currency = "BRL",
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  cols_label(estado_sigla = "UF")

  library(ggplot2)
  library(scales)
  
  ggplot(tabela_dados, aes(x = reorder(estado_sigla, preco_medio), y = preco_medio, fill = preco_medio)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~produto, scales = "free_x") + # Escalas independentes para o eixo X
    coord_flip() + # Invertido para facilitar a leitura das UFs
    scale_fill_viridis_c(option = "viridis") +
    scale_y_continuous(labels = label_number(accuracy = 0.01, decimal.mark = ",", big.mark = ".")) +
    theme_minimal() +
    labs(
      title = "Ranking de Preços Médios por Estado",
      x = "Unidade Federativa",
      y = "Preço Médio (R$)"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      # Removendo a grade vertical (eixo Y original) para valorizar a leitura dos níveis
      panel.grid.major.y = element_blank(), 
      panel.grid.major.x = element_line(color = "grey90"),
      strip.text = element_text(face = "bold")
    )
  
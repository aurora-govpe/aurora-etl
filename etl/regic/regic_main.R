# ==============================================================================
# PAINEL DE DESENVOLVIMENTO REGIONAL - INTELIGÊNCIA LOGÍSTICA E TERRITORIAL
# Baseado na metodologia REGIC 2018 (IBGE)
# ==============================================================================

# ---- 1. Carregar Pacotes ----
library(tidyverse)
library(googledrive)
library(readxl)
library(tidygraph)
library(janitor)

# ---- 2. Autenticação e Configuração ----
drive_auth(path = "data/r-sheets-hugo-0788cc358cb5.json")

pasta_destino <- "data/ibge/regic"
dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)

df_indicadores_municipais <- arrow::read_parquet('data/indicadores/df_indicadores.parquet')

id_pasta <- "1frsM_iKvj-d01VWJHsQq3Hde7gaKyvJu"
arquivos_alvo <- c(
  "REGIC2018_Cidades_v2.xlsx", 
  "REGIC2018_Ligacoes_entre_Cidades.xlsx", 
  "REGIC2018_Rotas_Brasil.xlsx"
)

# ---- 3. Download Iterativo ----
arquivos_drive <- drive_ls(as_id(id_pasta))

walk(arquivos_alvo, ~{
  arquivo_info <- arquivos_drive %>% filter(name == .x)
  if(nrow(arquivo_info) > 0) {
    caminho_local <- file.path(pasta_destino, .x)
    if(!file.exists(caminho_local)) {
      drive_download(as_id(arquivo_info$id[1]), path = caminho_local, overwrite = TRUE)
    }
  }
})

# ---- 4. Ingestão e Tratamento ----

# ---- 4.1. Vértices (PIB Atualizado e Centralidades) ----
df_pib_mun <- df_indicadores_municipais %>%
  filter(sigla_indicador == "pib_1000") %>%
  filter(ano == max(ano, na.rm = TRUE)) %>%
  select(cod_ibge = cod_municipio, pib_novo = indicador) %>%
  mutate(
    cod_ibge = as.character(cod_ibge),
    pib_novo = as.numeric(pib_novo)
  )

df_arranjos_pe <- tibble::tribble(
  ~cod_ibge, ~nome_mun, ~nome_arranjo, ~cod_arranjo,
  "2600054", "Abreu e Lima", "Arranjo Populacional do Recife/PE", "2611606",
  "2601052", "Araçoiaba", "Arranjo Populacional do Recife/PE", "2611606",
  "2601102", "Araripina", "Arranjo Populacional de Araripina/PE", "2601102",
  "2602902", "Cabo de Santo Agostinho", "Arranjo Populacional do Recife/PE", "2611606",
  "2603009", "Caetés", "Arranjo Populacional de Caetés - Capoeiras/PE", "2603009",
  "2603454", "Camaragibe", "Arranjo Populacional do Recife/PE", "2611606",
  "2603504", "Camutanga", "Arranjo Populacional de Itambé/PE - Pedras de Fogo/PB", "2607653",
  "2603801", "Capoeiras", "Arranjo Populacional de Caetés - Capoeiras/PE", "2603009",
  "2606804", "Igarassu", "Arranjo Populacional do Recife/PE", "2611606",
  "2607208", "Ipojuca", "Arranjo Populacional do Recife/PE", "2611606",
  "2607604", "Ilha de Itamaracá", "Arranjo Populacional do Recife/PE", "2611606",
  "2607653", "Itambé", "Arranjo Populacional de Itambé/PE - Pedras de Fogo/PB", "2607653",
  "2607752", "Itapissuma", "Arranjo Populacional do Recife/PE", "2611606",
  "2607901", "Jaboatão dos Guararapes", "Arranjo Populacional do Recife/PE", "2611606",
  "2609402", "Moreno", "Arranjo Populacional do Recife/PE", "2611606",
  "2609600", "Olinda", "Arranjo Populacional do Recife/PE", "2611606",
  "2610608", "Paudalho", "Arranjo Populacional do Recife/PE", "2611606",
  "2610707", "Paulista", "Arranjo Populacional do Recife/PE", "2611606",
  "2611101", "Petrolina", "Arranjo Populacional de Petrolina/PE - Juazeiro/BA", "2611101",
  "2611606", "Recife", "Arranjo Populacional do Recife/PE", "2611606",
  "2613305", "São José da Coroa Grande", "Arranjo Populacional de Maragogi/AL - São José da Coroa Grande/PE", "2704705",
  "2613701", "São Lourenço da Mata", "Arranjo Populacional do Recife/PE", "2611606"
)

write.csv2(df_arranjos_pe, "data/ibge/regic/arranjos_populacionais_pe.csv", row.names = FALSE)

df_arranjos_pe %>%
  arrow::write_parquet(
    "app_aurora_municipios/data/parquet/arranjos_populacionais_pe.parquet"
    )

df_pib_agregado <- df_pib_mun %>%
  left_join(df_arranjos_pe, by = "cod_ibge") %>%
  mutate(cod_cidade_regic = coalesce(cod_arranjo, cod_ibge)) %>%
  group_by(cod_cidade_regic) %>%
  summarise(pib_final_cidade = sum(pib_novo, na.rm = TRUE), .groups = "drop")

df_nos_brutos <- read_excel(file.path(pasta_destino, "REGIC2018_Cidades_v2.xlsx"))

df_nos <- df_nos_brutos %>%
  mutate(
    name = as.character(COD_CIDADE),
    nome_cidade = NOME_CIDADE,
    uf = as.character(COD_UF),
    hierarquia = as.character(VAR10), 
    cgt = as.numeric(VAR19),          
    ia = as.numeric(VAR56)            
  ) %>%
  left_join(df_pib_agregado, by = c("name" = "cod_cidade_regic")) %>%
  rename(pib = pib_final_cidade) %>%
  select(name, nome_cidade, uf, pib, hierarquia, cgt, ia) %>%
  drop_na(name)

## ---- 4.2. Matriz de Atrito Espacial ----
df_rotas_brutas <- read_excel(file.path(pasta_destino, "REGIC2018_Rotas_Brasil.xlsx"))

df_rotas <- df_rotas_brutas %>%
  clean_names() %>%
  mutate(
    from = as.character(cod_o),
    to   = as.character(cod_d),
    tempo_min = as.numeric(minutos),
    dist_km   = as.numeric(km)
  ) %>%
  select(from, to, tempo_min, dist_km) %>%
  drop_na(tempo_min, dist_km)

## ---- 4.3. Arestas (Redes Temáticas) ----
df_arestas_brutas <- read_excel(file.path(pasta_destino, "REGIC2018_Ligacoes_entre_Cidades.xlsx"))

df_arestas <- df_arestas_brutas %>%
  clean_names() %>%
  mutate(
    from = as.character(cod_ori),
    to   = as.character(cod_dest),
    uf_ori = as.character(coduf_ori),
    uf_dest = as.character(coduf_dest)
  ) %>%
  filter(uf_ori == "26" | uf_dest == "26") %>%
  pivot_longer(
    cols = starts_with("quest_"),
    names_to = "tema_cod",
    values_to = "intensidade"
  ) %>%
  filter(intensidade != "0") %>%
  mutate(
    tema = case_when(
      tema_cod == "quest_1" ~ "Vestuário e calçados",
      tema_cod == "quest_2" ~ "Móveis e eletroeletrônicos",
      tema_cod == "quest_3" ~ "Saúde (baixa/média complexidade)",
      tema_cod == "quest_4" ~ "Saúde (alta complexidade)",
      tema_cod == "quest_5" ~ "Ensino superior",
      tema_cod == "quest_6" ~ "Atividades culturais",
      tema_cod == "quest_7" ~ "Atividades esportivas",
      tema_cod == "quest_8" ~ "Uso de aeroportos",
      tema_cod == "quest_9" ~ "Jornais impressos",
      tema_cod == "quest_10" ~ "Transporte público",
      TRUE ~ tema_cod
    )
  ) %>%
  select(from, to, tema) %>%
  distinct() %>% 
  left_join(df_rotas, by = c("from", "to")) %>%
  drop_na(tempo_min)

# ---- 5. Acessibilidade Gravitacional (Ai) ----

# 5.1. Acessibilidade FROM (O que a cidade acessa lá fora)
df_acessibilidade_from <- df_arestas %>%
  left_join(df_nos %>% select(name, pib_destino = pib), by = c("to" = "name")) %>%
  group_by(from) %>%
  summarise(
    acessibilidade_from = sum(pib_destino / exp(0.02 * tempo_min), na.rm = TRUE)
  ) %>%
  rename(name = from)

# 5.2. Acessibilidade TO (O que chega na cidade)
df_acessibilidade_to <- df_arestas %>%
  left_join(df_nos %>% select(name, pib_origem = pib), by = c("from" = "name")) %>%
  group_by(to) %>%
  summarise(
    acessibilidade_to = sum(pib_origem / exp(0.02 * tempo_min), na.rm = TRUE)
  ) %>%
  rename(name = to)

# ---- 6. Compatibilização de Geometrias ----
nos_ativos <- unique(c(df_arestas$from, df_arestas$to))

df_nos_filtrados <- df_nos %>% 
  filter(name %in% nos_ativos) %>%
  left_join(df_acessibilidade_from, by = "name") %>%
  left_join(df_acessibilidade_to, by = "name") %>%
  mutate(
    acessibilidade_from = replace_na(acessibilidade_from, 0),
    acessibilidade_to = replace_na(acessibilidade_to, 0)
  )

# ---- 7. Modelagem em Redes (tidygraph) ----
grafo_pe <- tbl_graph(nodes = df_nos_filtrados, edges = df_arestas, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    centralidade_interm_tempo = centrality_betweenness(weights = tempo_min, directed = FALSE),
    centralidade_interm_dist = centrality_betweenness(weights = dist_km, directed = FALSE)
  )

# ---- 8. Relatório e Exportação ----
painel_pe <- grafo_pe %>%
  as_tibble() %>%
  filter(uf == "26") %>%
  arrange(desc(acessibilidade_from))

painel_pe <- painel_pe %>%
  mutate(
    across(
      c(acessibilidade_from, acessibilidade_to, centralidade_interm_tempo, centralidade_interm_dist),
      ~ round((.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)) * 100, 2),
      .names = "indice_{.col}"
    )
  )

painel_pe %>%
  select(
    nome_cidade, 
    hierarquia,         
    cgt,                
    ia,                 
    pib, 
    indice_centralidade_interm_tempo, 
    indice_centralidade_interm_dist, 
    indice_acessibilidade_from,
    indice_acessibilidade_to
  ) %>%
  head(15) %>%
  print()

painel_pe %>%
  write.csv2('data/ibge/regic/regic_pe_main.csv', row.names = FALSE)

painel_pe %>%
  arrow::write_parquet(
    'app_aurora_municipios/data/parquet/regic_pe_main.parquet'
  )

# REDES ----
df_fluxos_intra_pe <- df_arestas_brutas %>%
  clean_names() %>%
  mutate(
    origem = as.character(cod_ori),
    destino = as.character(cod_dest),
    uf_ori = as.character(coduf_ori),
    uf_dest = as.character(coduf_dest)
  ) %>%
  # 1. Filtro Estrito: Origem E Destino devem ser Pernambuco (26)
  filter(uf_ori == "26" & uf_dest == "26") %>%
  # 2. Transformar em formato longo para mapear os temas
  pivot_longer(
    cols = starts_with("quest_"),
    names_to = "tema_cod",
    values_to = "intensidade"
  ) %>%
  # 3. Remover fluxos inexistentes (intensidade zero)
  filter(intensidade != "0" & !is.na(intensidade)) %>%
  mutate(
    intensidade = as.numeric(intensidade),
    # 4. Mapeamento das 10 questões do questionário REGIC
    tema = case_when(
      tema_cod == "quest_1" ~ "vestuario_calcados",
      tema_cod == "quest_2" ~ "moveis_eletro",
      tema_cod == "quest_3" ~ "saude_baixa_media",
      tema_cod == "quest_4" ~ "saude_alta",
      tema_cod == "quest_5" ~ "ensino_superior",
      tema_cod == "quest_6" ~ "ativ_culturais",
      tema_cod == "quest_7" ~ "ativ_esportivas",
      tema_cod == "quest_8" ~ "aeroportos",
      tema_cod == "quest_9" ~ "jornais",
      tema_cod == "quest_10" ~ "transporte_publico",
      TRUE ~ tema_cod
    )
  )

# 5. Agregação em Formato Largo (Matriz Temática)
df_redes_tematicas_pe <- df_fluxos_intra_pe %>%
  group_by(origem, destino) %>%
  # Calcula o peso geral (quantos temas motivam a ligação entre as duas cidades)
  mutate(peso_geral = n()) %>% 
  ungroup() %>%
  # Cria uma coluna de peso (intensidade) para cada tema
  pivot_wider(
    id_cols = c(origem, destino, peso_geral),
    names_from = tema,
    values_from = intensidade,
    names_prefix = "peso_",
    values_fill = 0
  ) %>%
  # Traz os nomes amigáveis das cidades
  left_join(df_nos %>% select(name, origem_nome = nome_cidade), by = c("origem" = "name")) %>%
  left_join(df_nos %>% select(name, destino_nome = nome_cidade), by = c("destino" = "name")) %>%
  # Organiza as colunas de forma limpa
  select(
    origem, origem_nome, 
    destino, destino_nome, 
    peso_geral, 
    starts_with("peso_")
  ) %>%
  arrange(desc(peso_geral))

df_redes_tematicas_pe %>%
  arrow::write_parquet(
    'app_aurora_municipios/data/parquet/regic_pe_fluxos.parquet'
  )
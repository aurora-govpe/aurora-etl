library(arrow)
library(dplyr)
library(stringr)
library(tidyr)

# 1. SETUP E CARGA DE DADOS PROCESSADOS ----
painel_demografico <- read_parquet(
  'data/indicadores/demografia/painel_demografico_pe.parquet'
)

# 2. PROCESSAMENTO DOS INDICADORES DEMOGRÁFICOS ----

## 2.1 Filtro temporal até o ano atual e agrupamento das coortes ----
ano_atual <- as.numeric(format(Sys.Date(), "%Y"))

indicadores_wide <- painel_demografico %>%
  filter(as.numeric(ano) <= ano_atual) %>%
  group_by(locality_id, locality_name, ano) %>%
  summarise(
    # Agregações Populacionais por Coortes Funcionais
    pop_total   = sum(pop_projetada_calib, na.rm = TRUE),
    pop_0_14    = sum(pop_projetada_calib[coorte %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    pop_15_64   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", 
                                                        "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", 
                                                        "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos")], na.rm = TRUE),
    pop_65_mais = sum(pop_projetada_calib[str_detect(coorte, "65|70|75|80|85|90|95|100")], na.rm = TRUE),
    
    # Coortes Específicas de Envelhecimento, Renovação e Juventude
    pop_60_mais = sum(pop_projetada_calib[str_detect(coorte, "60|65|70|75|80|85|90|95|100")], na.rm = TRUE),
    pop_80_mais = sum(pop_projetada_calib[str_detect(coorte, "80|85|90|95|100")], na.rm = TRUE),
    pop_15_24   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos")], na.rm = TRUE),
    pop_20_29   = sum(pop_projetada_calib[coorte %in% c("20 a 24 anos", "25 a 29 anos")], na.rm = TRUE),
    pop_50_59   = sum(pop_projetada_calib[coorte %in% c("50 a 54 anos", "55 a 59 anos")], na.rm = TRUE),
    pop_55_64   = sum(pop_projetada_calib[coorte %in% c("55 a 59 anos", "60 a 64 anos")], na.rm = TRUE),
    pop_15_29   = sum(pop_projetada_calib[coorte %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  ## 2.2 Engenharia de Variáveis (Subdimensões 1.1, 1.2 e 1.5) ----
mutate(
  # Subdimensão 1.1: Estrutura Etária & Dependência
  rd_pop       = ((pop_0_14 + pop_65_mais) / pop_15_64) * 100,
  rd_jovem = (pop_0_14 / pop_15_64) * 100,
  rd_idosa = (pop_65_mais / pop_15_64) * 100,
  pct_pia  = (pop_15_64 / pop_total) * 100,
  
  # Subdimensão 1.2: Envelhecimento Populacional
  ind_envelhecimento       = (pop_60_mais / pop_0_14) * 100,
  pct_longevidade       = (pop_80_mais / pop_60_mais) * 100,
  ind_ir_pia   = (pop_20_29 / pop_50_59) * 100,
  
  # Subdimensão 1.5: Dinâmica da Juventude
  ind_renov_trabalho  = (pop_15_24 / pop_55_64) * 100,
  pct_jovem  = (pop_15_29 / pop_total) * 100
)

## 2.3 Padronização Tidy Data com 'grupo' e 'nm_indicador' ----
indicadores_demograficos_pe <- indicadores_wide %>%
  select(locality_id, locality_name, ano, rd_pop, rd_jovem, rd_idosa, pct_pia, ind_envelhecimento, pct_longevidade, ind_ir_pia, ind_renov_trabalho, pct_jovem) %>%
  pivot_longer(
    cols = c(rd_pop, rd_jovem, rd_idosa, pct_pia, ind_envelhecimento, pct_longevidade, ind_ir_pia, ind_renov_trabalho, pct_jovem),
    names_to = "sigla_indicador",
    values_to = "valor"
  ) %>%
  mutate(
    grupo = "demografia",
    nm_indicador = case_when(
      sigla_indicador == "rd_pop"       ~ "Razão de Dependência Total",
      sigla_indicador == "rd_jovem" ~ "Razão de Dependência Jovem",
      sigla_indicador == "rd_idosa" ~ "Razão de Dependência Idosa",
      sigla_indicador == "pct_pia"  ~ "Peso da População em Idade Ativa",
      sigla_indicador == "ind_envelhecimento"       ~ "Índice de Envelhecimento",
      sigla_indicador == "pct_longevidade"       ~ "Taxa de Superlongevidade",
      sigla_indicador == "ind_ir_pia"   ~ "Índice de Renovação da PIA",
      sigla_indicador == "ind_renov_trabalho"  ~ "Razão de Renovação do Trabalho",
      sigla_indicador == "pct_jovem"  ~ "Peso Relativo da Juventude"
    )
  ) %>%
  select(locality_id, locality_name, ano, grupo, sigla_indicador, nm_indicador, valor)


# 3. EXPORTAÇÃO DOS RESULTADOS ----

## 3.1 Persistência em Parquet ----
path_saida <- 'data/indicadores/demografia/indicadores_demograficos_pe.parquet'

write_parquet(indicadores_demograficos_pe, path_saida)
message(sprintf("--> Indicadores demográficos Tidy salvos em '%s'", path_saida))

## 3.2 Inspeção da estrutura gerada ----
glimpse(indicadores_demograficos_pe)

indicadores_demograficos_pe %>% distinct(sigla_indicador, nm_indicador)
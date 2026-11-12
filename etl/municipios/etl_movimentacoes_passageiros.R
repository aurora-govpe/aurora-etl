# ==============================================================================
# PROJETO AURORAPRIME - INGESTÃO DAS BASES ESTATÍSTICAS DA ANAC (2015 - 2026)
# ==============================================================================

# 1. Carregamento de pacotes
pacotes <- c("tidyverse", "data.table", "janitor", "fs")
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos_pacotes)) install.packages(novos_pacotes)

library(tidyverse)
library(data.table)
library(janitor)
library(fs)

# ==============================================================================
# 2. CONFIGURAÇÃO DE DIRETÓRIOS
# ==============================================================================

if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

dir_brutos <- path(DIR_RAW_AURORA, "aeroportuaria")
dir_temp   <- path(tempdir(), "anac_extracao")

# Estrutura de saída
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")

if (dir_exists(dir_temp)) dir_delete(dir_temp)
dir_create(dir_temp)

# ==============================================================================
# 3. LOCALIZAÇÃO E DESCOMPACTAÇÃO (SE HOUVER .ZIP)
# ==============================================================================
arquivos_pasta <- dir_ls(dir_brutos, type = "file") %>%
  keep(~ !str_detect(path_file(.x), "^log_")) # Ignora o log_rpa_anac.txt

arquivos_csv_para_ler <- c()

message("📦 Verificando arquivos na pasta: ", dir_brutos)

for (arq in arquivos_pasta) {
  ext <- tolower(path_ext(arq))
  
  if (ext == "zip") {
    message("   🗜️ Descompactando: ", path_file(arq))
    extraidos <- unzip(arq, exdir = dir_temp)
    csvs <- extraidos[tolower(path_ext(extraidos)) %in% c("csv", "txt")]
    arquivos_csv_para_ler <- c(arquivos_csv_para_ler, csvs)
    
  } else if (ext %in% c("csv", "txt")) {
    arquivos_csv_para_ler <- c(arquivos_csv_para_ler, arq)
  }
}

arquivos_csv_para_ler <- unique(arquivos_csv_para_ler)
message("📄 Total de arquivos tabulares prontos para leitura: ", length(arquivos_csv_para_ler))

# ==============================================================================
# 4. FUNÇÃO DE LEITURA PADRONIZADA POR ARQUIVO (CORRIGIDA)
# ==============================================================================
ler_base_anac <- function(caminho_arquivo) {
  nome_arq <- path_file(caminho_arquivo)
  ano_ref  <- str_extract(nome_arq, "20[0-9]{2}")
  
  message("   ➡️ Lendo: ", nome_arq, " (Ano ref: ", ano_ref, ")")
  
  # Lê as primeiras 15 linhas para descobrir onde começa o cabeçalho real
  primeiras_linhas <- read_lines(caminho_arquivo, n_max = 15, locale = locale(encoding = "Latin1"))
  linha_cabecalho  <- which(str_detect(toupper(primeiras_linhas), "EMPRESA|ANO|AEROPORTO"))[1]
  pular_linhas     <- ifelse(is.na(linha_cabecalho), 0, linha_cabecalho - 1)
  
  # Detecta automaticamente o separador (; ou ,)
  linha_header_txt <- primeiras_linhas[pular_linhas + 1]
  separador <- ifelse(str_count(linha_header_txt, ";") >= str_count(linha_header_txt, ","), ";", ",")
  
  # Importa usando fread forçando tudo como texto inicialmente
  df <- fread(
    file         = caminho_arquivo,
    sep          = separador,
    skip         = pular_linhas,
    colClasses   = "character",
    encoding     = "Latin-1", # <-- Hífen obrigatório no fread()
    fill         = TRUE,
    showProgress = FALSE
  ) %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(
      arquivo_origem = nome_arq,
      ano_arquivo    = ano_ref,
      .before        = 1
    )
  
  return(df)
}

# ==============================================================================
# 5. EMPILHAMENTO CONSOLIDADO (2015 A 2026)
# ==============================================================================
message("🚀 Iniciando consolidação das bases...")

df_anac_bruto <- arquivos_csv_para_ler %>%
  map(ler_base_anac) %>%
  bind_rows()

# Remove linhas completamente vazias (comuns no rodapé de arquivos da ANAC)
df_anac_bruto <- df_anac_bruto %>%
  filter(if_any(-c(arquivo_origem, ano_arquivo), ~ !is.na(.x) & str_trim(.x) != ""))

# Limpa arquivos temporários extraídos
if (dir_exists(dir_temp)) dir_delete(dir_temp)

# ==============================================================================
# 6. DIAGNÓSTICO RÁPIDO PARA INÍCIO DOS TRATAMENTOS
# ==============================================================================
message("==================================================")
message("✅ IMPORTAÇÃO CONCLUÍDA COM SUCESSO!")
message("📊 Total de registros carregados: ", format(nrow(df_anac_bruto), big.mark = "."))
message("📋 Total de colunas: ", ncol(df_anac_bruto))
message("==================================================")

# Resumo de registros por arquivo/ano importado
amostra <- df_anac_bruto %>%
  head(n = 30)

# df_anac_final <- df_anac_bruto %>% 
#   select(arquivo_origem, ano_arquivo, ano, mes, decolagens, empresa_nome, aeroporto_de_origem_sigla, aeroporto_de_origem_nome, aeroporto_de_origem_uf, aeroporto_de_destino_sigla, aeroporto_de_destino_nome, aeroporto_de_destino_uf, natureza, grupo_de_voo, passageiros_pagos, passageiros_gratis, carga_gratis_kg, carga_paga_kg, correio_kg, bagagem_kg)

# ==============================================================================
# 1. TABELA AUXILIAR DE UFs DO BRASIL (SIGLA -> NOME COMPLETO)
# ==============================================================================
tabela_ufs <- tribble(
  ~uf,  ~nome_uf,
  "AC", "Acre",
  "AL", "Alagoas",
  "AP", "Amapá",
  "AM", "Amazonas",
  "BA", "Bahia",
  "CE", "Ceará",
  "DF", "Distrito Federal",
  "ES", "Espírito Santo",
  "GO", "Goiás",
  "MA", "Maranhão",
  "MT", "Mato Grosso",
  "MS", "Mato Grosso do Sul",
  "MG", "Minas Gerais",
  "PA", "Pará",
  "PB", "Paraíba",
  "PR", "Paraná",
  "PE", "Pernambuco",
  "PI", "Piauí",
  "RJ", "Rio de Janeiro",
  "RN", "Rio Grande do Norte",
  "RS", "Rio Grande do Sul",
  "RO", "Rondônia",
  "RR", "Roraima",
  "SC", "Santa Catarina",
  "SP", "São Paulo",
  "SE", "Sergipe",
  "TO", "Tocantins"
)

# ==============================================================================
# 2. LIMPEZA E CONSOLIDAÇÃO EM UMA BASE ÚNICA (POR UF, NOME UF E ANO)
# ==============================================================================
df_anac_limpo <- df_anac_bruto %>%
  select(
    arquivo_origem, ano_arquivo, ano, mes, decolagens, empresa_nome,
    aeroporto_de_origem_sigla, aeroporto_de_origem_nome, aeroporto_de_origem_uf,
    aeroporto_de_destino_sigla, aeroporto_de_destino_nome, aeroporto_de_destino_uf,
    natureza, grupo_de_voo, passageiros_pagos, passageiros_gratis,
    carga_gratis_kg, carga_paga_kg, correio_kg, bagagem_kg
  ) %>%
  mutate(
    ano = as.integer(coalesce(na_if(str_trim(ano), ""), ano_arquivo)),
    uf_origem  = str_to_upper(str_trim(aeroporto_de_origem_uf)),
    uf_destino = str_to_upper(str_trim(aeroporto_de_destino_uf)),
    across(
      c(passageiros_pagos, passageiros_gratis),
      ~ replace_na(as.numeric(str_replace_all(str_trim(.x), ",", ".")), 0)
    )
  )

# Empilha os dois sentidos de movimento (Embarque e Desembarque) e pivota para colunas únicas
df_passageiros_uf_ano <- bind_rows(
  df_anac_limpo %>%
    filter(uf_origem %in% tabela_ufs$uf) %>%
    transmute(ano, uf = uf_origem, tipo = "embarque", passageiros_pagos, passageiros_gratis),
  
  df_anac_limpo %>%
    filter(uf_destino %in% tabela_ufs$uf) %>%
    transmute(ano, uf = uf_destino, tipo = "desembarque", passageiros_pagos, passageiros_gratis)
) %>%
  group_by(ano, uf, tipo) %>%
  summarise(
    pagos  = sum(passageiros_pagos, na.rm = TRUE),
    gratis = sum(passageiros_gratis, na.rm = TRUE),
    total  = sum(passageiros_pagos + passageiros_gratis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = tipo,
    values_from = c(pagos, gratis, total),
    values_fill = 0
  ) %>%
  left_join(tabela_ufs, by = "uf") %>%
  mutate(
    # Totais consolidados por modalidade e geral
    total_pagos  = pagos_embarque + pagos_desembarque,
    total_gratis = gratis_embarque + gratis_desembarque,
    total_geral  = total_embarque + total_desembarque
  ) %>%
  select(
    ano,
    uf,
    nome_uf,
    # Colunas separadas de Embarque
    pagos_embarque,
    gratis_embarque,
    total_embarque,
    # Colunas separadas de Desembarque
    pagos_desembarque,
    gratis_desembarque,
    total_desembarque,
    # Colunas de Soma Total (Embarque + Desembarque)
    total_pagos,
    total_gratis,
    total_geral
  ) %>%
  arrange(ano, uf) %>% 
  filter(ano != lubridate::year(Sys.Date()))

df_passageiros_uf_ano %>%
  writexl::write_xlsx(path(dir_brutos, "df_passageiros_uf_ano.xlsx"))

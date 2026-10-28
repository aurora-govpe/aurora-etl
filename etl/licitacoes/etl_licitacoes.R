# pacotes ----
library(BigDataPE)
library(cli)
library(data.table)
library(DBI)
library(dplyr)
library(glue)
library(googledrive)
library(hms)
library(janitor)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(RPostgres)
library(stringr)
library(tidyr)

# 1. Setup e Resolução Centralizada de Caminhos por Sistema Operacional ----
is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) Sys.setlocale(locale = "pt_BR.utf8")

# Resolução de caminhos centralizados se paths.R estiver disponível
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Diretórios Dinâmicos: compatibilidade total Local (Windows) vs Servidor (Linux)
DIR_BASE <- if (is_linux) {
  if (dir.exists("/srv/rotinas")) "/srv/rotinas" else "/srv/dados_brutos/aurora"
} else {
  if (exists("DIR_RAW_AURORA") && dir.exists(DIR_RAW_AURORA)) DIR_RAW_AURORA else "C:/dados/dados brutos/aurora"
}

DIR_SAIDA <- if (is_linux) {
  "/srv/dados/licitacoes"    # Volume montado no container de produção
} else {
  if (exists("DIR_OUT_AURORA") && dir.exists(DIR_OUT_AURORA)) DIR_OUT_AURORA else "C:/dados/aurora"
}

## 1.2 Tokens de Banco de Dados ----
config_candidates <- c(
  file.path(DIR_BASE, "config.yml"),
  file.path(DIR_BASE, "licitacoes/config.yml"),
  "config.yml"
)
config_path <- config_candidates[file.exists(config_candidates)][1]
config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'licitações',
  config_tokens$sgc_processos
)

# Define o caminho onde o cache será salvo
arquivo_cache <- file.path(DIR_SAIDA, "dados_licitacoes_cache.rds")

# Lista de status
lista_status_candidates <- c(
  file.path(DIR_BASE, "lista_status.xlsx"),
  file.path(DIR_BASE, "licitacoes/lista_status.xlsx"),
  "lista_status.xlsx"
)
arquivo_lista_status <- lista_status_candidates[file.exists(lista_status_candidates)][1]
lista_status <- readxl::read_excel(arquivo_lista_status)

lista_status %>% glimpse()

parse_json_seguro <- purrr::possibly(
  .f = function(x) jsonlite::fromJSON(x, simplifyDataFrame = TRUE),
  otherwise = NULL
)

# 1. Lógica para decidir se precisa baixar
baixar_novo <- TRUE

if (file.exists(arquivo_cache)) {
  info_arquivo <- file.info(arquivo_cache)
  # Calcula a diferença de horas desde a última vez que o arquivo foi modificado
  horas_desde_modificacao <- difftime(Sys.time(), info_arquivo$mtime, units = "hours")
  
  if (horas_desde_modificacao < 1) {
    baixar_novo <- FALSE
  }
}

# 2. Executa a requisição com tryCatch para lidar com falhas
dados_licitacoes <- tryCatch({
  
  if (baixar_novo) {
    cli::cli_alert_info("Iniciando download da API (cache antigo ou inexistente)...")
    
    dados_brutos <- BigDataPE::bdpe_fetch_data(
      base_name = "licitações",
      verbosity = 1,
      limit = Inf
    ) %>%
      janitor::clean_names()
    
    # -------------------------------------------------------------------------
    # EXTRAÇÃO PRÉVIA DA DATA DA "ANÁLISE SEI" NO HISTÓRICO
    # -------------------------------------------------------------------------
    data_analise_sei_df <- dados_brutos %>%
      select(id, historico_json) %>%
      filter(!is.na(historico_json), historico_json != "", historico_json != "[]") %>%
      mutate(historico_dados = purrr::map(historico_json, parse_json_seguro)) %>%
      select(-historico_json) %>%
      filter(!map_lgl(historico_dados, is.null)) %>%
      tidyr::unnest(historico_dados, keep_empty = TRUE) %>%
      janitor::clean_names() %>%
      mutate(
        created_date = as.Date(ymd_hms(created_at))
      ) %>%
      filter(trimws(action) == "Enviado para o Sei") %>%
      group_by(id) %>%
      summarise(data_analise_sei = min(created_date, na.rm = TRUE), .groups = "drop")
    
    dados <- dados_brutos %>%
      mutate(
        created_at = ymd_hms(created_at),
        updated_at = ymd_hms(updated_at),
        stage_started_at = ymd_hms(stage_started_at),
        created_date = as.Date(created_at),
        updated_date = as.Date(updated_at),
        stage_started_date = as.Date(stage_started_at),
        created_time = as_hms(created_at),
        updated_time = as_hms(updated_at),
        stage_started_time = as_hms(stage_started_at)
      ) %>%
      left_join(
        lista_status,
        by = c('stage' = 'status')
      ) %>%
      left_join(
        data_analise_sei_df,
        by = "id"
      ) %>%
      mutate(
        
        estagio_geral = case_when(
          ordem == 1 ~ "Em preparação",
          ordem >= 2 & ordem <= 15 ~ "Em andamento",
          ordem == 16 ~ "Concluído",
          ordem > 16 ~ "Cancelado",
          TRUE ~ "Não identificado"
        ),
        
        responsavel_atual = case_when(
          setor_responsavel == "orgao" ~ orgao,
          setor_responsavel == "gerencia_nome" ~ gerencia_nome,
          setor_responsavel == "-" ~ "Não se aplica",
          TRUE ~ NA_character_
        ),
        
        area = case_when(
          area %in% c('DGA', 'GUPP') ~ 'SEAF',
          TRUE ~ area
        ),
        
        # ---------------------------------------------------------------------
        # REGRAS DE ATRASO CALCULADAS NO ETL (BLINDADO CONTRA CAIXA ALTA/BAIXA)
        # ---------------------------------------------------------------------
        fase_norm = tolower(trimws(stage)),
        
        dias_na_fase = as.numeric(difftime(Sys.Date(), stage_started_date, units = "days")),
        dias_desde_analise_sei = as.numeric(difftime(Sys.Date(), data_analise_sei, units = "days")),
        dias_desde_criacao = as.numeric(difftime(Sys.Date(), created_date, units = "days")),
        
        prazo_fase = case_when(
          fase_norm == "processo em cadastramento" ~ 45,
          fase_norm %in% c("ajustes solicitados", "ajustes realizados") ~ 10,
          fase_norm %in% c("aguardando distribuição", "processo autorizado", "documento de cotas em construção", "enviado para análise de cotas", "cota reprovada") ~ 1,
          fase_norm == "processo em análise" ~ 10,
          fase_norm == "processo em análise sei" ~ 80,
          fase_norm == "processo em fase de disputa" ~ 80,
          TRUE ~ NA_real_
        ),
        
        dias_referencia_atraso = case_when(
          fase_norm == "processo em fase de disputa" & !is.na(dias_desde_analise_sei) ~ dias_desde_analise_sei,
          fase_norm == "processo em fase de disputa" & is.na(dias_desde_analise_sei) ~ dias_desde_criacao,
          TRUE ~ dias_na_fase
        ),
        
        is_atrasado = if_else(!is.na(prazo_fase) & dias_referencia_atraso > prazo_fase, TRUE, FALSE, missing = FALSE),
        
        dias_atraso = if_else(is_atrasado, floor(dias_referencia_atraso - prazo_fase), 0),
        
        status_prazo = if_else(is_atrasado, "Atrasado", "No Prazo"),
        
        motivo_atraso = case_when(
          is_atrasado & fase_norm == "processo em fase de disputa" ~
            paste0("Atrasado \u2013 contados a partir da An\u00e1lise SEI (Total decorrido: ", floor(dias_referencia_atraso), " dias)"),
          is_atrasado ~
            paste0("Atrasado \u2013 Passaram-se ", floor(dias_referencia_atraso), " dias. O limite \u00e9 ", prazo_fase, " dia(s)."),
          TRUE ~ ""
        )
      )
    
    # Salva/Sobrescreve o arquivo de cache com os dados novos e processados
    saveRDS(dados, arquivo_cache)
    cli::cli_alert_success("Dados baixados e cache atualizado com sucesso!")
    
    dados 
    
  } else {
    cli::cli_alert_info("Cache recente encontrado. Carregando dados locais...")
    readRDS(arquivo_cache)
  }
  
}, error = function(e) {
  # 3. Lógica de Fallback (Se der qualquer erro na API, ele cai aqui)
  cli::cli_alert_danger(glue::glue("Falha ao baixar da API. Erro original: {e$message}"))
  
  if (file.exists(arquivo_cache)) {
    cli::cli_alert_warning("Ativando contingência: Carregando a última versão local salva no cache.")
    return(readRDS(arquivo_cache))
  } else {
    stop("Falha na API e nenhum cache local foi encontrado em ", DIR_SAIDA)
  }
})

dados_licitacoes %>% glimpse()

## tbls auxs ----
# Criando a tabela auxiliar
dados_lotes_itens <- dados_licitacoes %>%
  # 1. Seleciona a chave que conecta com a base original e a coluna JSON
  select(id, lotes_json) %>% 
  
  # 2. Filtra vazios e nulos para não gastar processamento à toa
  filter(!is.na(lotes_json), lotes_json != "", lotes_json != "[]") %>%
  
  # 3. Converte o texto JSON em um objeto de dados (lista de data.frames)
  mutate(lotes_dados = purrr::map(lotes_json, parse_json_seguro)) %>%
  
  # Removemos o texto bruto para economizar memória
  select(-lotes_json) %>% 
  
  # Remove linhas que não puderam ser convertidas (JSON inválido)
  filter(!map_lgl(lotes_dados, is.null)) %>%
  
  # 4. Primeira explosão: Nível de LOTES
  # Isso vai gerar as colunas 'lote_id', 'nome_lote' e 'itens' (que será uma lista)
  tidyr::unnest(lotes_dados, keep_empty = TRUE) %>%
  
  # 5. Segunda explosão: Nível de ITENS
  # Isso vai abrir a lista 'itens' e criar colunas para item_id, descricao, valor, etc
  tidyr::unnest(itens, keep_empty = TRUE) %>%
  
  # 6. Padroniza o nome das colunas geradas
  janitor::clean_names() %>%
  mutate(
    valor_item = as.numeric(valor_item) / 10000,
    valor_total = as.numeric(valor_total) / 10000
  )

# Verifica o resultado
dados_lotes_itens %>% glimpse()

dados_pncp <- dados_licitacoes %>%
  select(id, pncp_json) %>% 
  
  # 1. Filtros mais robustos (incluindo a string literal "null")
  filter(
    !is.na(pncp_json), 
    pncp_json != "", 
    pncp_json != "[]", 
    pncp_json != "null" 
  ) %>%
  
  mutate(pncp_dados = purrr::map(pncp_json, parse_json_seguro)) %>%
  select(-pncp_json) %>% 
  filter(!map_lgl(pncp_dados, is.null)) %>%
  
  tidyr::unnest(pncp_dados, keep_empty = TRUE) %>%
  janitor::clean_names()

# 2. Conversão segura de tipos em uma etapa separada
# Só vai tentar converter as colunas se elas realmente nascerem no unnest()
dados_pncp <- dados_pncp %>%
  mutate(
    # Tenta converter o valor (se existir)
    across(any_of("valor_total_homologado"), as.numeric),
    
    # Tenta converter a data (cobri variações de nome por precaução)
    across(any_of(c("data_homologado", "data_homologacao")), as.Date) 
  ) %>%
  mutate(
    # Atualizado: Converte para número e divide por 10000
    across(any_of("valor_total_homologado"), ~ as.numeric(.x) / 10000),
    
    # Mantém a regra da data
    across(any_of(c("data_homologado", "data_homologacao")), as.Date) 
  )

# Verifica o resultado
dados_pncp %>% glimpse()

dados_item_pncp <- dados_lotes_itens %>%
  left_join(
    dados_pncp %>%
      rename(
        descricao_pncp = descricao
      ),
    by = c('id', 'position_item')
  )

dados_item_pncp %>% glimpse()

dados_historico <- dados_licitacoes %>%
  # 1. Seleciona a chave da licitação e a coluna do JSON
  select(id, historico_json) %>% 
  
  # 2. Filtra registros vazios
  filter(!is.na(historico_json), historico_json != "", historico_json != "[]") %>%
  
  # 3. Lê o JSON convertendo para data.frame
  mutate(historico_dados = purrr::map(historico_json, parse_json_seguro)) %>%
  
  # 4. Remove a coluna de texto e linhas com erro de leitura
  select(-historico_json) %>% 
  filter(!map_lgl(historico_dados, is.null)) %>%
  
  # 5. Explosão ÚNICA: Cria as colunas action, created_by, description, created_at
  tidyr::unnest(historico_dados, keep_empty = TRUE) %>%
  
  # 6. Padroniza os nomes
  janitor::clean_names() %>%
  
  # 7. Tratamento de Data e Hora (fechando o ciclo da sua primeira dúvida!)
  mutate(
    # Avisa ao R que o texto original é uma Data-Hora
    created_at = ymd_hms(created_at),
    
    # Extrai data e hora separadamente em novos formatos
    created_date = as.Date(created_at),
    created_time = hms::as_hms(created_at)
  ) %>%
  arrange(id, created_at)

# Verifica o resultado
glimpse(dados_historico)

# salvar ----
dados_licitacoes2 <- dados_licitacoes %>% 
  mutate( ano_corrente = year(Sys.Date()),
          contar_geral = case_when(
            estagio_geral %in% c("Em preparação") ~ "Não",
            # Regra SIM (a): Criado no ano corrente
            year(created_at) == ano_corrente ~ "Sim",
            # Regra SIM (b e c): Criado em anos anteriores, mas ativo/em andamento durante o ano corrente
            year(created_at) < ano_corrente & estagio_geral %in% c("Em andamento") ~ "Sim",
            # Exceção de exclusão (Regra NÃO): Se foi concluído/cancelado em anos anteriores
            year(created_at) < ano_corrente & estagio_geral %in% c("Concluído", "Cancelado") ~ "Não",
            TRUE ~ "Não"),   
          contar_andamento = case_when(
            # Regra SIM (a): Criado no ano corrente com estagio em andamento
            year(created_at) == ano_corrente & estagio_geral %in% c("Em andamento") ~ "Sim",
          #Regra SIM (b): Processos de anos anteriores que viraram o ano (1º dia do ano) já no estágio "Em andamento"
            year(created_at) < ano_corrente & estagio_geral == "Em andamento" ~ "Sim",
            # Exceção de exclusão (Regras NÃO): Concluído/Cancelado do ano anterior OU se virou "Em andamento" durante o ano
            year(created_at) < ano_corrente & estagio_geral %in% c("Concluído", "Cancelado") ~ "Não",
            TRUE ~ "Não"),
          contar_conclusao = case_when(
            year(stage_started_at) == ano_corrente & estagio_geral %in% c("Concluído") ~ "Sim",
            TRUE ~ "Não")
          )

# salvar ----

# 1. Salva em RDS (backup e compatibilidade)
saveRDS(dados_licitacoes2, file.path(DIR_SAIDA, "dados_licitacoes.rds"))
saveRDS(dados_lotes_itens, file.path(DIR_SAIDA, "dados_lotes_itens.rds"))
saveRDS(dados_pncp, file.path(DIR_SAIDA, "dados_pncp.rds"))
saveRDS(dados_item_pncp, file.path(DIR_SAIDA, "dados_item_pncp.rds"))

dados_carga <- tibble::tibble(
  data_hora_atualizacao = Sys.time()
)
saveRDS(dados_carga, file.path(DIR_SAIDA, "dados_carga.rds"))

# 2. Salva em JSON (Backend Go ultraleve Aurora)
cli::cli_alert_info("Exportando dados em formato JSON para o backend Go...")
jsonlite::write_json(dados_licitacoes2, file.path(DIR_SAIDA, "dados_licitacoes.json"), auto_unbox = TRUE)
jsonlite::write_json(dados_item_pncp, file.path(DIR_SAIDA, "dados_item_pncp.json"), auto_unbox = TRUE)
jsonlite::write_json(dados_carga, file.path(DIR_SAIDA, "dados_carga.json"), auto_unbox = TRUE)
cli::cli_alert_success(paste("Arquivos JSON e RDS gerados com sucesso em:", DIR_SAIDA))

dados_licitacoes2 %>% count(is_atrasado, status_prazo, is.na(dias_desde_analise_sei))


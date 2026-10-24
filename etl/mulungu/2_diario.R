# Carregando os pacotes necessários
library(tidyverse)  # Manipulação e visualização de dados
library(arrow)      # Leitura/escrita de arquivos Parquet
library(nanoarrow)  # Suporte para fluxos compatíveis com bancos de dados
library(DBI)        # Interface com banco de dados
library(RPostgres)  # Conexão com PostgreSQL/Greenplum
library(glue)       # Manipulação de strings com interpolação
library(pool)       # Gerenciamento de conexões ao banco
library(lubridate)  # Manipulação de datas
library(stringr)    # Manipulação de strings
library(janitor)    # Limpeza de nomes de colunas
library(httr2)      # Requisições HTTP
library(BigDataPE)  # Biblioteca específica para dados de BigData PE
library(vialactea)  # Biblioteca da SEME/SEPE
library(plug)       # Biblioteca para acessar dados do Plug/Pbf (DER)
library(diario)
#library(RJDBC)      # Biblioteca específica para dados de BigData PE


#' Liberar Memória de Objeto no Ambiente Global
#'
#' Remove um objeto específico do ambiente global e executa a coleta de lixo para liberar memória.
#'
#' @param objeto Um caractere indicando o nome do objeto a ser removido do ambiente global.
#' @param verbose Um valor lógico. Se `TRUE`, mensagens informativas sobre a operação serão exibidas.
#' O padrão é `TRUE`.
#'
#' @details
#' A função verifica se o objeto existe no ambiente global antes de removê-lo.
#' Após a remoção, a coleta de lixo é acionada para otimizar o uso de memória.
#'
#' @return Um valor lógico: `TRUE` se o objeto foi removido com sucesso, ou `FALSE` caso contrário.
#' @examples
#' # Criar um objeto e liberá-lo
#' meu_objeto <- 1:100
#' liberar_memoria("meu_objeto", verbose = TRUE)
#'
#' # Tentativa de liberar um objeto inexistente sem mensagens
#' liberar_memoria("objeto_inexistente", verbose = FALSE)
#' @export
liberar_memoria <- function(objeto, verbose = TRUE) {
  # Verifica se o argumento fornecido é um nome de objeto válido
  if (!is.character(objeto) || length(objeto) != 1) {
    stop("O argumento 'objeto' deve ser um nome de objeto (string).")
  }
  
  # Verifica se o objeto existe no ambiente global
  if (exists(objeto, envir = globalenv())) {
    # Remove o objeto e executa a coleta de lixo
    rm(list = c(objeto), envir = globalenv())
    gc(full = TRUE)
    if (verbose) {
      message(paste("Objeto", objeto, "removido com sucesso."))
    }
    return(TRUE)
  } else {
    if (verbose) {
      message(paste("Objeto", objeto, "não existe no ambiente global."))
    }
    return(FALSE)
  }
}

#' Processar e Enviar Base de Dados
#'
#' Esta função processa uma base de dados utilizando uma função de processamento especificada,
#' envia os dados processados para uma tabela em um banco de dados e, opcionalmente, ajusta
#' a distribuição das colunas no Greenplum.
#'
#' @param nome_base `character` Nome da base de dados que está sendo processada.
#' @param funcao_processamento `function` Função responsável por processar a base de dados.
#' Deve retornar um `data.frame` ou `NULL` se não houver processamento necessário.
#' @param pool_conexao `DBI::DBIConnection` Objeto de conexão com o banco de dados.
#' @param tabela_destino `character` Nome da tabela de destino no banco de dados onde os
#' dados processados serão armazenados.
#' @param colunas_distribuicao `character` Vetor com os nomes das colunas para definir a
#' distribuição no Greenplum. Se for `NULL`, o comando `ALTER TABLE` não será executado.
#' Caso sejam especificadas colunas inválidas, um erro será emitido.
#'
#' @return Retorna `TRUE` se o processamento e envio forem concluídos com sucesso. Retorna `NULL`
#' se a função de processamento não gerar nenhum dado (retornar `NULL`).
#'
#' @details
#' A função segue os seguintes passos:
#' 1. Executa a `funcao_processamento` para obter os dados processados.
#' 2. Se a função de processamento retornar `NULL`, não realiza nenhuma ação e encerra o fluxo.
#' 3. Envia os dados processados para a tabela especificada no banco de dados.
#' 4. Se `colunas_distribuicao` for especificado e válido, executa o comando
#' `ALTER TABLE` para ajustar a distribuição no banco de dados.
#'
#' @examples
#' # Exemplo com processamento e ajuste de distribuição:
#' processar_e_enviar_base(
#'   nome_base = "EFISCO_CONTRATOS",
#'   funcao_processamento = processamento_efisco_contratos,
#'   pool_conexao = pool,
#'   tabela_destino = "efisco_contratos",
#'   colunas_distribuicao = c("contrato")
#' )
#'
#' # Exemplo sem ajuste de distribuição:
#' processar_e_enviar_base(
#'   nome_base = "EFISCO_CONTRATOS",
#'   funcao_processamento = processamento_efisco_contratos,
#'   pool_conexao = pool,
#'   tabela_destino = "efisco_contratos"
#' )
#'
#' @importFrom DBI dbWriteTable dbExecute
#' @export
processar_e_enviar_base <- function(
    nome_base,
    funcao_processamento,
    pool_conexao,
    tabela_destino,
    caminho_parquet,
    caminho_qs,
    colunas_distribuicao = NULL
) {
  message(sprintf("Iniciando processamento para a base '%s'.", nome_base))
  
  # Executa a função de processamento
  dados_processados <- funcao_processamento()
  
  if (is.null(dados_processados)) {
    message(sprintf("Nenhuma atualização necessária ou possível para a base '%s'.", nome_base))
    return(invisible(NULL))
  } else {
    # Caso contrário, salva o resultado como Parquet
    message(glue::glue("Salvando o resultado no arquivo Parquet: {caminho_parquet}"))
    arrow::write_parquet(dados_processados, caminho_parquet)
    qs::qsave(dados_processados, caminho_qs)
  }
  
  # Verificação da validade das colunas de distribuição
  if (!is.null(colunas_distribuicao)) {
    colunas_validas <- colunas_distribuicao %in% names(dados_processados)
    
    if (!all(colunas_validas)) {
      stop(sprintf(
        "Erro: As colunas de distribuição especificadas (%s) não são válidas na base '%s'.",
        paste(colunas_distribuicao[!colunas_validas], collapse = ", "),
        nome_base
      ))
    }
  }
  
  # Enviar dados para o banco de dados
  message(sprintf("Enviando dados para a tabela '%s'.", tabela_destino))
  DBI::dbWriteTable(
    conn = pool_conexao,
    name = tabela_destino,
    value = dados_processados,
    overwrite = TRUE
  )
  
  # Alteração da distribuição das colunas (se aplicável)
  if (!is.null(colunas_distribuicao)) {
    comando_alter_table <- sprintf(
      "ALTER TABLE %s SET DISTRIBUTED BY (%s);",
      tabela_destino,
      paste0("\"", paste(colunas_distribuicao, collapse = "\" , \""), "\"")
    )
    
    message("Executando comando para ajuste de distribuição:")
    message(comando_alter_table)
    
    DBI::dbExecute(pool_conexao, comando_alter_table)
  }
  
  message(sprintf("Processamento e envio da base '%s' concluídos.", nome_base))
  message("Liberando memória do objeto temporário...")
  liberar_memoria("dados_processados", verbose = FALSE)
  return(invisible(TRUE))
}

#' Processa Dados de Contratos do Plug
#'
#' Lê e processa os dados de contratos disponíveis na base do sistema Plug.
#'
#' @details
#' Essa função baixa os dados usando o pacote `plug` e aplica transformações como:
#' conversão de datas, limpeza de colunas e conversão de valores numéricos.
#'
#' @return Uma tibble com os dados de contratos do Plug processados ou `NULL` em caso de erro.
#' @examples
#' plug_contratos <- processamento_plug_contratos()
#' @export
processamento_plug_contratos <- function() {
  # Simula o processamento e retorna NULL ou uma tibble
  message("Executando processamento de contratos do Plug.")
  
  # Tentar carregar e processar o arquivo
  base <- try({
    
    plug_raw <- plug_download_base(base_name = 'VW_InfoContratuais_View_Exp')
    
    plug_raw %>%
      janitor::clean_names() %>%
      mutate(
        data_de_assinatura = dmy_hms(data_de_assinatura),
        data_de_a_s = dmy_hms(data_de_a_s),
        data_base_do_reajuste = dmy_hms(data_base_do_reajuste),
        data_de_termino_da_vigencia = dmy_hms(data_de_termino_da_vigencia),
        data_de_termino_da_execucao = dmy_hms(data_de_termino_da_execucao),
        data_de_termino_aditada_da_execucao = dmy_hms(data_de_termino_aditada_da_execucao),
        data_de_termino_aditada_da_vigencia = dmy_hms(data_de_termino_aditada_da_vigencia),
        extensao_km = as.numeric(str_replace(extensao_km, ",", ".")),
        valor_atual_r = as.numeric(str_replace(valor_atual_r, ",", ".")),
        valor_medido_acumulado = as.numeric(str_replace(valor_medido_acumulado, ",", ".")),
        valor_da_po_r = as.numeric(str_replace(valor_da_po_r, ",", ".")),
        valor_aditado_r = as.numeric(str_replace(valor_aditado_r, ",", ".")),
        valor_inicial_r = as.numeric(str_replace(valor_inicial_r, ",", ".")),
        prazo_aditado_da_execucao = as.integer(prazo_aditado_da_execucao, ",", "."),
        prazo_aditado_da_vigencia = as.integer(prazo_aditado_da_vigencia, ",", "."),
        prazo_de_vigencia = as.integer(prazo_de_vigencia, ",", "."),
        prazo_de_execucao = as.integer(prazo_de_execucao, ",", "."),
        prazo_inicial_da_vigencia = as.integer(prazo_inicial_da_vigencia, ",", "."),
        prazo_inicial_da_execucao = as.integer(prazo_inicial_da_execucao, ",", ".")
      ) %>%
      mutate(across(where(is.character), \(x) if_else(x == "", NA_character_, x)))
    
  }, silent = TRUE)
  
  # Retornar NULL se ocorrer um erro
  if (inherits(base, "try-error")) {
    base <- NULL
    message("Erro ao processar o dados de contratos do Plug. Retornando NULL.")
  }
  
  return(base)
}



#' Processa Dados de Contratos do Plug
#'
#' Lê e processa os dados de contratos disponíveis na base do sistema Plug.
#'
#' @details
#' Essa função baixa os dados usando o pacote `plug` e aplica transformações como:
#' conversão de datas, limpeza de colunas e conversão de valores numéricos.
#'
#' @return Uma tibble com os dados de relatorios do Diario de Obras ou `NULL` em caso de erro.
#' @examples
#' diario_relatorios <- processamento_diario_relatorios()
#' @export
processamento_diario_relatorios <- function() {
  # Simula o processamento e retorna NULL ou uma tibble
  message("Executando processamento de relatorios do Diario de Obras")
  diario::diario_store_token("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpYXQiOjE3MTA1MDM5MTUsImp0aSI6IjljNWYxMzFiYmExZTZmZWRlZDJkN2NlMDI4MzE4Zjk5NTBlYzI1MDE3OTQyYjNiMDBhNjMzZjYxZDMwNmFlYTEzMDI0OTA4NyIsImNvZCI6IjYyMDUzZjZjM2EzYjkzYzcyNmQyYjM2NDhlZGEyZTkwOWZlZmUxMWEiLCJlbXByZXNhSWQiOiI2NWJkMWE0NWNjMmRmNWY4NjkwYTc4ZTQiLCJpc3MiOiJhcHAtYXBpIn0.VUCz4YIUms5XLCUovJvjankWCCJaENC5pLgYwNlhuJg")
  
  # Tentar carregar e processar o arquivo
  base <- try({
    
    
    obras <- diario::diario_get_projects()
    
    #### Relatórios
    
    obras_details <- obras %>%
      unnest(status, names_sep =  "_") %>%
      select(-c(totalRelatorios, totalFotos)) %>%
      mutate(details = map(`_id`,
                           function(x){
                             rst <- diario::diario_get_project_details(x)
                             
                             rst_tibble <- tibble(numeroContrato = rst %>% pluck("numeroContrato"),
                                                  fotoUrl = rst %>% pluck("fotoUrl"),
                                                  prazo_contratual = rst %>% pluck("prazo") %>% pluck("contratual"),
                                                  prazo_decorrido = rst %>% pluck("prazo") %>% pluck("decorrido"),
                                                  prazo_aVencer = rst %>% pluck("prazo") %>% pluck("aVencer"),
                                                  grupo_descricao = rst %>% pluck("grupo") %>% pluck("descricao"))
                             
                             return(rst_tibble)
                           }
      )
      ) %>%
      unnest(details, names_sep =  "_")
    
    obras_reports <- obras_details %>%
      mutate(reports = map(`_id`,
                           function(x){
                             rst <- diario::diario_get_reports(x, limit = 1e6)
                             
                             if (nrow(rst) > 0) {
                               tbl <- rst %>%
                                 #slice_max(order_by = numero, n = 5) %>%
                                 unnest(c(status, criadoPor), names_sep =  "_") %>%
                                 unnest(criadoPor_usuario, names_sep =  "_")  %>%
                                 select(`_id`, data, numero, status_descricao,
                                        criadoPor_usuario_nome, criadoPor_usuario_cargo,
                                        criadoPor_usuario_email)
                             } else {
                               tbl <- tibble(`_id` = NA_character_, data = NA_character_, numero = NA_integer_, status_descricao = NA_character_,
                                             criadoPor_usuario_nome = NA_character_, criadoPor_usuario_cargo = NA_character_,
                                             criadoPor_usuario_email = NA_character_)
                             }
                             return(tbl)
                           }
      )
      ) %>%
      unnest(reports, names_sep =  "_")
    
    obras_reports_links <- obras_reports %>%
      mutate(links = map2(`_id`,
                          reports__id,
                          function(x, y) {
                            if (!is.na(y)) {
                              rst <- diario::diario_get_report_details(x, y)
                              link_logo <- rst %>% pluck("logomarca") %>%
                                map(~ tibble(
                                  ativo = pluck(.x, "ativo"),
                                  nomeArquivo = pluck(.x, "nomeArquivo"),
                                  url = pluck(.x, "url")
                                )
                                ) %>% bind_rows() %>%
                                filter(ativo) %>%
                                slice_head(n = 1) %>%
                                pull(url)
                              link_pdf <- rst %>% pluck("linkPdf")
                              rst_links <- tibble(link_pdf = if_else(is.null(link_pdf), NA_character_, as.character(link_pdf)),
                                                  link_logo = if_else(is.null(link_logo), NA_character_, as.character(link_logo)))
                            } else {
                              rst_links <- tibble(link_pdf = NA_character_, link_logo = NA_character_)
                            }
                            return(rst_links)
                          }
      )
      ) %>%
      unnest(links)
    
    obras_reports_links <- obras_reports_links %>%
      mutate(contrato_obra = str_extract(details_numeroContrato, "(?<=(OBRA:\\s)).*?(?=(\\||$|(SUP)))"), .after = nome) %>%
      mutate(contrato_obra = str_trim(contrato_obra)) %>%
      mutate(contrato_supervisao = str_extract(details_numeroContrato, "(?<=(SUPEVISÃO:\\s)|(SUPERVISÃO:\\s)).*(?=$)"), .after = contrato_obra) %>%
      mutate(contrato_supervisao = str_trim(contrato_supervisao)) %>%
      mutate(contrato = if_else(str_detect(details_numeroContrato, "OBRA"), contrato_obra, details_numeroContrato), .before = contrato_obra)
    
    
    obras_reports_links  %>%
      mutate(obra = str_extract(nome, "(^[\\dº\\s{1,2}DRO])|((^BR[\\s,-]*\\d{3}))|([A,V]*PE[-,\\s]*\\d{3})|(Aeródromo)|(Aeroporto)|(Sinalização)"), .after= nome) %>%
      mutate(obra = str_remove_all(obra, "-|\\s")) %>%
      filter(!is.na(obra)) %>%
      mutate(obra = if_else(str_detect(obra, "^\\d$"), paste0(obra, "DRO") , obra)) %>%
      mutate(obra_ascii = stringi::stri_trans_general(obra, id = "latin-ascii"), .after= obra)   %>%
      rename(obra_nome = nome) %>%
      rename(id = `_id`, contrato_asis = details_numeroContrato) %>%
      relocate(contrato_asis, .after = obra_ascii)
    
  }, silent = TRUE)
  
  # Retornar NULL se ocorrer um erro
  if (inherits(base, "try-error")) {
    base <- NULL
    message("Erro ao processar o dados de relatorios do Diario de Obras. Retornando NULL.")
  }
  
  return(base)
}


#' Processa Dados de Medições do Plug
#'
#' Lê e processa os dados de medições disponíveis na base do sistema Plug.
#'
#' @details
#' Essa função baixa os dados usando o pacote `plug` e aplica transformações como:
#' conversão de datas, limpeza de colunas e conversão de valores numéricos.
#'
#' @return Uma tibble com os dados de contratos do Plug processados ou `NULL` em caso de erro.
#' @examples
#' plug_contratos <- processamento_plug_contratos()
#' @export
processamento_plug_medicoes <- function() {
  # Simula o processamento e retorna NULL ou uma tibble
  message("Executando processamento de medições do Plug.")
  
  # Tentar carregar e processar o arquivo
  base <- try({
    
    plug_raw <- plug_download_base(base_name = 'VW_Medicoes_View_Exp')
    
    plug_raw %>%
      janitor::clean_names() %>%
      mutate(
        data_inicial = dmy_hms(data_inicial),
        data_final = dmy_hms(data_final),
        valor_medido_r = as.numeric(str_replace(valor_medido_r, ",", ".")),
        valor_medido_reajustado_r = as.numeric(str_replace(valor_medido_reajustado_r, ",", ".")),
        boletim_da_medicao_no_do_bm = as.integer(boletim_da_medicao_no_do_bm, ",", ".")
      ) %>%
      mutate(across(where(is.character), \(x) if_else(x == "", NA_character_, str_trim(x))))
    
  }, silent = TRUE)
  
  # Retornar NULL se ocorrer um erro
  if (inherits(base, "try-error")) {
    base <- NULL
    message("Erro ao processar o dados de medições do Plug. Retornando NULL.")
  }
  
  return(base)
}


################################### EXECUÇÃO ###################################


# Exemplo de uso da função principal
pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "sepe",
  host = "10.238.107.182",
  port = "5432",
  user = "sepe_adm",
  password = "!sepe_adm!",
  minSize = 4,
  idleTimeout = 60 * 60
)

etl_config <- tibble::tribble(
  ~nome_base, ~funcao_processamento, ~caminho_parquet, ~caminho_qs, ~tabela_destino, ~colunas_distribuicao,
  "Contratos no Plug",
  "processamento_plug_contratos",
  "data/mulungu/plug_contratos.pqt",
  "app_mulungu/data/df_contratos_plug.qs",
  "plug_contratos",
  NULL,
  "Medições no Plug",
  "processamento_plug_medicoes",
  "data/mulungu/plug_medicoes.pqt",
  "app_mulungu/data/df_medicoes_plug.qs",
  "plug_medicoes",
  NULL,
  "Relatórios do Diario de Obras",
  "processamento_diario_relatorios",
  "data/mulungu/diariodeobras_relatorios.pqt",
  "app_mulungu/data/df_relatorios_diario.qs",
  "diariodeobras_relatorios",
  c("contrato", "contrato_obra", "contrato_supervisao")
)

##### Execução do ETL
for (i in 1:nrow(etl_config)) {
  processar_e_enviar_base(
    nome_base = etl_config$nome_base[i],
    funcao_processamento = base::get(etl_config$funcao_processamento[i]),
    caminho_parquet = etl_config$caminho_parquet[i],
    caminho_qs = etl_config$caminho_qs[i],
    pool = pool,
    tabela_destino = etl_config$tabela_destino[i],
    colunas_distribuicao = etl_config$colunas_distribuicao[i] %>% pluck(1)
  )
}


# Fechar pool após uso
pool::poolClose(pool)

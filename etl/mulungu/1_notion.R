#notion_key <- "secret_bkUAXm9I0u3xeY1hjckKbi5CzAig54XiTBQYRAD8pUm"
#notion_iniciativas <- "a19ce1ff6027441e957531e2d91fc489"
#notion_licitacoes <- "a02d8d05a87d484584afc1afd24327ea"
#notion_obras <- "eed382976b5b49a4bb18cf27c9509a0b"
#notion_projetos <- "e97ad692fed14557863583188ab3c9eb" # Não funciona

notion_key <- "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"

# iniciativas (NOVO) - Felipe Ferreira
notion_iniciativas <- "26d9d6905fd781cd8cbfc58d14de3493"

# licitações
notion_licitacoes <- "26d9d6905fd78170a307cccf672800c5"

# obras
notion_obras <- "26d9d6905fd781fcac5ac9a4bf1e0a74"

# portfólio GEP
notion_projetos <- "26d9d6905fd7810b9656dfdc59e7685c"

# Obras: https://www.notion.so/felipe-ferreira/26d9d6905fd781fcac5ac9a4bf1e0a74?v=26d9d6905fd7816694c9000c82c51f8e&source=copy_link
# Licitações: https://www.notion.so/felipe-ferreira/26d9d6905fd78170a307cccf672800c5?v=26d9d6905fd7814f8674000ccff9503a&source=copy_link
# Projetos: https://www.notion.so/felipe-ferreira/26d9d6905fd7810b9656dfdc59e7685c?v=26d9d6905fd7816580da000c9d4a345b&source=copy_link
# INICIATIVAS (NOVO) - Felipe Ferreira: https://www.notion.so/felipe-ferreira/26d9d6905fd781cd8cbfc58d14de3493?v=26d9d6905fd78129b70a000c7db20d91&source=copy_link
# SRE (NOVO) - Felipe Ferreira: https://www.notion.so/felipe-ferreira/26d9d6905fd781b0b5cdcb71bc470432?v=26d9d6905fd7816a951b000c36cd76eb&source=copy_link

library(tidyverse)
library(httr2)
library(furrr)
library(plug)

# Carregamento centralizado de caminhos
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../../R/paths.R")) source("../../R/paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")
library(scales)
library(glue)
library(htmltools)


# Define o plano para processamento paralelo (ajuste conforme seu sistema)
plan(multisession)

next_req <- function(resp, req) {
  cursor <- resp_body_json(resp)$next_cursor
  if (is.null(cursor))
    return(NULL)
  req |> req_body_json(list(page_size = 100, start_cursor = cursor))
}

# Função auxiliar para extrair multi_select
extrair_multi <- function(prop) {
  ms <- pluck(prop, "multi_select", .default = list())
  if (length(ms) > 0) {
    paste(map_chr(ms, "name"), collapse = ", ")
  } else {
    NA_character_
  }
}

# Função auxiliar para extrair formula do tipo number
extrair_formula <- function(prop) {
  f <- pluck(prop, "formula", .default = NULL)
  if (!is.null(f) && pluck(f, "type", .default = NA_character_) == "number") {
    as.character(pluck(f, "number", .default = NA_real_))
  } else {
    NA_character_
  }
}

# Função principal utilizando case_when e pluck para extrair o valor como character
extrair_valor <- function(prop) {
  case_when(
    prop$type == "rich_text" ~ pluck(prop, "rich_text", 1, "plain_text", .default = NA_character_),
    prop$type == "number"    ~ as.character(pluck(prop, "number", .default = NA_real_)),
    prop$type == "date"      ~ paste0(na.omit(as.character(pluck(prop, "date", .default = NA_character_))), collapse = ""),
    prop$type == "select"    ~ pluck(prop, "select", "name", .default = NA_character_),
    prop$type == "multi_select" ~ extrair_multi(prop),
    prop$type == "status"    ~ paste0(pluck(prop, "status", "name", .default = NA_character_), "__", pluck(prop, "status", "color", .default = NA_character_)),
    prop$type == "formula"   ~ extrair_formula(prop),
    prop$type == "last_edited_by" ~ pluck(prop, "last_edited_by", "id", .default = NA_character_),
    prop$type == "title"     ~ pluck(prop, "title", 1, "plain_text", .default = NA_character_),
    TRUE ~ NA_character_
  )
}

extrair_props_item <- function(item) {
  props <- item$properties
  map(props, extrair_valor) 
}

extrair_props_item <- function(item) {
  props <- item$properties
  # Extrai os valores das propriedades em uma lista nomeada
  valores <- map(props, extrair_valor)
  # Converte a lista em uma tibble com 1 linha
  as_tibble(valores, .name_repair = "unique")
}

get_id_data <- function(database_id, notion_key) {
  url <- paste0("https://api.notion.com/v1/databases/", database_id, "/query")
  req <- request(url) %>%
    req_method("POST") %>%
    req_headers(
      "Notion-Version" = "2022-06-28",
      "Authorization" = paste("Bearer", notion_key),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(list(page_size = 100))
  
  resps <- req_perform_iterative(req, next_req = next_req)
  
  resps |> resps_data(function(resp) {
    resp |> 
      resp_body_json() |> 
      pluck("results") |> 
      future_map_dfr(extrair_props_item)
  })
}

# teste <- get_id_data(notion_obras, notion_key) 
# teste %>% 
#   filter(!is.na(`Observações SEMOBI`)) %>% 
#   select(observacoes = `Observações SEMOBI`) %>% 
#   mutate(observacoes = str_replace(observacoes, "(\\d{2}/\\d{2}/)(\\d{2}\\s)","\\120\\2"))
#
# teste %>% count(ritmo = `RITMO DA OBRA`) %>% 
#   mutate(ritmo = str_to_sentence(ritmo))


# teste %>%
#   mutate(across(everything(), ~ if_else(.x == "" | .x == "-", NA_character_, .x))) %>% 
#   select(rodovia = RODOVIA,
#          contrato_projeto_plug = `CT PROJETO`,
#          contrato_obra_plug = `CT CONSTRUTORA`,
#          contrato_supervisao_plug = `CT SUPERVISÃO`,
#          status = STATUS,
#          inicio_execucao = `INÍCIO EXECUÇÃO (Real)`,  #
#          termino_execucao = `TERMINO EXECUÇÃO PREVISTO`,
#          # #avanco_fisico = `AVANÇO FÍSICO (PAINEL)`,
#          #avanco_fisico = `AVANÇO FÍSICO (PAINEL) - MENSAL`,
#          inauguracao = INAUGURAÇÃO,
#          observacoes = `OBSERVAÇÕES SEMOBI`,
#          revestimento = `TIPO DE PAVIMENTO`,
#          tipo = `TIPO DE INTERVENÇÃO`,
#          ritmo = `RITMO DA OBRA`
#   )

obras_notion <- get_id_data(notion_obras, notion_key) %>% 
  mutate(across(everything(), ~ if_else(.x == "" | .x == "-", NA_character_, .x))) %>% 
  select(rodovia = Rodovia,
         contrato_projeto_plug = `CT PROJETO`,
         contrato_obra_plug = `CT CONSTRUTORA`,
         contrato_supervisao_plug = `CT SUPERVISÃO`,
         status = STATUS,
         inicio_execucao = `INÍCIO EXECUÇÃO (Real)`,  #
         termino_execucao = `TERMINO EXECUÇÃO PREVISTO`,
         #avanco_fisico = `AVANÇO FÍSICO (PAINEL)`,
         #avanco_fisico = `AVANÇO FÍSICO (PAINEL) - MENSAL`,
         avanco_fisico = `AVANÇO FÍSICO (PLUG)`,
         inauguracao = INAUGURAÇÃO,
         observacoes = JUSTIFICATIVA,
         #revestimento = `TIPO DE PAVIMENTO`,
         revestimento = `A - TIPO DE PAVIMENTO (ANTES DA INTERVENÇÃO)`,
         tipo = `TIPO DE INTERVENÇÃO`,
         ritmo = `RITMO DA OBRA`
         ) %>% 
  filter(!is.na(rodovia)) %>% 
  mutate(observacoes = str_replace(observacoes, "(\\d{2}/\\d{2}/)(\\d{2}\\s)","\\120\\2")) %>% 
  mutate(ritmo = str_to_sentence(ritmo)) %>% 
  mutate(status = case_when(
    status == "01 - CONCLUÍDO" ~ "Concluída",
    status == "02 - EM ANDAMENTO" ~ "Em Andamento",
    status == "03 - A INICIAR" ~ "Obra a contratar",
    status == "CONCLUÍDO GA" ~ "Concluída",
    status == "PARALISADA" ~ "Paralisada",
    .default = status
  )) %>% 
  mutate(
    inauguracao = if_else(inauguracao == "-", NA_character_, inauguracao),
    inauguracao = str_to_sentence(inauguracao),
    status = str_remove(status, "^[0-9]{2}-"),
    status = str_to_sentence(status)) %>% 
  mutate(status = if_else(str_detect(status, "^Conclu"), "Concluída", status)) %>% 
  mutate(termino_execucao = if_else(str_detect(termino_execucao, "\\d{4}.\\d{2}.\\d{2}"), paste0(str_extract(termino_execucao, "(?<=[\\-,/])\\d{2}[\\-,/]"), str_extract(termino_execucao, "^\\d{4}")), termino_execucao)) %>% 
  mutate(termino_execucao = if_else(str_detect(termino_execucao, "\\w{3}-\\d{2}"), str_extract(termino_execucao, "\\w{3}-\\d{2}$"), termino_execucao)) %>% 
  mutate(termino_execucao = if_else(str_detect(termino_execucao, "\\w{3}-\\d{2}"), str_replace(termino_execucao, "-", "/"), termino_execucao)) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "/2(\\d)$", "/202\\1")) %>% 
  mutate(rodovia = str_trim(rodovia) %>% toupper()) %>% 
  mutate(termino_execucao = str_remove_all(termino_execucao, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  mutate(termino_execucao = str_to_lower(termino_execucao)) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(janeiro)|(january)|(jan)", "01")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(março)|(march)|(mar)", "03")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(junho)|(june)|(jun)", "06")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(julho)|(july)|(jul)", "07")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(novembro)|(november)|(nov)", "11")) %>% 
  mutate(termino_execucao = str_replace(termino_execucao, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  mutate(termino_execucao = if_else(str_detect(termino_execucao, "^\\d{2}$"), paste0(termino_execucao, "/", year(today())), termino_execucao)) %>% 
  mutate(termino_execucao = if_else(str_length(termino_execucao) == 7, paste0("01/", termino_execucao), termino_execucao)) %>% 
  mutate(termino_execucao = if_else(str_length(termino_execucao) != 10, NA_character_, termino_execucao)) %>% 
  mutate(inicio_execucao = if_else(str_detect(inicio_execucao, "\\d{4}.\\d{2}.\\d{2}"), paste0(str_extract(inicio_execucao, "(?<=[\\-,/])\\d{2}[\\-,/]"), str_extract(inicio_execucao, "^\\d{4}")), inicio_execucao)) %>% 
  mutate(inicio_execucao = if_else(str_detect(inicio_execucao, "\\w{3}-\\d{2}"), str_extract(inicio_execucao, "\\w{3}-\\d{2}$"), inicio_execucao)) %>% 
  mutate(inicio_execucao = if_else(str_detect(inicio_execucao, "\\w{3}-\\d{2}"), str_replace(inicio_execucao, "-", "/"), inicio_execucao)) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "/2(\\d)$", "/202\\1")) %>% 
  mutate(inicio_execucao = str_remove_all(inicio_execucao, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  mutate(inicio_execucao = str_to_lower(inicio_execucao)) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(janeiro)|(january)|(jan)", "01")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(março)|(march)|(mar)", "03")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(junho)|(june)|(jun)", "06")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(julho)|(july)|(jul)", "07")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(novembro)|(november)|(nov)", "11")) %>% 
  mutate(inicio_execucao = str_replace(inicio_execucao, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  mutate(inicio_execucao = if_else(str_detect(inicio_execucao, "^\\d{2}$"), paste0(inicio_execucao, "/", year(today())), inicio_execucao)) %>% 
  mutate(inicio_execucao = if_else(str_length(inicio_execucao) == 7, paste0("01/", inicio_execucao), inicio_execucao)) %>% 
  mutate(inicio_execucao = if_else(str_length(inicio_execucao) != 10, NA_character_, inicio_execucao)) %>% 
  mutate(inicio_execucao = dmy(inicio_execucao)) %>% 
  mutate(termino_execucao = dmy(termino_execucao)) %>% 
  mutate(inauguracao = if_else(inauguracao == "Parcialmente concluída", "Inaugurada Parcialmente", inauguracao)) %>% 
  mutate(tipo = if_else(is.na(tipo), "S/I", tipo))

#obras_notion %>% count(inauguracao)

write_rds(obras_notion, file.path(DIR_RAW_MULUNGU, "obras_notion.rds"))


licitacoes_notion <- get_id_data(notion_licitacoes, notion_key)
#projetos_notion <- get_id_data(notion_projetos, notion_key)
#iniciativas_notion <- get_id_data(notion_iniciativas, notion_key)

#licitacoes_notion %>% glimpse()

licitacoes_notion_clean <- licitacoes_notion %>% 
  select(
    #rodovia = `RODOVIA/LOCAL`,
    rodovia = RODOVIA,
    local_atual = `LOCAL ATUAL`,
    prioridade = PRIORIDADE,
    status = STATUS,
    observacoes = `OBSERVAÇÕES / HISTÓRICO`,
    #historico = `HISTÓRICO DE PROCESSAMENTO`,
    processo_sei = `PROCESSO SEI`,
    #previsao_entrega = `PREVISÃO ENTREGA`,
    #valor_vencedor = `VALOR VENCEDOR`,
    valor_vencedor = `VALOR LICITADO`,
    objeto = OBJETO,
    #valor_licitacao = `VALOR LICITAÇÃO`,
    valor_licitacao = `VALOR REFERENCIAL`,
    #publicacao = PUBLICAÇÃO,
    #acao = AÇÃO, 
    setor = TIPO,
    #item = ITEM,
    extensao = EXT,
    responsavel = RESPONSÁVEL,
    modalidade = Modalidade,
    #matriz_risco = `Precisa de Matriz de Riscos? `,
    envio_sad = `ENVIO PARA SAD`,
    devolucao_der = `DEVOLUÇÃO P/ DER`
    #dias_corridos_sad = `Dias Corridos\n(Parado SAD)`,
    #dias_corridos_der = `Dias Corridos\n(Parado DER)`,
    #combranca_sad = `Cobrança SAD`,
    #limite_publicacao = `Prev. Publicação`,
    #limite_homologacao = `Prev. Homologação`,
  ) %>% 
  filter(!is.na(rodovia)) %>% 
  mutate(across(where(is.character), ~ str_remove_all(.x, "NULL"))) %>% 
  mutate(across(where(is.character), ~ if_else(.x == "", NA_character_, .x))) %>% 
  # mutate(limite_publicacao = if_else(str_detect(limite_publicacao, "\\d{4}.\\d{2}.\\d{2}"), paste0(str_extract(limite_publicacao, "(?<=[\\-,/])\\d{2}[\\-,/]"), str_extract(limite_publicacao, "^\\d{4}")), limite_publicacao)) %>% 
  # mutate(limite_publicacao = if_else(str_detect(limite_publicacao, "\\w{3}-\\d{2}"), str_extract(limite_publicacao, "\\w{3}-\\d{2}$"), limite_publicacao)) %>% 
  # mutate(limite_publicacao = if_else(str_detect(limite_publicacao, "\\w{3}-\\d{2}"), str_replace(limite_publicacao, "-", "/"), limite_publicacao)) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "/2(\\d)$", "/202\\1")) %>% 
  # mutate(limite_publicacao = str_remove_all(limite_publicacao, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  # mutate(limite_publicacao = str_to_lower(limite_publicacao)) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(janeiro)|(january)|(jan)", "01")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(março)|(march)|(mar)", "03")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(junho)|(june)|(jun)", "06")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(julho)|(july)|(jul)", "07")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(novembro)|(november)|(nov)", "11")) %>% 
  # mutate(limite_publicacao = str_replace(limite_publicacao, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  # mutate(limite_publicacao = if_else(str_detect(limite_publicacao, "^\\d{2}$"), paste0(limite_publicacao, "/", year(today())), limite_publicacao)) %>% 
  # mutate(limite_publicacao = if_else(str_length(limite_publicacao) == 7, paste0("01/", limite_publicacao), limite_publicacao)) %>% 
  # mutate(limite_publicacao = if_else(str_length(limite_publicacao) != 10, NA_character_, limite_publicacao)) %>% 
  # mutate(limite_publicacao = dmy(limite_publicacao)) %>% 
  # mutate(limite_homologacao = if_else(str_detect(limite_homologacao, "\\d{4}.\\d{2}.\\d{2}"), paste0(str_extract(limite_homologacao, "(?<=[\\-,/])\\d{2}[\\-,/]"), str_extract(limite_homologacao, "^\\d{4}")), limite_homologacao)) %>% 
  # mutate(limite_homologacao = if_else(str_detect(limite_homologacao, "\\w{3}-\\d{2}"), str_extract(limite_homologacao, "\\w{3}-\\d{2}$"), limite_homologacao)) %>% 
  # mutate(limite_homologacao = if_else(str_detect(limite_homologacao, "\\w{3}-\\d{2}"), str_replace(limite_homologacao, "-", "/"), limite_homologacao)) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "/2(\\d)$", "/202\\1")) %>% 
  # mutate(limite_homologacao = str_remove_all(limite_homologacao, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  # mutate(limite_homologacao = str_to_lower(limite_homologacao)) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(janeiro)|(january)|(jan)", "01")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(março)|(march)|(mar)", "03")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(junho)|(june)|(jun)", "06")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(julho)|(july)|(jul)", "07")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(novembro)|(november)|(nov)", "11")) %>% 
  # mutate(limite_homologacao = str_replace(limite_homologacao, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  # mutate(limite_homologacao = if_else(str_detect(limite_homologacao, "^\\d{2}$"), paste0(limite_homologacao, "/", year(today())), limite_homologacao)) %>% 
  # mutate(limite_homologacao = if_else(str_length(limite_homologacao) == 7, paste0("01/", limite_homologacao), limite_homologacao)) %>% 
  # mutate(limite_homologacao = if_else(str_length(limite_homologacao) != 10, NA_character_, limite_homologacao)) %>% 
  # mutate(limite_homologacao = dmy(limite_homologacao)) %>% 
  # mutate(previsao_entrega = ymd(previsao_entrega)) %>% 
  mutate(devolucao_der = ymd(devolucao_der)) %>% 
  #mutate(publicacao = ymd(publicacao)) %>% 
  mutate(envio_sad = ymd(envio_sad)) %>% 
  mutate(setor = str_to_title(setor)) %>% 
  mutate(valor_vencedor = str_remove_all(valor_vencedor, "(\\s*\\(.*\\)\\s*)|(R\\$\\s*)|\n|\\s")) %>% 
  mutate(valor_vencedor = str_remove_all(valor_vencedor, "\\.")) %>% 
  mutate(valor_vencedor = if_else(valor_vencedor == "", NA_character_, valor_vencedor)) %>% 
  mutate(valor_vencedor = as.numeric(str_replace(valor_vencedor, ",", "."))) %>% 
  mutate(valor_licitacao = str_remove_all(valor_licitacao, "(\\s*\\(.*\\)\\s*)|(R\\$\\s*)|\n|\\s")) %>% 
  mutate(valor_licitacao = str_remove_all(valor_licitacao, "\\.")) %>% 
  mutate(valor_licitacao = if_else(valor_licitacao == "", NA_character_, valor_licitacao)) %>% 
  mutate(valor_licitacao = as.numeric(str_replace(valor_licitacao, ",", "."))) %>% 
  mutate(valor_licitacao = as.numeric(str_replace(valor_licitacao, ",", "."))) %>% 
  mutate(extensao = as.numeric(extensao))

write_rds(licitacoes_notion_clean, file.path(DIR_MULUNGU, "licitacoes_notion.rds"))

projetos_notion <- get_id_data(notion_projetos, notion_key)

projetos_notion_clean <- projetos_notion %>% 
  select(
    rodovia = Rodovia,
    contrato_plug = `Nº Contrato`,
    valor_projeto = `Valor do Projeto`,
    empresa_projeto = Empresa,
    extensao_projeto = `Extensão (KM)`,
    gestao_projeto = `Gestão.`,
    trecho_projeto = Trecho,
    valor_obra = `Valor da Obra`,
    database = `Data-base`,
    prazo_conclusão = `Prazo de Conclusão`,
    status = Status
  ) %>% 
  separate_wider_delim(status, delim = "__", names = c("status", "status_cor")) %>% 
  mutate(status = str_to_sentence(status)) %>% 
  mutate(status_cor = col2hcl(status_cor)) %>% 
  mutate(rodovia = str_remove_all(rodovia, "-(?=\\d{3})")) %>% 
  mutate(database = if_else(str_detect(database, "\\d{4}.\\d{2}.\\d{2}"), paste0(str_extract(database, "(?<=[\\-,/])\\d{2}[\\-,/]"), str_extract(database, "^\\d{4}")), database)) %>% 
  mutate(database = if_else(str_detect(database, "\\w{3}-\\d{2}"), str_extract(database, "\\w{3}-\\d{2}$"), database)) %>% 
  mutate(database = if_else(str_detect(database, "\\w{3}-\\d{2}"), str_replace(database, "-", "/"), database)) %>% 
  mutate(database = str_replace(database, "/2(\\d)$", "/202\\1")) %>% 
  mutate(database = str_remove_all(database, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  mutate(database = str_to_lower(database)) %>% 
  mutate(database = str_replace(database, "(janeiro)|(january)|(jan)", "01")) %>% 
  mutate(database = str_replace(database, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  mutate(database = str_replace(database, "(março)|(march)|(mar)", "03")) %>% 
  mutate(database = str_replace(database, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  mutate(database = str_replace(database, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  mutate(database = str_replace(database, "(junho)|(june)|(jun)", "06")) %>% 
  mutate(database = str_replace(database, "(julho)|(july)|(jul)", "07")) %>% 
  mutate(database = str_replace(database, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  mutate(database = str_replace(database, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  mutate(database = str_replace(database, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  mutate(database = str_replace(database, "(novembro)|(november)|(nov)", "11")) %>% 
  mutate(database = str_replace(database, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  mutate(database = if_else(str_detect(database, "^\\d{2}$"), paste0(database, "/", year(today())), database)) %>% 
  mutate(database = if_else(str_length(database) == 7, paste0("01/", database), database)) %>% 
  mutate(database = if_else(str_length(database) != 10, NA_character_, database)) %>% 
  mutate(database = dmy(database)) %>% 
  mutate(prazo_conclusão = if_else(str_detect(prazo_conclusão, "\\d{4}.\\d{2}.\\d{2}"), 
                                   paste0(str_extract(prazo_conclusão, "(?<=[\\-,/])\\d{2}[\\-,/]"), 
                                          str_extract(prazo_conclusão, "^\\d{4}")), 
                                   prazo_conclusão)) %>% 
  mutate(prazo_conclusão = if_else(str_detect(prazo_conclusão, "\\w{3}-\\d{2}"), 
                                   str_extract(prazo_conclusão, "\\w{3}-\\d{2}$"), 
                                   prazo_conclusão)) %>% 
  mutate(prazo_conclusão = if_else(str_detect(prazo_conclusão, "\\w{3}-\\d{2}"), 
                                   str_replace(prazo_conclusão, "-", "/"), 
                                   prazo_conclusão)) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "/2(\\d)$", "/202\\1")) %>% 
  mutate(prazo_conclusão = str_remove_all(prazo_conclusão, "(\", \")|(c\\(\")|(\"\\))")) %>% 
  mutate(prazo_conclusão = str_to_lower(prazo_conclusão)) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(janeiro)|(january)|(jan)", "01")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(fevereiro)|(february)|(fev)|(feb)", "02")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(março)|(march)|(mar)", "03")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(abril)|(april)|(abr)|(apr)", "04")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(maio)|(may)|(may)|(mai)", "05")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(junho)|(june)|(jun)", "06")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(julho)|(july)|(jul)", "07")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(agosto)|(august)|(ago)|(aug)", "08")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(setembro)|(september)|(set)|(sep)", "09")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(outubro)|(october)|(out)|(oct)", "10")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(novembro)|(november)|(nov)", "11")) %>% 
  mutate(prazo_conclusão = str_replace(prazo_conclusão, "(dezembro)|(december)|(dez)|(dec)", "12")) %>% 
  mutate(prazo_conclusão = if_else(str_detect(prazo_conclusão, "^\\d{2}$"), paste0(prazo_conclusão, "/", year(today())), prazo_conclusão)) %>% 
  mutate(prazo_conclusão = if_else(str_length(prazo_conclusão) == 7, paste0("01/", prazo_conclusão), prazo_conclusão)) %>% 
  mutate(prazo_conclusão = if_else(str_length(prazo_conclusão) != 10, NA_character_, prazo_conclusão)) %>% 
  mutate(prazo_conclusão = dmy(prazo_conclusão)) %>% 
  mutate(extensao_projeto = as.numeric(extensao_projeto)) %>% 
  mutate(valor_obra = as.numeric(valor_obra)) %>% 
  mutate(valor_projeto = as.numeric(valor_projeto)) %>% 
  rowwise() %>% 
  #mutate(status_badge = vialactea::tag_svg(value = status, color = status_cor)) %>% 
  mutate(status_badge = if_else(
      is.na(status), 
        "–",
      as.character(vialactea::tagBadge(
        HTML(paste0(vialactea::status_badge(color = paste0("color:black;background: color-mix(in hsl, ", status_cor, " 75%, #000000 25%);"), style = "margin-right:5px;"), status)), 
           NULL, status = "", style = paste0("color:black;background: color-mix(in hsl, ", status_cor, "50 95%, #000000 5%);"))
        )
    )
    ) %>% 
  ungroup()

write_rds(projetos_notion_clean, file.path(DIR_MULUNGU, "projetos_notion.rds"))

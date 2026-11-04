library(httr2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# ---------------------------------------------------------
# 1. Configurações e Credenciais (.Renviron)
# ---------------------------------------------------------
CHAVE_API <- Sys.getenv("API_CGU_TOKEN")

if (!nzchar(CHAVE_API)) {
  stop("Variável 'API_CGU_TOKEN' não configurada no .Renviron.")
}

ANO_BASE <- 2026
BASE_URL <- "https://api.portaldatransparencia.gov.br/api-de-dados"

# ---------------------------------------------------------
# 2. Funções de Consulta à API
# ---------------------------------------------------------

# Consulta uma página do endpoint macro de emendas
get_pagina_emendas <- function(pagina, ano, chave) {
  req <- request(paste0(BASE_URL, "/emendas")) |>
    req_headers(
      "chave-api-dados" = chave,
      "accept" = "application/json"
    ) |>
    req_url_query(
      ano = ano,
      pagina = pagina
    ) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  status <- resp_status(resp)
  
  if (status == 200) {
    dados <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(dados) || length(dados) == 0 || (is.data.frame(dados) && nrow(dados) == 0)) {
      return(NULL)
    }
    return(as_tibble(dados))
  } else if (status == 429) {
    message("Rate limit atingido. Aguardando 5 segundos...")
    Sys.sleep(5)
    return(get_pagina_emendas(pagina, ano, chave))
  } else {
    warning(paste("Erro na página", pagina, "- Código HTTP:", status))
    return(NULL)
  }
}

# Consulta os documentos contábeis (favorecido, CNPJ, histórico do objeto) de uma emenda
get_documentos_emenda <- function(codigo_emenda, chave) {
  req <- request(paste0(BASE_URL, "/emendas/documentos")) |>
    req_headers(
      "chave-api-dados" = chave,
      "accept" = "application/json"
    ) |>
    req_url_query(
      codigoEmenda = codigo_emenda,
      pagina = 1
    ) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    dados <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(dados) > 0 && is.data.frame(dados)) {
      return(as_tibble(dados))
    }
  }
  return(tibble())
}

# ---------------------------------------------------------
# 3. Extração das Emendas de Pernambuco (Loop de Paginação)
# ---------------------------------------------------------
cat("1/3 - Iniciando varredura das emendas de Pernambuco para", ANO_BASE, "...\n")

lista_emendas_pe <- list()
pagina_atual <- 1
total_pe <- 0

repeat {
  dados_pagina <- get_pagina_emendas(pagina = pagina_atual, ano = ANO_BASE, chave = CHAVE_API)
  
  if (is.null(dados_pagina) || nrow(dados_pagina) == 0) {
    cat(sprintf("\nFim das páginas alcançado (%d páginas analisadas).\n", pagina_atual - 1))
    break
  }
  
  emendas_pe_pagina <- dados_pagina |>
    filter(
      str_detect(localidadeDoGasto, regex("PERNAMBUCO| - PE|\\(PE\\)", ignore_case = TRUE)) |
        str_detect(nomeAutor, regex("BANCADA DE PERNAMBUCO", ignore_case = TRUE))
    )
  
  if (nrow(emendas_pe_pagina) > 0) {
    lista_emendas_pe[[length(lista_emendas_pe) + 1]] <- emendas_pe_pagina
    total_pe <- total_pe + nrow(emendas_pe_pagina)
  }
  
  if (pagina_atual %% 25 == 0) {
    cat(sprintf("Página %d consultada (%d emendas de PE mapeadas)...\n", pagina_atual, total_pe))
  }
  
  pagina_atual <- pagina_atual + 1
  Sys.sleep(0.2)
}

df_emendas_pe <- bind_rows(lista_emendas_pe)

# Converte colunas monetárias de string para numérico (ex: "57.400,00" -> 57400.00)
df_emendas_pe <- df_emendas_pe |>
  mutate(across(
    starts_with("valor"),
    ~ parse_number(.x, locale = locale(decimal_mark = ",", grouping_mark = "."))
  ))

cat(sprintf("Total de emendas de PE encontradas: %d\n", nrow(df_emendas_pe)))

# ---------------------------------------------------------
# 4. Enriquecimento: Favorecido, CNPJ e Objeto (Documentos)
# ---------------------------------------------------------
cat("2/3 - Buscando documentos de execução (Favorecido, CNPJ, Objeto)...\n")

# Filtra emendas que possuem execução financeira
emendas_com_execucao <- df_emendas_pe |>
  filter(valorEmpenhado > 0 | valorPago > 0)

documentos_totais <- list()

if (nrow(emendas_com_execucao) > 0) {
  for (i in seq_len(nrow(emendas_com_execucao))) {
    cod <- emendas_com_execucao$codigoEmenda[i]
    docs <- get_documentos_emenda(cod, chave = CHAVE_API)
    
    if (nrow(docs) > 0) {
      documentos_totais[[length(documentos_totais) + 1]] <- docs |> mutate(codigoEmenda = cod)
    }
    
    if (i %% 20 == 0 || i == nrow(emendas_com_execucao)) {
      cat(sprintf("Documentos consultados: %d de %d\n", i, nrow(emendas_com_execucao)))
    }
    
    Sys.sleep(0.2)
  }
}

df_documentos <- bind_rows(documentos_totais)

# ---------------------------------------------------------
# 5. Cruzamento Final e Exportação
# ---------------------------------------------------------
cat("3/3 - Cruzando bases e salvando resultados...\n")

if (nrow(df_documentos) > 0) {
  # Limpeza dos valores de documentos
  df_documentos_clean <- df_documentos |>
    mutate(
      valorDocumento = parse_number(as.character(valor), locale = locale(decimal_mark = ",", grouping_mark = "."))
    ) |>
    select(
      codigoEmenda,
      codigoDocumento,
      faseDocumento = fase,
      codigoFavorecido,   # CNPJ / CPF do recebedor
      nomeFavorecido = favorecido,
      valorDocumento,
      historicoObjeto = historico
    )
  
  df_final_detalhado <- df_emendas_pe |>
    left_join(df_documentos_clean, by = "codigoEmenda")
} else {
  # Caso o ano ainda não possua notas contábeis geradas
  df_final_detalhado <- df_emendas_pe |>
    mutate(
      codigoDocumento = NA_character_,
      faseDocumento = NA_character_,
      codigoFavorecido = NA_character_,
      nomeFavorecido = NA_character_,
      valorDocumento = NA_real_,
      historicoObjeto = NA_character_
    )
}

# Salva arquivo consolidado formatado para Excel em português (separador ';')
write_excel_csv2(df_final_detalhado, "emendas_pernambuco_2026_completo.csv")

cat("Processo finalizado com sucesso. Arquivo 'emendas_pernambuco_2026_completo.csv' gerado.\n")
glimpse(df_final_detalhado)
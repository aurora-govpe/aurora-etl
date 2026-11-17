# ==============================================================================
# SCRIPT: atualizar_gestores_brasilapi.R
# OBJETIVO: Cruzar chapa eleita no TSE 2024 (Titular + Vice) com o responsável
#           atual pelo CNPJ da Prefeitura na Receita Federal (via BrasilAPI)
#           para detectar sucessões/renúncias automaticamente em PE.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
  library(writexl)
})

# 1. Configuração de Diretórios (Padrão Aurora) ----
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

if (!exists("DIR_RAW_AURORA")) {
  DIR_RAW_AURORA <- "C:/dados/aurora/raw"
}

dir.create(file.path(DIR_RAW_AURORA, "ibge"), recursive = TRUE, showWarnings = FALSE)
caminho_saida      <- file.path(DIR_RAW_AURORA, "ibge/excecoes_gestores_pe.xlsx")
caminho_cache_cnpj <- file.path(DIR_RAW_AURORA, "ibge/cache_cnpjs_prefeituras_pe.rds")

# Funções auxiliares de normalização
normalizar_nome <- function(x) {
  x %>%
    toupper() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^A-Z ]", " ") %>%
    str_squish()
}

primeiro_ultimo_nome <- function(nome_norm) {
  partes <- str_split(nome_norm, " ")[[1]]
  partes <- partes[!partes %in% c("DE", "DA", "DO", "DAS", "DOS", "E")]
  if (length(partes) == 0) return("")
  if (length(partes) == 1) return(partes[1])
  paste(partes[1], partes[length(partes)])
}

# 2. Lista Oficial dos 185 Territórios de PE (IBGE) ----
message(">>> [1/4] Carregando municípios de Pernambuco (IBGE)...")
req_ibge <- request("https://servicodados.ibge.gov.br/api/v1/localidades/estados/26/municipios") %>%
  req_perform()

df_ibge <- map_dfr(resp_body_json(req_ibge), function(m) {
  tibble(
    cod_municipio = as.character(m$id),
    municipio     = as.character(m$nome)
  )
}) %>%
  arrange(municipio)

# 3. Extração da Chapa Completa TSE 2024 (Titular + Vice) ----
message(">>> [2/4] Extraindo Titulares e Vices eleitos no TSE 2024...")
req_tse_mun <- request("https://resultados.tse.jus.br/oficial/ele2024/619/config/mun-e000619-cm.json") %>%
  req_headers("User-Agent" = "Mozilla/5.0") %>%
  req_perform()

pe_tse_abr <- keep(resp_body_json(req_tse_mun)$abr, ~ .x$cd == "pe")[[1]]

df_tse_keys <- map_dfr(pe_tse_abr$mu, function(m) {
  tibble(
    cod_tse       = as.character(m$cd),
    cod_municipio = as.character(m$cdi)
  )
})

parse_tse_chapa <- function(resp) {
  vazio <- list(prefeito = NA_character_, partido = NA_character_,
                vice = NA_character_, partido_vice = NA_character_)
  if (is.null(resp) || resp_status(resp) != 200) return(vazio)
  
  json <- resp_body_json(resp)
  carg <- json$carg[[1]]
  candidatos <- list()
  
  for (agr in carg$agr) {
    for (par in agr$par) {
      for (cand in par$cand) {
        vice_info <- if (!is.null(cand$vs) && length(cand$vs) > 0) cand$vs[[1]] else NULL
        candidatos[[length(candidatos) + 1]] <- list(
          nome         = cand$nmu,
          partido      = par$sg,
          vice         = if (!is.null(vice_info)) vice_info$nmu else NA_character_,
          partido_vice = if (!is.null(vice_info)) vice_info$sgp else NA_character_,
          eleito       = !is.null(cand$e) && cand$e == "s",
          votos        = suppressWarnings(as.numeric(cand$vap))
        )
      }
    }
  }
  
  if (length(candidatos) == 0) return(vazio)
  eleitos <- keep(candidatos, ~ .x$eleito)
  escolhido <- if (length(eleitos) > 0) {
    eleitos[[1]]
  } else {
    ord <- order(map_dbl(candidatos, ~ ifelse(is.na(.x$votos), 0, .x$votos)), decreasing = TRUE)
    candidatos[[ord[1]]]
  }
  
  list(
    prefeito     = escolhido$nome,
    partido      = escolhido$partido,
    vice         = escolhido$vice,
    partido_vice = escolhido$partido_vice
  )
}

reqs_1t <- map(df_tse_keys$cod_tse, function(cd) {
  sprintf("https://resultados.tse.jus.br/oficial/ele2024/619/dados/pe/pe%s-c0011-e000619-u.json", cd) %>%
    request() %>%
    req_headers("User-Agent" = "Mozilla/5.0")
})

resps_1t  <- req_perform_parallel(reqs_1t, on_error = "continue", max_active = 10, progress = FALSE)
parsed_1t <- map(resps_1t, parse_tse_chapa)

df_tse_keys$prefeito_eleito_2024 <- map_chr(parsed_1t, "prefeito")
df_tse_keys$partido_eleito_2024  <- map_chr(parsed_1t, "partido")
df_tse_keys$vice_eleito_2024     <- map_chr(parsed_1t, "vice")
df_tse_keys$partido_vice_2024    <- map_chr(parsed_1t, "partido_vice")

# Atualiza 2º Turno (Olinda: 24910, Paulista: 25135)
for (cd_2t in c("24910", "25135")) {
  tryCatch({
    url_2t <- sprintf("https://resultados.tse.jus.br/oficial/ele2024/620/dados/pe/pe%s-c0011-e000620-u.json", cd_2t)
    r_2t <- request(url_2t) %>% req_headers("User-Agent" = "Mozilla/5.0") %>% req_perform()
    p_2t <- parse_tse_chapa(r_2t)
    idx <- which(df_tse_keys$cod_tse == cd_2t)
    if (length(idx) > 0 && !is.na(p_2t$prefeito)) {
      df_tse_keys$prefeito_eleito_2024[idx] <- p_2t$prefeito
      df_tse_keys$partido_eleito_2024[idx]  <- p_2t$partido
      df_tse_keys$vice_eleito_2024[idx]     <- p_2t$vice
      df_tse_keys$partido_vice_2024[idx]    <- p_2t$partido_vice
    }
  }, error = function(e) NULL)
}

# 4. Obtenção dos CNPJs das Prefeituras via API Aberta do Siconfi (Tesouro Nacional) ----
message(">>> [3/4] Obtendo CNPJs oficiais das 184 prefeituras (Siconfi/STN)...")

if (file.exists(caminho_cache_cnpj)) {
  df_cnpjs_pe <- readRDS(caminho_cache_cnpj)
} else {
  req_entes <- request("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/entes") %>%
    req_perform()
  entes_json <- resp_body_json(req_entes)$items
  
  df_cnpjs_pe <- map_dfr(entes_json, function(e) {
    tibble(
      cod_municipio = as.character(e$cod_ibge),
      uf            = as.character(e$uf),
      esfera        = as.character(e$esfera),
      cnpj          = str_pad(str_replace_all(as.character(e$cnpj), "\\D", ""), 14, pad = "0")
    )
  }) %>%
    filter(uf == "PE", esfera == "M", !is.na(cnpj), nchar(cnpj) == 14) %>%
    distinct(cod_municipio, .keep_all = TRUE)
  
  saveRDS(df_cnpjs_pe, caminho_cache_cnpj)
}

# 5. Consulta do Responsável Legal na Receita Federal via BrasilAPI ----
message(">>> [4/4] Consultando responsáveis ativos na Receita Federal via BrasilAPI (aguarde ~1 min)...")

consultar_responsavel_cnpj <- function(cnpj) {
  if (is.na(cnpj) || nchar(cnpj) != 14) return(NA_character_)
  
  url <- sprintf("https://brasilapi.com.br/api/cnpj/v1/%s", cnpj)
  tryCatch({
    resp <- request(url) %>%
      req_headers("User-Agent" = "aurora-etl") %>%
      req_retry(max_tries = 3, backoff = ~ 2) %>%
      req_perform()
    
    if (resp_status(resp) != 200) return(NA_character_)
    dados <- resp_body_json(resp)
    
    # Em órgãos públicos (103-1), o prefeito aparece no QSA como "Prefeito" (qualificação 05/16) ou primeiro sócio
    if (!is.null(dados$qsa) && length(dados$qsa) > 0) {
      nomes_qsa <- map_chr(dados$qsa, ~ ifelse(is.null(.x$nome_socio), "", as.character(.x$nome_socio)))
      nomes_qsa <- nomes_qsa[nomes_qsa != ""]
      if (length(nomes_qsa) > 0) return(nomes_qsa[1])
    }
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })
}

# Execução cadenciada (3 requisições/segundo) para evitar bloqueio HTTP 429 da BrasilAPI
df_cnpjs_pe$responsavel_rfb <- map_chr(seq_len(nrow(df_cnpjs_pe)), function(i) {
  Sys.sleep(0.35)
  consultar_responsavel_cnpj(df_cnpjs_pe$cnpj[i])
})

# 6. Cruzamento e Detecção Automática de Sucessão ----
df_consolidado <- df_ibge %>%
  left_join(df_tse_keys %>% select(-cod_tse), by = "cod_municipio") %>%
  left_join(df_cnpjs_pe %>% select(cod_municipio, cnpj, responsavel_rfb), by = "cod_municipio") %>%
  rowwise() %>%
  mutate(
    norm_titular = normalizar_nome(coalesce(prefeito_eleito_2024, "")),
    norm_vice    = normalizar_nome(coalesce(vice_eleito_2024, "")),
    norm_rfb     = normalizar_nome(coalesce(responsavel_rfb, "")),
    
    # Compara nome completo ou primeiro + último nome (ex: "JOAO HENRIQUE CAMPOS" vs "JOAO CAMPOS")
    bate_titular = norm_rfb != "" && (
      str_detect(norm_rfb, fixed(norm_titular)) ||
        str_detect(norm_titular, fixed(norm_rfb)) ||
        primeiro_ultimo_nome(norm_rfb) == primeiro_ultimo_nome(norm_titular)
    ),
    
    bate_vice = norm_rfb != "" && norm_vice != "" && (
      str_detect(norm_rfb, fixed(norm_vice)) ||
        str_detect(norm_vice, fixed(norm_rfb)) ||
        primeiro_ultimo_nome(norm_rfb) == primeiro_ultimo_nome(norm_vice)
    ),
    
    status_deteccao = case_when(
      cod_municipio == "2605459"               ~ "DISTRITO_ESTADUAL",
      bate_vice & !bate_titular                ~ "VICE_ASSUMIU_RFB",
      !bate_titular & !bate_vice & norm_rfb != "" ~ "DIVERGENTE_RFB",
      TRUE                                     ~ "TITULAR_MANTIDO"
    )
  ) %>%
  ungroup()

# 7. Camada de Overrides Garantidos + Montagem da Planilha Final ----
# Garante Recife (Victor Marques) mesmo que o contador ainda não tenha atualizado o DBE na Receita
df_final_excel <- df_consolidado %>%
  mutate(
    prefeito_em_exercicio = case_when(
      cod_municipio == "2605459"          ~ "ADMINISTRAÇÃO ESTADUAL",
      cod_municipio == "2611606"          ~ coalesce(vice_eleito_2024, "VICTOR MARQUES"),
      status_deteccao == "VICE_ASSUMIU_RFB" ~ vice_eleito_2024,
      status_deteccao == "DIVERGENTE_RFB"   ~ responsavel_rfb,
      TRUE                                ~ prefeito_eleito_2024
    ),
    partido_em_exercicio = case_when(
      cod_municipio == "2605459"          ~ "GOVERNO DE PE",
      cod_municipio == "2611606"          ~ coalesce(partido_vice_2024, "PCdoB"),
      status_deteccao == "VICE_ASSUMIU_RFB" ~ partido_vice_2024,
      status_deteccao == "DIVERGENTE_RFB"   ~ "VERIFICAR",
      TRUE                                ~ partido_eleito_2024
    ),
    motivo_alteracao = case_when(
      cod_municipio == "2605459"          ~ "Distrito Estadual (sem eleição municipal)",
      cod_municipio == "2611606"          ~ "Vice assumiu (Renúncia titular 2026)",
      status_deteccao == "VICE_ASSUMIU_RFB" ~ "Detectado automaticamente via CNPJ/RFB (Vice assumiu)",
      status_deteccao == "DIVERGENTE_RFB"   ~ "REVISAR: Responsável na RFB difere do titular e do vice",
      TRUE                                ~ "Titular eleito em 2024"
    ),
    ativo = case_when(
      cod_municipio %in% c("2605459", "2611606") ~ "sim",
      status_deteccao == "VICE_ASSUMIU_RFB"      ~ "sim",
      TRUE                                       ~ "nao"
    )
  ) %>%
  select(
    cod_municipio,
    municipio,
    cnpj_prefeitura = cnpj,
    prefeito_eleito_2024,
    partido_eleito_2024,
    vice_eleito_2024,
    partido_vice_2024,
    responsavel_receita_federal = responsavel_rfb,
    prefeito_em_exercicio,
    partido_em_exercicio,
    motivo_alteracao,
    ativo
  )

# 8. Gravação e Resumo no Console ----
writexl::write_xlsx(df_final_excel, caminho_saida)

message("=================================================================")
message(sprintf("Planilha salva em: %s", caminho_saida))
message(sprintf("-> Substituições ativadas automaticamente (ativo = 'sim'): %d", sum(df_final_excel$ativo == "sim")))
message(sprintf("-> Casos sinalizados para revisão manual (DIVERGENTE_RFB): %d", sum(str_detect(df_final_excel$motivo_alteracao, "^REVISAR"))))
message("=================================================================")
# ==============================================================================
# PIPELINE IVCM (INSTITUTO VOTORANTIM) 2023 / 2024
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(openxlsx)
  library(arrow)
  library(V8)
})

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ---- 1. CONFIGURAÇÕES E DIRETÓRIOS ----

## ---- 1.1 Variáveis e Pastas ----
ANOS    <- c(2024, 2023)
UFS     <- c("PE")
PAUSA_S <- 0.3

DIR_BRUTO   <- "C:/dados/dados brutos/aurora/meio ambiente"
DIR_PARQUET <- "C:/dados/aurora/parquet"

walk(c(DIR_BRUTO, DIR_PARQUET), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

EDICOES <- list(
  "2024" = list(site = "https://institutovotorantim.org.br/ivcm",      api = "/api/ivcm_v2"),
  "2023" = list(site = "https://institutovotorantim.org.br/ivcm-2023", api = "/api/ivcm")
)

TODAS_UFS <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT",
               "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
if (is.null(UFS)) UFS <- TODAS_UFS

## ---- 1.2 Cliente HTTP ----
executar_get <- function(url, is_json = TRUE) {
  req <- request(url) |>
    req_headers(`User-Agent` = "FarolPE-SDEC-ivcm/1.0 (pesquisa)") |>
    req_timeout(60) |>
    req_options(ssl_verifypeer = 0L, ssl_verifyhost = 0L) |>
    req_retry(max_tries = 4, backoff = ~ 3 * .x)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) %in% c(403, 429)) {
    stop(paste("Bloqueio HTTP:", resp_status(resp), "em", url))
  }
  
  if (is_json) resp_body_json(resp, simplifyVector = FALSE) else resp_body_string(resp)
}

# ---- 2. ESTRUTURA DOS INDICADORES ----

## ---- 2.1 Parser do JavaScript ----
extrair_array_js <- function(texto_js, termo = "estruturaIndicadores") {
  pos_termo <- regexpr(termo, texto_js)[1]
  if (pos_termo == -1) stop(paste("Termo", termo, "não localizado no JS"))
  
  sub_js <- substr(texto_js, pos_termo, nchar(texto_js))
  pos_abertura <- regexpr("\\[", sub_js)[1]
  if (pos_abertura == -1) stop("Caractere '[' não localizado")
  
  idx_inicio <- pos_termo + pos_abertura - 1
  chars <- strsplit(substr(texto_js, idx_inicio, nchar(texto_js)), "")[[1]]
  
  nivel <- 0
  em_str <- FALSE
  quote_char <- ""
  escape <- FALSE
  idx_fim <- 0
  
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (escape) {
      escape <- FALSE
      next
    }
    if (ch == "\\") {
      escape <- TRUE
      next
    }
    if (em_str) {
      if (ch == quote_char) em_str <- FALSE
    } else {
      if (ch %in% c("'", '"', '`')) {
        em_str <- TRUE
        quote_char <- ch
      } else if (ch == "[") {
        nivel <- nivel + 1
      } else if (ch == "]") {
        nivel <- nivel - 1
        if (nivel == 0) {
          idx_fim <- i
          break
        }
      }
    }
  }
  if (idx_fim == 0) stop("Fechamento ']' correspondente não encontrado")
  substr(texto_js, idx_inicio, idx_inicio + idx_fim - 1)
}

## ---- 2.2 Descompactação da Árvore ----
niveis_desc <- c("0" = "Subíndice (pilar de risco)", 
                 "1" = "Indicador (adaptação, ameaça ou vulnerabilidade)", 
                 "2" = "Subindicador", 
                 "3" = "Variável-base")

achatar_arvore <- function(nos, pai = NA_character_, prefixo = "", pilar = NA_character_, 
                           pilar_nome = NA_character_, dimensao = NA_character_, nivel = 0) {
  linhas <- list()
  for (k in seq_along(nos)) {
    no <- nos[[k]]
    if (!is.list(no)) next
    
    rot <- if (prefixo == "") as.character(k) else paste0(prefixo, ".", k)
    p  <- if (nivel == 0) as.character(no$indice %||% "") else pilar
    pn <- if (nivel == 0) as.character(no$nome %||% "") else pilar_nome
    
    d <- dimensao
    nome_str <- as.character(no$nome %||% "")
    if (nivel == 1) {
      if (grepl("^Adaptação", nome_str)) d <- "Adaptação"
      if (grepl("^Ameaça", nome_str)) d <- "Ameaça"
      if (grepl("^Vulnerabilidade", nome_str)) d <- "Vulnerabilidade"
    }
    
    desc_raw <- as.character(no$descricao %||% "")
    desc_limpa <- str_squish(gsub("<[^>]+>", " ", desc_raw))
    
    df_atual <- tibble(
      cod_indicador  = as.character(no$indice %||% ""),
      rotulo         = rot,
      nome           = str_trim(nome_str),
      nivel          = as.integer(nivel),
      tipo           = niveis_desc[as.character(nivel)] %||% paste("Nível", nivel),
      cod_pai        = as.character(pai),
      cod_pilar      = as.character(p),
      pilar          = as.character(pn),
      dimensao       = as.character(d),
      composicao_pct = as.numeric(no$composicao %||% NA_real_),
      descricao      = desc_limpa,
      fonte          = str_trim(as.character(no$fonte %||% ""))
    )
    
    linhas[[length(linhas) + 1]] <- df_atual
    
    if (is.list(no$sub) && length(no$sub) > 0) {
      sub_res <- achatar_arvore(no$sub, pai = no$indice, prefixo = rot, 
                                pilar = p, pilar_nome = pn, dimensao = d, nivel = nivel + 1)
      linhas[[length(linhas) + 1]] <- sub_res
    }
  }
  bind_rows(linhas)
}

## ---- 2.3 Construção da Dimensão ----
estruturas <- list()
HOST <- NULL

for (ano in ANOS) {
  js_url  <- paste0(EDICOES[[as.character(ano)]]$site, "/components/application.js")
  js_code <- executar_get(js_url, is_json = FALSE)
  
  host_match <- str_match(js_code, "const\\s+HOST\\s*=\\s*['\"]([^'\"]+)['\"]")
  HOST <- host_match[1, 2]
  
  array_js <- extrair_array_js(js_code, "estruturaIndicadores")
  
  ctx <- v8()
  ctx$eval(paste0("var _arv = ", array_js, ";"))
  json_limpo <- ctx$eval("JSON.stringify(_arv);")
  arvore_indicadores <- jsonlite::fromJSON(json_limpo, simplifyDataFrame = FALSE)
  
  estruturas[[as.character(ano)]] <- achatar_arvore(arvore_indicadores)
  Sys.sleep(PAUSA_S)
}

dim_indicador <- estruturas[[as.character(ANOS[1])]]

geral <- tibble(
  cod_indicador  = "ivcm",
  rotulo         = "0",
  nome           = "Índice de Vulnerabilidade Climática dos Municípios (IVCM)",
  nivel          = -1,
  tipo           = "Índice geral",
  cod_pai        = NA_character_,
  cod_pilar      = NA_character_,
  pilar          = NA_character_,
  dimensao       = NA_character_,
  composicao_pct = NA_real_,
  descricao      = "Índice geral: combina os 6 subíndices de risco climático (16,66% cada).",
  fonte          = "Instituto Votorantim / Metas Sociais"
)

dim_indicador <- bind_rows(geral, dim_indicador) |> 
  mutate(ordem = row_number())

# ---- 3. COLETA DOS DADOS (API) ----

## ---- 3.1 Loop de Municípios ----
registos_brutos <- list()

for (ano in ANOS) {
  api_prefixo <- paste0(HOST, EDICOES[[as.character(ano)]]$api)
  
  for (uf in UFS) {
    url_uf <- paste0(api_prefixo, "/dados/", uf, "?dimension=ivcm")
    lista_muni <- executar_get(url_uf)
    Sys.sleep(PAUSA_S)
    
    for (m in lista_muni) {
      url_detalhe <- paste0(api_prefixo, "/municipalities/", m$id)
      detalhe <- executar_get(url_detalhe)
      
      detalhe$`_ano` <- ano
      detalhe$`_uf`  <- uf
      registos_brutos[[length(registos_brutos) + 1]] <- detalhe
      
      Sys.sleep(PAUSA_S)
    }
  }
}

registos_limpos <- map(registos_brutos, function(rec) {
  lapply(rec, function(v) {
    if (is.null(v) || length(v) == 0) return(NA_character_)
    if (length(v) > 1) return(paste(as.character(v), collapse = "; "))
    as.character(v)
  })
})
df_bruto <- bind_rows(registos_limpos)

# ---- 4. EXPORTAÇÃO DOS DADOS BRUTOS (.XLSX) ----

caminho_bruto_xlsx <- file.path(DIR_BRUTO, "IVCM_dados_brutos.xlsx")

wb <- createWorkbook()
addWorksheet(wb, "Dados_Brutos")
writeData(wb, "Dados_Brutos", df_bruto)
saveWorkbook(wb, caminho_bruto_xlsx, overwrite = TRUE)

# ---- 5. MODELAGEM DIMENSIONAL E PARQUET ----

## ---- 5.1 Fato IVCM ----
codigos_indicadores_validos <- dim_indicador$cod_indicador

df_bruto <- df_bruto |>
  mutate(
    cod_territorio = as.integer(codigo_ibge),
    ano            = as.integer(`_ano`),
    uf_sigla       = `_uf`
  )

cols_valores <- intersect(names(df_bruto), codigos_indicadores_validos)

ivcm_fato <- df_bruto |>
  select(ano, cod_territorio, uf_sigla, all_of(cols_valores)) |>
  pivot_longer(
    cols      = all_of(cols_valores), 
    names_to  = "cod_indicador", 
    values_to = "valor_bruto"
  ) |>
  mutate(
    valor_char  = as.character(valor_bruto),
    valor_num   = suppressWarnings(as.numeric(valor_char)),
    valor_texto = if_else(is.na(valor_num) & !is.na(valor_char), str_trim(valor_char), NA_character_),
    valor = case_when(
      !is.na(valor_num) ~ valor_num,
      tolower(valor_texto) == "sim" ~ 1.0,
      tolower(valor_texto) %in% c("não", "nao") ~ 0.0,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(valor)) |>
  group_by(ano, uf_sigla, cod_indicador) |>
  mutate(
    posicao_uf       = as.integer(min_rank(desc(valor))),
    n_municipios_uf  = as.integer(n())
  ) |>
  ungroup() |>
  select(ano, cod_territorio, uf_sigla, cod_indicador, valor, valor_texto, posicao_uf, n_municipios_uf)

## ---- 5.2 Dimensão Município ----
dim_municipio <- df_bruto |>
  select(
    ano, 
    cod_territorio, 
    uf_sigla, 
    municipio          = nome_sem_formato, 
    cor_ivcm           = ivcm_cor, 
    populacao, 
    faixa_populacional, 
    cluster, 
    microregiao
  ) |>
  mutate(
    populacao = suppressWarnings(as.numeric(populacao)),
    cluster   = suppressWarnings(as.integer(cluster))
  )

ivcm_geral <- ivcm_fato |>
  filter(cod_indicador == "ivcm") |>
  select(ano, cod_territorio, ivcm = valor, posicao_ivcm_uf = posicao_uf)

dim_municipio <- dim_municipio |>
  left_join(ivcm_geral, by = c("ano", "cod_territorio"))

## ---- 5.3 Exportação Parquet ----
write_parquet(ivcm_fato, file.path(DIR_PARQUET, "ivcm_fato.parquet"))
write_parquet(dim_indicador, file.path(DIR_PARQUET, "ivcm_dim_indicador.parquet"))
write_parquet(dim_municipio, file.path(DIR_PARQUET, "ivcm_dim_municipio_ano.parquet"))
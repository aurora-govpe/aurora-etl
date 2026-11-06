# ==============================================================================
# SCRIPT: etl_caracterizacao_ibge_tse.R
# OBJETIVO: Extração e consolidação dos indicadores de caracterização municipal:
#   1. Nome do Prefeito (TSE 2024)
#   2. Partido do Prefeito (TSE 2024)
#   3. Densidade Demográfica (IBGE Censo 2022 - Tabela 4714)
#   4. População Quilombola (IBGE Censo 2022 - Tabela 10089)
#   5. População Indígena (IBGE Censo 2022 - Tabela 9971)
#   6. Índice de Gini (Atlas Brasil / PNUD / IPEA)
#   7. Aniversário dos Municípios (Fontes Cívicas Oficiais / IBGE)
# SAÍDA: Formato empilhado oficial do aurora-etl (parquet e rds)
# ==============================================================================

# 1. Bibliotecas ----
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(httr2)
  library(jsonlite)
  library(readxl)
  library(arrow)
})

# 2. Paths e Configuração de Ambiente ----
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

is_linux <- Sys.info()[["sysname"]] == "Linux"
if (!is_linux) {
  Sys.setenv(CURL_SSL_BACKEND = "openssl")
}

message(">>> Iniciando ETL de Caracterização Municipal (IBGE / TSE / Atlas)...")

# 3. Base Canônica de Municípios de Pernambuco ----
caminho_canonical <- file.path(DIR_OUT_AURORA, "municipios_pe_canonical.json")

if (file.exists(caminho_canonical)) {
  muns_canonical <- jsonlite::fromJSON(caminho_canonical) %>%
    transmute(
      cod_municipio = as.character(ibge7),
      cod_ibge      = as.character(ibge7),
      ibge6         = as.character(ibge6),
      nm_municipio  = as.character(municipio)
    )
} else {
  # Fallback direto via API de Localidades do IBGE
  url_loc <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados/26/municipios"
  req_loc <- request(url_loc) %>% req_perform()
  loc_data <- resp_body_json(req_loc)
  muns_canonical <- map_dfr(loc_data, function(m) {
    tibble(
      cod_municipio = as.character(m$id),
      cod_ibge      = as.character(m$id),
      ibge6         = substr(as.character(m$id), 1, 6),
      nm_municipio  = as.character(m$nome)
    )
  })
}

message(sprintf("Base municipal carregada: %d municípios de PE.", nrow(muns_canonical)))

# 4. Extração IBGE Agregados (Censo 2022) ----
# Função utilitária para chamar a API de agregados v3
extrair_agregado_v3 <- function(tabela, variavel, classificacao = NULL, sigla, nm_ind, nm_fant, grupo, tipo, pol) {
  url <- sprintf("https://servicodados.ibge.gov.br/api/v3/agregados/%s/periodos/2022/variaveis/%s", tabela, variavel)
  query <- list(localidades = "N6[N3[26]]")
  if (!is.null(classificacao)) {
    query$classificacao <- classificacao
  }
  
  req <- request(url) %>%
    req_url_query(!!!query) %>%
    req_headers("User-Agent" = "aurora-etl") %>%
    req_perform()
  
  res_json <- resp_body_json(req)
  series <- res_json[[1]]$resultados[[1]]$series
  
  map_dfr(series, function(s) {
    val <- s$serie[["2022"]]
    val_clean <- ifelse(is.null(val) || val == "-" || val == "...", "0", as.character(val))
    tibble(
      cod_municipio   = as.character(s$localidade$id),
      ano             = 2022,
      nm_indicador    = nm_ind,
      nm_fantasia     = nm_fant,
      indicador       = val_clean,
      mes_referencia  = NA_character_,
      sigla_indicador = sigla,
      grupo           = grupo,
      tipo            = tipo,
      polaridade      = pol
    )
  })
}

message("Extraindo Densidade Demográfica (Censo 2022)...")
df_densidade <- extrair_agregado_v3(
  tabela         = "4714",
  variavel       = "614",
  sigla          = "densidade_demografica",
  nm_ind         = "Densidade Demográfica (hab/km²)",
  nm_fant        = "Densidade Demográfica",
  grupo          = "Demografia",
  tipo           = "absoluto",
  pol            = "neutra"
)

message("Extraindo População Quilombola (Censo 2022)...")
df_quilombola <- extrair_agregado_v3(
  tabela         = "10089",
  variavel       = "4709",
  classificacao  = "2[6794]|58[95253]|2661[32776]|1[6795]",
  sigla          = "pop_quilombola",
  nm_ind         = "População Quilombola",
  nm_fant        = "População Quilombola",
  grupo          = "Demografia",
  tipo           = "absoluto",
  pol            = "positiva"
)

message("Extraindo População Indígena (Censo 2022)...")
df_indigena <- extrair_agregado_v3(
  tabela         = "9971",
  variavel       = "350",
  classificacao  = "2[6794]|58[95253]|2661[32776]|1[6795]",
  sigla          = "pop_indigena",
  nm_ind         = "População Indígena",
  nm_fant        = "População Indígena",
  grupo          = "Demografia",
  tipo           = "absoluto",
  pol            = "positiva"
)

# 5. Extração Índice de Gini (Atlas Brasil / IPEADATA) ----
message("Extraindo Índice de Gini Municipal (Atlas Brasil / Ipea)...")
url_gini <- "http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='ADH_GINI')"
req_gini <- request(url_gini) %>% req_perform()
raw_gini <- resp_body_json(req_gini)$value

df_gini_raw <- bind_rows(raw_gini) %>%
  filter(startsWith(as.character(TERCODIGO), "26")) %>%
  mutate(
    ano = as.numeric(substr(as.character(VALDATA), 1, 4)),
    cod_municipio = as.character(TERCODIGO)
  )

# Considera o último ano do censo disponível (2010)
df_gini <- df_gini_raw %>%
  filter(ano == 2010) %>%
  transmute(
    cod_municipio   = cod_municipio,
    ano             = ano,
    nm_indicador    = "Índice de Gini",
    nm_fantasia     = "Índice de Gini",
    indicador       = as.character(VALVALOR),
    mes_referencia  = NA_character_,
    sigla_indicador = "indice_gini",
    grupo           = "Vulnerabilidade",
    tipo            = "índice",
    polaridade      = "negativa"
  )

# 6. Extração Prefeito e Partido (TSE - Eleições 2024) ----
message("Extraindo Prefeitos e Partidos Eleitos (TSE 2024)...")
url_tse_mun <- "https://resultados.tse.jus.br/oficial/ele2024/619/config/mun-e000619-cm.json"
req_tse_mun <- request(url_tse_mun) %>%
  req_headers("User-Agent" = "Mozilla/5.0") %>%
  req_perform()

data_tse_mun <- resp_body_json(req_tse_mun)
pe_tse_abr <- keep(data_tse_mun$abr, ~ .x$cd == "pe")[[1]]

df_tse_keys <- map_dfr(pe_tse_abr$mu, function(m) {
  tibble(
    cod_tse       = m$cd,
    cod_municipio = m$cdi,
    nm_tse        = m$nm
  )
})

# Requisições em lote paralelo para os 184 municípios (1º turno: 619)
reqs_tse_619 <- map(df_tse_keys$cod_tse, function(cd) {
  sprintf("https://resultados.tse.jus.br/oficial/ele2024/619/dados/pe/pe%s-c0011-e000619-u.json", cd) %>%
    request() %>%
    req_headers("User-Agent" = "Mozilla/5.0")
})

resps_tse_619 <- req_perform_parallel(reqs_tse_619, on_error = "continue", max_active = 10, progress = FALSE)

parse_tse_cand <- function(resp) {
  if (is.null(resp) || resp_status(resp) != 200) {
    return(list(prefeito = NA_character_, partido = NA_character_))
  }
  json <- resp_body_json(resp)
  carg <- json$carg[[1]]
  
  candidatos <- list()
  for (agr in carg$agr) {
    for (par in agr$par) {
      for (cand in par$cand) {
        candidatos[[length(candidatos) + 1]] <- list(
          nome    = cand$nmu,
          partido = par$sg,
          eleito  = !is.null(cand$e) && cand$e == "s",
          votos   = suppressWarnings(as.numeric(cand$vap))
        )
      }
    }
  }
  
  if (length(candidatos) == 0) {
    return(list(prefeito = NA_character_, partido = NA_character_))
  }
  
  # 1. Procura explicitamente eleito
  eleitos <- keep(candidatos, ~ .x$eleito)
  if (length(eleitos) > 0) {
    return(list(prefeito = eleitos[[1]]$nome, partido = eleitos[[1]]$partido))
  }
  
  # 2. Fallback: candidato com maior votação nominal
  ord <- order(map_dbl(candidatos, ~ ifelse(is.na(.x$votos), 0, .x$votos)), decreasing = TRUE)
  top <- candidatos[[ord[1]]]
  return(list(prefeito = top$nome, partido = top$partido))
}

tse_parsed <- map(resps_tse_619, parse_tse_cand)
df_tse_keys$nm_prefeito <- map_chr(tse_parsed, "prefeito")
df_tse_keys$sg_partido  <- map_chr(tse_parsed, "partido")

# Caso de municípios com 2º turno (Olinda: 24910, Paulista: 25135)
cidades_2t <- c("24910", "25135")
for (cd_2t in cidades_2t) {
  tryCatch({
    url_2t <- sprintf("https://resultados.tse.jus.br/oficial/ele2024/620/dados/pe/pe%s-c0011-e000620-u.json", cd_2t)
    r_2t <- request(url_2t) %>% req_headers("User-Agent" = "Mozilla/5.0") %>% req_perform()
    p_2t <- parse_tse_cand(r_2t)
    idx <- which(df_tse_keys$cod_tse == cd_2t)
    if (length(idx) > 0 && !is.na(p_2t$prefeito)) {
      df_tse_keys$nm_prefeito[idx] <- p_2t$prefeito
      df_tse_keys$sg_partido[idx]  <- p_2t$partido
    }
  }, error = function(e) NULL)
}

# 6.1. Aplicação de Exceções de Gestão em Exercício (Renúncias / Sucessões) ----
caminho_excecoes <- file.path(DIR_RAW_AURORA, "ibge/excecoes_gestores_pe.xlsx")

if (file.exists(caminho_excecoes)) {
  message("Verificando camada de exceções de gestores em exercício (ibge/excecoes_gestores_pe.xlsx)...")
  df_excecoes <- readxl::read_excel(caminho_excecoes) %>%
    mutate(
      cod_municipio = as.character(cod_municipio),
      ativo_norm    = tolower(str_squish(as.character(ativo)))
    ) %>%
    filter(ativo_norm %in% c("sim", "s", "true", "1"))
  
  if (nrow(df_excecoes) > 0) {
    df_tse_keys <- df_tse_keys %>%
      left_join(
        df_excecoes %>% select(cod_municipio, prefeito_em_exercicio, partido_em_exercicio),
        by = "cod_municipio"
      ) %>%
      mutate(
        nm_prefeito = ifelse(!is.na(prefeito_em_exercicio) & str_trim(prefeito_em_exercicio) != "",
                             as.character(prefeito_em_exercicio), nm_prefeito),
        sg_partido  = ifelse(!is.na(partido_em_exercicio) & str_trim(partido_em_exercicio) != "",
                             as.character(partido_em_exercicio), sg_partido)
      ) %>%
      select(-any_of(c("prefeito_em_exercicio", "partido_em_exercicio")))
    
    message(sprintf("Exceções aplicadas com sucesso para %d município(s) (ex.: Recife -> %s / %s).",
                    nrow(df_excecoes),
                    df_tse_keys$nm_prefeito[df_tse_keys$cod_municipio == "2611606"],
                    df_tse_keys$sg_partido[df_tse_keys$cod_municipio == "2611606"]))
  }
}

df_prefeito <- df_tse_keys %>%
  transmute(
    cod_municipio   = cod_municipio,
    ano             = 2024,
    nm_indicador    = "Nome do Prefeito",
    nm_fantasia     = "Nome do Prefeito",
    indicador       = as.character(nm_prefeito),
    mes_referencia  = NA_character_,
    sigla_indicador = "nome_prefeito",
    grupo           = "Institucional",
    tipo            = "texto",
    polaridade      = "neutra"
  )

df_partido <- df_tse_keys %>%
  transmute(
    cod_municipio   = cod_municipio,
    ano             = 2024,
    nm_indicador    = "Partido do Prefeito",
    nm_fantasia     = "Partido do Prefeito",
    indicador       = as.character(sg_partido),
    mes_referencia  = NA_character_,
    sigla_indicador = "partido_prefeito",
    grupo           = "Institucional",
    tipo            = "texto",
    polaridade      = "neutra"
  )

# 7. Extração Aniversário dos Municípios ----
message("Extraindo Datas Oficiais de Aniversário Municipal...")
base_a77 <- "https://cidades.a77.com.br/aniversarios/"
paginas_a77 <- c(
  "datas-de-aniversarios-de-cidades-de-pernambuco-53.php",
  "datas-de-aniversarios-de-cidades-de-pernambuco-54.php",
  "datas-de-aniversarios-de-cidades-de-pernambuco-55.php"
)

aniversarios_lista <- list()

for (pg in paginas_a77) {
  tryCatch({
    url_pg <- paste0(base_a77, pg)
    req_pg <- request(url_pg) %>%
      req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0") %>%
      req_perform()
    html_pg <- resp_body_string(req_pg)
    
    matches <- str_match_all(html_pg, "<tr><td>(.*?)</td><td>(.*?)</td></tr>")[[1]]
    if (nrow(matches) > 0) {
      aniversarios_lista[[length(aniversarios_lista) + 1]] <- tibble(
        nm_clean    = tolower(str_squish(matches[, 2])),
        aniversario = str_squish(matches[, 3])
      )
    }
  }, error = function(e) {
    message("Aviso ao extrair página de aniversários: ", conditionMessage(e))
  })
}

df_aniversarios_raw <- bind_rows(aniversarios_lista) %>%
  distinct(nm_clean, .keep_all = TRUE)

# Função para normalizar strings para matching
normalizar_texto <- function(txt) {
  txt %>%
    tolower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^a-z0-9]", "")
}

df_aniv_norm <- df_aniversarios_raw %>%
  mutate(key = normalizar_texto(nm_clean))

df_aniversario <- muns_canonical %>%
  mutate(key = normalizar_texto(nm_municipio)) %>%
  left_join(df_aniv_norm %>% select(key, aniversario), by = "key") %>%
  transmute(
    cod_municipio   = cod_municipio,
    ano             = 2024,
    nm_indicador    = "Aniversário do Município",
    nm_fantasia     = "Aniversário do Município",
    indicador       = as.character(aniversario),
    mes_referencia  = NA_character_,
    sigla_indicador = "aniversario_municipio",
    grupo           = "Institucional",
    tipo            = "texto",
    polaridade      = "neutra"
  )

# 8. Empilhamento e Enriquecimento ----
message("Empilhando novos indicadores de caracterização...")

df_empilhado_novos <- bind_rows(
  df_prefeito,
  df_partido,
  df_densidade,
  df_quilombola,
  df_indigena,
  df_gini,
  df_aniversario
) %>%
  left_join(
    muns_canonical %>% select(cod_municipio, nm_municipio, cod_ibge, ibge6),
    by = "cod_municipio"
  ) %>%
  filter(!is.na(indicador)) %>%
  select(
    cod_municipio,
    nm_municipio,
    ano,
    nm_indicador,
    nm_fantasia,
    indicador,
    mes_referencia,
    sigla_indicador,
    grupo,
    tipo,
    polaridade,
    cod_ibge,
    ibge6
  )

# 9. Cálculo de Ranking PE ----
df_final_novos <- df_empilhado_novos %>%
  group_by(sigla_indicador, ano) %>%
  mutate(
    val_num_temp = suppressWarnings(as.numeric(indicador)),
    rank_pe      = ifelse(is.na(val_num_temp), NA_integer_, as.integer(min_rank(desc(val_num_temp))))
  ) %>%
  select(-val_num_temp) %>%
  ungroup()

message(sprintf("Total de registros consolidados: %d linhas.", nrow(df_final_novos)))

# 10. Gravação dos Arquivos (Saída compatível com etl_caracter_mun.R) ----
dir.create(file.path(DIR_OUT_AURORA, "parquet"), recursive = TRUE, showWarnings = FALSE)

# A) Arquivos específicos desta extração
path_rds_extra   <- file.path(DIR_OUT_AURORA, "df_caracterizacao_ibge_tse.rds")
path_pqt_extra   <- file.path(DIR_OUT_AURORA, "parquet/df_caracterizacao_ibge_tse.parquet")

saveRDS(df_final_novos, path_rds_extra)
arrow::write_parquet(df_final_novos, path_pqt_extra, compression = "snappy")
message(sprintf("Salvo arquivo específico em: %s", path_pqt_extra))

# B) Integração no df_indicadores.parquet e df_indicadores_empilhados.rds oficiais
path_pqt_oficial <- file.path(DIR_OUT_AURORA, "parquet/df_indicadores.parquet")
path_rds_oficial <- file.path(DIR_OUT_AURORA, "df_indicadores_empilhados.rds")

if (file.exists(path_pqt_oficial)) {
  message("Integrando indicadores ao parquet oficial existente...")
  df_oficial_antigo <- arrow::read_parquet(path_pqt_oficial)
  
  # Remove versões anteriores destas mesmas siglas para atualização limpa
  siglas_novas <- unique(df_final_novos$sigla_indicador)
  
  df_oficial_atualizado <- df_oficial_antigo %>%
    filter(!sigla_indicador %in% siglas_novas) %>%
    bind_rows(df_final_novos)
  
  arrow::write_parquet(df_oficial_atualizado, path_pqt_oficial, compression = "snappy")
  message(sprintf("Parquet oficial atualizado com sucesso (%d linhas no total).", nrow(df_oficial_atualizado)))
  
  if (file.exists(path_rds_oficial)) {
    saveRDS(df_oficial_atualizado, path_rds_oficial)
    message("RDS oficial atualizado com sucesso.")
  }
  
  # Cópias de segurança em DIR_RAW_AURORA se os diretórios existirem
  if (dir.exists(file.path(DIR_RAW_AURORA, "indicadores"))) {
    arrow::write_parquet(
      df_oficial_atualizado,
      file.path(DIR_RAW_AURORA, "indicadores/df_indicadores.parquet"),
      compression = "snappy"
    )
  }
  if (dir.exists(file.path(DIR_RAW_AURORA, "ibge"))) {
    saveRDS(df_oficial_atualizado, file.path(DIR_RAW_AURORA, "ibge/df_indicadores_empilhados.rds"))
  }
}

message(">>> ETL de Caracterização concluído com êxito!")

# ==============================================================================
# R/obitos_maternos.R
# Leitura da planilha "Número de óbitos maternos segundo cor/raça"
# (Observatório da Criança e do Adolescente - Fundação Abrinq).
#
# Usada por etl_caracter_mun.R (municípios de PE) e etl_indicadores_ufs.R (UFs),
# para que os dois níveis tratem a planilha exatamente da mesma forma.
#
# Planilha: aba "Índice" + 7 abas de dados (Indígena, Preta, Branca, Amarela,
# Parda, Ignorada e Total), skip = 2, colunas Localidade | Código IBGE |
# Código DataSUS | 2000 ... 2025. Valores são contagens (absolutos).
#
# ------------------------------------------------------------------------------
# DECISÃO REVISÁVEL D-001 (2026-11-10, decidida pela usuária Kattine Costa Pedrosa)
#   Na planilha, de 2016 em diante os municípios não têm nenhum "0": o zero
#   aparece como "-" (de 2000 a 2015 o zero vem explícito e não há "-").
#   Decisão: tratar "-" como 0 óbitos.
#   Para REVOGAR: definir AURORA_OBITOS_MAT_TRACO_COMO_ZERO=false (o "-" passa a
#   ser descartado, como nos demais indicadores Abrinq) ou apagar este bloco.
#   Toda execução registra no log quantas células foram convertidas.
# ------------------------------------------------------------------------------
# ==============================================================================

OBITOS_MAT_CATEGORIAS <- data.frame(
  padrao_aba      = c("Ind[ií]gena", "Preta", "Branca", "Amarela", "Parda", "Ignorada", "mater"),
  sigla_indicador = paste0("obitos_mat_", c("indigena", "preta", "branca", "amarela", "parda", "ignorada", "total")),
  nm_indicador    = paste0("Óbitos maternos - ", c("Indígena", "Preta", "Branca", "Amarela", "Parda", "Ignorada", "Total")),
  stringsAsFactors = FALSE
)

obitos_mat_traco_como_zero <- function() {
  tolower(Sys.getenv("AURORA_OBITOS_MAT_TRACO_COMO_ZERO", "true")) == "true"
}

.obitos_mat_log <- function(msg) {
  cli::cli_alert_warning(msg)
  if (exists("log_warn", mode = "function")) log_warn(msg)
}

# nivel = "municipio" -> cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia (só PE)
# nivel = "uf"        -> cod_uf, ano, indicador, mes_referencia, sigla_indicador (o script junta sg_uf/nm_uf)
ler_obitos_maternos <- function(caminho, nivel = c("municipio", "uf")) {
  nivel <- match.arg(nivel)
  if (!file.exists(caminho)) {
    cli::cli_alert_warning("Planilha de óbitos maternos ausente em {.path {caminho}}")
    return(dplyr::tibble())
  }

  abas <- grep("^[0-9]+ *-", readxl::excel_sheets(caminho), value = TRUE)
  traco_zero <- obitos_mat_traco_como_zero()
  n_traco <- 0L

  out <- lapply(seq_len(nrow(OBITOS_MAT_CATEGORIAS)), function(i) {
    aba <- grep(OBITOS_MAT_CATEGORIAS$padrao_aba[i], abas, value = TRUE)
    stopifnot("aba da categoria nao encontrada ou ambigua" = length(aba) == 1)

    d <- suppressMessages(suppressWarnings(
      readxl::read_excel(caminho, sheet = aba, skip = 2, col_types = "text")
    ))
    nomes_ano <- names(d)[-(1:3)]
    names(d)[1:3] <- c("nm_localidade", "cod_ibge", "cod_datasus")  # por posição (cabeçalhos podem ter caracteres invisíveis)
    d$cod_ibge <- stringr::str_trim(d$cod_ibge)

    d <- if (nivel == "municipio") {
      # 185 municípios de PE; "Município Ignorado - PE" (2600000) fica de fora
      dplyr::filter(d, grepl("^26[0-9]{5}$", cod_ibge), cod_ibge != "2600000")
    } else {
      dplyr::filter(d, grepl("^[0-9]{2}$", cod_ibge), cod_ibge >= "11")  # 27 UFs (exclui Brasil e regiões)
    }

    d <- tidyr::pivot_longer(d, cols = -(1:3), names_to = "ano_raw", values_to = "indicador")
    d$ano <- as.numeric(stringr::str_extract(d$ano_raw, "[0-9]{4}"))
    d$indicador <- stringr::str_trim(d$indicador)

    eh_traco <- !is.na(d$indicador) & d$indicador == "-"
    n_traco <<- n_traco + sum(eh_traco)
    if (traco_zero) d$indicador[eh_traco] <- "0"
    d <- d[!is.na(d$indicador) & !d$indicador %in% c("-", "ND", "nan", "NA"), ]

    d$sigla_indicador <- OBITOS_MAT_CATEGORIAS$sigla_indicador[i]
    d$nm_indicador    <- OBITOS_MAT_CATEGORIAS$nm_indicador[i]
    d$mes_referencia  <- NA_character_
    d
  })
  d <- dplyr::bind_rows(out)

  if (traco_zero) {
    .obitos_mat_log(sprintf(
      "DECISAO REVISAVEL D-001 (2026-11-10): %d celulas '-' da planilha de obitos maternos (%s) foram convertidas em 0 obito. Revogar: AURORA_OBITOS_MAT_TRACO_COMO_ZERO=false.",
      n_traco, nivel))
  } else {
    .obitos_mat_log(sprintf(
      "D-001 REVOGADA por variavel de ambiente: %d celulas '-' (%s) descartadas, nao convertidas em 0.", n_traco, nivel))
  }

  if (nivel == "municipio") {
    dplyr::transmute(d, cod_municipio = cod_ibge, nm_municipio = nm_localidade, ano,
                     nm_indicador, indicador, mes_referencia)
  } else {
    dplyr::transmute(d, cod_uf = cod_ibge, ano, indicador, mes_referencia, sigla_indicador)
  }
}

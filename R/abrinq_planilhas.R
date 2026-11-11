# ==============================================================================
# R/abrinq_planilhas.R
# Lote de planilhas do Observatório da Criança e do Adolescente (Fundação Abrinq):
#   - Notificações de acidentes de trabalho de crianças e adolescentes (5 a 17 anos):
#       total, por faixa etária, cor/raça, sexo e evolução dos casos (tema: cidadania)
#   - Taxa de mortalidade atribuída a fontes de água inadequadas, saneamento
#       inadequado e falta de higiene, por grupos etários (tema: saúde)
#
# Usado por etl_caracter_mun.R (municípios de PE) e etl_indicadores_ufs.R (UFs).
# O catálogo abaixo é a única fonte das siglas/nomes: `abrinq_lote_metadados()`
# devolve exatamente as linhas que vão na `lista de indicadores.xlsx`
# (abas Sheet1 e UFs).
#
# As abas são localizadas pelo RÓTULO da célula A2 (não pela ordem), porque as
# planilhas trazem as abas embaralhadas e com nomes truncados.
#
# ------------------------------------------------------------------------------
# DECISÕES REVISÁVEIS (usuária Kattine Costa Pedrosa, 2026-11-11)
#
# D-002  Acidentes de trabalho: o "-" vale 0 notificação.
#        Nos municípios o "-" é quase todo o dado; o "0" explícito só aparece em
#        2017-2019 (convive com "-"). A soma dos 185 municípios de PE bate com o
#        total da UF. Revogar: AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO=false
#        (o "-" passa a ser descartado).
#
# D-003  Mortalidade por saneamento inadequado, MUNICÍPIOS: a taxa por 100 mil
#        hab. só é carregada até 2019. De 2020 em diante a taxa municipal da
#        planilha está em escala inconsistente (ex.: Recife 2024 = 0,06; o
#        esperado seria ~7; população implícita absurda). O número absoluto
#        municipal e todas as séries das UFs entram completos.
#        Revogar: AURORA_MORT_SANEA_TAXA_MUN_ANO_MAX=2025.
#
# Nota (não é decisão): na mortalidade o "-" é descartado (não vira 0) porque
# nessa planilha o 0 aparece explícito no absoluto; "-" = dado não calculado.
# ==============================================================================

ABRINQ_ARQUIVOS <- c(
  acid_idade = "cidadania/Notificações acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade.xlsx",
  acid_cor   = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo cor_raça.xlsx",
  acid_evol  = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo evolução dos casos.xlsx",
  acid_sexo  = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo sexo.xlsx",
  mort       = "saude/Taxa de mortalidade atribuída a fontes de água inadequadas, saneamento inadequado e falta de higiene segundo grupos etários.xlsx"
)

.abq_catalogo <- function() {
  acid <- function(arquivo, rotulo, sigla, nm, fantasia) {
    data.frame(arquivo = arquivo, rotulo = rotulo, serie = "valor",
               sigla_indicador = paste0("acid_trab_", sigla),
               nm_indicador = paste0("Acidentes de trabalho (5 a 17 anos) - ", nm),
               nm_fantasia = paste0("Acidentes de trabalho 5-17 anos (", fantasia, ")"),
               grupo = "cidadania", tipo = "absoluto", polaridade = "negativa",
               traco_zero = TRUE, stringsAsFactors = FALSE)
  }
  mort <- function(rotulo, sigla, faixa, taxa = TRUE) {
    f <- function(serie, pre, nm_pre, tipo, fan_pre) data.frame(
      arquivo = "mort", rotulo = rotulo, serie = serie,
      sigla_indicador = paste0("mort_sanea_", pre, "_", sigla),
      nm_indicador = paste0("Mortalidade por saneamento inadequado - ", nm_pre, " (", faixa, ")"),
      nm_fantasia = paste0("Mort. saneamento inadequado: ", fan_pre, " (", faixa, ")"),
      grupo = "saúde", tipo = tipo, polaridade = "negativa", traco_zero = FALSE, stringsAsFactors = FALSE)
    rbind(if (taxa) f("taxa", "taxa", "Taxa por 100 mil hab.", "índice", "taxa"),
          f("abs", "abs", "Nº de óbitos", "absoluto", "nº"))
  }
  rbind(
    acid("acid_cor",   "^Total de acidentes relacionados ao trabalho de crianças e adolescentes", "5_17_total", "Total", "total"),
    acid("acid_idade", "^5 a 9 anos de idade$",   "5_9",   "5 a 9 anos",   "5 a 9 anos"),
    acid("acid_idade", "^10 a 14 anos de idade$", "10_14", "10 a 14 anos", "10 a 14 anos"),
    acid("acid_idade", "^15 a 17 anos de idade$", "15_17", "15 a 17 anos", "15 a 17 anos"),
    acid("acid_cor", "^Amarela$",   "cor_amarela",   "Cor/raça Amarela",   "cor/raça amarela"),
    acid("acid_cor", "^Indígena$",  "cor_indigena",  "Cor/raça Indígena",  "cor/raça indígena"),
    acid("acid_cor", "^Preta$",     "cor_preta",     "Cor/raça Preta",     "cor/raça preta"),
    acid("acid_cor", "^Parda$",     "cor_parda",     "Cor/raça Parda",     "cor/raça parda"),
    acid("acid_cor", "^Ignorado$",  "cor_ignorada",  "Cor/raça Ignorada",  "cor/raça ignorada"),
    acid("acid_cor", "^Branca$",    "cor_branca",    "Cor/raça Branca",    "cor/raça branca"),
    acid("acid_sexo", "^Sexo ignorado$", "sexo_ignorado",  "Sexo Ignorado", "sexo ignorado"),
    acid("acid_sexo", "^Feminino$",      "sexo_feminino",  "Sexo Feminino", "sexo feminino"),
    acid("acid_sexo", "^Masculino$",     "sexo_masculino", "Sexo Masculino", "sexo masculino"),
    acid("acid_evol", "^Ignorado/Branco$",                  "evol_ignorada",       "Evolução: Ignorada/Em branco",        "evolução ignorada"),
    acid("acid_evol", "^Incapacidade parcial permanente$",  "evol_incap_parcial",  "Evolução: Incapacidade parcial permanente", "incapacidade parcial permanente"),
    acid("acid_evol", "^Incapacidade total permanente$",    "evol_incap_total",    "Evolução: Incapacidade total permanente",   "incapacidade total permanente"),
    acid("acid_evol", "^Outra$",                            "evol_outra",          "Evolução: Outra",                     "evolução outra"),
    acid("acid_evol", "^Óbito por outras causas$",          "evol_obito_outras",   "Evolução: Óbito por outras causas",   "óbito por outras causas"),
    acid("acid_evol", "^Óbito pelo acidente$",              "evol_obito_acidente", "Evolução: Óbito pelo acidente",       "óbito pelo acidente"),
    acid("acid_evol", "^Incapacidade temporária$",          "evol_incap_temp",     "Evolução: Incapacidade temporária",   "incapacidade temporária"),
    acid("acid_evol", "^Cura$",                             "evol_cura",           "Evolução: Cura",                      "cura"),
    mort("^zero a quatro anos de idade$", "0_4",    "0 a 4 anos"),
    mort("^cinco a nove anos de idade$",  "5_9",    "5 a 9 anos"),
    mort("^dez a 14 anos de idade$",      "10_14",  "10 a 14 anos"),
    mort("^15 a 19 anos de idade$",       "15_19",  "15 a 19 anos"),
    mort("^20 a 29 anos de idade$",       "20_29",  "20 a 29 anos"),
    mort("^30 a 39 anos de idade$",       "30_39",  "30 a 39 anos"),
    mort("^40 a 49 anos de idade$",       "40_49",  "40 a 49 anos"),
    mort("^50 anos ou mais de idade$",    "50_mais", "50 anos ou mais"),
    mort("^Idade ignorada$",              "ignorada", "idade ignorada", taxa = FALSE),  # taxa inexistente (tudo "-")
    mort("^Taxa de mortalidade total$",   "total",  "total")
  )
}

abrinq_lote_metadados <- function() {
  .abq_catalogo()[, c("sigla_indicador", "nm_indicador", "nm_fantasia", "grupo", "tipo", "polaridade")]
}

.abq_log <- function(msg) {
  cli::cli_alert_warning(msg)
  if (exists("log_warn", mode = "function")) log_warn(msg)
}
.abq_flag <- function(var, padrao) tolower(Sys.getenv(var, padrao)) == "true"

# nivel = "municipio" -> cod_municipio, nm_municipio, ano, nm_indicador, indicador, mes_referencia (PE)
# nivel = "uf"        -> cod_uf, ano, indicador, mes_referencia, sigla_indicador
ler_abrinq_lote <- function(dir_raw, nivel = c("municipio", "uf")) {
  nivel <- match.arg(nivel)
  cat_ <- .abq_catalogo()
  acid_traco_zero <- .abq_flag("AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO", "true")
  ano_max_taxa_mun <- suppressWarnings(as.numeric(Sys.getenv("AURORA_MORT_SANEA_TAXA_MUN_ANO_MAX", "2019")))
  if (is.na(ano_max_taxa_mun)) ano_max_taxa_mun <- 2019
  n_traco <- 0L; n_taxa_cortada <- 0L
  out <- list()

  for (chave in names(ABRINQ_ARQUIVOS)) {
    caminho <- file.path(dir_raw, ABRINQ_ARQUIVOS[[chave]])
    if (!file.exists(caminho)) {
      cli::cli_alert_warning("Planilha Abrinq ausente: {.path {caminho}}")
      next
    }
    abas <- grep("^[0-9]+ *-", readxl::excel_sheets(caminho), value = TRUE)
    rotulos <- vapply(abas, function(a) {
      r <- suppressMessages(suppressWarnings(
        readxl::read_excel(caminho, sheet = a, range = "A2:A2", col_names = FALSE, col_types = "text")))
      if (nrow(r) == 0) NA_character_ else stringr::str_squish(as.character(r[[1]][1]))
    }, character(1))

    for (i in which(cat_$arquivo == chave)) {
      cl <- cat_[i, ]
      aba <- abas[!is.na(rotulos) & grepl(cl$rotulo, rotulos)]
      stopifnot("aba nao encontrada ou ambigua no catalogo Abrinq" = length(aba) == 1)

      d <- suppressMessages(suppressWarnings(
        readxl::read_excel(caminho, sheet = aba, skip = 2, col_types = "text", .name_repair = "minimal")))
      nomes <- stringr::str_squish(names(d))
      names(d) <- nomes
      d <- d[, !duplicated(nomes) | seq_along(nomes) <= 3, drop = FALSE]   # algumas abas repetem um ano
      names(d)[1:3] <- c("nm_localidade", "cod_ibge", "cod_datasus")      # por posição (cabeçalho pode ter caracteres invisíveis)
      d$cod_ibge <- stringr::str_trim(d$cod_ibge)

      d <- if (nivel == "municipio") {
        dplyr::filter(d, grepl("^26[0-9]{5}$", cod_ibge), cod_ibge != "2600000")  # 185 municípios de PE (sem "Município Ignorado")
      } else {
        dplyr::filter(d, grepl("^[0-9]{2}$", cod_ibge), cod_ibge >= "11")          # 27 UFs (sem Brasil e regiões)
      }
      if (nrow(d) == 0) next

      d <- tidyr::pivot_longer(d, cols = -(1:3), names_to = "coluna", values_to = "indicador")
      d$ano <- as.numeric(stringr::str_extract(d$coluna, "[0-9]{4}"))
      d$serie_col <- ifelse(grepl("[(]%[)]", d$coluna), "taxa", ifelse(grepl("absoluto", d$coluna), "abs", "valor"))
      d <- d[d$serie_col == cl$serie & !is.na(d$ano), ]
      d$indicador <- stringr::str_trim(d$indicador)

      eh_traco <- !is.na(d$indicador) & d$indicador == "-"
      if (cl$traco_zero && acid_traco_zero) {
        n_traco <- n_traco + sum(eh_traco)
        d$indicador[eh_traco] <- "0"
      }
      d <- d[!is.na(d$indicador) & !d$indicador %in% c("-", "ND", "nan", "NA"), ]

      if (cl$serie == "taxa") {
        if (nivel == "municipio") {                                           # D-003
          n_taxa_cortada <- n_taxa_cortada + sum(d$ano > ano_max_taxa_mun)
          d <- d[d$ano <= ano_max_taxa_mun, ]
        }
        d$indicador <- as.character(round(as.numeric(d$indicador), 2))        # remove ruído de ponto flutuante
      }

      d$sigla_indicador <- cl$sigla_indicador
      d$nm_indicador <- cl$nm_indicador
      d$mes_referencia <- NA_character_
      out[[length(out) + 1]] <- d
    }
  }
  d <- dplyr::bind_rows(out)

  if (acid_traco_zero) {
    .abq_log(sprintf("DECISAO REVISAVEL D-002 (2026-11-11): %d celulas '-' das planilhas de acidentes de trabalho (%s) foram convertidas em 0. Revogar: AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO=false.", n_traco, nivel))
  } else {
    .abq_log(sprintf("D-002 REVOGADA por variavel de ambiente: celulas '-' das planilhas de acidentes (%s) descartadas, nao convertidas em 0.", nivel))
  }
  if (nivel == "municipio") {
    .abq_log(sprintf("DECISAO REVISAVEL D-003 (2026-11-11): taxa municipal de mortalidade por saneamento limitada a ate %d (%d valores posteriores descartados; escala inconsistente na fonte). Revogar: AURORA_MORT_SANEA_TAXA_MUN_ANO_MAX=2025.", ano_max_taxa_mun, n_taxa_cortada))
  }

  if (nivel == "municipio") {
    dplyr::transmute(d, cod_municipio = cod_ibge, nm_municipio = nm_localidade, ano,
                     nm_indicador, indicador, mes_referencia)
  } else {
    dplyr::transmute(d, cod_uf = cod_ibge, ano, indicador, mes_referencia, sigla_indicador)
  }
}

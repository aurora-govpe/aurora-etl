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
#
# --- Lote trabalho infantil (usuária Kattine Costa Pedrosa, 2026-11-12) ---
# D-004  Apenas autoconsumo: em 4 das 5 séries (5-9, 10-13, 14-15 e 16-17 anos) todo
#        o 2022 é idêntico ao 2019 (28 de 28 linhas) -> cópia da fonte; 2022 descartado
#        nessas 4 séries (o total 5-17 mantém 2022, que difere de 2019).
#        Revogar: AURORA_TI_AUTOCONSUMO_2022_DESCARTAR=false.
#
# D-005  Lista TIP: o % de 2022 das UFs nas 4 faixas etárias está em escala errada
#        (denominador implícito ~755 mil = total do Brasil, em todas as UFs; ex.: PE
#        14-15 anos = 0,35% com 2.660 casos; nos outros anos ~25%). Descartado só o % de
#        2022 das UFs nessas 4 faixas; absolutos e total 5-17 ficam.
#        Revogar: AURORA_TI_TIP_PCT_2022_UF_DESCARTAR=false.
#
# D-006  Atividades econômicas ou autoconsumo, 16-17 anos: o % de 2017 é ~2x o esperado
#        (Brasil 29,4% contra 15,9% em 2016 e 15,3% em 2018; o absoluto é coerente).
#        Descartado só o % de 2017 dessa série.
#        Revogar: AURORA_TI_ECON_1617_PCT_2017_DESCARTAR=false.
#
# Nota (não é decisão): nas planilhas de trabalho infantil só há UFs (sem municípios);
# a de estabelecimentos agropecuários traz municípios e UFs (só 2017). O "%" dos
# arquivos "Lista TIP" é composição: nas faixas etárias, % do total em atividades da
# Lista TIP; no total 5-17, % dos que estão em trabalho infantil.
#
# --- Lote 3: Gini, medidas socioeducativas, casamentos e Lista TIP 14-17 anos
#     (usuária Kattine Costa Pedrosa, 2026-11-16) ---
# D-007  Casamentos de meninas: o "-" vale 0 casamento (municípios e UFs), nas 3
#        abas. Evidência: em PE o "-" aparece em no máximo 1 município por ano na
#        aba total e a soma dos demais municípios bate com a UF em todos os 22
#        anos (2003-2024). Revogar: AURORA_CASAMENTOS_TRACO_COMO_ZERO=false.
#
# Notas (não são decisões):
#  - Medidas socioeducativas: o "-" é descartado (anos sem levantamento, não 0;
#    ex.: "Internação sanção" só tem 2023).
#  - Gini: só UFs; grupo "economia", tipo "índice" (valores 0,4-0,6, fora da regra
#    0-1 -> % do consolidado municipal, que só vale para "percentual").
#  - Lista TIP 14-17 anos (2 planilhas): séries TRIMESTRAIS (1T2012 a 2T2026),
#    carregadas como vieram: `ano` + `mes_referencia` = "T1".."T4". Só UFs.
#  - Brasil e regiões (código IBGE de 1 dígito) ficam de fora, como nos lotes
#    anteriores.
# ==============================================================================

ABRINQ_ARQUIVOS <- c(
  gini       = "economia/Coeficiente de Gini de desigualdade.xlsx",
  liberdade  = "cidadania/Número de adolescentes cumprindo medidas de privação e de restrição de liberdade.xlsx",
  casamentos = "cidadania/Número de casamentos registrados crianças e adolescentes do sexo feminino.xlsx",
  tip_ocup   = "cidadania/Proporção da população ocupada de 14 a 17 anos de idade desempenhando atividades identificadas às piores formas do trabalho infantil (Lista TIP).xlsx",
  ti_ocup    = "cidadania/Proporção de adolescentes de 14 a 17 anos de idade em situação de trabalho infantil em relação à população ocupada desta faixa etária.xlsx",
  acid_idade = "cidadania/Notificações acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade.xlsx",
  acid_cor   = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo cor_raça.xlsx",
  acid_evol  = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo evolução dos casos.xlsx",
  acid_sexo  = "cidadania/Notificações de acidentes de trabalho entre crianças e adolescentes entre 5 e 17 anos de idade segundo sexo.xlsx",
  mort       = "saude/Taxa de mortalidade atribuída a fontes de água inadequadas, saneamento inadequado e falta de higiene segundo grupos etários.xlsx",
  ti_econ    = "cidadania/População de 5 a 17 anos de idade em situação de trabalho infantil que realizaram atividades econômicas ou autoconsumo (a partir de 2016).xlsx",
  ti_tip     = "cidadania/População de 5 a 17 anos de idade em situação de trabalho infantil que realizaram atividades identificadas na Lista TIP das piores formas de trabalho infantil (a partir de 2016).xlsx",
  ti_autoc   = "cidadania/População de cinco a 17 anos de idade em situação de trabalho infantil que realizaram apenas atividades de autoconsumo (a partir de 2016).xlsx",
  ti_agro    = "cidadania/População com até 14 anos de idade ocupada em estabelecimentos agropecuários.xlsx"
)

# Decisões D-004/D-005/D-006: exclusões pontuais (sigla, tipo de série, ano, nível)
.abq_exclusoes <- data.frame(
  id = c("D-004", "D-005", "D-006"),
  env = c("AURORA_TI_AUTOCONSUMO_2022_DESCARTAR", "AURORA_TI_TIP_PCT_2022_UF_DESCARTAR", "AURORA_TI_ECON_1617_PCT_2017_DESCARTAR"),
  sigla_rx = c("^ti_autoc_(5_9|10_13|14_15|16_17)_abs$", "^ti_tip_(5_9|10_13|14_15|16_17)_pct$", "^ti_econ_16_17_pct$"),
  ano = c(2022, 2022, 2017),
  nivel = c("uf", "uf", "uf"),
  stringsAsFactors = FALSE
)

# Decisões "-" = 0: a coluna `traco_zero` do catálogo guarda o nome da variável de
# ambiente que revoga a decisão (NA = o "-" é descartado).
.abq_traco_decisoes <- data.frame(
  env  = c("AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO", "AURORA_CASAMENTOS_TRACO_COMO_ZERO"),
  id   = c("D-002", "D-007"),
  data = c("2026-11-11", "2026-11-16"),
  desc = c("planilhas de acidentes de trabalho", "planilhas de casamentos de meninas"),
  stringsAsFactors = FALSE
)

.abq_catalogo <- function() {
  acid <- function(arquivo, rotulo, sigla, nm, fantasia) {
    data.frame(arquivo = arquivo, rotulo = rotulo, serie = "valor",
               sigla_indicador = paste0("acid_trab_", sigla),
               nm_indicador = paste0("Acidentes de trabalho (5 a 17 anos) - ", nm),
               nm_fantasia = paste0("Acidentes de trabalho 5-17 anos (", fantasia, ")"),
               grupo = "cidadania", tipo = "absoluto", polaridade = "negativa",
               traco_zero = "AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO", stringsAsFactors = FALSE)
  }
  mort <- function(rotulo, sigla, faixa, taxa = TRUE) {
    f <- function(serie, pre, nm_pre, tipo, fan_pre) data.frame(
      arquivo = "mort", rotulo = rotulo, serie = serie,
      sigla_indicador = paste0("mort_sanea_", pre, "_", sigla),
      nm_indicador = paste0("Mortalidade por saneamento inadequado - ", nm_pre, " (", faixa, ")"),
      nm_fantasia = paste0("Mort. saneamento inadequado: ", fan_pre, " (", faixa, ")"),
      grupo = "saúde", tipo = tipo, polaridade = "negativa", traco_zero = NA_character_, stringsAsFactors = FALSE)
    rbind(if (taxa) f("taxa", "taxa", "Taxa por 100 mil hab.", "índice", "taxa"),
          f("abs", "abs", "Nº de óbitos", "absoluto", "nº"))
  }
  # Trabalho infantil (5 a 17 anos): pct = "taxa" (colunas "(%)"), abs = "abs" ou "valor" (sem sufixo)
  ti <- function(arquivo, rotulo, serie, sigla, nm, fantasia, tipo) data.frame(
    arquivo = arquivo, rotulo = rotulo, serie = serie, sigla_indicador = sigla,
    nm_indicador = nm, nm_fantasia = fantasia, grupo = "cidadania", tipo = tipo,
    polaridade = "negativa", traco_zero = NA_character_, stringsAsFactors = FALSE)
  ti_faixa <- function(arquivo, pre, nm_base, fan_base, rotulo, sigla, faixa, pct_txt, serie_abs = "abs", com_pct = TRUE) {
    rbind(
      if (com_pct) ti(arquivo, rotulo, "taxa", paste0(pre, "_", sigla, "_pct"),
                      paste0(nm_base, " - ", faixa, " - ", pct_txt),
                      paste0(fan_base, " (", faixa, ", %)"), "percentual"),
      ti(arquivo, rotulo, serie_abs, paste0(pre, "_", sigla, "_abs"),
         paste0(nm_base, " - ", faixa, " - Nº de pessoas"),
         paste0(fan_base, " (", faixa, ", nº)"), "absoluto"))
  }
  faixas <- list(c("5_9", "5 a 9 anos", "^5 a 9 anos de idade$"), c("10_13", "10 a 13 anos", "^10 a 13 anos de idade$"),
                 c("14_15", "14 a 15 anos", "^14 a 15 anos de idade$"), c("16_17", "16 a 17 anos", "^16 a 17 anos de idade$"))
  rot_total <- "^Total de população de 5 a 17 anos"
  ti_serie <- function(arquivo, pre, nm_base, fan_base, pct_faixa, pct_total, serie_abs = "abs", com_pct = TRUE) {
    do.call(rbind, c(
      list(ti_faixa(arquivo, pre, nm_base, fan_base, rot_total, "total", "Total 5 a 17 anos", pct_total, serie_abs, com_pct)),
      lapply(faixas, function(f) ti_faixa(arquivo, pre, nm_base, fan_base, f[3], f[1], f[2], pct_faixa, serie_abs, com_pct))))
  }
  ti_todos <- rbind(
    ti_serie("ti_econ", "ti_econ", "Trabalho infantil (5 a 17 anos) - Atividades econômicas ou autoconsumo",
             "Trabalho infantil: econômicas ou autoconsumo", "% da população da faixa etária", "% da população de 5 a 17 anos"),
    ti_serie("ti_tip", "ti_tip", "Trabalho infantil (5 a 17 anos) - Atividades da Lista TIP (piores formas)",
             "Trabalho infantil: Lista TIP", "% do total em atividades da Lista TIP", "% do total em trabalho infantil"),
    ti_serie("ti_autoc", "ti_autoc", "Trabalho infantil (5 a 17 anos) - Apenas autoconsumo",
             "Trabalho infantil: apenas autoconsumo", NA, NA, serie_abs = "valor", com_pct = FALSE),
    ti("ti_agro", "^Menores de 14 anos", "taxa", "ocup_agro_ate14_pct",
       "População até 14 anos ocupada em estabelecimentos agropecuários - % da população até 14 anos",
       "Ocupados em estabelecimentos agropecuários até 14 anos (%)", "percentual"),
    ti("ti_agro", "^Menores de 14 anos", "abs", "ocup_agro_ate14_abs",
       "População até 14 anos ocupada em estabelecimentos agropecuários - Nº de pessoas",
       "Ocupados em estabelecimentos agropecuários até 14 anos (nº)", "absoluto")
  )

  # --- Lote 3 (2026-11-16): Gini, medidas socioeducativas, casamentos, Lista TIP 14-17 (trimestral) ---
  simples <- function(arquivo, rotulo, serie, sigla, nm, fantasia, grupo, tipo, traco_zero = NA_character_) data.frame(
    arquivo = arquivo, rotulo = rotulo, serie = serie, sigla_indicador = sigla,
    nm_indicador = nm, nm_fantasia = fantasia, grupo = grupo, tipo = tipo,
    polaridade = "negativa", traco_zero = traco_zero, stringsAsFactors = FALSE)
  socioedu <- function(rotulo, sigla, nm, fantasia) simples(
    "liberdade", rotulo, "valor", paste0("socioedu_medida_", sigla),
    paste0("Adolescentes em medidas socioeducativas de privação/restrição de liberdade - ", nm),
    paste0("Adolescentes privados de liberdade (", fantasia, ")"), "cidadania", "absoluto")
  casam <- function(rotulo, sigla, nm, fantasia) simples(
    "casamentos", rotulo, "valor", paste0("casamento_fem_", sigla),
    paste0("Casamentos registrados de crianças e adolescentes do sexo feminino - ", nm),
    paste0("Casamentos de meninas (", fantasia, ")"), "cidadania", "absoluto",
    traco_zero = "AURORA_CASAMENTOS_TRACO_COMO_ZERO")                                   # D-007
  trimestral <- function(arquivo, rotulo, pre, nm_base, fan_base, pct_txt) rbind(
    simples(arquivo, rotulo, "taxa", paste0(pre, "_pct"), paste0(nm_base, " - ", pct_txt),
            paste0(fan_base, " (%)"), "cidadania", "percentual"),
    simples(arquivo, rotulo, "abs", paste0(pre, "_abs"), paste0(nm_base, " - Nº de pessoas"),
            paste0(fan_base, " (nº)"), "cidadania", "absoluto"))
  lote3 <- rbind(
    simples("gini", "^Coeficiente Gini", "valor", "gini_desigualdade",
            "Coeficiente de Gini de desigualdade", "Coeficiente de Gini", "economia", "índice"),
    socioedu("/ Internação$",            "internacao",        "Internação",            "internação"),
    socioedu("/ Internação provisória$", "intern_provisoria", "Internação provisória", "internação provisória"),
    socioedu("/ Semiliberdade$",         "semiliberdade",     "Semiliberdade",         "semiliberdade"),
    socioedu("/ Internação Sanção$",     "intern_sancao",     "Internação sanção",     "internação sanção"),
    socioedu("^Outros$",                 "outros",            "Outros",                "outros"),
    socioedu("^Total de adolescentes",   "total",             "Total",                 "total"),
    casam("^Total de casamentos de meninas menores de 19 anos", "ate19_total", "Total de meninas menores de 19 anos", "total, menores de 19 anos"),
    casam("^15 a 19 anos$",       "15_19", "15 a 19 anos",       "15 a 19 anos"),
    casam("^Menores de 15 anos$", "ate15", "Menores de 15 anos", "menores de 15 anos"),
    trimestral("tip_ocup", "^Proporção da população ocupada de 14 a 17 anos", "tip_ocup_14_17",
               "Ocupados de 14 a 17 anos em atividades da Lista TIP (piores formas de trabalho infantil), trimestral",
               "Ocupados 14-17 anos em atividades da Lista TIP, trimestral", "% da população ocupada de 14 a 17 anos"),
    trimestral("ti_ocup", "^Proporção de adolescentes de 14 a 17 anos", "ti_ocup_14_17",
               "Adolescentes de 14 a 17 anos em situação de trabalho infantil, trimestral",
               "Trabalho infantil 14-17 anos sobre ocupados, trimestral", "% da população ocupada de 14 a 17 anos")
  )

  rbind(
    lote3,
    ti_todos,
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
  ano_max_taxa_mun <- suppressWarnings(as.numeric(Sys.getenv("AURORA_MORT_SANEA_TAXA_MUN_ANO_MAX", "2019")))
  if (is.na(ano_max_taxa_mun)) ano_max_taxa_mun <- 2019
  n_traco <- setNames(rep(0L, nrow(.abq_traco_decisoes)), .abq_traco_decisoes$env)   # D-002, D-007
  n_taxa_cortada <- 0L
  n_excl <- setNames(rep(0L, nrow(.abq_exclusoes)), .abq_exclusoes$id)
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
      # Colunas trimestrais ("Trimestre 1/2012" = %, "Trimestre 1/2012 (absoluto)" = nº):
      # ano + mes_referencia = "T1".."T4"; colunas anuais ficam com mes_referencia NA.
      d$trimestre <- stringr::str_match(d$coluna, "^Trimestre ([1-4])/")[, 2]
      eh_abs <- grepl("absoluto", d$coluna)
      d$serie_col <- ifelse(grepl("[(]%[)]", d$coluna) | (!is.na(d$trimestre) & !eh_abs), "taxa",
                            ifelse(eh_abs, "abs", "valor"))
      d <- d[d$serie_col == cl$serie & !is.na(d$ano), ]
      d$indicador <- stringr::str_trim(d$indicador)

      eh_traco <- !is.na(d$indicador) & d$indicador == "-"
      if (!is.na(cl$traco_zero) && .abq_flag(cl$traco_zero, "true")) {             # D-002, D-007
        n_traco[cl$traco_zero] <- n_traco[cl$traco_zero] + sum(eh_traco)
        d$indicador[eh_traco] <- "0"
      }
      d <- d[!is.na(d$indicador) & !d$indicador %in% c("-", "ND", "nan", "NA"), ]

      for (j in seq_len(nrow(.abq_exclusoes))) {                              # D-004, D-005, D-006
        ex <- .abq_exclusoes[j, ]
        if (nivel == ex$nivel && grepl(ex$sigla_rx, cl$sigla_indicador) &&
            .abq_flag(ex$env, "true")) {
          sai <- d$ano == ex$ano
          n_excl[ex$id] <- n_excl[ex$id] + sum(sai)
          d <- d[!sai, ]
        }
      }

      if (cl$serie == "taxa") {
        if (nivel == "municipio" && cl$arquivo == "mort") {                   # D-003
          n_taxa_cortada <- n_taxa_cortada + sum(d$ano > ano_max_taxa_mun)
          d <- d[d$ano <= ano_max_taxa_mun, ]
        }
        d$indicador <- as.character(round(as.numeric(d$indicador), 2))        # remove ruído de ponto flutuante
      }

      d$sigla_indicador <- cl$sigla_indicador
      d$nm_indicador <- cl$nm_indicador
      d$mes_referencia <- ifelse(is.na(d$trimestre), NA_character_, paste0("T", d$trimestre))
      out[[length(out) + 1]] <- d
    }
  }
  d <- dplyr::bind_rows(out)

  for (j in seq_len(nrow(.abq_traco_decisoes))) {                                   # D-002, D-007
    td <- .abq_traco_decisoes[j, ]
    if (.abq_flag(td$env, "true")) {
      .abq_log(sprintf("DECISAO REVISAVEL %s (%s): %d celulas '-' das %s (%s) foram convertidas em 0. Revogar: %s=false.",
                       td$id, td$data, n_traco[[td$env]], td$desc, nivel, td$env))
    } else {
      .abq_log(sprintf("%s REVOGADA por variavel de ambiente %s: celulas '-' das %s (%s) descartadas, nao convertidas em 0.",
                       td$id, td$env, td$desc, nivel))
    }
  }
  if (nivel == "uf") {
    desc <- c("D-004" = "apenas autoconsumo: 2022 (copia de 2019 na fonte) descartado em 4 series",
              "D-005" = "Lista TIP: % de 2022 das UFs (escala errada na fonte) descartado em 4 faixas",
              "D-006" = "economicas ou autoconsumo 16-17 anos: % de 2017 (~2x o esperado) descartado")
    for (j in seq_len(nrow(.abq_exclusoes))) {
      ex <- .abq_exclusoes[j, ]
      if (.abq_flag(ex$env, "true")) {
        .abq_log(sprintf("DECISAO REVISAVEL %s (2026-11-12): %s (%d valores). Revogar: %s=false.", ex$id, desc[[ex$id]], n_excl[[ex$id]], ex$env))
      } else {
        .abq_log(sprintf("%s REVOGADA por variavel de ambiente %s: dados mantidos como vieram da fonte.", ex$id, ex$env))
      }
    }
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

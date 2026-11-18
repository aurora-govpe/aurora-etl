# ==============================================================================
# R/abrinq_planilhas.R
# Lote de planilhas do Observatório da Criança e do Adolescente (Fundação Abrinq):
#   - Notificações de acidentes de trabalho de crianças e adolescentes (5 a 17 anos):
#       total, por faixa etária, cor/raça, sexo e evolução dos casos (tema: cidadania)
#   - Taxa de mortalidade atribuída a fontes de água inadequadas, saneamento
#       inadequado e falta de higiene, por grupos etários (tema: saúde)
#   - Demais lotes (trabalho infantil, Gini, socioeducativas, casamentos, Lista TIP
#       14-17, suicídio e lesões autoprovocadas até 19 anos): ver as seções de decisões abaixo
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
#
# --- Lote 4: óbitos por suicídio de indivíduos até 19 anos (usuária Kattine Costa
#     Pedrosa, 2026-11-17) ---
# D-008  Suicídio até 19 anos: o "-" vale 0 óbito (municípios e UFs), nas 4 planilhas.
#        Evidência: com o "-" = 0, a soma das categorias bate com o total em 5.538 de
#        5.538 combinações localidade x ano na escolaridade e nos grupos etários
#        (0 a 19 anos); nos totais o "-" e o "0" se alternam para o mesmo fato.
#        Revogar: AURORA_SUICIDIO_TRACO_COMO_ZERO=false.
# D-009  Suicídio até 19 anos, planilha por grupos etários: 2007 e 2020 descartados
#        (municípios e UFs). 2007 é cópia de 2008 na fonte (71% das células iguais;
#        nos outros pares de anos, ~10%); em 2020 o total 0-19 diverge das outras três
#        planilhas, que concordam entre si (PE: 34 contra 25).
#        Revogar: AURORA_SUICIDIO_IDADE_2007_2020_DESCARTAR=false.
#
# Notas (não são decisões):
#  - O total até 19 anos sai só da planilha por escolaridade (igual ao da cor/raça). Os
#    totais das planilhas por sexo (municípios de 2017 errados: PE soma 19 contra 44) e
#    por grupos etários ficam de fora, assim como a aba "Menores de 19 anos" (até 2023).
#  - Fora do lote: faixas de 20 anos ou mais, total de todas as idades e idade ignorada
#    (fora do recorte "até 19 anos"); "Sexo ignorado" (tudo "-"); "Menores de 1 ano" e
#    "1 a 4 anos" (12 casos em todo o país, nenhum em PE).
#  - Cor/raça 2024: em 7 UFs a soma das categorias fica 1 abaixo do total (fonte).
#
# --- Lote 5: notificações de lesões autoprovocadas de crianças e adolescentes até
#     19 anos (usuária Kattine Costa Pedrosa, 2026-11-18) ---
# D-010  Lesões autoprovocadas até 19 anos: o "-" vale 0 notificação (municípios e UFs),
#        nas 4 planilhas. Evidência: com o "-" = 0, a soma das categorias bate com o
#        total em 3.621 de 3.621 combinações localidade x ano nas 4 planilhas, e a soma
#        dos municípios de PE bate com a UF em todos os anos.
#        Revogar: AURORA_LESOES_TRACO_COMO_ZERO=false.
# D-011  Lesões autoprovocadas até 19 anos, planilha por sexo: 2024 descartado (municípios
#        e UFs). Na fonte, 2024 é cópia de 2023 (100% das células com valor iguais; nos
#        outros pares de anos, nenhum acima de 30%); o total 0-19 dessa planilha em 2024
#        diverge das outras três, que concordam entre si (PE: 2.155 contra 2.226).
#        Revogar: AURORA_LESOES_SEXO_2024_DESCARTAR=false.
#
# Notas (não são decisões): o que ficou de fora vai para o log a cada execução
# (`.abq_notas_lesoes`).
# ==============================================================================

ABRINQ_ARQUIVOS <- c(
  lesao_sexo  = "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo sexo.xlsx",
  lesao_cor   = "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo cor_raça.xlsx",
  lesao_escol = "saude/Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade segundo escolaridade.xlsx",
  lesao_idade = "saude/Notificações de lesões autoprovocadas segundo grupos etários.xlsx",
  suic_sexo  = "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo sexo.xlsx",
  suic_cor   = "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo cor_raça.xlsx",
  suic_escol = "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo escolaridade.xlsx",
  suic_idade = "saude/Número de óbitos de indivíduos de até 19 anos de idade por suicídio segundo grupos etários.xlsx",
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

# Decisões D-004/D-005/D-006/D-009/D-011: exclusões pontuais (sigla, anos, níveis; anos e
# níveis separados por vírgula)
.abq_exclusoes <- data.frame(
  id = c("D-004", "D-005", "D-006", "D-009", "D-011"),
  env = c("AURORA_TI_AUTOCONSUMO_2022_DESCARTAR", "AURORA_TI_TIP_PCT_2022_UF_DESCARTAR",
          "AURORA_TI_ECON_1617_PCT_2017_DESCARTAR", "AURORA_SUICIDIO_IDADE_2007_2020_DESCARTAR",
          "AURORA_LESOES_SEXO_2024_DESCARTAR"),
  sigla_rx = c("^ti_autoc_(5_9|10_13|14_15|16_17)_abs$", "^ti_tip_(5_9|10_13|14_15|16_17)_pct$",
               "^ti_econ_16_17_pct$", "^suicidio_ate19_idade_", "^lesao_autoprov_ate19_sexo_"),
  ano = c("2022", "2022", "2017", "2007,2020", "2024"),
  nivel = c("uf", "uf", "uf", "uf,municipio", "uf,municipio"),
  data = c("2026-11-12", "2026-11-12", "2026-11-12", "2026-11-17", "2026-11-18"),
  desc = c("apenas autoconsumo: 2022 (copia de 2019 na fonte) descartado em 4 series",
           "Lista TIP: % de 2022 das UFs (escala errada na fonte) descartado em 4 faixas",
           "economicas ou autoconsumo 16-17 anos: % de 2017 (~2x o esperado) descartado",
           paste("suicidio ate 19 anos por grupos etarios: 2007 e 2020 descartados nas faixas 5-9, 10-14 e 15-19.",
                 "2007 e copia de 2008 na fonte (71% das celulas iguais; ~10% nos outros pares de anos);",
                 "em 2020 o total 0-19 diverge das planilhas por sexo, cor/raca e escolaridade, que concordam entre si (PE: 34 contra 25)"),
           paste("lesoes autoprovocadas ate 19 anos por sexo: 2024 descartado (feminino, masculino e ignorado).",
                 "2024 e copia de 2023 na fonte (100% das celulas com valor iguais); o total 0-19 dessa planilha em 2024",
                 "diverge das planilhas por cor/raca, escolaridade e grupos etarios, que concordam entre si (PE: 2155 contra 2226)")),
  stringsAsFactors = FALSE
)
.abq_lista <- function(x) trimws(strsplit(x, ",")[[1]])

# Lote 4 (suicídio até 19 anos): o que ficou de fora e por quê; vai para o log a cada execução
# (usuária Kattine Costa Pedrosa, 2026-11-17).
.abq_notas_suicidio <- c(
  "total ate 19 anos carregado so da planilha por escolaridade (igual ao da cor/raca); os totais das planilhas por sexo (municipios de 2017 errados na fonte: PE soma 19 contra 44) e por grupos etarios (2007 e 2020, ver D-009) nao foram carregados",
  "aba 'Menores de 19 anos' da planilha por grupos etarios nao carregada (vai so ate 2023 e repete o total 0-19)",
  "faixas de 20 anos ou mais, total de todas as idades e idade ignorada nao carregados (fora do recorte 'ate 19 anos' do lote)",
  "'Sexo ignorado' nao carregado (todas as celulas sao '-')",
  "'Menores de 1 ano' e '1 a 4 anos' nao carregados (12 casos em todo o pais em 2000-2025, nenhum em PE)",
  "cor/raca 2024: em 7 UFs a soma das categorias fica 1 abaixo do total (dado da fonte, mantido como veio)"
)

# Lote 5 (lesões autoprovocadas até 19 anos): o que ficou de fora e por quê; vai para o log
# a cada execução (usuária Kattine Costa Pedrosa, 2026-11-18).
.abq_notas_lesoes <- c(
  "total ate 19 anos carregado so da planilha por cor/raca (igual ao da escolaridade e ao 0-19 dos grupos etarios); o total 0-19 da planilha por sexo nao foi carregado (2024 copiado de 2023, ver D-011)",
  "faixas de 20 anos ou mais, 'Numero de casos notificados' (todas as idades) e idade ignorada nao carregados (fora do recorte 'ate 19 anos' do lote)",
  "'Menores de 1 ano' e '1 a 4 anos' foram carregados como vieram da fonte (PE: 188 e 288 notificacoes em 2009-2025), embora lesao autoprovocada nessas idades seja improvavel; conferir com a area tecnica"
)

# Decisões "-" = 0: a coluna `traco_zero` do catálogo guarda o nome da variável de
# ambiente que revoga a decisão (NA = o "-" é descartado).
.abq_traco_decisoes <- data.frame(
  env  = c("AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO", "AURORA_CASAMENTOS_TRACO_COMO_ZERO",
           "AURORA_SUICIDIO_TRACO_COMO_ZERO", "AURORA_LESOES_TRACO_COMO_ZERO"),
  id   = c("D-002", "D-007", "D-008", "D-010"),
  data = c("2026-11-11", "2026-11-16", "2026-11-17", "2026-11-18"),
  desc = c("planilhas de acidentes de trabalho", "planilhas de casamentos de meninas",
           "planilhas de suicidio ate 19 anos", "planilhas de lesoes autoprovocadas ate 19 anos"),
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

  # --- Lote 4 (2026-11-17): óbitos por suicídio de indivíduos até 19 anos ---
  suic <- function(arquivo, rotulo, sigla, nm, fantasia) simples(
    arquivo, rotulo, "valor", paste0("suicidio_ate19_", sigla),
    paste0("Óbitos por suicídio de indivíduos até 19 anos - ", nm),
    paste0("Suicídios até 19 anos (", fantasia, ")"), "saúde", "absoluto",
    traco_zero = "AURORA_SUICIDIO_TRACO_COMO_ZERO")                                     # D-008
  lote4 <- rbind(
    suic("suic_escol", "^Número de óbitos de indivíduos de até 19 anos de idade por suicídio$", "total", "Total", "total"),
    suic("suic_sexo", "^Feminino$",  "sexo_feminino",  "Sexo Feminino",  "sexo feminino"),
    suic("suic_sexo", "^Masculino$", "sexo_masculino", "Sexo Masculino", "sexo masculino"),
    suic("suic_cor", "^Branca$",            "cor_branca",   "Cor/raça Branca",   "cor/raça branca"),
    suic("suic_cor", "^Preta$",             "cor_preta",    "Cor/raça Preta",    "cor/raça preta"),
    suic("suic_cor", "^Parda$",             "cor_parda",    "Cor/raça Parda",    "cor/raça parda"),
    suic("suic_cor", "^Amarela$",           "cor_amarela",  "Cor/raça Amarela",  "cor/raça amarela"),
    suic("suic_cor", "^Indígena$",          "cor_indigena", "Cor/raça Indígena", "cor/raça indígena"),
    suic("suic_cor", "^Cor/raça ignorada$", "cor_ignorada", "Cor/raça Ignorada", "cor/raça ignorada"),
    suic("suic_escol", "^Nenhuma escolaridade$",  "escol_nenhuma",  "Escolaridade: Nenhuma",          "nenhuma escolaridade"),
    suic("suic_escol", "^1 a 3 de anos$",         "escol_1_3",      "Escolaridade: 1 a 3 anos",       "1 a 3 anos de estudo"),
    suic("suic_escol", "^4 a 7 anos$",            "escol_4_7",      "Escolaridade: 4 a 7 anos",       "4 a 7 anos de estudo"),
    suic("suic_escol", "^8 a 11 anos$",           "escol_8_11",     "Escolaridade: 8 a 11 anos",      "8 a 11 anos de estudo"),
    suic("suic_escol", "^12 anos e mais$",        "escol_12_mais",  "Escolaridade: 12 anos ou mais",  "12 anos ou mais de estudo"),
    suic("suic_escol", "^Escolaridade ignorada$", "escol_ignorada", "Escolaridade: Ignorada",         "escolaridade ignorada"),
    suic("suic_idade", "^5 a 9 anos de idade$",   "idade_5_9",   "5 a 9 anos",   "5 a 9 anos"),                    # D-009
    suic("suic_idade", "^10 a 14 anos de idade$", "idade_10_14", "10 a 14 anos", "10 a 14 anos"),
    suic("suic_idade", "^15 a 19 anos de idade$", "idade_15_19", "15 a 19 anos", "15 a 19 anos")
  )

  # --- Lote 5 (2026-11-18): notificações de lesões autoprovocadas até 19 anos ---
  lesao <- function(arquivo, rotulo, sigla, nm, fantasia) simples(
    arquivo, rotulo, "valor", paste0("lesao_autoprov_ate19_", sigla),
    paste0("Notificações de lesões autoprovocadas de crianças e adolescentes até 19 anos - ", nm),
    paste0("Lesões autoprovocadas até 19 anos (", fantasia, ")"), "saúde", "absoluto",
    traco_zero = "AURORA_LESOES_TRACO_COMO_ZERO")                                       # D-010
  lote5 <- rbind(
    lesao("lesao_cor", "^Notificações de lesões autoprovocadas de crianças e adolescentes de até 19 anos de idade$", "total", "Total", "total"),
    lesao("lesao_sexo", "^Feminino$",      "sexo_feminino", "Sexo Feminino", "sexo feminino"),          # D-011
    lesao("lesao_sexo", "^Masculino$",     "sexo_masculino", "Sexo Masculino", "sexo masculino"),
    lesao("lesao_sexo", "^Sexo ignorado$", "sexo_ignorado", "Sexo Ignorado", "sexo ignorado"),
    lesao("lesao_cor", "^Branca$",            "cor_branca",   "Cor/raça Branca",   "cor/raça branca"),
    lesao("lesao_cor", "^Preta$",             "cor_preta",    "Cor/raça Preta",    "cor/raça preta"),
    lesao("lesao_cor", "^Parda$",             "cor_parda",    "Cor/raça Parda",    "cor/raça parda"),
    lesao("lesao_cor", "^Amarela$",           "cor_amarela",  "Cor/raça Amarela",  "cor/raça amarela"),
    lesao("lesao_cor", "^Indígena$",          "cor_indigena", "Cor/raça Indígena", "cor/raça indígena"),
    lesao("lesao_cor", "^Cor/raça ignorada$", "cor_ignorada", "Cor/raça Ignorada", "cor/raça ignorada"),
    lesao("lesao_escol", "^Analfabeto$", "escol_analfabeto", "Escolaridade: Analfabeto", "analfabeto"),
    lesao("lesao_escol", "^1ª a 4ª série incompleta do Ensino Fundamental$", "escol_ef_1_4_incompleta",
          "Escolaridade: 1ª a 4ª série incompleta do Ensino Fundamental", "1ª a 4ª série incompleta"),
    lesao("lesao_escol", "^4ª série completa do Ensino Fundamental$", "escol_ef_4_completa",
          "Escolaridade: 4ª série completa do Ensino Fundamental", "4ª série completa"),
    lesao("lesao_escol", "^5ª a 8ª série incompleta do Ensino Fundamental$", "escol_ef_5_8_incompleta",
          "Escolaridade: 5ª a 8ª série incompleta do Ensino Fundamental", "5ª a 8ª série incompleta"),
    lesao("lesao_escol", "^Ensino Fundamental completo$", "escol_ef_completo", "Escolaridade: Ensino Fundamental completo", "fundamental completo"),
    lesao("lesao_escol", "^Ensino Médio incompleto$",     "escol_em_incompleto", "Escolaridade: Ensino Médio incompleto", "médio incompleto"),
    lesao("lesao_escol", "^Ensino Médio completo$",       "escol_em_completo", "Escolaridade: Ensino Médio completo", "médio completo"),
    lesao("lesao_escol", "^Educação superior incompleta$", "escol_sup_incompleta", "Escolaridade: Educação superior incompleta", "superior incompleta"),
    lesao("lesao_escol", "^Educação superior completa$",   "escol_sup_completa", "Escolaridade: Educação superior completa", "superior completa"),
    lesao("lesao_escol", "^Não se aplica$",         "escol_nao_se_aplica", "Escolaridade: Não se aplica", "escolaridade não se aplica"),
    lesao("lesao_escol", "^Escolaridade ignorada$", "escol_ignorada", "Escolaridade: Ignorada", "escolaridade ignorada"),
    lesao("lesao_idade", "^Menores de 1 ano de idade$", "idade_menor_1", "Menores de 1 ano", "menores de 1 ano"),
    lesao("lesao_idade", "^1 a 4 anos de idade$",   "idade_1_4",   "1 a 4 anos",   "1 a 4 anos"),
    lesao("lesao_idade", "^5 a 9 anos de idade$",   "idade_5_9",   "5 a 9 anos",   "5 a 9 anos"),
    lesao("lesao_idade", "^10 a 14 anos de idade$", "idade_10_14", "10 a 14 anos", "10 a 14 anos"),
    lesao("lesao_idade", "^15 a 19 anos de idade$", "idade_15_19", "15 a 19 anos", "15 a 19 anos")
  )

  rbind(
    lote5,
    lote4,
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

      for (j in seq_len(nrow(.abq_exclusoes))) {                              # D-004, D-005, D-006, D-009
        ex <- .abq_exclusoes[j, ]
        if (nivel %in% .abq_lista(ex$nivel) && grepl(ex$sigla_rx, cl$sigla_indicador) &&
            .abq_flag(ex$env, "true")) {
          sai <- d$ano %in% as.numeric(.abq_lista(ex$ano))
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
  for (j in seq_len(nrow(.abq_exclusoes))) {                                        # D-004, D-005, D-006, D-009
    ex <- .abq_exclusoes[j, ]
    if (!nivel %in% .abq_lista(ex$nivel)) next
    if (.abq_flag(ex$env, "true")) {
      .abq_log(sprintf("DECISAO REVISAVEL %s (%s): %s (%s, %d valores). Revogar: %s=false.", ex$id, ex$data, ex$desc, nivel, n_excl[[ex$id]], ex$env))
    } else {
      .abq_log(sprintf("%s REVOGADA por variavel de ambiente %s: dados mantidos como vieram da fonte.", ex$id, ex$env))
    }
  }
  if (any(startsWith(names(ABRINQ_ARQUIVOS), "suic_"))) {                          # lote 4: o que ficou de fora e por que
    for (nota in .abq_notas_suicidio) .abq_log(sprintf("NOTA LOTE SUICIDIO ATE 19 ANOS (2026-11-17, %s): %s", nivel, nota))
  }
  if (any(startsWith(names(ABRINQ_ARQUIVOS), "lesao_"))) {                         # lote 5: o que ficou de fora e por que
    for (nota in .abq_notas_lesoes) .abq_log(sprintf("NOTA LOTE LESOES AUTOPROVOCADAS ATE 19 ANOS (2026-11-18, %s): %s", nivel, nota))
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

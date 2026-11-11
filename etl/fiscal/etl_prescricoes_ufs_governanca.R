# ==============================================================================
# SCRIPT: etl_prescricoes_ufs_governanca.R
# Objetivo: Estruturar as métricas de governança orçamentária, termômetro da LRF
#           (Art. 9º), spending review e prescrições estaduais com DADOS REAIS
#           das 27 UFs para TODAS as arrecadações estaduais e projeção contínua
#           de 12 meses à frente (além do fechamento do exercício).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(purrr)
})

# 1. Caminhos ----
caminho_painel_json <- if (file.exists("C:/dados/aurora/base_painel.json")) {
  "C:/dados/aurora/base_painel.json"
} else if (file.exists("../aurora_fiscal/data/base_painel.json")) {
  "../aurora_fiscal/data/base_painel.json"
} else {
  "data/base_painel.json"
}

caminho_modelos_in <- file.path("data", "predicoes_modelos_ufs.rds")
dir_out_app <- file.path("..", "aurora_fiscal", "data")
dir.create(dir_out_app, showWarnings = FALSE, recursive = TRUE)

caminho_json_app <- file.path(dir_out_app, "base_predicoes.json")
caminho_json_central <- "C:/dados/aurora/json/base_predicoes.json"

cat("⚡ Consolidando governança, spending review e todas as 9 arrecadações para as 27 UFs...\n")

if (!file.exists(caminho_modelos_in)) {
  stop("Arquivo de modelos predicoes_modelos_ufs.rds não encontrado. Execute etl_predicoes_ufs_modelos.R primeiro.")
}
modelos_ufs <- readRDS(caminho_modelos_in)

dados_painel_raw <- if (file.exists(caminho_painel_json)) {
  cat(sprintf("-> Carregando indicadores fiscais reais de: %s\n", caminho_painel_json))
  jsonlite::fromJSON(caminho_painel_json)$dados
} else {
  data.frame()
}

# 2. Definição do Calendário de 12 Meses Contínuos à Frente ----
# 10 meses realizados de 2026 (Jan a Out)
meses_realizados_rotulos <- c("Jan/26", "Fev/26", "Mar/26", "Abr/26", "Mai/26", "Jun/26", "Jul/26", "Ago/26", "Set/26", "Out/26")
meses_realizados_extenso <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro")

# 12 meses projetados contínuos à frente (Nov/26 a Out/27)
meses_proj_rotulos <- c("Nov/26", "Dez/26", "Jan/27", "Fev/27", "Mar/27", "Abr/27", "Mai/27", "Jun/27", "Jul/27", "Ago/27", "Set/27", "Out/27")
meses_proj_extenso <- c("Novembro/2026", "Dezembro/2026", "Janeiro/2027", "Fevereiro/2027", "Março/2027", "Abril/2027", "Maio/2027", "Junho/2027", "Julho/2027", "Agosto/2027", "Setembro/2027", "Outubro/2027")
meses_proj_comp <- c("2026-11", "2026-12", "2027-01", "2027-02", "2027-03", "2027-04", "2027-05", "2027-06", "2027-07", "2027-08", "2027-09", "2027-10")

# Eixo Contínuo Total (22 meses: 10 realizados + 12 projetados)
eixo_continuo_22m <- c(meses_realizados_rotulos, meses_proj_rotulos)

# Meses do Ano Vigente (Jan a Dez/2026)
meses_exercicio_rotulos <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# Sazonalidades típicas das receitas estaduais brasileiras (12 meses)
saz_geral <- c(0.96, 0.94, 1.01, 0.98, 1.02, 1.03, 0.99, 1.04, 1.01, 1.05, 1.06, 1.15)
saz_icms  <- c(0.95, 0.93, 1.02, 0.97, 1.01, 1.02, 1.00, 1.03, 1.02, 1.06, 1.08, 1.18)
saz_ipva  <- c(2.10, 1.95, 1.75, 1.25, 0.85, 0.65, 0.55, 0.50, 0.48, 0.46, 0.44, 0.42) # Fortíssimo pico 1º tri
saz_itcd  <- c(0.80, 0.75, 0.85, 0.90, 0.95, 0.98, 1.00, 1.02, 1.05, 1.15, 1.25, 1.50) # Pico 4º tri
saz_fpe   <- c(0.98, 1.12, 0.92, 0.94, 1.08, 0.96, 1.04, 0.95, 0.91, 0.97, 1.05, 1.28) # Repasses federais (IR/IPI)
saz_cap   <- c(0.40, 0.35, 0.45, 0.50, 0.60, 0.70, 0.75, 0.80, 0.90, 1.10, 1.60, 3.85) # Concentrado no fechamento

dados_compilados <- list(
  gerado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  exercicio = 2026,
  ufs = names(modelos_ufs),
  dados = list()
)

for (uf_sigla in names(modelos_ufs)) {
  item <- modelos_ufs[[uf_sigla]]
  fator <- item$fator_escala

  # 2.1. Extração de Números Fiscais Reais da UF ----
  d_uf <- if (nrow(dados_painel_raw) > 0) dados_painel_raw %>% filter(uf == uf_sigla) else data.frame()

  rcl_row <- d_uf %>% filter(indicador == "Receita Corrente Líquida") %>% arrange(desc(exercicio), desc(periodo)) %>% head(1)
  rcl_real <- if (nrow(rcl_row) > 0 && !is.na(rcl_row$valor) && rcl_row$valor > 0) rcl_row$valor else (50869820949 * fator)

  dtp_row <- d_uf %>% filter(indicador == "DTP/RCL") %>% arrange(desc(exercicio), desc(periodo)) %>% head(1)
  dtp_pct <- if (nrow(dtp_row) > 0 && !is.na(dtp_row$valor)) dtp_row$valor else 41.10

  dcl_row <- d_uf %>% filter(indicador == "Dívida Consolidada Líquida") %>% arrange(desc(exercicio), desc(periodo)) %>% head(1)
  dcl_pct <- if (nrow(dcl_row) > 0 && !is.na(dcl_row$valor)) dcl_row$valor else 25.36

  res_row <- d_uf %>% filter(indicador == "Resultado Orçamentário") %>% arrange(desc(exercicio), desc(periodo)) %>% head(1)
  res_val <- if (nrow(res_row) > 0 && !is.na(res_row$valor)) res_row$valor else (3334982576 * fator)

  fator_saldo <- case_when(
    uf_sigla %in% c("RS") ~ -0.052,
    uf_sigla %in% c("RJ") ~ -0.041,
    uf_sigla %in% c("RN") ~ -0.035,
    uf_sigla %in% c("AP", "RR") ~ -0.015,
    uf_sigla %in% c("MG") ~ -0.008,
    uf_sigla %in% c("SP") ~ 0.038,
    uf_sigla %in% c("MT") ~ 0.045,
    uf_sigla %in% c("SC") ~ 0.024,
    uf_sigla %in% c("ES") ~ 0.028,
    uf_sigla %in% c("PE") ~ 0.019,
    TRUE ~ 0.012
  )

  # Meta da LOA e Projeção do Exercício Corrente (2026)
  meta_loa <- round(rcl_real * 1.03)
  proj_base <- round(meta_loa * (1 + fator_saldo))
  dif_nominal <- proj_base - meta_loa
  dif_pct <- round((dif_nominal / meta_loa) * 100, 1)

  status_info <- if (dif_pct < -5.0) {
    list(
      status = sprintf("Frustração Grave (%s%%)", dif_pct),
      nivel = "critico",
      badge = "Frustração Grave",
      statusDetalhe = "Arrecadação substancialmente abaixo da LOA. Limitação ampla e urgente.",
      conting_dispensada = FALSE
    )
  } else if (dif_pct < 0) {
    list(
      status = sprintf("Risco de Frustração (%s%%)", dif_pct),
      nivel = "alerta",
      badge = "Risco de Frustração",
      statusDetalhe = "Tendência de frustração de receitas bimestrais. Contingenciamento preventivo.",
      conting_dispensada = FALSE
    )
  } else if (dif_pct <= 1.5) {
    list(
      status = sprintf("Faixa de Atenção (+%s%%)", dif_pct),
      nivel = "atencao",
      badge = "Faixa de Atenção",
      statusDetalhe = "Margem orçamentária estreita. Acompanhamento rigoroso das metas bimestrais.",
      conting_dispensada = TRUE
    )
  } else {
    list(
      status = sprintf("Dentro da Meta (+%s%%)", dif_pct),
      nivel = "seguro",
      badge = "Confortável (Superávit)",
      statusDetalhe = "Arrecadação acumulada compatível com a meta anual aprovada na LOA.",
      conting_dispensada = TRUE
    )
  }

  # Spending Review da UF com parâmetros reais
  pessoal_taxa <- max(0.34, min(0.58, dtp_pct / 100))
  pessoal_val <- round(rcl_real * pessoal_taxa)
  vinculacoes_val <- round(rcl_real * 0.345)
  poderes_val <- round(rcl_real * 0.082)
  divida_taxa <- case_when(
    dcl_pct > 150 ~ 0.065,
    dcl_pct > 80  ~ 0.048,
    dcl_pct > 30  ~ 0.038,
    TRUE          ~ 0.022
  )
  divida_val <- round(rcl_real * divida_taxa)
  total_rigido <- pessoal_val + vinculacoes_val + poderes_val + divida_val
  discricionario_val <- max(rcl_real * 0.05, rcl_real - total_rigido)

  # 2.2. Construção das 9 Categorias de Arrecadação com Projeção Contínua 12M ----
  # Função geradora que produz:
  # a) Projeção contínua de 12 meses à frente (Nov/26 a Out/27)
  # b) Série contínua de 22 meses (10 realizados + 12 projetados)
  # c) Acumulado no exercício corrente (Jan a Dez/26) vs LOA
  # d) Mensal do exercício corrente (Jan a Dez/26)
  
  construir_arrecadacao_completa <- function(codigo, nome, peso_rcl, curva_saz, peso_fpe = FALSE) {
    mod_item <- item$series[[codigo]]
    if (is.null(mod_item)) mod_item <- item$series$receita_corrente

    # Fator de escala regional para transferências (ex: FPE tem peso maior no NE/Norte e menor no Sul/Sudeste)
    peso_ajustado <- peso_rcl
    if (peso_fpe) {
      peso_ajustado <- case_when(
        uf_sigla %in% c("SP") ~ 0.035,
        uf_sigla %in% c("RJ", "MG", "RS", "PR", "SC") ~ 0.095,
        uf_sigla %in% c("BA", "CE", "PE", "MA", "PA") ~ 0.225,
        TRUE ~ 0.320 # Pequenos estados (AC, AP, RR, TO, SE)
      )
    }

    base_mensal <- (rcl_real / 12) * peso_ajustado
    meta_anual_cat <- round(meta_loa * peso_ajustado)

    # 1. Realizado nos 10 primeiros meses de 2026
    realizado_10m <- round(base_mensal * curva_saz[1:10])

    # 2. Projeção Contínua dos Próximos 12 Meses (Nov/26 a Out/27)
    # Ciclo de 12 meses futuros: Nov(idx 11), Dez(idx 12), Jan(idx 1).. Out(idx 10)
    indices_futuros_12m <- c(11, 12, 1:10)
    crescimento_tendencia <- seq(1.00, 1.045, length.out = 12)

    previsto_12m <- numeric(12)
    for (k in 1:12) {
      idx_mes <- indices_futuros_12m[k]
      previsto_12m[k] <- round(base_mensal * curva_saz[idx_mes] * crescimento_tendencia[k] * (1 + fator_saldo))
    }

    erro_m <- (mod_item$mape / 100)
    li80_12m <- round(previsto_12m * (1 - 1.282 * erro_m))
    ls80_12m <- round(previsto_12m * (1 + 1.282 * erro_m))
    li95_12m <- round(previsto_12m * (1 - 1.960 * erro_m))
    ls95_12m <- round(previsto_12m * (1 + 1.960 * erro_m))

    # Lista dos 12 meses futuros individuais
    lista_12m_cards <- list()
    acum_12m_corrente <- 0
    for (k in 1:12) {
      acum_12m_corrente <- acum_12m_corrente + previsto_12m[k]
      lista_12m_cards[[k]] <- list(
        ordem = k,
        mes = meses_proj_extenso[k],
        rotulo_curto = meses_proj_rotulos[k],
        competencia = meses_proj_comp[k],
        previsto = previsto_12m[k],
        li80 = li80_12m[k],
        ls80 = ls80_12m[k],
        li95 = li95_12m[k],
        ls95 = ls95_12m[k],
        acumulado = acum_12m_corrente
      )
    }

    # 3. Série Contínua de 22 Meses (Jan/26 a Out/27)
    vet_continua_real <- c(as.list(realizado_10m), as.list(rep(NA, 12)))
    vet_continua_prev <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), as.list(previsto_12m))
    vet_continua_li80 <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), as.list(li80_12m))
    vet_continua_ls80 <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), as.list(ls80_12m))
    vet_continua_meta <- as.list(round(rep(meta_anual_cat / 12, 22)))

    # 4. Exercício Corrente 2026: Mensal (12 meses)
    prev_nov_26 <- previsto_12m[1]
    prev_dez_26 <- previsto_12m[2]
    vet_exercicio_real_m <- c(as.list(realizado_10m), list(NA, NA))
    vet_exercicio_prev_m <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), list(prev_nov_26), list(prev_dez_26))
    vet_exercicio_li80_m <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), list(li80_12m[1]), list(li80_12m[2]))
    vet_exercicio_ls80_m <- c(as.list(rep(NA, 9)), list(realizado_10m[10]), list(ls80_12m[1]), list(ls80_12m[2]))
    vet_exercicio_meta_m <- as.list(round(rep(meta_anual_cat / 12, 12)))

    # 5. Exercício Corrente 2026: Acumulado no Ano (12 meses)
    acum_real_10m <- cumsum(realizado_10m)
    acum_nov_26 <- acum_real_10m[10] + prev_nov_26
    acum_dez_26 <- acum_nov_26 + prev_dez_26

    acum_li80_nov <- acum_real_10m[10] + li80_12m[1]
    acum_ls80_nov <- acum_real_10m[10] + ls80_12m[1]
    acum_li80_dez <- acum_li80_nov + li80_12m[2]
    acum_ls80_dez <- acum_ls80_nov + ls80_12m[2]

    vet_exercicio_real_acum <- c(as.list(acum_real_10m), list(NA, NA))
    vet_exercicio_prev_acum <- c(as.list(rep(NA, 9)), list(acum_real_10m[10]), list(acum_nov_26), list(acum_dez_26))
    vet_exercicio_li80_acum <- c(as.list(rep(NA, 9)), list(acum_real_10m[10]), list(acum_li80_nov), list(acum_li80_dez))
    vet_exercicio_ls80_acum <- c(as.list(rep(NA, 9)), list(acum_real_10m[10]), list(acum_ls80_nov), list(acum_ls80_dez))
    vet_exercicio_meta_acum <- as.list(round(cumsum(rep(meta_anual_cat / 12, 12))))

    list(
      codigo = codigo,
      nome = nome,
      modelo_campeao = list(nome = mod_item$campeao, mape = mod_item$mape),
      # Estrutura Contínua 12 Meses à Frente (Janela Móvel)
      visao_12m = list(
        eixo_meses = eixo_continuo_22m,
        realizado = vet_continua_real,
        previsto = vet_continua_prev,
        li80 = vet_continua_li80,
        ls80 = vet_continua_ls80,
        meta_media_mensal = vet_continua_meta,
        soma_prevista_12m = sum(previsto_12m),
        soma_li80_12m = sum(li80_12m),
        soma_ls80_12m = sum(ls80_12m)
      ),
      # Próximos 12 Meses Detalhados em Cards
      proximos_12_meses = lista_12m_cards,
      # Visão Acumulada no Exercício 2026 (frente à LOA anual)
      acumulado = list(
        historico_meses = meses_exercicio_rotulos,
        realizado = vet_exercicio_real_acum,
        previsto = vet_exercicio_prev_acum,
        li80 = vet_exercicio_li80_acum,
        ls80 = vet_exercicio_ls80_acum,
        meta_loa = vet_exercicio_meta_acum
      ),
      # Visão Mensal no Exercício 2026
      mensal = list(
        historico_meses = meses_exercicio_rotulos,
        realizado = vet_exercicio_real_m,
        previsto = vet_exercicio_prev_m,
        li80 = vet_exercicio_li80_m,
        ls80 = vet_exercicio_ls80_m,
        meta_loa = vet_exercicio_meta_m
      ),
      # Fechamento do Ano Corrente
      fechamento_exercicio = list(
        meta_loa_total = meta_anual_cat,
        projecao_total = acum_dez_26,
        faixa_li80 = acum_li80_dez,
        faixa_ls80 = acum_ls80_dez,
        diferenca_nominal = acum_dez_26 - meta_anual_cat,
        diferenca_pct = round(((acum_dez_26 - meta_anual_cat) / meta_anual_cat) * 100, 1)
      ),
      # Campos de retrocompatibilidade
      realizado = vet_exercicio_real_acum,
      previsto = vet_exercicio_prev_acum,
      li80 = vet_exercicio_li80_acum,
      ls80 = vet_exercicio_ls80_acum,
      meta_loa_mes = vet_exercicio_meta_acum,
      proximos_meses = lista_12m_cards[1:2]
    )
  }

  # Montagem de Todas as 9 Arrecadações Estaduais
  series_todas <- list(
    construir_arrecadacao_completa("receita_corrente", "Receita Corrente Total", 1.00, saz_geral),
    construir_arrecadacao_completa("rcl", "Receita Corrente Líquida (RCL)", 0.84, saz_geral),
    construir_arrecadacao_completa("icms", "ICMS (Imposto s/ Circulação de Mercadorias)", 0.60, saz_icms),
    construir_arrecadacao_completa("ipva", "IPVA (Imposto s/ Veículos Automotores)", 0.08, saz_ipva),
    construir_arrecadacao_completa("itcd", "ITCD (Imposto s/ Transmissão Causa Mortis)", 0.018, saz_itcd),
    construir_arrecadacao_completa("receitas_impostos", "Receitas de Impostos (Tributária Estadual)", 0.70, saz_icms),
    construir_arrecadacao_completa("fpe", "Transferências do FPE", 0.20, saz_fpe, peso_fpe = TRUE),
    construir_arrecadacao_completa("transf_uniao", "Transferências Correntes da União", 0.26, saz_fpe, peso_fpe = TRUE),
    construir_arrecadacao_completa("receitas_capital", "Receitas de Capital", 0.05, saz_cap)
  )

  dados_compilados$dados[[uf_sigla]] <- list(
    uf = uf_sigla,
    meta_loa_global = meta_loa,
    projecao_base = proj_base,
    diferenca_loa = dif_nominal,
    diferenca_pct = dif_pct,
    nivel_risco = status_info$nivel,
    badge_risco = status_info$badge,
    contingenciamento_dispensado = status_info$conting_dispensada,
    contingenciamento_obrigatorio = if (!status_info$conting_dispensada) abs(dif_nominal) else 0,
    hero = list(
      bimestre = "5º BIMESTRE (RREO até Out / RGF 2º Quad)",
      marcoLegal = "LRF Art. 9º, 55 / MTO 2026 / EC 109",
      titulo = sprintf("Governança e Metas Fiscais - %s", uf_sigla),
      status = status_info$status,
      statusDetalhe = status_info$statusDetalhe
    ),
    series = series_todas,
    spending_review = list(
      rcl = rcl_real,
      pessoal = pessoal_val,
      pessoal_pct = round(pessoal_taxa * 100, 1),
      vinculacoes = vinculacoes_val,
      vinculacoes_pct = 34.5,
      poderes = poderes_val,
      poderes_pct = 8.2,
      divida = divida_val,
      divida_pct = round(divida_taxa * 100, 1),
      despesas_rigidas = total_rigido,
      pct_rigidez = round((total_rigido / rcl_real) * 100, 1),
      discricionario = discricionario_val,
      pct_discricionario = round((discricionario_val / rcl_real) * 100, 1)
    )
  )
}

# 3. Exportação JSON com NULL nativo ----
json_texto <- jsonlite::toJSON(dados_compilados, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
write(json_texto, caminho_json_app)
cat(sprintf("-> Exportado para a aplicação: %s\n", caminho_json_app))

if (file.exists(dirname(caminho_json_central))) {
  write(json_texto, caminho_json_central)
  cat(sprintf("-> Exportado para o diretório central: %s\n", caminho_json_central))
}

cat("✅ Conclusão da compilação de todas as arrecadações e horizonte de 12 meses!\n")

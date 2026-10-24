# pacotes ----
library(dplyr)
library(googledrive)
library(ibger)
library(purrr)
library(stringr)
library(tesouror)

# config ----
ano_atual              <- 2026
forcar_redownload_atual <- TRUE # Mude para TRUE se o SICONFI retificar o bimestre/quadrimestre atual

descobrir_ultimo_periodo <- function(ano, relatorio = c("RREO", "RGF"), ente_teste = 35) {
  relatorio <- match.arg(relatorio)
  
  if (relatorio == "RREO") {
    cat("Buscando o último Bimestre disponível no SICONFI...\n")
    for (p in 6:1) {
      tryCatch({
        teste <- suppressWarnings(suppressMessages(
          tesouror::get_budget_report(
            fiscal_year = ano, period = p, report_type = "RREO", 
            appendix = "RREO-Anexo 01", sphere = "E", entity_id = ente_teste
          )
        ))
        if (!is.null(teste) && nrow(teste) > 0) return(p)
      }, error = function(e) NULL)
    }
  } else {
    cat("Buscando o último Quadrimestre disponível no SICONFI...\n")
    for (p in 3:1) {
      tryCatch({
        teste <- suppressWarnings(suppressMessages(
          tesouror::get_rgf(
            an_exercicio = ano, in_periodicidade = "Q", nr_periodo = p,
            co_tipo_demonstrativo = "RGF", no_anexo = "RGF-Anexo 06",
            co_esfera = "E", co_poder = "E", id_ente = ente_teste
          )
        ))
        if (!is.null(teste) && nrow(teste) > 0) return(p)
      }, error = function(e) NULL)
    }
  }
  return(1) # Fallback padrão de segurança
}

#bimestre_atual         <- 2
#quadrimestre_atual     <- 1

bimestre_atual     <- descobrir_ultimo_periodo(ano_atual, "RREO")
quadrimestre_atual <- descobrir_ultimo_periodo(ano_atual, "RGF")

# caminhos e pastas ----
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("app_aurora_fiscal/data", showWarnings = FALSE, recursive = TRUE)

caminho_cache_raw <- "data/raw_siconfi_cache.rds"

# codigos ufs ----
codigos_ufs <- c(
  11:17,       
  21:29,       
  31:33, 35,   
  41:43,       
  50:53        
)

# 1. Carregar Cache Local (se existir) ----
if (file.exists(caminho_cache_raw)) {
  cache_raw <- readRDS(caminho_cache_raw)
  cat("-> Cache histórico carregado com sucesso!\n")
} else {
  cache_raw <- list(
    rreo_1 = NULL, 
    rreo_2 = NULL, 
    rreo_4 = NULL,
    rreo_9 = NULL, 
    rreo_14 = NULL, 
    rgf_06 = NULL
  )
  cat("-> Nenhum cache encontrado. Uma carga completa será feita nesta primeira execução.\n")
}

# 2. Definir Tabela de Parâmetros Alvo ----
params_rreo_alvo <- bind_rows(
  expand.grid(
    fiscal_year = 2019:(ano_atual - 1), 
    period = unique(c(bimestre_atual, 6)), # Sempre baixa o bi_atual e o bi de encerramento
    entity_id = codigos_ufs
  ),
  expand.grid(
    fiscal_year = ano_atual, 
    period = bimestre_atual, 
    entity_id = codigos_ufs
  )
) %>% distinct()

# Alvo exclusivo para o Anexo 09 (Apenas 6º bimestre dos anos fechados)
params_rreo_9_alvo <- expand.grid(
  fiscal_year = 2019:(ano_atual - 1), 
  period = 6, 
  entity_id = codigos_ufs
) %>% distinct()

params_rgf_alvo <- bind_rows(
  expand.grid(
    an_exercicio = 2019:(ano_atual - 1), 
    nr_periodo = unique(c(quadrimestre_atual, 3)), 
    id_ente = codigos_ufs
  ),
  expand.grid(
    an_exercicio = ano_atual, 
    nr_periodo = quadrimestre_atual, 
    id_ente = codigos_ufs
  )
) %>% distinct()

# 3. Funções Auxiliares de Download Incremental ----

baixar_rreo_incremental <- function(params_alvo, anexo, df_cache, forcar = FALSE) {
  # Se forçar recarga, remove os dados do período atual do cache
  if (forcar && !is.null(df_cache) && nrow(df_cache) > 0) {
    df_cache <- df_cache %>% 
      filter(!(exercicio == ano_atual & periodo == bimestre_atual))
  }
  
  # Descobre o que ainda não foi baixado
  if (!is.null(df_cache) && nrow(df_cache) > 0) {
    chaves_existentes <- paste(df_cache$exercicio, df_cache$periodo, df_cache$cod_ibge, sep = "_")
    params_faltantes <- params_alvo %>% 
      mutate(chave = paste(fiscal_year, period, entity_id, sep = "_")) %>% 
      filter(!chave %in% chaves_existentes) %>% 
      select(-chave)
  } else {
    params_faltantes <- params_alvo
  }
  
  if (nrow(params_faltantes) == 0) {
    cat("  [RREO -", anexo, "] Todos os dados já estão atualizados no cache.\n")
    return(df_cache)
  }
  
  cat("  [RREO -", anexo, "] Baixando", nrow(params_faltantes), "novas requisições...\n")
  
  novos_dados <- pmap_dfr(params_faltantes, function(fiscal_year, period, entity_id) {
    cat("  -> Baixando RREO", anexo, "| UF:", entity_id, "| Ano:", fiscal_year, "| Bimestre:", period, "\n")
    Sys.sleep(1)
    
    tryCatch({
      get_budget_report(
        fiscal_year = fiscal_year,
        period = period,
        report_type = "RREO",
        appendix = anexo,
        sphere = "E",
        entity_id = entity_id
      )
    }, error = function(e) {
      cat("  ! Erro ao baixar UF", entity_id, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  bind_rows(df_cache, novos_dados) %>% distinct()
}

baixar_rgf_incremental <- function(params_alvo, df_cache, forcar = FALSE) {
  if (forcar && !is.null(df_cache) && nrow(df_cache) > 0) {
    df_cache <- df_cache %>% 
      filter(!(exercicio == ano_atual & periodo == quadrimestre_atual))
  }
  
  if (!is.null(df_cache) && nrow(df_cache) > 0) {
    chaves_existentes <- paste(df_cache$exercicio, df_cache$periodo, df_cache$cod_ibge, sep = "_")
    params_faltantes <- params_alvo %>% 
      mutate(chave = paste(an_exercicio, nr_periodo, id_ente, sep = "_")) %>% 
      filter(!chave %in% chaves_existentes) %>% 
      select(-chave)
  } else {
    params_faltantes <- params_alvo
  }
  
  if (nrow(params_faltantes) == 0) {
    cat("  [RGF - Anexo 06] Todos os dados já estão atualizados no cache.\n")
    return(df_cache)
  }
  
  cat("  [RGF - Anexo 06] Baixando", nrow(params_faltantes), "novas requisições...\n")
  
  novos_dados <- pmap_dfr(params_faltantes, function(an_exercicio, nr_periodo, id_ente) {
    cat("  -> Baixando RGF 06 | UF:", id_ente, "| Ano:", an_exercicio, "| Quadrimestre:", nr_periodo, "\n")
    Sys.sleep(1)
    
    tryCatch({
      get_rgf(
        an_exercicio = an_exercicio,
        in_periodicidade = "Q",
        nr_periodo = nr_periodo,            
        co_tipo_demonstrativo = "RGF",
        no_anexo = "RGF-Anexo 06",
        co_esfera = "E",
        co_poder = "E",
        id_ente = id_ente
      )
    }, error = function(e) {
      cat("  ! Erro ao baixar RGF UF", id_ente, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  bind_rows(df_cache, novos_dados) %>% distinct()
}

# 4. Executar Downloads Incrementais ----
cat("\n--- Atualizando Cache Bruto ---\n")
cache_raw$rreo_1  <- baixar_rreo_incremental(params_rreo_alvo, "RREO-Anexo 01", cache_raw$rreo_1, forcar_redownload_atual)

cache_raw$rreo_2  <- baixar_rreo_incremental(params_rreo_alvo, "RREO-Anexo 02", cache_raw$rreo_2, forcar_redownload_atual)

cache_raw$rreo_4 <- baixar_rreo_incremental(
  params_rreo_alvo, 
  "RREO-Anexo 04", 
  cache_raw$rreo_4, 
  forcar_redownload_atual
)

cache_raw$rreo_9  <- baixar_rreo_incremental(params_rreo_9_alvo, "RREO-Anexo 09", cache_raw$rreo_9, forcar_redownload_atual)

cache_raw$rreo_14 <- baixar_rreo_incremental(params_rreo_alvo, "RREO-Anexo 14", cache_raw$rreo_14, forcar_redownload_atual)
cache_raw$rgf_06  <- baixar_rgf_incremental(params_rgf_alvo, cache_raw$rgf_06, forcar_redownload_atual)

# Salva o cache bruto atualizado no disco
saveRDS(cache_raw, caminho_cache_raw)
cat("-> Cache de dados brutos atualizado e salvo localmente.\n")

# 5. Processamento dos Indicadores ----
cat("\n--- Processando Indicadores do Painel ---\n")

base_bruta_unica <- bind_rows(
  cache_raw$rreo_1  %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rreo_2  %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rreo_4  %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rreo_9  %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rreo_14 %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rgf_06  %>% mutate(periodicidade = "Quadrimestral")
)

base_painel <- base_bruta_unica %>%
  filter(
    (periodicidade == "Bimestral" & periodo %in% c(bimestre_atual, 6)) | 
      (periodicidade == "Quadrimestral" & periodo %in% c(quadrimestre_atual, 3))
  ) %>%
  mutate(
    indicador = case_when(
      # --- REGRAS DO RREO ---
      coluna %in% c("Até o Bimestre") & conta %in% c("Superávit Orçamentário", "Déficit Orçamentário") ~ "Resultado Orçamentário",
      coluna == "Resultado Apurado até o Bimestre (b)" & conta %in% c("Resultado Primário - Acima da Linha", "RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha") ~ "Resultado Primário",
      coluna == "Resultado Apurado até o Bimestre (b)" & conta %in% c("Resultado Nominal - Acima da Linha", "RESULTADO NOMINAL (SEM RPPS) - Abaixo da Linha") ~ "Resultado Nominal",
      coluna == "Até o Bimestre" & conta == "Receita Corrente Líquida" ~ "Receita Corrente Líquida",
      coluna == "% Aplicado Até o Bimestre" & startsWith(conta, "Despesas com Ações e Serviços Públicos de Saúde Executadas com Recursos") ~ "Vinculação - Saúde",
      coluna == "% Aplicado Até o Bimestre" & grepl("Manutenção e Desenvolvimento do Ensino", conta) ~ "Vinculação - Educação",
      
      coluna == "Até o Bimestre" & cod_conta %in% c('ReceitasRealizadasDemonstrativoSimplificadoBalancoOrcamentario') & conta == "Receitas Realizadas" ~ "Receitas Realizadas",
      
      coluna == "Até o Bimestre (c)" & cod_conta %in% c('TransferenciasDeCapitalDaUniaoEDeSuasEntidades') ~ "Transf. União (Capital)",
      
      coluna == "Até o Bimestre (c)" & cod_conta %in% c('TransferenciasCorrentesDaUniaoEDeSuasEntidades') ~ "Transf. União (Corrente)",
      
      coluna %in% c("Até o Bimestre (c)") & conta == "Impostos" ~ "Receitas de Impostos",
      
      coluna %in% c("Até o Bimestre (c)") & conta == "RECEITAS DE CAPITAL" ~ "Receitas de Capital",
      
      coluna %in% c("DESPESAS EMPENHADAS ATÉ O BIMESTRE (f)") & conta == "DESPESAS DE CAPITAL" ~ "Despesas de Capital - Emp.",
      
      # --- REGRAS DO RREO 2 ---
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Administração") ~ "Administração",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Agricultura") ~ "Agricultura",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Assistência Social") ~ "Assistência Social",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Ciência e Tecnologia") ~ "Ciência e Tecnologia",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Cultura") ~ "Cultura",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Direitos da Cidadania") ~ "Direitos da Cidadania",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Educação") ~ "Educação",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Gestão Ambiental") ~ "Gestão Ambiental",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Habitação") ~ "Habitação",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Judiciária") ~ "Judiciária",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Legislativa") ~ "Legislativa",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Previdência Social") ~ "Previdência Social",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Saneamento") ~ "Saneamento",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Saúde") ~ "Saúde",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Segurança Pública") ~ "Segurança Pública",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Trabalho") ~ "Trabalho",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Transporte") ~ "Transporte",
      
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Urbanismo") ~ "Urbanismo",
      
      # --- REGRAS DO RGF ---
      coluna == "% SOBRE A RCL AJUSTADA" & startsWith(conta, "Despesa Total com Pessoal - DTP") ~ "DTP/RCL",
      coluna == "% SOBRE A RCL AJUSTADA" & grepl("Dívida Consolidada Líquida", conta, ignore.case = TRUE) ~ "Dívida Consolidada Líquida",
      
      TRUE ~ NA_character_
    ),
    
    grupo = case_when(
      indicador %in% c('DTP/RCL', 'Dívida Consolidada Líquida', 'Resultado Nominal', 'Resultado Orçamentário', 'Resultado Primário', 'Vinculação - Educação', 'Vinculação - Saúde') ~ 'Resultados',
      
      indicador %in% c(
        'Receita Corrente Líquida', 'Receitas Realizadas', 'Receitas de Impostos', 'Transf. União (Capital)', 'Transf. União (Corrente)', 'Receitas de Capital'
        ) ~ 'Receitas',
      
      indicador %in% c(
        'Despesas de Capital - Emp.',
        'Ciência e Tecnologia', 'Educação', 'Habitação', 'Saúde', 'Segurança Pública', 'Transporte', 'Urbanismo') ~ 'Despesas',
      
      TRUE ~ NA_character_
      
      ),
    
    ordem = case_when(
      grupo %in% c('Resultados') ~ 1,
      grupo %in% c('Receitas') ~ 2,
      grupo %in% c('Despesas') ~ 3,
      
      TRUE ~ NA_integer_
    )
    
  ) %>%
  filter(!is.na(indicador)) %>%
  mutate(
    valor = ifelse(conta == "Déficit Orçamentário", valor * -1, valor)
  ) %>%
  select(uf, exercicio, periodicidade, periodo, indicador, grupo, ordem, valor) %>%
  arrange(uf, exercicio, indicador) %>% 
  distinct()

base_painel %>% glimpse()

## população -----
dados_populacao_uf <- ibge_variables(6579, localities = "N3")

de_para_uf <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP", "17" = "TO",
  "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
  "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "50" = "MS", "51" = "MT", "52" = "GO", "53" = "DF"
)

populacao_tratada <- dados_populacao_uf %>%
  mutate(
    uf = de_para_uf[locality_id],
    exercicio = as.numeric(period),
    populacao = as.numeric(value)
  ) %>%
  select(uf, exercicio, populacao)

# --- INÍCIO DA PROJEÇÃO ---
# Descobre o último ano disponível na base do IBGE e o ano mais recente do seu painel
ano_max_ibge <- max(populacao_tratada$exercicio, na.rm = TRUE)
ano_limite_dados <- max(base_painel$exercicio, na.rm = TRUE) 

# Se a base do IBGE estiver defasada em relação ao RREO/RGF (ex: IBGE vai até 2022 e RREO até 2026)
if (ano_limite_dados > ano_max_ibge) {
  
  # Pega a população do último ano disponível
  ultima_pop <- populacao_tratada %>% 
    filter(exercicio == ano_max_ibge) %>% 
    select(uf, populacao)
  
  # Cria as combinações para os anos que faltam (ex: 2023 a 2026)
  anos_faltantes <- expand.grid(
    uf = unique(ultima_pop$uf),
    exercicio = (ano_max_ibge + 1):ano_limite_dados
  )
  
  # Junta as populações replicadas
  projecao <- anos_faltantes %>% 
    left_join(ultima_pop, by = "uf")
  
  # Empilha com a base original de população
  populacao_tratada <- bind_rows(populacao_tratada, projecao) %>%
    arrange(uf, exercicio)
}

## indicadores derivados ----
# Resultado Orçamentário Relativo
ind_orc_rcl <- base_painel %>%
  filter(indicador == "Resultado Orçamentário") %>%
  rename(val_resultado = valor) %>%
  # Faz o join com as linhas de RCL garantindo que uf, exercicio, e periodo batam
  inner_join(
    base_painel %>% 
      filter(indicador == "Receita Corrente Líquida") %>% 
      select(uf, exercicio, periodicidade, periodo, val_rcl = valor),
    by = c("uf", "exercicio", "periodicidade", "periodo")
  ) %>%
  mutate(
    valor = val_resultado / val_rcl,
    indicador = "Resultado Orçamentário Relativo",
    grupo = "Resultados",
    ordem = 1
  ) %>%
  select(uf, exercicio, periodicidade, periodo, indicador, grupo, ordem, valor)

# Resultado Orçamentário per capita
ind_orc_per_capita <- base_painel %>%
  filter(indicador == "Resultado Orçamentário") %>%
  # Faz o join com a população com base na uf e exercicio (ano)
  inner_join(populacao_tratada, by = c("uf", "exercicio")) %>%
  mutate(
    valor = valor / populacao,
    indicador = "Resultado Orçamentário per capita",
    grupo = "Resultados",
    ordem = 1
  ) %>%
  select(uf, exercicio, periodicidade, periodo, indicador, grupo, ordem, valor)

# Resultado fiscal = Resultado Primário / RCL
ind_res_fiscal <- base_painel %>%
  filter(indicador == "Resultado Primário") %>%
  rename(val_primario = valor) %>%
  # Faz o join com a RCL garantindo as mesmas chaves
  inner_join(
    base_painel %>% 
      filter(indicador == "Receita Corrente Líquida") %>% 
      select(uf, exercicio, periodicidade, periodo, val_rcl = valor),
    by = c("uf", "exercicio", "periodicidade", "periodo")
  ) %>%
  mutate(
    valor = val_primario / val_rcl,
    indicador = "Resultado Fiscal",
    grupo = "Resultados", 
    ordem = 1
  ) %>%
  select(uf, exercicio, periodicidade, periodo, indicador, grupo, ordem, valor)

base_painel <- bind_rows(
  base_painel,
  ind_orc_rcl,
  ind_orc_per_capita,
  ind_res_fiscal
) %>%
  arrange(uf, exercicio, ordem, indicador)

# Monta o objeto para API
df_bimestral <- base_painel[base_painel$periodicidade == "Bimestral", ]

dict_periodos <- lapply(
  split(df_bimestral$periodo, df_bimestral$exercicio),
  function(x) sort(unique(na.omit(as.numeric(x))))
)

## ipca ----
# 1. Puxa os dados
ipca_bruto <- ibge_variables(
  aggregate = 1737,    
  variable = 2266,     
  periods = "all",     
  localities = "BR"    
)

# 2. Cria a tabela enxuta
tabela_fatores <- ipca_bruto %>%
  mutate(
    # Usa parse_ibge_value para tratar strings sujas, apontando para a coluna 'value'
    valor_numerico = parse_ibge_value(value), 
    
    # Extrai o ano da coluna 'period' (ex: "197912" -> 1979)
    ano = as.integer(str_sub(period, 1, 4))
  ) %>%
  filter(ano >= 2018) %>%
  # Ordena pelo 'period' cronologicamente
  arrange(ano, period) %>%
  group_by(ano) %>%
  # Pega o último mês disponível de cada ano
  slice_tail(n = 1) %>% 
  ungroup() %>%
  mutate(
    # Fator = Último índice de todos / Índice do ano específico
    fator_atualizacao = last(valor_numerico) / valor_numerico
  ) %>%
  select(ano, indice_ipca = valor_numerico, fator_atualizacao)

dict_ipca <- as.list(setNames(tabela_fatores$fator_atualizacao, as.character(tabela_fatores$ano)))

## RREO 04 (PREVIDÊNCIA) ----
base_previdencia <- cache_raw$rreo_4 %>%
  filter(
    periodo %in% c(bimestre_atual, 6),
    # Captura flexível das colunas do estágio da despesa
    grepl("DOTAÇÃO ATUALIZADA|EMPENHADAS ATÉ O BIMESTRE|LIQUIDADAS ATÉ O BIMESTRE|PAGAS ATÉ O BIMESTRE", coluna, ignore.case = TRUE)
  ) %>%
  mutate(
    # Padronização dos estágios
    estagio = case_when(
      grepl("DOTAÇÃO", coluna, ignore.case = TRUE) ~ "dotacao",
      grepl("EMPENHADAS", coluna, ignore.case = TRUE) ~ "empenhado",
      grepl("LIQUIDADAS", coluna, ignore.case = TRUE) ~ "liquidado",
      grepl("PAGAS", coluna, ignore.case = TRUE) ~ "pago",
      TRUE ~ coluna
    ),
    
    # Classificação dos blocos
    bloco = case_when(
      grepl("MILITARES", conta, ignore.case = TRUE) | conta %in% c("Inatividade", "Pensões", "Outras Despesas") ~ "Militares",
      TRUE ~ "RPPS"
    ),
    
    # Criação da Ordem Numérica para preservar o layout original no Shiny
    ordem_linha = case_when(
      # Bloco RPPS
      grepl("^Benefícios", conta, ignore.case = TRUE) ~ 1,
      grepl("^Aposentadorias", conta, ignore.case = TRUE) ~ 2,
      grepl("^Pensões por Morte", conta, ignore.case = TRUE) ~ 3,
      grepl("Outras Despesas Previdenciárias", conta, ignore.case = TRUE) ~ 4,
      grepl("Compensação Previdenciária", conta, ignore.case = TRUE) ~ 5,
      grepl("Demais Despesas Previdenciárias", conta, ignore.case = TRUE) ~ 6,
      grepl("TOTAL DAS DESPESAS DO FUNDO EM REPARTIÇÃO", conta, ignore.case = TRUE) ~ 7,
      grepl("RESULTADO PREVIDENCIÁRIO", conta, ignore.case = TRUE) ~ 8,
      
      # Bloco Militares
      grepl("^Inatividade", conta, ignore.case = TRUE) ~ 1,
      grepl("^Pensões$", conta, ignore.case = TRUE) ~ 2,
      grepl("^Outras Despesas$", conta, ignore.case = TRUE) ~ 3,
      grepl("TOTAL DAS DESPESAS COM INATIVOS E PENSIONISTAS MILITARES", conta, ignore.case = TRUE) ~ 4,
      grepl("RESULTADO ASSOCIADO ÀS PENSÕES", conta, ignore.case = TRUE) ~ 5,
      
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(ordem_linha)) %>%
  select(uf, exercicio, periodo, bloco, ordem_linha, conta, estagio, valor) %>%
  distinct()

# Montagem do objeto final
objeto_pronto_api <- list(
  dados = base_painel,
  filtros = list(
    uf = sort(unique(base_painel$uf)),
    exercicio = sort(unique(base_painel$exercicio), decreasing = TRUE),
    periodos_por_ano = dict_periodos
  ),
  ipca = dict_ipca,
  dados_previdencia = base_previdencia
)

# 6. Salvar Arquivos Finais ----
saveRDS(objeto_pronto_api, 'app_aurora_fiscal/data/base_painel.rds')

write.csv2(base_painel, 'app_aurora_fiscal/data/base_painel.csv', row.names = FALSE)

cat("-> Arquivos locais compilados com sucesso.\n")

# 7. Upload para o Google Drive ----
cat("\n--- Realizando Upload no Google Drive ---\n")

caminho_json_servico <- "data/r-sheets-hugo-0788cc358cb5.json"
drive_auth(path = caminho_json_servico)

id_pasta_rreo <- "1vuu4lPZmCrlVSq2vIbwtz4lM_Vhy-pQ3"
id_pasta_rgf  <- "1YfhlRUJ2r-Kqemi_9Lh_6TYEfskjXQ8x"

tmp_csv_rreo <- tempfile(fileext = "_rreo.csv")
tmp_csv_rgf  <- tempfile(fileext = "_rgf.csv")

write.csv2(cache_raw$rreo_14, tmp_csv_rreo, row.names = FALSE)
write.csv2(cache_raw$rgf_06, tmp_csv_rgf, row.names = FALSE)

drive_upload(
  media = tmp_csv_rreo,
  path = as_id(id_pasta_rreo),
  name = paste0("RREO_Indicadores_Bimestre_", bimestre_atual, ".csv"),
  overwrite = TRUE
)

drive_upload(
  media = tmp_csv_rgf,
  path = as_id(id_pasta_rgf),
  name = paste0("RGF_Indicadores_Quadrimestre_", quadrimestre_atual, ".csv"),
  overwrite = TRUE
)

unlink(c(tmp_csv_rreo, tmp_csv_rgf))

cat("-> Upload concluído com sucesso para ambas as pastas!\n")

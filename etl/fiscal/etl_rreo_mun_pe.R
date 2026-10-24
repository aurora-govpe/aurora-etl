# pacotes ----
library(dplyr)
library(googledrive)
library(ibger)
library(purrr)
library(stringr)
library(tesouror)

# config ----
ano_atual              <- 2026
forcar_redownload_atual <- FALSE # Mude para TRUE se o SICONFI retificar o bimestre/quadrimestre atual

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

bimestre_atual     <- descobrir_ultimo_periodo(ano_atual, "RREO")
quadrimestre_atual <- descobrir_ultimo_periodo(ano_atual, "RGF")

# caminhos e pastas ----
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("app_aurora_municipios/data", showWarnings = FALSE, recursive = TRUE)

# Alterado o nome do cache para não misturar com os estados
caminho_cache_raw <- "data/raw_siconfi_cache_mun.rds" 

# 1. Carregar Cache Local (se existir) ----
if (file.exists(caminho_cache_raw)) {
  cache_raw <- readRDS(caminho_cache_raw)
  cat("-> Cache histórico carregado com sucesso!\n")
} else {
  cache_raw <- list(rreo_1 = NULL, rreo_2 = NULL, rreo_9 = NULL, rreo_14 = NULL, rgf_06 = NULL)
  cat("-> Nenhum cache encontrado. Uma carga completa será feita nesta primeira execução.\n")
}

# 2. Definir Tabela de Parâmetros Alvo (Sem código IBGE, pois a API puxa o estado inteiro) ----
params_rreo_alvo <- bind_rows(
  expand.grid(
    fiscal_year = 2019:(ano_atual - 1), 
    period = unique(c(bimestre_atual, 6))
  ),
  expand.grid(
    fiscal_year = ano_atual, 
    period = bimestre_atual
  )
) %>% distinct()

# Alvo exclusivo para o Anexo 09 (Apenas 6º bimestre dos anos fechados)
params_rreo_9_alvo <- expand.grid(
  fiscal_year = 2019:(ano_atual - 1), 
  period = 6
) %>% distinct()

params_rgf_alvo <- bind_rows(
  expand.grid(
    an_exercicio = 2019:(ano_atual - 1), 
    nr_periodo = unique(c(quadrimestre_atual, 3))
  ),
  expand.grid(
    an_exercicio = ano_atual, 
    nr_periodo = quadrimestre_atual
  )
) %>% distinct()

# 3. Funções Auxiliares de Download Incremental (Adaptadas para Municípios) ----
baixar_rreo_incremental_mun <- function(params_alvo, anexo, df_cache, forcar = FALSE) {
  if (forcar && !is.null(df_cache) && nrow(df_cache) > 0) {
    df_cache <- df_cache %>% 
      filter(!(exercicio == ano_atual & periodo == bimestre_atual))
  }
  
  if (!is.null(df_cache) && nrow(df_cache) > 0) {
    # Verifica apenas o ano e período, assumindo que se tem, tem pra todos os municípios
    chaves_existentes <- paste(df_cache$exercicio, df_cache$periodo, sep = "_")
    params_faltantes <- params_alvo %>% 
      mutate(chave = paste(fiscal_year, period, sep = "_")) %>% 
      filter(!chave %in% chaves_existentes) %>% 
      select(-chave)
  } else {
    params_faltantes <- params_alvo
  }
  
  if (nrow(params_faltantes) == 0) {
    cat("  [RREO Municípios -", anexo, "] Todos os dados já estão atualizados no cache.\n")
    return(df_cache)
  }
  
  cat("  [RREO Municípios -", anexo, "] Baixando", nrow(params_faltantes), "novas requisições em lote (PE)...\n")
  
  novos_dados <- pmap_dfr(params_faltantes, function(fiscal_year, period) {
    cat("  -> Baixando RREO", anexo, "| Municípios PE | Ano:", fiscal_year, "| Bimestre:", period, "\n")
    Sys.sleep(1)
    
    tryCatch({
      get_rreo_for_state(
        state_uf = "PE",
        an_exercicio = fiscal_year,
        nr_periodo = period,
        co_tipo_demonstrativo = "RREO",
        no_anexo = anexo,
        include_capital = TRUE
      )
    }, error = function(e) {
      cat("  ! Erro ao baixar período", period, "do ano", fiscal_year, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  bind_rows(df_cache, novos_dados) %>% distinct()
}

baixar_rgf_incremental_mun <- function(params_alvo, df_cache, forcar = FALSE) {
  if (forcar && !is.null(df_cache) && nrow(df_cache) > 0) {
    df_cache <- df_cache %>% 
      filter(!(exercicio == ano_atual & periodo == quadrimestre_atual))
  }
  
  if (!is.null(df_cache) && nrow(df_cache) > 0) {
    chaves_existentes <- paste(df_cache$exercicio, df_cache$periodo, sep = "_")
    params_faltantes <- params_alvo %>% 
      mutate(chave = paste(an_exercicio, nr_periodo, sep = "_")) %>% 
      filter(!chave %in% chaves_existentes) %>% 
      select(-chave)
  } else {
    params_faltantes <- params_alvo
  }
  
  if (nrow(params_faltantes) == 0) {
    cat("  [RGF Municípios - Anexo 06] Todos os dados já estão atualizados no cache.\n")
    return(df_cache)
  }
  
  cat("  [RGF Municípios - Anexo 06] Baixando", nrow(params_faltantes), "novas requisições em lote (PE)...\n")
  
  novos_dados <- pmap_dfr(params_faltantes, function(an_exercicio, nr_periodo) {
    cat("  -> Baixando RGF 06 | Municípios PE | Ano:", an_exercicio, "| Quadrimestre:", nr_periodo, "\n")
    Sys.sleep(1)
    
    tryCatch({
      get_rgf_for_state(
        state_uf = "PE",
        an_exercicio = an_exercicio,
        in_periodicidade = "Q",
        nr_periodo = nr_periodo,
        co_tipo_demonstrativo = "RGF",
        no_anexo = "RGF-Anexo 06",
        co_poder = "E",
        include_capital = TRUE
      )
    }, error = function(e) {
      cat("  ! Erro ao baixar RGF período", nr_periodo, "do ano", an_exercicio, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  bind_rows(df_cache, novos_dados) %>% distinct()
}

# 4. Executar Downloads Incrementais ----
cat("\n--- Atualizando Cache Bruto ---\n")
cache_raw$rreo_1  <- baixar_rreo_incremental_mun(params_rreo_alvo, "RREO-Anexo 01", cache_raw$rreo_1, forcar_redownload_atual)

cache_raw$rreo_2  <- baixar_rreo_incremental_mun(params_rreo_alvo, "RREO-Anexo 02", cache_raw$rreo_2, forcar_redownload_atual)

cache_raw$rreo_3  <- baixar_rreo_incremental_mun(params_rreo_alvo, "RREO-Anexo 03", cache_raw$rreo_3, forcar_redownload_atual)

cache_raw$rreo_9  <- baixar_rreo_incremental_mun(params_rreo_9_alvo, "RREO-Anexo 09", cache_raw$rreo_9, forcar_redownload_atual)

cache_raw$rreo_14 <- baixar_rreo_incremental_mun(params_rreo_alvo, "RREO-Anexo 14", cache_raw$rreo_14, forcar_redownload_atual)

cache_raw$rgf_06  <- baixar_rgf_incremental_mun(params_rgf_alvo, cache_raw$rgf_06, forcar_redownload_atual)

# Salva o cache bruto atualizado no disco
saveRDS(cache_raw, caminho_cache_raw)
cat("-> Cache de dados brutos atualizado e salvo localmente.\n")

# 5. Processamento dos Indicadores ----
cat("\n--- Processando Indicadores do Painel ---\n")

base_bruta_unica <- bind_rows(
  cache_raw$rreo_1  %>% mutate(periodicidade = "Bimestral"),
  cache_raw$rreo_2  %>% mutate(periodicidade = "Bimestral"),
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
    # Conversão temporal sugerida anteriormente
    mes_referencia = case_when(
      periodicidade == "Bimestral" ~ as.integer(periodo * 2),
      periodicidade == "Quadrimestral" ~ as.integer(periodo * 4)
    ),
    
    indicador = case_when(
      
      ## RREO 1 ----
      ### Resultados ----
      coluna %in% c("Até o Bimestre") & conta %in% c("Superávit Orçamentário", "Déficit Orçamentário") ~ "Resultado Orçamentário",
      coluna == "Resultado Apurado até o Bimestre (b)" & conta %in% c("Resultado Primário - Acima da Linha", "RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha") ~ "Resultado Primário",
      coluna == "Resultado Apurado até o Bimestre (b)" & conta %in% c("Resultado Nominal - Acima da Linha", "RESULTADO NOMINAL (SEM RPPS) - Abaixo da Linha") ~ "Resultado Nominal",
      coluna == "Até o Bimestre" & conta == "Receita Corrente Líquida" ~ "Receita Corrente Líquida",
      coluna == "% Aplicado Até o Bimestre" & startsWith(conta, "Despesas com Ações e Serviços Públicos de Saúde Executadas com Recursos") ~ "Vinculação - Saúde",
      coluna == "% Aplicado Até o Bimestre" & grepl("Manutenção e Desenvolvimento do Ensino", conta) ~ "Vinculação - Educação",
      
      ### Receitas ----
      
      coluna == "Até o Bimestre" & cod_conta %in% c('ReceitasRealizadasDemonstrativoSimplificadoBalancoOrcamentario') & conta == "Receitas Realizadas" ~ "Receitas Realizadas",
      coluna == "Até o Bimestre (c)" & cod_conta %in% c('TransferenciasDeCapitalDaUniaoEDeSuasEntidades') ~ "Transf. União (Capital)",
      coluna == "Até o Bimestre (c)" & cod_conta %in% c('TransferenciasCorrentesDaUniaoEDeSuasEntidades') ~ "Transf. União (Corrente)",
      coluna %in% c("Até o Bimestre (c)") & conta == "Impostos" ~ "Receitas de Impostos",
      coluna %in% c("Até o Bimestre (c)") & conta == "RECEITAS DE CAPITAL" ~ "Receitas de Capital",
      
      ### Despesas ----
      
      coluna %in% c("DESPESAS EMPENHADAS ATÉ O BIMESTRE (f)") & conta == "DESPESAS DE CAPITAL" ~ "Despesas de Capital - Emp.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "PessoalEEncargosSociais" ~ "Despesas de Pessoal - Liq.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "JurosEEncargosDaDivida" ~ "Juros e Encargos - Liq.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "AmortizacaoDaDivida" ~ "Amortização da Dívida - Liq.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "DespesasCorrentes" ~ "Despesas Correntes - Liq.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "Investimentos" ~ "Investimentos - Liq.",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)") & cod_conta == "InversoesFinanceiras" ~ "Inversões - Liq.",
      
      ## RREO 2 ----
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Ciência e Tecnologia") ~ "Ciência e Tecnologia",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Educação") ~ "Educação",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Habitação") ~ "Habitação",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Saúde") ~ "Saúde",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Segurança Pública") ~ "Segurança Pública",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Transporte") ~ "Transporte",
      coluna %in% c("DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)") & cod_conta %in% c('RREO2TotalDespesas') & conta %in% c("Urbanismo") ~ "Urbanismo",
      
      # --- REGRAS DO RGF ---
      coluna == "% SOBRE A RCL AJUSTADA" & startsWith(conta, "Despesa Total com Pessoal - DTP") ~ "DTP/RCL",
      coluna == "% SOBRE A RCL AJUSTADA" & grepl("Dívida Consolidada Líquida", conta, ignore.case = TRUE) ~ "Dívida Consolidada Líquida",
      TRUE ~ NA_character_
    ),
    
    grupo = case_when(
      indicador %in% c('DTP/RCL', 'Dívida Consolidada Líquida', 'Resultado Nominal', 'Resultado Orçamentário', 'Resultado Primário', 'Vinculação - Educação', 'Vinculação - Saúde') ~ 'Resultados',
      indicador %in% c('Receita Corrente Líquida', 'Receitas Realizadas', 'Receitas de Impostos', 'Transf. União (Capital)', 'Transf. União (Corrente)', 'Receitas de Capital') ~ 'Receitas',
      indicador %in% c(
        'Despesas de Capital - Emp.', 
        'Despesas de Pessoal - Liq.',
        'Juros e Encargos - Liq.',
        'Amortização da Dívida - Liq.',
        'Despesas Correntes - Liq.',
        'Investimentos - Liq.',
        'Inversões - Liq.',
        'Ciência e Tecnologia', 
        'Educação', 
        'Habitação', 
        'Saúde', 
        'Segurança Pública',
        'Transporte', 
        'Urbanismo') ~ 'Despesas',
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
    valor = ifelse(conta == "Déficit Orçamentário", valor * -1, valor),
    cod_ibge = as.character(cod_ibge)
  ) %>%
  # Trocado 'uf' para 'cod_ibge' e 'ente' (instituicao no tesouror)
  select(cod_ibge, ente = instituicao, exercicio, periodicidade, periodo, mes_referencia, indicador, grupo, ordem, valor) %>%
  arrange(cod_ibge, exercicio, indicador) %>% 
  distinct()

base_painel %>% glimpse()

## população -----
lista_municipios_pe <- readRDS("data/lista_municipios_pe.rds")

dados_populacao_mun_pe <- ibge_variables(
  6579, 
  localities = list(N6 = lista_municipios_pe)
)

populacao_tratada <- dados_populacao_mun_pe %>%
  mutate(
    cod_ibge = as.character(locality_id),
    exercicio = as.numeric(period),
    populacao = as.numeric(value)
  ) %>%
  select(cod_ibge, exercicio, populacao)

# --- INÍCIO DA PROJEÇÃO ---
ano_max_ibge <- max(populacao_tratada$exercicio, na.rm = TRUE)
ano_limite_dados <- max(base_painel$exercicio, na.rm = TRUE) 

if (ano_limite_dados > ano_max_ibge) {
  ultima_pop <- populacao_tratada %>% 
    filter(exercicio == ano_max_ibge) %>% 
    select(cod_ibge, populacao)
  
  anos_faltantes <- expand.grid(
    cod_ibge = unique(ultima_pop$cod_ibge),
    exercicio = (ano_max_ibge + 1):ano_limite_dados
  ) %>% mutate(cod_ibge = as.character(cod_ibge))
  
  projecao <- anos_faltantes %>% 
    left_join(ultima_pop, by = "cod_ibge")
  
  populacao_tratada <- bind_rows(populacao_tratada, projecao) %>%
    arrange(cod_ibge, exercicio)
}

## indicadores derivados ----
# Resultado Orçamentário Relativo
ind_orc_rcl <- base_painel %>%
  filter(indicador == "Resultado Orçamentário") %>%
  rename(val_resultado = valor) %>%
  inner_join(
    base_painel %>% 
      filter(indicador == "Receita Corrente Líquida") %>% 
      select(cod_ibge, exercicio, periodicidade, periodo, val_rcl = valor),
    by = c("cod_ibge", "exercicio", "periodicidade", "periodo")
  ) %>%
  mutate(
    valor = val_resultado / val_rcl,
    indicador = "Resultado Orçamentário Relativo",
    grupo = "Resultados",
    ordem = 1
  ) %>%
  select(cod_ibge, ente, exercicio, periodicidade, periodo, mes_referencia, indicador, grupo, ordem, valor)

# Resultado Orçamentário per capita
ind_orc_per_capita <- base_painel %>%
  filter(indicador == "Resultado Orçamentário") %>%
  inner_join(populacao_tratada, by = c("cod_ibge", "exercicio")) %>%
  mutate(
    valor = valor / populacao,
    indicador = "Resultado Orçamentário per capita",
    grupo = "Resultados",
    ordem = 1
  ) %>%
  select(cod_ibge, ente, exercicio, periodicidade, periodo, mes_referencia, indicador, grupo, ordem, valor)

# Resultado fiscal = Resultado Primário / RCL
ind_res_fiscal <- base_painel %>%
  filter(indicador == "Resultado Primário") %>%
  rename(val_primario = valor) %>%
  inner_join(
    base_painel %>% 
      filter(indicador == "Receita Corrente Líquida") %>% 
      select(cod_ibge, exercicio, periodicidade, periodo, val_rcl = valor),
    by = c("cod_ibge", "exercicio", "periodicidade", "periodo")
  ) %>%
  mutate(
    valor = val_primario / val_rcl,
    indicador = "Resultado Fiscal",
    grupo = "Resultados", 
    ordem = 1
  ) %>%
  select(cod_ibge, ente, exercicio, periodicidade, periodo, mes_referencia, indicador, grupo, ordem, valor)

base_painel <- bind_rows(
  base_painel,
  ind_orc_rcl,
  ind_orc_per_capita,
  ind_res_fiscal
) %>%
  arrange(cod_ibge, exercicio, ordem, indicador) %>%
  mutate(
    ibge6 = substr(as.character(cod_ibge), 1, 6)
  )

# Monta o objeto para API
df_bimestral <- base_painel[base_painel$periodicidade == "Bimestral", ]

dict_periodos <- lapply(
  split(df_bimestral$periodo, df_bimestral$exercicio),
  function(x) sort(unique(na.omit(as.numeric(x))))
)

#base_painel %>% arrange(desc(exercicio)) %>% View()

base_painel %>% count(exercicio, periodicidade, periodo) %>% print(n=100)

base_painel %>% filter(grupo == 'Despesas') %>% count(indicador)

# Monta o objeto final SEM o dicionário IPCA
objeto_pronto_api <- list(
  dados = base_painel,
  filtros = list(
    ente = sort(unique(base_painel$ente)),
    exercicio = sort(unique(base_painel$exercicio), decreasing = TRUE),
    periodos_por_ano = dict_periodos
  )
)

# 6. Salvar Arquivos Finais ----
saveRDS(populacao_tratada, 'app_aurora_municipios/data/populacao_mun.rds')

saveRDS(objeto_pronto_api, 'app_aurora_municipios/data/base_painel_mun.rds')

write.csv2(base_painel, 'app_aurora_municipios/data/base_painel_mun.csv', row.names = FALSE)

saveRDS(populacao_tratada, 'app_aurora_fiscal/data/populacao_mun.rds')

saveRDS(objeto_pronto_api, 'app_aurora_fiscal/data/base_painel_mun.rds')

write.csv2(base_painel, 'app_aurora_fiscal/data/base_painel_mun.csv', row.names = FALSE)

cat("-> Arquivos locais compilados com sucesso.\n")

# 7. Upload para o Google Drive ----
cat("\n--- Realizando Upload no Google Drive ---\n")

caminho_json_servico <- "data/r-sheets-hugo-0788cc358cb5.json"
drive_auth(path = caminho_json_servico)

id_pasta_rreo <- "1vuu4lPZmCrlVSq2vIbwtz4lM_Vhy-pQ3"
id_pasta_rgf  <- "1YfhlRUJ2r-Kqemi_9Lh_6TYEfskjXQ8x"

tmp_csv_rreo <- tempfile(fileext = "_rreo_mun.csv")
tmp_csv_rgf  <- tempfile(fileext = "_rgf_mun.csv")

write.csv2(cache_raw$rreo_14, tmp_csv_rreo, row.names = FALSE)
write.csv2(cache_raw$rgf_06, tmp_csv_rgf, row.names = FALSE)

drive_upload(
  media = tmp_csv_rreo,
  path = as_id(id_pasta_rreo),
  name = paste0("RREO_Indicadores_Mun_PE_Bimestre_", bimestre_atual, ".csv"),
  overwrite = TRUE
)

drive_upload(
  media = tmp_csv_rgf,
  path = as_id(id_pasta_rgf),
  name = paste0("RGF_Indicadores_Mun_PE_Quadrimestre_", quadrimestre_atual, ".csv"),
  overwrite = TRUE
)

unlink(c(tmp_csv_rreo, tmp_csv_rgf))

cat("-> Upload concluído com sucesso para ambas as pastas!\n")
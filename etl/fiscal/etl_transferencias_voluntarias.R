library(DBI)
library(RSQLite)
library(dplyr)
library(purrr)
library(lubridate)

# 1. Validação do Sistema Operacional e Definição dos Caminhos
is_linux <- Sys.info()[["sysname"]] == "Linux"

if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
  
  path_db <- "/srv/dados/convenios_receitas/db.sqlite3"
  dir_saida_novo <- "/srv/dados/aurora"
} else {
  path_db <- "data/db.sqlite3"
  dir_saida_novo <- "app_aurora_novo/data"
}

# 2. Conexão com o banco de dados SQLite
con <- dbConnect(RSQLite::SQLite(), path_db)

# 3. Extração da tabela core_convenio com todos os relacionamentos e campos
df_convenios_raw <- dbGetQuery(con, "
  SELECT 
    c.modalidade AS modalidade_do_instrumento,
    c.numero_instrumento AS no_do_instrumento,
    c.numero_proposta AS no_da_proposta,
    c.cod_efisco,
    oc.nome AS orgao_concedente,
    cv.nome AS orgao_convenente,
    ex.nome AS orgao_executor,
    c.objeto,
    c.nome,
    c.situacao,
    c.situacao_detalhada,
    c.link_transferegov,
    c.link_saci,
    c.inicio_vigencia,
    c.fim_vigencia,
    c.subacao,
    c.valor_desembolsado_uniao,
    c.valor_pactuado_concedente,
    c.valor_pactuado_convenente,
    c.valor_global,
    c.valor_executado,
    c.municipio,
    c.numero_emenda_loa AS no_emenda_loa,
    c.deputado,
    CASE WHEN c.pac = 1 THEN 'SIM' ELSE 'NÃO' END AS pac_nao_sim,
    c.valor_apto_a_receber AS valor_apto_a_receber_livre,
    c.valor_contingenciado AS valor_contingenciado_que_ainda_tem_pendencia,
    c.historico_texto AS historico,
    c.esfera_pendencia AS esfera_pendencia_estado_uniao,
    CASE WHEN c.meta_prioritaria = 1 THEN 'SIM' ELSE 'NÃO' END AS meta_prioritaria_sim_nao,
    c.observacoes
  FROM core_convenio c
  LEFT JOIN core_orgaoconcedente oc ON c.orgao_concedente_id = oc.id
  LEFT JOIN core_orgaoconvenente cv ON c.orgao_convenente_id = cv.id
  LEFT JOIN core_executor ex ON c.orgao_executor_id = ex.id
  WHERE c.excluido_em IS NULL
")

# Fechar conexão
dbDisconnect(con)

# 4. Aplicação do ETL + Derivação das variáveis calculadas
df_convenios_receita <- df_convenios_raw %>%
  # Converter datas para extração do ano
  mutate(
    inicio_vigencia = as.Date(inicio_vigencia),
    fim_vigencia = as.Date(fim_vigencia),
    ano_inicio_vigencia = year(inicio_vigencia),
    ano_fim_da_vigencia = year(fim_vigencia)
  ) %>%
  filter(!is.na(modalidade_do_instrumento)) %>%
  mutate(
    across(
      where(is.list),
      ~ map_chr(., ~ if(is.null(.x)) NA_character_ else as.character(.x))
    )
  ) %>%
  mutate(
    # Modalidade 
    modalidade_do_instrumento = coalesce(toupper(modalidade_do_instrumento), "OUTRAS TRANSFERÊNCIAS"),
    modalidade_do_instrumento = case_when(
      modalidade_do_instrumento %in% c('CONTRATO DE REPASSE', 'REPASSE') ~ 'REPASSE',
      modalidade_do_instrumento %in% c('CONTRATO DE FINANCIAMENTO') ~ 'FINANCIAMENTO',
      .default = modalidade_do_instrumento
    ),
    # Ministério
    ministerio = orgao_concedente,
    ministerio = case_when(
      ministerio %in% c('CIDADES', 'MINISTÉRIO DAS CIDADES', 'MCID') ~ 'MCID',
      ministerio %in% c('CULTURA', 'MINISTÉRIO DA CULTURA') ~ 'MinC',
      ministerio %in% c('MINISTÉRIO DE PORTOS E AEROPORTOS - MPOR', 'SECRETARIA DE AVIAÇÃO CIVIL - SAC') ~ 'MPOR',
      ministerio %in% c('MEC', 'INEP') ~ 'MEC',
      ministerio %in% c('MINISTÉRIO DA SAÚDE', 'FUNASA') ~ 'MS',
      ministerio %in% c('MINISTÉRIO DO TURISMO') ~ 'MTur',
      ministerio %in% c('JUSTIÇA') ~ 'MJSP',
      ministerio %in% c('MDR/CAIXA', 'MIDR') ~ 'MIDR',
      ministerio %in% c('MINISTÉRIOS DOS TRANSPORTES-DNIT') ~ 'MTR',
      is.na(ministerio) ~ "NI",
      .default = ministerio
    ),
    # Situação / Status
    situacao = toupper(situacao),
    status = case_when(
      situacao %in% c('CLÁUSULA SUSPENSIVA', 'CONDIÇÃO SUSPENSIVA', 'MONITORAMENTO SUSPENSO') ~ 'SUSPENSO',
      situacao %in% c('CONVÊNIO ANULADO') ~ 'ANULADO',
      situacao %in% c('EM LICITAÇÃO', 'EM LICITAÇÃO/ EM CONTRATAÇÃO') ~ 'EM LICITAÇÃO',
      situacao %in% c('EM PRESTAÇÃO DE CONTAS', 'PREST. CONTAS EM ANÁLISE', 'PRESTAÇÃO DE CONTAS EM COMPLEMENTAÇÃO', 'PRESTAÇÃO DE CONTAS ENVIADA PARA ANÁLISE', 'PRESTAÇÃO DE CONTAS', 'PRESTAÇÃO DE CONTAS EM ANÁLISE') ~ 'EM PRESTAÇÃO DE CONTAS',
      situacao %in% c('AGUARDANDO PRESTAÇÃO DE CONTAS', 'PREST. CONTAS EM ABERTO', 'OBRA CONCLUÍDA AGUARDANDO RAE FINAL') ~ 'PRESTAÇÃO DE CONTAS EM ABERTO',
      situacao %in% c('PRESTAÇÃO DE CONTAS CONCLUÍDA') ~ 'PRESTAÇÃO DE CONTAS CONCLUÍDA',
      situacao %in% c('EM REPROGRAMAÇÃO NA CAIXA', 'REPROGRAMAÇÃO APROVADA C.E. 1621/2026') ~ 'REPROGRAMAÇÃO',
      situacao %in% c('EDITAL PUBLICADO NO DOE EM 16/01/2026 ABERTURA EM 02/04/2026 P.L. Nº 003/2026', 'HOMOLOGADO EM 09/02/2026') ~ 'ASSINADO',
      situacao %in% c('LICITADO', 'EM EXECUÇÃO') ~ 'EM EXECUÇÃO',
      situacao %in% c('NÃO INICIADA') ~ 'A INICIAR',
      situacao %in% c('PROPOSTA E PLANO DE TRABALHO APROVADO') ~ 'APROVADO',
      is.na(situacao) ~ "NÃO INFORMADO",
      .default = situacao
    )
  )

# 5. Salvando os arquivos nos caminhos definidos
df_convenios_receita %>% 
  readr::write_rds(file.path(dir_saida_novo, "df_convenios_receita.rds"))

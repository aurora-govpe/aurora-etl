# CONTEXTO DO TRABALHO

Arquivo de contexto das alterações feitas com o Claude neste repositório. Atualizar a cada mudança.

## Regras combinadas com a usuária
- **Antes de qualquer `git push`**: rodar `git fetch` e comparar com `origin/main` (`git status -sb`, `git log HEAD..origin/main`). Se a nuvem mudou, fazer pull/merge primeiro e avisar.
- Não commitar nem dar push sem pedido explícito.
- Manter este arquivo atualizado.

## Projeto
- Repositório: https://github.com/aurora-govpe/aurora-etl (clone local: `C:\Users\kattinecosta\aurora-etl`)
- Stack: **R** (scripts em `etl/`, utilitários em `R/`) + alguns RPAs em Python (`etl/rpa/`).
- Orquestrador: `etl/ETL_AUTO.R` (chamado por `run_pipeline.R`).
- Entrada (raw): `C:/dados/dados brutos/aurora`; saída (gold): `C:/dados/aurora` (ver `R/paths.R`).
- Saída consumida pelos portais/apps front end (parquet, rds, json, geojson).

## Ambiente local (2026-11-05)
- Git 2.56 instalado via winget em `%LOCALAPPDATA%\Programs\Git` (ainda fora do PATH da sessão do Claude).
- **R não está instalado** nesta máquina e `C:\dados` não existe: o código novo ainda **não foi executado/testado**.

## Histórico
### 2026-11-05 — Log local
- Criado `R/logger.R`: log em `logs/etl_AAAA-MM-DD.log` (pasta alterável por `AURORA_LOG_DIR`). Funções: `log_info`, `log_warn`, `log_error`, `log_step` (mede tempo, captura warnings e erros).
- `etl/ETL_AUTO.R`: cada script agora roda dentro de `log_step`. Por padrão continua parando no primeiro erro (comportamento anterior); `AURORA_ETL_CONTINUE_ON_ERROR=true` faz seguir e listar falhas ao final.
- `logs/` e `*.log` já estavam no `.gitignore`.
- Limitação: `cat()`/`print()` dos scripts ainda vão só ao console; `message()` também. Só warnings/erros e início/fim/duração entram no log.

### 2026-11-05 — R instalado e captura de saída
- R 4.6.2 instalado via winget em `%LOCALAPPDATA%\Programs\R\R-4.6.2` (Rscript em `bin\Rscript.exe`).
- `R/logger.R`: adicionados `log_capture_start()` / `log_capture_stop()` (sink com split: tudo que `cat`/`print` imprimem vai ao arquivo e ao console) e captura de `message()` dentro de `log_step` (nível MSG). Linhas de `cat`/`print` entram sem data/hora.
- `etl/ETL_AUTO.R`: captura ligada durante o loop e encerrada em `finally`.
- Logger testado com Rscript num script de teste (info, cat, print, message, warning, erro com e sem parada): OK. `ETL_AUTO.R` completo ainda não foi executado (faltam `C:\dados` e pacotes como tictoc/glue).

## ETL nova em andamento
- Objetivo: carregar no Aurora os dados de **um indicador**, vindos de **uma planilha**.
- Pendente: arquivo da planilha, nome do indicador, colunas, formato de saída, regras de transformação.
- Convenção do repo: ler de `raw_path(...)`, gravar em `out_path(...)` (R/paths.R); saídas em parquet/rds/json. Há exemplo de envio ao S3 (`R/exemplo_datalake.R`, bucket `sds`, credenciais via `.Renviron`, nunca no Git).

## Instruções da chefia (Hugo Medeiros, Seplag PE — repassadas pela usuária em 2026-11-05)
- Padrão de pastas: `C:\dados\dados brutos` (entrada) e `C:\dados\aurora` (saída).
- Scripts de referência para indicadores: `etl/municipios/etl_caracter_mun.R` (municípios), `etl_indicadores_ufs.R` (UFs) e `etl_indicadores_regioes_br.R` (regiões e Brasil).
- Dado bruto novo vai em `C:\dados\dados brutos\aurora\<pasta do tema>` (ex.: `educacao`, `saude`).
- A planilha que atualiza a ficha de indicador: `C:\dados\dados brutos\aurora\indicadores\lista de indicadores.xlsx`.

## Padrão observado nos scripts de indicadores (UFs / regiões)
- Lê o dado bruto de `file.path(DIR_RAW_AURORA, "<tema>/<arquivo>")`, normaliza para formato longo e colunas: chave territorial (`cod_uf, sg_uf, nm_uf` ou `cod_regiao, sg_regiao, nm_regiao`), `ano`, `indicador` (texto), `mes_referencia`, `sigla_indicador`.
- Descarta valores `"-"`, `"ND"`, `"nan"`, `NA`.
- Junta com a aba de metadados da `lista de indicadores.xlsx` (aba `UFs`, dicionários `dic_UFs` e `dic_Regioes`) por `sigla_indicador`, trazendo `nm_indicador, nm_fantasia, grupo, tipo, polaridade`.
- Grava parquet em `DIR_OUT_AURORA/parquet/df_indicadores_ufs.parquet` e `df_indicadores_regioes.parquet`. Municípios: `df_indicadores.parquet` (+ rds).
- Observação: os scripts de UFs/regiões chamam `View()`/`glimpse()` e, no de UFs, há um caminho relativo `data/meio ambiente/indicadores_mapbiomas.parquet`.
- Um novo indicador exige: (1) o dado bruto na pasta do tema, (2) uma função `extrair_*` no script do nível territorial certo, (3) a `sigla_indicador` cadastrada na `lista de indicadores.xlsx`.

## Indicador novo: Proporção de crianças de 5 a 10 anos abaixo do peso ideal (2026-11-09)
- Fonte: Observatório da Criança e do Adolescente (Fundação Abrinq). Planilha em `Downloads` (2 cópias idênticas, MD5 igual); uma cópia já existe em `C:\dados\dados\dados brutos\aurora\saude\`.
- Formato: aba 2, `skip = 2`; colunas `Localidade, Código IBGE, Código DataSUS` + `AAAA (%)` e `AAAA (absoluto)` de 2008 a 2025. Mesmo formato do bloco `df_atliqcreche_long` de `etl_caracter_mun.R`. Níveis: Brasil, regiões, UFs e 5.570 municípios (IBGE 7 dígitos); PE = 185 (184 + Fernando de Noronha incluso). Valores ausentes vêm como `-` (78 células de % em PE).
- Join do script municipal é por `nm_indicador` (aba `Sheet1` da lista), não por sigla.
- **Atenção 1:** `etl_caracter_mun.R` multiplica por 100 todo valor entre 0 e 1 quando `tipo == "percentual"`. Neste indicador os valores já estão em %, e 129 valores de PE ficam entre 0 e 1 — seriam corrompidos. Precisa de exceção por sigla.
- **Atenção 2 (RESOLVIDA em 2026-11-09):** as pastas estavam em `C:\dados\dados\...` (um nível a mais). Com autorização da usuária, `aurora`, `dados brutos` e `mulungu` foram movidas para `C:\dados\` e a pasta `dados\dados` vazia foi apagada. Agora vale o padrão do `paths.R` sem variáveis de ambiente (`C:/dados/dados brutos/aurora` e `C:/dados/aurora`). Os caminhos `C:\dados\dados\...` citados acima neste arquivo são antigos.
- Pacotes R instalados em `R_LIBS_USER`: readxl, tidyverse, arrow, janitor, glue, tictoc.

### Feito neste indicador (2026-11-09)
- `C:\dados\dados\dados brutos\aurora\indicadores\lista de indicadores.xlsx` (aba Sheet1): adicionadas 2 linhas — `peso_abaixo_5_10_pct` (percentual) e `peso_abaixo_5_10_abs` (absoluto), grupo saúde, polaridade negativa. Backup idêntico ao original na mesma pasta: `lista de indicadores.BACKUP_2026-11-09.xlsx`. Demais abas conferidas e intactas.
- `etl/municipios/etl_caracter_mun.R`: novo bloco "9.1" (`df_peso_ideal_long`), incluído no `bind_rows` final, e exceção na regra de conversão 0-1 → % para `peso_abaixo_5_10_pct`.
- Testado só o bloco novo + join com a lista + regra (script completo não roda aqui: baixa dados do Google Drive com credencial): 185 municípios, 2008-2025, 3.252 valores de % e 3.330 absolutos, 0 sem match na lista, 0 valores alterados pela regra, conferido Abreu e Lima 2025 (2,16% / 55).
- NÃO feito: execução completa do `etl_caracter_mun.R`, commit e push.

## Pendências / perguntas em aberto
- Detalhes da planilha e do indicador (ver acima).
- Destino: arquivo em `C:/dados/aurora` ou bucket S3?

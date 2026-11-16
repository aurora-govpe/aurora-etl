# CONTEXTO DO TRABALHO

Arquivo de contexto das alterações feitas com o Claude neste repositório. Atualizar a cada mudança.

## Regras combinadas com a usuária
- **Antes de qualquer `git push`**: rodar `git fetch` e comparar com `origin/main` (`git status -sb`, `git log HEAD..origin/main`). Se a nuvem mudou, fazer pull/merge primeiro e avisar.
- **Nunca push direto na `main`** (instrução do Hugo, 2026-11-09): o trabalho vai numa branch e vira Pull Request. Em 2026-11-09 a conta da usuária só tinha leitura (push 403); em 2026-11-10 já tinha escrita, a branch foi enviada e a usuária abriu a PR pelo navegador (sem `gh` instalado). Mesmo com escrita, nunca na `main`.
- **Repositório do app/front-end do Aurora e servidor (instrução do Hugo, 2026-11-12):** nenhum push, commit ou carga (`etl_carga_servidor.R`). Só clonar para ler e rodar em localhost, com os dados locais em modo leitura. O link do repositório do app será enviado pela usuária.
- Antes de qualquer indicador novo ir adiante: testar se ele aparece na **base final** (`df_indicadores.parquet`).
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
- Teste na base final (2026-11-09): `etl_caracter_mun.R` completo rodou OK em ~74 s (via `log_step`, log em `logs/`). Em `C:/dados/aurora/parquet/df_indicadores.parquet`: `peso_abaixo_5_10_pct` (3.252 linhas) e `peso_abaixo_5_10_abs` (3.330), 185 municípios, 2008-2025, grupo/tipo/polaridade preenchidos, `rank_pe` e `ibge6` calculados, 0 divergências contra a planilha, 129 valores entre 0 e 1 preservados (sem ×100).
- **Achado:** `etl_caracter_mun.R` regera `df_indicadores.parquet` do zero e **apaga os 6 indicadores INFOCOM** (`emp_infocom`, `estab_infocom`, `pct_emp_infocom_serv`, `pct_estab_infocom_serv`, `pct_vab_infocom_serv`, `ql_infocom_serv`; 4.440 linhas). Eles são acrescentados depois por `etl/municipios/etl_infocom_servicos.R`, que **não está no `ETL_AUTO.R`** e usa caminho fixo `C:/Users/hugo.vasconcelos/...` (não roda em outras máquinas). Não é causado por este indicador; avisar o Hugo. Localmente as linhas foram restauradas a partir do backup.
- Backup dos parquet/rds anteriores à execução: `C:\Users\kattinecosta\Documents\backup_etl_2026-11-09\`.
- 2026-11-10: permissão de escrita confirmada; branch `feat/indicador-peso-ideal` rebaseada na `main` (b84c9a6) e enviada; PR aberta pela usuária no navegador. Pendentes fora do Git: 2 linhas na `lista de indicadores.xlsx` e aviso do achado INFOCOM ao Hugo.

## Decisões revisáveis (registro para revisão/revogação posterior)
| ID | Data | Quem decidiu | Decisão | Como revogar |
|---|---|---|---|---|
| D-001 | 2026-11-10 | Kattine Costa Pedrosa (usuária) | Na planilha de óbitos maternos, o `-` (que de 2016 em diante substitui o 0 nos municípios) é tratado como **0 óbito**. Evidência a favor: de 2000 a 2015 o 0 vem explícito e não há `-`; de 2016 a 2025 não há nenhum 0 e o `-` aparece em ~140-150 municípios/ano; a soma dos 185 municípios de PE bate com o total da UF em todas as 182 combinações categoria x ano (0 divergências). | `AURORA_OBITOS_MAT_TRACO_COMO_ZERO=false` (o `-` passa a ser descartado) ou remover o tratamento em `R/obitos_maternos.R`. Cada execução registra no log (WARN) quantas células foram convertidas. |
| D-002 | 2026-11-11 | Kattine Costa Pedrosa (usuária) | Nas 4 planilhas de **acidentes de trabalho (5 a 17 anos)**, o `-` é tratado como **0 notificação** (municípios e UFs). Ressalva: nos municípios o 0 explícito só aparece em 2017-2019 e nesses anos convive com `-` (ex.: PE 2017: 88 zeros e 85 `-`); fora desses anos tudo é `-`. A soma dos 185 municípios bate com a UF em 335 de 336 combinações sigla x ano (única exceção: `acid_trab_evol_cura` 2010, municípios 1 x UF 4, divergência da própria fonte). | `AURORA_ACIDENTES_TRABALHO_TRACO_COMO_ZERO=false` (o `-` passa a ser descartado) ou remover o tratamento em `R/abrinq_planilhas.R`. Log (WARN) a cada execução com o nº de células convertidas. |
| D-003 | 2026-11-11 | Kattine Costa Pedrosa (usuária) | **Mortalidade por saneamento inadequado, municípios:** a taxa por 100 mil hab. só é carregada **até 2019**. De 2020 em diante a taxa municipal da planilha está em escala inconsistente (Recife 2024 = 0,06, esperado ~7; população implícita de 112 milhões em 2020). O absoluto municipal e todas as séries das UFs entram completos. Os "-" dessa planilha **não** viram 0 (o 0 aparece explícito no absoluto). | `AURORA_MORT_SANEA_TAXA_MUN_ANO_MAX=2025` (carrega todos os anos) ou ajustar em `R/abrinq_planilhas.R`. Log (WARN) a cada execução com o nº de valores descartados. |
| D-004 | 2026-11-12 | Kattine Costa Pedrosa (usuária) | **Trabalho infantil, apenas autoconsumo:** em 4 das 5 séries (5-9, 10-13, 14-15, 16-17 anos) todo o 2022 é idêntico ao 2019 (28 de 28 linhas) → cópia da fonte; **2022 descartado** nessas 4 séries (o total 5-17 mantém 2022, que difere de 2019). | `AURORA_TI_AUTOCONSUMO_2022_DESCARTAR=false`. Log (WARN) a cada execução com o nº de valores descartados. |
| D-005 | 2026-11-12 | Kattine Costa Pedrosa (usuária) | **Trabalho infantil, Lista TIP:** o % de 2022 das UFs nas 4 faixas etárias está em escala errada (denominador implícito ~755 mil = total do Brasil em todas as UFs; PE 14-15 anos = 0,35% com 2.660 casos, enquanto nos outros anos fica ~25%). **Descartado só o % de 2022 das UFs** nessas 4 faixas; absolutos e total 5-17 ficam. | `AURORA_TI_TIP_PCT_2022_UF_DESCARTAR=false`. Log (WARN) a cada execução. |
| D-006 | 2026-11-12 | Kattine Costa Pedrosa (usuária) | **Trabalho infantil, atividades econômicas ou autoconsumo, 16-17 anos:** o % de 2017 é ~2x o esperado (Brasil 29,4% contra 15,9% em 2016 e 15,3% em 2018; o absoluto é coerente). **Descartado só o % de 2017** dessa série. | `AURORA_TI_ECON_1617_PCT_2017_DESCARTAR=false`. Log (WARN) a cada execução. |
| D-007 | 2026-11-16 | Kattine Costa Pedrosa (usuária) | Nas 3 planilhas de **casamentos registrados de meninas** (total menores de 19, 15 a 19, menores de 15), o `-` é tratado como **0 casamento** (municípios e UFs). Evidência: em PE o `-` aparece em no máximo 1 município por ano na aba total e a soma dos demais municípios bate com a UF em todos os 22 anos (2003-2024); com o tratamento, a soma dos 185 municípios = UF em 66 de 66 combinações série x ano. Células em branco continuam descartadas. | `AURORA_CASAMENTOS_TRACO_COMO_ZERO=false` (o `-` passa a ser descartado) ou remover em `R/abrinq_planilhas.R`. Log (WARN) a cada execução com o nº de células convertidas. |

## Lote Abrinq 3: Gini, medidas socioeducativas, casamentos de meninas e Lista TIP 14-17 anos trimestral (2026-11-16)
- Cinco planilhas de `docs/` (Observatório da Criança e do Adolescente), copiadas para `economia/` (Gini) e `cidadania/` (as outras 4). **14 séries novas** no mesmo `R/abrinq_planilhas.R` (catálogo `lote3`); nenhuma mudança nos scripts de UFs e municípios, que já chamam `ler_abrinq_lote()`.
- **Coeficiente de Gini** (`gini_desigualdade`): só UFs, 2012-2025, grupo `economia`, tipo `índice`, polaridade negativa (decisão da usuária: grupo economia). Valores 0,408-0,601; fora da regra 0-1 → % porque o tipo não é percentual.
- **Medidas socioeducativas de privação/restrição de liberdade** (6 séries `socioedu_medida_*`: internação, internação provisória, semiliberdade, internação sanção, outros, total): só UFs, absoluto, cidadania. O `-` é **descartado** (anos sem levantamento): internação sanção só tem 2023 (23 UFs); "outros" só 2015-2016; internação provisória sem 2017; total 2008-2017 e 2023-2025. Não confundir com `pop_socioeducanda`/`tx_socioeducanda` do bloco de Segurança Pública (outra fonte).
- **Casamentos registrados de meninas** (3 séries `casamento_fem_ate19_total|15_19|ate15`): municípios de PE (185) e UFs, 2003-2024, absoluto, cidadania. `-` = 0 (**D-007**). Soma dos 185 municípios = UF em todas as 66 combinações série x ano. Células em branco (não `-`) são descartadas (1 na aba total, 180 em "menores de 15").
- **Lista TIP 14-17 anos, trimestral** (`tip_ocup_14_17_pct|abs`: ocupados de 14 a 17 em atividades da Lista TIP, % da população ocupada da faixa; `ti_ocup_14_17_pct|abs`: adolescentes de 14 a 17 em trabalho infantil sobre a população ocupada da faixa): só UFs, 1T2012 a 2T2026 (58 trimestres), carregados como vieram com `ano` + `mes_referencia` = "T1".."T4" (decisão da usuária). Primeiro dado trimestral do repositório: 1.566 linhas por sigla (27 x 58). % arredondado em 2 casas. Sem `-`.
- Mudança no helper: a coluna `traco_zero` do catálogo passou de lógico para o nome da variável de ambiente da decisão (`.abq_traco_decisoes`: D-002 acidentes, D-007 casamentos); colunas `Trimestre N/AAAA` e `Trimestre N/AAAA (absoluto)` reconhecidas como % e nº.
- Lista de indicadores: +14 linhas em `Sheet1` e +14 em `UFs` (backup `lista de indicadores.BACKUP_2026-11-16.xlsx`); cópia atualizada também no kit de migração.
- Teste isolado (R 4.5.0, sem os brutos completos): UFs 35.733 linhas no helper (26.041 dos lotes anteriores + 9.692 novas), 14 siglas, 27 UFs, 0 sem metadados após a lista; municípios 75.917 (63.888 + 12.029 de casamentos), 185 municípios, 0 sem match na Sheet1. Revogação de D-007 testada (12.029 → 10.526 linhas).
- **Teste na base final (2026-11-16, máquina nova com os brutos completos copiados de `C:\dados1`):** `etl_caracter_mun.R` completo OK em 80 s (238.124 registros; INFOCOM restaurado depois a partir de `C:\dados1`): as 3 siglas de casamentos com 185 municípios, 2003-2024, grupo/tipo/polaridade e `rank_pe` preenchidos, Recife 2024 = 188 / 188 / 0; peso ideal, óbitos maternos e os 45 indicadores Abrinq municipais presentes. `etl_indicadores_ufs.R` completo OK (101.595 registros; 174 siglas): as 14 siglas do lote 3 com 27 UFs, as 4 trimestrais com `mes_referencia` T1-T4 e 1.566 linhas cada; 81 de 81 siglas Abrinq e 7 de óbitos maternos presentes; o JSON `metadados_indicadores_ufs.json` traz as 14. PE 2025: Gini 0,52, total socioeducativo 443.
- **Dois achados preexistentes da `main` (não são deste lote; avisar o Hugo):** (1) o CSV `indicador_cad_pessoas_pe.csv` do Google Drive passou a vir com as colunas `referencia_cad_unico` e `pessoas` em vez de `referencia` e `pessoas_cad_unico`, e `etl_caracter_mun.R` para com "Column `referencia` doesn't exist" (bloco Vulnerabilidade); o teste rodou numa cópia temporária com um `rename()`, o script do repositório não foi alterado. (2) `seguranca/df_seguranca_publica_forum_nacional.xlsx` não tem as abas `feminicidio`, `populacao_presa` e `socioeducandos` que o bloco de Segurança do script de UFs lê (tem `mvi, cvp, rfv, celulares, mdip, obito_prisional, suicid_pol, vitim_pol`); o teste pulou o bloco. O trecho final de exportação para o Google Sheets também foi excluído do teste de propósito (escreve na planilha do Hugo).
- Branch `feat/lote-abrinq-gini-liberdade-casamentos-tip`, empilhada sobre `feat/lote-abrinq-trabalho-infantil` (já com a `main` de 2026-11-15 mesclada). PR própria.

## Lote Abrinq: trabalho infantil (5 a 17 anos) e ocupação em estabelecimentos agropecuários (2026-11-12)
- Quatro planilhas da Abrinq em `cidadania/` (cópias de Downloads), tema **cidadania**; **27 séries novas** no mesmo `R/abrinq_planilhas.R` (catálogo, `abrinq_lote_metadados()`, exclusões D-004/D-005/D-006).
- **Atividades econômicas ou autoconsumo** (10 séries): total 5-17 + faixas 5-9/10-13/14-15/16-17, cada uma em % (da população da faixa) e nº. **Lista TIP** (10 séries): mesmas faixas; o % é **composição** (nas faixas, % do total TIP; no total 5-17, % dos que estão em trabalho infantil). **Apenas autoconsumo** (5 séries): só nº. Só **UFs** (27), anos 2016-2019 e 2022-2025 (2020-2021 não existem na fonte). Siglas `ti_econ_*`, `ti_tip_*`, `ti_autoc_*`.
- **Ocupados até 14 anos em estabelecimentos agropecuários** (2 séries, `ocup_agro_ate14_pct|abs`): **municípios de PE (185) e UFs (27), só 2017**. A sigla `%` entrou na exceção da regra 0-1→% do consolidado (Recife = 0,85%). Soma dos 185 municípios = 26.487 contra 27.629 da UF (diferença da própria fonte; a linha "Município Ignorado - PE" é `-`).
- Tipo `percentual` nos %, `absoluto` nos nº; grupo `cidadania`; polaridade `negativa`.
- Branch `feat/lote-abrinq-trabalho-infantil`, empilhada sobre `feat/lote-abrinq-cidadania-saneamento`. Exige 27 linhas novas em `Sheet1` e 27 em `UFs` da `lista de indicadores.xlsx` (backup `…BACKUP_2026-11-12.xlsx`).
- Teste: `etl_caracter_mun.R` completo OK (226.095 linhas; INFOCOM restaurado → 230.535); as 2 siglas agro com 185 municípios cada, rank_pe calculado, % dentro de 0-100. UFs: bloco testado isolado (5.205 linhas, 27 UFs, 0 sem metadados); a soma das faixas bate com o total (PE 2025 = 48.321).

## Lote Abrinq: acidentes de trabalho (5 a 17 anos) e mortalidade por saneamento inadequado (2026-11-11)
- Cinco planilhas do Observatório da Criança e do Adolescente (Abrinq), em `cidadania/` (4 de acidentes, tema **cidadania**) e `saude/` (mortalidade, tema **saúde**). Leitura única em `R/abrinq_planilhas.R` (catálogo com as 40 siglas; `abrinq_lote_metadados()` gera as linhas da lista de indicadores). Abas localizadas pelo **rótulo da célula A2**, não pela ordem (as abas vêm embaralhadas).
- **Acidentes (21 séries, absolutas, município PE + UF, 2010-2025):** total 5-17 (`acid_trab_5_17_total`), faixas 5-9/10-14/15-17, cor/raça (6), sexo (3) e evolução dos casos (8). Município: 62.160 linhas; UF: 9.072.
- **Mortalidade por saneamento inadequado (19 séries, 2000-2025):** 10 grupos etários (incl. total) x {taxa, nº de óbitos}; "idade ignorada" só tem nº (a taxa é toda `-`). A coluna "(%)" da planilha é, na verdade, **taxa por 100 mil hab.** (conferido pela população implícita de PE); por isso tipo `índice` e fora da regra 0-1 → % do consolidado. UFs: 27; municípios: só 5 de PE (Recife, Jaboatão, Olinda, Caruaru, Petrolina). Taxa arredondada em 2 casas.
- **Decisão sua: o "Total geral" de todas as idades não entra.** As 4 planilhas de acidentes repetem o total de todas as idades (44.767 no Brasil em 2010) e o total 5-17 aparece em 3 arquivos; o total 5-17 é carregado uma vez (do arquivo de cor/raça).
- **Problemas da fonte (mantidos como vieram):** (1) a aba "Notificações de acidentes... 5 a 17 anos" do arquivo de faixas etárias tem a coluna 2019 duplicada (1.660 x 1.655) e não é usada; (2) a soma por sexo não fecha com o total em 2.429 células (ex.: Brasil 2020: 462 x 1.593); por evolução em 192; por faixa etária em 175; por cor/raça em 9; (3) o arquivo de faixas e o de evolução não têm a linha "Município Ignorado" (5.604 linhas contra 5.631).
- Branch `feat/lote-abrinq-cidadania-saneamento`, empilhada sobre `feat/indicador-obitos-maternos-cor-raca` (que está sobre `feat/indicador-peso-ideal`). PR própria, separada das duas anteriores (decisão da usuária).
- Exige 40 linhas novas em `Sheet1` e 40 em `UFs` (aba UFs) da `lista de indicadores.xlsx` (backup `lista de indicadores.BACKUP_2026-11-11.xlsx`). Para gerar: `abrinq_lote_metadados()`.
- Teste: `etl_caracter_mun.R` completo OK; na base final as 40 siglas, 63.518 linhas, grupo/tipo/polaridade preenchidos, `rank_pe` calculado. `etl_indicadores_ufs.R` completo ainda falha no bloco de Segurança Pública (`rf_celulares`, preexistente); o bloco novo foi testado isolado (20.836 linhas, 27 UFs, 0 sem metadados).

## Indicador novo: Óbitos maternos por cor/raça (2026-11-10)
- Fonte: Observatório da Criança e do Adolescente (Fundação Abrinq), `saude/Número de óbitos maternos segundo cor_raça.xlsx` (cópia da `(3)` de Downloads; as 4 cópias tinham md5 diferentes, 2 delas de ~3 KB). Só contagens (absolutos), 2000-2025.
- 7 indicadores (`obitos_mat_indigena|preta|branca|amarela|parda|ignorada|total`), grupo saúde, tipo absoluto, polaridade negativa, em município (PE, 185; exclui a linha "Município Ignorado - PE" 2600000) e UF (27). Leitura única em `R/obitos_maternos.R`, usada por `etl_caracter_mun.R` (bloco 5.8.2) e `etl_indicadores_ufs.R` (bloco 5.7.1).
- Esperado: municípios 185 x 26 anos x 7 = 33.670 linhas; UFs 27 x 26 x 7 = 4.914. Conferido no helper.
- Branch `feat/indicador-obitos-maternos-cor-raca`, **empilhada** sobre `feat/indicador-peso-ideal` (precisa do logger e deste arquivo, que só existem naquela PR).
- Exige 7 linhas novas em `Sheet1` e 7 em `UFs` da `lista de indicadores.xlsx` (backup `lista de indicadores.BACKUP_2026-11-10.xlsx`).

## Pasta `docs/` — planilhas-fonte (2026-11-12)
- A partir de 2026-11-12 a usuária guarda as planilhas-fonte (bases) em `docs/` (antes ficavam em Downloads). A pasta está no `.gitignore`, então nada dela vai ao Git.
- Em toda tarefa, listar o `docs/` e comparar com as bases já processadas (`ABRINQ_ARQUIVOS`, `R/obitos_maternos.R`, peso ideal) para achar arquivos novos ou alterados.
- O ETL continua lendo de `C:\dados\dados brutos\aurora\<tema>`. Ao processar uma base, copiar do `docs/` para lá; os caminhos do código não mudam.

## Migração de máquina (2026-11-13)
- A usuária vai trocar de computador e continuar a missão na máquina nova. O kit de migração está em `Documents\kit_migracao_aurora\` (instruções em `LEIA-ME.md` dentro do kit).
- Fora do Git e essenciais: `lista de indicadores.xlsx` (com as 76 linhas novas: 2 + 7 + 40 + 27), `docs/`, textos das PRs e memória do Claude Code. As saídas em `C:\dados\aurora` podem ser regeneradas pelo ETL, e os brutos completos (`C:\dados\dados brutos\aurora`, ~3,6 GB) devem ser copiados à parte.
- Estado na troca: 4 PRs empilhadas (peso ideal → óbitos maternos → lote Abrinq 1 → lote trabalho infantil), nenhuma na `main` (c8a118a em 2026-11-12). Branch de trabalho: `feat/lote-abrinq-trabalho-infantil`.

## Máquina nova e merge da `main` (2026-11-16)
- Máquina nova montada: clone em `C:\Users\kattinecosta\aurora-etl`, Git 2.56, R 4.5.0 (`C:\Program Files\R\R-4.5.0`; o 4.6.2 não tinha instalador winget por usuário), os 33 pacotes do kit instalados em `R_LIBS_USER`. Recriados `C:\dados\dados brutos\aurora\{indicadores,saude,cidadania}` (lista de indicadores com as 76 linhas + planilhas Abrinq, óbitos maternos e peso ideal) e `C:\dados\aurora`. Em 2026-11-16 (tarde) a usuária trouxe a cópia completa da máquina antiga em `C:\dados1` (3,9 GB: brutos, saídas e mulungu, com a credencial do Google); foi mesclada em `C:\dados` com `robocopy /E /XO` (nada mais novo foi sobrescrito; `C:\dados1` ficou como backup). Desde então os ETLs completos rodam aqui.
- Estado na nuvem em 2026-11-16: as 4 PRs (#1 a #4) continuam abertas, sem comentário nem revisão, todas com base `main`. A `main` recebeu 6 commits da Julia Barreto (11 a 15/11): `ETL_AUTO.R` reescrito como orquestrador por periodicidade; `etl_indicadores_ufs.R` com passageiros aéreos, JSON de metadados (`json/metadados_indicadores_ufs.json`, colunas novas da lista: eixo, subdimensão, natureza, fonte, hiperlink, fórmula) e exportação para Google Sheets no final (token `r-sheets-hugo-*.json` em `DIR_RAW_AURORA`); gerador de PDFs, agricultura SIDRA, RPA ANAC, `localizar_arquivos.R`.
- **Merge `origin/main` (db1261e) → `feat/lote-abrinq-trabalho-infantil`** (commit de merge, sem rebase, para não reescrever as PRs abertas). Conflitos resolvidos: (1) `etl/ETL_AUTO.R`: ficou a versão da `main` com o logger reinserido (cada script da fila, R ou Python, roda dentro de `log_step`; `log_capture_start/stop`; `AURORA_ETL_CONTINUE_ON_ERROR`); (2) `etl/municipios/etl_indicadores_ufs.R`: mantidos os `source()` de `R/obitos_maternos.R` e `R/abrinq_planilhas.R` e os blocos 5.7.1/Abrinq no `bind_rows`, junto com tudo da `main`. Depois do merge a branch mergeia limpo com a `main`.
- Atenção: a lista de indicadores do kit ainda não tem as colunas novas de metadados que a `main` passou a ler (`any_of`, não quebra). Conferir com o Hugo se existe versão nova da planilha.
- Bases novas em `docs/` ainda não processadas (5, de 2026-11-13): Coeficiente de Gini; adolescentes em privação/restrição de liberdade; casamentos de crianças e adolescentes do sexo feminino; proporção da população ocupada de 14 a 17 anos em atividades da Lista TIP; proporção de adolescentes de 14 a 17 anos em trabalho infantil sobre a população ocupada.

## Pendências / perguntas em aberto
- Detalhes da planilha e do indicador (ver acima).
- Destino: arquivo em `C:/dados/aurora` ou bucket S3?

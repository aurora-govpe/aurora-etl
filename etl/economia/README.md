# ETL de Empresas - Plataforma Aurora

Pipeline automatizado para processamento das bases cadastrais de empresas (Ativas e Inativas) da SEPLAG de Pernambuco.

---

## 1. Arquitetura dos Dados

### Fontes de Entrada (Raw / Bronze):
* `C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_ATIVAS.parquet` (971.031 registros)
* `C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_INATIVAS.parquet` (964.807 registros)
* `C:/dados/aurora/geojson/pernambuco_municipios.geojson` (Malha oficial dos 185 municípios de PE)

### Saídas Consolidadas (Curated / Gold):
* **Parquets em `C:/dados/aurora/parquet/`:**
  * `df_empresas_saldo_mensal_pe.parquet`: Série temporal do saldo estadual de empresas mês a mês (Aberturas, Fechamentos, Saldo Líquido e Acumulado de 2000 a 2026).
  * `df_empresas_saldo_mensal_porte.parquet`: Saldo mês a mês detalhado por porte (MEI, Microempresa, Pequeno Porte, Demais).
  * `df_empresas_saldo_mensal_muni.parquet`: Saldo mês a mês desagregado por município de Pernambuco.
  * `df_empresas_resumo_municipal.parquet`: Tabela consolidada com os 185 municípios contendo contagem de ativas, inativas, MEI, ME, EPP, taxa de MEIzação, idade média, tempo médio de vida até baixa, capital social total/mediano e saldo dos últimos 12 meses.
  * `df_empresas_faixa_etaria.parquet`: Distribuição de empresas ativas e inativas por faixas de idade (`Até 1 ano`, `1 a 2 anos`, `2 a 5 anos`, `5 a 10 anos`, `10 a 20 anos`, `Mais de 20 anos`), por município e setor.
  * `df_empresas_cnae_setor.parquet`: Segmentação das empresas por Grandes Setores da economia (Comércio, Serviços, Indústria, Construção, etc.) e Divisões do CNAE.
  * `df_empresas_bairros_recife.parquet`: Análise intraurbana dos bairros do Recife (maior polo econômico do estado).
* **GeoJSON em `C:/dados/aurora/geojson/`:**
  * `empresas_municipios_pe.geojson`: Camada cartográfica com os 185 municípios de Pernambuco enriquecida com todas as métricas de empresas prontas para visualização em mapas temáticos.

---

## 2. Regras de Tratamento e Negócio

1. **Decodificação de Datas:**
   * A coluna `dt_inicio_atividade` na base de ativas continha números de série originários do Excel (ex: 28853 = 1978, 40893 = 2011). Foi aplicada conversão com `origin='1899-12-30'`, expurgando anos anômalos (como 1500 ou valores acima de 2030).
2. **Definição de Data de Baixa (Inativas):**
   * Como a coluna `dt_termiativ` estava nula, a data efetiva de encerramento foi consolidada via coalescência entre `dt_ult_alteracao` e `dt_digitacao`.
3. **Classificação de Porte e MEI:**
   * Empresas com `in_identifica_mei == 'S'` foram isoladas na categoria prioritária `MEI`.
   * Demais empresas foram mapeadas para `Microempresa (ME)`, `Pequeno Porte (EPP)` ou `Demais Portes`.
4. **Normalização e Compatibilização Municipal:**
   * Nomes de municípios foram limpos de acentos e tratados com dicionário de sinônimos/grafias históricas (`Iguaraci` -> `Iguaracy`, `São Caitano` -> `São Caetano`, `Lagoa do Itaenga` -> `Lagoa de Itaenga`), alcançando 100% de junção com a malha do IBGE.

---

## 3. Como Executar

### Via Python (Motor Otimizado):
```bash
python etl/empresas/etl_empresas.py
```

### Via R:
```R
source("etl/empresas/etl_empresas.R")
```

# aurora-etl

Repositório central de engenharia de dados e pipelines (ETL) para a plataforma **Aurora** (e preparado para novas plataformas como **Argos**).

## 1. Arquitetura de Pastas e Dados

Este repositório não armazena dados binários. Ele lê as fontes de entrada da **camada bruta (*Raw*)** e escreve os dados consolidados e otimizados na **camada de consumo (*Out*)** dos aplicativos.

| Ambiente | Entrada (Raw / Bronze) | Saída (Curated / Gold) |
| :--- | :--- | :--- |
| **Windows (Local)** | `C:\dados\dados brutos\aurora` | `C:\dados\aurora` |
| **Linux (Servidor)** | `/srv/dados_brutos/aurora` | `/srv/dados/aurora` |
| **Futuro: Argos** | `C:\dados\dados brutos\argos` | `C:\dados\argos` |

## 2. Variáveis de Ambiente

Os caminhos são resolvidos dinamicamente por [`R/paths.R`](file:///C:/Users/hugo.vasconcelos/Documents/projetos/aurora/aurora-etl/R/paths.R). Você pode sobrescrever os diretórios definindo no seu `.Renviron`:

```ini
AURORA_RAW_DIR="C:/dados/dados brutos/aurora"
AURORA_DATA_DIR="C:/dados/aurora"

# Para novas plataformas
ARGOS_RAW_DIR="C:/dados/dados brutos/argos"
ARGOS_DATA_DIR="C:/dados/argos"
```

Se as variáveis não forem informadas, o sistema detecta o SO e aplica os padrões padrão automaticamente.

## 3. Como Executar

### Execução Completa (Pipeline Central)
Abra o R/RStudio na raiz do projeto e execute:
```r
source("run_pipeline.R")
```
Ou diretamente o orquestrador:
```r
source("etl/ETL_AUTO.R")
```

### Execução de um Módulo Específico
Cada script pode ser executado individualmente. Eles carregam automaticamente o `paths.R`:
```r
source("etl/etl_00_iniciativas_global.R")
source("etl/fiscal/etl_emendas.R")
```

## 4. Estrutura do Repositório

```text
aurora-etl/
├── .gitignore               # Protege dados, tokens e temporários
├── README.md                # Documentação técnica
├── run_pipeline.R           # Ponto de entrada principal
│
├── R/                       # Utilitários compartilhados do ETL
│   ├── paths.R              # Resolução dinâmica de caminhos (Win/Linux)
│   ├── utils.R              # Funções utilitárias auxiliares
│   └── esquemas_de_dados.R  # Esquemas e dicionários estruturados
│
└── etl/                     # Scripts do Pipeline de Dados
    ├── etl_00_iniciativas_global.R
    ├── etl_01_iniciativas_compesa.R
    ├── ...
    ├── etl_05_iniciativas_join.R
    ├── ETL_AUTO.R           # Orquestrador sequencial
    ├── fiscal/              # Pipelines do domínio orçamentário e fiscal
    ├── municipios/          # Pipelines demográficos, IBGE e territoriais
    └── ...                  # Demais domínios setoriais
```

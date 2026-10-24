# bibliotecas ----
library(BigDataPE)
library(bslib)
library(googledrive)
library(glue)
library(reactable)
library(readxl)
library(sf)
library(tidyverse)
library(vialactea)

# setup ----
source('scripts/utils.R')

valid_status <- c(
  "Em formulação", "Em elaboração de projeto", "Concluída", "A licitar",
  "Em licitação", "A iniciar", "Em execução", "Paralisada", "Cancelada"
)

valid_eixo <- c("Conhecimento e inovação", "Desenvolvimento sustentável", "Saúde e qualidade de vida", "Segurança e cidadania" )

drive_auth(path = "data/token_eco-condition-406519-adac7b9fb8e5.json", cache = TRUE)

bdpe_store_token(base_name = "maespe_beneficiarias_municipio", token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjb2xlY2FvIjoiNjg0MDgzY2I2NTQzZjUyMmNlZmFiZTA1IiwidXN1YXJpbyI6IjY0NjM3NTQ4OTJiZDg2NGUwMzkxNjJhYSIsImlhdCI6MTc0OTE0OTU1M30.RK_LrYuwIPi7nLO808ypf3rICeC252tD-MDwCSBgMv8")

# extrair ----
source('scripts/esquemas de dados.R')

shared_iniciativas <- read_rds("data/shared_iniciativas.rds")

# transformar ----
## reactable ----
library(reactable)
tbl_base <- shared_iniciativas %>% 
  # mutate(
  #   detalhes = map(
  #   id_produto, ~ as.character(
  #     tags$div(
  #       class = "mlk-details-btn-list",
  #       tags$button(
  #         class = "mlk-details-btn ",
  #         onclick = paste0("mlkRowButtonDetails('", .x, "', '", "main-detalhes_mapa", "');"),
  #         tooltip(
  #           fontawesome::fa("map", a11y = "sem", title = "Teste"),
  #           "Abrir mapa do trecho",
  #           options = list(trigger = "hover")
  #         )
  #       )
  #     )
  #   )
  # ), .after = ordem) %>% unnest(detalhes) %>% 
  mutate(status_html = case_when(
    status == "Em formulação" ~ '<span class="mlk-hex-status-ef"></span>',
    status == "Em elaboração de projeto" ~ '<span class="mlk-hex-status-ep"></span>',
    status == "A licitar" ~ '<span class="mlk-hex-status-al"></span>',
    status == "Em licitação" ~ '<span class="mlk-hex-status-el"></span>',
    status == "A iniciar" ~ '<span class="mlk-hex-status-ai"></span>',
    status == "Em execução" ~ '<span class="mlk-hex-status-ee"></span>',
    status == "Concluída" ~ '<span class="mlk-hex-status-c"></span>',
    status == "Não informado" ~ '<span class="mlk-hex-status-ni"></span>',
    status == "Cancelada" ~ '<span class="mlk-hex-status-cl"></span>',
    status == "Paralisada" ~ '<span class="mlk-hex-status-p"></span>',
    .default = as.character(vialactea::status_badge(width = "1rem", height = "1rem", color = "#ecececF0", style = ""))
  )) %>%
  mutate(eixo_html = case_when(
    eixo_estrategico == "Conhecimento e inovação" ~ '<span class="mlk-hex-icon-ci"></span>',
    # leaf (sustentável)
    eixo_estrategico == "Desenvolvimento sustentável" ~ '<span class="mlk-hex-icon-ds"></span>',
    # heart (saúde e qualidade de vida)
    eixo_estrategico == "Saúde e qualidade de vida" ~ '<span class="mlk-hex-icon-sqv"></span>',
    # shield-check (segurança e cidadania)
    eixo_estrategico == "Segurança e cidadania" ~ '<span class="mlk-hex-icon-sc"></span>'
  )) %>%
  rowwise() %>% 
  mutate(
    eixo_html = as.character(tooltip(HTML(eixo_html), eixo_estrategico, options = list(html = TRUE, trigger = "hover"))),
    status_html = as.character(tooltip(HTML(status_html), status, options = list(html = TRUE, trigger = "hover")))
  ) %>% 
  ungroup() %>%
  select(
    -any_of(
      c(
        "eixo_icon", 
        "status_icon",
        'endereco',
        'construtora',
        'obras_incidentes',
        'proponentes',
        'entidades',
        'uh',
        'tipologia_das_uh',
        'status_da_contratacao',
        'valor_de_contrapartida_do_estado_far_fds_e_retomadas_valor_contratual_aditivado_reforma_no_lar_i',
        'valor_avaliado_do_terreno_doado',
        'comunidade',
        'shapefile',
        'area_total_m2',
        'status_de_ocupacao_pcx',
        'situacao_de_transferencia_de_propriedade_pc',
        'situacao_de_transferencia_de_propriedade_pcx',
        'tipo_de_uh_producao_ou_melhoria',
        'cadastrados',
        'a_iniciar',
        'em_andamento',
        'concluido',
        'doacao_de_terreno',
        'obras_nao_incidentes',
        'processo_seletivo',
        'financiamento'
      )
    )
  )

tbl <- tbl_base %>% 
  relocate(fonte, .after = secretaria) %>% 
  reactable(
    language = reactable::reactableLang(
      searchPlaceholder = "Pesquisar...",
      noData = "Nenhum item encontrado.",
      pageSizeOptions = "Mostrar {rows}",
      pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
      pagePrevious = "\u276e",
      pageNext = "\u276f",
      pagePreviousLabel = "Pág. anterior",
      pageNextLabel = "Pág. seguinte"
    ),
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      # searchInputStyle = list(width = "100%"),
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    ),
    wrap = TRUE,
    fullWidth = TRUE,
    striped = TRUE,
    filterable = TRUE,
    searchable = FALSE,
    outlined = FALSE,
    bordered = FALSE,
    highlight = TRUE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 20, 50, 100),
    # height = 600,
    showSortable = TRUE,
    defaultSorted = "ordem",
    defaultSortOrder = "asc",
    defaultColDef = colDef(
      html = TRUE,
      footerStyle = list(
        fontWeight = "bold",
        fontSize = ".8rem",
        `border-top` = "2px solid"
      ),
      headerStyle = list(
        display = "flex",
        fontSize = ".8rem",
        justifyContent = "center",
        alignItens = "center"
      ),
      headerClass = "header-tbl",
      align = "center",
      headerVAlign = "bottom",
      class = "cell",
      style = list(fontSize = ".8rem"),
    ),
    columnGroups = list(),
    columns = list(
      # detalhes = colDef(
      #   html = TRUE,
      #   sticky = "left",
      #   filterable = FALSE,
      #   sortable = FALSE,
      #   minWidth = 80,
      #   align = "center",
      #   name = "Info",
      #   header = function(value) {
      #     tooltip(value, "Outras informações sobre o produto/iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
      #       as.character()
      #   }
      # ),
      avanco_cor = colDef(show = FALSE),
      fator_de_divisao_municipio = colDef(show = FALSE),
      id_produto = colDef(show = FALSE),
      ordem = colDef(
        name = "N",
        align = "right",
        width = 50,
        show = FALSE,
        header = function(value) {
          tooltip(value, "Ordem da iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        }
      ),
      secretaria = colDef(
        name = "Órgão",
        sticky = "left",
        align = "center",
        width = 90,
        filterMethod = JS("function(rows, columnId, filterValue) {
            const pattern = new RegExp(filterValue, 'i')
            return rows.filter(function(row) {
              return pattern.test(row.values[columnId])
            })
          }"),
        header = function(value) {
          tooltip(value, "Secretaria responsável para iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        cell = function(value) {
          rst <- vialactea::tagBadge(value, NULL, status = "bg-black", style = "color:white")
          
          tooltipContent <- paste0('<span style="text-align:left"> ', secretarias$nome[secretarias$sigla == value], '</span>')
          
          # Retorna o tooltip com o conteúdo e o ícone, conforme definido
          return(paste0(
            '<bslib-tooltip placement="auto" bsOptions="{&quot;html&quot;:true}" data-require-bs-version="5" data-require-bs-caller="tooltip()">',
            '<template>', tooltipContent, '</template>',
            rst,
            '</bslib-tooltip>'
          ))
        }
      ),
      fonte = colDef(
        name = "Dados",
        align = "center",
        width = 90,
        filterMethod = JS("function(rows, columnId, filterValue) {
            const pattern = new RegExp(filterValue, 'i')
            return rows.filter(function(row) {
              return pattern.test(row.values[columnId])
            })
          }"),
        header = function(value) {
          tooltip(value, "Secretaria responsável para iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        cell = function(value) {
          rst <- vialactea::tagBadge(value, NULL, status = "bg-secondary", style = "color:black")
          
          tooltipContent <- paste0('<span style="text-align:left"> ', secretarias$nome[secretarias$sigla == value], '</span>')
          
          # Retorna o tooltip com o conteúdo e o ícone, conforme definido
          return(paste0(
            '<bslib-tooltip placement="auto" bsOptions="{&quot;html&quot;:true}" data-require-bs-version="5" data-require-bs-caller="tooltip()">',
            '<template>', tooltipContent, '</template>',
            rst,
            '</bslib-tooltip>'
          ))
        }
      ),
      eixo_estrategico = colDef(
        name = "Eixo",
        filterMethod = JS("function(rows, columnId, filterValue) {
            const pattern = new RegExp(filterValue, 'i')
            return rows.filter(function(row) {
              return pattern.test(row.values[columnId])
            })
          }"),
        header = function(value) {
          tooltip(value, HTML("Eixo estratégico da iniciativa"), options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "center",
        html = TRUE,
        width = 80,
        #cell = function(value, index) tbl_base$eixo_html[index]
        cell = JS('
                  function(cellInfo) {
                    return `${cellInfo.row["eixo_html"]}`
                  }
                ')
      ),
      status_html = colDef(show = FALSE),
      eixo_html = colDef(show = FALSE),
      status = colDef(
        name = "Status",
        filterMethod = JS("function(rows, columnId, filterValue) {
            const pattern = new RegExp(filterValue, 'i')
            return rows.filter(function(row) {
              return pattern.test(row.values[columnId])
            })
          }"),
        header = function(value) {
          tooltip(value, HTML("Status da iniciativa"), options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "center",
        html = TRUE,
        width = 80,
        cell = JS('
                  function(cellInfo) {
                    return `${cellInfo.row["status_html"]}`
                  }
                ')
      ),
      data_de_inicio = colDef(
        name = "Início",
        header = function(value) {
          tooltip(value, "Data de início da iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "center", show = FALSE,
        width = 125,
        cell = function(value) if_else(is.na(value), "–", paste0(
          lubridate::month(value, abbr = TRUE, label = TRUE, locale = "pt_BR.UTF-8"), "/", year(value)))
        # width = 60
      ),
      data_de_termino = colDef(
        name = "Término",
        align = "center",
        width = 125,
        header = function(value) {
          tooltip(value, "Data de término da iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        cell = function(value) if_else(is.na(value), "–", paste0(
          lubridate::month(value, abbr = TRUE, label = TRUE, locale = "pt_BR.UTF-8"), "/", year(value)))
        # width = 60
      ),
      percent_execucao = colDef(
        name = "Execução",
        header = function(value) {
          tooltip(value, "Execução da iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        cell = JS("renderAvancoFisico"),
        filterMethod = JS("
                    function(rows, columnId, filterValue) {
                      return rows.filter(
                        function(row) {
                          return row.values[columnId] >= filterValue;
                        });
                    }"),
        html = TRUE,
        align = "center",
        width = 90,
        class = "avanco-financeiro"
      ),
      produto = colDef(
        name = "Produto",
        header = function(value) {
          tooltip(value, "Descrição do produto da iniciativa estratégica", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "left",
        minWidth = 200,
        cell = function(value) if_else(is.na(value), "", value)
      ),
      iniciativa_estrategica = colDef(
        name = "Iniciativa",
        header = function(value) {
          tooltip(value, "Descrição da iniciativa estratégica", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "left",
        minWidth = 200,
        cell = function(value) if_else(is.na(value), "", value)
      ),
      valor_total = colDef(
        name = "Valor",
        align = "right",
        width = 125,
        header = function(value) {
          tooltip(value, "Valor do produto da iniciativa", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        filterMethod = JS("
                    function(rows, columnId, filterValue) {
                      return rows.filter(
                        function(row) {
                          return row.values[columnId] >= filterValue;
                        });
                    }"),
        cell = function(value) if_else(is.na(value), "", paste0("R$ ", vialactea::comprss(value))), # –
        show = TRUE
      ),
      local = colDef(
        name = "Local",
        align = "left",
        header = function(value) {
          tooltip(value, "Local do produto", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        width = 150,
        cell = function(value) if_else(is.na(value), "–", value),
        show = FALSE
      ),
      municipio = colDef(
        name = "Município",
        align = "left",
        header = function(value) {
          tooltip(value, "Município", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        width = 125,
        cell = function(value) if_else(is.na(value), "–", value),
        show = TRUE
      ),
      regiao_de_desenvolvimento = colDef(
        name = "RD",
        header = function(value) {
          tooltip(value, "Região de desenvolvimento", options = list(html = TRUE, trigger = "hover")) %>%
            as.character()
        },
        align = "left",
        # minWidth = 200,
        cell = function(value) if_else(is.na(value), "–", value),
        show = FALSE
      )
    )
  )

tabela_iniciativa <-  htmlwidgets::onRender(tbl, sprintf("
() => {
  // 0) Variável para guardar os dados atuais da tabela
  let lastSortedData = [];

  // 1) Referências aos elementos do DOM
  const switchEl   = document.getElementById('%s');
  const boxValor   = document.getElementById('%s');
  const boxCount   = document.getElementById('%s');
  const boxMun     = document.getElementById('%s');
  const boxCo      = document.getElementById('%s');

  // Verificações iniciais
  //console.log('🔀 switchEl:', switchEl);
  //console.log('📦 boxes:', boxValor, boxCount, boxMun, boxCo);

  // Helper: conta ordens distintas e soma valor*fator
  function getCountAndSum(rows) {
    const count = new Set(rows.map(r => r.ordem)).size;
    const sum = rows.reduce((acc, r) => {
      const v = Number(r.valor_total); //, f = Number(r.fator_de_divisao_municipio);
      return acc + ((!isNaN(v)) ? v : 0);
      //return acc + ((!isNaN(v) && !isNaN(f)) ? v * f : 0);
    }, 0);
    return { count, sum };
  }

  // Função única que faz todo o update de UIs
  function updateSummary(data) {
    //console.log('🔄 updateSummary chamado. data.length =', data.length);
    const mode = switchEl && switchEl.checked;
    //console.log('   • mode =', mode);

    // totais gerais
    const overall = getCountAndSum(data);
    const distinctIniciativas = new Set(data.map(r => r.iniciativa_estrategica).filter(v => v)).size;
    
    //console.log('   • overall =', overall);
    
     // Soma específica para 'cofinanciamento' ou 'convênio' em iniciativa_estrategica
    const filteredCo = data.filter(r => {
      if (!r.iniciativa_estrategica) return false;
      const str = r.iniciativa_estrategica.toLowerCase();
      return str.includes('cofinanciamento') || str.includes('convênio');
    });
    const sumCo = filteredCo.reduce((acc, r) => {
      const v = Number(r.valor_total);
      return acc + ((!isNaN(v)) ? v : 0);
    }, 0);
     
    // atualiza valor vs contagem
    if (boxValor && boxCount) {
      //if (mode) {
        //boxValor.textContent = overall.count.toLocaleString('pt-BR');
        //boxCount.textContent = 'R$ ' + compress_number(overall.sum);
      //} else {
        boxValor.textContent = 'R$ ' + compress_number(overall.sum);
        boxCount.textContent = distinctIniciativas.toLocaleString('pt-BR') + ' (' + overall.count.toLocaleString('pt-BR') + ')';
      //}
    }

    // municípios / RDs distintos
    const distinctMun = new Set(data.map(r => r.municipio).filter(v => v)).size;
    const distinctRD  = new Set(data.map(r => r.regiao_de_desenvolvimento).filter(v => v)).size;
    console.log('   • distinctMun =', distinctMun, '| distinctRD =', distinctRD);
    if (boxMun) boxMun.textContent = distinctMun + ' (' + distinctRD + ')';
    
    if (boxCo) {
      boxCo.textContent = 'R$ ' + compress_number(sumCo);
    }

    // status e eixos
    const statusCfg = [
      { status: 'Em formulação',           cls: 'mlk-hex-val-ef' },
      { status: 'Em elaboração de projeto', cls: 'mlk-hex-val-ep' },
      { status: 'A licitar',               cls: 'mlk-hex-val-al' },
      { status: 'Em licitação',            cls: 'mlk-hex-val-el' },
      { status: 'A iniciar',               cls: 'mlk-hex-val-ai' },
      { status: 'Em execução',             cls: 'mlk-hex-val-ee' },
      { status: 'Concluída',               cls: 'mlk-hex-val-c'  },
      { status: 'Não informado',           cls: 'mlk-hex-val-ni' }
    ];
    statusCfg.forEach(cfg => {
      const rows = data.filter(r => r.status === cfg.status);
      const { count, sum } = getCountAndSum(rows);
      const val = mode ? sum : count;
      const txt = mode
        ? 'R$ ' + compress_number(val, \"pt\", \"complete\", true)
        : val.toLocaleString('pt-BR');
      console.log('   • Status:', cfg.status, '→', txt);
      const el = document.querySelector('.' + cfg.cls);
      if (el) el.textContent = txt;
    });

/*
    const eixoCfg = [
      { eixo: 'Conhecimento e inovação',     cls: 'mlk-hex-val-ci' },
      { eixo: 'Desenvolvimento sustentável', cls: 'mlk-hex-val-ds' },
      { eixo: 'Saúde e qualidade de vida',   cls: 'mlk-hex-val-sqv' },
      { eixo: 'Segurança e cidadania',      cls: 'mlk-hex-val-sc' }
    ];
    eixoCfg.forEach(cfg => {
      const rows = data.filter(r => r.eixo_estrategico === cfg.eixo);
      const { count, sum } = getCountAndSum(rows);
      const val = mode ? sum : count;
      const txt = mode
        ? 'R$ ' + compress_number(val)
        : val.toLocaleString('pt-BR');
      console.log('   • Eixo:', cfg.eixo, '→', txt);
      const el = document.querySelector('.' + cfg.cls);
      if (el) el.textContent = txt;
    }); */
  }


  // 2) Quando o switch muda, chama updateSummary com o último sortedData
  if (switchEl) {
    switchEl.addEventListener('change', () => {
      console.log('⏹ switch mudou, chamando updateSummary()');
      updateSummary(lastSortedData);
    });
  }

  
  
  
  // 3) Sempre que a tabela muda (filtro/sort), atualiza lastSortedData e chama updateSummary
  Reactable.onStateChange('%s', state => {
    lastSortedData = state.sortedData;
    console.log('📈 Reactable.onStateChange – dados atualizados');
    updateSummary(lastSortedData);
    updateButtonClass();
  });
}
",
## placeholders:
"main-type_switch",       # switchEl
"main-box_valor",         # boxValor
"main-box_iniciativas",   # boxCount
"main-box_municipios",    # boxMun
"main-box_cofinanciamentos", # boxCo
"main-tabela"             # onStateChange
))

# salvar ----
write_rds(tabela_iniciativa, "app_aurora/data/tabela_iniciativa.rds")

write_rds(tbl_base, "app_aurora/data/tbl_base_2.rds")
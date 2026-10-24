# bibliotecas ----
library(tidyverse)
library(reactable)
library(htmltools)
library(vialactea)

# extrair ----
shared_iniciativas <- read_rds("data/shared_iniciativas.rds")

# transformar ----

# 1. Preparação dos dados
# Mantivemos as colunas para o select do Excel, mas criando classes para o visual
tbl_base <- shared_iniciativas %>% 
  mutate(
    status_class = case_when(
      status == "Em formulação" ~ "ef",
      status == "Em elaboração de projeto" ~ "ep",
      status == "A licitar" ~ "al",
      status == "Em licitação" ~ "el",
      status == "A iniciar" ~ "ai",
      status == "Em execução" ~ "ee",
      status == "Concluída" ~ "c",
      .default = "ni"
    ),
    eixo_class = case_when(
      eixo_estrategico == "Conhecimento e inovação" ~ "ci",
      eixo_estrategico == "Desenvolvimento sustentável" ~ "ds",
      eixo_estrategico == "Saúde e qualidade de vida" ~ "sqv",
      eixo_estrategico == "Segurança e cidadania" ~ "sc",
      .default = "default"
    )
  ) %>%
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

# 2. Definição da Reactable com SEU estilo original
tbl <- tbl_base %>% 
  relocate(fonte, .after = secretaria) %>% 
  reactable(
    language = reactable::reactableLang(
      searchPlaceholder = "Pesquisar...",
      noData = "Nenhum item encontrado.",
      pageSizeOptions = "Mostrar {rows}",
      pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
      pagePrevious = "\u276e", pageNext = "\u276f"
    ),
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px"
    ),
    wrap = TRUE, fullWidth = TRUE, striped = TRUE, filterable = TRUE,
    highlight = TRUE, compact = TRUE, showSortable = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 20, 50, 100),
    defaultSorted = "ordem",
    defaultColDef = colDef(
      html = TRUE,
      headerStyle = list(display = "flex", fontSize = ".8rem", justifyContent = "center"),
      headerClass = "header-tbl",
      align = "center",
      style = list(fontSize = ".8rem")
    ),
    columns = list(
      # Colunas Ocultas (Mantendo as que você tinha)
      status_class = colDef(show = FALSE),
      eixo_class = colDef(show = FALSE),
      avanco_cor = colDef(show = FALSE),
      fator_de_divisao_municipio = colDef(show = FALSE),
      id_produto = colDef(show = FALSE),
      ordem = colDef(show = FALSE),
      local = colDef(show = FALSE),
      regiao_de_desenvolvimento = colDef(show = FALSE),
      
      # Órgão (Secretaria) com seu estilo de Badge Preto
      secretaria = colDef(
        name = "Órgão", sticky = "left", width = 90,
        header = function(value) as.character(tooltip(value, "Secretaria responsável")),
        cell = function(value) {
          as.character(vialactea::tagBadge(value, NULL, status = "bg-black", style = "color:white"))
        }
      ),
      
      # secretaria_unidade = colDef(
      #   name = "Unidade", sticky = "left", width = 90,
      #   header = function(value) as.character(tooltip(value, "Unidade responsável")),
      #   cell = function(value) {
      #     as.character(vialactea::tagBadge(value, NULL, status = "bg-black", style = "color:white"))
      #   }
      # ),
      
      # Fonte de Dados com Badge Cinza
      fonte = colDef(
        name = "Dados", width = 90,
        cell = function(value) {
          as.character(vialactea::tagBadge(value, NULL, status = "bg-secondary", style = "color:black"))
        }
      ),
      
      # Status e Eixo (Otimizados com JS mas mantendo nomes)
      status = colDef(
        name = "Status", width = 80,
        header = function(value) as.character(tooltip(value, "Status da iniciativa")),
        cell = JS("function(cellInfo) {
          return `<span class='mlk-hex-status-${cellInfo.row.status_class}' title='${cellInfo.value}'></span>`
        }")
      ),
      
      eixo_estrategico = colDef(
        name = "Eixo", width = 80,
        header = function(value) as.character(tooltip(value, "Eixo estratégico")),
        cell = JS("function(cellInfo) {
          return `<span class='mlk-hex-icon-${cellInfo.row.eixo_class}' title='${cellInfo.value}'></span>`
        }")
      ),
      
      # Avanço Físico (Mantendo sua função JS customizada)
      percent_execucao = colDef(
        name = "Execução", width = 90,
        cell = JS("renderAvancoFisico"),
        class = "avanco-financeiro"
      ),
      
      # Produto e Iniciativa (Texto alinhado à esquerda)
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
      
      iniciativa_estrategica = colDef(name = "Iniciativa", align = "left", minWidth = 200),
      
      # Valor Total
      valor_total = colDef(
        name = "Valor", align = "right", width = 125,
        cell = function(value) if(is.na(value)) "" else paste0("R$ ", vialactea::comprss(value))
      ),
      
      # Município
      municipio = colDef(name = "Município", align = "left", width = 125),
      
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
      )
    )
  )

# 3. Injeção da lógica de sincronização (Preservando IDs do seu Módulo)
tabela_iniciativa <- htmlwidgets::onRender(tbl, sprintf("
function(el, x) {
  const switchEl = document.getElementById('main-type_switch');
  const boxes = {
    valor: document.getElementById('main-box_valor'),
    count: document.getElementById('main-box_iniciativas'),
    mun: document.getElementById('main-box_municipios'),
    co: document.getElementById('main-box_cofinanciamentos')
  };

  function updateSummary(data) {
    const isCurrency = switchEl && switchEl.checked;
    
    const totalVal = data.reduce((acc, r) => acc + (Number(r.valor_total) || 0), 0);
    const distIni = new Set(data.map(r => r.iniciativa_estrategica)).size;
    const distMun = new Set(data.map(r => r.municipio)).size;
    const distRD  = new Set(data.map(r => r.regiao_de_desenvolvimento)).size;

    if (boxes.valor) boxes.valor.textContent = 'R$ ' + compress_number(totalVal);
    if (boxes.count) boxes.count.textContent = distIni + ' (' + data.length + ')';
    if (boxes.mun)   boxes.mun.textContent   = distMun + ' (' + distRD + ')';
    
    // Sincronização dos hexágonos superiores (EF, EP, AL...)
    ['ef', 'ep', 'al', 'el', 'ai', 'ee', 'c', 'ni'].forEach(cls => {
      const filtered = data.filter(r => r.status_class === cls);
      const val = isCurrency 
        ? 'R$ ' + compress_number(filtered.reduce((acc, r) => acc + (Number(r.valor_total) || 0), 0))
        : filtered.length.toLocaleString('pt-BR');
      const elHex = document.querySelector('.mlk-hex-val-' + cls);
      if (elHex) elHex.textContent = val;
    });
  }

  Reactable.onStateChange('main-tabela', state => {
    updateSummary(state.sortedData);
  });
}
"))

# salvar
write_rds(tabela_iniciativa, "app_aurora/data/tabela_iniciativa.rds")
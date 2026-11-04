library(tidyverse)
library(furrr)
library(sf)
library(pool)
library(DBI)
library(vialactea)
library(RPostgres)
library(bslib)
library(arrow)
library(geojsonsf)

# Carregamento centralizado de caminhos
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../../R/paths.R")) source("../../R/paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

palheta <- MetBrewer::met.brewer("Peru1", n=100, type = "continuous")

contratos_plug <- read_rds(file.path(DIR_RAW_MULUNGU, "contratos_plug.rds"))
financeiro_plug <- read_rds(file.path(DIR_RAW_MULUNGU, "financeiro_plug.rds"))
dicionario_sre <- read_rds(file.path(DIR_RAW_MULUNGU, "dicionario_sre.rds")) 
obras_notion <- read_rds(file.path(DIR_RAW_MULUNGU, "obras_notion.rds"))

contratos_plug_enriquecidos <- contratos_plug %>% 
  left_join(
    financeiro_plug,
    by = join_by(contrato_plug)
  ) #%>% glimpse()

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "sepe",
  host = "10.238.107.182",
  port = "5432",
  user = "sepe_adm",
  password = "!sepe_adm!",
  minSize = 4,
  idleTimeout = 60 * 60
)

pernambuco_rodovias_sre_2025 <- read_rds(file.path(DIR_RAW_MULUNGU, "rodovias.rds"))

pernambuco_regioes_administrativas <- tbl(pool, "pernambuco_regioes_administrativas") %>%
  collect() %>%
  mutate(geometry = st_as_sfc(geometry)) %>%
  st_as_sf()

diariodeobras_relatorios <- tbl(pool, "diariodeobras_relatorios") %>%
  collect()

pernambuco_rodovias_2025 <- pernambuco_rodovias_sre_2025 %>% 
  group_by(nome) %>% 
  arrange(km_ini) %>% 
  summarise(inicio = first(inicio, order_by = km_ini), 
            fim = first(fim, order_by = km_ini), 
            extensao = sum(ext, na.rm = TRUE),
            sre = paste0(sre, collapse = ", "),
            geometry = st_union(geometry),
            .groups = "drop"
            ) %>% 
  mutate(nome = str_remove(nome, "^E")) %>% 
    mutate(
      data = map(geometry,
                 function(x){
                   pernambuco_regioes_administrativas %>%
                     st_filter(x) %>%
                     st_drop_geometry() %>%
                     select(Município, ibge7, DRO, `Região de Desenvolvimento`) %>%
                     summarise(
                       municipios = paste0(sort(na.omit(unique(Município))), collapse = ", "),
                       dro = paste0(sort(na.omit(unique(DRO))), collapse = ", "),
                       regiao_desenvolvimento = paste0(sort(na.omit(unique(`Região de Desenvolvimento`))), collapse = ", ")
                     )
                 })) %>%
    unnest(data)

DBI::dbWriteTable(
  conn = pool,
  name = "pernambuco_rodovias_2025",
  value = pernambuco_rodovias_2025,
  overwrite = TRUE
)

poolClose(pool)

obras_notion_plug <- obras_notion %>% 
  select(-tipo) %>% 
  mutate(order = row_number()) %>% 
  left_join(
    contratos_plug_enriquecidos %>% 
      mutate(valor_aditado = replace_na(valor_aditado, 0)) %>% 
      mutate(valor_inicial = replace_na(valor_inicial, 0)) %>% 
      mutate(valor_atual_obra = valor_inicial + valor_aditado) %>% 
      mutate(valor_atual_obra = replace_na(valor_atual_obra, 0)) %>% 
      mutate(valor_vigente_obra = valor_atual_obra + valor_reajuste) %>% 
      mutate(avanco_financeiro = valor_medido/valor_atual_obra) %>% 
      select(contrato_plug, contratada, data_ordem_de_servico, 
             valor_vigente_obra,
             valor_atual_obra,
             avanco_fisico_plug,
             avanco_financeiro,
             extensao_contrato, 
             tipo = contrato_tipo_plug,
             acao_subacao, fonte_de_recurso_sintetica_codigo, codigos_sre),
    by = c("contrato_obra_plug" = "contrato_plug")
  ) %>% 
  left_join(
    contratos_plug_enriquecidos %>% 
      mutate(valor_aditado = replace_na(valor_aditado, 0)) %>% 
      mutate(valor_inicial = replace_na(valor_inicial, 0)) %>% 
      mutate(valor_atual_supervisao = valor_inicial + valor_aditado) %>% 
      mutate(valor_atual_supervisao = replace_na(valor_atual_supervisao, 0)) %>%
      mutate(valor_vigente_supervisao = valor_atual_supervisao + valor_reajuste) %>% 
      select(contrato_plug, valor_vigente_supervisao, valor_atual_supervisao),
    by = c("contrato_supervisao_plug" = "contrato_plug")
  ) %>% 
  mutate(valor_vigente_obra = replace_na(valor_vigente_obra, 0)) %>% 
  mutate(valor_vigente_supervisao = replace_na(valor_vigente_supervisao, 0)) %>%
  mutate(valor_atual_obra = replace_na(valor_atual_obra, 0)) %>% 
  mutate(valor_atual_supervisao = replace_na(valor_atual_supervisao, 0)) %>%
  mutate(valor_atual = valor_atual_supervisao + valor_atual_obra) %>% 
  mutate(valor_vigente = valor_vigente_obra + valor_vigente_supervisao) %>% 
  select(
    -valor_vigente_obra,
    -valor_vigente_supervisao,
    -valor_atual_supervisao,
    -valor_atual_obra,
    observacoes,
    ritmo
    ) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-014", "014EPE0010", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-073", "073EPE0010", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-158", "158EPE0070, 158EPE0090", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-250", "250EPE0030", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-460", "460EPE0070", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "PE-613", "613EPE0010", codigos_sre)) %>% 
  mutate(codigos_sre = if_else(rodovia == "BR-232" & is.na(codigos_sre), "232APE0417", codigos_sre)) %>% 
  relocate(avanco_financeiro, .after = avanco_fisico) %>% 
  relocate(avanco_fisico_plug, .after = avanco_fisico)

notion_extra <- obras_notion_plug %>% 
  select(order, contrato_obra_plug, rodovia, codigos_sre) %>% 
  mutate(codigos_sre = str_split(codigos_sre, ",")) %>% 
  unnest(codigos_sre) %>% 
  mutate(codigos_sre = str_trim(codigos_sre)) %>% 
  filter(!is.na(codigos_sre)) %>% 
  left_join(dicionario_sre %>% st_drop_geometry(),
            by = c("codigos_sre" = "sre")) %>% 
  #filter(is.na(local_inicio))
  group_by(order, rodovia) %>% 
  arrange(nome, km_ini) %>% 
  summarise(
    extensao_sre = sum(ext, na.rm = TRUE),
    trecho = paste0(
      unique(first(inicio, na_rm = TRUE, km_ini)),
      " ⟷ ", #↔ ⟺
      unique(last(fim, na_rm = TRUE, km_ini))
      ),
    km_inicio = first(km_ini, na_rm = TRUE),
    municipios = paste0(na.omit(unique(sort(str_split(paste0(na.omit(mun), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    dro = paste0(na.omit(unique(sort(str_split(paste0(na.omit(dro), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    regiao_desenvolvimento = paste0(na.omit(unique(sort(str_split(paste0(na.omit(rd), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    .groups =  "drop"
  ) %>%  
  mutate(extensao_sre = if_else(extensao_sre <= 0, NA_real_, extensao_sre)) %>% 
  mutate(trecho = if_else(is.na(extensao_sre), "⚠️ SRE incorreto, corrigir no Plug", trecho))

resvestimento_badges_list_cards <- list(CBUQ = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#9b59b6;stroke-width:1.75;stroke:#9b59b6;fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CBUQ</text>
</svg>') |> tooltip("Concreto betuminoso usinado a quente"),
                                  CONCRETO = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#2980b9;stroke-width:1.75;stroke:#2980b9; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CCP</text>
</svg>') |> tooltip("Concreto de cimento Portland"),
                                  CCP = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#2980b9;stroke-width:1.75;stroke:#2980b9; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CCP</text>
</svg>') |> tooltip("Concreto de cimento Portland"),
                                  PLPI = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#16a085;stroke-width:1.75;stroke:#16a085; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">PLPI</text>
</svg>') |> tooltip("Paralelepípedo"),
                                  RP = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#f39c12;stroke-width:1.75;stroke:#f39c12; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">RP</text>
</svg>') |> tooltip("Revestimento primário"),
                                  TERRA = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#d35400;stroke-width:1.75;stroke:#d35400; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TERRA</text>
</svg>') |> tooltip("Sem revestimento"),
                                  TSD = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#27ae60;stroke-width:1.75;stroke:#27ae60; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TSD</text>
</svg>') |> tooltip("Tratamento superficial duplo"),
                                  TSS = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#7f8c8d;stroke-width:1.75;stroke:#7f8c8d; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TSS</text>
</svg>') |> tooltip("Tratamento superficial simples"),
                                  MRAF = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#8c7f8d;stroke-width:1.75;stroke:#8c7f8d; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">MRAF</text>
</svg>') |> tooltip("Microrevestimento Asfáltico Frio ")
)


resvestimento_badges_list_no_tooltip <- list(CBUQ = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#9b59b6;stroke-width:1.75;stroke:#9b59b6;fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CBUQ</text>
</svg>'),
                                             CONCRETO = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#2980b9;stroke-width:1.75;stroke:#2980b9; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CCP</text>
</svg>'),
                                             CCP = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#2980b9;stroke-width:1.75;stroke:#2980b9; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">CCP</text>
</svg>'),
                                             PLPI = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#16a085;stroke-width:1.75;stroke:#16a085; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">PLPI</text>
</svg>'),
                                             RP = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#f39c12;stroke-width:1.75;stroke:#f39c12; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">RP</text>
</svg>'),
                                             TERRA = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#d35400;stroke-width:1.75;stroke:#d35400; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TERRA</text>
</svg>'),
                                             TSD = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#27ae60;stroke-width:1.75;stroke:#27ae60; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TSD</text>
</svg>'),
                                             TSS = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#7f8c8d;stroke-width:1.75;stroke:#7f8c8d; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">TSS</text>
</svg>'),
                                             MRAF = HTML('<svg style="width:40px;height:auto;font-size:12px;font-family:inherit;margin:0px 0 0px 0;" xmlns="http://www.w3.org/2000/svg" class="pikchr"  viewBox="9.2248 1.78 49 31" data-pikchr-date="20250321215505">
<path d="M11.2248,17.28 L 11.2248,11.28 Q 11.2248,8.28 14.1665,7.69165 L 30.7831,4.36835 Q 33.7248,3.78 36.6665,4.36835 L 53.2831,7.69165 Q 56.2248,8.28 56.2248,11.28 L 56.2248,23.28 Q 56.2248,26.28 53.2831,26.8683 L 36.6665,30.1917 Q 33.7248,30.78 30.7831,30.1917 L 14.1665,26.8683 Q 11.2248,26.28 11.2248,24.03 Q 11.2248,21.78 11.2248,19.53 L 11.2248,17.28Z" style="fill:#8c7f8d;stroke-width:1.75;stroke:#8c7f8d; fill-opacity: .5;" />
<text lengthAdjust="spacingAndGlyphs"  x="33.5" y="17.28" text-anchor="middle" font-weight="bold" fill="rgb(0,0,0)" dominant-baseline="central">MRAF</text>
</svg>')
)


obras_notion_enriquecido <- obras_notion_plug %>% 
  filter(!is.na(codigos_sre)) %>% 
  left_join(notion_extra, by = join_by(rodovia, order)) %>% 
  select(-order) %>% 
  mutate(contrato_diario = str_remove(contrato_obra_plug, "^0*")) %>% 
  left_join(
    diariodeobras_relatorios %>% 
      mutate(contrato = str_remove(contrato, "^0*")) %>% 
      filter(!is.na(contrato)) %>% 
      group_by(contrato) %>% 
      summarise(
        link_pdf = last(link_pdf, order_by = reports_numero, na_rm = TRUE),
        link_logo = last(link_logo, order_by = reports_numero, na_rm = TRUE),
        .groups = "drop"),
    by = c("contrato_diario" = "contrato")
  ) %>% 
  select(-contrato_diario) %>% 
  mutate(rodovia = str_remove_all(rodovia,  "-")) %>% 
  mutate(avanco_fisico_notion = as.numeric(avanco_fisico)) %>% 
  mutate(avanco_fisico = as.numeric(avanco_fisico)) %>% 
  #mutate(avanco_fisico = avanco_fisico_plug) %>% 
  
  mutate(
  avanco_fisico = case_when(
    is.na(avanco_fisico) ~ NA_real_,
    avanco_fisico > 1  ~ avanco_fisico / 100,
    avanco_fisico <= 1   ~ avanco_fisico,
    TRUE ~ avanco_fisico
  )
  )%>%
  
  arrange(rodovia) %>% 
  mutate(ordem = row_number()) %>% 
  mutate(revestimento = if_else(revestimento == "CONCRETO", "CCP", revestimento))

tabela_pe_na_estrada <- obras_notion_enriquecido %>% 
  select(
    ordem,
    rodovia, 
    avanco_fisico,
    avanco_fisico_plug,
    avanco_financeiro,
    status,
    inauguracao,
    tipo,
    revestimento,
    link_pdf,
    trecho, 
    extensao_contrato, 
    contratada,
    inicio_execucao, 
    termino_execucao,
    valor_vigente,
    municipios,
    regiao_desenvolvimento,
    acao_subacao,
    fonte_de_recurso_sintetica_codigo,
    observacoes
  ) %>% 
  mutate(rodovia = str_remove_all(rodovia, "\\-")) %>% 
  mutate(avanco_fisico = as.numeric(avanco_fisico)) %>% 
  mutate(avanco_cor = vialactea::get_score_color(avanco_fisico)) %>% 
  mutate(avanco_fisico = round(100*avanco_fisico)) %>% 
  mutate(avanco_plug_cor = vialactea::get_score_color(avanco_fisico_plug, low_color = "#d3b6ab", high_color = "#4d3328")) %>% 
  mutate(avanco_fisico_plug = round(100*avanco_fisico_plug)) %>% 
  mutate(avanco_financeiro_cor = vialactea::get_score_color(avanco_financeiro, low_color = "#ffd90d", high_color = "#df6c02")) %>% 
  mutate(avanco_financeiro = round(100*avanco_financeiro)) %>% 
  mutate(reais_por_km = valor_vigente/extensao_contrato) %>% 
  mutate(inicio_execucao = dmy(paste("01", month(inicio_execucao), year(inicio_execucao), sep = "/"), quiet = TRUE)) %>% 
  mutate(termino_execucao = dmy(paste("01",month(termino_execucao), year(termino_execucao), sep = "/"), quiet = TRUE))

tabela_timeline <- tabela_pe_na_estrada %>% mutate(status_cor = case_when(
  status == "Obra a contratar"  ~ "#1982c4F0", #"#ffca3aF0",
  status == "Concluída"  ~ "#8ac926F0", #"#8ac926F0",
  status == "Em andamento"  ~ "#ffca3aF0", #"#1982c4F0",
  status == "Paralisada" ~ "#ff595eF0", #"#ff595eF0",
  .default = "#ecececf0"
)) %>%
  mutate(palheta_cor = if_else(is.na(avanco_fisico), "#C0C0C0", palheta[1 + .99*round(avanco_fisico)])) %>% 
  mutate(avanco_fisico = if_else(is.na(avanco_fisico), "S/I", paste0(avanco_fisico, "%"))) %>% 
  arrange(rodovia) %>% 
  mutate(avanco_label = case_when(
    inauguracao == "A inaugurar" ~ paste0(avanco_fisico, " 🏳️ "),
    inauguracao == "Inaugurada" ~ paste0(avanco_fisico, " 🏁 "),
    inauguracao == "Finalizada" ~ paste0(avanco_fisico, " 🏴 "),
    inauguracao == "Inaugurada Parcialmente" ~ paste0(avanco_fisico, " ½🏁 "),
    .default = avanco_fisico
  )) %>% 
 rowwise() %>% 
  mutate(badge_execucao = badge_svg("Execução", if_else(is.na(avanco_fisico), "S/I", avanco_fisico), palheta_cor, font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(
    badge_update = withr::with_locale(
      c(LC_TIME = "pt_BR.UTF-8"),
      paste0(
        "Atualizado em ",
        format(now(), "%d de %B de %Y às %H:%M:%S"),
        "."
      )
    )
  ) %>%
  mutate(badge_valor = badge_svg("Valor", if_else(is.na(valor_vigente), "S/I", vialactea::comprss(valor_vigente)), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_extensao = badge_svg("KM", if_else(is.na(extensao_contrato) | extensao_contrato == 0, "S/I", format(extensao_contrato,decimal.mark = ",")), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_status = badge_svg("Status", if_else(is.na(status), "S/I", status), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(
    inicio = if_else(
      is.na(inicio_execucao),
      "S/I",
      withr::with_locale(
        c(LC_TIME = "pt_BR.UTF-8"),
        format(inicio_execucao, "%b/%Y")
      )
    ),
    termino = if_else(
      is.na(termino_execucao),
      "S/I",
      withr::with_locale(
        c(LC_TIME = "pt_BR.UTF-8"),
        format(termino_execucao, "%b/%Y")
      )
    )
  ) %>%
  mutate(label = make_popup(ordem = ordem, 
                            rodovia = HTML(str_trim(str_remove_all(rodovia, "\\s*\\(.*\\)"))),
                            inauguracao = inauguracao,
                            execucao = avanco_fisico,
                            trecho = trecho,
                            status = status,
                            contratada = contratada,
                            municipio = municipios,
                            regiao_desenvolvimento = regiao_desenvolvimento,
                            inicio = HTML(inicio),
                            fim = HTML(termino),
                            subacao = acao_subacao, ####
                            palheta_cor = palheta_cor, ###
                            cor = status_cor,####
                            badge_execucao = badge_execucao, ####
                            badge_valor = badge_valor, ####
                            badge_status = badge_status,
                            badge_update = badge_update,
                            badge_extensao = badge_extensao, 
                            revestimento = resvestimento_badges_list_cards[[revestimento]]) 
  ) %>% 
  mutate(badge_tabela = badge_svg(label = rodovia, 
                                  avanco_label, 
                                  status_cor, 
                                  font = "Jost,sans-serif", 
                                  fontsize = 12,
                                  height = 12
  ) %>% popover(
    label,
    title = character(),
    options = list(customClass = "mlk-popover-timeline")
  ) %>% as.character()
  ) %>% 
  ungroup() 

# make_tv_box <- function(rodovia, 
#                         inauguracao, 
#                         execucao,
#                         trecho, 
#                         status, 
#                         contratada,
#                         municipio, 
#                         regiao_desenvolvimento,
#                         inicio, 
#                         fim, 
#                         subacao,
#                         palheta_cor, 
#                         cor, 
#                         badge_execucao, 
#                         badge_valor, 
#                         badge_status){
#   
#   valor_rodovia <- div(rodovia, style="display: flex; flex-direction: row; align-items: center; justify-content: start;") #
#   
#   if(is.na(inauguracao)) {
#     valor <- ""
#   } else {
#     if (inauguracao == "A inaugurar")
#       valor <- vialactea::tag_svg("A inaugurar", color = "forestgreen")
#     #fa("flag-checkered", height = "1rem", vertical_align = "center", fill = "forestgreen")
#     if (inauguracao == "Inaugurada")
#       valor <- vialactea::tag_svg("Inaugurada", color = "purple")
#     
#     if (inauguracao == "Inaugurada Parcialmente")
#       valor <- vialactea::tag_svg("Parcial", color = "purple")
#     
#     if (inauguracao == "Finalizada")
#       valor <- vialactea::tag_svg("Finalizada", color = "black")
#     # fa("flag-checkered", height = "1rem", vertical_align = "center", fill = "purple")
#   }
#   
#   bslib::value_box(class = "rodovias-cards",
#     title = NULL,
#     #tags$img(alt="Execução", src="https://img.shields.io/badge/Execução-20%25-8ac926"), #tags$p("Execução ", tags$b(execucao)),
#     value = tags$span(class = "card-rodovia-value", valor_rodovia),
#     tags$p(style = "padding-bottom:10px",
#            HTML(badge_execucao), HTML(badge_valor), valor #HTML(badge_status)
#     ),
#     tags$p(tags$b("Trecho: "), if_else(is.na(trecho), "S/I", trecho)),
#     tags$p(tags$b("Contratada: "), if_else(is.na(contratada), "S/I", contratada)),
#     #tags$p(tags$b("Status: "), status),
#     tags$p(tags$b("Municípios: "), tags$span(class = "card-mun-value", if_else(is.na(municipio), "S/I", municipio))),
#     tags$p(tags$b("Região de desenvolvimento: "), tags$span(class = "card-rd-value", if_else(is.na(regiao_desenvolvimento), "S/I", regiao_desenvolvimento))),
#     tags$p(tags$b("Data de início: "), inicio),
#     tags$p(tags$b("Data de conclusão: "), fim),
#     #tags$p(tags$b("Subação: "), subacao),
#     full_screen = FALSE,
#     theme = bslib::value_box_theme(bg = cor)) 
# }

make_tv_box <- function(
    rodovia, 
    inauguracao, 
    execucao,
    trecho, 
    status, 
    contratada,
    municipio, 
    regiao_desenvolvimento,
    inicio, 
    fim, 
    subacao,
    palheta_cor, 
    cor, 
    badge_execucao, 
    badge_valor, 
    badge_status
) {
  
  # Título da rodovia
  valor_rodovia <- div(
    rodovia, 
    style = "display: flex; flex-direction: row; align-items: center; justify-content: start;"
  )
  
  # valor começa sempre definido
  valor <- ""
  
  # Só tenta criar o selo se tiver algo em inauguracao
  if (!is.na(inauguracao)) {
    
    if (inauguracao == "A inaugurar") {
      valor <- vialactea::tag_svg("A inaugurar", color = "forestgreen")
      
    } else if (inauguracao == "Inaugurada") {
      valor <- vialactea::tag_svg("Inaugurada", color = "purple")
      
    } else if (inauguracao == "Inaugurada Parcialmente") {
      valor <- vialactea::tag_svg("Parcial", color = "purple")
      
    } else if (inauguracao == "Finalizada") {
      valor <- vialactea::tag_svg("Finalizada", color = "black")
      
    } else {
      # qualquer valor "estranho" cai aqui e mantém valor como ""
      valor <- ""
    }
  }
  
  bslib::value_box(
    class = "rodovias-cards",
    title = NULL,
    
    value = tags$span(
      class = "card-rodovia-value",
      valor_rodovia
    ),
    
    tags$p(
      style = "padding-bottom:10px",
      HTML(badge_execucao),
      HTML(badge_valor),
      valor   # aqui 'valor' já está sempre definido
    ),
    
    tags$p(tags$b("Trecho: "), if_else(is.na(trecho), "S/I", trecho)),
    tags$p(tags$b("Contratada: "), if_else(is.na(contratada), "S/I", contratada)),
    
    tags$p(
      tags$b("Municípios: "),
      tags$span(class = "card-mun-value", if_else(is.na(municipio), "S/I", municipio))
    ),
    
    tags$p(
      tags$b("Região de desenvolvimento: "),
      tags$span(class = "card-rd-value", if_else(is.na(regiao_desenvolvimento), "S/I", regiao_desenvolvimento))
    ),
    
    tags$p(tags$b("Data de início: "), inicio),
    tags$p(tags$b("Data de conclusão: "), fim),
    
    full_screen = FALSE,
    theme = bslib::value_box_theme(bg = cor)
  )
}

tabela_cards <- tabela_pe_na_estrada %>% 
  mutate(palheta_cor = if_else(is.na(avanco_fisico), "#C0C0C0", palheta[1 + .99*round(avanco_fisico)])) %>% 
  mutate(avanco_fisico = if_else(is.na(avanco_fisico), "S/I", str_replace(paste0(round(avanco_fisico, 1), "%"), "\\.", ","))) %>% 
  arrange(rodovia) %>% 
  mutate(avanco_label = case_when(
    inauguracao == "A inaugurar" ~ paste0(avanco_fisico, " 🏳️"),
    inauguracao == "Inaugurada" ~ paste0(avanco_fisico, " 🏁"),
    inauguracao == "Finalizada" ~ paste0(avanco_fisico, " 🏴"),
    inauguracao == "Inaugurada Parcialmente" ~ paste0(avanco_fisico, " ½🏁"),
    .default = avanco_fisico
  )) %>% 
  mutate(status_cor = case_when(
    status == "Obra a contratar"  ~ "#1982c4F0", #"#ffca3aF0",
    status == "Concluída"  ~ "#8ac926F0", #"#8ac926F0",
    status == "Em andamento"  ~ "#ffca3aF0", #"#1982c4F0",
    status == "Paralisada" ~ "#ff595eF0", #"#ff595eF0",
    .default = "#ecececf0"
  )) %>%
  rowwise() %>% 
  mutate(badge_execucao = badge_svg("Execução", if_else(is.na(avanco_fisico), "S/I", avanco_fisico), palheta_cor, font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  #mutate(badge_subacao = badge_svg("Ação.Subação", if_else(is.na(acao_subacao), "S/I", acao_subacao), "white", font = "Jost,sans-serif", fontsize = 12)) %>% 
  mutate(badge_valor = badge_svg("Valor", if_else(is.na(valor_vigente), "S/I", vialactea::comprss(valor_vigente)), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_status = badge_svg("Status", if_else(is.na(status), "S/I", status), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
 mutate(
    inicio  = if_else(
      is.na(inicio_execucao),
      "Sem Informação",
      withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"), format(inicio_execucao, "%B de %Y"))
    ),
    termino = if_else(
      is.na(termino_execucao),
      "Sem Informação",
      withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"), format(termino_execucao, "%B de %Y"))
    )
  ) %>%
  ungroup() %>% 
  group_split(ordem) %>% 
  future_map(~ make_tv_box(rodovia = HTML(.x$rodovia[1]),
                           inauguracao = .x$inauguracao[1],
                           execucao = .x$avanco_fisico[1],
                           trecho = .x$trecho[1],
                           status = .x$status[1],
                           contratada = .x$contratada[1],
                           municipio = .x$municipios[1],
                           regiao_desenvolvimento = .x$regiao_desenvolvimento[1],
                           inicio = HTML(.x$inicio[1]),
                           fim = HTML(.x$termino[1]),
                           subacao = .x$acao_subacao[1], ####
                           palheta_cor = .x$palheta_cor[1], ###
                           cor = .x$status_cor[1],####
                           badge_execucao = .x$badge_execucao[1], ####
                           badge_valor = .x$badge_valor[1], ####
                           badge_status = .x$badge_status[1]), .options = furrr_options(seed = TRUE)) ####

tabela_cards2 <- tabela_pe_na_estrada %>% 
  mutate(palheta_cor = if_else(is.na(avanco_fisico), "#C0C0C0", palheta[1 + .99*round(avanco_fisico)])) %>% 
  mutate(avanco_fisico = if_else(is.na(avanco_fisico), "S/I", str_replace(paste0(round(avanco_fisico, 1), "%"), "\\.", ","))) %>% 
  arrange(rodovia) %>% 
  mutate(avanco_label = case_when(
    inauguracao == "A inaugurar" ~ paste0(avanco_fisico, " 🏳️"),
    inauguracao == "Inaugurada" ~ paste0(avanco_fisico, " 🏁"),
    inauguracao == "Finalizada" ~ paste0(avanco_fisico, " 🏴"),
    inauguracao == "Inaugurada Parcialmente" ~ paste0(avanco_fisico, " ½🏁"),
    .default = avanco_fisico
  )) %>% 
  mutate(status_cor = case_when(
    status == "Obra a contratar"  ~ "#1982c4F0", #"#ffca3aF0",
    status == "Concluída"  ~ "#8ac926F0", #"#8ac926F0",
    status == "Em andamento"  ~ "#ffca3aF0", #"#1982c4F0",
    status == "Paralisada" ~ "#ff595eF0", #"#ff595eF0",
    .default = "#ecececf0"
  )) %>%
  rowwise() %>% 
  mutate(badge_execucao = badge_svg("Execução", if_else(is.na(avanco_fisico), "S/I", avanco_fisico), palheta_cor, font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  #mutate(badge_subacao = badge_svg("Ação.Subação", if_else(is.na(acao_subacao), "S/I", acao_subacao), "white", font = "Jost,sans-serif", fontsize = 12)) %>% 
  mutate(badge_valor = badge_svg("Valor", if_else(is.na(valor_vigente) | valor_vigente == 0, "S/I", vialactea::comprss(valor_vigente)), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_extensao = badge_svg("KM", if_else(is.na(extensao_contrato) | extensao_contrato == 0, "S/I", format(extensao_contrato,decimal.mark = ",")), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_status = badge_svg("Status", if_else(is.na(status), "S/I", status), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
 mutate(
    inicio = if_else(
      is.na(inicio_execucao),
      "Sem Informação",
      withr::with_locale(
        c(LC_TIME = "pt_BR.UTF-8"),
        format(inicio_execucao, "%B de %Y")
      )
    ),
    termino = if_else(
      is.na(termino_execucao),
      "Sem Informação",
      withr::with_locale(
        c(LC_TIME = "pt_BR.UTF-8"),
        format(termino_execucao, "%B de %Y")
      )
    )
  ) %>%
  mutate(
    badge_update = withr::with_locale(
      c(LC_TIME = "pt_BR.UTF-8"),
      paste0(
        "Atualizado em ",
        format(now(), "%d de %B de %Y às %H:%M:%S"),
        "."
      )
    )
  ) %>%
  mutate(label = make_popup(ordem = ordem, 
                            rodovia = HTML(str_trim(str_remove_all(rodovia, "\\s*\\(.*\\)"))),
                            inauguracao = inauguracao,
                            execucao = avanco_fisico,
                            trecho = trecho,
                            status = status,
                            contratada = contratada,
                            municipio = municipios,
                            regiao_desenvolvimento = regiao_desenvolvimento,
                            inicio = HTML(inicio),
                            fim = HTML(termino),
                            subacao = acao_subacao, ####
                            palheta_cor = palheta_cor, ###
                            cor = status_cor,####
                            badge_execucao = badge_execucao, ####
                            badge_valor = badge_valor, ####
                            badge_status = badge_status,
                            badge_update = badge_update,
                            badge_extensao = badge_extensao,
                            revestimento = resvestimento_badges_list_no_tooltip[[revestimento]]
  ) 
  ) %>% 
  ungroup() %>% 
  pull(label) %>% 
  map(HTML)
  
notion_geometrias <- obras_notion_enriquecido %>% select(ordem, codigos_sre) %>% 
  mutate(codigos_sre = str_split(codigos_sre, ",")) %>% 
  unnest(codigos_sre) %>% 
  mutate(codigos_sre = str_trim(codigos_sre)) %>% 
  left_join(pernambuco_rodovias_sre_2025 %>% 
              select(nome, sre, geometry), 
            by = c("codigos_sre" = "sre")) %>% 
  filter(!is.na(nome)) %>% 
  select(-nome) %>% 
  st_set_geometry("geometry") %>% 
  st_transform(4326) %>% 
  group_by(ordem)  %>% 
  count()

obras_notion_geometrias <- notion_geometrias %>% 
  left_join(obras_notion_enriquecido,
            by = join_by(ordem)) %>% 
  mutate(status_cor = case_when(
    status == "Obra a contratar"  ~ "#1982c4F0", #"#ffca3aF0",
    status == "Concluída"  ~ "#8ac926F0", #"#8ac926F0",
    status == "Em andamento"  ~ "#ffca3aF0", #"#1982c4F0",
    status == "Paralisada" ~ "#ff595eF0", #"#ff595eF0",
    .default = "#ecececf0"
  )) %>% 
  mutate(label = pmap_chr(
    list(rodovia, status, inicio_execucao, termino_execucao, avanco_fisico, trecho, inauguracao, contratada, valor_vigente, extensao_contrato, link_pdf, link_logo),
    function(rodovia, status, inicio_execucao, termino_execucao, avanco_fisico, trecho, inauguracao, contratada, valor_vigente, extensao_contrato, link_pdf, link_logo) {
      sprintf(
        '<div style = "display:flex;flex-direction:column;flex-wrap: wrap;"><span width: 250px; white-space: normal;><b><center>%s</center></span></b>
          <div style="height: 2px; width: 100%%; background: antiquewhite; margin-bottom: 10px; margin-top: 5px;"></div>
          <div style="margin-bottom: 2px;">
          <b>Rodovia:</b> %s</div>
          <div style="margin-bottom: 2px;">
          <b>Início:</b> %s</div>
          <div style="margin-bottom: 2px;">
          <b>Término:</b> %s</div>
          <div style="width: 250px; white-space: normal;margin-bottom: 2px;">
          <b>Contratada:</b> %s</div>
          <div><b>Valor:</b> %s</div>
          <div style="margin-bottom: 5px;">
          %s&nbsp;%s&nbsp;%s</div></div>',
        trecho,
        rodovia,
        if_else(is.na(inicio_execucao), "–",
                withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"),
                                   format(inicio_execucao, "%b/%Y"))),
        if_else(is.na(termino_execucao), "–",
                withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"),
                                   format(termino_execucao, "%b/%Y"))),
        contratada,
        if_else(is.na(valor_vigente), "–", vialactea::comprss(valor_vigente)),
        vialactea::badge_svg(label = "Status", value = if_else(is.na(status), "–", status), color = "#fcfaf9", height = 12),
        vialactea::badge_svg(label = "Execução", value = if_else(is.na(avanco_fisico), "–", paste0(round(100*(avanco_fisico)), "%")), color = "#fcfaf9", height = 12),
        vialactea::badge_svg(label = "Extensão", value = if_else(is.na(extensao_contrato), "–", paste0(format(extensao_contrato, decimal.mark = ",", big.mark = ".", nsmal = 1), " ㎞")), 
                             color = "#fcfaf9", height = 12)
      )
    }
  )) %>%
  rowwise() %>%
  mutate(label = HTML(label)) %>%
  ungroup()

#####################
obras_notion_geometrias2 <- notion_geometrias %>% 
  left_join(obras_notion_enriquecido,
            by = join_by(ordem)) %>% 
  mutate(rodovia = str_remove_all(rodovia, "\\-")) %>%  
  #mutate(avanco_cor = get_score_color(avanco_fisico)) %>% 
  mutate(avanco_fisico = round(100*avanco_fisico)) %>% 
  mutate(reais_por_km = valor_vigente/extensao_contrato) %>% 
  mutate(inicio_execucao = dmy(paste("01", month(inicio_execucao), year(inicio_execucao), sep = "/"), quiet = TRUE)) %>% 
  mutate(termino_execucao = dmy(paste("01",month(termino_execucao), year(termino_execucao), sep = "/"), quiet = TRUE)) %>% 
  mutate(palheta_cor = if_else(is.na(avanco_fisico), "#C0C0C0", palheta[1 + .99*round(avanco_fisico)])) %>% 
  
  mutate(
    avanco_fisico = case_when(
      avanco_fisico > 100 ~ avanco_fisico / 100, # Ex: 7198 -> 71.98
      avanco_fisico <= 1  ~ avanco_fisico * 100, # Ex: 0.71 -> 71.00
      TRUE ~ avanco_fisico                    # Ex: 71 -> 71
    )
  ) %>%
  
  mutate(avanco_fisico = if_else(is.na(avanco_fisico), "S/I", str_replace(paste0(round(avanco_fisico, 1), "%"), "\\.", ","))) %>% 
  arrange(rodovia) %>% 
  mutate(avanco_label = case_when(
    inauguracao == "A inaugurar" ~ paste0(avanco_fisico, " 🏳️ "),
    inauguracao == "Inaugurada" ~ paste0(avanco_fisico, " 🏁 "),
    inauguracao == "Finalizada" ~ paste0(avanco_fisico, " 🏴 "),
    inauguracao == "Inaugurada Parcialmente" ~ paste0(avanco_fisico, " ½🏁 "),
    .default = avanco_fisico
  )) %>% 
  mutate(status_cor = case_when(
    status == "Obra a contratar"  ~ "#1982c4", #"#ffca3aF0",
    status == "Concluída"  ~ "#8ac926", #"#8ac926F0",
    status == "Em andamento"  ~ "#ffca3a", #F4D03F", #, "#1982c4F0",
    status == "Paralisada" ~ "#ff595e", #"#ff595eF0",
    .default = "#ececec"
  )) %>% 
  rowwise() %>% 
  mutate(badge_execucao = badge_svg("Execução", if_else(is.na(avanco_fisico), "S/I", avanco_fisico), palheta_cor, font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(
    badge_update = withr::with_locale(
      c(LC_TIME = "pt_BR.UTF-8"),
      paste0("Atualizado em ", format(now(), "%d de %B de %Y às %H:%M:%S"), ".")
    )
  ) %>%
  mutate(badge_valor = badge_svg("Valor", if_else(is.na(valor_vigente), "S/I", vialactea::comprss(valor_vigente)), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(badge_status = badge_svg("Status", if_else(is.na(status), "S/I", status), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(
    inicio = if_else(
      is.na(inicio_execucao), "S/I",
      withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"), format(inicio_execucao, "%b/%Y"))
    ),
    termino = if_else(
      is.na(termino_execucao), "S/I",
      withr::with_locale(c(LC_TIME = "pt_BR.UTF-8"), format(termino_execucao, "%b/%Y"))
    )
  ) %>%
  mutate(badge_extensao = badge_svg("KM", if_else(is.na(extensao_contrato) | extensao_contrato == 0, "S/I", format(extensao_contrato,decimal.mark = ",")), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)) %>% 
  mutate(label = make_popup(ordem = ordem, 
                            rodovia = HTML(str_trim(str_remove_all(rodovia, "\\s*\\(.*\\)"))),
                            inauguracao = inauguracao,
                            execucao = avanco_fisico,
                            trecho = trecho,
                            status = status,
                            contratada = contratada,
                            municipio = municipios,
                            regiao_desenvolvimento = regiao_desenvolvimento,
                            inicio = HTML(inicio),
                            fim = HTML(termino),
                            subacao = acao_subacao, ####
                            palheta_cor = palheta_cor, ###
                            cor = status_cor,####
                            badge_execucao = badge_execucao, ####
                            badge_valor = badge_valor, ####
                            badge_status = badge_status,
                            badge_update = badge_update,
                            badge_extensao = badge_extensao,
                            revestimento = resvestimento_badges_list_no_tooltip[[revestimento]]
                            ) 
  ) 

######################
extensoes <- pernambuco_regioes_administrativas %>% 
  select(Município,ibge7, `Região de Desenvolvimento`, geometry) %>% 
  st_intersection(obras_notion_geometrias %>% 
                    select(ordem, line_geometry = geometry) %>% 
                    mutate(extensao = st_length(line_geometry)))
  
fracoes_obras_municipio <- extensoes %>% 
  mutate(intersection = st_length(geometry)) %>% 
  st_drop_geometry() %>% 
  mutate(fracao_intersecao = intersection/extensao) %>% 
  select(ordem, municipio = Município, ibge7, 
         regiao_desenvolvimento = Região.de.Desenvolvimento, 
         obra_extensao = extensao, intersecao_municipio = intersection, fracao_intersecao)


fracoes_obras_rd <- fracoes_obras_municipio %>% group_by(ordem, regiao_desenvolvimento, obra_extensao) %>% 
  summarise(intersecao_municipio  = sum(intersecao_municipio, na.rm = TRUE), 
            fracao_intersecao =  sum(fracao_intersecao, na.rm = TRUE), .groups = "drop") %>% 
  left_join(obras_notion_enriquecido %>% select(ordem, valor_atual, valor_vigente, extensao_contrato)) %>% 
  group_by(regiao_desenvolvimento) %>% 
  summarise(
    valor_atual = sum(valor_atual*fracao_intersecao, na.rm = TRUE),
    valor_vigente = sum(valor_vigente*fracao_intersecao, na.rm = TRUE),
    extensao_contrato = sum(extensao_contrato*fracao_intersecao, na.rm = TRUE),
    extensao_sre = sum(intersecao_municipio*fracao_intersecao, na.rm = TRUE)
  ) 

fracoes_obras_municipio_resumo <-  full_join(
  fracoes_obras_municipio %>% 
    mutate(m = 
             if_else(as.numeric(fracao_intersecao) >= .999,
                     municipio,
                     paste0(municipio, " (", str_trim(format(round(as.numeric(fracao_intersecao)*100,1), decimal.mark = ",", nsmall = 1)), "%)"))) %>% 
    group_by(ordem) %>% 
    summarise(municipios = paste0(m, collapse = ", "))  
  
  ,
  fracoes_obras_municipio %>% 
    group_by(ordem, regiao_desenvolvimento) %>% 
    summarise(fracao_intersecao = sum(fracao_intersecao)) %>% 
    mutate(r = 
             if_else(as.numeric(fracao_intersecao) >= .999,
                     regiao_desenvolvimento,
                     paste0(regiao_desenvolvimento, " (", str_trim(format(round(as.numeric(fracao_intersecao)*100,1), decimal.mark = ",", nsmall = 1)), "%)"))) %>% 
    group_by(ordem) %>% 
    summarise(regiao_desenvolvimento = paste0(r, collapse = ", ")),
  by = "ordem"
)

################### BASE AURORA ##################

caminho_pe_sf <- if (file.exists(file.path(DIR_OUT_AURORA, "pernambuco_sf.rds"))) {
  file.path(DIR_OUT_AURORA, "pernambuco_sf.rds")
} else if (file.exists(file.path(DIR_MULUNGU, "pernambuco_sf.rds"))) {
  file.path(DIR_MULUNGU, "pernambuco_sf.rds")
} else {
  "../auroraprime/data/pernambuco_sf.rds"
}
pe_sf <- read_rds(caminho_pe_sf)

base_mulungu_aurora <- fracoes_obras_municipio %>%
  left_join(
    pe_sf %>%
      st_drop_geometry() %>%
      select(ibge7, longitude_sede = longitude, latitude_sede = latitude, populacao = População),
    by = join_by(ibge7)
  ) %>%
  select(
    id_mulungu = ordem, municipio, ibge7, populacao,
    regiao_de_desenvolvimento = regiao_desenvolvimento,
    fator_de_divisao_municipio = fracao_intersecao
  ) %>%
  mutate(fator_de_divisao_municipio = as.numeric(fator_de_divisao_municipio)) %>%
  left_join(
    tabela_pe_na_estrada %>%
      mutate(trecho = if_else(!is.na(trecho), paste0("Trecho: ", trecho), trecho)) %>% 
      mutate(local = rodovia) %>%  
      mutate(rodovia = if_else(!is.na(tipo) & (tipo != "S/I"), paste0(tipo," da ", rodovia), rodovia)) %>% 
      mutate(status = case_when(
        status == "Obra a contratar" ~ "Em licitação",
        status == "Em andamento" ~ "Em execução",
        .default = status
      )) %>% 
      select(
        id_mulungu = ordem, percent_execucao = avanco_fisico, status, 
        iniciativa_estrategica = rodovia, produto = trecho, local,
        valor_total = valor_vigente, observacoes,
        data_de_inicio = inicio_execucao,
        data_de_termino = termino_execucao
      ),
    by = join_by(id_mulungu)
  ) %>% 
  mutate(secretaria = "SEMOBI", 
         eixo_estrategico = "Desenvolvimento sustentável") %>% 
  mutate(avanco_cor = as.character(vialactea::get_score_color(percent_execucao))) %>%  
  mutate(valor_total = valor_total*fator_de_divisao_municipio) %>% 
 select(id_mulungu, eixo_estrategico, secretaria, status, percent_execucao, produto, iniciativa_estrategica, local, valor_total,
        avanco_cor,  data_de_inicio, data_de_termino, municipio, regiao_de_desenvolvimento, fator_de_divisao_municipio) %>%
  mutate(percent_execucao = case_when(
    
    percent_execucao > 100 ~ percent_execucao / 100,
    
    percent_execucao <= 1 ~ percent_execucao * 100,
    
    TRUE ~ percent_execucao
  ))

#### Rodovias
rodovias <- read_rds(file.path(DIR_RAW_MULUNGU, "rodovias.rds"))

rodovias_mulungu <- rodovias %>% left_join(
  obras_notion_enriquecido %>% select(ordem, codigos_sre) %>% 
    mutate(codigos_sre = str_split(codigos_sre, ",")) %>% 
    unnest(codigos_sre) %>% 
    mutate(codigos_sre = str_trim(codigos_sre)) %>% 
    filter(!is.na(codigos_sre)) %>% 
    count(codigos_sre) %>% 
    select(-n) %>% mutate(obra = "Sim"),
  by = c("sre" = "codigos_sre")
) %>% 
  mutate(obra = replace_na(obra, "Não"),
         pci_cls =
  case_when(
    round(pci) >= 86 ~ "Excelente",
    round(pci) >= 71 ~ "Muito bom",
    round(pci) >= 56 ~ "Bom",
    round(pci) >= 41 ~ "Regular",
    round(pci) >= 26 ~ "Ruim",
    round(pci) >= 11 ~ "Muito ruim",
    round(pci) >= 0 ~ "Péssimo",
    .default =  "S/I"
  )) %>% 
  left_join(dicionario_sre %>% select(nome, sre, mun, rd) %>% st_drop_geometry(),
            by = join_by(nome, sre)) %>% 
  select(nome, denom, sre, inicio, fim, ext, rev, sit, obra, pci, pci_cls, mun, rd, dro, geometry) 
  
##### Projetos Plug
write_rds(obras_notion_enriquecido, file.path(DIR_RAW_MULUNGU, "obras_notion_enriquecido.rds")) # ok

obras_notion_enriquecido <- read_rds(file.path(DIR_RAW_MULUNGU, "obras_notion_enriquecido.rds"))

projetos_obras_lista <- obras_notion_enriquecido %>% pull(contrato_projeto_plug) %>% na.omit()

tabela_ordem_plug <- obras_notion_enriquecido  %>% 
  select(ordem, starts_with("contrato_")) %>% 
  pivot_longer(cols = -ordem, names_to = "tipo_notion", values_to = "contrato_plug") %>% 
  mutate(tipo_notion = case_when(
    tipo_notion == "contrato_projeto_plug" ~ "Projeto",
    tipo_notion ==  "contrato_obra_plug" ~ "Obra",  
    tipo_notion == "contrato_supervisao_plug" ~ "Supervisão")) %>% 
  filter(!is.na(contrato_plug))

tabela_ordem_plug %>% saveRDS(
  file.path(DIR_MULUNGU, 'tabela_ordem_plug.rds')
)

sre_obras_lista <- obras_notion_enriquecido %>% 
  select(codigos_sre) %>% 
  filter(!is.na(codigos_sre)) %>%
  mutate(codigos_sre = str_split(codigos_sre, ",")) %>% 
  unnest(codigos_sre) %>% 
  mutate(codigos_sre = str_trim(codigos_sre)) %>% 
  pull(codigos_sre)

contratos_plug_projetos <- read_rds(file.path(DIR_RAW_MULUNGU, "contratos_plug.rds")) %>% 
  filter(
    str_detect(contrato_tipo_plug, "Projeto"), 
    !is.na(codigos_sre), 
    !(contrato_plug %in% projetos_obras_lista)) %>% 
  arrange(data_ordem_de_servico) %>% 
  mutate(ordem = row_number())

projetos_sf <- contratos_plug_projetos %>% select(ordem, codigos_sre) %>%
  mutate(codigos_sre = str_split(codigos_sre, ",")) %>%
  unnest(codigos_sre) %>%
  mutate(codigos_sre = str_trim(codigos_sre)) %>%
  filter(!(codigos_sre %in% sre_obras_lista)) %>%
  left_join(pernambuco_rodovias_sre_2025 %>%
              select(nome, sre, geometry),
            by = c("codigos_sre" = "sre")) %>%
  filter(!is.na(nome)) %>%
  #select(-nome) %>%
  st_set_geometry("geometry") %>%
  st_transform(4326) %>%
  group_by(ordem, rodovia = nome)  %>%
  count()

projetos_extra <- contratos_plug_projetos %>% select(ordem, codigos_sre) %>% 
  mutate(codigos_sre = str_split(codigos_sre, ",")) %>% 
  unnest(codigos_sre) %>% 
  mutate(codigos_sre = str_trim(codigos_sre)) %>% 
  filter(!(codigos_sre %in% sre_obras_lista)) %>% 
  left_join(dicionario_sre %>% st_drop_geometry(),
            by = c("codigos_sre" = "sre")) %>% 
  #filter(is.na(local_inicio))
  group_by(ordem, rodovia = nome) %>% 
  arrange(nome, km_ini) %>% 
  summarise(
    extensao_sre = sum(ext, na.rm = TRUE),
    trecho = paste0(
      unique(first(inicio, na_rm = TRUE, km_ini)),
      " ⟷ ", #↔ ⟺
      unique(last(fim, na_rm = TRUE, km_ini))
    ),
    km_inicio = first(km_ini, na_rm = TRUE),
    municipios = paste0(na.omit(unique(sort(str_split(paste0(na.omit(mun), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    dro = paste0(na.omit(unique(sort(str_split(paste0(na.omit(dro), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    regiao_desenvolvimento = paste0(na.omit(unique(sort(str_split(paste0(na.omit(rd), collapse = ", "), ", ", simplify = TRUE)))), collapse = ", "),
    .groups =  "drop"
  ) 

make_popup_projeto <- function(
    ordem, rodovia, contrato_plug, contrato_classe_plug, 
    trecho, situacao_plug, contratada, extensao_contrato, municipio, regiao_desenvolvimento, update) {
  # Rodovia label container
  valor_rodovia <- htmltools::tags$div(
    rodovia,
    style = "display: flex; flex-direction: row; align-items: center; justify-content: start;"
  )
  
  # Inauguration badge via SVG
  valor <- if (is.na(situacao_plug)) {
    ""
  } else {
    switch(
      situacao_plug,
      "A Iniciar" = vialactea::tag_svg(
        "Projeto a iniciar", color = "#1982c4F0", font = "Jost,sans-serif", fontsize = 12
      ),
      "Andamento" = vialactea::tag_svg(
        "Projeto em andamento", color = "#ffca3aF0", font = "Jost,sans-serif", fontsize = 12
      ),
      "Concluído" = vialactea::tag_svg(
        "Projeto concluído", color = "#8ac926F0", font = "Jost,sans-serif", fontsize = 12
      ),
      "Paralisado" = vialactea::tag_svg(
        "Projeto paralisado", color = "#ff595eF0", font = "Jost,sans-serif", fontsize = 12
      ),
      ""
    )
  }
  
  # Compose the value box
  bslib::value_box(
    class = glue::glue("rodovias-cards card-projeto-{ordem}"),
    style = "margin:5px;",
    title = NULL,
    value = htmltools::tags$span(
      style = "align-items: center; column-gap: 10px;",
      class = "card-rodovia-value d-flex",
      valor_rodovia, valor
    ),
    # Badges for execution and value
    htmltools::tags$p(
      style = "padding-bottom:10px",
      htmltools::HTML(badge_svg(label = "Plug", if_else(is.na(contrato_plug), "S/I", contrato_plug), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      htmltools::HTML(badge_svg(label = "Tipo", if_else(is.na(contrato_classe_plug), "S/I", contrato_classe_plug), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      htmltools::HTML(badge_svg(label = "KM", value = if_else(is.na(extensao_contrato) | extensao_contrato == 0, "S/I", format(extensao_contrato, decimal.mark = ",")), "white", font = "Jost,sans-serif", fontsize = 12, height = 12))
    ),
    # Detail rows
    htmltools::tags$div(
      style = 'font-size: .75rem; display: flex; flex-direction: column; row-gap: 10px; padding-bottom:10px',
      make_span(label = "Trecho", value = trecho, class = "card-trecho-value"),
      make_span(label = "Contratada", value = contratada, class = "card-contratada-value")
    ),
    htmltools::tags$div(
      style = 'font-size: .75rem; display: flex; flex-direction: row; column-gap: 10px; align-items: stretch; padding-bottom:10px',
      make_span(label = "Municípios", value = municipio, class = "card-mun-value"),
      make_span(label = "RD", value =  regiao_desenvolvimento, class = "card-rd-value")
    ),
    # Update timestamp
    htmltools::tags$p(
      style = "font-weight: 300; font-size: 90%;",
      htmltools::HTML(update)
    ),
    full_screen = FALSE,
    theme = bslib::value_box_theme(bg = "#9400D3")
  ) %>%
    as.character() %>%
    htmltools::HTML()
}



projetos_geometrias <- projetos_sf %>%
  left_join(contratos_plug_projetos,
            by = join_by(ordem)) %>% 
  left_join(projetos_extra, by = join_by(ordem, rodovia)) %>% 
  arrange(rodovia) %>% 
  mutate(
    update = withr::with_locale(
      c(LC_TIME = "pt_BR.UTF-8"),
      paste0(
        "Atualizado em ",
        format(now(), "%d de %B de %Y às %H:%M:%S"),
        "."
      )
    )
  ) %>%
  rowwise() %>% 
  mutate(label = make_popup_projeto(
    ordem = ordem, 
    rodovia = rodovia, 
    contrato_plug = contrato_plug,
    contrato_classe_plug = contrato_classe_plug, 
    trecho = trecho, 
    situacao_plug = situacao_plug, 
    contratada = contratada, 
    extensao_contrato = extensao_contrato,
    municipio = municipios, 
    regiao_desenvolvimento = regiao_desenvolvimento,
    update = update))

# salvar ----

# Diretórios para parquet e geojson
dir.create(file.path(DIR_OUT_AURORA, "parquet"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_OUT_AURORA, "geojson"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_MULUNGU, "parquet"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_MULUNGU, "geojson"), recursive = TRUE, showWarnings = FALSE)

# 1. Base DER/Mulungu consumida pelo Aurora (etl_05_iniciativas_join.R)
write_rds(base_mulungu_aurora, file.path(DIR_OUT_AURORA, "base_mulungu_aurora.rds"))
write_rds(base_mulungu_aurora, file.path(DIR_RAW_AURORA, "base_mulungu_aurora.rds"))
arrow::write_parquet(as.data.frame(base_mulungu_aurora), file.path(DIR_OUT_AURORA, "parquet/base_mulungu_aurora.parquet"))
arrow::write_parquet(as.data.frame(base_mulungu_aurora), file.path(DIR_OUT_AURORA, "base_mulungu_aurora.parquet"))

# Geometrias DER/Mulungu consumidas pelo Aurora
write_rds(obras_notion_geometrias2, file.path(DIR_OUT_AURORA, "obras_notion_geometrias2.rds"))
geojsonsf::sf_geojson(obras_notion_geometrias2) %>% write_lines(file.path(DIR_OUT_AURORA, "geojson/obras_notion_geometrias2.geojson"))

write_rds(projetos_geometrias, file.path(DIR_OUT_AURORA, "projetos_geometrias.rds"))
geojsonsf::sf_geojson(projetos_geometrias) %>% write_lines(file.path(DIR_OUT_AURORA, "geojson/projetos_geometrias.geojson"))

# 2. Dados Tabulares do Mulungu (Parquet e RDS)
write_rds(contratos_plug_enriquecidos, file.path(DIR_MULUNGU, "contratos_plug_enriquecidos.rds"))
arrow::write_parquet(as.data.frame(contratos_plug_enriquecidos), file.path(DIR_MULUNGU, "contratos_plug_enriquecidos.parquet"))
arrow::write_parquet(as.data.frame(contratos_plug_enriquecidos), file.path(DIR_MULUNGU, "parquet/contratos_plug_enriquecidos.parquet"))

write_rds(obras_notion_enriquecido, file.path(DIR_MULUNGU, "obras_notion_enriquecido.rds"))
arrow::write_parquet(as.data.frame(obras_notion_enriquecido), file.path(DIR_MULUNGU, "obras_notion_enriquecido.parquet"))
arrow::write_parquet(as.data.frame(obras_notion_enriquecido), file.path(DIR_MULUNGU, "parquet/obras_notion_enriquecido.parquet"))

write_rds(tabela_pe_na_estrada, file.path(DIR_MULUNGU, "tabela_pe_na_estrada.rds"))
arrow::write_parquet(as.data.frame(tabela_pe_na_estrada), file.path(DIR_MULUNGU, "tabela_pe_na_estrada.parquet"))
arrow::write_parquet(as.data.frame(tabela_pe_na_estrada), file.path(DIR_MULUNGU, "parquet/tabela_pe_na_estrada.parquet"))

write_rds(tabela_timeline, file.path(DIR_MULUNGU, "tabela_timeline.rds"))
arrow::write_parquet(as.data.frame(tabela_timeline), file.path(DIR_MULUNGU, "tabela_timeline.parquet"))
arrow::write_parquet(as.data.frame(tabela_timeline), file.path(DIR_MULUNGU, "parquet/tabela_timeline.parquet"))

if (exists("tabela_ordem_plug")) {
  write_rds(tabela_ordem_plug, file.path(DIR_MULUNGU, "tabela_ordem_plug.rds"))
  arrow::write_parquet(as.data.frame(tabela_ordem_plug), file.path(DIR_MULUNGU, "tabela_ordem_plug.parquet"))
  arrow::write_parquet(as.data.frame(tabela_ordem_plug), file.path(DIR_MULUNGU, "parquet/tabela_ordem_plug.parquet"))
}

# 3. Geometrias do Mulungu (GeoJSON e RDS)
write_rds(obras_notion_geometrias, file.path(DIR_MULUNGU, "obras_notion_geometrias.rds"))
geojsonsf::sf_geojson(obras_notion_geometrias) %>% write_lines(file.path(DIR_MULUNGU, "obras_notion_geometrias.geojson"))
geojsonsf::sf_geojson(obras_notion_geometrias) %>% write_lines(file.path(DIR_MULUNGU, "geojson/obras_notion_geometrias.geojson"))

write_rds(obras_notion_geometrias2, file.path(DIR_MULUNGU, "obras_notion_geometrias2.rds"))
geojsonsf::sf_geojson(obras_notion_geometrias2) %>% write_lines(file.path(DIR_MULUNGU, "obras_notion_geometrias2.geojson"))
geojsonsf::sf_geojson(obras_notion_geometrias2) %>% write_lines(file.path(DIR_MULUNGU, "geojson/obras_notion_geometrias2.geojson"))

write_rds(projetos_geometrias, file.path(DIR_MULUNGU, "projetos_geometrias.rds"))
geojsonsf::sf_geojson(projetos_geometrias) %>% write_lines(file.path(DIR_MULUNGU, "projetos_geometrias.geojson"))
geojsonsf::sf_geojson(projetos_geometrias) %>% write_lines(file.path(DIR_MULUNGU, "geojson/projetos_geometrias.geojson"))

write_rds(rodovias_mulungu, file.path(DIR_MULUNGU, "rodovias_mulungu.rds"))
geojsonsf::sf_geojson(rodovias_mulungu) %>% write_lines(file.path(DIR_MULUNGU, "rodovias_mulungu.geojson"))
geojsonsf::sf_geojson(rodovias_mulungu) %>% write_lines(file.path(DIR_MULUNGU, "geojson/rodovias_mulungu.geojson"))

# 4. Objetos UI/Cards (listas Shiny mantidas em RDS)
write_rds(tabela_cards, file.path(DIR_MULUNGU, "tabela_cards.rds"))
write_rds(tabela_cards2, file.path(DIR_MULUNGU, "tabela_cards2.rds"))

# 5. Bases Raw/Intermediárias
write_rds(contratos_plug_enriquecidos, file.path(DIR_RAW_MULUNGU, "contratos_plug_enriquecidos.rds"))
write_rds(fracoes_obras_rd, file.path(DIR_RAW_MULUNGU, "fracoes_obras_rd.rds"))
write_rds(fracoes_obras_municipio_resumo, file.path(DIR_RAW_MULUNGU, "fracoes_obras_municipio_resumo.rds"))
write_rds(fracoes_obras_municipio, file.path(DIR_RAW_MULUNGU, "fracoes_obras_municipio.rds"))

caminho_config <- file.path(DIR_MULUNGU, "config.yml")
if (file.exists(caminho_config)) {
  config_file <- read_lines(caminho_config)
  config_file[4] <- glue::glue("  update: \"{ format(Sys.Date(), '%Y/%m/%d') }\"")
  write_lines(x = config_file, caminho_config, append = FALSE)
}

# bibliotecas ----
library(dplyr)
library(glue)
library(googledrive)
library(htmltools)
library(janitor)
library(purrr)
library(readxl)
library(stringr)

# setup ----
options(scipen = 999)

drive_auth(email = "hugoavmedeiros@gmail.com")

# helpers ----
fmt <- function(x) {
  if (!is.null(x)) {
    ifelse(is.na(x), "—",
           format(x, big.mark=".", decimal.mark=",", scientific=FALSE))
  } else "—"
}

card_line_num <- function(label, v_num, v_fmt) {
  ifelse(!is.na(v_num) & v_num > 0,
         sprintf(
           "<div><div style='color:#667;font-size:11px;'>%s</div>
       <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>",
           label, v_fmt
         ),
         ""
  )
}

card_line_text <- function(label, v) {
  v2 <- if (is.logical(v)) ifelse(v, "Sim", "Não") else as.character(v)
  v2 <- trimws(v2)
  ifelse(!is.na(v2) & v2 != "",
         sprintf(
           "<div><div style='color:#667;font-size:11px;'>%s</div>
       <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;word-break:break-word;overflow-wrap:anywhere;'>%s</div></div>",
           label, htmlEscape(v2)
         ),
         ""
  )
}

# ler ----
## censo 2024 quanti ----
ed_PE_censo_2024 <- readxl::read_excel(
  #'data/educacao/microdados do censo 2024 todas as ESCOLAS com endereço.xlsx'
  'data/educacao/Microdados censo 2024 - detalhamento das matrículas.xlsx'
) %>%
  janitor::clean_names() %>%
  filter(
    co_uf == 26
  )

ed_PE_censo_2024 %>% glimpse()

## censo 2024 quali ----
ed_PE_censo_2024_quali <- 
  qs::qread('data/educacao/ed_PE_censo_2024.qs') %>%
  select(
    co_entidade, 18:221
  )

ed_PE_censo_2024 <- ed_PE_censo_2024_quali %>%
  left_join(
    ed_PE_censo_2024,
    by = c('co_entidade')
  )

ed_PE_censo_2024 %>% glimpse()

## siepe 2025 raça ----
ed_siepe_raca_2025 <- readxl::read_excel(
  'data/educacao/Distribuicao de Alunos declarados Siepe por raca_cor rede estadual.xlsx'
) %>%
  janitor::clean_names() %>%
  mutate(
    fonte = 'SIEPE',
    ano = 2025
  ) %>%
  select(
    co_entidade,
    fonte,
    ano,
    amarela = amarela_2,
    branca = branca_3,
    indigena = indigena_4,
    parda = parda_5,
    preta = preta_6,
    total = total_de_declarados
  )

ed_PE_censo_2024 <- ed_PE_censo_2024 %>%
  left_join(
    ed_siepe_raca_2025,
    by = c('co_entidade')
  )

## censo 2024 geo ----
escolas_pe_geo <- readxl::read_excel(
  'data/educacao/escolas_pe_geo.xlsx'
) %>%
  janitor::clean_names()

escolas_pe_geo %>% glimpse()

## obras ----
url_educacao_obras <- "https://docs.google.com/spreadsheets/d/1YT6PbqMXI1bLSNDUH4rLc2hhk2SjduNa0xg1iAXu4i0/edit?gid=0#gid=0"

arquivo_educacao_obras <- drive_download(
  as_id(url_educacao_obras),
  #path = tempfile(fileext = ".xlsx"),
  path = file.path(getwd(), "data", "educacao_obras.xlsx"),
  overwrite = TRUE)

abas_educacao_obras <- excel_sheets(arquivo_educacao_obras$local_path)

plan_educacao_obras <- map2(
  abas_educacao_obras,
  seq_along(abas_educacao_obras),
  ~ if (.y == 1) {
    read_excel(
      arquivo_educacao_obras$local_path, 
      sheet = .x,
      skip = 1)
  } else {
    read_excel(
      arquivo_educacao_obras$local_path, 
      sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_educacao_obras) <- abas_educacao_obras

dados_educacao_obras <- plan_educacao_obras$Escolas %>%
  #janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
  clean_names()

# transformação ----
ed_PE_censo_2024_2 <- ed_PE_censo_2024 %>%
  left_join(
    escolas_pe_geo %>%
      select(
        code_school, lon, lat
      ), 
    by = c('co_entidade' = 'code_school')
  ) %>%
  left_join(
    dados_educacao_obras %>%
      select(
        codigo_inep, quadra_da_escola, climatizacao, subestacao, adaptacoes_para_climatizar, status_da_manutencao, status_21, 
      ),
    by = c('co_entidade' = 'codigo_inep')
  )

cols_mat <- c(
  "qt_mat_eja_fund","qt_mat_eja_med",
  "qt_mat_inf_cre","qt_mat_inf_pre",
  "qt_mat_fund_ai","qt_mat_fund_af",
  "qt_mat_med",
  "qt_mat_prof","qt_mat_prof_tec_subs",
  "qt_mat_fund_ai_int","qt_mat_fund_af_int","qt_mat_med_int"
)

ed_PE_censo_2024_3 <- ed_PE_censo_2024_2 %>%
  mutate(
    
    across(all_of(cols_mat),
           ~ as.integer(coalesce(.x, 0) > 0),
           .names = "{.col}_dum"),
    
    tipo = case_when(
      tp_dependencia == 1 ~ "federal",
      tp_dependencia == 2 ~ "estadual",
      tp_dependencia == 3 ~ "municipal",
      tp_dependencia == 4 ~ "privada",
      TRUE                ~ "outro"
    ),
    
    icon = dplyr::recode(
      tipo,
      "municipal" = "school-municipal",
      "estadual"  = "school-estadual",
      "privada"   = "school-privada",
      "federal"   = "school-federal",
      .default    = "school-municipal"
    ),
    
    qt_mat_eja = qt_mat_eja_fund + qt_mat_eja_med,
    
    cor_tipo = dplyr::recode(
      tipo,
      "municipal" = "#2878B5",
      "estadual"  = "#E67E22",
      "privada"   = "#7D3C98",
      "federal"   = "#27AE60",
      .default    = "#6C757D"
    ),
    nome_label = coalesce(as.character(.data$no_entidade), "Escola (s/ nome)"),
    tooltip    = nome_label,
    
    # >>> formatações
    mat_eja_fmt  = fmt(qt_mat_eja),
    mat_cre_fmt  = fmt(qt_mat_inf_cre),
    mat_pre_fmt  = fmt(qt_mat_inf_pre),
    mat_fai_fmt  = fmt(qt_mat_fund_ai),
    mat_faf_fmt  = fmt(qt_mat_fund_af),
    mat_med_fmt  = fmt(qt_mat_med),
    mat_prof_fmt = fmt(qt_mat_prof),
    mat_profsub_fmt = fmt(qt_mat_prof_tec_subs),
    mat_faii_fmt = fmt(qt_mat_fund_ai_int),
    mat_fafi_fmt = fmt(qt_mat_fund_af_int),
    mat_medi_fmt = fmt(qt_mat_med_int),
    
    # >>> popup enxuto e organizado em grid
#     popup = sprintf(
#       "
# <div style='font-family:system-ui,-apple-system,Segoe UI,Roboto;
#             background:%s; color:#fff; border-radius:10px;
#             box-shadow:0 6px 20px rgba(0,0,0,.25); overflow:hidden; min-width:320px;'>
#   <div style='padding:12px 14px; font-weight:700; font-size:14px;'>%s</div>
#   <div style='background:rgba(255,255,255,.92); color:#333; padding:12px 14px;'>
#     <div style='display:grid; grid-template-columns:repeat(4,1fr); gap:8px; font-size:12px;'>
#       
#       <div><div style='color:#667;font-size:11px;'>Tipo</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>EJA</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Creche</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Pré-Esc</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>EFAI</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>EFAF</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Médio</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Prof.</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Prof. Sub.</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>EFAI Int</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>EFAF Int.</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#       
#       <div><div style='color:#667;font-size:11px;'>Médio Int.</div>
#            <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;text-align:right;'>%s</div></div>
#     </div>
#   </div>
# </div>
#       ",
#       cor_tipo,
#       htmlEscape(as.character(nome_label)),
#       str_to_title(.data$tipo),
#       mat_eja_fmt,
#       mat_cre_fmt,
#       mat_pre_fmt,
#       mat_fai_fmt,
#       mat_faf_fmt,
#       mat_med_fmt,
#       mat_prof_fmt,
#       mat_profsub_fmt,
#       mat_faii_fmt,
#       mat_fafi_fmt,
#       mat_medi_fmt
#     )

# linhas_condicionais = paste0(
#   card_line_num("EJA",        qt_mat_eja,           mat_eja_fmt),
#   card_line_num("Creche",     qt_mat_inf_cre,       mat_cre_fmt),
#   card_line_num("Pré-Esc",    qt_mat_inf_pre,       mat_pre_fmt),
#   card_line_num("EFAI",       qt_mat_fund_ai,       mat_fai_fmt),
#   card_line_num("EFAF",       qt_mat_fund_af,       mat_faf_fmt),
#   card_line_num("Médio",      qt_mat_med,           mat_med_fmt),
#   card_line_num("Prof.",      qt_mat_prof,          mat_prof_fmt),
#   card_line_num("Prof. Sub.", qt_mat_prof_tec_subs, mat_profsub_fmt),
#   card_line_num("EFAI Int",   qt_mat_fund_ai_int,   mat_faii_fmt),
#   card_line_num("EFAF Int.",  qt_mat_fund_af_int,   mat_fafi_fmt),
#   card_line_num("Médio Int.", qt_mat_med_int,       mat_medi_fmt)
# ),

linhas_condicionais = paste0(
  # --- qualitativas logo após "Tipo"
  card_line_text("Climatização",              climatizacao),
  card_line_text("Subestação",                subestacao),
  card_line_text("Adaptações p/ climat.",     adaptacoes_para_climatizar),
  card_line_text("Status da manutenção",      status_da_manutencao),
  
  # --- depois as numéricas > 0 (já usando sua card_line_num)
  card_line_num("EJA",        qt_mat_eja,           mat_eja_fmt),
  card_line_num("Creche",     qt_mat_inf_cre,       mat_cre_fmt),
  card_line_num("Pré-Esc",    qt_mat_inf_pre,       mat_pre_fmt),
  card_line_num("EFAI",       qt_mat_fund_ai,       mat_fai_fmt),
  card_line_num("EFAF",       qt_mat_fund_af,       mat_faf_fmt),
  card_line_num("Médio",      qt_mat_med,           mat_med_fmt),
  card_line_num("Prof.",      qt_mat_prof,          mat_prof_fmt),
  card_line_num("Prof. Sub.", qt_mat_prof_tec_subs, mat_profsub_fmt),
  card_line_num("EFAI Int",   qt_mat_fund_ai_int,   mat_faii_fmt),
  card_line_num("EFAF Int.",  qt_mat_fund_af_int,   mat_fafi_fmt),
  card_line_num("Médio Int.", qt_mat_med_int,       mat_medi_fmt)
),

popup = sprintf(
  "
<div style='font-family:system-ui,-apple-system,Segoe UI,Roboto;
            background:%s; color:#fff; border-radius:10px;
            box-shadow:0 6px 20px rgba(0,0,0,.25); overflow:hidden; min-width:320px;'>
  <div style='padding:12px 14px; font-weight:700; font-size:14px;'>%s</div>
  <div style='background:rgba(255,255,255,.92); color:#333; padding:12px 14px;'>
    <div style='display:grid; grid-template-columns: repeat(4, minmax(120px, 1fr)); gap:8px; font-size:12px;'>
      <div><div style='color:#667;font-size:11px;'>Tipo</div>
           <div style='background:#fff;border-radius:6px;padding:6px;font-weight:600;'>%s</div></div>
      %s
    </div>
  </div>
</div>
      ",
  cor_tipo,
  htmlEscape(as.character(nome_label)),
  str_to_title(.data$tipo),
  linhas_condicionais
)

  )

ed_PE_censo_2024_4 <- ed_PE_censo_2024_3

ed_PE_censo_2024_4 %>% class()

ed_PE_censo_2024_4 %>% glimpse()

# salvar ----
ed_PE_censo_2024_4 %>%
  saveRDS('app_aurora/data/escolas.rds')



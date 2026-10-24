# =========================================================
# BIBLIOTECAS
# =========================================================
library(tidyverse)
library(readxl)
library(googledrive)
library(sf)
library(fs)
library(purrr)

# =========================================================
# EXTRACT — PLANILHA SEDUH / SEHAB
# =========================================================
drive_auth(email = "hugo.vasconcelos@gd.seplag.pe.gov.br")
drive_user()

url_seduh_sehab <- "https://docs.google.com/spreadsheets/d/190eQuQx5qe7Ofjl6BzQqnRg4zxm_rLB5/edit?gid=1847341728#gid=1847341728"

arquivo_seduh_sehab <- drive_download(
  as_id(url_seduh_sehab),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE
)

dados_seduh_sehab <- arquivo_seduh_sehab$local_path |>
  excel_sheets() |>
  set_names() |>
  map(\(sheet_name)
      read_excel(
        arquivo_seduh_sehab$local_path,
        sheet = sheet_name,
        col_types = "text"
      )
  )

seduh_sehab_df <- dados_seduh_sehab$`Meta com Vários Produtos` %>%
  janitor::clean_names() %>%
  select(!starts_with("x")) %>%
  filter(
    iniciativa_estrategica == "REFORMA NO LAR I"
  ) %>%
  mutate(
    secretaria_unidade = "SEDUH - SEHAB",
    secretaria = str_remove(secretaria, " - .*")
  )

# =========================================================
# EXTRACT — SHAPES (POLÍGONOS)
# =========================================================
caminho_shp <- "data/mapbox/seduh"
CRS_DESTINO <- 4326

diretorios_lotes <- fs::dir_ls(caminho_shp, type = "directory")

seduh_sehab_shp <- purrr::map_dfr(diretorios_lotes, function(dir_path) {
  
  shp_path <- fs::dir_ls(dir_path, glob = "*.shp")
  
  if (length(shp_path) == 0) return(NULL)
  
  shp <- st_read(shp_path[1], quiet = TRUE)
  
  if (st_crs(shp)$epsg != CRS_DESTINO) {
    shp <- st_transform(shp, CRS_DESTINO)
  }
  
  shp$ID_LOTE_PASTA <- basename(dir_path)
  shp
})

# =========================================================
# TRANSFORM — IDENTIFICA INICIATIVA / PRODUTO
# =========================================================
seduh_sehab_shp <- seduh_sehab_shp %>%
  mutate(
    geometry = st_make_valid(geometry),
    geometry = st_buffer(geometry, 0),
    temp_col = str_replace(ID_LOTE_PASTA, "RNL I", "REFORMA NO LAR I")
  ) %>%
  separate(
    temp_col,
    into = c("iniciativa_estrategica", "produto"),
    sep = " - ",
    extra = "merge",
    remove = TRUE
  ) %>%
  mutate(
    iniciativa_estrategica = str_trim(iniciativa_estrategica),
    produto = str_trim(produto)
  ) %>%
  st_cast("MULTIPOLYGON")

# =========================================================
# TRANSFORM — AGREGA POLÍGONOS POR PRODUTO
# =========================================================
seduh_sehab_shp_agregado <- seduh_sehab_shp %>%
  group_by(iniciativa_estrategica, produto) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  )

# =========================================================
# LOAD — JOIN PLANILHA + GEOMETRIA
# =========================================================
seduh_sehab_sf <- seduh_sehab_df %>%
  left_join(
    seduh_sehab_shp_agregado,
    by = c("iniciativa_estrategica", "produto")
  ) %>%
  st_as_sf(crs = 4326) %>%
  mutate(
    fonte = 'SEDUH'
  ) %>%
  mutate(
    eixo_estrategico = case_when(
      str_detect(eixo_estrategico, regex("SEGURANÇ", ignore_case = TRUE)) ~ 
        "SEGURANÇA E CIDADANIA",
      str_detect(eixo_estrategico, regex("SAÚDE", ignore_case = TRUE)) ~ 
        "SAÚDE E QUALIDADE DE VIDA",
      str_detect(eixo_estrategico, regex("HABITAÇÃO|INFRAESTRUTURA", ignore_case = TRUE)) ~ 
        "DESENVOLVIMENTO SUSTENTÁVEL",
      str_detect(eixo_estrategico, regex("EDUCAÇÃO|CULTURA|INOVAÇÃO", ignore_case = TRUE)) ~ 
        "CONHECIMENTO E INOVAÇÃO",
      .default = eixo_estrategico,
    )
  ) %>% 
  mutate(
    secretaria = if_else(secretaria == "CEHAB/SEDUH", "CEHAB", secretaria)
  ) %>%
  filter(str_to_upper(status) %in% str_to_upper(valid_status)) %>% 
  mutate(status = str_to_sentence(status)) %>% 
  filter(str_to_upper(eixo_estrategico) %in% str_to_upper(eixos_cores$eixo)) %>% 
  mutate(eixo_estrategico = str_to_sentence(eixo_estrategico)) %>% 
  mutate(across(c(data_de_inicio, data_de_termino), ~ str_remove(.x, "\\.0$"))) %>% 
  mutate(data_excel = if_else(str_detect(data_de_inicio, "^\\d{5}$"), data_de_inicio, NA_character_)) %>% 
  mutate(data_dmy=  if_else(str_detect(data_de_inicio, "^\\d{2}.\\d{2}.\\d{4}$"), data_de_inicio, NA_character_)) %>% 
  mutate(data_excel = as_date(as.integer(data_excel), origin="1899-12-30")) %>% 
  mutate(data_dmy = dmy(data_dmy)) %>%
  mutate(data_de_inicio = case_when(
    !is.na(data_excel) ~ data_excel,
    !is.na(data_dmy) ~ data_dmy,
    .default = NA_Date_
  )) %>% 
  select(-data_dmy, -data_excel) %>% 
  mutate(data_excel = if_else(str_detect(data_de_termino, "^\\d{5}$"), data_de_termino, NA_character_)) %>% 
  mutate(data_dmy=  if_else(str_detect(data_de_termino, "^\\d{2}.\\d{2}.\\d{4}$"), data_de_termino, NA_character_)) %>% 
  mutate(data_excel = as_date(as.integer(data_excel), origin="1899-12-30")) %>% 
  mutate(data_dmy = dmy(data_dmy)) %>%
  mutate(data_de_termino = case_when(
    !is.na(data_excel) ~ data_excel,
    !is.na(data_dmy) ~ data_dmy,
    .default = NA_Date_
  )) %>% 
  select(-data_dmy, -data_excel) %>% 
  mutate(percent_execucao = str_replace(percent_execucao, ",", ".")) %>% 
  mutate(percent_execucao = parse_number(str_replace(percent_execucao, "%", "E-2"))) %>% 
  mutate(percent_execucao = if_else(percent_execucao > 1.01, percent_execucao/100, percent_execucao)) %>% 
  mutate(valor_total = if_else(str_detect(valor_total, "-"), NA, valor_total)) %>% 
  mutate(valor_total = str_remove_all(valor_total, "(\\.|,)\\d+$")) %>% 
  mutate(valor_total = if_else(str_detect(valor_total, "E\\d+$"), valor_total, str_remove_all(valor_total, "\\."))) %>% 
  mutate(valor_total = parse_number(if_else(str_detect(valor_total, "R\\$"), str_replace(str_remove_all(valor_total, "(R\\$\\s*)"),",", "."), valor_total))) %>% 
  mutate(latitude = parse_number(latitude)) %>% 
  mutate(longitude = parse_number(longitude)) %>% 
  mutate(id = row_number(), .before = eixo_estrategico) %>%
  mutate(municipio = str_split(municipio, ",")) %>% 
  unnest(municipio) %>% 
  mutate(municipio = str_trim(municipio)) %>% 
  mutate(municipio = case_match(municipio, 
                                "BELÉM DE SÃO FRANCISCO" ~ "BELÉM DO SÃO FRANCISCO",
                                "Belém de São Francisco" ~ "BELÉM DO SÃO FRANCISCO",
                                "BELÉM DE S. FRANCISCO" ~ "BELÉM DO SÃO FRANCISCO",
                                "QUIXABÁ" ~ "QUIXABA",
                                "SÃO CAITANO" ~ "SÃO CAETANO",
                                "São Caitano" ~ "SÃO CAETANO",
                                "TACAIMBO" ~ "TACAIMBÓ",
                                "IGUARACI" ~ "IGUARACY", 
                                "OURUCURI" ~ "OURICURI",
                                "TUPARATEMA" ~ "TUPARETAMA",
                                "AFOGADOS DA IGAZEIRA" ~ "AFOGADOS DA INGAZEIRA",
                                "GRANTO" ~ "GRANITO",
                                "ITAMARACÁ" ~ "ILHA DE ITAMARACÁ",       
                                "LAGOA DO ITAENGA" ~ "LAGOA DE ITAENGA",
                                "STA. CRUZ DO CAPIBARIBE" ~ "SANTA CRUZ DO CAPIBARIBE",
                                "STA. MARIA DO CAMBUCÁ" ~ "SANTA MARIA DO CAMBUCÁ",
                                "STA. CRUZ DA BAIXA VERDE" ~ "SANTA CRUZ DA BAIXA VERDE",
                                .default = municipio
  )) %>% 
  mutate(municipio = str_to_lower(municipio)) %>% 
  left_join(pe_sf %>% st_drop_geometry() %>% 
              select(Município, ibge7, `Região de Desenvolvimento`, longitude_sede = longitude, latitude_sede = latitude, População) %>% 
              mutate(municipio = str_to_lower(Município)),
            by = join_by(municipio)) %>% 
  select(-municipio) %>% 
  mutate(latitude = if_else((is.na(latitude)) & (!is.na(latitude_sede)), latitude_sede, latitude)) %>% 
  mutate(longitude = if_else((is.na(longitude)) & (!is.na(longitude_sede)), longitude_sede, longitude)) %>% 
  janitor::clean_names() %>% 
  ungroup() %>% 
  group_by(id) %>% 
  mutate(fator_de_divisao_municipio = 1/n()) %>%
  mutate(valor_total = valor_total*fator_de_divisao_municipio) %>% 
  mutate(fator_de_divisao_rd = 1/length(na.omit(unique(regiao_de_desenvolvimento)))) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  filter(
    !(longitude < (-180 - .Machine$double.eps) | 
        longitude > (360 + .Machine$double.eps) | 
        latitude < (-90 - .Machine$double.eps) | 
        latitude > (90 + .Machine$double.eps))) %>% 
  ungroup() %>% 
  group_by(latitude) %>% 
  mutate(latitude = latitude + (n() > 1)*(.0001/2)*runif(n(), -1, 1), latitude) %>%
  ungroup() %>% 
  group_by(longitude) %>% 
  mutate(longitude = longitude + (n() > 1)*(.0001/4)*runif(n(), -1, 1), longitude) %>%
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)  %>% 
  ungroup() %>% 
  select(-starts_with(match = "x")) %>%
  mutate(
    inicio = if_else(
      is.na(data_de_inicio),
      "S/I",
      format(data_de_inicio, "%b/%Y", locale = "pt_BR.UTF-8")
    ),
    termino = if_else(
      is.na(data_de_termino),
      "S/I",
      format(data_de_termino, "%b/%Y", locale = "pt_BR.UTF-8")
    )
  ) %>%
  mutate(ordem = row_number()) %>%
  mutate(avanco_cor = as.character(vialactea::get_score_color(percent_execucao))) %>%
  rowwise() %>%
  mutate(label = make_card_iniciativa(ordem, secretaria, fonte, eixo_estrategico, iniciativa_estrategica, produto, local, status, inicio,
                                      termino, percent_execucao, valor_total, observacoes, cores_status[status]
  )) %>%
  ungroup() %>%
  mutate(
    id_produto = row_number(),
    
    #secretaria_unidade = secretaria,
    
    secretaria_unidade = if_else(
      is.na(secretaria_unidade) | secretaria_unidade == "",
      str_remove(secretaria, " - .*"),
      secretaria_unidade
    ),
    
    secretaria = str_remove(secretaria, " - .*"),
    
    iniciativa_estrategica = str_squish(iniciativa_estrategica)
  )

# teste ----
centro_mapa <- st_bbox(seduh_sehab_sf) %>%
  st_as_sfc() %>%
  st_centroid() %>%
  st_coordinates()

centro_mapa

mapgl::maplibre() %>%
  fit_bounds(seduh_sehab_sf) %>%
  add_fill_layer(
    id = "lotes_restantes",
    source = seduh_sehab_sf, 
    fill_color = 'pink',
    fill_opacity = 0.6
  ) %>%
  add_line_layer(
    id = "lotes_borda",
    source = seduh_sehab_sf,
    line_color = "red",
    line_width = 2,
    line_opacity = 1
  )

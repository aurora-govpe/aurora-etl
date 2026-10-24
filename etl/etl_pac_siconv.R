# library(dplyr)
# library(data.table)
# library(transferegovr)
# library(writexl)
# 
# # EP SICONV ----
# dados_siconv_convenio <- fread(
#   'data/pac/siconv_convenio.csv'
# )
# 
# dados_siconv_proposta <- fread(
#   'data/pac/siconv_proposta.csv'
# )
# 
# dados_siconv_formalizacao_pac <- fread(
#   'data/pac/siconv_proposta_formalizacao_pac.csv'
# )
# 
# dados_siconv_selecao_pac <- fread(
#   'data/pac/siconv_proposta_selecao_pac.csv'
# )
# 
# dados_siconv_selecao_pac_2 <- dados_siconv_selecao_pac %>%
#   left_join(
#     dados_siconv_proposta,
#     by = c('ID_PROPOSTA_SELECAO_PAC' = 'ID_PROPOSTA')
#   )
# 
# dados_siconv_selecao_pac %>% glimpse()
# dados_siconv_selecao_pac_2 %>% glimpse()
# 
# dados_siconv_selecao_pac_2 %>%
#   write_xlsx(
#     'data/pac/dados_siconv_completo_pac.xlsx'
#   )

library(data.table)
library(dplyr)
library(googledrive)
library(transferegovr)
library(writexl)

# 1. Autenticação via JSON ----
drive_auth(path = "data/r-sheets-hugo-0788cc358cb5.json")

pasta_id <- "1OgHNEJq5p9Jb8WRi_Fwx1CvDe2qwWKYY" 

read_csv_from_drive <- function(file_name, folder_id) {
  # Busca o arquivo dentro da pasta especificada
  file_info <- drive_ls(as_id(folder_id), pattern = file_name)
  
  if(nrow(file_info) == 0) {
    stop(paste("Arquivo", file_name, "não encontrado na pasta", folder_id))
  }
  
  # Cria um arquivo temporário
  temp_file <- tempfile(fileext = ".csv")
  
  # Baixa o arquivo
  drive_download(as_id(file_info$id[1]), path = temp_file, overwrite = TRUE)
  
  # Lê com o fread
  dados <- fread(temp_file)
  return(dados)
}

# 2. Leitura (EP SICONV) ----
dados_siconv_convenio <- read_csv_from_drive('siconv_convenio.csv', pasta_id)

dados_siconv_proposta <- read_csv_from_drive('siconv_proposta.csv', pasta_id)

dados_siconv_formalizacao_pac <- read_csv_from_drive('siconv_proposta_formalizacao_pac.csv', pasta_id)

dados_siconv_selecao_pac <- read_csv_from_drive('siconv_proposta_selecao_pac.csv', pasta_id)

# 3. Transformação ----
dados_siconv_selecao_pac_2 <- dados_siconv_selecao_pac %>%
  left_join(
    dados_siconv_proposta,
    by = c('ID_PROPOSTA_SELECAO_PAC' = 'ID_PROPOSTA')
  )

# Visualizações
dados_siconv_selecao_pac %>% glimpse()
dados_siconv_selecao_pac_2 %>% glimpse()

# 4. Escrita no Google Drive ----
temp_xlsx <- tempfile(fileext = ".xlsx")
write_xlsx(dados_siconv_selecao_pac_2, temp_xlsx)

# Faz o upload do arquivo para a mesma pasta no Google Drive
drive_upload(
  media = temp_xlsx,
  path = as_id(pasta_id),
  name = "dados_siconv_completo_pac.xlsx",
  overwrite = TRUE # Sobrescreve o arquivo no Drive se ele já existir
)

# Limpa o arquivo temporário gerado localmente
unlink(temp_xlsx)
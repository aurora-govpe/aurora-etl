library(tidyverse)
library(googledrive)
library(ibger)
library(openxlsx2)
options("openxlsx2.dateFormat" = "dd/mm/yyyy")
#library(writexl) # Necessário para salvar em .xlsx


# --- Configuração ---
FOLDER_ID <- "1mdu_EQtlV4PvcXpg10XC0MXXEuNoZfnf"
AUTH_PATH <- "data/r-sheets-hugo-0788cc358cb5.json"
LOCAL_PATH <- "data/ibge"

if (!dir.exists(LOCAL_PATH)) dir.create(LOCAL_PATH, recursive = TRUE)

drive_auth(path = AUTH_PATH)

# --- Helpers ---
parse_classification <- function(class_str, cat_str) {
  cls  <- strsplit(class_str, ";")[[1]]
  cats <- strsplit(gsub('"', "", cat_str), ";")[[1]]
  setNames(
    lapply(cats, \(x) if (x == "all") "all" else as.numeric(strsplit(x, ",")[[1]])),
    cls
  )
}

# --- ETL ---
dic <- googlesheets4::read_sheet("1i29rCn2jHlCE6PxWFD79dp8HkZmAtTuF2qLbsSihUnM", col_types = "c")
safe_variables <- possibly(ibge_variables, otherwise = NULL)

# Processamento individual por linha do dicionário
walk(seq_len(nrow(dic)), \(i) {
  row <- dic[i, ]
  agg_id <- row$`ID Agregado`
  
  meta <- ibge_metadata(agg_id)
  
  df <- safe_variables(
    aggregate = agg_id,
    variable = row$`ID Variável`,
    periods = paste0(row$`Início Periodo`, "-", meta$periodicity$end), 
    localities = row$`ID Local`, 
    classification = parse_classification(row$`ID Classificação`, row$`ID Categoria`)
  )
  
  if (!is.null(df)) {
    df <- df |> mutate(value = parse_ibge_value(value))
    
    # # Nome do arquivo
    file_name <- paste0(row$Nome, " - Agregado ", agg_id, ".xlsx")
    full_path <- file.path(tempdir(), file_name)

    
    
    # # 1. Salva Localmente (.xlsx)
    dims_mat_header <- wb_dims(rows = 1, cols = seq_len(ncol(df)))
    class(df$value) <- c(df$value, "number")
    n_linhas <- nrow(df)
    wb <- wb_workbook() %>%
      wb_add_worksheet("Dados") %>%
      wb_add_data_table(x = df, banded_rows = TRUE, table_style = "TableStyleLight16") %>%
      wb_add_font(dims = dims_mat_header, name = "Arial", bold = TRUE, color = wb_color("black"), size = 11) %>%
      wb_add_cell_style(dims = dims_mat_header, horizontal = "center") %>% 
      wb_set_col_widths(cols = seq_len(ncol(df)), widths = "auto")

    wb$save(file = full_path)
    
    # 2. Sobe para o Drive
    drive_upload(
      full_path,
      path = as_id(FOLDER_ID),
      name = file_name,
      overwrite = TRUE
    )
    
    cli::cli_alert_success("Arquivo {file_name} processado e enviado.")
  } else {
    cli::cli_alert_danger("Falha ao baixar agregado {agg_id}.")
  }
}, .progress = "Processando Tabelas IBGE")

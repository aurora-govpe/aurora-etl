# Script para consolidar arquivos de UI em um único TXT
library(dplyr)
library(purrr)

# 1. Definir os caminhos dos arquivos
arquivo_raiz <- "app_aurora_novo/build_ui.R"
pasta_modulos <- "app_aurora_novo/ui_modules"

# Listar todos os arquivos .R dentro da pasta de módulos (com o caminho relativo completo)
arquivos_modulos <- list.files(
  path = pasta_modulos, 
  pattern = "\\.[Rr]$", # Busca arquivos que terminam com .R ou .r
  full.names = TRUE, 
  recursive = TRUE
)

# Combinar o arquivo da raiz com os arquivos da pasta de módulos
todos_arquivos <- c(arquivo_raiz, arquivos_modulos)

# Filtrar apenas arquivos que realmente existem (evita erros se o da raiz mudar de nome)
todos_arquivos <- todos_arquivos[file.exists(todos_arquivos)]

# 2. Criar o cabeçalho do arquivo TXT (Lista de Endereços)
cabecalho <- c(
  "=======================================================================",
  "   RELATÓRIO DE CONSOLIDAÇÃO DE DOCUMENTOS - INTERFACES (UI)",
  paste("   Gerado em:", Sys.time()),
  "=======================================================================",
  "",
  "ARQUIVOS LIDOS E SEUS ENDEREÇOS:",
  paste0("[", seq_along(todos_arquivos), "] ", todos_arquivos),
  "",
  "=======================================================================",
  "   CONTEÚDO DOS ARQUIVOS",
  "=======================================================================",
  ""
)

# 3. Ler o conteúdo de cada arquivo e preparar para o TXT
corpo_consolidado <- map(todos_arquivos, function(caminho) {
  # Ler as linhas do arquivo de forma segura
  linhas <- readLines(caminho, warn = FALSE)
  
  # Criar uma divisória visual antes do conteúdo de cada arquivo
  divisoria_inicio <- c(
    "",
    paste("-----------------------------------------------------------------------"),
    paste(" INÍCIO DO ARQUIVO:", caminho),
    paste("-----------------------------------------------------------------------"),
    ""
  )
  
  divisoria_fim <- c(
    "",
    paste(" --- FIM DO ARQUIVO:", caminho, "---"),
    ""
  )
  
  # Retorna o bloco completo desse arquivo
  return(c(divisoria_inicio, linhas, divisoria_fim))
}) %>% 
  flatten_chr() # Transforma a lista em um vetor de texto linear

# 4. Juntar o Cabeçalho com o Corpo e Gravar o arquivo final uis.txt
writeLines(
  text = c(cabecalho, corpo_consolidado), 
  con = "uis.txt", 
  useBytes = TRUE # Garante compatibilidade de acentos (UTF-8)
)

cat("Sucesso! O arquivo 'uis.txt' foi gerado com", length(todos_arquivos), "arquivos consolidados.\n")
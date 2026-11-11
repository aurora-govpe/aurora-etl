# 1. Defina o diretório onde estão os scripts e o termo que deseja buscar
diretorio <- "etl"
#termo_busca <- "fato_contratos.parquet"
termo_busca <- "fato_contratos.parquet"

# 2. Liste todos os arquivos .R no diretório (e subdiretórios, se inclusive = TRUE)
arquivos_r <- list.files(path = diretorio, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)

# 3. Varre cada arquivo procurando o termo
resultados <- sapply(arquivos_r, function(arquivo) {
  linhas <- readLines(arquivo, warn = FALSE)
  linhas_encontradas <- grep(termo_busca, linhas)
  if (length(linhas_encontradas) > 0) {
    paste0("Linha ", linhas_encontradas, ": ", linhas[linhas_encontradas])
  } else {
    NULL
  }
}, simplify = FALSE)

# 4. Remove os arquivos que não tiveram correspondência e mostra o resultado
resultados <- resultados[!sapply(resultados, is.null)]
print(resultados)

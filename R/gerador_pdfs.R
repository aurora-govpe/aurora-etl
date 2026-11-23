# Instale se necessário: install.packages(c("callr", "chromote", "fs", "jsonlite", "googledrive"))
library(callr)
library(chromote)
library(fs)
library(jsonlite)
library(googledrive)

# ==============================================================================
# CONFIGURAÇÕES GLOBAIS DE MEMÓRIA
# ==============================================================================
Sys.setenv(CHROMOTE_MAX_PAYLOAD_SIZE = "1048576000")
options(chromote.timeout = 180)

# ==============================================================================
# 1. INICIAR A API EM BACKGROUND
# ==============================================================================
caminho_api <- file.path("..", "aurora_municipios")

cat("🚀 Iniciando a API Go em background...\n")
api_process <- processx::process$new(
  command = "go",
  args = c("run", "main.go"), # troque 'main.go' pelo nome do seu arquivo principal Go
  wd = caminho_api,
  stdout = "|",
  stderr = "|"
)

cat("⏳ Aguardando a API inicializar...\n")
Sys.sleep(6)

if (!api_process$is_alive()) {
  cat("❌ Erro ao subir a API Go:\n")
  cat(api_process$read_all_error_lines(), sep = "\n")
  stop("A API falhou ao iniciar.")
}
cat("✅ API Go rodando com sucesso!\n")

# Volta para a pasta raiz
# setwd("..")

# ==============================================================================
# 2. AUTENTICAÇÃO E PREPARAÇÃO DO NAVEGADOR
# ==============================================================================
cat("🔐 Autenticando no Google Drive...\n")
drive_auth(path = "data/r-sheets-hugo-0788cc358cb5.json")
id_pasta_destino <- as_id("1FJkcgK1iomEZCJ_HnKHzewZZzChCvxe1")

Sys.setenv(CHROMOTE_CHROME_ARGS = "--enable-webgl --use-gl=swiftshader --window-size=1920,1080")
url_app <- "http://127.0.0.1:8000/"
pasta_destino <- "data/relatorios"
dir_create(pasta_destino)

cat("🤖 Iniciando o navegador Headless...\n")
b <- ChromoteSession$new()
b$Page$navigate(url_app)

Sys.sleep(5)

# --- CORREÇÃO DA NAVEGAÇÃO: Mudar para a Aba 'Municípios' ---
cat("🔄 Alterando para a aba 'Municípios'...\n")
js_trocar_aba <- "
  let btnMunicipios = Array.from(document.querySelectorAll('button')).find(btn => btn.textContent.includes('Municípios'));
  if (btnMunicipios) {
    btnMunicipios.click();
  } else if (typeof mudarAbaAurora === 'function') {
    mudarAbaAurora('Municípios', null);
  }
"
b$Runtime$evaluate(js_trocar_aba)
Sys.sleep(3) # Aguarda o carregamento e renderização do select de municípios

# Extrai a lista de municípios
js_get_muns <- "Array.from(document.querySelectorAll('#ficha_f_municipio option')).map(opt => opt.value).filter(v => v && v.trim() !== '');"
municipios <- b$Runtime$evaluate(js_get_muns, returnByValue = TRUE)$result$value

if (length(municipios) == 0) {
  api_process$kill()
  stop("Nenhum município encontrado. Verifique se o app conectou à API ou se a aba Municípios carregou corretamente.")
}

cat(sprintf("✅ %d municípios encontrados!\n", length(municipios)))

# ==============================================================================
# 3. DIVISÃO DOS LOTES
# ==============================================================================
muns_gerais <- municipios[municipios != "Recife"]
mun_recife <- "Recife"

# ==============================================================================
# 4. LOOP 1: TODOS OS MUNICÍPIOS (EXCETO RECIFE)
# ==============================================================================
cat("\n======================================================\n")
cat("🚀 INICIANDO LOTE 1: TODOS OS MUNICÍPIOS (EXCETO RECIFE)\n")
cat("======================================================\n")

for (mun in muns_gerais) {
  cat(sprintf("\n➡️ Processando LOTE 1: %s...\n", mun))
  
  js_change <- sprintf("
    // Atualiza o select do Bootstrap se existir, ou o select normal
    if ($('#ficha_f_municipio').data('selectpicker')) {
      $('#ficha_f_municipio').selectpicker('val', '%s').trigger('change');
    } else {
      $('#ficha_f_municipio').val('%s').trigger('change');
    }
    
    if (typeof dtFichaIniciativas !== 'undefined' && dtFichaIniciativas) dtFichaIniciativas.page.len(-1).draw(false);
    if (typeof dtFichaConvenios !== 'undefined' && dtFichaConvenios) dtFichaConvenios.page.len(-1).draw(false);
    if (typeof dtFichaEmendas !== 'undefined' && dtFichaEmendas) dtFichaEmendas.page.len(-1).draw(false);
    
    setTimeout(() => {
        ['ficha_chart_iniciativas_sec', 'ficha_chart_valor_sec', 'ficha_chart_eme_parlamentares', 'ficha_chart_eme_partidos', 'ficha_chart_eme_temas'].forEach(id => {
            let dom = document.getElementById(id);
            if(dom) { let chart = echarts.getInstanceByDom(dom); if(chart) chart.resize(); }
        });
    }, 1500);
  ", mun, mun)
  
  b$Runtime$evaluate(js_change)
  
  Sys.sleep(8) 
  
  pdf_data <- b$Page$printToPDF(landscape = TRUE, printBackground = TRUE, timeout_ = 180)
  nome_arquivo <- paste0(mun, ".pdf")
  caminho_arquivo <- path(pasta_destino, nome_arquivo)
  writeBin(jsonlite::base64_dec(pdf_data$data), caminho_arquivo)
  
  cat(sprintf("   ☁️ Fazendo upload de %s para o Drive...\n", nome_arquivo))
  tryCatch({
    drive_upload(media = caminho_arquivo, path = id_pasta_destino, name = nome_arquivo, overwrite = TRUE)
    cat("   ✅ Upload concluído!\n")
  }, error = function(e) {
    cat("   ❌ Erro ao fazer upload:", e$message, "\n")
  })
}

# ==============================================================================
# 5. LOOP 2: APENAS RECIFE (DIVIDIDO EM 2 PARTES PARA NÃO ESTOURAR A MEMÓRIA)
# ==============================================================================
if ("Recife" %in% municipios) {
  cat("\n======================================================\n")
  cat("🚀 INICIANDO LOTE 2: RECIFE (OPERAÇÃO CIRÚRGICA)\n")
  cat("======================================================\n")
  
  # ---------------------------------------------------------
  # RECIFE - PARTE 1: Resumo e Iniciativas
  # ---------------------------------------------------------
  cat("\n➡️ Processando Recife (Parte 1: Resumo e Iniciativas)...\n")
  
  b$Page$navigate(url_app)
  Sys.sleep(5)
  
  # Garante troca de aba após recarregar
  b$Runtime$evaluate(js_trocar_aba)
  Sys.sleep(3)
  
  js_parte1 <- "
    if ($('#ficha_f_municipio').data('selectpicker')) {
      $('#ficha_f_municipio').selectpicker('val', 'Recife').trigger('change');
    } else {
      $('#ficha_f_municipio').val('Recife').trigger('change');
    }
    
    setTimeout(() => {
        // Oculta Convênios e Emendas cirurgicamente
        $('#sec_emendas_parlamentares').hide().nextAll().hide();
        $('#ficha_tabela_convenios').closest('.bg-white').hide();
        $('#ficha_tabela_convenios').closest('.bg-white').prev('.grid').hide();
        $('h5:contains(\"Convênios de Despesa\")').hide();
        
        // Expande apenas a tabela de Iniciativas
        if (typeof dtFichaIniciativas !== 'undefined' && dtFichaIniciativas) dtFichaIniciativas.page.len(-1).draw(false);
        
        // Ajusta gráficos visíveis
        ['ficha_chart_iniciativas_sec', 'ficha_chart_valor_sec'].forEach(id => {
            let dom = document.getElementById(id);
            if(dom) { let chart = echarts.getInstanceByDom(dom); if(chart) chart.resize(); }
        });
    }, 2000);
  "
  b$Runtime$evaluate(js_parte1)
  Sys.sleep(10) 
  
  tryCatch({
    pdf_data1 <- b$Page$printToPDF(landscape = TRUE, printBackground = TRUE, timeout_ = 180)
    writeBin(jsonlite::base64_dec(pdf_data1$data), path(pasta_destino, "Recife_01_Resumo_Iniciativas.pdf"))
    cat("   ☁️ Fazendo upload de Recife Parte 1...\n")
    drive_upload(media = path(pasta_destino, "Recife_01_Resumo_Iniciativas.pdf"), path = id_pasta_destino, name = "Recife_01_Resumo_Iniciativas.pdf", overwrite = TRUE)
    cat("   ✅ Recife Parte 1 concluído!\n")
  }, error = function(e) { cat("   ❌ Erro em Recife Parte 1:", e$message, "\n") })
  
  # ---------------------------------------------------------
  # RECIFE - PARTE 2: Convênios e Emendas
  # ---------------------------------------------------------
  cat("\n➡️ Processando Recife (Parte 2: Convênios e Emendas)...\n")
  
  b$Page$navigate(url_app) 
  Sys.sleep(5)
  
  # Garante troca de aba após recarregar
  b$Runtime$evaluate(js_trocar_aba)
  Sys.sleep(3)
  
  js_parte2 <- "
    if ($('#ficha_f_municipio').data('selectpicker')) {
      $('#ficha_f_municipio').selectpicker('val', 'Recife').trigger('change');
    } else {
      $('#ficha_f_municipio').val('Recife').trigger('change');
    }
    
    setTimeout(() => {
        // Esconde tudo que está entre a Capa e a Seção de Iniciativas Regionais
        $('#sec_caracterizacao').hide();
        $('#sec_caracterizacao').nextUntil('#sec_iniciativas_regionais').hide();
        
        // Oculta a tabela e os gráficos de Iniciativas (deixando Convênios e Emendas)
        $('#ficha_tabela_iniciativas').closest('.bg-white').hide();
        $('#ficha_tabela_iniciativas').closest('.bg-white').prev('.row').hide();
        $('#ficha_tabela_iniciativas').closest('.bg-white').prev('.row').prev('.grid').hide();
        $('h5:contains(\"Ações e Programas Governamentais\")').hide();
        
        // Expande as tabelas de Convênios e Emendas
        if (typeof dtFichaConvenios !== 'undefined' && dtFichaConvenios) dtFichaConvenios.page.len(-1).draw(false);
        if (typeof dtFichaEmendas !== 'undefined' && dtFichaEmendas) dtFichaEmendas.page.len(-1).draw(false);
        
        // Ajusta gráficos visíveis
        ['ficha_chart_eme_parlamentares', 'ficha_chart_eme_partidos', 'ficha_chart_eme_temas'].forEach(id => {
            let dom = document.getElementById(id);
            if(dom) { let chart = echarts.getInstanceByDom(dom); if(chart) chart.resize(); }
        });
    }, 2000);
  "
  b$Runtime$evaluate(js_parte2)
  Sys.sleep(10)
  
  tryCatch({
    pdf_data2 <- b$Page$printToPDF(landscape = TRUE, printBackground = TRUE, timeout_ = 180)
    writeBin(jsonlite::base64_dec(pdf_data2$data), path(pasta_destino, "Recife_02_Convenios_Emendas.pdf"))
    cat("   ☁️ Fazendo upload de Recife Parte 2...\n")
    drive_upload(media = path(pasta_destino, "Recife_02_Convenios_Emendas.pdf"), path = id_pasta_destino, name = "Recife_02_Convenios_Emendas.pdf", overwrite = TRUE)
    cat("   ✅ Recife Parte 2 concluído!\n")
  }, error = function(e) { cat("   ❌ Erro em Recife Parte 2:", e$message, "\n") })
}
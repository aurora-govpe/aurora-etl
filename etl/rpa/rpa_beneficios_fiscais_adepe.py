import asyncio
import datetime
import os
import json
import traceback
from pathlib import Path
import pandas as pd
from bs4 import BeautifulSoup
from playwright.async_api import async_playwright

# =============================================================================
# MAPEAMENTO DE ENTRADA, SAÍDA E CONFIGURAÇÕES
# =============================================================================
URL_ARTIFACT = "https://claude.ai/artifact/7i1M7CEhGxPPZo16GqJEta"

# 1. Diretório para Conferência Humana (Excel Raw)
DIR_RAW = Path(r"C:\dados\dados brutos\aurora\incentivos")
DIR_RAW.mkdir(parents=True, exist_ok=True)
ARQUIVO_XLSX = DIR_RAW / "beneficios_fiscais_pe.xlsx"

# 2. Diretório Oficial para a Pipeline ETL (JSON e Parquet)
DIR_ETL = Path(r"C:\dados\aurora")
DIR_ETL.mkdir(parents=True, exist_ok=True)
ARQUIVO_JSON = DIR_ETL / "beneficios_fiscais_pe.json"

DIR_PARQUET = DIR_ETL / "parquet"
DIR_PARQUET.mkdir(parents=True, exist_ok=True)

LOG_FILE = DIR_ETL / "log.txt"
CARACTERES_INVALIDOS = set('[]:*?/\\')

def log(mensagem):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")

def extrair_tabelas_do_html(html_str, contexto=""):
    """Varre um bloco de HTML e extrai todas as tabelas encontradas."""
    soup = BeautifulSoup(html_str, "html.parser")
    tabelas = soup.find_all("table")
    tabelas_processadas = []

    for idx, table in enumerate(tabelas, start=1):
        cabecalhos = []
        thead = table.find("thead")
        if thead:
            cabecalhos = [th.get_text(" ", strip=True) for th in thead.find_all(["th", "td"])]

        linhas = []
        tbody = table.find("tbody") or table
        for tr in tbody.find_all("tr"):
            if tr.find_parent("thead"):
                continue
            celulas = [td.get_text(" ", strip=True) for td in tr.find_all(["td", "th"])]
            if celulas:
                linhas.append(celulas)

        if not linhas and not cabecalhos:
            continue

        df = pd.DataFrame(linhas)
        if cabecalhos and len(cabecalhos) == df.shape[1]:
            # Para o Parquet, os nomes das colunas devem ser todas strings para evitar erros
            df.columns = [str(c) for c in cabecalhos]
        else:
            df.columns = [f"Coluna_{i+1}" for i in range(df.shape[1])]

        df.dropna(how="all", inplace=True)
        if df.empty:
            continue

        titulo = None
        caption = table.find("caption")
        if caption:
            titulo = caption.get_text(strip=True)
        else:
            tag_ant = table.find_previous(["h1", "h2", "h3", "h4", "h5", "p", "strong"])
            if tag_ant and len(tag_ant.get_text(strip=True)) <= 40:
                titulo = tag_ant.get_text(strip=True)

        nome_base = titulo or f"Tabela_{idx}"
        tabelas_processadas.append({"nome": nome_base, "df": df})

    return tabelas_processadas

async def executar_rpa_beneficios():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA EXTRAÇÃO - MAPA DOS BENEFÍCIOS FISCAIS DE PE ===\n")
        f.write(f"URL Alvo: {URL_ARTIFACT}\n")
        f.write(f"Conferência (XLSX): {ARQUIVO_XLSX}\n")
        f.write(f"ETL Analítico (Parquet): {DIR_PARQUET}\n")

    async with async_playwright() as p:
        try:
            log("1. Inicializando navegador Chromium...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(viewport={"width": 1600, "height": 900})
            page = await context.new_page()

            log(f"2. Acessando URL do Artifact: {URL_ARTIFACT}")
            await page.goto(URL_ARTIFACT, wait_until="domcontentloaded", timeout=60000)

            log("3. Aguardando montagem do container e do iframe do Artifact...")
            await page.wait_for_selector("iframe#frame-content, iframe[src*='claudeusercontent.com'], iframe", timeout=45000)
            await page.wait_for_timeout(4000)

            log("4. Inspecionando frames e extraindo tabelas...")
            todas_tabelas = []

            for idx_f, frame in enumerate(page.frames):
                try:
                    conteudo = await frame.content()
                    tabs = extrair_tabelas_do_html(conteudo, contexto=f"Frame_{idx_f}")
                    if tabs:
                        log(f"   📊 {len(tabs)} tabela(s) encontrada(s) no Frame {idx_f} ({frame.url[:60]}...)")
                        todas_tabelas.extend(tabs)
                except Exception as ef:
                    log(f"   ⚠️ Não foi possível ler o Frame {idx_f}: {ef}")

            if not todas_tabelas:
                log("5. Aguardando aparição da tag <table> no iframe por mais 15s...")
                try:
                    frame_target = page.frame_locator("iframe#frame-content, iframe").first
                    await frame_target.locator("table").first.wait_for(timeout=15000)
                    for frame in page.frames:
                        conteudo = await frame.content()
                        tabs = extrair_tabelas_do_html(conteudo)
                        if tabs:
                            todas_tabelas.extend(tabs)
                            break
                except Exception:
                    pass

            if not todas_tabelas:
                log("❌ Nenhuma tabela foi encontrada no Artifact.")
                await browser.close()
                return

            log(f"6. Consolidando e exportando {len(todas_tabelas)} tabela(s) nos formatos alvo...")
            
            dicionario_json = {}
            
            with pd.ExcelWriter(ARQUIVO_XLSX, engine="openpyxl") as writer:
                nomes_usados = set()

                for idx, item in enumerate(todas_tabelas, start=1):
                    df_tab = item["df"]
                    nome_base = item["nome"]

                    # Limpeza rigorosa do nome para não quebrar Abas e Nomes de Ficheiros
                    nome_limpo = "".join(c for c in nome_base if c not in CARACTERES_INVALIDOS)[:25].strip()
                    nome_limpo = nome_limpo.replace(" ", "_") # Troca espaços por underscore para os arquivos parquet
                    if not nome_limpo:
                        nome_limpo = f"tabela_{idx}"

                    nome_aba = nome_limpo
                    contador = 2
                    while nome_aba.lower() in nomes_usados:
                        nome_aba = f"{nome_limpo[:22]}_{contador}"
                        contador += 1
                    nomes_usados.add(nome_aba.lower())

                    # A. Exportar para Excel (Conferência Humana)
                    df_tab.to_excel(writer, sheet_name=nome_aba, index=False)
                    
                    # B. Exportar para Parquet (Data Lake / Aurora Prime)
                    arquivo_parquet = DIR_PARQUET / f"beneficios_fiscais_pe_{nome_aba}.parquet"
                    df_tab.to_parquet(arquivo_parquet, index=False)
                    
                    # C. Adicionar ao Dicionário Global JSON
                    dicionario_json[nome_aba] = df_tab.to_dict(orient="records")
                    
                    log(f"   ✅ Tabela processada: '{nome_aba}' -> XLSX / Parquet ({len(df_tab)} linhas)")

            # Gravar o JSON final unificado
            with open(ARQUIVO_JSON, "w", encoding="utf-8") as f:
                json.dump(dicionario_json, f, ensure_ascii=False, indent=2)
                log(f"   ✅ Arquivo JSON unificado gerado.")

            log("==================================================")
            log("🎉 EXTRAÇÃO E PIPELINE MULTI-FORMATO CONCLUÍDOS!")
            log(f"📁 Raw/Conferência : {ARQUIVO_XLSX}")
            log(f"📁 ETL (Parquet)   : {DIR_PARQUET}\\")
            log(f"📁 ETL (JSON)      : {ARQUIVO_JSON}")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"💥 [ERRO CRÍTICO]: {str(e)}")
            log(traceback.format_exc())

if __name__ == "__main__":
    asyncio.run(executar_rpa_beneficios())

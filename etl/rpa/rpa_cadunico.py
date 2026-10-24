import asyncio
import datetime
import os
import re
import traceback
from pathlib import Path
import pandas as pd
from playwright.async_api import async_playwright

# =============================================================================
# MAPEAMENTO DINÂMICO DE DIRETÓRIOS DO PROJETO AURORAPRIME
# =============================================================================
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
DATA_DIR = PROJECT_ROOT / "data" / "cadunico"
DATA_DIR.mkdir(parents=True, exist_ok=True)

URL_INICIAL = "https://paineis.mds.gov.br/public/extensions/observatorio-do-cadastro-unico/index.html"
LOG_FILE = DATA_DIR / "log.txt"

MESES_MAP = {
    "janeiro": "01", "fevereiro": "02", "março": "03", "marco": "03",
    "abril": "04", "maio": "05", "junho": "06", "julho": "07",
    "agosto": "08", "setembro": "09", "outubro": "10",
    "novembro": "11", "dezembro": "12"
}

def log(mensagem):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")

async def extrair_cadunico_municipios():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA EXTRAÇÃO OBSERVATÓRIO CADASTRO ÚNICO (MDS) ===\n")

    async with async_playwright() as p:
        try:
            log("1. Ligando navegador com motor Firefox...")
            browser = await p.firefox.launch(
                headless=False,
                firefox_user_prefs={
                    "network.websocket.timeout.open": 180,
                    "dom.webdriver.enabled": False
                }
            )
            
            context = await browser.new_context(
                accept_downloads=True,
                viewport={"width": 1600, "height": 900}
            )
            page = await context.new_page()

            log(f"2. Acessando página inicial: {URL_INICIAL}")
            await page.goto(URL_INICIAL, wait_until="commit", timeout=120000)

            # =========================================================================
            # PASSO 1: ACEITE DO TERMO LGPD
            # =========================================================================
            log("3. Monitorando modal de privacidade (LGPD)...")
            btn_prosseguir = page.locator("button").filter(has_text="Prosseguir").filter(has_not_text="Desativar").first
            try:
                await btn_prosseguir.wait_for(state="visible", timeout=60000)
                log("   🔘 Modal LGPD localizado. Clicando em 'Prosseguir'...")
                await btn_prosseguir.click(force=True)
                await page.wait_for_timeout(2000)
            except Exception:
                log("   ℹ️ Modal não apareceu ou já foi fechado.")

            # =========================================================================
            # PASSO 2: ESPERAR CARREGAMENTO INICIAL E CAPTURAR MÊS BASE
            # =========================================================================
            overlay_loader = page.get_by_text("Estamos preparando tudo para você", exact=False).first
            try:
                if await overlay_loader.is_visible(timeout=5000):
                    log("   ⏳ Aguardando término do carregamento da tela inicial...")
                    await overlay_loader.wait_for(state="hidden", timeout=120000)
                    log("   ✅ Tela inicial carregada!")
            except Exception:
                pass

            log("4. Identificando mês base de referência do CadÚnico...")
            texto_pagina = await page.evaluate("() => document.body.innerText")
            match_ref = re.search(r"Referência:\s*([a-zA-ZçÇãÃõÕ]+)\s+de\s+(\d{4})", texto_pagina, re.IGNORECASE)

            if match_ref:
                mes_extenso = match_ref.group(1).lower()
                ano_ref = match_ref.group(2)
                mes_num = MESES_MAP.get(mes_extenso, "00")
                sufixo_referencia = f"{ano_ref}_{mes_num}"
                log(f"   📅 Referência identificada: {match_ref.group(1).capitalize()} de {ano_ref} -> sufixo: {sufixo_referencia}")
            else:
                sufixo_referencia = datetime.datetime.now().strftime("%Y_%m")
                log(f"   ⚠️ Referência textual não identificada no cabeçalho. Utilizando padrão: {sufixo_referencia}")

            arquivo_final = DATA_DIR / f"familias_pessoas_municipios_cadunico_{sufixo_referencia}.xlsx"

            # =========================================================================
            # PASSO 3: CLICAR NA ABA 'TABELA' DO MENU SUPERIOR (VIA JAVASCRIPT)
            # =========================================================================
            log("5. Acionando a aba 'Tabela' no menu superior...")
            clique_aba_tabela = """
            () => {
                let aba = document.querySelector("a[data-target='tabela_familias_pessoas']");
                if (!aba) {
                    let elementos = Array.from(document.querySelectorAll('a, button, li, span'));
                    aba = elementos.find(el => el.innerText && el.innerText.trim() === 'Tabela');
                }
                if (aba) {
                    aba.click();
                    return true;
                }
                return false;
            }
            """
            sucesso_clique_aba = await page.evaluate(clique_aba_tabela)
            if sucesso_clique_aba:
                log("   🔘 Comando de clique na aba 'Tabela' enviado com sucesso.")
            else:
                log("   ⚠️ Aba 'Tabela' não encontrada no DOM.")

            # =========================================================================
            # PASSO 4: AGUARDAR O CARREGAMENTO DA SEÇÃO DE TABELAS
            # =========================================================================
            log("6. Aguardando a tela de carga 'Estamos preparando tudo para você' finalizar...")
            await page.wait_for_timeout(2000)
            try:
                if await overlay_loader.is_visible(timeout=5000):
                    await overlay_loader.wait_for(state="hidden", timeout=180000)
                    log("   ✅ Dados da tela de tabelas liberados pelo Qlik!")
            except Exception:
                pass

            # =========================================================================
            # PASSO 5: CLICAR NO BOTÃO 'EXPORTAR' VISÍVEL DOS MUNICÍPIOS
            # =========================================================================
            log("7. Aguardando renderização do card 'Famílias e pessoas por município'...")
            titulo_mun = page.get_by_text("Famílias e pessoas por município", exact=False).first
            await titulo_mun.wait_for(state="visible", timeout=60000)
            await page.wait_for_timeout(3000)

            log("8. Clicando no botão 'EXPORTAR' visível da tabela de municípios...")
            js_clique_visivel = """
            () => {
                let botoesVisiveis = Array.from(document.querySelectorAll('button, a')).filter(el => {
                    let rect = el.getBoundingClientRect();
                    let visivel = (el.offsetParent !== null) && 
                                  rect.width > 0 && 
                                  rect.height > 0 && 
                                  window.getComputedStyle(el).display !== 'none' && 
                                  window.getComputedStyle(el).visibility !== 'hidden';
                    return visivel && el.innerText.trim().includes('EXPORTAR');
                });
                
                if (botoesVisiveis.length > 0) {
                    let btnAlvo = botoesVisiveis[botoesVisiveis.length - 1];
                    btnAlvo.click();
                    return { ok: true, id: btnAlvo.id || 'sem_id', total: botoesVisiveis.length };
                }
                return { ok: false, total: 0 };
            }
            """

            async with page.expect_download(timeout=180000) as download_info:
                resultado = await page.evaluate(js_clique_visivel)
                log(f"   Status do clique: {resultado}")
                
                try:
                    modal_link = page.get_by_text("Clique aqui para baixar", exact=False).first
                    if await modal_link.is_visible(timeout=6000):
                        log("   🔘 Modal de confirmação detectado, confirmando download...")
                        await modal_link.click(force=True)
                except Exception:
                    pass

            download = await download_info.value
            nome_original = download.suggested_filename
            caminho_bruto = DATA_DIR / nome_original
            await download.save_as(str(caminho_bruto))
            log(f"   📥 Arquivo baixado com sucesso: {caminho_bruto}")

            # =========================================================================
            # PASSO 6: PADRONIZAÇÃO, ADIÇÃO DE METADADO E GRAVAÇÃO COM PANDAS
            # =========================================================================
            log("9. Formatando os dados com Pandas...")
            if str(caminho_bruto).endswith(".csv"):
                df = pd.read_csv(caminho_bruto, sep=None, engine="python", encoding="utf-8-sig")
            else:
                df = pd.read_excel(caminho_bruto)

            df.columns = [str(c).strip() for c in df.columns]
            
            # Formatação do Código IBGE para garantir 7 dígitos
            col_ibge = next((c for c in df.columns if "ibge" in c.lower() or "cód" in c.lower()), None)
            if col_ibge:
                df[col_ibge] = df[col_ibge].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(7)
                log(f"   🔢 Código IBGE padronizado na coluna: '{col_ibge}'")

            # Adiciona a coluna de referência do mês no próprio DataFrame
            df["Referência CadÚnico"] = f"{mes_extenso.capitalize()}/{ano_ref}" if match_ref else sufixo_referencia

            df.to_excel(arquivo_final, index=False, engine="openpyxl")
            log(f"📁 Arquivo final gerado com sucesso ({len(df)} municípios): {arquivo_final}")

            # Remove o arquivo temporário baixado originalmente pelo Qlik
            if caminho_bruto.exists() and caminho_bruto != arquivo_final:
                try:
                    caminho_bruto.unlink()
                except Exception:
                    pass

            log("==================================================")
            log("🎉 EXTRAÇÃO DO CADÚNICO CONCLUÍDA COM SUCESSO!")
            log(f"📊 Arquivo salvo: {arquivo_final.name}")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"💥 [ERRO CRÍTICO]: {str(e)}")
            log(traceback.format_exc())

if __name__ == "__main__":
    asyncio.run(extrair_cadunico_municipios())

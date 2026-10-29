import asyncio
import datetime
import json
import re
import traceback
from pathlib import Path
import pandas as pd
from playwright.async_api import async_playwright

# =============================================================================
# MAPEAMENTO DE ENTRADA, SAÍDA E CONFIGURAÇÕES
# =============================================================================
URL_PPCOF = "https://ppcof.sas.pe.gov.br/"
ANOS_COMPETENCIA = [2024, 2025, 2026]

# 1. Diretório Bruto (arquivos por ano e consolidado para conferência)
DIR_RAW = Path(r"C:\dados\dados brutos\aurora\ppcof")
DIR_RAW.mkdir(parents=True, exist_ok=True)
ARQUIVO_RAW_CONSOLIDADO = DIR_RAW / "ppcof_consolidado_2024_2026.xlsx"

# 2. Diretório Analítico Oficial (JSON e Parquet)
DIR_ETL = Path(r"C:\dados\aurora")
DIR_ETL.mkdir(parents=True, exist_ok=True)
ARQUIVO_JSON = DIR_ETL / "ppcof_co_financiamento.json"

DIR_PARQUET = DIR_ETL / "parquet"
DIR_PARQUET.mkdir(parents=True, exist_ok=True)
ARQUIVO_PARQUET = DIR_PARQUET / "ppcof_co_financiamento.parquet"

LOG_FILE = DIR_ETL / "log_ppcof.txt"

def log(mensagem):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    linha_segura = linha.encode("utf-8", errors="replace").decode("utf-8", errors="replace")
    try:
        print(linha_segura)
    except Exception:
        print(linha_segura.encode("ascii", errors="replace").decode("ascii"))
    try:
        with open(LOG_FILE, "a", encoding="utf-8", errors="replace") as f:
            f.write(linha_segura + "\n")
    except Exception:
        pass

def limpar_valor_moeda(val):
    """Converte 'R$ 96.000,00' -> 96000.00"""
    if pd.isna(val) or val is None:
        return 0.0
    val_str = str(val).strip()
    val_str = re.sub(r"[R$\s]", "", val_str)
    val_str = val_str.replace(".", "").replace(",", ".")
    try:
        return float(val_str)
    except ValueError:
        return 0.0

def limpar_percentual(val):
    """Converte '75%' ou '75,5%' -> 75.0 ou 75.5"""
    if pd.isna(val) or val is None:
        return None
    val_str = str(val).strip().replace("%", "").replace(",", ".")
    try:
        return float(val_str)
    except ValueError:
        return None

def mapear_coluna(col_name):
    """Identifica o significado semântico da coluna preservando distinção entre R$ e %."""
    c_orig = str(col_name).lower().strip()
    c_clean = re.sub(r"[áàãâ]", "a", c_orig)
    c_clean = re.sub(r"[éê]", "e", c_clean)
    c_clean = re.sub(r"[íî]", "i", c_clean)
    c_clean = re.sub(r"[óôõ]", "o", c_clean)
    c_clean = re.sub(r"[úû]", "u", c_clean)
    c_clean = re.sub(r"[ç]", "c", c_clean)
    
    if "municipio" in c_clean:
        return "municipio"
    if "esfera" in c_clean:
        return "esfera"
    if "execucao" in c_clean:
        return "tipo_execucao"
    if "recurso" in c_clean:
        return "tipo_recurso"
    if "validado" in c_clean:
        return "valor_validado"
    if "a_pagar" in c_clean or "a pagar" in c_clean or "apagar" in c_clean:
        return "valor_a_pagar"
    if "%" in c_orig or "percent" in c_clean or "perc" in c_clean:
        return "percentual_pago"
    if "parcela" in c_clean:
        return "parcelas_pagas"
    if "pago" in c_clean:
        return "valor_pago"
    return re.sub(r"[^a-z0-9]", "_", c_clean).strip("_")

async def selecionar_ano_competencia(page, ano: int):
    """Localiza o container de Ano Competência e seleciona o ano desejado."""
    log(f"   Alterando 'Ano competencia' para: {ano}...")

    js_abrir_dropdown = """
    () => {
        const todos = Array.from(document.querySelectorAll('*'));
        const label = todos.find(el => {
            const t = (el.innerText || el.textContent || '').trim().toLowerCase();
            return t.includes('ano competência') && el.children.length === 0;
        }) || todos.find(el => {
            const t = (el.innerText || el.textContent || '').trim().toLowerCase();
            return t.includes('ano competência') && t.length < 25;
        });

        if (!label) return { sucesso: false, erro: "Label 'Ano competência' nao localizada." };

        let parent = label.parentElement;
        let controle = null;

        for (let i = 0; i < 3; i++) {
            if (!parent) break;
            const candidatos = Array.from(parent.querySelectorAll('.p-dropdown, select, [role="combobox"], [class*="select"], input, button, div[tabindex]'));
            controle = candidatos.find(c => c !== label && !c.contains(label));
            if (controle) break;
            parent = parent.parentElement;
        }

        if (!controle) {
            const possivelAnoAtual = todos.find(el => {
                const t = (el.innerText || el.textContent || '').trim();
                return /^202[0-9]$/.test(t) && el.children.length === 0;
            });
            if (possivelAnoAtual) {
                controle = possivelAnoAtual.closest('.p-dropdown, [role="combobox"], div') || possivelAnoAtual;
            }
        }

        if (!controle) return { sucesso: false, erro: "Controle visual de selecao nao localizado." };

        if (controle.tagName.toLowerCase() === 'select') {
            return { sucesso: true, tipo: 'select_nativo' };
        }

        controle.click();
        return { sucesso: true, tipo: 'custom_dropdown' };
    }
    """

    res_abertura = await page.evaluate(js_abrir_dropdown)
    if not res_abertura.get("sucesso"):
        raise Exception(f"Erro ao abrir dropdown de Ano: {res_abertura.get('erro')}")

    if res_abertura.get("tipo") == "select_nativo":
        await page.evaluate(f"""() => {{
            const selects = Array.from(document.querySelectorAll('select'));
            for (const s of selects) {{
                const opts = Array.from(s.options).map(o => o.text.trim());
                if (opts.includes('{ano}')) {{
                    s.value = Array.from(s.options).find(o => o.text.trim() === '{ano}').value;
                    s.dispatchEvent(new Event('change', {{ bubbles: true }}));
                    s.dispatchEvent(new Event('input', {{ bubbles: true }}));
                }}
            }}
        }}""")
        await page.wait_for_timeout(1000)
        return

    await page.wait_for_timeout(1000)

    js_clicar_opcao = f"""
    () => {{
        const itens = Array.from(document.querySelectorAll('.p-dropdown-item, [role="option"], li, .v-list-item, div[class*="item"]'));
        const visiveis = itens.filter(el => {{
            const rect = el.getBoundingClientRect();
            return rect.width > 0 && rect.height > 0 && window.getComputedStyle(el).display !== 'none';
        }});

        const opcaoAlvo = visiveis.find(el => (el.innerText || el.textContent || '').trim() === '{ano}');
        if (opcaoAlvo) {{
            opcaoAlvo.click();
            return {{ sucesso: true }};
        }}

        const todosVisiveis = Array.from(document.querySelectorAll('body *')).filter(el => {{
            const rect = el.getBoundingClientRect();
            return rect.width > 0 && rect.height > 0 && 
                   (el.innerText || el.textContent || '').trim() === '{ano}' &&
                   el.children.length === 0;
        }});

        if (todosVisiveis.length > 0) {{
            todosVisiveis[todosVisiveis.length - 1].click();
            return {{ sucesso: true, modo: 'fallback' }};
        }}

        return {{ sucesso: false }};
    }}
    """

    res_opcao = await page.evaluate(js_clicar_opcao)
    if not res_opcao.get("sucesso"):
        loc = page.locator(".p-dropdown-item, [role='option'], li").filter(has_text=re.compile(rf"^\s*{ano}\s*$")).last
        if await loc.is_visible(timeout=3000):
            await loc.click(force=True)
            log(f"   [OK] Opcao {ano} selecionada via locator.")
        else:
            raise Exception(f"Ano {ano} nao localizado na lista suspensa.")
    else:
        log(f"   [OK] Opcao {ano} selecionada com sucesso.")

    await page.wait_for_timeout(1000)

async def raspar_tabela_dom(page):
    """Fallback visual caso o download falhe."""
    linhas = []
    headers = [
        "municipio", "esfera", "tipo_execucao", "tipo_recurso",
        "valor_validado", "valor_pago", "valor_a_pagar", "percentual_pago", "parcelas_pagas"
    ]
    trs = page.locator("table tbody tr")
    count = await trs.count()
    for i in range(count):
        tds = trs.nth(i).locator("td")
        td_count = await tds.count()
        if td_count >= 9:
            offset = 1 if td_count > 9 else 0
            dados_linha = []
            for j in range(offset, offset + 9):
                txt = await tds.nth(j).inner_text()
                dados_linha.append(txt.strip())
            linhas.append(dados_linha)
    return pd.DataFrame(linhas, columns=headers)

async def executar_rpa_ppcof():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INICIO DA EXTRACAO CONSOLIDADA - PPCOF SAS/PE ===\n")
        f.write(f"Anos Alvo: {ANOS_COMPETENCIA}\n")

    dfs_anos = []

    async with async_playwright() as p:
        try:
            log("1. Ligando navegador Chromium...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(
                viewport={"width": 1600, "height": 900},
                accept_downloads=True
            )
            page = await context.new_page()

            log(f"2. Acessando painel: {URL_PPCOF}")
            await page.goto(URL_PPCOF, wait_until="networkidle", timeout=120000)
            await page.wait_for_selector("button:has-text('Buscar')", timeout=45000)
            await page.wait_for_timeout(3000)

            for ano in ANOS_COMPETENCIA:
                log("--------------------------------------------------")
                log(f"[EXEC] Processando Competencia: {ano}")
                log("--------------------------------------------------")

                await selecionar_ano_competencia(page, ano)

                btn_buscar = page.locator("button:has-text('Buscar')").first
                await btn_buscar.click(force=True)
                await page.wait_for_timeout(4000)

                log(f"   Solicitando arquivo Excel para o ano {ano}...")
                btn_excel = page.locator("button:has-text('Excel'), a:has-text('Excel')").first

                arquivo_ano_raw = DIR_RAW / f"ppcof_bruto_{ano}.xlsx"
                df_ano = None
                download_concluido = False

                try:
                    async with page.expect_download(timeout=90000) as download_info:
                        await btn_excel.click(force=True)
                    
                    download = await download_info.value
                    await download.save_as(str(arquivo_ano_raw))
                    download_concluido = True
                except Exception as ed:
                    log(f"   [AVISO] Download via botao falhou ({ed}). Tentando extrair do DOM...")

                # Leitura da planilha baixada
                if download_concluido and arquivo_ano_raw.exists():
                    log(f"   [DOWNLOAD] Arquivo salvo: {arquivo_ano_raw.name}")
                    try:
                        df_ano = pd.read_excel(arquivo_ano_raw)
                    except Exception:
                        try:
                            df_ano = pd.read_csv(arquivo_ano_raw, sep=None, engine="python", encoding="utf-8-sig")
                        except Exception:
                            tabs = pd.read_html(arquivo_ano_raw)
                            df_ano = tabs[0] if tabs else None
                else:
                    df_ano = await raspar_tabela_dom(page)
                    df_ano.to_excel(arquivo_ano_raw, index=False, engine="openpyxl")

                # Inserção de metadados
                if df_ano is not None and not df_ano.empty:
                    df_ano.columns = [mapear_coluna(c) for c in df_ano.columns]
                    df_ano.insert(0, "ano_competencia", int(ano))
                    dfs_anos.append(df_ano)
                    log(f"   [OK] {len(df_ano)} registros carregados para {ano}.")
                else:
                    log(f"   [ALERTA] Nenhum registro retornado para o ano {ano}.")

            await browser.close()

            if not dfs_anos:
                log("[ERRO CRITICO] Nenhuma base foi capturada.")
                return

            # =========================================================================
            # CONSOLIDAÇÃO E LIMPEZA ANALÍTICA (2024 + 2025 + 2026)
            # =========================================================================
            log("--------------------------------------------------")
            log("3. Consolidando e padronizando todas as competencias...")
            df_final = pd.concat(dfs_anos, ignore_index=True)

            # Normalização de tipos numéricos
            for col_moeda in ["valor_validado", "valor_pago", "valor_a_pagar"]:
                if col_moeda in df_final.columns:
                    df_final[col_moeda] = df_final[col_moeda].apply(limpar_valor_moeda)

            if "percentual_pago" in df_final.columns:
                df_final["percentual_pago"] = df_final["percentual_pago"].apply(limpar_percentual)

            if "parcelas_pagas" in df_final.columns:
                df_final["parcelas_pagas"] = pd.to_numeric(df_final["parcelas_pagas"], errors="coerce").astype("Int64")

            for col_str in ["municipio", "esfera", "tipo_execucao", "tipo_recurso"]:
                if col_str in df_final.columns:
                    df_final[col_str] = df_final[col_str].astype(str).str.strip()

            log(f"   Total de registros consolidados: {len(df_final)}")

            # =========================================================================
            # EXPORTAÇÕES FINAIS
            # =========================================================================
            log("4. Gravando arquivos consolidados...")

            df_final.to_excel(ARQUIVO_RAW_CONSOLIDADO, index=False, engine="openpyxl")
            log(f"   [ARQUIVO] Excel conferencia : {ARQUIVO_RAW_CONSOLIDADO}")

            df_final.to_parquet(ARQUIVO_PARQUET, index=False)
            log(f"   [ARQUIVO] Parquet analitico : {ARQUIVO_PARQUET}")

            registros = df_final.to_dict(orient="records")
            registros_limpos = [
                {k: (None if pd.isna(v) else v) for k, v in r.items()}
                for r in registros
            ]
            with open(ARQUIVO_JSON, "w", encoding="utf-8") as f:
                json.dump(registros_limpos, f, ensure_ascii=False, indent=2)
            log(f"   [ARQUIVO] JSON definitivo   : {ARQUIVO_JSON}")

            log("==================================================")
            log("ETL PPCOF CONCLUIDO COM SUCESSO!")
            log("==================================================")

        except Exception as e:
            log(f"[ERRO CRITICO]: {str(e)}")
            log(traceback.format_exc())

if __name__ == "__main__":
    asyncio.run(executar_rpa_ppcof())

import asyncio
import datetime
import os
import traceback
import yaml
from pathlib import Path
import pandas as pd
from bs4 import BeautifulSoup
from playwright.async_api import async_playwright

# =============================================================================
# MAPEAMENTO DINÂMICO DE DIRETÓRIOS DO PROJETO AURORAPRIME
# =============================================================================
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
DATA_DIR = PROJECT_ROOT / "data" / "efisco"
CREDENTIALS_FILE = PROJECT_ROOT / "data" / "credentials.yml"

DATA_DIR.mkdir(parents=True, exist_ok=True)

LOG_FILE = DATA_DIR / "log.txt"
EXCEL_OFICIAL_PATH = DATA_DIR / "export_oficial_seplag.xlsx"
NOME_FINAL_XLSX = DATA_DIR / "emendas_seplag_2026.xlsx"

URL_LOGIN = "https://emendaspe.seplag.pe.gov.br/usuarios/login/?next=/"
URL_TABELA = "https://emendaspe.seplag.pe.gov.br/emendas/por-ciclo/"

def log(mensagem):
    """Grava o log com data/hora no console e no arquivo data/efisco/log.txt"""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")

def limpar_valor_monetario(val):
    """Converte strings no padrão brasileiro (R$ 1.000,00 ou traços '-') para float."""
    if pd.isna(val) or val is None:
        return 0.0
    if isinstance(val, (int, float)):
        return float(val)
    s = str(val).strip().replace("R$", "").replace(" ", "")
    if s in ["-", "—", "", "nan", "None"]:
        return 0.0
    s = s.replace(".", "").replace(",", ".")
    try:
        return float(s)
    except ValueError:
        return 0.0

async def executar_rpa():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA EXECUÇÃO DO ROBÔ AURORAPRIME (EMENDAS) ===\n")
        f.write(f"Diretório dos Dados: {DATA_DIR}\n")

    try:
        with open(CREDENTIALS_FILE, 'r', encoding='utf-8') as f:
            creds = yaml.safe_load(f)
            USUARIO = creds['emendas']['usuario']
            SENHA = str(creds['emendas']['senha'])
    except Exception as e:
        log(f"❌ [ERRO] Falha ao ler o ficheiro {CREDENTIALS_FILE}.")
        log(str(e))
        return

    async with async_playwright() as p:
        try:
            log("1. Ligando o navegador (Chromium)...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(accept_downloads=True)
            page = await context.new_page()

            log(f"2. Acessando tela de login: {URL_LOGIN}")
            await page.goto(URL_LOGIN, wait_until="domcontentloaded")
            
            if await page.locator("input[name='username']").count() > 0:
                await page.fill("input[name='username']", USUARIO)
            else:
                await page.fill("input[name='login']", USUARIO)
                
            await page.fill("input[name='password']", SENHA)
            log("3. Submetendo credenciais...")
            
            try:
                async with page.expect_navigation(wait_until="networkidle", timeout=20000):
                    await page.click("button[type='submit'], input[type='submit']")
            except Exception:
                pass

            if "login" in page.url:
                log("❌ [ERRO] O login falhou! O servidor permaneceu na página de login.")
                await browser.close()
                return

            log(f"4. Acessando a página da tabela: {URL_TABELA}")
            await page.goto(URL_TABELA, wait_until="domcontentloaded")
            await page.wait_for_load_state("networkidle")

            # =========================================================================
            # ETAPA A: APLICAR FILTROS (UO + ZERADAS ON + BOTÃO FILTRAR)
            # =========================================================================
            log("5. Aplicando Filtros de UO e 'Zeradas: ON'...")
            try:
                select_uo = page.locator("select[name='uo']")
                await select_uo.wait_for(state="visible", timeout=20000)
                
                options = await select_uo.locator("option").all()
                target_uuid = None
                for option in options:
                    texto = await option.inner_text()
                    if "119" in texto or "Secretaria de Planejamento" in texto:
                        target_uuid = await option.get_attribute("value")
                        break
                
                if target_uuid:
                    await select_uo.select_option(value=target_uuid)
                    await asyncio.sleep(1)
            except Exception as e:
                log(f"❌ [ERRO] Falha ao selecionar UO: {e}")

            estado_zeradas = await page.evaluate("document.getElementById('mostrarZeradasToggle')?.checked")
            if not estado_zeradas:
                log("   🔘 Marcando o filtro 'Zeradas: ON'...")
                await page.locator("label[for='mostrarZeradasToggle']").click(force=True)
                await asyncio.sleep(1)

            log("   🔍 Clicando em 'Filtrar' para processar a base completa no servidor...")
            btn_filtrar = page.locator("button:has-text('Filtrar'), a:has-text('Filtrar')").first
            if await btn_filtrar.count() > 0:
                try:
                    async with page.expect_navigation(timeout=15000):
                        await btn_filtrar.click()
                except Exception:
                    await page.wait_for_timeout(3000)

            # =========================================================================
            # ETAPA B: RASPAGEM DA TABELA WEB PAGINADA
            # =========================================================================
            log("6. Iniciando a raspagem paginada da tabela web...")
            todas_linhas_dados = []
            cabecalhos = []
            pagina = 1

            while True:
                log(f"   -> Raspando Página {pagina}...")
                try:
                    await page.wait_for_selector("#emendasTableScrollBody table, section.table-shell table", timeout=15000)
                except Exception:
                    log(f"   ⚠️ Tabela não detectada na página {pagina}.")

                soup = BeautifulSoup(await page.content(), "html.parser")
                tabela = soup.select_one("#emendasTableScrollBody table") or soup.select_one("section.table-shell table")
                
                if tabela:
                    if not cabecalhos:
                        ths = tabela.select("thead th")
                        cabecalhos = [th.get_text(strip=True) for th in ths]

                    trs = tabela.select("tbody tr")
                    for tr in trs:
                        tds = tr.select("td")
                        if tds:
                            valores = [td.get_text(strip=True) for td in tds]
                            todas_linhas_dados.append(valores)

                proxima = pagina + 1
                link_prox = page.locator(f"ul.pagination li a:text-is('{proxima}'), a.page-link:text-is('{proxima}')").first
                btn_next = page.locator("a:has-text('Próxima'), a:has-text('Próximo'), a:has-text('»'), a:has-text('>')").last
                
                clicou = False
                if await link_prox.count() > 0 and await link_prox.is_visible():
                    try:
                        async with page.expect_navigation(timeout=10000):
                            await link_prox.click()
                    except Exception:
                        await page.wait_for_timeout(2500)
                    clicou = True
                elif await btn_next.count() > 0 and await btn_next.is_visible():
                    classe_pai = await btn_next.evaluate("el => el.parentElement.className")
                    if "disabled" not in classe_pai:
                        try:
                            async with page.expect_navigation(timeout=10000):
                                await btn_next.click()
                        except Exception:
                            await page.wait_for_timeout(2500)
                        clicou = True

                if clicou:
                    pagina += 1
                else:
                    log("   Fim das páginas detectado.")
                    break

            df_raspado = pd.DataFrame(todas_linhas_dados)
            if cabecalhos and len(cabecalhos) == df_raspado.shape[1]:
                df_raspado.columns = cabecalhos

            log(f"   ✅ Tabela Web raspada: {len(df_raspado)} linhas extraídas.")

            col_remover = [c for c in df_raspado.columns if c.upper() in ["AÇÕES", "ACOES", "HISTÓRICO", "HISTORICO", ""] or "UNNAMED" in c.upper()]
            if col_remover:
                df_raspado.drop(columns=col_remover, inplace=True, errors='ignore')

            # =========================================================================
            # ETAPA C: EXPORTAR O EXCEL OFICIAL
            # =========================================================================
            log("7. Clicando no botão 'Exportar Excel' para baixar a base oficial...")
            async with page.expect_download(timeout=30000) as download_info:
                await page.click("a:has-text('Exportar Excel'), button:has-text('Exportar Excel')")
            
            download = await download_info.value
            await download.save_as(str(EXCEL_OFICIAL_PATH))
            log(f"   📥 Download Oficial concluído!")

            # =========================================================================
            # ETAPA D: UNIFICAÇÃO, FORMATAÇÃO NUMÉRICA, CLASSIFICAÇÃO E ORDENAÇÃO
            # =========================================================================
            log("8. Unificando as bases e tratando campos monetários...")
            df_excel_oficial = pd.read_excel(EXCEL_OFICIAL_PATH)
            
            df_raspado.columns = [str(c).strip() for c in df_raspado.columns]
            df_excel_oficial.columns = [str(c).strip() for c in df_excel_oficial.columns]

            col_subacao_rasp = next((c for c in df_raspado.columns if c.lower() in ["subação", "subacao"]), None)
            col_subacao_ofic = next((c for c in df_excel_oficial.columns if c.lower() in ["subação", "subacao"]), None)

            if col_subacao_rasp and col_subacao_ofic:
                df_raspado[col_subacao_rasp] = df_raspado[col_subacao_rasp].astype(str).str.strip().str.upper()
                df_excel_oficial[col_subacao_ofic] = df_excel_oficial[col_subacao_ofic].astype(str).str.strip().str.upper()
                
                df_consolidado = pd.merge(
                    df_excel_oficial, 
                    df_raspado, 
                    left_on=col_subacao_ofic, 
                    right_on=col_subacao_rasp, 
                    how="left", 
                    suffixes=("", "_web")
                )
                colunas_para_dropar = [c for c in df_consolidado.columns if str(c).endswith('_web')]
                df_consolidado.drop(columns=colunas_para_dropar, inplace=True, errors='ignore')
            else:
                colunas_novas = [c for c in df_raspado.columns if c not in df_excel_oficial.columns]
                df_consolidado = pd.concat([df_excel_oficial, df_raspado[colunas_novas]], axis=1)

            df_consolidado = df_consolidado.loc[:, ~df_consolidado.columns.duplicated()]

            # Formatação numérica dos valores e ciclos
            alvos_numericos = [
                "valor loa",
                "1° ciclo", "1º ciclo",
                "2° ciclo", "2º ciclo",
                "3° ciclo", "3º ciclo",
                "4° ciclo", "4º ciclo",
                "valor atual"
            ]

            for col in df_consolidado.columns:
                col_normalizada = col.lower().replace("º", "°")
                if any(alvo in col_normalizada for alvo in alvos_numericos):
                    df_consolidado[col] = df_consolidado[col].apply(limpar_valor_monetario)
                    log(f"   🔢 Coluna formatada como número: '{col}'")

            # Localização das colunas para regra de negócio
            def buscar_coluna(termo):
                return next((c for c in df_consolidado.columns if termo in c.lower().replace("º", "°")), None)

            col_loa = buscar_coluna("valor loa")
            col_c1 = next((c for c in df_consolidado.columns if ("1° ciclo" in c.lower().replace("º", "°") or "1º ciclo" in c.lower())), None)
            col_c2 = next((c for c in df_consolidado.columns if ("2° ciclo" in c.lower().replace("º", "°") or "2º ciclo" in c.lower())), None)
            col_c3 = next((c for c in df_consolidado.columns if ("3° ciclo" in c.lower().replace("º", "°") or "3º ciclo" in c.lower())), None)
            col_c4 = next((c for c in df_consolidado.columns if ("4° ciclo" in c.lower().replace("º", "°") or "4º ciclo" in c.lower()) and "set" in c.lower()), None)
            col_atual = buscar_coluna("valor atual")

            # Regra de classificação para o 4º Ciclo
            def classificar_4_ciclo(row):
                v_c4 = round(row.get(col_c4, 0.0), 2) if col_c4 else 0.0
                v_atual = round(row.get(col_atual, 0.0), 2) if col_atual else 0.0
                v_loa = round(row.get(col_loa, 0.0), 2) if col_loa else 0.0
                v_c1 = round(row.get(col_c1, 0.0), 2) if col_c1 else 0.0
                v_c2 = round(row.get(col_c2, 0.0), 2) if col_c2 else 0.0
                v_c3 = round(row.get(col_c3, 0.0), 2) if col_c3 else 0.0

                if v_c4 < 0 and v_atual == 0:
                    return "Cancelada no 4º Ciclo"
                elif v_c4 > 0 and (v_loa == 0 and v_c1 == 0 and v_c2 == 0 and v_c3 == 0):
                    return "Criada no 4º Ciclo"
                else:
                    return "Demanda de ciclo anterior ao 4º"

            col_nova = "4º Ciclo?"
            if col_c4:
                series_classificada = df_consolidado.apply(classificar_4_ciclo, axis=1)
                if col_nova in df_consolidado.columns:
                    df_consolidado.drop(columns=[col_nova], inplace=True)
                
                idx_c4 = df_consolidado.columns.get_loc(col_c4)
                df_consolidado.insert(idx_c4 + 1, col_nova, series_classificada)
                log(f"   🏷️ Coluna '{col_nova}' gerada e inserida imediatamente após '{col_c4}'.")

                # Ordenação: Menor para maior pelo 4° Ciclo - SET
                df_consolidado.sort_values(by=col_c4, ascending=True, inplace=True)
                df_consolidado.reset_index(drop=True, inplace=True)
                log(f"   🔄 Base ordenada de forma ascendente pela coluna: '{col_c4}'")
            else:
                log("   ⚠️ Coluna '4° Ciclo - SET' não localizada para inserção e ordenação.")

            try:
                df_consolidado.to_excel(NOME_FINAL_XLSX, index=False, engine="openpyxl")
                log(f"📁 Arquivo Final gerado com sucesso: {NOME_FINAL_XLSX}")
            except PermissionError:
                log(f"⚠️ [AVISO DE PERMISSÃO] O arquivo '{NOME_FINAL_XLSX}' está aberto no Excel!")
                agora_str = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
                fallback_path = DATA_DIR / f"emendas_seplag_2026_{agora_str}.xlsx"
                df_consolidado.to_excel(fallback_path, index=False, engine="openpyxl")

            log("==================================================")
            log("🎉 PROCESSO CONCLUÍDO COM SUCESSO!")
            log(f"📊 Registos Finais Exportados: {len(df_consolidado)} | Total de colunas: {df_consolidado.shape[1]}")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"💥 [ERRO CRÍTICO]: {str(e)}")
            log(traceback.format_exc())

if __name__ == "__main__":
    asyncio.run(executar_rpa())

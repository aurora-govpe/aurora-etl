import asyncio
import datetime
import os
import traceback
import json
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
DATA_DIR = PROJECT_ROOT / "data" / "compesa"
CREDENTIALS_FILE = PROJECT_ROOT / "data" / "credentials.yml"

DATA_DIR.mkdir(parents=True, exist_ok=True)

LOG_FILE = DATA_DIR / "log.txt"
XLSX_PORTFOLIO = DATA_DIR / "portfolio_projetos_compesa.xlsx"
XLSX_DEFICIT = DATA_DIR / "municipios_deficit_bndes.xlsx"
XLSX_NR = DATA_DIR / "necessidade_recursos_compesa.xlsx"
XLSX_SRHS = DATA_DIR / "iniciativas_srhs_compesa.xlsx"
GEOJSON_MAPA = DATA_DIR / "mapa_compesa_todas_camadas.geojson"
XLSX_MAPA_TABULAR = DATA_DIR / "municipios_cenarios_mapa.xlsx"

URL_LOGIN = "https://pi.compesa.com.br/wp-login.php?redirect_to=https%3A%2F%2Fpi.compesa.com.br%2F&reauth=1"
URL_PAINEL = "https://pi.compesa.com.br/"

def log(mensagem):
    """Grava o log com data/hora no console e no arquivo data/compesa/log.txt"""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")

async def executar_rpa_compesa():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA EXECUÇÃO DO ROBÔ COMPESA (EXTRAÇÃO COMPLETA + MAPAS) ===\n")
        f.write(f"Diretório dos Dados: {DATA_DIR}\n")

    try:
        with open(CREDENTIALS_FILE, 'r', encoding='utf-8') as f:
            creds = yaml.safe_load(f)
            USUARIO = creds['compesa']['usuario']
            SENHA = str(creds['compesa']['senha'])
    except Exception as e:
        log(f"❌ [ERRO] Falha ao ler o arquivo {CREDENTIALS_FILE}: {e}")
        return

    async with async_playwright() as p:
        try:
            log("1. Ligando o navegador (Chromium)...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context()
            page = await context.new_page()

            log(f"2. Acessando tela de login: {URL_LOGIN}")
            await page.goto(URL_LOGIN, wait_until="domcontentloaded")
            await page.wait_for_load_state("networkidle")

            campo_usuario = page.locator("input#user_login, input[name='log'], input[type='text']").first
            campo_senha = page.locator("input#user_pass, input[name='pwd'], input[type='password']").first
            botao_acessar = page.locator("#wp-submit, button:has-text('Acessar'), input[type='submit']").first

            await campo_usuario.wait_for(state="visible", timeout=20000)
            await campo_usuario.fill(USUARIO)
            await campo_senha.fill(SENHA)

            log("🚀 Submetendo credenciais de acesso...")
            try:
                async with page.expect_navigation(wait_until="networkidle", timeout=25000):
                    await botao_acessar.click()
            except Exception:
                pass

            if "wp-login.php" in page.url:
                log("❌ [ERRO] Falha na autenticação.")
                await browser.close()
                return

            log("✅ Autenticação realizada com sucesso!")

            # =========================================================================
            # RASPAGEM 1: PORTFÓLIO DE PROJETOS (#tblProj)
            # =========================================================================
            log(f"3. NAVEGANDO -> Painel Principal: {URL_PAINEL}")
            await page.goto(URL_PAINEL, wait_until="domcontentloaded")
            await page.wait_for_load_state("networkidle")

            log("4. Extraindo Tabela 1: Portfólio de Projetos (#tblProj)...")
            await page.wait_for_selector("#tblProj tbody tr", state="attached", timeout=25000)
            soup_p = BeautifulSoup(await page.content(), "html.parser")
            tabela_proj = soup_p.select_one("#tblProj")

            if tabela_proj:
                projetos_extraidos = []
                for tr in tabela_proj.select("tbody tr"):
                    tds = tr.select("td")
                    if len(tds) < 11:
                        continue

                    diretoria = tds[0].get_text(strip=True)
                    td_p = tds[1]
                    div_m = td_p.select_one("div")
                    municipios = div_m.get_text(strip=True) if div_m else ""
                    if div_m:
                        div_m.extract()
                    nome_projeto = td_p.get_text(strip=True)

                    status = tds[2].get_text(strip=True)
                    fonte = tds[3].get_text(strip=True)
                    valor_rm = tds[4].get_text(strip=True)
                    pop_benef = tds[5].get_text(strip=True)
                    exec_pct = tds[6].get_text(strip=True)
                    termino = tds[7].get_text(strip=True)
                    proxima_etapa = tds[8].get_text(strip=True)
                    carteira = tds[9].get_text(strip=True)
                    objetivo = tds[10].get_text(strip=True)

                    desembolso_info = ""
                    if len(tds) > 11:
                        prev_tip = tds[11].select_one(".prev-tip")
                        if prev_tip:
                            desembolso_info = " | ".join([
                                r.get_text(" ", strip=True) for r in prev_tip.select(".prev-tip-row")
                            ])

                    projetos_extraidos.append({
                        "Diretoria": diretoria,
                        "Nome do Projeto": nome_projeto,
                        "Municípios Atendidos": municipios,
                        "Status": status,
                        "Fonte de Recursos": fonte,
                        "Valor LOA (R$ M)": valor_rm,
                        "População Beneficiada": pop_benef,
                        "Executado (%)": exec_pct,
                        "Previsão Término": termino,
                        "Próxima Etapa": proxima_etapa,
                        "Carteira": carteira,
                        "Objetivo": objetivo,
                        "Detalhamento Desembolso": desembolso_info
                    })

                df_p = pd.DataFrame(projetos_extraidos)
                df_p.to_excel(XLSX_PORTFOLIO, index=False, engine="openpyxl")
                log(f"   ✅ Portfólio gravado ({len(df_p)} registros): {XLSX_PORTFOLIO}")

            # =========================================================================
            # RASPAGEM 2: ANÁLISE BNDES (#tblDef)
            # =========================================================================
            log("5. Alternando para o separador 'Análise BNDES'...")
            try:
                aba_bndes = page.get_by_text("Análise BNDES", exact=False).first
                await aba_bndes.click(force=True, timeout=5000)
                await asyncio.sleep(1.5)
            except Exception:
                pass

            log("6. Extraindo Tabela 2: Municípios com déficit persistente (#tblDef)...")
            await page.wait_for_selector("#tblDef tbody tr", state="attached", timeout=15000)
            soup_d = BeautifulSoup(await page.content(), "html.parser")
            tabela_def = soup_d.select_one("#tblDef")

            if tabela_def:
                deficits_extraidos = []
                for tr in tabela_def.select("tbody tr"):
                    tds = tr.select("td")
                    if len(tds) < 6:
                        continue
                    deficits_extraidos.append({
                        "Município": tds[0].get_text(strip=True),
                        "Cat.": tds[1].get_text(strip=True),
                        "Microrregião": tds[2].get_text(strip=True),
                        "Maior déficit": tds[3].get_text(strip=True),
                        "1º ano": tds[4].get_text(strip=True),
                        "Intervenção": tds[5].get_text(strip=True)
                    })
                df_d = pd.DataFrame(deficits_extraidos)
                df_d.to_excel(XLSX_DEFICIT, index=False, engine="openpyxl")
                log(f"   ✅ Déficit BNDES gravado ({len(df_d)} municípios): {XLSX_DEFICIT}")

            # =========================================================================
            # RASPAGEM 3: NECESSIDADE DE RECURSOS (#tblNR)
            # =========================================================================
            log("7. Alternando para o separador 'Necessidade de Recursos'...")
            try:
                aba_nr = page.get_by_text("Necessidade de Recursos", exact=False).first
                await aba_nr.click(force=True, timeout=5000)
                await asyncio.sleep(1.5)
            except Exception:
                pass

            log("8. Extraindo Tabela 3: Resumo por Empreendimento e Fonte (#tblNR)...")
            await page.wait_for_selector("#tblNR tbody tr, #nrTbody tr", state="attached", timeout=15000)
            soup_nr = BeautifulSoup(await page.content(), "html.parser")
            tabela_nr = soup_nr.select_one("#tblNR")

            if tabela_nr:
                nr_extraidos = []
                for tr in tabela_nr.select("tbody tr"):
                    tds = tr.select("td")
                    if len(tds) == 2 and tds[0].has_attr("colspan") and tds[0]["colspan"] == "6":
                        emp_nome = tds[0].get_text(strip=True)
                        tot_val = tds[1].get_text(strip=True)
                        nr_extraidos.append({
                            "Empreendimento": emp_nome,
                            "FONTE AGRUPADORA": "TOTAL AGRUPADO",
                            "Status": "-",
                            "Término": "-",
                            "Próxima Etapa": "-",
                            "Data": "-",
                            "Total Geral (R$)": tot_val
                        })
                    elif len(tds) >= 7:
                        nr_extraidos.append({
                            "Empreendimento": tds[0].get_text(strip=True),
                            "FONTE AGRUPADORA": tds[1].get_text(strip=True),
                            "Status": tds[2].get_text(strip=True),
                            "Término": tds[3].get_text(strip=True),
                            "Próxima Etapa": tds[4].get_text(strip=True),
                            "Data": tds[5].get_text(strip=True),
                            "Total Geral (R$)": tds[6].get_text(strip=True)
                        })

                df_nr = pd.DataFrame(nr_extraidos)
                df_nr.to_excel(XLSX_NR, index=False, engine="openpyxl")
                log(f"   ✅ Necessidade de Recursos gravada ({len(df_nr)} linhas): {XLSX_NR}")

            # =========================================================================
            # RASPAGEM 4: INICIATIVAS SRHS (#tblSRHS)
            # =========================================================================
            log("9. Alternando para o separador 'Iniciativas SRHS'...")
            try:
                aba_srhs = page.get_by_text("Iniciativas SRHS", exact=False).first
                await aba_srhs.click(force=True, timeout=5000)
                await asyncio.sleep(1.5)
            except Exception:
                pass

            log("10. Extraindo Tabela 4: Iniciativas SRHS (#tblSRHS)...")
            await page.wait_for_selector("#tblSRHS tbody tr", state="attached", timeout=15000)
            soup_srhs = BeautifulSoup(await page.content(), "html.parser")
            tabela_srhs = soup_srhs.select_one("#tblSRHS")

            if tabela_srhs:
                srhs_extraidos = []
                for tr in tabela_srhs.select("tbody tr"):
                    tds = tr.select("td")
                    if len(tds) < 6:
                        continue
                    td_acao = tds[0]
                    div_obs = td_acao.select_one("div")
                    observacao = div_obs.get_text(strip=True) if div_obs else ""
                    if div_obs:
                        div_obs.extract()
                    nome_acao = td_acao.get_text(strip=True)

                    srhs_extraidos.append({
                        "Ação": nome_acao,
                        "Observação Ação": observacao,
                        "Municípios": tds[1].get_text(strip=True),
                        "Status": tds[2].get_text(strip=True),
                        "Fonte": tds[3].get_text(strip=True),
                        "Valor": tds[4].get_text(strip=True),
                        "Período": tds[5].get_text(strip=True)
                    })

                df_srhs = pd.DataFrame(srhs_extraidos)
                df_srhs.to_excel(XLSX_SRHS, index=False, engine="openpyxl")
                log(f"   ✅ Iniciativas SRHS gravadas ({len(df_srhs)} ações): {XLSX_SRHS}")

            # =========================================================================
            # RASPAGEM 5: EXTRAÇÃO DE TODAS AS CAMADAS DO MAPA (GEOJSON + EXCEL)
            # =========================================================================
            log("11. Alternando para o separador 'Mapa de Municípios'...")
            try:
                aba_mapa = page.get_by_text("Mapa de Municípios", exact=False).first
                await aba_mapa.click(force=True, timeout=5000)
                await page.wait_for_load_state("networkidle")
            except Exception:
                pass

            log("12. Extraindo todas as camadas e cenários do dropdown 'Colorir por:'...")
            await page.wait_for_selector(".leaflet-container, #map", timeout=20000)

            # Executa a varredura completa por todas as opções de camadas
            js_extrator_camadas = """
            async () => {
                let sel = Array.from(document.querySelectorAll('select')).find(s => s.parentElement && s.parentElement.innerText.includes('Colorir por')) || document.querySelector('select');
                if (!sel) return { erro: 'Dropdown não encontrado' };

                let opts = Array.from(sel.options).map(o => ({ value: o.value, text: o.text.trim() }));
                let mapaObj = null;
                let c = document.querySelector('.leaflet-container');
                if (c) {
                    for (let k in c) {
                        if (k.indexOf('_leaflet_map') === 0 || (c[k] && typeof c[k].eachLayer === 'function')) {
                            mapaObj = c[k];
                            break;
                        }
                    }
                }
                if (!mapaObj) {
                    for (let w in window) {
                        try {
                            if (window[w] && typeof window[w].eachLayer === 'function' && window[w]._panes) {
                                mapaObj = window[w];
                                break;
                            }
                        } catch(e) {}
                    }
                }

                if (!mapaObj) return { erro: 'Instância Leaflet não encontrada' };

                let municipiosData = {};
                let linhasAdutoras = [];

                for (let opt of opts) {
                    sel.value = opt.value;
                    sel.dispatchEvent(new Event('change'));
                    await new Promise(r => setTimeout(r, 600));

                    mapaObj.eachLayer(layer => {
                        try {
                            if (typeof layer.toGeoJSON === 'function') {
                                let geo = layer.toGeoJSON();
                                if (!geo || !geo.geometry) return;
                                let props = geo.properties || {};
                                let nomeMun = props.name || props.Nome || props.municipio || props.NM_MUN;

                                if (geo.geometry.type === 'Polygon' || geo.geometry.type === 'MultiPolygon') {
                                    let key = nomeMun || ('mun_' + Object.keys(municipiosData).length);
                                    if (!municipiosData[key]) {
                                        municipiosData[key] = {
                                            type: 'Feature',
                                            geometry: geo.geometry,
                                            properties: Object.assign({}, props)
                                        };
                                    }
                                    let cor = layer.options && layer.options.fillColor ? layer.options.fillColor : '';
                                    municipiosData[key].properties['Cenario: ' + opt.text] = props.status || props.situacao || cor || 'Ativo';
                                } else if (geo.geometry.type === 'LineString' || geo.geometry.type === 'MultiLineString') {
                                    geo.properties = geo.properties || {};
                                    geo.properties.camada_origem = opt.text;
                                    linhasAdutoras.push(geo);
                                }
                            }
                        } catch(e) {}
                    });
                }

                let features = Object.values(municipiosData).concat(linhasAdutoras);
                let tabelaMunicipios = Object.values(municipiosData).map(f => f.properties);

                return {
                    geojson: { type: 'FeatureCollection', total_camadas: opts.length, features: features },
                    tabela: tabelaMunicipios,
                    total_camadas: opts.length
                };
            }
            """

            resultado_mapa = await page.evaluate(js_extrator_camadas)

            if "erro" in resultado_mapa:
                log(f"   ⚠️ Aviso: {resultado_mapa['erro']}")
            else:
                # 1. Grava o GeoJSON consolidado com todas as geometrias e atributos
                geojson_dados = resultado_mapa.get("geojson", {})
                with open(GEOJSON_MAPA, "w", encoding="utf-8") as f:
                    json.dump(geojson_dados, f, ensure_ascii=False, indent=2)
                log(f"   🗺️ GeoJSON com todas as camadas ({resultado_mapa.get('total_camadas')} cenários): {GEOJSON_MAPA}")

                # 2. Grava uma planilha Excel com as colunas de cada cenário lado a lado
                tabela_dados = resultado_mapa.get("tabela", [])
                if tabela_dados:
                    df_mapa = pd.DataFrame(tabela_dados)
                    df_mapa.to_excel(XLSX_MAPA_TABULAR, index=False, engine="openpyxl")
                    log(f"   📊 Tabela de Cenários dos Municípios gravada ({len(df_mapa)} cidades): {XLSX_MAPA_TABULAR}")

            log("==================================================")
            log("🎉 TODAS AS EXTRAÇÕES COMPESA (TABELAS + MAPAS) CONCLUÍDAS COM SUCESSO!")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"💥 [ERRO CRÍTICO]: {str(e)}")
            log(traceback.format_exc())

if __name__ == "__main__":
    asyncio.run(executar_rpa_compesa())

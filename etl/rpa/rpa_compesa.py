import asyncio
import datetime
import os
import re
import traceback
import json
from pathlib import Path
import numpy as np
import pandas as pd
from bs4 import BeautifulSoup
from playwright.async_api import async_playwright
from dotenv import load_dotenv

# =============================================================================
# MAPEAMENTO DE DIRETÓRIOS E CREDENCIAIS (PADRÃO AURORA)
# =============================================================================
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

load_dotenv(".Renviron")
load_dotenv(PROJECT_ROOT / ".Renviron")

# 1. Diretório Bruto / Conferência Humana (.xlsx)
DIR_RAW = Path(r"C:\dados\dados brutos\aurora\compesa")
DIR_RAW.mkdir(parents=True, exist_ok=True)

XLSX_PORTFOLIO = DIR_RAW / "portfolio_projetos_compesa.xlsx"
XLSX_DEFICIT = DIR_RAW / "municipios_deficit_bndes.xlsx"
XLSX_NR = DIR_RAW / "necessidade_recursos_compesa.xlsx"
XLSX_SRHS = DIR_RAW / "iniciativas_srhs_compesa.xlsx"
XLSX_MAPA_TABULAR = DIR_RAW / "municipios_cenarios_mapa.xlsx"

# 2. Diretório Analítico Oficial (JSON, Parquet e GeoJSON)
DIR_ETL = Path(r"C:\dados\aurora")
DIR_ETL.mkdir(parents=True, exist_ok=True)

DIR_PARQUET = DIR_ETL / "parquet"
DIR_PARQUET.mkdir(parents=True, exist_ok=True)

DIR_GEOJSON = DIR_ETL / "geojson"
DIR_GEOJSON.mkdir(parents=True, exist_ok=True)

GEOJSON_MAPA = DIR_GEOJSON / "mapa_compesa_todas_camadas.geojson"
JSON_CONSOLIDADO = DIR_ETL / "compesa_plano_investimentos.json"
LOG_FILE = DIR_ETL / "log_compesa.txt"

URL_LOGIN = "https://pi.compesa.com.br/wp-login.php?redirect_to=https%3A%2F%2Fpi.compesa.com.br%2F&reauth=1"
URL_PAINEL = "https://pi.compesa.com.br/"


def log(mensagem: str):
    """Grava o log com data/hora no console e em C:\\dados\\aurora\\log_compesa.txt."""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    linha_segura = linha.encode("utf-8", errors="replace").decode("utf-8", errors="replace")
    try:
        print(linha_segura)
    except Exception:
        print(linha_segura.encode("ascii", errors="replace").decode("ascii"))
    with open(LOG_FILE, "a", encoding="utf-8", errors="replace") as f:
        f.write(linha_segura + "\n")


def limpar_numero_br(val) -> float:
    """Converte strings numéricas no padrão PT-BR ('R$ 1.234,56', '45,2%', '120.450') para float."""
    if pd.isna(val) or val is None:
        return np.nan
    if isinstance(val, (int, float, np.integer, np.floating)):
        return float(val)

    s = str(val).strip()
    if not s or s in {"-", "–", "—", "n/d", "N/D", "N/A", "nan", "None"}:
        return np.nan

    # Remove prefixos/sufixos monetários, percentuais e unidades comuns
    s = re.sub(r"(?i)(R\$|%|hab\.?|l/s|m³/s|m3/s|milhões|milhoes|mi|\bM\b)", "", s).strip()

    # Captura o número com separador de milhar (.) e decimal (,) ou decimal simples
    m = re.search(r"-?\d{1,3}(?:\.\d{3})+(?:,\d+)?|-?\d+(?:,\d+)?", s)
    if not m:
        return np.nan

    num_str = m.group(0).replace(".", "").replace(",", ".")
    try:
        return float(num_str)
    except ValueError:
        return np.nan


def sanitizar_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    """Garante compatibilidade de tipos para exportação segura em Parquet e JSON."""
    df_clean = df.copy()
    df_clean.columns = [str(c).strip() for c in df_clean.columns]

    for col in df_clean.columns:
        df_clean[col] = df_clean[col].apply(
            lambda v: json.dumps(v, ensure_ascii=False) if isinstance(v, (dict, list)) else v
        )
    return df_clean


def serializar_valor_json(v):
    """Converte valores Pandas/NumPy (NaN, pd.NA, int64, float64) para tipos nativos JSON."""
    if pd.isna(v):
        return None
    if isinstance(v, (np.integer,)):
        return int(v)
    if isinstance(v, (np.floating,)):
        return float(v)
    if isinstance(v, (np.bool_,)):
        return bool(v)
    return v


def salvar_base_analitica(
    df: pd.DataFrame,
    chave_json: str,
    nome_parquet: str,
    acumulador_json: dict,
    caminho_xlsx: Path = None
):
    """Exporta o DataFrame tipado para XLSX (bruto), Parquet (analítico) e dicionário JSON."""
    df_safe = sanitizar_dataframe(df)

    if caminho_xlsx is not None:
        df_safe.to_excel(caminho_xlsx, index=False, engine="openpyxl")

    caminho_parquet = DIR_PARQUET / nome_parquet
    df_safe.to_parquet(caminho_parquet, index=False)

    registros = df_safe.to_dict(orient="records")
    acumulador_json[chave_json] = [
        {k: serializar_valor_json(v) for k, v in r.items()}
        for r in registros
    ]


async def executar_rpa_compesa():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INICIO DA EXECUCAO DO ROBO COMPESA (PADRAO AURORA - TIPADO) ===\n")
        f.write(f"Destino Bruto (XLSX): {DIR_RAW}\n")
        f.write(f"Destino Analitico   : {DIR_ETL}\n")
        f.write(f"Destino GeoJSON     : {DIR_GEOJSON}\n")

    try:
        USUARIO = os.environ.get("COMPESA_USUARIO")
        SENHA = os.environ.get("COMPESA_SENHA")

        if not USUARIO or not SENHA:
            log("[ERRO] Credenciais nao encontradas. Verifique COMPESA_USUARIO e COMPESA_SENHA no .Renviron.")
            return
    except Exception as e:
        log(f"[ERRO] Falha ao carregar credenciais do ambiente: {e}")
        return

    dados_consolidados_json = {}

    async with async_playwright() as p:
        try:
            log("1. Ligando o navegador (Chromium)...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(viewport={"width": 1600, "height": 900})
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

            log("Submetendo credenciais de acesso...")
            try:
                async with page.expect_navigation(wait_until="networkidle", timeout=25000):
                    await botao_acessar.click()
            except Exception:
                pass

            if "wp-login.php" in page.url:
                log("[ERRO] Falha na autenticacao.")
                await browser.close()
                return

            log("[OK] Autenticacao realizada com sucesso!")

            # =========================================================================
            # RASPAGEM 1: PORTFÓLIO DE PROJETOS (#tblProj)
            # =========================================================================
            log(f"3. NAVEGANDO -> Painel Principal: {URL_PAINEL}")
            await page.goto(URL_PAINEL, wait_until="domcontentloaded")
            await page.wait_for_load_state("networkidle")

            log("4. Extraindo Tabela 1: Portfolio de Projetos (#tblProj)...")
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
                    valor_rm = limpar_numero_br(tds[4].get_text(strip=True))
                    pop_benef = limpar_numero_br(tds[5].get_text(strip=True))
                    exec_pct = limpar_numero_br(tds[6].get_text(strip=True))
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
                for col_num in ["Valor LOA (R$ M)", "População Beneficiada", "Executado (%)"]:
                    df_p[col_num] = pd.to_numeric(df_p[col_num], errors="coerce").astype("float64")

                salvar_base_analitica(
                    df_p,
                    chave_json="portfolio_projetos",
                    nome_parquet="compesa_portfolio_projetos.parquet",
                    acumulador_json=dados_consolidados_json,
                    caminho_xlsx=XLSX_PORTFOLIO
                )
                log(f"   [OK] Portfolio gravado ({len(df_p)} registros) -> XLSX + Parquet + JSON")

            # =========================================================================
            # RASPAGEM 2: ANÁLISE BNDES (#tblDef)
            # =========================================================================
            log("5. Alternando para o separador 'Analise BNDES'...")
            try:
                aba_bndes = page.get_by_text("Análise BNDES", exact=False).first
                await aba_bndes.click(force=True, timeout=5000)
                await asyncio.sleep(1.5)
            except Exception:
                pass

            log("6. Extraindo Tabela 2: Municipios com deficit persistente (#tblDef)...")
            await page.wait_for_selector("#tblDef tbody tr", state="attached", timeout=15000)
            soup_d = BeautifulSoup(await page.content(), "html.parser")
            tabela_def = soup_d.select_one("#tblDef")

            if tabela_def:
                deficits_extraidos = []
                for tr in tabela_def.select("tbody tr"):
                    tds = tr.select("td")
                    if len(tds) < 6:
                        continue
                    maior_def_raw = tds[3].get_text(strip=True)
                    deficits_extraidos.append({
                        "Município": tds[0].get_text(strip=True),
                        "Cat.": tds[1].get_text(strip=True),
                        "Microrregião": tds[2].get_text(strip=True),
                        "Maior déficit": limpar_numero_br(maior_def_raw),
                        "Maior déficit (Original)": maior_def_raw,
                        "1º ano": tds[4].get_text(strip=True),
                        "Intervenção": tds[5].get_text(strip=True)
                    })
                df_d = pd.DataFrame(deficits_extraidos)
                df_d["Maior déficit"] = pd.to_numeric(df_d["Maior déficit"], errors="coerce").astype("float64")
                df_d["1º ano"] = pd.to_numeric(df_d["1º ano"], errors="coerce").astype("Int64")

                salvar_base_analitica(
                    df_d,
                    chave_json="municipios_deficit_bndes",
                    nome_parquet="compesa_municipios_deficit_bndes.parquet",
                    acumulador_json=dados_consolidados_json,
                    caminho_xlsx=XLSX_DEFICIT
                )
                log(f"   [OK] Deficit BNDES gravado ({len(df_d)} municipios) -> XLSX + Parquet + JSON")

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
                            "is_total_agrupado": True,
                            "Status": "-",
                            "Término": "-",
                            "Próxima Etapa": "-",
                            "Data": "-",
                            "Total Geral (R$)": limpar_numero_br(tot_val)
                        })
                    elif len(tds) >= 7:
                        nr_extraidos.append({
                            "Empreendimento": tds[0].get_text(strip=True),
                            "FONTE AGRUPADORA": tds[1].get_text(strip=True),
                            "is_total_agrupado": False,
                            "Status": tds[2].get_text(strip=True),
                            "Término": tds[3].get_text(strip=True),
                            "Próxima Etapa": tds[4].get_text(strip=True),
                            "Data": tds[5].get_text(strip=True),
                            "Total Geral (R$)": limpar_numero_br(tds[6].get_text(strip=True))
                        })

                df_nr = pd.DataFrame(nr_extraidos)
                df_nr["Total Geral (R$)"] = pd.to_numeric(df_nr["Total Geral (R$)"], errors="coerce").astype("float64")
                df_nr["is_total_agrupado"] = df_nr["is_total_agrupado"].astype(bool)

                salvar_base_analitica(
                    df_nr,
                    chave_json="necessidade_recursos",
                    nome_parquet="compesa_necessidade_recursos.parquet",
                    acumulador_json=dados_consolidados_json,
                    caminho_xlsx=XLSX_NR
                )
                log(f"   [OK] Necessidade de Recursos gravada ({len(df_nr)} linhas) -> XLSX + Parquet + JSON")

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
                    valor_raw = tds[4].get_text(strip=True)

                    srhs_extraidos.append({
                        "Ação": nome_acao,
                        "Observação Ação": observacao,
                        "Municípios": tds[1].get_text(strip=True),
                        "Status": tds[2].get_text(strip=True),
                        "Fonte": tds[3].get_text(strip=True),
                        "Valor": limpar_numero_br(valor_raw),
                        "Valor (Original)": valor_raw,
                        "Período": tds[5].get_text(strip=True)
                    })

                df_srhs = pd.DataFrame(srhs_extraidos)
                df_srhs["Valor"] = pd.to_numeric(df_srhs["Valor"], errors="coerce").astype("float64")

                salvar_base_analitica(
                    df_srhs,
                    chave_json="iniciativas_srhs",
                    nome_parquet="compesa_iniciativas_srhs.parquet",
                    acumulador_json=dados_consolidados_json,
                    caminho_xlsx=XLSX_SRHS
                )
                log(f"   [OK] Iniciativas SRHS gravadas ({len(df_srhs)} acoes) -> XLSX + Parquet + JSON")

            # =========================================================================
            # RASPAGEM 5: EXTRAÇÃO DE TODAS AS CAMADAS DO MAPA (GEOJSON + EXCEL + PARQUET)
            # =========================================================================
            log("11. Alternando para o separador 'Mapa de Municipios'...")
            try:
                aba_mapa = page.get_by_text("Mapa de Municípios", exact=False).first
                await aba_mapa.click(force=True, timeout=5000)
                await page.wait_for_load_state("networkidle")
                await asyncio.sleep(1.5)
            except Exception:
                pass

            log("12. Extraindo todas as camadas do dropdown 'Colorir por:' (incluindo Rodizio atual)...")
            await page.wait_for_selector(".leaflet-container, #map", timeout=20000)

            js_extrator_camadas = """
            async () => {
                const selects = Array.from(document.querySelectorAll('select'));
                let sel = selects.find(s => {
                    const textos = Array.from(s.options).map(o => o.text.trim().toLowerCase());
                    return textos.some(t =>
                        t.includes('rodízio') ||
                        t.includes('situação c2') ||
                        t.includes('sistemas produtores')
                    );
                });

                if (!sel) {
                    sel = selects.find(s => {
                        let p = s.parentElement;
                        for (let i = 0; i < 4 && p; i++) {
                            if ((p.innerText || '').includes('Colorir por')) return true;
                            p = p.parentElement;
                        }
                        return false;
                    });
                }

                if (!sel) return { erro: 'Dropdown "Colorir por" nao encontrado no DOM.' };

                const chkMulti = Array.from(document.querySelectorAll('input[type="checkbox"]')).find(cb => {
                    const lbl = cb.closest('label') || cb.parentElement;
                    return lbl && (lbl.innerText || '').toLowerCase().includes('múltiplos cenários');
                });
                if (chkMulti && chkMulti.checked) {
                    chkMulti.click();
                    await new Promise(r => setTimeout(r, 400));
                }

                let mapaObj = null;
                const containers = Array.from(document.querySelectorAll('.leaflet-container'));
                for (const c of containers) {
                    for (const k in c) {
                        if (k.startsWith('_leaflet_map') || (c[k] && typeof c[k].eachLayer === 'function')) {
                            mapaObj = c[k];
                            break;
                        }
                    }
                    if (mapaObj) break;
                }
                if (!mapaObj) {
                    for (const w in window) {
                        try {
                            if (window[w] && typeof window[w].eachLayer === 'function' && window[w]._panes) {
                                mapaObj = window[w];
                                break;
                            }
                        } catch (e) {}
                    }
                }
                if (!mapaObj) return { erro: 'Instancia Leaflet nao encontrada.' };

                const normalizarCor = (corStr) => {
                    if (!corStr) return '';
                    const s = corStr.trim().toLowerCase();
                    if (s.startsWith('#')) {
                        if (s.length === 4) {
                            return '#' + s[1] + s[1] + s[2] + s[2] + s[3] + s[3];
                        }
                        return s.slice(0, 7);
                    }
                    const m = s.match(/rgba?\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)/);
                    if (m) {
                        const hex = [m[1], m[2], m[3]]
                            .map(n => parseInt(n, 10).toString(16).padStart(2, '0'))
                            .join('');
                        return '#' + hex;
                    }
                    return s;
                };

                const lerLegendaAtual = () => {
                    const mapaCores = {};
                    const barraMapa = sel.closest('div')?.parentElement || document;
                    const swatches = Array.from(barraMapa.querySelectorAll('span, div, i')).filter(el => {
                        const rect = el.getBoundingClientRect();
                        const st = window.getComputedStyle(el);
                        const bg = normalizarCor(st.backgroundColor);
                        return rect.width >= 8 && rect.width <= 24 &&
                               rect.height >= 8 && rect.height <= 24 &&
                               bg && bg !== '#000000' && bg !== '#ffffff' && bg !== 'transparent' &&
                               st.backgroundColor !== 'rgba(0, 0, 0, 0)';
                    });

                    for (const sw of swatches) {
                        const bg = normalizarCor(window.getComputedStyle(sw).backgroundColor);
                        const parent = sw.parentElement;
                        if (parent) {
                            const texto = (parent.innerText || '').trim();
                            if (texto && texto.length < 60 && !mapaCores[bg]) {
                                mapaCores[bg] = texto;
                            }
                        }
                    }
                    return mapaCores;
                };

                const limparHtml = (htmlOrEl) => {
                    if (!htmlOrEl) return '';
                    if (typeof htmlOrEl === 'string') {
                        const tmp = document.createElement('div');
                        tmp.innerHTML = htmlOrEl;
                        return (tmp.innerText || tmp.textContent || '').replace(/\\s+/g, ' ').trim();
                    }
                    if (htmlOrEl.innerText) {
                        return htmlOrEl.innerText.replace(/\\s+/g, ' ').trim();
                    }
                    return '';
                };

                const opts = Array.from(sel.options)
                    .filter(o => o.value !== '' && !o.disabled)
                    .map(o => ({ value: o.value, text: o.text.trim() }));

                const municipiosData = {};
                const linhasOverlays = [];
                const pontosOverlays = [];
                const chavesLinhasVistas = new Set();
                const chavesPontosVistos = new Set();

                for (const opt of opts) {
                    sel.value = opt.value;
                    sel.dispatchEvent(new Event('change', { bubbles: true }));
                    sel.dispatchEvent(new Event('input', { bubbles: true }));
                    await new Promise(r => setTimeout(r, 900));

                    const legendaCores = lerLegendaAtual();

                    mapaObj.eachLayer(layer => {
                        try {
                            if (typeof layer.toGeoJSON !== 'function') return;
                            const geo = layer.toGeoJSON();
                            if (!geo || !geo.geometry) return;

                            const props = Object.assign({}, geo.properties || {});
                            const tipoGeom = geo.geometry.type;

                            let tooltipTxt = '';
                            if (typeof layer.getTooltip === 'function' && layer.getTooltip()) {
                                tooltipTxt = limparHtml(layer.getTooltip().getContent());
                            } else if (layer._tooltip && layer._tooltip._content) {
                                tooltipTxt = limparHtml(layer._tooltip._content);
                            }

                            let popupTxt = '';
                            if (typeof layer.getPopup === 'function' && layer.getPopup()) {
                                popupTxt = limparHtml(layer.getPopup().getContent());
                            }

                            if (tipoGeom === 'Polygon' || tipoGeom === 'MultiPolygon') {
                                const nomeMun = props.name || props.Nome || props.municipio || props.NM_MUN || props.MUNICIPIO;
                                const key = nomeMun || ('mun_' + Object.keys(municipiosData).length);

                                if (!municipiosData[key]) {
                                    municipiosData[key] = {
                                        type: 'Feature',
                                        geometry: geo.geometry,
                                        properties: Object.assign({}, props)
                                    };
                                }

                                const corRaw = (layer.options && layer.options.fillColor) ? layer.options.fillColor : '';
                                const corHex = normalizarCor(corRaw);
                                const rotuloLegenda = legendaCores[corHex] || '';

                                const valorCenario = rotuloLegenda || props[opt.value] || props.status || props.situacao || corHex || 'Sem dados';

                                municipiosData[key].properties[opt.text] = valorCenario;
                                if (corHex) {
                                    municipiosData[key].properties[opt.text + ' (Cor)'] = corHex;
                                }
                                if (tooltipTxt && tooltipTxt !== nomeMun) {
                                    municipiosData[key].properties[opt.text + ' (Detalhe)'] = tooltipTxt;
                                }
                            } else if (tipoGeom === 'LineString' || tipoGeom === 'MultiLineString') {
                                const assinatura = opt.text + '|' + JSON.stringify(geo.geometry.coordinates).slice(0, 120);
                                if (!chavesLinhasVistas.has(assinatura)) {
                                    chavesLinhasVistas.add(assinatura);
                                    geo.properties = Object.assign({}, props, {
                                        camada_origem: opt.text,
                                        detalhe_tooltip: tooltipTxt || popupTxt || ''
                                    });
                                    linhasOverlays.push(geo);
                                }
                            } else if (tipoGeom === 'Point' || tipoGeom === 'MultiPoint') {
                                const assinatura = opt.text + '|' + JSON.stringify(geo.geometry.coordinates);
                                if (!chavesPontosVistos.has(assinatura)) {
                                    chavesPontosVistos.add(assinatura);
                                    geo.properties = Object.assign({}, props, {
                                        camada_origem: opt.text,
                                        detalhe_tooltip: tooltipTxt || popupTxt || '',
                                        latitude: Array.isArray(geo.geometry.coordinates) ? geo.geometry.coordinates[1] : null,
                                        longitude: Array.isArray(geo.geometry.coordinates) ? geo.geometry.coordinates[0] : null
                                    });
                                    pontosOverlays.push(geo);
                                }
                            }
                        } catch (e) {}
                    });
                }

                const features = Object.values(municipiosData)
                    .concat(linhasOverlays)
                    .concat(pontosOverlays);

                return {
                    geojson: {
                        type: 'FeatureCollection',
                        camadas_extraidas: opts.map(o => o.text),
                        total_camadas: opts.length,
                        features: features
                    },
                    tabela_municipios: Object.values(municipiosData).map(f => f.properties),
                    tabela_pontos: pontosOverlays.map(f => f.properties),
                    tabela_linhas: linhasOverlays.map(f => f.properties),
                    camadas_lista: opts.map(o => o.text),
                    total_camadas: opts.length
                };
            }
            """

            resultado_mapa = await page.evaluate(js_extrator_camadas)

            if "erro" in resultado_mapa:
                log(f"   [AVISO] {resultado_mapa['erro']}")
            else:
                camadas_lista = resultado_mapa.get("camadas_lista", [])
                log(f"   [INFO] {len(camadas_lista)} camadas processadas: {', '.join(camadas_lista)}")

                geojson_dados = resultado_mapa.get("geojson", {})
                with open(GEOJSON_MAPA, "w", encoding="utf-8") as f:
                    json.dump(geojson_dados, f, ensure_ascii=False, indent=2)
                log(f"   [OK] GeoJSON consolidado gravado: {GEOJSON_MAPA}")

                tabela_mun = resultado_mapa.get("tabela_municipios", [])
                tabela_pts = resultado_mapa.get("tabela_pontos", [])
                tabela_lin = resultado_mapa.get("tabela_linhas", [])

                with pd.ExcelWriter(XLSX_MAPA_TABULAR, engine="openpyxl") as writer:
                    if tabela_mun:
                        df_mapa = pd.DataFrame(tabela_mun)
                        sanitizar_dataframe(df_mapa).to_excel(writer, sheet_name="Municipios_Camadas", index=False)
                        salvar_base_analitica(
                            df_mapa,
                            chave_json="mapa_municipios_camadas",
                            nome_parquet="compesa_mapa_municipios_camadas.parquet",
                            acumulador_json=dados_consolidados_json
                        )
                        log(f"   [OK] Camadas Municipais ({len(df_mapa)} cidades) -> XLSX + Parquet + JSON")

                    if tabela_pts:
                        df_pts = pd.DataFrame(tabela_pts)
                        for coord in ["latitude", "longitude"]:
                            if coord in df_pts.columns:
                                df_pts[coord] = pd.to_numeric(df_pts[coord], errors="coerce").astype("float64")
                        sanitizar_dataframe(df_pts).to_excel(writer, sheet_name="Sistemas_e_Pontos", index=False)
                        salvar_base_analitica(
                            df_pts,
                            chave_json="mapa_sistemas_pontos",
                            nome_parquet="compesa_mapa_sistemas_pontos.parquet",
                            acumulador_json=dados_consolidados_json
                        )
                        log(f"   [OK] Sistemas e Pontos ({len(df_pts)} pontos) -> XLSX + Parquet + JSON")

                    if tabela_lin:
                        df_lin = pd.DataFrame(tabela_lin)
                        sanitizar_dataframe(df_lin).to_excel(writer, sheet_name="Linhas_e_Adutoras", index=False)
                        salvar_base_analitica(
                            df_lin,
                            chave_json="mapa_linhas_adutoras",
                            nome_parquet="compesa_mapa_linhas_adutoras.parquet",
                            acumulador_json=dados_consolidados_json
                        )
                        log(f"   [OK] Linhas e Adutoras ({len(df_lin)} trechos) -> XLSX + Parquet + JSON")

            # =========================================================================
            # GRAVAÇÃO DO JSON UNIFICADO EM C:\dados\aurora
            # =========================================================================
            with open(JSON_CONSOLIDADO, "w", encoding="utf-8") as f:
                json.dump(dados_consolidados_json, f, ensure_ascii=False, indent=2, allow_nan=False)
            log(f"   [OK] JSON consolidado gravado: {JSON_CONSOLIDADO}")

            log("==================================================")
            log("EXTRACAO COMPESA CONCLUIDA COM TIPAGEM NUMERICA!")
            log(f"Bruto (XLSX) : {DIR_RAW}")
            log(f"Analitico    : {DIR_ETL}")
            log(f"Parquet      : {DIR_PARQUET}")
            log(f"GeoJSON      : {DIR_GEOJSON}")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"[ERRO CRITICO]: {str(e)}")
            log(traceback.format_exc())


if __name__ == "__main__":
    asyncio.run(executar_rpa_compesa())

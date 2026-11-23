import asyncio
import datetime
import json
import re
import traceback
import unicodedata
from pathlib import Path
from urllib.parse import parse_qs
import numpy as np
import pandas as pd
from bs4 import BeautifulSoup
from playwright.async_api import async_playwright

# =============================================================================
# MAPEAMENTO DE ENTRADA, SAÍDA E CONFIGURAÇÕES (PADRÃO AURORA)
# =============================================================================
URL_REPASSE = "https://www.sefaz.pe.gov.br:8443/RPM/Scripts/TransfConstitucionalCons.asp"

FORCAR_RECOLETA = False
CONCORRENCIA_REQUISICOES = 5

# 1. Diretório Bruto / Conferência Humana (.xlsx e checkpoints)
DIR_RAW = Path(r"C:\dados\dados brutos\aurora\efisco")
DIR_RAW.mkdir(parents=True, exist_ok=True)

DIR_CHECKPOINT = DIR_RAW / "checkpoints_repasse"
DIR_CHECKPOINT.mkdir(parents=True, exist_ok=True)

ARQUIVO_RAW_XLSX = DIR_RAW / "repasse_constitucional_municipios_pe.xlsx"

# 2. Diretório Analítico Oficial (JSON e Parquet)
DIR_ETL = Path(r"C:\dados\aurora")
DIR_ETL.mkdir(parents=True, exist_ok=True)

DIR_PARQUET = DIR_ETL / "parquet"
DIR_PARQUET.mkdir(parents=True, exist_ok=True)

PARQUET_REPASSE = DIR_PARQUET / "sefaz_repasse_constitucional_municipios.parquet"
JSON_REPASSE = DIR_ETL / "sefaz_repasse_constitucional_municipios.json"
LOG_FILE = DIR_ETL / "log_sefaz_repasse.txt"

MESES_MAP = {
    "JANEIRO": 1, "JAN": 1,
    "FEVEREIRO": 2, "FEV": 2,
    "MARCO": 3, "MAR": 3,
    "ABRIL": 4, "ABR": 4,
    "MAIO": 5, "MAI": 5,
    "JUNHO": 6, "JUN": 6,
    "JULHO": 7, "JUL": 7,
    "AGOSTO": 8, "AGO": 8,
    "SETEMBRO": 9, "SET": 9,
    "OUTUBRO": 10, "OUT": 10,
    "NOVEMBRO": 11, "NOV": 11,
    "DEZEMBRO": 12, "DEZ": 12,
}

CODIGOS_NAO_MUNICIPIO = {"76", "999"}  # DRT-DEAR e OUTROS ESTADOS


def log(mensagem: str):
    """Grava o log com data/hora no console e em C:\\dados\\aurora\\log_sefaz_repasse.txt."""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    linha_segura = linha.encode("utf-8", errors="replace").decode("utf-8", errors="replace")
    try:
        print(linha_segura)
    except Exception:
        print(linha_segura.encode("ascii", errors="replace").decode("ascii"))
    with open(LOG_FILE, "a", encoding="utf-8", errors="replace") as f:
        f.write(linha_segura + "\n")


def normalizar_texto_ascii(texto: str) -> str:
    s = unicodedata.normalize("NFKD", str(texto)).encode("ascii", "ignore").decode("ascii")
    return s.strip()


def normalizar_nome_coluna(texto: str) -> str:
    s = normalizar_texto_ascii(texto).lower()
    s = re.sub(r"[^a-z0-9]+", "_", s).strip("_")
    return s or "coluna"


def limpar_numero_br(val) -> float:
    """Converte strings numéricas PT-BR ('1.234.567,89' ou '(1.234,56)') para float."""
    if pd.isna(val) or val is None:
        return np.nan
    if isinstance(val, (int, float, np.integer, np.floating)):
        return float(val)

    s = str(val).strip()
    if not s or s in {"-", "–", "—", "n/d", "N/D", "N/A", "nan", "None", "\xa0"}:
        return np.nan

    s = re.sub(r"(?i)(R\$|%|\s)", "", s)
    negativo = False
    if s.startswith("(") and s.endswith(")"):
        negativo = True
        s = s[1:-1]

    m = re.search(r"-?\d{1,3}(?:\.\d{3})+(?:,\d+)?|-?\d+(?:,\d+)?", s)
    if not m:
        return np.nan

    num_str = m.group(0).replace(".", "").replace(",", ".")
    try:
        res = float(num_str)
        return -res if negativo else res
    except ValueError:
        return np.nan


def decodificar_html_asp(raw_bytes: bytes) -> str:
    """Decodifica respostas Classic ASP (UTF-8 ou Windows-1252/Latin-1)."""
    for enc in ("utf-8", "windows-1252", "latin-1"):
        try:
            return raw_bytes.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw_bytes.decode("latin-1", errors="replace")


def extrair_tabela_repasse(html_str: str, cod_mun: str, nome_mun: str, ano: int) -> pd.DataFrame:
    """Localiza a tabela de repasses no HTML retornado, expande cabeçalhos e tipa os dados."""
    soup = BeautifulSoup(html_str, "html.parser")
    candidatas = []

    for tbl in soup.find_all("table"):
        if tbl.find(["select", "input"]):
            continue
        trs = tbl.find_all("tr")
        if len(trs) < 2:
            continue

        texto_tbl = normalizar_texto_ascii(tbl.get_text(" ", strip=True)).upper()
        score = len(trs)
        if any(k in texto_tbl for k in ["JANEIRO", "FEVEREIRO", "ICMS", "TOTAL", "QUOTA"]):
            score += 100
        candidatas.append((score, tbl))

    if not candidatas:
        return pd.DataFrame()

    candidatas.sort(key=lambda x: x[0], reverse=True)
    tabela = candidatas[0][1]
    trs = tabela.find_all("tr")

    header_rows = []
    data_start_idx = 0
    for idx, tr in enumerate(trs):
        cells = tr.find_all(["th", "td"])
        if not cells:
            continue
        first_txt = normalizar_texto_ascii(cells[0].get_text(" ", strip=True)).upper()
        if first_txt in MESES_MAP or "TOTAL" in first_txt:
            data_start_idx = idx
            break

        if tr.find("th") or idx == 0:
            header_rows.append(tr)
            data_start_idx = idx + 1
        else:
            valores_num = [limpar_numero_br(c.get_text(strip=True)) for c in cells]
            if all(pd.isna(v) for v in valores_num):
                header_rows.append(tr)
                data_start_idx = idx + 1
            else:
                break

    grid = {}
    for r_idx, tr in enumerate(header_rows):
        c_idx = 0
        for cell in tr.find_all(["th", "td"]):
            while (r_idx, c_idx) in grid:
                c_idx += 1
            rs = int(cell.get("rowspan", 1))
            cs = int(cell.get("colspan", 1))
            txt = cell.get_text(" ", strip=True)
            for dr in range(rs):
                for dc in range(cs):
                    grid[(r_idx + dr, c_idx + dc)] = txt
            c_idx += cs

    max_cols = max((c for _, c in grid.keys()), default=0) + 1 if grid else 0
    colunas = []
    for c in range(max_cols):
        partes = []
        for r in range(len(header_rows)):
            val = grid.get((r, c), "").strip()
            if val and (not partes or val != partes[-1]):
                partes.append(val)
        nome_col = normalizar_nome_coluna("_".join(partes)) if partes else f"col_{c+1}"
        if nome_col in colunas:
            nome_col = f"{nome_col}_{c+1}"
        colunas.append(nome_col)

    linhas_dados = []
    for tr in trs[data_start_idx:]:
        cells = [td.get_text(" ", strip=True) for td in tr.find_all(["td", "th"])]
        if not cells or all(c == "" for c in cells):
            continue
        if len(cells) == 1 and len(cells[0]) > 40:
            continue
        linhas_dados.append(cells)

    if not linhas_dados:
        return pd.DataFrame()

    n_cols = max(len(r) for r in linhas_dados)
    if len(colunas) < n_cols:
        colunas.extend([f"col_{i+1}" for i in range(len(colunas), n_cols)])
    elif len(colunas) > n_cols:
        colunas = colunas[:n_cols]

    linhas_padronizadas = [r + [""] * (n_cols - len(r)) for r in linhas_dados]
    df = pd.DataFrame(linhas_padronizadas, columns=colunas)

    col_periodo = df.columns[0]
    if col_periodo not in {"mes", "periodo", "competencia"}:
        df.rename(columns={col_periodo: "periodo_label"}, inplace=True)
        col_periodo = "periodo_label"

    for col in df.columns:
        if col == col_periodo:
            continue
        serie_num = df[col].apply(limpar_numero_br)
        if serie_num.notna().sum() > 0:
            df[col] = serie_num.astype("float64")

    labels_norm = df[col_periodo].apply(lambda x: normalizar_texto_ascii(x).upper())
    df.insert(0, "cod_municipio_sefaz", str(cod_mun))
    df.insert(1, "municipio", str(nome_mun).strip())
    df.insert(2, "is_municipio_pe", str(cod_mun) not in CODIGOS_NAO_MUNICIPIO)
    df.insert(3, "ano_referencia", int(ano))

    meses_num = labels_norm.map(MESES_MAP)
    df.insert(4, "mes_numero", pd.Series(meses_num, dtype="Int64"))
    df.insert(
        5,
        "competencia",
        [
            f"{int(ano):04d}-{int(m):02d}-01" if pd.notna(m) else None
            for m in meses_num
        ],
    )
    df.insert(
        6,
        "is_total_anual",
        labels_norm.apply(lambda t: "TOTAL" in t or "SUBTOTAL" in t),
    )

    return df


def serializar_valor_json(v):
    if pd.isna(v):
        return None
    if isinstance(v, (np.integer,)):
        return int(v)
    if isinstance(v, (np.floating,)):
        return round(float(v), 4)
    if isinstance(v, (np.bool_,)):
        return bool(v)
    return v


async def selecionar_anual_e_consultar_ui(page, cod_mun: str, ano: str):
    """Executa uma consulta completa pela interface do navegador e retorna o HTML resultante."""
    await page.select_option("select[name='cboMunicipio']", value=str(cod_mun))
    await page.wait_for_timeout(300)

    radio_anual = page.locator("input[type='radio'][value='A'], input[type='radio'][value*='Anual']").first
    if await radio_anual.count() > 0:
        if not await radio_anual.is_checked():
            await radio_anual.check(force=True)
    else:
        lbl_anual = page.get_by_text("Anual", exact=True).first
        if await lbl_anual.count() > 0:
            await lbl_anual.click(force=True)

    await page.select_option("select[name='cboAno']", value=str(ano))
    await page.wait_for_timeout(200)

    btn_localizar = page.locator(
        "input[value*='Localizar'], button:has-text('Localizar'), input[type='submit']"
    ).first

    popup_page = None

    def on_popup(new_p):
        nonlocal popup_page
        popup_page = new_p

    page.context.on("page", on_popup)
    try:
        async with page.expect_navigation(wait_until="domcontentloaded", timeout=15000):
            await btn_localizar.click(force=True)
    except Exception:
        await page.wait_for_timeout(1500)
    finally:
        page.context.remove_listener("page", on_popup)

    target_page = popup_page if popup_page else page
    await target_page.wait_for_load_state("domcontentloaded")
    html = await target_page.content()

    if popup_page:
        await popup_page.close()

    return html


async def executar_rpa_repasse():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INICIO DA PIPELINE - SEFAZ-PE REPASSE CONSTITUCIONAL ===\n")
        f.write(f"URL Alvo      : {URL_REPASSE}\n")
        f.write(f"Destino Bruto : {DIR_RAW}\n")
        f.write(f"Destino ETL   : {DIR_ETL}\n")

    async with async_playwright() as p:
        try:
            log("1. Inicializando Chromium (ignore_https_errors=True para porta 8443)...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(
                viewport={"width": 1440, "height": 900},
                ignore_https_errors=True,
            )
            page = await context.new_page()

            log(f"2. Acessando sistema RPM SEFAZ-PE: {URL_REPASSE}")
            await page.goto(URL_REPASSE, wait_until="domcontentloaded", timeout=60000)
            await page.wait_for_selector("select[name='cboMunicipio']", timeout=30000)

            municipios = await page.evaluate("""
            () => {
                const sel = document.querySelector("select[name='cboMunicipio']");
                return Array.from(sel.options)
                    .map(o => ({ value: o.value.trim(), text: o.text.trim() }))
                    .filter(o => o.value !== "" && o.value !== "0" && o.value !== "9999");
            }
            """)

            anos = await page.evaluate("""
            () => {
                const sel = document.querySelector("select[name='cboAno']");
                return Array.from(sel.options)
                    .map(o => o.value.trim())
                    .filter(v => /^\\d{4}$/.test(v));
            }
            """)

            anos = sorted(anos, reverse=True)
            log(f"   Municipios/Entidades mapeados: {len(municipios)} (excluindo '0' e '9999')")
            log(f"   Anos mapeados: {len(anos)} ({anos[-1]} a {anos[0]})")

            log("3. Calibrando requisicao ASP com consulta de teste (RECIFE)...")
            req_capturada = {}

            def capturar_req(request):
                if request.resource_type == "document" and "TransfConstitucional" in request.url:
                    req_capturada["url"] = request.url
                    req_capturada["method"] = request.method
                    req_capturada["post_data"] = request.post_data

            page.on("request", capturar_req)
            mun_teste = next((m for m in municipios if m["value"] == "1"), municipios[0])
            ano_teste = anos[0]
            html_teste = await selecionar_anual_e_consultar_ui(page, mun_teste["value"], ano_teste)
            page.remove_listener("request", capturar_req)

            df_teste = extrair_tabela_repasse(html_teste, mun_teste["value"], mun_teste["text"], int(ano_teste))
            log(f"   [CALIBRACAO] Tabela detectada com {len(df_teste)} linhas e colunas: {list(df_teste.columns)}")

            modo_http_rapido = False
            template_payload = {}
            target_url = req_capturada.get("url", URL_REPASSE)

            if req_capturada.get("post_data"):
                parsed = parse_qs(req_capturada["post_data"], keep_blank_values=True)
                template_payload = {k: v[0] for k, v in parsed.items()}
                payload_check = dict(template_payload)
                payload_check["cboMunicipio"] = mun_teste["value"]
                payload_check["cboAno"] = ano_teste
                resp_check = await context.request.post(
                    target_url,
                    form=payload_check,
                    timeout=30000,
                )
                html_check = decodificar_html_asp(await resp_check.body())
                df_check = extrair_tabela_repasse(html_check, mun_teste["value"], mun_teste["text"], int(ano_teste))
                if not df_check.empty and len(df_check) == len(df_teste):
                    modo_http_rapido = True
                    log("   [OK] Modo HTTP Paralelo validado na sessao ASP!")

            if not modo_http_rapido:
                log("   [INFO] Usando navegacao paralela via abas Chromium...")

            dfs_todos_anos = []
            semaforo = asyncio.Semaphore(CONCORRENCIA_REQUISICOES)

            for idx_ano, ano_str in enumerate(anos, start=1):
                ano_int = int(ano_str)
                arq_ckpt = DIR_CHECKPOINT / f"repasse_sefaz_{ano_int}.parquet"

                if arq_ckpt.exists() and not FORCAR_RECOLETA:
                    df_ano_ckpt = pd.read_parquet(arq_ckpt)
                    if not df_ano_ckpt.empty:
                        dfs_todos_anos.append(df_ano_ckpt)
                        log(f"[{idx_ano}/{len(anos)}] Ano {ano_int}: carregado do checkpoint ({len(df_ano_ckpt)} linhas).")
                        continue

                log(f"[{idx_ano}/{len(anos)}] Coletando Ano {ano_int} ({len(municipios)} municipios)...")
                dfs_ano = []

                if modo_http_rapido:
                    async def consultar_mun_http(mun_item):
                        async with semaforo:
                            payload = dict(template_payload)
                            payload["cboMunicipio"] = mun_item["value"]
                            payload["cboAno"] = ano_str
                            for tentativa in range(3):
                                try:
                                    resp = await context.request.post(
                                        target_url,
                                        form=payload,
                                        timeout=45000,
                                    )
                                    raw_bytes = await resp.body()
                                    html_resp = decodificar_html_asp(raw_bytes)
                                    df_m = extrair_tabela_repasse(
                                        html_resp, mun_item["value"], mun_item["text"], ano_int
                                    )
                                    await asyncio.sleep(0.1)
                                    return df_m
                                except Exception:
                                    await asyncio.sleep(1.0 * (tentativa + 1))
                            return pd.DataFrame()

                    tarefas = [consultar_mun_http(m) for m in municipios]
                    resultados = await asyncio.gather(*tarefas)
                    dfs_ano = [d for d in resultados if d is not None and not d.empty]
                else:
                    fila = asyncio.Queue()
                    for m in municipios:
                        fila.put_nowait(m)

                    async def worker_browser():
                        worker_page = await context.new_page()
                        await worker_page.goto(URL_REPASSE, wait_until="domcontentloaded", timeout=60000)
                        while not fila.empty():
                            mun_item = await fila.get()
                            try:
                                html_resp = await selecionar_anual_e_consultar_ui(
                                    worker_page, mun_item["value"], ano_str
                                )
                                df_m = extrair_tabela_repasse(
                                    html_resp, mun_item["value"], mun_item["text"], ano_int
                                )
                                if not df_m.empty:
                                    dfs_ano.append(df_m)
                                if "cboMunicipio" not in await worker_page.content():
                                    await worker_page.goto(URL_REPASSE, wait_until="domcontentloaded", timeout=60000)
                            except Exception:
                                try:
                                    await worker_page.goto(URL_REPASSE, wait_until="domcontentloaded", timeout=60000)
                                except Exception:
                                    pass
                            finally:
                                fila.task_done()
                        await worker_page.close()

                    workers = [asyncio.create_task(worker_browser()) for _ in range(min(4, CONCORRENCIA_REQUISICOES))]
                    await asyncio.gather(*workers)

                if dfs_ano:
                    df_ano_consolidado = pd.concat(dfs_ano, ignore_index=True)
                    df_ano_consolidado.to_parquet(arq_ckpt, index=False)
                    dfs_todos_anos.append(df_ano_consolidado)
                    log(f"   [OK] Ano {ano_int} concluido e salvo em checkpoint ({len(df_ano_consolidado)} linhas).")
                else:
                    log(f"   [AVISO] Nenhum registro extraido para o ano {ano_int}.")

            await browser.close()

            if not dfs_todos_anos:
                log("[ERRO CRITICO] Nenhuma base foi extraida do sistema RPM.")
                return

            log("==================================================")
            log("4. Consolidando serie historica completa (todos os municipios e anos)...")
            df_final = pd.concat(dfs_todos_anos, ignore_index=True)

            df_final["ano_referencia"] = pd.to_numeric(df_final["ano_referencia"], errors="coerce").astype("Int64")
            df_final["mes_numero"] = pd.to_numeric(df_final["mes_numero"], errors="coerce").astype("Int64")
            df_final["is_municipio_pe"] = df_final["is_municipio_pe"].astype(bool)
            df_final["is_total_anual"] = df_final["is_total_anual"].astype(bool)

            df_final.sort_values(
                by=["ano_referencia", "municipio", "is_total_anual", "mes_numero"],
                ascending=[False, True, True, True],
                inplace=True,
            )
            df_final.reset_index(drop=True, inplace=True)

            log(f"   Total de registros consolidados: {len(df_final)}")
            log(f"   Municipios/Entidades unicos    : {df_final['municipio'].nunique()}")
            log(f"   Anos cobertos                  : {df_final['ano_referencia'].min()} a {df_final['ano_referencia'].max()}")

            # A. Parquet Analítico
            df_final.to_parquet(PARQUET_REPASSE, index=False)
            log(f"   [ARQUIVO] Parquet analitico : {PARQUET_REPASSE}")

            # B. JSON Estruturado
            registros = df_final.to_dict(orient="records")
            registros_limpos = [
                {k: serializar_valor_json(v) for k, v in r.items()}
                for r in registros
            ]
            with open(JSON_REPASSE, "w", encoding="utf-8") as f:
                json.dump(registros_limpos, f, ensure_ascii=False, indent=2, allow_nan=False)
            log(f"   [ARQUIVO] JSON definitivo   : {JSON_REPASSE}")

            # C. Excel Bruto/Conferência (salvo em C:\dados\dados brutos\aurora\efisco)
            if len(df_final) <= 1_000_000:
                df_final.to_excel(ARQUIVO_RAW_XLSX, index=False, engine="openpyxl")
                log(f"   [ARQUIVO] Excel conferencia : {ARQUIVO_RAW_XLSX}")
            else:
                arq_csv_fallback = ARQUIVO_RAW_XLSX.with_suffix(".csv")
                df_final.to_csv(arq_csv_fallback, sep=";", index=False, encoding="utf-8-sig")
                log(f"   [ARQUIVO] CSV conferencia   : {arq_csv_fallback}")

            log("==================================================")
            log("ETL SEFAZ-PE (REPASSE CONSTITUCIONAL) CONCLUIDO!")
            log("==================================================")

        except Exception as e:
            log(f"[ERRO CRITICO]: {str(e)}")
            log(traceback.format_exc())


if __name__ == "__main__":
    asyncio.run(executar_rpa_repasse())

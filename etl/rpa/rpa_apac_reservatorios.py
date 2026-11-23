import csv
import json
from pathlib import Path
import re
import requests

PORTAL_BASE = (
    "https://geoportal.apac.pe.gov.br/portal/sharing/rest/content/items"
)
DASHBOARD_ID = "407b6eec7fc4448b94055cf02b53ebc0"

DIRETORIO_SAIDA = Path(r"C:\dados\dados brutos\aurora\apac")
DIRETORIO_SAIDA.mkdir(parents=True, exist_ok=True)

session = requests.Session()
session.headers.update({"User-Agent": "Mozilla/5.0"})


def coletar_fontes_dashboard(obj, webmaps=None, urls_diretas=None):
    """Varre o JSON do Dashboard atrás de WebMaps e URLs diretas de widgets."""
    if webmaps is None:
        webmaps = set()
    if urls_diretas is None:
        urls_diretas = set()

    if isinstance(obj, dict):
        if obj.get("type") == "mapWidget" and "itemId" in obj:
            webmaps.add(obj["itemId"])
        if "url" in obj and isinstance(obj["url"], str):
            if any(k in obj["url"] for k in ("MapServer", "FeatureServer")):
                urls_diretas.add(obj["url"])
        for v in obj.values():
            coletar_fontes_dashboard(v, webmaps, urls_diretas)
    elif isinstance(obj, list):
        for item in obj:
            coletar_fontes_dashboard(item, webmaps, urls_diretas)

    return webmaps, urls_diretas


def expandir_servico(url, titulo_base):
    """Se a URL for raiz de MapServer/FeatureServer, expande sublayers e tables."""
    url_limpa = url.rstrip("/")
    if url_limpa.endswith(("MapServer", "FeatureServer")):
        meta = session.get(f"{url_limpa}?f=json", timeout=30).json()
        alvos = []
        for item in meta.get("layers", []) + meta.get("tables", []):
            nome = re.sub(r"[^\w\-]+", "_", f"{titulo_base}_{item['name']}")
            alvos.append((f"{url_limpa}/{item['id']}", nome))
        return alvos
    return [(url_limpa, re.sub(r"[^\w\-]+", "_", titulo_base))]


def baixar_dados_e_atributos(layer_url, nome_arquivo):
    """Baixa todos os registros e atributos (outFields=*) e salva em CSV e JSON."""
    meta = session.get(f"{layer_url}?f=json", timeout=30).json()
    campos = [f["name"] for f in meta.get("fields", [])]
    print(f"\n-> {nome_arquivo} | URL: {layer_url}")
    print(f"   Colunas: {', '.join(campos) if campos else 'N/A'}")

    query_url = f"{layer_url}/query"
    offset = 0
    batch_size = 1000
    registros = []

    while True:
        params = {
            "where": "1=1",
            "outFields": "*",
            "returnGeometry": "true",
            "outSR": "4326",
            "f": "json",
            "resultOffset": offset,
            "resultRecordCount": batch_size,
        }
        resp = session.get(query_url, params=params, timeout=30).json()
        if "error" in resp:
            print(f"   Erro ao consultar: {resp['error']}")
            return

        batch = resp.get("features", [])
        if not batch:
            break

        registros.extend(batch)
        if len(batch) < batch_size and not resp.get("exceededTransferLimit"):
            break
        offset += batch_size

    if not registros:
        print("   Nenhum registro retornado.")
        return

    caminho_json = DIRETORIO_SAIDA / f"{nome_arquivo}.json"
    caminho_csv = DIRETORIO_SAIDA / f"{nome_arquivo}.csv"

    with open(caminho_json, "w", encoding="utf-8") as f:
        json.dump(registros, f, ensure_ascii=False, indent=2)

    linhas_csv = []
    for feat in registros:
        linha = dict(feat.get("attributes", {}))
        geom = feat.get("geometry") or {}
        if "x" in geom and "y" in geom:
            linha["longitude"] = geom["x"]
            linha["latitude"] = geom["y"]
        linhas_csv.append(linha)

    colunas_csv = list(linhas_csv[0].keys())
    with open(caminho_csv, "w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=colunas_csv)
        writer.writeheader()
        writer.writerows(linhas_csv)

    print(f"   Salvo: {caminho_csv} ({len(registros)} registros)")


dash_data = session.get(
    f"{PORTAL_BASE}/{DASHBOARD_ID}/data?f=json", timeout=30
).json()
webmap_ids, urls_widgets = coletar_fontes_dashboard(dash_data)

endpoints = []

for wm_id in webmap_ids:
    wm_data = session.get(f"{PORTAL_BASE}/{wm_id}/data?f=json", timeout=30).json()
    for item in wm_data.get("operationalLayers", []) + wm_data.get("tables", []):
        if item.get("url"):
            endpoints.extend(
                expandir_servico(item["url"], item.get("title", "camada"))
            )

for i, u in enumerate(urls_widgets):
    endpoints.extend(expandir_servico(u, f"widget_{i}"))

vistos = set()
for url, nome in endpoints:
    if url not in vistos:
        vistos.add(url)
        baixar_dados_e_atributos(url, nome)

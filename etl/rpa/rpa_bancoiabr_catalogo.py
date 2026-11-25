import json
import re
import time
from datetime import datetime
from pathlib import Path
from urllib.parse import urljoin

import pandas as pd
import requests
from bs4 import BeautifulSoup

CATALOG_URL = "https://bancobrasileiro.ia.br/catalogo"
OUTPUT_DIR = Path(r"C:\dados\dados brutos\aurora\inovacao\bancobrasileiro_ia")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


def create_session() -> requests.Session:
    session = requests.Session()
    session.headers.update({
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/124.0.0.0 Safari/537.36"
        ),
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,application/json,*/*;q=0.8",
        "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
        "Referer": CATALOG_URL,
    })
    return session


def find_largest_dict_list(obj, min_len: int = 5) -> list[dict] | None:
    """Procura recursivamente a maior lista de objetos (soluções) dentro de um JSON."""
    best = None
    if isinstance(obj, list):
        if len(obj) >= min_len and all(isinstance(x, dict) for x in obj):
            best = obj
        for item in obj:
            cand = find_largest_dict_list(item, min_len)
            if cand and (best is None or len(cand) > len(best)):
                best = cand
    elif isinstance(obj, dict):
        for val in obj.values():
            cand = find_largest_dict_list(val, min_len)
            if cand and (best is None or len(cand) > len(best)):
                best = cand
    return best


def try_embedded_json(soup: BeautifulSoup) -> list[dict] | None:
    """Tenta extrair dados embutidos em __NEXT_DATA__ ou scripts JSON da página."""
    next_script = soup.find("script", id="__NEXT_DATA__")
    if next_script and next_script.string:
        data = json.loads(next_script.string)
        records = find_largest_dict_list(data)
        if records:
            print(f"[INFO] Extraídos {len(records)} registros de __NEXT_DATA__.")
            return records

    for script in soup.find_all("script"):
        text = script.string or ""
        if "Prefeitura Municipal" in text or "software público" in text or "catalog" in text.lower():
            for match in re.finditer(r"(\[\s*\{.*?\}\s*\])", text, re.DOTALL):
                try:
                    parsed = json.loads(match.group(1))
                    if isinstance(parsed, list) and len(parsed) >= 3:
                        print(f"[INFO] Extraídos {len(parsed)} registros de script inline.")
                        return parsed
                except json.JSONDecodeError:
                    continue
    return None


def discover_api_endpoints(session: requests.Session, soup: BeautifulSoup, base_url: str) -> list[str]:
    """Varre os bundles JS carregados pela página para encontrar URLs reais de API."""
    candidates = set()
    script_urls = [
        urljoin(base_url, s["src"])
        for s in soup.find_all("script", src=True)
    ]

    api_pattern = re.compile(
        r"""["'`]((?:https?://[^"'`\s]+)?/(?:api|v1|v2|rest|graphql|_next/data)[^"'`\s]*)["'`]"""
    )

    for js_url in script_urls[:15]:
        try:
            r = session.get(js_url, timeout=15)
            if r.ok:
                for m in api_pattern.findall(r.text):
                    full = urljoin(base_url, m)
                    if not full.endswith((".js", ".css", ".png", ".svg")):
                        candidates.add(full)
        except requests.RequestException:
            continue

    return sorted(candidates)


def parse_ssr_cards(soup: BeautifulSoup) -> list[dict]:
    """Fallback: extrai os dados diretamente dos cards HTML renderizados na página."""
    records = []
    # Busca os títulos dos cards (classes identificadas no snippet enviado)
    title_spans = soup.find_all("span", class_=re.compile(r"text-lg.*font-bold"))

    for title_el in title_spans:
        card = title_el.find_parent("div", class_=re.compile(r"flex-col"))
        if not card:
            continue

        spans = [s.get_text(" ", strip=True) for s in card.find_all("span") if s.get_text(strip=True)]
        paragraphs = [p.get_text(" ", strip=True) for p in card.find_all("p") if p.get_text(strip=True)]
        link_el = card.find_parent("a", href=True) or card.find("a", href=True)

        records.append({
            "titulo": title_el.get_text(" ", strip=True),
            "orgao": paragraphs[0] if len(paragraphs) > 0 else "",
            "descricao": paragraphs[1] if len(paragraphs) > 1 else "",
            "tags_e_status": " | ".join(spans),
            "url_detalhe": urljoin(CATALOG_URL, link_el["href"]) if link_el else "",
        })

    return records


def normalize_for_excel(records: list[dict]) -> pd.DataFrame:
    df = pd.json_normalize(records, sep="_")
    for col in df.columns:
        df[col] = df[col].apply(
            lambda x: ", ".join(str(i) for i in x) if isinstance(x, list) else x
        )
    for col in df.select_dtypes(include=["object", "string"]).columns:
        df[col] = df[col].astype(str).str.replace(r"\s+", " ", regex=True).str.strip()
        df[col] = df[col].replace({"nan": "", "None": ""})

    df.insert(0, "data_extracao", datetime.now().strftime("%Y-%m-%d"))
    return df


def main():
    session = create_session()
    print(f"Acessando {CATALOG_URL}...")
    resp = session.get(CATALOG_URL, timeout=30)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "lxml")

    # Tentativa 1: JSON embutido na página (__NEXT_DATA__ / scripts)
    records = try_embedded_json(soup)

    # Tentativa 2: Descobrir endpoints nos bundles JS
    if not records:
        endpoints = discover_api_endpoints(session, soup, CATALOG_URL)
        print(f"[INFO] Endpoints candidatos encontrados nos JS: {endpoints}")
        for ep in endpoints:
            try:
                r = session.get(ep, timeout=20)
                if r.ok and "application/json" in r.headers.get("Content-Type", ""):
                    found = find_largest_dict_list(r.json(), min_len=3)
                    if found:
                        print(f"[INFO] Dados obtidos do endpoint: {ep}")
                        records = found
                        break
            except Exception:
                continue

    # Tentativa 3: Parse direto dos cards HTML (SSR)
    if not records:
        print("[INFO] Tentando extração direta dos cards HTML...")
        records = parse_ssr_cards(soup)

    if not records:
        print(
            "\n[AVISO] Os dados são carregados dinamicamente via XHR protegido ou paginação JS.\n"
            "Abra o DevTools (F12 -> Network -> Fetch/XHR) em https://bancobrasileiro.ia.br/catalogo "
            "e cole aqui a URL da requisição que traz a lista."
        )
        return

    # Salva JSON bruto
    json_path = OUTPUT_DIR / "catalogo_ia_bruto.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(records, f, ensure_ascii=False, indent=2)

    # Salva CSV Excel PT-BR
    df = normalize_for_excel(records)
    csv_path = OUTPUT_DIR / "catalogo_ia.csv"
    df.to_csv(
        csv_path,
        index=False,
        encoding="utf-8-sig",
        sep=";",
        decimal=",",
        lineterminator="\r\n",
    )

    print(f"\n[OK] Total extraído: {len(df)} soluções | {len(df.columns)} colunas")
    print(f"JSON bruto: {json_path.resolve()}")
    print(f"CSV Excel:  {csv_path.resolve()}")


if __name__ == "__main__":
    main()

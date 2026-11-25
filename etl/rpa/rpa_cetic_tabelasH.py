import json
import re
import time
from pathlib import Path
from urllib.parse import urljoin

import pandas as pd
import requests
from bs4 import BeautifulSoup

# Configurações de origem e destino
BASE_URL = "https://cetic.br/pt/tics/governo/2025/orgaos/"
OUTPUT_DIR = Path(r"C:\dados\dados brutos\aurora\inovacao\cetic")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


def extract_year_from_url(url: str) -> int:
    """Extrai o ano da pesquisa a partir da URL (ex.: /2025/ -> 2025)."""
    match = re.search(r"/(\d{4})/", url)
    if match:
        return int(match.group(1))
    raise ValueError(f"Não foi possível identificar o ano na URL: {url}")


def create_session() -> requests.Session:
    session = requests.Session()
    session.headers.update({
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/124.0.0.0 Safari/537.36"
        ),
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
    })
    return session


def discover_h_tables(session: requests.Session, index_url: str, year: int) -> list[tuple[str, str]]:
    """Descobre dinamicamente todas as URLs de tabelas da categoria H na página índice."""
    resp = session.get(index_url, timeout=30)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "lxml")

    discovered = {}
    pattern = re.compile(rf"/pt/tics/governo/{year}/orgaos/(H[0-9A-Z]+)/?$", re.IGNORECASE)

    for a_tag in soup.find_all("a", href=True):
        full_url = urljoin(index_url, a_tag["href"].strip())
        match = pattern.search(full_url)
        if match:
            code = match.group(1).upper()
            clean_url = full_url if full_url.endswith("/") else f"{full_url}/"
            discovered[code] = clean_url

    return sorted(discovered.items(), key=lambda item: item[0])


def clean_dataframe_for_excel(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Achata cabeçalhos multi-nível, limpa quebras de linha e adiciona a coluna 'ano'."""
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [
            " - ".join(
                str(col).strip()
                for col in cols
                if str(col).strip() and not str(col).startswith("Unnamed:")
            )
            for cols in df.columns.values
        ]
    else:
        df.columns = [str(c).strip() for c in df.columns]

    # Remove linhas totalmente vazias
    df = df.dropna(how="all").reset_index(drop=True)

    # Normaliza espaços e quebras de linha internas para não quebrar células no Excel
    df.columns = [re.sub(r"\s+", " ", col).strip() for col in df.columns]
    for col in df.select_dtypes(include=["object", "string"]).columns:
        df[col] = df[col].astype(str).str.replace(r"\s+", " ", regex=True).str.strip()
        df[col] = df[col].replace({"nan": "", "None": ""})

    # Insere a coluna 'ano' como a primeira coluna da tabela
    df.insert(0, "ano", year)
    return df


def extract_and_save_table(session: requests.Session, code: str, url: str, year: int) -> dict:
    """Extrai a tabela do indicador e salva os arquivos brutos (JSON + CSV padrão Excel)."""
    resp = session.get(url, timeout=30)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "lxml")

    title_tag = soup.find(["h1", "h2"])
    title = title_tag.get_text(" ", strip=True) if title_tag else code

    export_links = [
        urljoin(url, a["href"])
        for a in soup.find_all("a", href=True)
        if any(ext in a["href"].lower() for ext in (".csv", ".xls", ".xlsx", ".json"))
    ]

    tables = pd.read_html(resp.text, decimal=",", thousands=".")
    if not tables:
        raise ValueError(f"Nenhuma tabela encontrada em {url}")

    df = max(tables, key=lambda t: t.shape[0] * t.shape[1])
    df = clean_dataframe_for_excel(df, year=year)

    # 1. Salva JSON bruto (backup auditável com metadados + registros)
    raw_payload = {
        "ano": year,
        "indicador": code,
        "titulo": title,
        "url_origem": url,
        "links_exportacao_detectados": export_links,
        "total_linhas": len(df),
        "colunas": list(df.columns),
        "dados": json.loads(df.to_json(orient="records", force_ascii=False)),
    }
    json_path = OUTPUT_DIR / f"{code}_{year}.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(raw_payload, f, ensure_ascii=False, indent=2)

    # 2. Salva CSV no padrão Excel PT-BR (UTF-8 com BOM, separador ';' e quebra de linha Windows)
    csv_path = OUTPUT_DIR / f"{code}_{year}.csv"
    df.to_csv(
    csv_path,
    index=False,
    encoding="utf-8-sig",
    sep=";",
    decimal=",",
    lineterminator="\r\n",
    na_rep="",  # Evita que nulos apareçam como a palavra "NaN"
)

    return {
        "indicador": code,
        "ano": year,
        "linhas": len(df),
        "colunas": len(df.columns),
        "csv": str(csv_path),
    }


def main():
    year = extract_year_from_url(BASE_URL)
    session = create_session()

    print(f"Buscando tabelas H ({year}) em: {BASE_URL}")
    targets = discover_h_tables(session, BASE_URL, year)

    if not targets:
        print("Nenhuma tabela H encontrada na página índice.")
        return

    print(f"Encontradas {len(targets)} tabelas: {[code for code, _ in targets]}\n")

    for code, url in targets:
        try:
            info = extract_and_save_table(session, code, url, year)
            print(f"[OK] {code}_{year}: {info['linhas']} linhas x {info['colunas']} colunas")
        except Exception as e:
            print(f"[ERRO] {code} ({url}): {e}")
        time.sleep(0.5)

    print(f"\nConcluído! Arquivos salvos em: {OUTPUT_DIR.resolve()}")


if __name__ == "__main__":
    main()

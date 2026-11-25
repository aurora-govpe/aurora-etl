import io
import json
from datetime import datetime
from pathlib import Path

import pandas as pd
import requests

SHEET_ID = "15xmJz68eTa1dp9n_v1A4likPBWqIyfDvnGjWfJZB-Sg"  # tabela-brasil (Panorama Brasil - Todos)
CSV_URL = f"https://docs.google.com/spreadsheets/d/{SHEET_ID}/export?format=csv"

OUTPUT_DIR = Path(r"C:\dados\dados brutos\aurora\inovacao\lispbrasil")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


def main():
    print(f"Baixando tabela-brasil ({SHEET_ID})...")
    resp = requests.get(CSV_URL, timeout=30)
    resp.raise_for_status()
    resp.encoding = "utf-8"

    # Lê o CSV completo direto da planilha
    df = pd.read_csv(io.StringIO(resp.text))
    df = df.dropna(how="all").reset_index(drop=True)

    # Limpa quebras de linha internas e espaços extras para não quebrar células no Excel
    df.columns = [str(col).strip() for col in df.columns]
    for col in df.select_dtypes(include=["object", "string"]).columns:
        df[col] = df[col].astype(str).str.replace(r"\s+", " ", regex=True).str.strip()
        df[col] = df[col].replace({"nan": "", "None": ""})

    # Adiciona data de extração na primeira coluna
    df.insert(0, "data_extracao", datetime.now().strftime("%Y-%m-%d"))

    # 1. Salva JSON bruto completo (backup auditável)
    json_path = OUTPUT_DIR / "lispbrasil_panorama_bruto.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(df.to_dict(orient="records"), f, ensure_ascii=False, indent=2)

    # 2. Salva CSV no padrão Excel PT-BR (UTF-8 com BOM e separador ';')
    csv_path = OUTPUT_DIR / "lispbrasil_panorama.csv"
    df.to_csv(
        csv_path,
        index=False,
        encoding="utf-8-sig",
        sep=";",
        lineterminator="\r\n",
    )

    print(f"[OK] {len(df)} laboratórios | {len(df.columns)} colunas")
    print(f"Colunas: {list(df.columns)}")
    print(f"JSON bruto: {json_path.resolve()}")
    print(f"CSV Excel:  {csv_path.resolve()}")


if __name__ == "__main__":
    main()

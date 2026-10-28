import os
import json
import datetime
import traceback
from pathlib import Path
import pandas as pd
import requests
import urllib3

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

# =============================================================================
# MAPEAMENTO DE ENTRADA E SAÍDA
# =============================================================================
URL_DADOS_ABERTOS = "https://sibf.sudene.gov.br/dados_abertos/pleitos.csv"

# 1. Destino Bruto (Excel de conferência da base completa da SUDENE)
DIR_RAW = Path(r"C:\dados\dados brutos\aurora\incentivos")
DIR_RAW.mkdir(parents=True, exist_ok=True)
ARQUIVO_TEMP_CSV = DIR_RAW / "pleitos_temp.csv"
ARQUIVO_RAW_EXCEL = DIR_RAW / "incentivos_sudene_bruto.xlsx"

# 2. Destino Tratado (Pernambuco - JSON e Parquet)
DIR_ETL = Path(r"C:\dados\aurora")
DIR_ETL.mkdir(parents=True, exist_ok=True)
ARQUIVO_JSON = DIR_ETL / "incentivos_sudene_pe.json"

DIR_PARQUET = DIR_ETL / "parquet"
DIR_PARQUET.mkdir(parents=True, exist_ok=True)
ARQUIVO_PARQUET = DIR_PARQUET / "incentivos_sudene_pe.parquet"

LOG_FILE = DIR_ETL / "log_sudene.txt"

def log(mensagem):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")

def baixar_csv_temporario():
    """Baixa o arquivo CSV bruto via HTTP para consumo temporário."""
    log(f"1. Baixando CSV temporário da SUDENE: {URL_DADOS_ABERTOS}")
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        "Accept": "text/csv,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    }

    try:
        response = requests.get(URL_DADOS_ABERTOS, headers=headers, verify=False, timeout=60, stream=True)
        response.raise_for_status()

        with open(ARQUIVO_TEMP_CSV, "wb") as f:
            for chunk in response.iter_content(chunk_size=65536):
                if chunk:
                    f.write(chunk)

        log("   📥 CSV temporário obtido com sucesso.")
        return True
    except Exception as e:
        log(f"❌ [ERRO NO DOWNLOAD]: {e}")
        return False

def processar_bases():
    """Converte o bruto para Excel, remove o temporário, filtra PE e exporta JSON/Parquet."""
    log("2. Carregando dados para memória...")
    df = pd.read_csv(ARQUIVO_TEMP_CSV, sep=";", encoding="utf-8")
    log(f"   Total de pleitos no Brasil: {len(df)}")

    # -------------------------------------------------------------------------
    # A. Salvar base bruta completa em Excel e descartar CSV temporário
    # -------------------------------------------------------------------------
    log(f"3. Gerando planilha bruta de conferência em Excel: {ARQUIVO_RAW_EXCEL}")
    df.to_excel(ARQUIVO_RAW_EXCEL, index=False, engine="openpyxl")
    log("   ✅ Planilha bruta gerada.")

    if ARQUIVO_TEMP_CSV.exists():
        try:
            ARQUIVO_TEMP_CSV.unlink()
            log("   🗑️ Arquivo temporário (.csv) removido com sucesso.")
        except Exception as e:
            log(f"   ⚠️ Não foi possível excluir o temp CSV: {e}")

    # -------------------------------------------------------------------------
    # B. Filtrar e Tratar Base de Pernambuco
    # -------------------------------------------------------------------------
    log("4. Filtrando e tratando pleitos de Pernambuco (UF == 'PE')...")
    df["uf"] = df["uf"].astype(str).str.strip().str.upper()
    df_pe = df[df["uf"] == "PE"].copy()
    log(f"   🎯 Pleitos de Pernambuco: {len(df_pe)} registros em {df_pe['municipio'].nunique()} municípios.")

    # Identificadores e Strings
    df_pe["numero_processo"] = df_pe["numero_processo"].astype(str).str.strip()
    df_pe["cnpj"] = df_pe["cnpj"].astype(str).str.replace(r"\.0$", "", regex=True).str.zfill(14)
    df_pe["cnpj_formatado"] = df_pe["cnpj"].apply(
        lambda c: f"{c[:2]}.{c[2:5]}.{c[5:8]}/{c[8:12]}-{c[12:]}" if len(c) == 14 else c
    )

    colunas_texto = [
        "incentivo", "tipo_projeto", "setor_economico_abreviado",
        "setor_economico", "razao_social", "municipio"
    ]
    for col in colunas_texto:
        if col in df_pe.columns:
            df_pe[col] = df_pe[col].astype(str).str.strip().replace({"nan": None, "None": None, "": None})

    # Inteiros anuláveis (sem representação decimal .0)
    colunas_inteiras = ["empregos_diretos", "empregos_terceirizados", "numero_laudo", "numero_portaria"]
    for col in colunas_inteiras:
        if col in df_pe.columns:
            df_pe[col] = pd.to_numeric(df_pe[col], errors="coerce").astype("Int64")

    # Datas em formato ISO-8601 (YYYY-MM-DD)
    colunas_datas = ["data_processo", "data_laudo", "data_portaria"]
    for col in colunas_datas:
        if col in df_pe.columns:
            dt_series = pd.to_datetime(df_pe[col], format="%d/%m/%Y", errors="coerce")
            df_pe[col] = dt_series.dt.strftime("%Y-%m-%d")

    # Valores monetários
    if "valor_investimento" in df_pe.columns:
        df_pe["valor_investimento"] = pd.to_numeric(df_pe["valor_investimento"], errors="coerce").fillna(0.0).round(2)

    df_pe.sort_values(by="data_processo", ascending=False, inplace=True)
    df_pe.reset_index(drop=True, inplace=True)

    # -------------------------------------------------------------------------
    # C. Exportações Finais
    # -------------------------------------------------------------------------
    log("5. Exportando arquivos tratados...")

    # Parquet
    df_pe.to_parquet(ARQUIVO_PARQUET, index=False)
    log(f"   📁 Parquet salvo: {ARQUIVO_PARQUET}")

    # JSON Estruturado (convertendo NaNs remanescentes em null)
    registros = df_pe.to_dict(orient="records")
    registros_limpos = [
        {k: (None if pd.isna(v) else v) for k, v in r.items()}
        for r in registros
    ]

    with open(ARQUIVO_JSON, "w", encoding="utf-8") as f:
        json.dump(registros_limpos, f, ensure_ascii=False, indent=2)
    log(f"   📁 JSON definitivo salvo: {ARQUIVO_JSON}")

    log("==================================================")
    log("🎉 ETL SUDENE CONCLUÍDO COM SUCESSO!")
    log(f"Bruto Excel      : {ARQUIVO_RAW_EXCEL.name}")
    log(f"Tratado JSON (PE): {ARQUIVO_JSON.name}")
    log(f"Tratado Parquet  : {ARQUIVO_PARQUET.name}")
    log("==================================================")

def executar():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA PIPELINE - INCENTIVOS FISCAIS SUDENE (PE) ===\n")

    sucesso = baixar_csv_temporario()
    if sucesso or ARQUIVO_TEMP_CSV.exists():
        processar_bases()
    else:
        log("❌ Falha na execução: arquivo temporário indisponível.")

if __name__ == "__main__":
    executar()

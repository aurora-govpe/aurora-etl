import itertools
import json
import re
import time
import unicodedata
from pathlib import Path

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

# ==============================================================================
# 1. CONFIGURAÇÃO DO ENDPOINT E DIRETÓRIO ÚNICO DE SAÍDA
# ==============================================================================
API_URL = "https://atlasmobilidadesocial.org.br/wp-json/caleidosapi/ler_excel"

# Todos os arquivos sairão diretamente na raiz desta pasta (sem subpastas)
OUTPUT_DIR = Path(r"C:\dados\dados brutos\aurora\mobilidade_social")

REQUEST_DELAY_SEC = 0.20  # Pausa cortês entre requisições

# ==============================================================================
# 2. CÓDIGOS IBGE DOS 27 ESTADOS E SUBGRUPOS DO MODAL
# ==============================================================================
ESTADOS_IBGE = {
    "11": "Rondônia",
    "12": "Acre",
    "13": "Amazonas",
    "14": "Roraima",
    "15": "Pará",
    "16": "Amapá",
    "17": "Tocantins",
    "21": "Maranhão",
    "22": "Piauí",
    "23": "Ceará",
    "24": "Rio Grande do Norte",
    "25": "Paraíba",
    "26": "Pernambuco",
    "27": "Alagoas",
    "28": "Sergipe",
    "29": "Bahia",
    "31": "Minas Gerais",
    "32": "Espírito Santo",
    "33": "Rio de Janeiro",
    "35": "São Paulo",
    "41": "Paraná",
    "42": "Santa Catarina",
    "43": "Rio Grande do Sul",
    "50": "Mato Grosso do Sul",
    "51": "Mato Grosso",
    "52": "Goiás",
    "53": "Distrito Federal",
}

SEXO_OPCOES = {
    "total": "Todos",
    "masculino": "Homens",
    "feminino": "Mulheres",
}

COR_OPCOES = {
    "total": "Todos",
    "branco": "Brancos",
    "nao-branco": "Pretos e outros",
}

ESCOLA_OPCOES = {
    "total": "Todos",
    "nem-primario": "Sem EF",
    "primario": "EF",
    "medio": "EM",
    "superior": "ES ou mais",
}

RENDA_PADRAO = ("total", "Abaixo da Mediana (P50)")
COORTE_PADRAO = ("total", "Total")

META_COLS = [
    "ID IBGE",
    "Estado",
    "sexo_cod",
    "sexo",
    "cor_cod",
    "cor",
    "escola_cod",
    "escolaridade_pais",
    "renda",
    "coorte",
]

# Termos exclusivos dos 38 indicadores contextuais (usados para bloqueio explícito)
CONTEXTUAL_BLACKLIST = (
    "2000", "2002", "2005", "2007", "2013",
    "gini", "pib", "ideb", "homicid", "catolic", "protestant",
    "saneamento", "mortalidade", "baixo_peso", "expectativa",
    "gasto", "evasao", "distorcao", "analfabetismo", "informalidade",
    "emprego", "jovens", "renda_domiciliar", "populacao", "familias",
)


# ==============================================================================
# 3. SESSÃO HTTP E NORMALIZAÇÃO DO PAYLOAD
# ==============================================================================
def build_session() -> requests.Session:
    session = requests.Session()
    retries = Retry(
        total=4,
        backoff_factor=1.0,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["GET"],
    )
    session.mount("https://", HTTPAdapter(max_retries=retries))
    session.headers.update({
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:160.0) Gecko/20100101 Firefox/160.0",
        "Accept": "application/json, text/javascript, */*; q=0.01",
        "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
        "X-Requested-With": "XMLHttpRequest",
        "Referer": "https://atlasmobilidadesocial.org.br/atlas/",
    })
    return session


def slugify(text: str, max_len: int = 55) -> str:
    norm = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    return re.sub(r"[^a-zA-Z0-9]+", "_", norm).strip("_").lower()[:max_len]


def fetch_slice(
    session: requests.Session,
    id_ibge: str,
    sexo: str,
    cor: str,
    escola: str,
) -> object:
    params = {
        "id_ibge": id_ibge,
        "sexo": sexo,
        "cor": cor,
        "escola": escola,
        "renda": RENDA_PADRAO[0],
        "coorte": COORTE_PADRAO[0],
    }
    resp = session.get(API_URL, params=params, timeout=30)
    resp.raise_for_status()
    payload = resp.json()
    if isinstance(payload, str):
        payload = json.loads(payload)
    return payload


def flatten_payload_to_rows(
    payload: object,
    id_ibge_fallback: str,
    estado_fallback: str,
    meta: dict,
) -> list[dict]:
    if isinstance(payload, list):
        items = payload
    elif isinstance(payload, dict):
        list_key = next(
            (k for k, v in payload.items() if isinstance(v, list) and len(v) > 0),
            None,
        )
        items = payload[list_key] if list_key else [payload]
    else:
        return []

    df_norm = pd.json_normalize(items)
    rows: list[dict] = []

    for rec in df_norm.to_dict(orient="records"):
        raw_id = str(
            rec.get("id_ibge")
            or rec.get("ID_IBGE")
            or rec.get("codigo")
            or rec.get("cod_ibge")
            or id_ibge_fallback
        ).strip()

        estado_nome = ESTADOS_IBGE.get(raw_id, estado_fallback)

        row = {
            "ID IBGE": raw_id,
            "Estado": estado_nome,
            **meta,
        }
        for col, val in rec.items():
            if col not in ("id_ibge", "ID_IBGE", "codigo", "cod_ibge"):
                row[col] = val
        rows.append(row)

    return rows


def filtrar_colunas_mobilidade(df: pd.DataFrame) -> list[str]:
    """
    Retorna exclusivamente as colunas dos 10 Indicadores de Mobilidade Intergeracional:
      1. Exclui metadados e colunas da blacklist contextual (anos 2000-2013 e termos socioeconômicos).
      2. Valida variação entre subgrupos demográficos dentro de cada UF (indicadores contextuais
         são constantes por UF e têm variância zero entre subgrupos).
    """
    candidatas = [c for c in df.columns if c not in META_COLS]
    selecionadas: list[str] = []

    for col in candidatas:
        col_slug = slugify(col, max_len=120)
        if any(termo in col_slug for termo in CONTEXTUAL_BLACKLIST):
            continue

        serie_num = pd.to_numeric(df[col], errors="coerce")
        if serie_num.notna().sum() == 0:
            continue

        # Indicadores de mobilidade variam conforme sexo, cor e escolaridade dos pais
        max_valores_por_uf = (
            df.assign(_val=serie_num)
            .groupby("ID IBGE")["_val"]
            .nunique(dropna=True)
            .max()
        )
        if max_valores_por_uf > 1:
            selecionadas.append(col)

    return selecionadas


# ==============================================================================
# 4. EXECUÇÃO PRINCIPAL DA INGESTÃO
# ==============================================================================
def run_ingestion() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    session = build_session()
    all_rows: list[dict] = []
    seen_keys: set[tuple] = set()
    erros: list[dict] = []

    subgrupos = list(
        itertools.product(
            SEXO_OPCOES.items(),
            COR_OPCOES.items(),
            ESCOLA_OPCOES.items(),
        )
    )
    total_subgrupos = len(subgrupos)
    print(f"Diretório de destino: {OUTPUT_DIR.resolve()}")
    print(f"Iniciando extração de {total_subgrupos} subgrupos × {len(ESTADOS_IBGE)} estados...")

    for idx_sub, ((sexo_cod, sexo_lbl), (cor_cod, cor_lbl), (esc_cod, esc_lbl)) in enumerate(subgrupos, start=1):
        sub_slug = f"sexo-{sexo_cod}__cor-{cor_cod}__escola-{esc_cod}"
        sub_payloads_raw: dict[str, object] = {}

        meta = {
            "sexo_cod": sexo_cod,
            "sexo": sexo_lbl,
            "cor_cod": cor_cod,
            "cor": cor_lbl,
            "escola_cod": esc_cod,
            "escolaridade_pais": esc_lbl,
            "renda": RENDA_PADRAO[1],
            "coorte": COORTE_PADRAO[1],
        }

        for id_ibge, estado_nome in ESTADOS_IBGE.items():
            dedup_key = (id_ibge, sexo_cod, cor_cod, esc_cod)
            if dedup_key in seen_keys:
                continue

            try:
                payload = fetch_slice(session, id_ibge, sexo_cod, cor_cod, esc_cod)
                sub_payloads_raw[id_ibge] = payload

                rows = flatten_payload_to_rows(payload, id_ibge, estado_nome, meta)
                for r in rows:
                    r_key = (r["ID IBGE"], sexo_cod, cor_cod, esc_cod)
                    if r_key not in seen_keys:
                        seen_keys.add(r_key)
                        all_rows.append(r)

                time.sleep(REQUEST_DELAY_SEC)

            except Exception as exc:
                erros.append({
                    "id_ibge": id_ibge,
                    "estado": estado_nome,
                    "subgrupo": sub_slug,
                    "erro": str(exc),
                })

        # Backup bruto JSON do subgrupo na raiz
        raw_path = OUTPUT_DIR / f"raw_{idx_sub:02d}_{sub_slug}.json"
        with open(raw_path, "w", encoding="utf-8") as f_json:
            json.dump(sub_payloads_raw, f_json, ensure_ascii=False, indent=2)

        print(f"[{idx_sub:02d}/{total_subgrupos}] {sub_slug} | Registros acumulados: {len(all_rows)}")

    if not all_rows:
        print("Nenhum dado extraído. Verifique log_erros.json.")
        return

    df_full = pd.DataFrame(all_rows)
    mobilidade_cols = filtrar_colunas_mobilidade(df_full)
    print(f"\nIndicadores de mobilidade selecionados ({len(mobilidade_cols)}):")
    for col in mobilidade_cols:
        print(f"  - {col}")

    # Mantém apenas as 10 colunas de dimensão + os 10 indicadores de mobilidade
    df_master = df_full[META_COLS + mobilidade_cols].copy()
    for col in mobilidade_cols:
        df_master[col] = pd.to_numeric(df_master[col], errors="coerce")

    # 1) Base consolidada filtrada apenas para mobilidade intergeracional
    master_csv = OUTPUT_DIR / "atlas_estados_consolidado.csv"
    df_master.to_csv(master_csv, index=False, encoding="utf-8-sig")
    print(f"\nConsolidado salvo: {master_csv.name} ({df_master.shape[0]} linhas × {df_master.shape[1]} colunas)")

    # 2) Planilhas CSV individuais (10 indicadores × 45 subgrupos = 450 arquivos)
    arquivos_gerados = 0
    for ind_col in mobilidade_cols:
        ind_slug = slugify(ind_col, max_len=55)
        for (sexo_cod, cor_cod, esc_cod), df_slice in df_master.groupby(
            ["sexo_cod", "cor_cod", "escola_cod"], sort=False
        ):
            df_out = df_slice[["ID IBGE", "Estado", ind_col]].sort_values("ID IBGE")
            if df_out[ind_col].isna().all():
                continue

            fname = f"{ind_slug}__sexo-{sexo_cod}__cor-{cor_cod}__escola-{esc_cod}.csv"
            df_out.to_csv(OUTPUT_DIR / fname, index=False, encoding="utf-8-sig")
            arquivos_gerados += 1

    print(f"Planilhas individuais de mobilidade geradas na raiz: {arquivos_gerados} arquivos CSV")

    if erros:
        err_path = OUTPUT_DIR / "log_erros.json"
        with open(err_path, "w", encoding="utf-8") as f_err:
            json.dump(erros, f_err, ensure_ascii=False, indent=2)
        print(f"Atenção: {len(erros)} falhas registradas em {err_path.name}")


if __name__ == "__main__":
    run_ingestion()

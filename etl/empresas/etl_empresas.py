"""
==============================================================================
ETL DE EMPRESAS - PLATAFORMA AURORA
==============================================================================
Processa as bases da SEPLAG (Ativas e Inativas) de Pernambuco, consolidando:
  1. Saldo de empresas (aberturas x fechamentos) mês a mês (Estadual, Municipal e por Porte)
  2. Tipologia e porte (MEI, ME, EPP, Demais, Natureza Jurídica)
  3. Longevidade e faixas de idade (Tempo de vida de ativas e sobrevida de extintas)
  4. Segmentação setorial (CNAE - Grandes Setores e Divisões)
  5. Espacialização geográfica (GeoJSON enriquecido com indicadores municipais e bairros)

Diretórios de Entrada:
  - C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_ATIVAS.parquet
  - C:/dados/dados brutos/aurora/empresas/df_SEPLAG_EMPRESAS_INATIVAS.parquet
  - C:/dados/aurora/geojson/pernambuco_municipios.geojson

Diretórios de Saída:
  - C:/dados/aurora/parquet/
  - C:/dados/aurora/geojson/
==============================================================================
"""

import os
import sys
import json
import unicodedata
from datetime import datetime
import numpy as np
import pandas as pd

# Definição dos caminhos padrão com suporte a variáveis de ambiente
IS_LINUX = sys.platform.startswith("linux")

DIR_RAW = os.getenv(
    "AURORA_RAW_DIR",
    "/srv/dados_brutos/aurora" if IS_LINUX else r"C:\dados\dados brutos\aurora"
)
DIR_OUT = os.getenv(
    "AURORA_DATA_DIR",
    "/srv/dados/aurora" if IS_LINUX else r"C:\dados\aurora"
)

PATH_ATIVAS = os.path.join(DIR_RAW, "empresas", "df_SEPLAG_EMPRESAS_ATIVAS.parquet")
PATH_INATIVAS = os.path.join(DIR_RAW, "empresas", "df_SEPLAG_EMPRESAS_INATIVAS.parquet")
PATH_GEO_MUNIS = os.path.join(DIR_OUT, "geojson", "pernambuco_municipios.geojson")

DIR_OUT_PARQUET = os.path.join(DIR_OUT, "parquet")
DIR_OUT_GEOJSON = os.path.join(DIR_OUT, "geojson")

os.makedirs(DIR_OUT_PARQUET, exist_ok=True)
os.makedirs(DIR_OUT_GEOJSON, exist_ok=True)


def normalizar_texto(texto):
    """Remove acentos, converte para maiúsculo e remove espaços extras."""
    if not isinstance(texto, str):
        return ""
    texto = unicodedata.normalize("NFD", texto)
    texto = "".join(c for c in texto if unicodedata.category(c) != "Mn")
    return texto.strip().upper()


# Aliases para compatibilização perfeita com os 185 municípios do IBGE / GeoJSON
MUNI_ALIASES = {
    "IGUARACI": "IGUARACY",
    "SAO CAITANO": "SAO CAETANO",
    "LAGOA DO ITAENGA": "LAGOA DE ITAENGA",
    "BELEM DE SAO FRANCISCO": "BELEM DO SAO FRANCISCO",
}


def mapear_grande_setor(cnae_num):
    """Mapeia o código CNAE (divisão de 2 dígitos) para a Seção do CNAE 2.0."""
    try:
        val = int(cnae_num)
        div = int(str(val).zfill(7)[:2])
    except Exception:
        return "Não Identificado"

    if 1 <= div <= 3:
        return "Agropecuária"
    elif 5 <= div <= 9:
        return "Indústrias Extrativas"
    elif 10 <= div <= 33:
        return "Indústrias de Transformação"
    elif div == 35:
        return "Eletricidade e Gás"
    elif 36 <= div <= 39:
        return "Água, Esgoto e Gestão de Resíduos"
    elif 41 <= div <= 43:
        return "Construção"
    elif 45 <= div <= 47:
        return "Comércio"
    elif 49 <= div <= 53:
        return "Transporte, Armazenagem e Correio"
    elif 55 <= div <= 56:
        return "Alojamento e Alimentação"
    elif 58 <= div <= 63:
        return "Informação e Comunicação"
    elif 64 <= div <= 66:
        return "Atividades Financeiras"
    elif div == 68:
        return "Atividades Imobiliárias"
    elif 69 <= div <= 75:
        return "Atividades Profissionais, Científicas e Técnicas"
    elif 77 <= div <= 82:
        return "Serviços Administrativos"
    elif div == 84:
        return "Administração Pública"
    elif div == 85:
        return "Educação"
    elif 86 <= div <= 88:
        return "Saúde Humana e Serviços Sociais"
    elif 90 <= div <= 93:
        return "Artes, Cultura, Esporte e Recreação"
    elif 94 <= div <= 96:
        return "Outras Atividades de Serviços"
    elif div == 97:
        return "Serviços Domésticos"
    elif div == 99:
        return "Organismos Internacionais"
    else:
        return "Outros"


def converter_data_excel(serie):
    """
    Converte inteiros seriais do Excel para datetime.
    Valores válidos entre 1 e 50.000 (~1900 a ~2036).
    Descarta datas sentinelas anômalas (como 1500, ano 0001, etc.) para evitar overflow.
    """
    s_str = serie.astype(str).str.strip()
    is_numeric = s_str.str.isnumeric()
    serials = pd.to_numeric(s_str[is_numeric], errors="coerce")
    valid_mask = (serials >= 1) & (serials <= 50000)
    
    res = pd.Series(pd.NaT, index=serie.index, dtype="datetime64[ns]")
    if valid_mask.sum() > 0:
        valid_indices = serials[valid_mask].index
        res.loc[valid_indices] = pd.to_datetime(
            serials.loc[valid_indices], unit="D", origin="1899-12-30"
        )
    
    # Para casos que eram strings de data convencionais (ex: YYYY-MM-DD ou DD/MM/YYYY)
    non_num_idx = s_str[~is_numeric].index
    if len(non_num_idx) > 0:
        parsed_other = pd.to_datetime(s_str.loc[non_num_idx], errors="coerce", dayfirst=True)
        # Filtra anos razoáveis (1900 a 2030) para evitar overflow em nanosegundos
        valid_years = (parsed_other.dt.year >= 1900) & (parsed_other.dt.year <= 2030)
        res.loc[non_num_idx[valid_years]] = parsed_other[valid_years]
        
    return res


def carregar_referencia_municipios():
    """Carrega GeoJSON de municípios e monta mapa de normalização e IBGE."""
    if not os.path.exists(PATH_GEO_MUNIS):
        print(f"[AVISO] Arquivo {PATH_GEO_MUNIS} não encontrado. Municípios serão processados sem IBGE.")
        return {}, None

    with open(PATH_GEO_MUNIS, "r", encoding="utf-8") as f:
        geo = json.load(f)

    mapa = {}
    for feat in geo["features"]:
        props = feat["properties"]
        nome_norm = normalizar_texto(props.get("municipio", ""))
        cod_ibge = str(props.get("cod_ibge", "")).strip()
        ibge7 = str(props.get("ibge7", "")).strip()
        pop = props.get("pop_2026", props.get("populacao", 0))
        reg_des = props.get("regiao_de_desenvolvimento", "")
        
        info = {
            "municipio_oficial": props.get("municipio", ""),
            "cod_ibge": cod_ibge,
            "ibge7": ibge7,
            "populacao": pop,
            "regiao_desenvolvimento": reg_des
        }
        mapa[nome_norm] = info

    # Registra aliases
    for alias_orig, alias_dest in MUNI_ALIASES.items():
        if alias_dest in mapa:
            mapa[alias_orig] = mapa[alias_dest]

    return mapa, geo


def processar_etl():
    print("=" * 70)
    print("INICIANDO ETL DE EMPRESAS (SEPLAG-PE) - PLATAFORMA AURORA")
    print("=" * 70)

    mapa_munis, geo_base = carregar_referencia_municipios()

    # -------------------------------------------------------------------------
    # 1. CARGA E TRATAMENTO: ATIVAS
    # -------------------------------------------------------------------------
    print(f"\n[1/5] Carregando empresas ATIVAS de: {PATH_ATIVAS}...")
    df_ativas = pd.read_parquet(PATH_ATIVAS)
    print(f"      Total de registros carregados: {len(df_ativas):,}")

    df_ativas["muni_norm"] = df_ativas["municipio"].apply(normalizar_texto)
    df_ativas["muni_norm"] = df_ativas["muni_norm"].replace(MUNI_ALIASES)
    
    # Mapeamento do código IBGE e nome oficial
    df_ativas["cod_ibge"] = df_ativas["muni_norm"].apply(lambda m: mapa_munis[m]["cod_ibge"] if m in mapa_munis else None)
    df_ativas["municipio_oficial"] = df_ativas["muni_norm"].apply(lambda m: mapa_munis[m]["municipio_oficial"] if m in mapa_munis else m)

    # Conversão de datas
    print("      Convertendo datas de início de atividade (Ativas)...")
    df_ativas["dt_abertura"] = converter_data_excel(df_ativas["dt_inicio_atividade"])
    df_ativas["ano_abertura"] = df_ativas["dt_abertura"].dt.year
    df_ativas["ano_mes_abertura"] = df_ativas["dt_abertura"].dt.to_period("M").astype(str)

    # Padronização de Porte e MEI
    df_ativas["is_mei"] = df_ativas["in_identifica_mei"].astype(str).str.upper() == "S"
    df_ativas["porte_ajustado"] = df_ativas["porte"].fillna("OUTROS").astype(str).str.upper()
    df_ativas.loc[df_ativas["is_mei"], "porte_ajustado"] = "MEI"
    df_ativas["porte_ajustado"] = df_ativas["porte_ajustado"].replace({
        "ME": "Microempresa (ME)",
        "EPP": "Pequeno Porte (EPP)",
        "NO": "Demais Portes",
        "MEI": "MEI"
    })

    # Setorização CNAE
    df_ativas["grande_setor"] = df_ativas["cnae"].apply(mapear_grande_setor)
    df_ativas["cnae_divisao"] = df_ativas["cnae"].fillna(0).astype(int).astype(str).str.zfill(7).str[:2]

    # Idade em anos na data atual
    data_ref = pd.to_datetime("today")
    df_ativas["idade_anos"] = (data_ref - df_ativas["dt_abertura"]).dt.days / 365.25
    df_ativas["idade_anos"] = df_ativas["idade_anos"].clip(lower=0, upper=120)

    # -------------------------------------------------------------------------
    # 2. CARGA E TRATAMENTO: INATIVAS
    # -------------------------------------------------------------------------
    print(f"\n[2/5] Carregando empresas INATIVAS de: {PATH_INATIVAS}...")
    df_inativas = pd.read_parquet(PATH_INATIVAS)
    print(f"      Total de registros carregados: {len(df_inativas):,}")

    muni_inativas_col = "municipio_2" if "municipio_2" in df_inativas.columns else "municipio"
    df_inativas["muni_norm"] = df_inativas[muni_inativas_col].apply(normalizar_texto)
    df_inativas["muni_norm"] = df_inativas["muni_norm"].replace(MUNI_ALIASES)
    
    df_inativas["cod_ibge"] = df_inativas["muni_norm"].apply(lambda m: mapa_munis[m]["cod_ibge"] if m in mapa_munis else None)
    df_inativas["municipio_oficial"] = df_inativas["muni_norm"].apply(lambda m: mapa_munis[m]["municipio_oficial"] if m in mapa_munis else m)

    # Conversão de datas: Abertura e Fechamento
    print("      Tratando datas de abertura e encerramento (Inativas)...")
    if pd.api.types.is_datetime64_any_dtype(df_inativas["dt_inicio_atividade"]):
        df_inativas["dt_abertura"] = pd.to_datetime(df_inativas["dt_inicio_atividade"]).dt.tz_localize(None)
    else:
        df_inativas["dt_abertura"] = converter_data_excel(df_inativas["dt_inicio_atividade"])

    # Limpa anos anômalos de abertura (< 1900 ou > 2030)
    ano_ab_in = df_inativas["dt_abertura"].dt.year
    df_inativas.loc[(ano_ab_in < 1900) | (ano_ab_in > 2030), "dt_abertura"] = pd.NaT

    df_inativas["ano_abertura"] = df_inativas["dt_abertura"].dt.year
    df_inativas["ano_mes_abertura"] = df_inativas["dt_abertura"].dt.to_period("M").astype(str)

    # Data de encerramento efetiva: coalesce de dt_ult_alteracao com dt_digitacao
    dt_fech = df_inativas["dt_ult_alteracao"].combine_first(df_inativas["dt_digitacao"])
    if hasattr(dt_fech.dt, "tz_localize"):
        try:
            dt_fech = dt_fech.dt.tz_localize(None)
        except TypeError:
            dt_fech = dt_fech.dt.tz_convert(None)

    # Limpa anos anômalos de fechamento (< 1900 ou > 2030)
    df_inativas["dt_fechamento"] = dt_fech
    ano_fech = df_inativas["dt_fechamento"].dt.year
    df_inativas.loc[(ano_fech < 1900) | (ano_fech > 2030), "dt_fechamento"] = pd.NaT

    df_inativas["ano_fechamento"] = df_inativas["dt_fechamento"].dt.year
    df_inativas["ano_mes_fechamento"] = df_inativas["dt_fechamento"].dt.to_period("M").astype(str)

    # Tempo de vida até o encerramento (anos)
    df_inativas["tempo_vida_anos"] = (df_inativas["dt_fechamento"] - df_inativas["dt_abertura"]).dt.days / 365.25
    df_inativas["tempo_vida_anos"] = df_inativas["tempo_vida_anos"].clip(lower=0, upper=120)

    # Padronização de Porte e MEI (Inativas)
    df_inativas["is_mei"] = df_inativas["in_identifica_mei"].astype(str).str.upper() == "S"
    df_inativas["porte_ajustado"] = df_inativas["porte"].fillna("OUTROS").astype(str).str.upper()
    df_inativas.loc[df_inativas["is_mei"], "porte_ajustado"] = "MEI"
    df_inativas["porte_ajustado"] = df_inativas["porte_ajustado"].replace({
        "ME": "Microempresa (ME)",
        "EPP": "Pequeno Porte (EPP)",
        "NO": "Demais Portes",
        "MEI": "MEI"
    })

    df_inativas["grande_setor"] = df_inativas["cnae"].apply(mapear_grande_setor)
    df_inativas["cnae_divisao"] = df_inativas["cnae"].fillna(0).astype(int).astype(str).str.zfill(7).str[:2]

    # -------------------------------------------------------------------------
    # 3. GERAÇÃO DOS INDICADORES CONSOLIDADOS
    # -------------------------------------------------------------------------
    print("\n[3/5] Calculando indicadores analíticos e demografia de empresas...")

    # A) Saldo Mensal Estadual (Aberturas x Fechamentos)
    # Considera aberturas de ambas as bases (pois quem abriu no passado e já fechou também foi abertura daquele mês!)
    aberturas_ativas = df_ativas[["ano_mes_abertura", "porte_ajustado", "cod_ibge", "municipio_oficial"]].rename(columns={"ano_mes_abertura": "ano_mes"})
    aberturas_inativas = df_inativas[["ano_mes_abertura", "porte_ajustado", "cod_ibge", "municipio_oficial"]].rename(columns={"ano_mes_abertura": "ano_mes"})
    todas_aberturas = pd.concat([aberturas_ativas, aberturas_inativas], ignore_index=True)
    todas_aberturas = todas_aberturas[todas_aberturas["ano_mes"].str.startswith(("19", "20"), na=False)]

    fechamentos = df_inativas[["ano_mes_fechamento", "porte_ajustado", "cod_ibge", "municipio_oficial"]].rename(columns={"ano_mes_fechamento": "ano_mes"})
    fechamentos = fechamentos[fechamentos["ano_mes"].str.startswith(("19", "20"), na=False)]

    # Saldo Estadual Mês a Mês (Geral)
    ab_est = todas_aberturas["ano_mes"].value_counts().rename("aberturas")
    fc_est = fechamentos["ano_mes"].value_counts().rename("fechamentos")
    df_saldo_pe = pd.concat([ab_est, fc_est], axis=1).fillna(0).astype(int).reset_index().rename(columns={"index": "ano_mes"})
    df_saldo_pe["saldo"] = df_saldo_pe["aberturas"] - df_saldo_pe["fechamentos"]
    df_saldo_pe["ano"] = df_saldo_pe["ano_mes"].str[:4].astype(int)
    df_saldo_pe["mes"] = df_saldo_pe["ano_mes"].str[5:7].astype(int)
    df_saldo_pe = df_saldo_pe[(df_saldo_pe["ano"] >= 2000) & (df_saldo_pe["ano"] <= 2026)].sort_values("ano_mes").reset_index(drop=True)
    df_saldo_pe["saldo_acumulado"] = df_saldo_pe["saldo"].cumsum()

    # Saldo Mensal por Porte
    ab_porte = todas_aberturas.groupby(["ano_mes", "porte_ajustado"]).size().rename("aberturas")
    fc_porte = fechamentos.groupby(["ano_mes", "porte_ajustado"]).size().rename("fechamentos")
    df_saldo_porte = pd.concat([ab_porte, fc_porte], axis=1).fillna(0).astype(int).reset_index()
    df_saldo_porte["saldo"] = df_saldo_porte["aberturas"] - df_saldo_porte["fechamentos"]
    df_saldo_porte["ano"] = df_saldo_porte["ano_mes"].str[:4].astype(int)
    df_saldo_porte = df_saldo_porte[(df_saldo_porte["ano"] >= 2000) & (df_saldo_porte["ano"] <= 2026)].sort_values(["ano_mes", "porte_ajustado"]).reset_index(drop=True)

    # Saldo Mensal Municipal
    ab_mun = todas_aberturas.groupby(["ano_mes", "cod_ibge", "municipio_oficial"]).size().rename("aberturas")
    fc_mun = fechamentos.groupby(["ano_mes", "cod_ibge", "municipio_oficial"]).size().rename("fechamentos")
    df_saldo_mun = pd.concat([ab_mun, fc_mun], axis=1).fillna(0).astype(int).reset_index()
    df_saldo_mun["saldo"] = df_saldo_mun["aberturas"] - df_saldo_mun["fechamentos"]
    df_saldo_mun["ano"] = df_saldo_mun["ano_mes"].str[:4].astype(int)
    df_saldo_mun = df_saldo_mun[(df_saldo_mun["ano"] >= 2018) & (df_saldo_mun["ano"] <= 2026)].sort_values(["ano_mes", "municipio_oficial"]).reset_index(drop=True)

    # B) Faixas Etárias de Empresas
    def atribuir_faixa_etaria(anos):
        if pd.isna(anos):
            return "Indeterminada"
        elif anos < 1.0:
            return "Até 1 ano"
        elif anos < 2.0:
            return "1 a 2 anos"
        elif anos < 5.0:
            return "2 a 5 anos"
        elif anos < 10.0:
            return "5 a 10 anos"
        elif anos < 20.0:
            return "10 a 20 anos"
        else:
            return "Mais de 20 anos"

    df_ativas["faixa_idade"] = df_ativas["idade_anos"].apply(atribuir_faixa_etaria)
    df_inativas["faixa_sobrevida"] = df_inativas["tempo_vida_anos"].apply(atribuir_faixa_etaria)

    faixas_ativas = df_ativas.groupby(["cod_ibge", "municipio_oficial", "grande_setor", "faixa_idade"]).size().rename("qtd_ativas").reset_index()
    faixas_inativas = df_inativas.groupby(["cod_ibge", "municipio_oficial", "grande_setor", "faixa_sobrevida"]).size().rename("qtd_inativas").reset_index()
    df_faixas_etarias = pd.merge(
        faixas_ativas, faixas_inativas,
        left_on=["cod_ibge", "municipio_oficial", "grande_setor", "faixa_idade"],
        right_on=["cod_ibge", "municipio_oficial", "grande_setor", "faixa_sobrevida"],
        how="outer"
    )
    df_faixas_etarias["faixa_etaria"] = df_faixas_etarias["faixa_idade"].combine_first(df_faixas_etarias["faixa_sobrevida"])
    df_faixas_etarias = df_faixas_etarias.drop(columns=["faixa_idade", "faixa_sobrevida"]).fillna(0)

    # C) Setor Econômico e CNAE
    setor_ativas = df_ativas.groupby(["cod_ibge", "municipio_oficial", "grande_setor", "cnae_divisao"]).agg(
        total_ativas=("cnpj", "count"),
        total_mei_ativas=("is_mei", lambda x: int(x.sum())),
        capital_social_total=("vl_capital", "sum"),
        capital_social_mediano=("vl_capital", "median")
    ).reset_index()

    setor_inativas = df_inativas.groupby(["cod_ibge", "municipio_oficial", "grande_setor", "cnae_divisao"]).agg(
        total_inativas=("pessoa", "count"),
        total_mei_inativas=("is_mei", lambda x: int(x.sum()))
    ).reset_index()

    df_cnae_setor = pd.merge(setor_ativas, setor_inativas, on=["cod_ibge", "municipio_oficial", "grande_setor", "cnae_divisao"], how="outer").fillna(0)

    # D) Resumo Municipal Consolidado (185 Municípios de Pernambuco)
    print("      Consolidando indicadores executivos por município...")
    
    # Filtro dos últimos 12 meses (2025/2026)
    ult_12m_aberturas = todas_aberturas[todas_aberturas["ano_mes"] >= "2025-11"].groupby("cod_ibge").size().rename("aberturas_12m")
    ult_12m_fechamentos = fechamentos[fechamentos["ano_mes"] >= "2025-11"].groupby("cod_ibge").size().rename("fechamentos_12m")

    res_ativas = df_ativas.groupby(["cod_ibge", "municipio_oficial"]).agg(
        total_ativas=("cnpj", "count"),
        total_mei=("is_mei", lambda x: int(x.sum())),
        total_me=("porte_ajustado", lambda x: int((x == "Microempresa (ME)").sum())),
        total_epp=("porte_ajustado", lambda x: int((x == "Pequeno Porte (EPP)").sum())),
        total_outros_portes=("porte_ajustado", lambda x: int((x == "Demais Portes").sum())),
        capital_social_total=("vl_capital", "sum"),
        capital_social_mediano=("vl_capital", "median"),
        idade_media_ativas=("idade_anos", "mean")
    ).reset_index()

    res_inativas = df_inativas.groupby("cod_ibge").agg(
        total_inativas=("pessoa", "count"),
        tempo_medio_vida_inativas=("tempo_vida_anos", "mean")
    ).reset_index()

    df_muni_resumo = pd.merge(res_ativas, res_inativas, on="cod_ibge", how="left").fillna(0)
    df_muni_resumo = pd.merge(df_muni_resumo, ult_12m_aberturas, on="cod_ibge", how="left").fillna(0)
    df_muni_resumo = pd.merge(df_muni_resumo, ult_12m_fechamentos, on="cod_ibge", how="left").fillna(0)

    df_muni_resumo["saldo_12m"] = df_muni_resumo["aberturas_12m"] - df_muni_resumo["fechamentos_12m"]
    df_muni_resumo["taxa_meizacao"] = np.where(
        df_muni_resumo["total_ativas"] > 0,
        (df_muni_resumo["total_mei"] / df_muni_resumo["total_ativas"]) * 100,
        0.0
    ).round(2)
    df_muni_resumo["idade_media_ativas"] = df_muni_resumo["idade_media_ativas"].round(2)
    df_muni_resumo["tempo_medio_vida_inativas"] = df_muni_resumo["tempo_medio_vida_inativas"].round(2)

    # Incorporar população e taxa por mil habitantes
    pop_series = pd.Series({info["cod_ibge"]: info["populacao"] for info in mapa_munis.values() if info["cod_ibge"]})
    reg_series = pd.Series({info["cod_ibge"]: info["regiao_desenvolvimento"] for info in mapa_munis.values() if info["cod_ibge"]})
    df_muni_resumo["populacao"] = df_muni_resumo["cod_ibge"].map(pop_series).fillna(0)
    df_muni_resumo["regiao_desenvolvimento"] = df_muni_resumo["cod_ibge"].map(reg_series).fillna("")
    df_muni_resumo["empresas_por_mil_hab"] = np.where(
        df_muni_resumo["populacao"] > 0,
        (df_muni_resumo["total_ativas"] / df_muni_resumo["populacao"]) * 1000,
        0.0
    ).round(2)

    # E) Bairros do Recife (Maior polo econômico de PE)
    print("      Consolidando polos intraurbanos dos bairros do Recife...")
    df_recife = df_ativas[df_ativas["muni_norm"] == "RECIFE"].copy()
    df_recife["bairro_norm"] = df_recife["bairro"].apply(normalizar_texto)
    df_bairros_recife = df_recife.groupby("bairro_norm").agg(
        total_ativas=("cnpj", "count"),
        total_mei=("is_mei", lambda x: int(x.sum())),
        capital_social_total=("vl_capital", "sum"),
        capital_social_mediano=("vl_capital", "median"),
        idade_media=("idade_anos", "mean")
    ).reset_index().sort_values("total_ativas", ascending=False)
    df_bairros_recife["taxa_meizacao"] = (df_bairros_recife["total_mei"] / df_bairros_recife["total_ativas"] * 100).round(2)
    df_bairros_recife["idade_media"] = df_bairros_recife["idade_media"].round(2)

    # -------------------------------------------------------------------------
    # 4. SALVAMENTO DOS PARQUETS
    # -------------------------------------------------------------------------
    print(f"\n[4/5] Gravando Parquets consolidados em: {DIR_OUT_PARQUET}...")
    
    arquivos_parquet = {
        "df_empresas_saldo_mensal_pe.parquet": df_saldo_pe,
        "df_empresas_saldo_mensal_porte.parquet": df_saldo_porte,
        "df_empresas_saldo_mensal_muni.parquet": df_saldo_mun,
        "df_empresas_resumo_municipal.parquet": df_muni_resumo,
        "df_empresas_faixa_etaria.parquet": df_faixas_etarias,
        "df_empresas_cnae_setor.parquet": df_cnae_setor,
        "df_empresas_bairros_recife.parquet": df_bairros_recife
    }

    for nome_arq, df_obj in arquivos_parquet.items():
        caminho_final = os.path.join(DIR_OUT_PARQUET, nome_arq)
        df_obj.to_parquet(caminho_final, index=False)
        print(f"      -> {nome_arq} ({len(df_obj):,} linhas)")

    # -------------------------------------------------------------------------
    # 5. GERAÇÃO DO GEOJSON ENRIQUECIDO
    # -------------------------------------------------------------------------
    print(f"\n[5/5] Gerando GeoJSON georreferenciado em: {DIR_OUT_GEOJSON}...")
    if geo_base:
        dict_muni_resumo = df_muni_resumo.set_index("cod_ibge").to_dict(orient="index")
        
        for feat in geo_base["features"]:
            cod = str(feat["properties"].get("cod_ibge", "")).strip()
            if cod in dict_muni_resumo:
                muni_info = dict_muni_resumo[cod]
                # Injeta métricas de empresas diretamente nas propriedades da feature geográfica
                feat["properties"].update({
                    "emp_total_ativas": int(muni_info.get("total_ativas", 0)),
                    "emp_total_inativas": int(muni_info.get("total_inativas", 0)),
                    "emp_total_mei": int(muni_info.get("total_mei", 0)),
                    "emp_taxa_meizacao": float(muni_info.get("taxa_meizacao", 0.0)),
                    "emp_saldo_12m": int(muni_info.get("saldo_12m", 0)),
                    "emp_aberturas_12m": int(muni_info.get("aberturas_12m", 0)),
                    "emp_fechamentos_12m": int(muni_info.get("fechamentos_12m", 0)),
                    "emp_empresas_por_mil_hab": float(muni_info.get("empresas_por_mil_hab", 0.0)),
                    "emp_capital_social_total": float(muni_info.get("capital_social_total", 0.0)),
                    "emp_capital_social_mediano": float(muni_info.get("capital_social_mediano", 0.0)),
                    "emp_idade_media_ativas": float(muni_info.get("idade_media_ativas", 0.0)),
                    "emp_tempo_medio_inativas": float(muni_info.get("tempo_medio_vida_inativas", 0.0))
                })

        path_geojson_out = os.path.join(DIR_OUT_GEOJSON, "empresas_municipios_pe.geojson")
        with open(path_geojson_out, "w", encoding="utf-8") as f:
            json.dump(geo_base, f, ensure_ascii=False)
        print(f"      -> empresas_municipios_pe.geojson gravado com sucesso ({len(geo_base['features'])} municípios)!")

    print("\n" + "=" * 70)
    print("ETL DE EMPRESAS CONCLUÍDO COM SUCESSO!")
    print("=" * 70)


if __name__ == "__main__":
    processar_etl()

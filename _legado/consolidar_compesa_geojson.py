import json
import os
import pandas as pd

print("1. Carregando dados base...")
src_geo = r"C:\dados\aurora\geojson\mapa_compesa_todas_camadas.geojson"
df_demo = pd.read_parquet(r"C:\dados\aurora\parquet\df_demografia_pe.parquet")
ibge_mun_map = dict(zip(df_demo['ibge6'].astype(str).str[:6], df_demo['municipio']))

# Dicionários de rótulos amigáveis por camada e cor
DICIONARIO_CORES = {
    "Rodízio atual": {
        "#0a5240": "Abastecimento Diário (Sem Rodízio)",
        "#1d9e75": "Diário com Ajuste Operacional",
        "#378add": "Rodízio Moderado",
        "#ef9f27": "Rodízio Intermediário",
        "#7f77dd": "Rodízio Severo",
        "#e24b4a": "Rodízio Crítico / Intermitente",
        "#c8c6bc": "Sem Dados / Em Avaliação Técnica",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Rodízio pós obras": {
        "#0a5240": "Abastecimento Diário (Universalizado)",
        "#1d9e75": "Diário com Ajuste Operacional",
        "#378add": "Rodízio Moderado",
        "#ef9f27": "Rodízio Intermediário",
        "#7f77dd": "Rodízio Severo",
        "#e24b4a": "Rodízio Crítico",
        "#c8c6bc": "Em Avaliação Técnica",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Situação C2 (com obras)": {
        "#1d9e75": "Sem Déficit (Atendido)",
        "#ef9f27": "Déficit Solucionado com Obras",
        "#e24b4a": "Déficit Persistente",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Situação C1 (sem obras)": {
        "#1d9e75": "Sem Déficit",
        "#e24b4a": "Com Déficit",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Situação C3 (Dem. PRSB)": {
        "#1d9e75": "Superávit Pleno",
        "#5dcaa5": "Superávit Moderado",
        "#7f77dd": "Déficit Parcial",
        "#e24b4a": "Déficit Severo",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Categoria (A/B/C)": {
        "#185fa5": "Categoria A (Grande Porte)",
        "#ef9f27": "Categoria B (Médio Porte)",
        "#888780": "Categoria C (Pequeno Porte)",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Concessão": {
        "#185fa5": "Concedido à Compesa",
        "#e24b4a": "Não Concedido / Vencido",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Com projetos": {
        "#7f77dd": "Com Projetos Previstos",
        "#c8c6bc": "Sem Projetos no Plano",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Investimento planejado": {
        "#0c447c": "Mais de R$ 100 Mi",
        "#185fa5": "R$ 50 Mi a R$ 100 Mi",
        "#378add": "R$ 20 Mi a R$ 50 Mi",
        "#64a8e8": "R$ 10 Mi a R$ 20 Mi",
        "#a8cef0": "R$ 5 Mi a R$ 10 Mi",
        "#d4e8fa": "Até R$ 5 Mi",
        "#c8c6bc": "Sem Investimento Previsto",
        "#b8b6ae": "Não Operado pela Compesa"
    },
    "Regionalização Anexo XIV": {
        "#0c447c": "Microrregião Estruturada (RMR)",
        "#1d9e75": "Microrregião Atendida",
        "#ef9f27": "Microrregião em Transição",
        "#7f77dd": "Microrregião Regionalizada",
        "#d85a30": "Microrregião Especial",
        "#c8c6bc": "Bloco em Estruturação",
        "#b8b6ae": "Não Operado pela Compesa"
    }
}

print("2. Lendo GeoJSON original...")
with open(src_geo, 'r', encoding='utf-8') as f:
    raw_gj = json.load(f)

polys = {}
lines = []
points = []

for ft in raw_gj['features']:
    geom = ft.get('geometry')
    if not geom: continue
    gt = geom.get('type')
    props = ft.get('properties', {})
    
    if gt in ['Polygon', 'MultiPolygon']:
        cod = str(props.get('codarea', ''))
        if not cod: continue
        if cod not in polys:
            ibge6 = cod[:6]
            nome_mun = ibge_mun_map.get(ibge6, f"Município {cod}")
            polys[cod] = {
                "type": "Feature",
                "geometry": geom,
                "properties": {
                    "codarea": cod,
                    "ibge6": ibge6,
                    "municipio": nome_mun
                }
            }
        # Agregar propriedades de todas as camadas para o mesmo polígono
        for k, v in props.items():
            if v is not None and v != "":
                polys[cod]['properties'][k] = v
                
    elif gt in ['LineString', 'MultiLineString']:
        lines.append(ft)
    elif gt in ['Point', 'MultiPoint']:
        points.append(ft)

print(f"Polígonos únicos consolidados: {len(polys)}")
print(f"Linhas preservadas: {len(lines)}")
print(f"Pontos preservados: {len(points)}")

# Enriquecer os polígonos municipais com os rótulos de detalhe
for cod, ft in polys.items():
    p = ft['properties']
    for layer, color_map in DICIONARIO_CORES.items():
        cor = p.get(f"{layer} (Cor)") or p.get(layer)
        if cor:
            cor_clean = str(cor).lower().strip()
            rotulo = color_map.get(cor_clean, f"Situação {cor}")
            p[f"{layer} (Detalhe)"] = rotulo
            p[f"{layer} (Cor)"] = cor_clean
            p[layer] = rotulo  # Permite usar o nome do rótulo na chave principal

# Montar a FeatureCollection final
features_finais = list(polys.values()) + lines + points
novo_geojson = {
    "type": "FeatureCollection",
    "name": "mapa_compesa_todas_camadas",
    "crs": raw_gj.get("crs"),
    "features": features_finais
}

dest_orig = r"C:\dados\aurora\geojson\mapa_compesa_todas_camadas.geojson"
dest_www = r"C:\Users\hugo.vasconcelos\Documents\projetos\aurora\aurora_municipios\www\data\mapa_compesa_todas_camadas.geojson"

os.makedirs(os.path.dirname(dest_www), exist_ok=True)

print("3. Salvando GeoJSON unificado...")
with open(dest_www, 'w', encoding='utf-8') as f:
    json.dump(novo_geojson, f, ensure_ascii=False)

size_mb = os.path.getsize(dest_www) / (1024 * 1024)
print(f"✅ Gravado em www/data com sucesso! Tamanho final: {size_mb:.2f} MB.")

try:
    with open(dest_orig, 'w', encoding='utf-8') as f:
        json.dump(novo_geojson, f, ensure_ascii=False)
    print(f"✅ Gravado em {dest_orig} com sucesso!")
except Exception as e:
    print(f"Aviso ao gravar em {dest_orig}: {e} (mantendo o original)")

import json
import os
import pandas as pd
import numpy as np

# ==============================================================================
# RESOLUÇÃO CENTRALIZADA DE DIRETÓRIOS
# ==============================================================================
DIR_OUT = os.environ.get('AURORA_DATA_DIR', r'C:\dados\aurora')
parquet_path = os.path.join(DIR_OUT, 'parquet', 'df_iniciativas.parquet')
if not os.path.exists(parquet_path):
    # Fallback local se rodar dentro da pasta data
    alt_parquet = os.path.join('data', 'parquet', 'df_iniciativas.parquet')
    if os.path.exists(alt_parquet):
        parquet_path = alt_parquet

geojson_path = os.path.join(DIR_OUT, 'geojson', 'obras_notion_geometrias2.geojson')
if not os.path.exists(geojson_path):
    alt_geo = os.path.join('data', 'geojson', 'obras_notion_geometrias2.geojson')
    if os.path.exists(alt_geo):
        geojson_path = alt_geo

print(f"Lendo iniciativas de: {parquet_path}...")
df = pd.read_parquet(parquet_path)
der = df[df['fonte'].astype(str).str.upper() == 'DER'].copy()

# Tratamento e normalizacao
der['local_norm'] = der['local'].astype(str).str.strip().str.upper()
der['valor_total_num'] = pd.to_numeric(der['valor_total'], errors='coerce').fillna(0)
der['valor_pago_num'] = pd.to_numeric(der['valor_pago'], errors='coerce').fillna(0)
der['percent_exec_num'] = pd.to_numeric(der['percent_execucao'], errors='coerce').fillna(0)

def normalize_status(st):
    st = str(st).strip()
    if 'conclu' in st.lower():
        return 'Concluída'
    elif 'execu' in st.lower():
        return 'Em Execução'
    elif 'paralisad' in st.lower():
        return 'Paralisada'
    return st

der['status_norm'] = der['status'].apply(normalize_status)

print(f"Lendo GeoJSON de rodovias de: {geojson_path}...")
with open(geojson_path, 'r', encoding='utf-8') as f:
    geo = json.load(f)

# Resumo por rodovia
rodovia_stats = {}
for rod, grp in der.groupby('local_norm'):
    total_val = grp['valor_total_num'].sum()
    total_pago = grp['valor_pago_num'].sum()
    obras_count = len(grp)
    statuses = grp['status_norm'].tolist()
    
    # Status predominante / mais representativo
    if any('Execução' in s for s in statuses):
        status_pred = 'Em Execução'
        cor = '#0284C7'
    elif any('Paralisada' in s for s in statuses):
        status_pred = 'Paralisada'
        cor = '#EF4444'
    else:
        status_pred = 'Concluída'
        cor = '#10B981'
        
    rodovia_stats[rod] = {
        'rodovia': rod,
        'obras_count': obras_count,
        'status_pred': status_pred,
        'cor': cor,
        'valor_total': float(total_val),
        'valor_pago': float(total_pago),
        'avanco_medio': float(grp['percent_exec_num'].mean()),
        'municipios': sorted(list(set(grp['municipio'].dropna().tolist()))),
        'rds': sorted(list(set(grp['regiao_de_desenvolvimento'].dropna().tolist())))
    }

# Enriquecer o GeoJSON
for feat in geo['features']:
    props = feat.get('properties', {})
    rod = str(props.get('rodovia', '')).strip().upper()
    if rod in rodovia_stats:
        st = rodovia_stats[rod]
        props['status_predominante'] = st['status_pred']
        props['cor_status'] = st['cor']
        props['total_obras'] = st['obras_count']
        props['valor_total'] = st['valor_total']
        props['valor_total_fmt'] = f"R$ {st['valor_total']:,.2f}".replace(',', 'X').replace('.', ',').replace('X', '.')
        props['avanco_medio'] = round(st['avanco_medio'], 1)
        props['municipios'] = ", ".join(st['municipios'][:5]) + ("..." if len(st['municipios']) > 5 else "")
    else:
        props['status_predominante'] = 'Planejada'
        props['cor_status'] = '#94A3B8'
        props['total_obras'] = 0
        props['valor_total'] = 0
        props['valor_total_fmt'] = "R$ 0,00"
        props['avanco_medio'] = 0

# Lista de obras detalhadas para a tabela
obras_list = []
for idx, row in der.iterrows():
    obras_list.append({
        'local': str(row['local_norm']),
        'iniciativa': str(row['iniciativa_estrategica']),
        'municipio': str(row['municipio']),
        'rd': str(row['regiao_de_desenvolvimento']),
        'status': str(row['status_norm']),
        'percent_execucao': round(float(row['percent_exec_num']), 1),
        'valor_total': float(row['valor_total_num']),
        'valor_total_fmt': f"R$ {row['valor_total_num']:,.2f}".replace(',', 'X').replace('.', ',').replace('X', '.'),
        'valor_pago': float(row['valor_pago_num']),
        'valor_pago_fmt': f"R$ {row['valor_pago_num']:,.2f}".replace(',', 'X').replace('.', ',').replace('X', '.')
    })

# Macro KPIs
total_obras = len(der)
total_rodovias = der['local_norm'].nunique()
total_investimento = der['valor_total_num'].sum()
concluidas = int((der['status_norm'] == 'Concluída').sum())
em_execucao = int((der['status_norm'] == 'Em Execução').sum())
paralisadas = int((der['status_norm'] == 'Paralisada').sum())
avanco_medio = round(float(der['percent_exec_num'].mean()), 1)

output_payload = {
    'kpis': {
        'total_obras': total_obras,
        'total_rodovias': total_rodovias,
        'valor_total_bi': round(total_investimento / 1e9, 2),
        'valor_total_fmt': f"R$ {total_investimento:,.2f}".replace(',', 'X').replace('.', ',').replace('X', '.'),
        'concluidas': concluidas,
        'em_execucao': em_execucao,
        'paralisadas': paralisadas,
        'avanco_medio': avanco_medio
    },
    'rodovias_unicas': sorted(list(rodovia_stats.keys())),
    'obras': obras_list,
    'geojson': geo
}

# Salva nos destinos oficiais
destinos_saida = [
    os.path.join(DIR_OUT, 'estradas_pe.json'),
    os.path.join(DIR_OUT, 'json', 'estradas_pe.json')
]

for dest in destinos_saida:
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    try:
        with open(dest, 'w', encoding='utf-8') as f:
            json.dump(output_payload, f, ensure_ascii=False, indent=2)
        print(f"✅ Arquivo salvo em: {dest}")
    except Exception as e:
        print(f"Aviso ao salvar {dest}: {e}")

# Fallback se existir pasta www/data local
if os.path.exists('www/data'):
    try:
        with open('www/data/estradas_pe.json', 'w', encoding='utf-8') as f:
            json.dump(output_payload, f, ensure_ascii=False, indent=2)
    except Exception:
        pass

print("Sucesso! ETL de estradas concluído.")

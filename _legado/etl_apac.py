import json
import math
import os
from datetime import datetime

# ==============================================================================
# 1. FUNÇÕES AUXILIARES DE GEOMETRIA (DOUGLAS-PEUCKER PURO PYTHON)
# ==============================================================================

def perpendicular_distance(point, line_start, line_end):
    dx = line_end[0] - line_start[0]
    dy = line_end[1] - line_start[1]
    mag = math.hypot(dx, dy)
    if mag == 0:
        return math.hypot(point[0] - line_start[0], point[1] - line_start[1])
    # Distância perpendicular
    u = ((point[0] - line_start[0]) * dx + (point[1] - line_start[1]) * dy) / (mag * mag)
    ix = line_start[0] + u * dx
    iy = line_start[1] + u * dy
    return math.hypot(point[0] - ix, point[1] - iy)

def simplify_points(pts, tolerance):
    if len(pts) <= 2:
        return pts
    # Encontra o ponto com maior distância
    dmax = 0.0
    index = 0
    for i in range(1, len(pts) - 1):
        d = perpendicular_distance(pts[i], pts[0], pts[-1])
        if d > dmax:
            index = i
            dmax = d
    if dmax > tolerance:
        rec1 = simplify_points(pts[:index + 1], tolerance)
        rec2 = simplify_points(pts[index:], tolerance)
        return rec1[:-1] + rec2
    else:
        return [pts[0], pts[-1]]

def simplify_ring(ring, tolerance=0.0015):
    if len(ring) <= 4:
        return ring
    is_closed = (ring[0][0] == ring[-1][0] and ring[0][1] == ring[-1][1])
    simplified = simplify_points(ring[:-1] if is_closed else ring, tolerance)
    if is_closed and (simplified[0][0] != simplified[-1][0] or simplified[0][1] != simplified[-1][1]):
        simplified.append(simplified[0])
    if len(simplified) < 4:
        return ring  # Não degenerar o polígono
    return simplified

# ==============================================================================
# 2. DIRETÓRIOS E ARQUIVOS DE ENTRADA / SAÍDA
# ==============================================================================

INPUT_DIR = r"C:\dados\dados brutos\aurora\apac"
OUTPUT_DIR = r"C:\Users\hugo.vasconcelos\Documents\projetos\aurora\aurora_municipios\www\data"
os.makedirs(OUTPUT_DIR, exist_ok=True)

PATH_RES_EVENTS = os.path.join(INPUT_DIR, "Reservatório_monitorado_sirh_views_sde_view_reservatorio_monitorado_gov_dash6_Events.json")
PATH_BACIAS = os.path.join(INPUT_DIR, "Bacia_hidrográfica.json")

# ==============================================================================
# 3. ETL RESERVATÓRIOS MONITORADOS (SIRH/APAC)
# ==============================================================================

print("Carregando reservatórios monitorados...")
with open(PATH_RES_EVENTS, "r", encoding="utf-8") as f:
    raw_res = json.load(f)

reservatorios_features = []
reservatorios_lista = []

faixa_cores = {
    "VERTENDO": "#0ea5e9",             # Azul celeste vivo
    "ENTRE 80% e 100%": "#10b981",    # Verde Esmeralda (Cheio / Seguro)
    "ENTRE 30% e 80%": "#f59e0b",     # Âmbar (Alerta Moderado)
    "ENTRE 10% e 30%": "#f97316",     # Laranja (Alerta Severo)
    "COLAPSO": "#ef4444"              # Vermelho (Colapso / Crítico)
}

faixa_rotulos = {
    "VERTENDO": "Vertendo (> 100%)",
    "ENTRE 80% e 100%": "Cheio (80% a 100%)",
    "ENTRE 30% e 80%": "Moderado (30% a 80%)",
    "ENTRE 10% e 30%": "Alerta Severo (10% a 30%)",
    "COLAPSO": "Colapso (< 10%)"
}

total_cap_estadual = 0
total_vol_estadual = 0
bacia_agregados = {}

for item in raw_res:
    attr = item.get("attributes", {})
    geom = item.get("geometry", {})
    lon = geom.get("x") or attr.get("longitude")
    lat = geom.get("y") or attr.get("latitude")

    if lon is None or lat is None:
        continue

    nome_res = (attr.get("nome_reservatorio") or "Reservatório").strip()
    nome_mun = (attr.get("nome_municipio") or "").strip()
    bacia = (attr.get("baciahidrografica") or "Outra").strip()
    faixa_bruta = (attr.get("faixa") or "COLAPSO").strip()
    situacao = (attr.get("situacao") or "").strip()
    uso = (attr.get("usoprincipal") or "Abastecimento").strip()
    tipo_barragem = (attr.get("tipobarragem") or "").strip()

    cap = float(attr.get("capacidade") or 0)
    vol = float(attr.get("volumeatual") or 0)
    per = float(attr.get("per_volume") or 0)
    cota = float(attr.get("cota_atual") or 0)

    total_cap_estadual += cap
    total_vol_estadual += vol

    # Data da cota
    dt_ms = attr.get("data_cota")
    dt_str = ""
    if dt_ms:
        try:
            dt_str = datetime.fromtimestamp(dt_ms / 1000.0).strftime("%d/%m/%Y")
        except Exception:
            dt_str = ""

    cor = faixa_cores.get(faixa_bruta, "#94a3b8")
    rotulo_faixa = faixa_rotulos.get(faixa_bruta, faixa_bruta)

    # Detalhe para tooltip rico
    cap_m = round(cap / 1e6, 2)
    vol_m = round(vol / 1e6, 2)
    tooltip_html = (
        f"<b>{nome_res}</b><br/>"
        f"📍 {nome_mun} (Bacia: {bacia})<br/>"
        f"💧 Volume: <b>{per:.1f}%</b> ({vol_m:,.2f} Mi m³ de {cap_m:,.2f} Mi m³)<br/>"
        f"🏷️ Faixa: <span style='color:{cor};font-weight:bold;'>{rotulo_faixa}</span><br/>"
        f"🎯 Finalidade: {uso}"
        + (f"<br/>📅 Cota de {dt_str}" if dt_str else "")
    )

    registro = {
        "id_reservatorio": attr.get("id_reservatorio"),
        "nome_reservatorio": nome_res,
        "nome_municipio": nome_mun,
        "baciahidrografica": bacia,
        "capacidade": cap,
        "volumeatual": vol,
        "capacidade_m": cap_m,
        "volumeatual_m": vol_m,
        "per_volume": round(per, 2),
        "cota_atual": cota,
        "data_cota": dt_str,
        "faixa": faixa_bruta,
        "faixa_rotulo": rotulo_faixa,
        "faixa_cor": cor,
        "situacao": situacao,
        "usoprincipal": uso,
        "tipobarragem": tipo_barragem,
        "latitude": round(lat, 6),
        "longitude": round(lon, 6),
        "detalhe_tooltip": tooltip_html
    }
    reservatorios_lista.append(registro)

    # Feature GeoJSON
    feat = {
        "type": "Feature",
        "geometry": {
            "type": "Point",
            "coordinates": [round(lon, 6), round(lat, 6)]
        },
        "properties": registro
    }
    reservatorios_features.append(feat)

    # Agregação por Bacia
    b_norm = bacia.upper().strip()
    if b_norm not in bacia_agregados:
        bacia_agregados[b_norm] = {
            "nome_bacia": bacia,
            "total_reservatorios": 0,
            "capacidade_total": 0.0,
            "volume_total": 0.0,
            "colapso_count": 0,
            "critico_count": 0,
            "seguro_count": 0
        }
    bacia_agregados[b_norm]["total_reservatorios"] += 1
    bacia_agregados[b_norm]["capacidade_total"] += cap
    bacia_agregados[b_norm]["volume_total"] += vol
    if faixa_bruta == "COLAPSO":
        bacia_agregados[b_norm]["colapso_count"] += 1
    elif faixa_bruta in ("ENTRE 10% e 30%"):
        bacia_agregados[b_norm]["critico_count"] += 1
    elif faixa_bruta in ("ENTRE 80% e 100%", "VERTENDO"):
        bacia_agregados[b_norm]["seguro_count"] += 1

geojson_reservatorios = {
    "type": "FeatureCollection",
    "features": reservatorios_features
}

path_geo_res = os.path.join(OUTPUT_DIR, "apac_reservatorios.geojson")
with open(path_geo_res, "w", encoding="utf-8") as f:
    json.dump(geojson_reservatorios, f, ensure_ascii=False)
print(f"Salvo: {path_geo_res} ({len(reservatorios_features)} reservatórios, {os.path.getsize(path_geo_res)/1024:.1f} KB)")

path_json_res = os.path.join(OUTPUT_DIR, "apac_reservatorios.json")
with open(path_json_res, "w", encoding="utf-8") as f:
    json.dump(reservatorios_lista, f, ensure_ascii=False, indent=2)
print(f"Salvo: {path_json_res} ({os.path.getsize(path_json_res)/1024:.1f} KB)")


# ==============================================================================
# 4. ETL BACIAS HIDROGRÁFICAS (SIMPLIFICAÇÃO & ENRIQUECIMENTO)
# ==============================================================================

print("\nCarregando e simplificando bacias hidrográficas (29 UPs)...")
with open(PATH_BACIAS, "r", encoding="utf-8") as f:
    raw_bacias = json.load(f)

bacias_features = []
bacias_lista = []

for b in raw_bacias:
    attr = b.get("attributes", {})
    geom = b.get("geometry", {})
    rings = geom.get("rings", [])
    if not rings:
        continue

    nm_up = (attr.get("nm_up") or "").strip()
    cod_up = (attr.get("cod_up") or "").strip()
    up = (attr.get("up") or "").strip()

    # Simplifica cada anel poligonal com Douglas-Peucker
    simplified_rings = []
    for r in rings:
        s_ring = simplify_ring(r, tolerance=0.0018)  # ~200m de tolerância geográfica
        if len(s_ring) >= 4:
            # arredonda coordenadas para 5 casas decimais
            rounded = [[round(pt[0], 5), round(pt[1], 5)] for pt in s_ring]
            simplified_rings.append(rounded)

    if not simplified_rings:
        continue

    # Identifica métricas de mananciais associadas a esta bacia
    b_match = None
    for k in bacia_agregados:
        if k in nm_up.upper() or nm_up.upper() in k or k in up.upper():
            b_match = bacia_agregados[k]
            break

    cap_bacia = b_match["capacidade_total"] if b_match else 0.0
    vol_bacia = b_match["volume_total"] if b_match else 0.0
    per_bacia = round((vol_bacia / cap_bacia * 100), 1) if cap_bacia > 0 else None
    tot_res = b_match["total_reservatorios"] if b_match else 0
    col_res = b_match["colapso_count"] if b_match else 0

    # Cor da bacia baseada no estresse hídrico
    if per_bacia is None or tot_res == 0:
        cor_bacia = "#94a3b8"  # Cinza slate (sem reservatório monitorado direto)
        status_bacia = "Sem Monitoramento Direto de Mananciais"
    elif per_bacia < 15:
        cor_bacia = "#ef4444"  # Vermelho (Estresse Crítico)
        status_bacia = "Estresse Hídrico Crítico"
    elif per_bacia < 35:
        cor_bacia = "#f97316"  # Laranja (Alerta Severo)
        status_bacia = "Alerta Severo de Volume"
    elif per_bacia < 70:
        cor_bacia = "#f59e0b"  # Âmbar (Alerta Moderado)
        status_bacia = "Volume Moderado"
    else:
        cor_bacia = "#10b981"  # Verde (Capacidade Segura)
        status_bacia = "Armazenamento Seguro"

    prop_bacia = {
        "cod_up": cod_up,
        "nome_bacia": nm_up,
        "up": up,
        "capacidade_total_m": round(cap_bacia / 1e6, 2),
        "volume_total_m": round(vol_bacia / 1e6, 2),
        "percentual_armazenado": per_bacia,
        "total_reservatorios": tot_res,
        "reservatorios_colapso": col_res,
        "status_bacia": status_bacia,
        "cor_bacia": cor_bacia,
        "detalhe_tooltip": (
            f"<b>Bacia Hidrográfica: {nm_up} ({cod_up})</b><br/>"
            f"Mananciais Monitorados: <b>{tot_res}</b> ({col_res} em colapso)<br/>"
            + (f"Armazenamento Geral: <b>{per_bacia:.1f}%</b> ({round(vol_bacia/1e6,1)} de {round(cap_bacia/1e6,1)} Mi m³)<br/>" if per_bacia is not None else "Sem barragens estaduais no SIRH<br/>")
            + f"Status: <b style='color:{cor_bacia}'>{status_bacia}</b>"
        )
    }
    bacias_lista.append(prop_bacia)

    feat_bacia = {
        "type": "Feature",
        "geometry": {
            "type": "Polygon" if len(simplified_rings) == 1 else "MultiPolygon",
            "coordinates": simplified_rings if len(simplified_rings) == 1 else [simplified_rings]
        },
        "properties": prop_bacia
    }
    bacias_features.append(feat_bacia)

geojson_bacias = {
    "type": "FeatureCollection",
    "features": bacias_features
}

path_geo_bacias = os.path.join(OUTPUT_DIR, "apac_bacias_hidrograficas.geojson")
with open(path_geo_bacias, "w", encoding="utf-8") as f:
    json.dump(geojson_bacias, f, ensure_ascii=False)
print(f"Salvo: {path_geo_bacias} ({len(bacias_features)} bacias, {os.path.getsize(path_geo_bacias)/1024:.1f} KB)")

path_json_bacias = os.path.join(OUTPUT_DIR, "apac_bacias_resumo.json")
with open(path_json_bacias, "w", encoding="utf-8") as f:
    json.dump(bacias_lista, f, ensure_ascii=False, indent=2)
print(f"Salvo: {path_json_bacias} ({os.path.getsize(path_json_bacias)/1024:.1f} KB)")

# Resumo estadual consolidado
resumo_geral = {
    "data_extracao": datetime.now().strftime("%d/%m/%Y"),
    "total_reservatorios": len(reservatorios_lista),
    "capacidade_total_bi_m3": round(total_cap_estadual / 1e9, 2),
    "volume_atual_bi_m3": round(total_vol_estadual / 1e9, 2),
    "capacidade_total_mi_m3": round(total_cap_estadual / 1e6, 1),
    "volume_atual_mi_m3": round(total_vol_estadual / 1e6, 1),
    "percentual_geral": round((total_vol_estadual / total_cap_estadual * 100), 1),
    "colapso_count": sum(1 for r in reservatorios_lista if r["faixa"] == "COLAPSO"),
    "critico_count": sum(1 for r in reservatorios_lista if r["faixa"] == "ENTRE 10% e 30%"),
    "moderado_count": sum(1 for r in reservatorios_lista if r["faixa"] == "ENTRE 30% e 80%"),
    "cheio_count": sum(1 for r in reservatorios_lista if r["faixa"] in ("ENTRE 80% e 100%", "VERTENDO")),
    "vertendo_count": sum(1 for r in reservatorios_lista if r["faixa"] == "VERTENDO"),
    "municipios_atendidos": len(set(r["nome_municipio"] for r in reservatorios_lista if r["nome_municipio"]))
}

path_resumo = os.path.join(OUTPUT_DIR, "apac_resumo_estadual.json")
with open(path_resumo, "w", encoding="utf-8") as f:
    json.dump(resumo_geral, f, ensure_ascii=False, indent=2)
print(f"Salvo: {path_resumo} ({resumo_geral})")
print("\nETL APAC concluído com sucesso!")

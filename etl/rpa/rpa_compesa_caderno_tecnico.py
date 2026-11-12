# -*- coding: utf-8 -*-
"""
RPA / Parser: Caderno Técnico de Abastecimento PE (COMPESA / SRHS-PE)
Extrai dados estruturados de todos os 185 municípios de Pernambuco a partir do PDF vetorial
e gera uma base tabular consolidada em Excel (.xlsx).

Autor: Hugo Vasconcelos / Aurora Project
Data: 2026-11-12
"""

import os
import re
import struct
import unicodedata
import zlib
import openpyxl
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter

# ==============================================================================
# CONFIGURAÇÕES E CAMINHOS
# ==============================================================================
DEFAULT_PDF_PATH = r"C:\dados\dados brutos\aurora\compesa\Caderno Técnico Abastecimento PE.pdf"
POPULACAO_REF_PATH = r"C:\dados\dados brutos\aurora\municipios_pernambuco_populacao.xlsx"
MATRIZ_RD_PATH = r"C:\dados\dados brutos\aurora\indicadores\matriz_desenvolvimento_regional.csv"
OUTPUT_XLSX_PATH = r"C:\Users\hugo.vasconcelos\Documents\projetos\aurora\aurora-etl\compesa_caderno_tecnico_abastecimento_pe.xlsx"


def normalize_name(name: str) -> str:
    """Normaliza nome para correspondência sem acentos, espaços ou pontuação."""
    if not name:
        return ""
    n = unicodedata.normalize("NFKD", name).encode("ASCII", "ignore").decode("ASCII").lower()
    return re.sub(r"[^a-z0-9]", "", n)


# ==============================================================================
# 1. CARREGAMENTO DAS BASES DE REFERÊNCIA (IBGE, POPULAÇÃO E RD)
# ==============================================================================
def load_reference_data():
    ibge_map = {}
    if os.path.exists(POPULACAO_REF_PATH):
        try:
            wb_pop = openpyxl.load_workbook(POPULACAO_REF_PATH, data_only=True)
            ws_pop = wb_pop.active
            for r in list(ws_pop.iter_rows(values_only=True))[1:]:
                ibge = str(r[0]).strip()
                m_name = str(r[1]).replace(" - PE", "").strip()
                pop = r[3] if len(r) > 3 else None
                ibge_map[normalize_name(m_name)] = {
                    "cod_ibge": ibge,
                    "municipio_oficial": m_name,
                    "populacao_2025": pop,
                }
        except Exception as e:
            print(f"[AVISO] Falha ao ler base de população: {e}")

    # Aliases conhecidos entre PDF e IBGE oficial
    aliases = {
        normalize_name("Belém de São Francisco"): normalize_name("Belém do São Francisco"),
        normalize_name("Iguaraci"): normalize_name("Iguaracy"),
        normalize_name("Lagoa do Itaenga"): normalize_name("Lagoa de Itaenga"),
    }
    for k, v in aliases.items():
        if v in ibge_map:
            ibge_map[k] = ibge_map[v]

    # Regiões de Desenvolvimento (RD)
    rd_map = {}
    if os.path.exists(MATRIZ_RD_PATH):
        try:
            with open(MATRIZ_RD_PATH, "r", encoding="utf-8") as f:
                header = f.readline().strip().split(";")
                cod_idx = None
                rd_idx = None
                for idx, h in enumerate(header):
                    clean_h = h.replace('"', "").strip()
                    if clean_h == "cod_ibge":
                        cod_idx = idx
                    elif clean_h == "rd_pe":
                        rd_idx = idx
                if cod_idx is not None and rd_idx is not None:
                    for line in f:
                        parts = line.strip().split(";")
                        if len(parts) > max(cod_idx, rd_idx):
                            c_ibge = parts[cod_idx].replace('"', "").strip()
                            rd_val = parts[rd_idx].replace('"', "").strip()
                            rd_map[c_ibge] = rd_val
        except Exception as e:
            print(f"[AVISO] Falha ao ler matriz de RD: {e}")

    return ibge_map, rd_map


# ==============================================================================
# 2. MOTOR DE EXTRAÇÃO PDF DE BAIXO NÍVEL (INDEPENDENTE DE DEPENDÊNCIAS C)
# ==============================================================================
class PdfTextEngine:
    def __init__(self, pdf_path: str):
        self.pdf_path = pdf_path
        with open(pdf_path, "rb") as f:
            self.data = f.read()

        # Parse XRef table
        xref_m = re.findall(rb"\bstartxref\s+(\d+)\s+%%EOF", self.data)
        if not xref_m:
            raise ValueError("Incapaz de localizar startxref no PDF.")
        xref_pos = int(xref_m[-1])

        sm = re.search(rb"stream\r?\n(.*?)\r?\nendstream", self.data[xref_pos:], re.DOTALL)
        xref_stream = zlib.decompress(sm.group(1))

        # Tamanho de entrada W = [1 4 1]
        entry_size = 6
        num_entries = len(xref_stream) // entry_size
        self.entries = {}
        for i in range(num_entries):
            chunk = xref_stream[i * entry_size : (i + 1) * entry_size]
            t, offset, gen = struct.unpack(">B I B", chunk)
            self.entries[i] = (t, offset, gen)

        self.obj_stms = {}
        self.fonts_cmap_by_obj = {}
        self._load_all_cmaps()

        # Localizar Kids de Páginas
        cat_id = 884
        cat_obj = self.get_obj(cat_id)
        pm = re.search(rb"/Pages\s+(\d+)\s+0\s+R", cat_obj)
        pages_id = int(pm.group(1)) if pm else 883
        pages_obj = self.get_obj(pages_id)
        self.kids = [int(x) for x in re.findall(rb"(\d+)\s+0\s+R", pages_obj)]

    def get_obj(self, num: int) -> bytes:
        if num not in self.entries:
            return b""
        t, off, gen = self.entries[num]
        if t == 1:
            m = re.match(rb"\d+\s+\d+\s+obj\s*(.*?)\s*(?:endobj|stream)", self.data[off:], re.DOTALL)
            return m.group(1) if m else self.data[off : off + 200]
        elif t == 2:
            stm_num = off
            if stm_num not in self.obj_stms:
                _, s_off, _ = self.entries[stm_num]
                sm = re.search(rb"stream\r?\n(.*?)\r?\nendstream", self.data[s_off:], re.DOTALL)
                dec = zlib.decompress(sm.group(1))
                fm = re.search(rb"/First\s+(\d+)", self.data[s_off : s_off + 300])
                first_off = int(fm.group(1))
                hdr = dec[:first_off].decode("latin1").split()
                objs = {}
                for i in range(0, len(hdr), 2):
                    o_num = int(hdr[i])
                    o_off = int(hdr[i + 1])
                    next_off = int(hdr[i + 3]) if i + 2 < len(hdr) else len(dec) - first_off
                    objs[o_num] = dec[first_off + o_off : first_off + next_off]
                self.obj_stms[stm_num] = objs
            return self.obj_stms[stm_num].get(num, b"")
        return b""

    def get_stream(self, num: int) -> bytes:
        if num not in self.entries:
            return b""
        t, off, gen = self.entries[num]
        if t == 1:
            m = re.match(rb"\d+\s+\d+\s+obj.*?\bstream\r?\n", self.data[off:], re.DOTALL)
            if m:
                s_start = off + m.end()
                len_m = re.search(rb"/Length\s+(\d+)", self.data[off:s_start])
                if len_m:
                    l_val = int(len_m.group(1))
                    raw = self.data[s_start : s_start + l_val]
                else:
                    end_m = re.search(rb"\r?\nendstream", self.data[s_start:])
                    raw = self.data[s_start : s_start + end_m.start()]
                try:
                    return zlib.decompress(raw)
                except Exception:
                    return raw
        return b""

    @staticmethod
    def parse_cmap(dec: bytes):
        cmap = {}
        for block in re.findall(rb"beginbfchar(.*?)endbfchar", dec, re.DOTALL):
            pairs = re.findall(rb"<([0-9a-fA-F]+)>\s+<([0-9a-fA-F]+)>", block)
            for src, dst in pairs:
                d_str = dst.decode()
                if len(d_str) % 2 != 0:
                    d_str = "0" + d_str
                cmap[int(src, 16)] = bytes.fromhex(d_str).decode("utf-16-be", errors="ignore")
        for block in re.findall(rb"beginbfrange(.*?)endbfrange", dec, re.DOTALL):
            ranges = re.findall(rb"<([0-9a-fA-F]+)>\s+<([0-9a-fA-F]+)>\s+<([0-9a-fA-F]+)>", block)
            for s1, s2, d1 in ranges:
                start_c = int(s1, 16)
                end_c = int(s2, 16)
                base_dst = int(d1, 16)
                for c in range(start_c, end_c + 1):
                    cmap[c] = chr(base_dst + (c - start_c))
        return cmap

    def _load_all_cmaps(self):
        for i, (t, off, gen) in self.entries.items():
            if t in (1, 2):
                obj_text = self.get_obj(i)
                if b"/Type/Font" in obj_text:
                    m = re.search(rb"/ToUnicode\s+(\d+)\s+0\s+R", obj_text)
                    if m:
                        cmap_id = int(m.group(1))
                        cmap_stm = self.get_stream(cmap_id)
                        self.fonts_cmap_by_obj[i] = self.parse_cmap(cmap_stm)

    def extract_page_blocks(self, page_id: int):
        p_obj = self.get_obj(page_id)
        font_alias_to_cmap = {}
        res_m = re.search(rb"/Resources\s+(\d+)\s+0\s+R", p_obj)
        res_text = self.get_obj(int(res_m.group(1))) if res_m else p_obj

        font_section = re.search(rb"/Font\s*<<([^>]*)>>", res_text)
        if font_section:
            for fm in re.finditer(rb"/(\w+)\s+(\d+)\s+0\s+R", font_section.group(1)):
                alias = fm.group(1).decode()
                f_id = int(fm.group(2))
                if f_id in self.fonts_cmap_by_obj:
                    font_alias_to_cmap[alias] = self.fonts_cmap_by_obj[f_id]
                else:
                    f_obj = self.get_obj(f_id)
                    tu = re.search(rb"/ToUnicode\s+(\d+)\s+0\s+R", f_obj)
                    if tu:
                        font_alias_to_cmap[alias] = self.parse_cmap(self.get_stream(int(tu.group(1))))

        c_m = re.search(rb"/Contents\s*\[(.*?)\]", p_obj)
        if c_m:
            c_ids = [int(x) for x in re.findall(rb"(\d+)\s+0\s+R", c_m.group(1))]
        else:
            c_single = re.search(rb"/Contents\s+(\d+)\s+0\s+R", p_obj)
            c_ids = [int(c_single.group(1))] if c_single else []

        full_c_stm = b"".join(self.get_stream(cid) for cid in c_ids)
        if not full_c_stm:
            return []

        blocks = []
        bt_chunks = re.findall(rb"BT\s*(.*?)\s*ET", full_c_stm, re.DOTALL)
        for chunk in bt_chunks:
            cur_cmap = {}
            text_parts = []
            for line in chunk.splitlines():
                fm = re.search(rb"/(\w+)\s+[\d\.]+\s+Tf", line)
                if fm:
                    alias = fm.group(1).decode()
                    cur_cmap = font_alias_to_cmap.get(alias, {})
                tj_m = re.findall(rb"<([0-9a-fA-F]+)>\s*Tj", line)
                for token in tj_m:
                    token_str = token.decode()
                    chars = [int(token_str[i : i + 4], 16) for i in range(0, len(token_str), 4)]
                    w = "".join(cur_cmap.get(c, "") for c in chars)
                    text_parts.append(w)
            full_text = "".join(text_parts).strip()
            if full_text:
                blocks.append(full_text)
        return blocks


# ==============================================================================
# 3. PARSER ESTRUTURAL DA FICHA DO MUNICÍPIO
# ==============================================================================
def parse_municipality_page(blocks, page_num: int, ibge_ref: dict, rd_ref: dict):
    d = {"pagina_pdf": page_num}

    # 1. Município
    municipio = blocks[1] if len(blocks) > 1 else ""
    d["municipio"] = municipio

    # Match com IBGE e População
    norm = normalize_name(municipio)
    ref_item = ibge_ref.get(norm, {})
    cod_ibge = ref_item.get("cod_ibge", "")
    d["cod_ibge"] = cod_ibge
    d["populacao_2025"] = ref_item.get("populacao_2025", "")

    # 2. Atendimento Compesa
    full_text = " ".join(blocks)
    d["atendido_compesa"] = "NÃO ATENDIDO PELA COMPESA" not in full_text

    # 3. Região e Região de Desenvolvimento
    reg = ""
    for item in blocks[:10]:
        if item.startswith("Região "):
            reg = item.replace("Região ", "").strip()
            break
    d["regiao_caderno"] = reg
    d["rd_pe"] = rd_ref.get(cod_ibge, "")

    # 4. Investimento Total Geral e Vazão Geral
    inv_tot = ""
    vaz_val = ""
    vaz_var = ""
    for i, item in enumerate(blocks[:25]):
        if item == "investimento total" and i > 0 and not inv_tot:
            inv_tot = blocks[i - 1]
        if "vazão atual" in item.lower():
            pre = blocks[max(0, i - 4) : i]
            for p in pre:
                if re.match(r"^[+\-]?\d+%$", p) or p == "estável":
                    vaz_var = p
                elif re.match(r"^\d+$", p) or p == "—":
                    vaz_val = p
    d["investimento_total_geral"] = inv_tot
    d["vazao_atual_ls"] = vaz_val
    d["vazao_variacao_2022"] = vaz_var

    # 5. Mananciais e Sistemas
    mananciais_list = []
    if "Mananciais" in blocks:
        im = blocks.index("Mananciais")
        is_end = blocks.index("SISTEMAS QUE ABASTECEM") if "SISTEMAS QUE ABASTECEM" in blocks else len(blocks)
        slice_m = blocks[im + 1 : is_end]
        if len(slice_m) == 1 and "Nenhum manancial" in slice_m[0]:
            mananciais_list.append((municipio, cod_ibge, slice_m[0], "Monitoramento Seca: Nenhum cadastrado"))
        else:
            k = 0
            while k < len(slice_m):
                if k + 1 < len(slice_m) and slice_m[k + 1] in ("Normal", "Colapso", "Pré-colapso", "Alerta", "Atenção"):
                    mananciais_list.append((municipio, cod_ibge, slice_m[k], slice_m[k + 1]))
                    k += 2
                else:
                    mananciais_list.append((municipio, cod_ibge, slice_m[k], ""))
                    k += 1

    d["total_mananciais"] = len(mananciais_list) if mananciais_list and mananciais_list[0][3] != "Monitoramento Seca: Nenhum cadastrado" else 0
    d["mananciais_resumo"] = "; ".join([f"{m[2]} ({m[3]})" if m[3] else m[2] for m in mananciais_list])

    if "SISTEMAS QUE ABASTECEM" in blocks:
        is_idx = blocks.index("SISTEMAS QUE ABASTECEM")
        if is_idx + 1 < len(blocks) and blocks[is_idx + 1] not in ("02", "Área urbana"):
            d["sistemas_abastecimento"] = blocks[is_idx + 1]
        else:
            d["sistemas_abastecimento"] = ""
    else:
        d["sistemas_abastecimento"] = ""

    # 6. Investimentos Compesa
    inv_compesa_tot = ""
    inv_compesa_real = ""
    inv_compesa_exec = ""
    if "COMPESA" in blocks:
        ic = blocks.index("COMPESA")
        sub = blocks[ic + 1 : ic + 15]
        for idx_s, s_val in enumerate(sub):
            if s_val == "investimento total" and idx_s > 0 and not inv_compesa_tot:
                inv_compesa_tot = sub[idx_s - 1]
            if s_val == "realizados" and idx_s > 0:
                inv_compesa_real = sub[idx_s - 1]
            if s_val == "a executar" and idx_s > 0:
                inv_compesa_exec = sub[idx_s - 1]
    d["compesa_investimento_total"] = inv_compesa_tot
    d["compesa_realizados"] = inv_compesa_real
    d["compesa_a_executar"] = inv_compesa_exec

    # 7. Investimentos Concessionária (2026-2060)
    inv_conc_tot = ""
    inv_conc_agua = ""
    inv_conc_esgoto = ""
    if "CONCESSIONÁRIA" in blocks:
        iconc = blocks.index("CONCESSIONÁRIA")
        sub = blocks[iconc + 1 : iconc + 15]
        for idx_s, s_val in enumerate(sub):
            if s_val == "investimento total" and idx_s > 0 and not inv_conc_tot:
                inv_conc_tot = sub[idx_s - 1]
            if s_val == "distribuição de água" and idx_s > 0:
                inv_conc_agua = sub[idx_s - 1]
            if s_val == "esgotamento sanitário" and idx_s > 0:
                inv_conc_esgoto = sub[idx_s - 1]
    d["concessionaria_investimento_total"] = inv_conc_tot
    d["concessionaria_agua"] = inv_conc_agua
    d["concessionaria_esgoto"] = inv_conc_esgoto

    # 8. Antes e Agora (Urbano)
    vaz_ant = ""
    vaz_atu = ""
    vaz_v = ""
    pipa_ant = ""
    pipa_atu = ""
    pipa_v = ""
    if "ANTES E AGORA" in blocks:
        ia = blocks.index("ANTES E AGORA")
        sub = blocks[ia : ia + 25]
        if "Vazão" in sub:
            iv = sub.index("Vazão")
            vals = [item_v for item_v in sub[iv + 1 : iv + 6] if item_v != "L/s"]
            if len(vals) >= 3:
                vaz_ant, vaz_atu, vaz_v = vals[0], vals[1], vals[2]
        if "Caminhões-pipa" in sub:
            ip = sub.index("Caminhões-pipa")
            vals = [item_p for item_p in sub[ip + 1 : ip + 6] if item_p not in ("por ano", "ano")]
            if len(vals) >= 3:
                pipa_ant, pipa_atu, pipa_v = vals[0], vals[1], vals[2]
    d["urbano_vazao_anterior"] = vaz_ant
    d["urbano_vazao_atual"] = vaz_atu
    d["urbano_vazao_variacao"] = vaz_v
    d["urbano_pipa_ano_anterior"] = pipa_ant
    d["urbano_pipa_ano_atual"] = pipa_atu
    d["urbano_pipa_ano_variacao"] = pipa_v

    # 9. Principais Ações / Obras
    acoes_list = []
    if "PRINCIPAIS AÇÕES" in blocks:
        ipa = blocks.index("PRINCIPAIS AÇÕES")
        end_idx = blocks.index("RODÍZIO") if "RODÍZIO" in blocks else (blocks.index("03") if "03" in blocks else len(blocks))
        raw_acts = blocks[ipa + 1 : end_idx]
        buf = []
        for act in raw_acts:
            if act in ("Ações a informar", "—") or act.startswith("03"):
                continue
            if buf and not buf[-1].endswith((".", ";", ":")) and len(act.split()) <= 3 and act[0].islower():
                buf[-1] = buf[-1] + " " + act
            else:
                buf.append(act)
        for act in buf:
            acoes_list.append((municipio, cod_ibge, act))
    d["total_acoes_obras"] = len(acoes_list)
    d["principais_acoes"] = " | ".join([a[2] for a in acoes_list])

    # 10. Regime de Rodízio
    rod_ant = ""
    rod_atu = ""
    if "RODÍZIO" in blocks:
        ir = blocks.index("RODÍZIO")
        end_r = blocks.index("03") if "03" in blocks else len(blocks)
        sub_r = blocks[ir + 1 : end_r]
        if "Anterior" in sub_r and "Atual" in sub_r:
            idx_ant = sub_r.index("Anterior")
            idx_atu = sub_r.index("Atual")
            rod_ant = " ".join(sub_r[idx_ant + 1 : idx_atu])
            rod_atu = " ".join(sub_r[idx_atu + 1 :])
        elif "Atual" in sub_r:
            idx_atu = sub_r.index("Atual")
            rod_atu = " ".join(sub_r[idx_atu + 1 :])
    d["rodizio_anterior"] = rod_ant
    d["rodizio_atual"] = rod_atu

    # 11. Zona Rural
    dessal_exec = ""
    dessal_em_exec = ""
    dessal_tot = ""
    pocos_func = ""
    pipa_fed = ""
    inv_r_tot = ""
    inv_r_real = ""
    inv_r_exec = ""
    inv_r_obs = ""

    if "03" in blocks:
        i03 = blocks.index("03")
        sub_03 = blocks[i03:]
        if "DESSALINIZADORES" in sub_03 and "TOTAL" in sub_03:
            itot = sub_03.index("TOTAL")
            cand = [x for x in sub_03[itot + 1 : itot + 6] if x not in ("Poços em funcionamento", "Caminhões-pipa", "INVESTIMENTOS")]
            if len(cand) >= 3:
                dessal_exec, dessal_em_exec, dessal_tot = cand[0], cand[1], cand[2]
        if "Poços em funcionamento" in sub_03:
            ipoc = sub_03.index("Poços em funcionamento")
            if ipoc + 1 < len(sub_03):
                pocos_func = sub_03[ipoc + 1]
        for idx_p, item_p in enumerate(sub_03):
            if "Operação Pipa" in item_p and idx_p + 1 < len(sub_03):
                pipa_fed = sub_03[idx_p + 1]
                break
        if "INVESTIMENTOS" in sub_03:
            inv_idx = sub_03.index("INVESTIMENTOS")
            sub_inv = sub_03[inv_idx + 1 :]
            for idx_i, item_i in enumerate(sub_inv):
                if item_i == "investimento total" and idx_i > 0 and not inv_r_tot:
                    inv_r_tot = sub_inv[idx_i - 1]
                if item_i == "realizados" and idx_i > 0:
                    inv_r_real = sub_inv[idx_i - 1]
                if item_i == "a executar" and idx_i > 0:
                    inv_r_exec = sub_inv[idx_i - 1]
            if "Sem investimentos" in " ".join(sub_inv):
                inv_r_obs = "Sem investimentos em dessalinizadores ou poços no município."

    d["rural_dessalinizadores_executados"] = dessal_exec
    d["rural_dessalinizadores_em_execucao"] = dessal_em_exec
    d["rural_dessalinizadores_total"] = dessal_tot
    d["rural_pocos_funcionamento"] = pocos_func
    d["rural_pipa_operacao_federal"] = pipa_fed
    d["rural_investimento_total"] = inv_r_tot
    d["rural_investimento_realizados"] = inv_r_real
    d["rural_investimento_a_executar"] = inv_r_exec
    d["rural_investimento_obs"] = inv_r_obs

    return d, mananciais_list, acoes_list


# ==============================================================================
# 4. GERAÇÃO DE PAINEL EXCEL FORMATADO (.XLSX)
# ==============================================================================
def create_styled_excel(df_municipios, df_mananciais, df_acoes, output_path: str):
    wb = openpyxl.Workbook()

    header_font = Font(name="Calibri", size=11, bold=True, color="FFFFFF")
    header_fill = PatternFill(start_color="1F497D", end_color="1F497D", fill_type="solid")
    sub_fill = PatternFill(start_color="366092", end_color="366092", fill_type="solid")
    border_thin = Side(border_style="thin", color="D9D9D9")
    cell_border = Border(left=border_thin, right=border_thin, top=border_thin, bottom=border_thin)

    # ----------------------------------------------------
    # Aba 1: Diagnóstico dos Municípios
    # ----------------------------------------------------
    ws1 = wb.active
    ws1.title = "Municípios - Diagnóstico"
    ws1.views.sheetView[0].showGridLines = True

    cols_ws1 = list(df_municipios[0].keys())
    ws1.append(cols_ws1)

    for cell in ws1[1]:
        cell.font = header_font
        cell.fill = header_fill
        cell.alignment = Alignment(horizontal="center", vertical="center")

    for row_idx, item in enumerate(df_municipios, start=2):
        row_vals = [item[c] for c in cols_ws1]
        ws1.append(row_vals)
        for c_idx in range(1, len(cols_ws1) + 1):
            cell = ws1.cell(row=row_idx, column=c_idx)
            cell.border = cell_border
            if c_idx in (1, 3, 5, 6, 8, 9):
                cell.alignment = Alignment(horizontal="center")

    # ----------------------------------------------------
    # Aba 2: Mananciais e Status Operacional
    # ----------------------------------------------------
    ws2 = wb.create_sheet(title="Mananciais Detalhados")
    ws2.views.sheetView[0].showGridLines = True
    cols_ws2 = ["municipio", "cod_ibge", "manancial", "status_operacional"]
    ws2.append(cols_ws2)
    for cell in ws2[1]:
        cell.font = header_font
        cell.fill = sub_fill
        cell.alignment = Alignment(horizontal="center", vertical="center")

    for row_idx, m in enumerate(df_mananciais, start=2):
        ws2.append(list(m))
        for c_idx in range(1, len(cols_ws2) + 1):
            cell = ws2.cell(row=row_idx, column=c_idx)
            cell.border = cell_border
            if c_idx in (2, 4):
                cell.alignment = Alignment(horizontal="center")

    # ----------------------------------------------------
    # Aba 3: Obras e Principais Ações
    # ----------------------------------------------------
    ws3 = wb.create_sheet(title="Obras e Ações")
    ws3.views.sheetView[0].showGridLines = True
    cols_ws3 = ["municipio", "cod_ibge", "acao_obra"]
    ws3.append(cols_ws3)
    for cell in ws3[1]:
        cell.font = header_font
        cell.fill = sub_fill
        cell.alignment = Alignment(horizontal="center", vertical="center")

    for row_idx, a in enumerate(df_acoes, start=2):
        ws3.append(list(a))
        for c_idx in range(1, len(cols_ws3) + 1):
            cell = ws3.cell(row=row_idx, column=c_idx)
            cell.border = cell_border
            if c_idx == 2:
                cell.alignment = Alignment(horizontal="center")

    # Autoajuste de largura de colunas em todas as abas
    for ws in [ws1, ws2, ws3]:
        for col in ws.columns:
            max_len = 0
            col_letter = get_column_letter(col[0].column)
            for cell in col:
                val_str = str(cell.value or "")
                if len(val_str) > max_len:
                    max_len = len(val_str)
            ws.column_dimensions[col_letter].width = min(max(max_len + 3, 12), 60)

    wb.save(output_path)
    print(f"[OK] Arquivo Excel gerado com sucesso em: {output_path}")


# ==============================================================================
# 5. EXECUÇÃO PRINCIPAL
# ==============================================================================
def run_extraction(pdf_path: str = DEFAULT_PDF_PATH, output_xlsx: str = OUTPUT_XLSX_PATH):
    print("=" * 70)
    print("Iniciando raspagem técnica do Caderno de Abastecimento PE...")
    print(f"PDF de Entrada: {pdf_path}")
    print(f"Destino Excel : {output_xlsx}")
    print("=" * 70)

    ibge_ref, rd_ref = load_reference_data()
    engine = PdfTextEngine(pdf_path)

    municipios_rows = []
    all_mananciais = []
    all_acoes = []

    # Municípios começam na página 8 (índice 7 dos kids) até 192 (índice 191)
    for kid_idx in range(7, 192):
        page_num = kid_idx + 1
        page_id = engine.kids[kid_idx]
        blocks = engine.extract_page_blocks(page_id)
        if not blocks:
            continue

        row_dict, mananciais, acoes = parse_municipality_page(blocks, page_num, ibge_ref, rd_ref)
        municipios_rows.append(row_dict)
        all_mananciais.extend(mananciais)
        all_acoes.extend(acoes)

        if len(municipios_rows) % 30 == 0 or len(municipios_rows) == 185:
            print(f"Progresso: {len(municipios_rows)}/185 municípios processados...")

    print("-" * 70)
    print(f"Total de Municípios raspados : {len(municipios_rows)}")
    print(f"Total de Registros Mananciais: {len(all_mananciais)}")
    print(f"Total de Obras e Ações       : {len(all_acoes)}")
    print("-" * 70)

    create_styled_excel(municipios_rows, all_mananciais, all_acoes, output_xlsx)
    return municipios_rows


if __name__ == "__main__":
    run_extraction()

import re
import time
from pathlib import Path
import pandas as pd
import requests
from lxml import html
from playwright.sync_api import sync_playwright

# 1. Configurações de diretórios
pasta_bruta = Path(r"C:\dados\dados brutos\aurora\incentivos")
pasta_bruta.mkdir(parents=True, exist_ok=True)
arquivo_catalogo = pasta_bruta / "Alepe_Catalogo_Incentivos.xlsx"

pasta_final = Path(r"C:\dados\aurora")
pasta_final.mkdir(parents=True, exist_ok=True)
arquivo_final = pasta_final / "Alepe_Incentivos_Condicoes_Detalhadas.parquet"

PROGRAMAS = ["PRODEPE", "PRODEAUTO", "PROIND", "PEAP", "PROCALÇADO", "PRODINPE"]

def clean_text(text):
    if not isinstance(text, str):
        return ""
    text = re.sub(r'[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]', '-', text)
    text = re.sub(r'[\u2018\u2019\u201a\u201b\u201c\u201d\u201e\u201f]', '"', text)
    return re.sub(r'\s+', ' ', text).strip()

def extrair_empresas_ementa(ementa):
    if not isinstance(ementa, str) or not ementa.strip():
        return ""
    t = clean_text(ementa)

    padroes_gerais = [
        r'diretrizes orçamentárias', r'regulamenta a lei', r'regulamenta o programa',
        r'institui o programa', r'consolida e altera o programa', r'relação dos produtos enquadrados',
        r'montante mínimo de recolhimento', r'crédito suplementar', r'fundo prodepe'
    ]
    termos_individuais = [r'à empresa', r'ao contribuinte', r'pelo contribuinte', r'às empresas']

    if any(re.search(p, t, re.IGNORECASE) for p in padroes_gerais) and not any(re.search(p, t, re.IGNORECASE) for p in termos_individuais):
        return "[Norma Geral / Regulamentar]"

    delimitadores = r'(?:[\.\;]|\,\s*(?:pelo[s]?\s+Decreto|transferid|atualmente\s+denominada|inscrit|situad|com\s+sede|relativamente|cujo|em\s+razão)|$)'
    
    padrao_empresa = re.compile(
        r'(?:à|a|da|para a|para as|pela|pelo|ao|favor da)\s+(?:empresa|empresas|contribuinte|contribuintes|sociedade|filial da empresa)\s+(.+?)' + delimitadores,
        re.IGNORECASE
    )
    padrao_denominada = re.compile(
        r'atualmente denominada\s+(.+?)' + delimitadores,
        re.IGNORECASE
    )

    encontradas = []
    for m in padrao_empresa.finditer(t):
        trecho = m.group(1).strip(' .,;-\t')
        if 'transferid' in trecho.lower():
            partes = re.split(r'transferid[^\s]*\s+(?:posteriormente\s+)?(?:para\s+a\s+empresa\s+)?', trecho, flags=re.IGNORECASE)
            for p in partes:
                cand = p.strip(' .,;-\t')
                if len(cand) >= 3 and not cand.lower().startswith('de que trata'):
                    encontradas.append(cand.upper())
        else:
            if len(trecho) >= 3 and not trecho.lower().startswith('de que trata') and not trecho.lower().startswith('que concede'):
                encontradas.append(trecho.upper())

    for m in padrao_denominada.finditer(t):
        trecho = m.group(1).strip(' .,;-\t')
        if len(trecho) >= 3:
            encontradas.append(trecho.upper())

    finais = []
    for cand in encontradas:
        cand = re.sub(r'\s+', ' ', cand).strip(' .,;-\t')
        if 3 <= len(cand) and len(cand.split()) <= 30:
            if not any(w in cand.lower() for w in ['que concede', 'que dispõe', 'de que trata', 'lei nº', 'decreto nº']):
                if cand not in finais:
                    finais.append(cand)

    if finais:
        return " | ".join(finais)

    if any(w in t.lower() for w in ['modifica', 'altera', 'institui', 'regulamenta', 'revoga', 'dispõe sobre a adequação', 'extingue']):
        return "[Norma Geral / Regulamentar]"

    return "[Não Identificada na Ementa]"

# ==============================================================================
# ETAPA 1: RPA ALEPE LEGIS (COM COLUNA EMPRESA NO EXCEL BRUTO)
# ==============================================================================
def executar_rpa_paginacao():
    print("==================================================")
    print("   INICIANDO RPA UNIFICADO - ALEPE LEGIS")
    print("==================================================")
    
    registros_acumulados = []
    chaves_coletadas = set()

    if arquivo_catalogo.exists():
        df_existente = pd.read_excel(arquivo_catalogo, dtype=str)
        # Garante que as colunas essenciais existem no arquivo existente
        if "Programa" not in df_existente.columns:
            df_existente["Programa"] = "PRODEPE"
        if "Empresa" not in df_existente.columns:
            df_existente["Empresa"] = df_existente["Ementa"].apply(extrair_empresas_ementa)

        registros_acumulados = df_existente.to_dict(orient="records")
        for r in registros_acumulados:
            chaves_coletadas.add((str(r.get("Programa", "")), str(r.get("ID_Norma", ""))))
        print(f"Catálogo carregado com {len(registros_acumulados)} registros pré-existentes.")

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        context = browser.new_context(viewport={"width": 1366, "height": 768})
        page = context.new_page()

        for programa in PROGRAMAS:
            # Se todas as normas do programa já foram coletadas na sessão anterior, pode seguir
            print(f"\n>>> Verificando política: {programa} <<<")
            page.goto("https://legis.alepe.pe.gov.br/pesquisa.aspx", timeout=60000)

            page.fill("#tbxTextoPesquisa", programa)
            try:
                with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                    page.click("#btnPesquisar")
            except Exception:
                page.wait_for_timeout(3000)

            page.wait_for_selector("#lblQtd, #msgResultBuscaSimples", timeout=20000)
            qtd_elem = page.locator("#lblQtd")
            qtd_total = int(qtd_elem.inner_text().strip()) if qtd_elem.count() > 0 else 0
            print(f"Resultados no portal para {programa}: {qtd_total}")

            if qtd_total == 0:
                continue

            if qtd_total > 25:
                tamanho_atual = page.locator("#ddlTamPagina").input_value()
                if tamanho_atual != "200":
                    try:
                        with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                            page.select_option("#ddlTamPagina", "200")
                        page.wait_for_selector("table.table-hover", timeout=30000)
                        page.wait_for_timeout(1000)
                    except Exception:
                        pass

            pagina_atual = 1

            while True:
                html_pagina = page.content()
                tree = html.fromstring(html_pagina)
                linhas = tree.xpath('//table[contains(@class, "table-hover")]//tbody//tr')

                novos_do_ciclo = 0
                for tr in linhas:
                    td_norma = tr.xpath('.//td[contains(@class, "td-nome-norma")]')
                    td_textos = tr.xpath('.//td[contains(@class, "textos")]')
                    td_ementa = tr.xpath('.//td[contains(@class, "ementa-norma")]')

                    if td_norma and td_ementa:
                        norma_nome = " ".join([t.strip() for t in td_norma[0].xpath('.//span[contains(@class, "nome-norma")]//text()') if t.strip()])
                        pub_data = " ".join([t.strip() for t in td_norma[0].xpath('.//span[contains(@class, "publicacao")]//text()') if t.strip()])

                        links_txt = td_textos[0].xpath('.//a[contains(@href, "texto.aspx")]/@href') if td_textos else []
                        link_orig = links_txt[0] if links_txt else ""
                        if link_orig and not link_orig.startswith("http"):
                            link_orig = f"https://legis.alepe.pe.gov.br/{link_orig}"

                        match_id = re.search(r'id=(\d+)', link_orig)
                        norma_id = match_id.group(1) if match_id else ""

                        ementa_nodes = td_ementa[0].xpath('.//div[contains(@class, "fLeft")]//text()') or td_ementa[0].xpath('.//text()')
                        ementa = re.sub(r'\s+', ' ', " ".join([t.strip() for t in ementa_nodes if t.strip()]))

                        # Extrai a empresa da ementa
                        empresa_extraida = extrair_empresas_ementa(ementa)

                        chave = (programa, norma_id)
                        if norma_id and chave not in chaves_coletadas:
                            chaves_coletadas.add(chave)
                            item_novo = {
                                "Programa": programa,
                                "ID_Norma": norma_id,
                                "Norma": norma_nome,
                                "Empresa": empresa_extraida,
                                "Publicacao": pub_data,
                                "URL_Texto": link_orig,
                                "Ementa": ementa
                            }
                            registros_acumulados.append(item_novo)
                            novos_do_ciclo += 1

                if novos_do_ciclo > 0:
                    df_salvar = pd.DataFrame(registros_acumulados)
                    # Reordena colunas com Programa e Empresa em destaque
                    cols_base = ["Programa", "Empresa", "ID_Norma", "Norma", "Publicacao", "URL_Texto", "Ementa"]
                    cols_finais = [c for c in cols_base if c in df_salvar.columns] + [c for c in df_salvar.columns if c not in cols_base]
                    df_salvar[cols_finais].to_excel(arquivo_catalogo, index=False)
                    print(f"[{programa} - Pág {pagina_atual}] +{novos_do_ciclo} salvos. Total acumulado: {len(registros_acumulados)}")

                botao_prox = page.locator("#lbtnProx")
                if botao_prox.count() == 0 or not botao_prox.is_visible():
                    break

                parent_class = botao_prox.evaluate("el => (el.parentElement ? el.parentElement.className : '')")
                if "disabled" in parent_class.lower():
                    break

                try:
                    with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                        botao_prox.click()
                    page.wait_for_selector("table.table-hover", timeout=30000)
                    page.wait_for_timeout(1000)
                except Exception:
                    page.wait_for_timeout(2500)

                pagina_atual += 1

        browser.close()

    # Aplica a extração de empresa retroativamente caso já houvesse registros sem a coluna
    df_final_cat = pd.DataFrame(registros_acumulados)
    if "Empresa" not in df_final_cat.columns or df_final_cat["Empresa"].isna().any():
        df_final_cat["Empresa"] = df_final_cat["Ementa"].apply(extrair_empresas_ementa)
        df_final_cat.to_excel(arquivo_catalogo, index=False)

    print("\n--- ETAPA 1 CONCLUÍDA: Catálogo consolidado em Excel com empresas ---")

# ==============================================================================
# ETAPA 2: PARSER DO TEXTO INTEGRAL (SALVA EM .PARQUET)
# ==============================================================================
def extrair_detalhes_contratuais():
    print("\n==================================================")
    print("   INICIANDO ETAPA 2: PARSER DE CONDIÇÕES")
    print("==================================================")
    if not arquivo_catalogo.exists():
        print(f"Arquivo {arquivo_catalogo} não encontrado!")
        return

    df_catalogo = pd.read_excel(arquivo_catalogo, dtype=str)
    print(f"Total de registros a processar: {len(df_catalogo)}")

    session = requests.Session()
    session.headers.update({"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"})

    detalhes = []
    for idx, row in df_catalogo.iterrows():
        url = row.get("URL_Texto", "")
        percentual = ""
        municipio = ""
        prazo_anos = ""
        enquadramento = ""

        if pd.notna(url) and url.startswith("http"):
            try:
                resp = session.get(url, timeout=10)
                if resp.status_code == 200:
                    tree_doc = html.fromstring(resp.text)
                    texto = " ".join(tree_doc.xpath("//body//text()"))
                    texto = re.sub(r"\s+", " ", texto)

                    # 1. Percentual do benefício (ex.: 75%, 85%, 90%)
                    m_perc = re.search(r"(?:crédito presumido|estímulo|incentivo).*?(\d{1,2}(?:,\d+)?\s*%)", texto, re.IGNORECASE)
                    if m_perc: percentual = m_perc.group(1)

                    # 2. Município de implantação/sede
                    m_muni = re.search(r"estabelecid[ao]\s+na?[^,]+,[^,]+,\s*([^/,\n]+)/PE", texto, re.IGNORECASE)
                    if m_muni: municipio = m_muni.group(1).strip()

                    # 3. Prazo contratual de fruição
                    m_prazo = re.search(r"(\d{1,2})\s*(?:\([^)]+\))?\s*anos", texto, re.IGNORECASE)
                    if m_prazo: prazo_anos = m_prazo.group(1)

                    # 4. Tipo de enquadramento
                    m_enq = re.search(r"(central de distribuição|indústria|comércio atacadista|importador|sistemista|montadora)", texto, re.IGNORECASE)
                    if m_enq: enquadramento = m_enq.group(1).title()
            except Exception:
                pass

        detalhes.append({
            "Percentual_ICMS": percentual,
            "Municipio_Decreto": municipio,
            "Prazo_Anos": prazo_anos,
            "Enquadramento": enquadramento
        })

        if (idx + 1) % 100 == 0 or (idx + 1) == len(df_catalogo):
            print(f"Progresso: {idx + 1}/{len(df_catalogo)} textos analisados...")
            time.sleep(0.05)

    df_detalhes = pd.DataFrame(detalhes)
    df_resultado_final = pd.concat([df_catalogo, df_detalhes], axis=1)

    # Ordenação de colunas no formato colunar final
    cols_finais = ["Programa", "Empresa", "ID_Norma", "Norma", "Publicacao", "Percentual_ICMS", "Municipio_Decreto", "Prazo_Anos", "Enquadramento", "URL_Texto", "Ementa"]
    cols_existentes = [c for c in cols_finais if c in df_resultado_final.columns] + [c for c in df_resultado_final.columns if c not in cols_finais]
    df_resultado_final = df_resultado_final[cols_existentes]

    # Exportação para Parquet em C:\dados\aurora
    df_resultado_final.to_parquet(arquivo_final, engine="pyarrow", index=False)
    print(f"\nBase Parquet gerada com sucesso em:\n{arquivo_final}")

if __name__ == "__main__":
    executar_rpa_paginacao()
    extrair_detalhes_contratuais()

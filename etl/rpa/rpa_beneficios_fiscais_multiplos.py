import re
import time
from pathlib import Path
import pandas as pd
import requests
from lxml import html
from playwright.sync_api import sync_playwright

# 1. Configuração de diretórios
pasta_bruta = Path(r"C:\dados\dados brutos\aurora\incentivos")
pasta_bruta.mkdir(parents=True, exist_ok=True)

# Arquivo anterior do PRODEPE e o novo consolidado
arquivo_prodepe_original = pasta_bruta / "Alepe_Catalogo_PRODEPE.xlsx"
arquivo_catalogo = pasta_bruta / "Alepe_Catalogo_Incentivos.xlsx"

pasta_final = Path(r"C:\dados\aurora")
pasta_final.mkdir(parents=True, exist_ok=True)
arquivo_final = pasta_final / "Alepe_Incentivos_Condicoes_Detalhadas.parquet"

# Novas políticas a pesquisar (PRODEPE não será re-pesquisado)
PROGRAMAS_NOVOS = ["PRODEAUTO", "PROIND", "PEAP", "PROCALÇADO", "PRODINPE"]

# ==============================================================================
# ETAPA 1: RPA MULTI-PROGRAMAS NO ALEPE LEGIS (SALVA EM .XLSX)
# ==============================================================================
def executar_rpa_paginacao():
    print("--- INICIANDO RPA DE POLÍTICAS DE INCENTIVO ALEPE LEGIS ---")
    
    registros_acumulados = []
    chaves_coletadas = set()  # Tupla (Programa, ID_Norma)

    # 1. Carrega ou migra a base existente preservando o PRODEPE
    if arquivo_catalogo.exists():
        df_existente = pd.read_excel(arquivo_catalogo, dtype=str)
        if "Programa" not in df_existente.columns:
            df_existente["Programa"] = "PRODEPE"
        registros_acumulados = df_existente.to_dict(orient="records")
        for r in registros_acumulados:
            chaves_coletadas.add((str(r.get("Programa", "")), str(r.get("ID_Norma", ""))))
        print(f"Catálogo carregado com {len(registros_acumulados)} registros prévios.")
        
    elif arquivo_prodepe_original.exists():
        print("Migrando dados do PRODEPE já existentes...")
        df_prodepe = pd.read_excel(arquivo_prodepe_original, dtype=str)
        df_prodepe["Programa"] = "PRODEPE"
        registros_acumulados = df_prodepe.to_dict(orient="records")
        for r in registros_acumulados:
            chaves_coletadas.add(("PRODEPE", str(r.get("ID_Norma", ""))))
        # Salva o arquivo novo com a coluna Programa
        pd.DataFrame(registros_acumulados).to_excel(arquivo_catalogo, index=False)
        print(f"{len(registros_acumulados)} registros do PRODEPE preservados com sucesso!")

    # Programas que já foram 100% raspados
    programas_na_base = {r.get("Programa") for r in registros_acumulados}

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        context = browser.new_context(viewport={"width": 1366, "height": 768})
        page = context.new_page()

        for programa in PROGRAMAS_NOVOS:
            if programa in programas_na_base:
                print(f"\n[PULANDO] {programa} já consta na base de dados.")
                continue

            print(f"\n{'='*50}")
            print(f"Iniciando busca para a política: {programa}")
            print(f"{'='*50}")

            page.goto("https://legis.alepe.pe.gov.br/pesquisa.aspx", timeout=60000)

            # Preenche o input e clica em Pesquisar
            page.fill("#tbxTextoPesquisa", programa)
            try:
                with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                    page.click("#btnPesquisar")
            except Exception:
                page.wait_for_timeout(3000)

            # Verifica quantidade de resultados
            page.wait_for_selector("#lblQtd, #msgResultBuscaSimples", timeout=20000)
            qtd_element = page.locator("#lblQtd")
            qtd_total = int(qtd_element.inner_text().strip()) if qtd_element.count() > 0 else 0
            print(f"Resultados encontrados para {programa}: {qtd_total}")

            if qtd_total == 0:
                print(f"Nenhum decreto localizado para {programa}.")
                continue

            # Ajusta para 200 resultados se houver mais de 25 registros
            if qtd_total > 25:
                tamanho_atual = page.locator("#ddlTamPagina").input_value()
                if tamanho_atual != "200":
                    print("Ajustando exibição para 200 resultados por página...")
                    try:
                        with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                            page.select_option("#ddlTamPagina", "200")
                        page.wait_for_selector("table.table-hover", timeout=30000)
                        page.wait_for_timeout(1000)
                    except Exception as e:
                        print(f"Aviso ao alterar paginação: {e}")

            pagina_atual = 1

            while True:
                html_pagina = page.content()
                tree = html.fromstring(html_pagina)
                linhas = tree.xpath('//table[contains(@class, "table-hover")]//tbody//tr')

                novos_do_lote = 0
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

                        chave = (programa, norma_id)
                        if norma_id and chave not in chaves_coletadas:
                            chaves_coletadas.add(chave)
                            item_novo = {
                                "Programa": programa,
                                "ID_Norma": norma_id,
                                "Norma": norma_nome,
                                "Publicacao": pub_data,
                                "URL_Texto": link_orig,
                                "Ementa": ementa
                            }
                            registros_acumulados.append(item_novo)
                            novos_do_lote += 1

                # Salva o arquivo Excel incrementalmente
                if novos_do_lote > 0:
                    pd.DataFrame(registros_acumulados).to_excel(arquivo_catalogo, index=False)
                    print(f"[{programa} - Pág {pagina_atual}] +{novos_do_lote} decretos salvos. Total geral no Excel: {len(registros_acumulados)}")

                # Paginação
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
    print("\n--- ETAPA 1 CONCLUÍDA: Catálogo consolidado em Excel ---")

# ==============================================================================
# ETAPA 2: PARSER DO TEXTO INTEGRAL (SALVA EM .PARQUET)
# ==============================================================================
def extrair_detalhes_contratuais():
    print("\n--- INICIANDO ETAPA 2: DETALHAMENTO DE CONDIÇÕES CONTRATUAIS ---")
    if not arquivo_catalogo.exists():
        print(f"Arquivo {arquivo_catalogo} não encontrado!")
        return

    df_catalogo = pd.read_excel(arquivo_catalogo, dtype=str)
    print(f"Total de registros a detalhar: {len(df_catalogo)}")

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
                    texto_completo = " ".join(tree_doc.xpath("//body//text()"))
                    texto_completo = re.sub(r"\s+", " ", texto_completo)

                    m_perc = re.search(r"(?:crédito presumido|estímulo|incentivo).*?(\d{1,2}(?:,\d+)?\s*%)", texto_completo, re.IGNORECASE)
                    if m_perc: percentual = m_perc.group(1)

                    m_muni = re.search(r"estabelecid[ao]\s+na?[^,]+,[^,]+,\s*([^/,\n]+)/PE", texto_completo, re.IGNORECASE)
                    if m_muni: municipio = m_muni.group(1).strip()

                    m_prazo = re.search(r"(\d{1,2})\s*(?:\([^)]+\))?\s*anos", texto_completo, re.IGNORECASE)
                    if m_prazo: prazo_anos = m_prazo.group(1)

                    m_enq = re.search(r"(central de distribuição|indústria|comércio atacadista|importador|sistemista|montadora)", texto_completo, re.IGNORECASE)
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

    # Garante a ordenação com Programa como primeira coluna
    colunas_ordenadas = ["Programa"] + [col for col in df_resultado_final.columns if col != "Programa"]
    df_resultado_final = df_resultado_final[colunas_ordenadas]

    # Salva no formato Parquet em C:\dados\aurora
    df_resultado_final.to_parquet(arquivo_final, engine="pyarrow", index=False)
    print(f"\nFinalizado com sucesso! Base Parquet disponível em:\n{arquivo_final}")

if __name__ == "__main__":
    executar_rpa_paginacao()
    extrair_detalhes_contratuais()

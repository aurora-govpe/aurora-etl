import re
import time
from pathlib import Path
import pandas as pd
import requests
from lxml import html
from playwright.sync_api import sync_playwright

# 1. Definição dos caminhos locais
pasta_bruta = Path(r"C:\dados\dados brutos\aurora\incentivos")
pasta_bruta.mkdir(parents=True, exist_ok=True)
arquivo_catalogo = pasta_bruta / "Alepe_Catalogo_PRODEPE.xlsx"

pasta_final = Path(r"C:\dados\aurora")
pasta_final.mkdir(parents=True, exist_ok=True)
arquivo_final = pasta_final / "Alepe_PRODEPE_Condicoes_Detalhadas.parquet"

# ==============================================================================
# ETAPA 1: RPA DE PAGINAÇÃO NO ALEPE LEGIS (SALVA EM .XLSX)
# ==============================================================================
def executar_rpa_paginacao():
    print("--- INICIANDO RPA ALEPE LEGIS ---")
    
    coletados_ids = set()
    registros_acumulados = []
    
    # Se já existir progresso salvo, carrega para retomar sem duplicar
    if arquivo_catalogo.exists():
        try:
            df_existente = pd.read_excel(arquivo_catalogo, dtype=str)
            if "ID_Norma" in df_existente.columns:
                coletados_ids = set(df_existente["ID_Norma"].dropna().tolist())
                registros_acumulados = df_existente.to_dict(orient="records")
                print(f"Retomando execução: {len(coletados_ids)} normas já catalogadas.")
        except Exception as e:
            print(f"Não foi possível ler progresso anterior ({e}), iniciando do zero.")

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        context = browser.new_context(viewport={"width": 1366, "height": 768})
        page = context.new_page()

        print("Acessando portal Alepe Legis...")
        page.goto("https://legis.alepe.pe.gov.br/pesquisa.aspx", timeout=60000)

        # Preenche a busca e aguarda os resultados
        page.fill("#tbxTextoPesquisa", "PRODEPE")
        with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
            page.click("#btnPesquisar")
        page.wait_for_selector("table.table-hover", timeout=30000)

        # Ajusta para 200 resultados por página sincronizando com o postback do servidor
        tamanho_atual = page.locator("#ddlTamPagina").input_value()
        if tamanho_atual != "200":
            print("Configurando tabela para 200 resultados por página...")
            try:
                with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                    page.select_option("#ddlTamPagina", "200")
                page.wait_for_selector("table.table-hover", timeout=30000)
                page.wait_for_timeout(1500)
            except Exception as e:
                print(f"Aviso ao alterar para 200 itens ({e}). Prosseguindo com o padrão...")

        pagina_atual = 1

        while True:
            # Captura e faz o parse do HTML renderizado
            html_pagina = page.content()
            tree = html.fromstring(html_pagina)
            linhas = tree.xpath('//table[contains(@class, "table-hover")]//tbody//tr')
            
            novos_registros = []
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

                    if norma_id and norma_id not in coletados_ids:
                        coletados_ids.add(norma_id)
                        novo_item = {
                            "ID_Norma": norma_id,
                            "Norma": norma_nome,
                            "Publicacao": pub_data,
                            "URL_Texto": link_orig,
                            "Ementa": ementa
                        }
                        novos_registros.append(novo_item)
                        registros_acumulados.append(novo_item)

            # Salva o arquivo Excel a cada página processada
            if novos_registros:
                df_acumulado = pd.DataFrame(registros_acumulados)
                df_acumulado.to_excel(arquivo_catalogo, index=False)
                print(f"[Página {pagina_atual}] +{len(novos_registros)} decretos salvos. Total acumulado: {len(coletados_ids)}")

            # Localiza o botão 'Próxima' (›)
            botao_prox = page.locator("#lbtnProx")
            if botao_prox.count() == 0 or not botao_prox.is_visible():
                print("Fim das páginas alcançado!")
                break

            # Checa se o item pai está desabilitado (última página)
            parent_class = botao_prox.evaluate("el => (el.parentElement ? el.parentElement.className : '')")
            if "disabled" in parent_class.lower():
                print("Última página alcançada com sucesso!")
                break

            # Avança de página aguardando o postback ASP.NET ser concluído
            print(f"Carregando página {pagina_atual + 1}...")
            try:
                with page.expect_response(lambda r: "pesquisa.aspx" in r.url and r.request.method == "POST", timeout=30000):
                    botao_prox.click()
                page.wait_for_selector("table.table-hover", timeout=30000)
                page.wait_for_timeout(1000)
            except Exception as e:
                print(f"Aviso no carregamento da página {pagina_atual + 1} ({e}), tentando continuar...")
                page.wait_for_timeout(3000)

            pagina_atual += 1

        browser.close()

# ==============================================================================
# ETAPA 2: PARSER RÁPIDO DO TEXTO INTEGRAL (SALVA EM .PARQUET)
# ==============================================================================
def extrair_detalhes_contratuais():
    print("\n--- INICIANDO EXTRAÇÃO DE CONDIÇÕES CONTRATUAIS DOS DECRETOS ---")
    if not arquivo_catalogo.exists():
        print(f"Arquivo de catálogo {arquivo_catalogo} não encontrado!")
        return

    df_catalogo = pd.read_excel(arquivo_catalogo, dtype=str)
    print(f"Total de decretos a detalhar: {len(df_catalogo)}")

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

                    m_enq = re.search(r"(central de distribuição|indústria|comércio atacadista|importador)", texto_completo, re.IGNORECASE)
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
            time.sleep(0.1)

    df_detalhes = pd.DataFrame(detalhes)
    df_resultado_final = pd.concat([df_catalogo, df_detalhes], axis=1)
    
    # Exportação para Parquet na pasta final
    df_resultado_final.to_parquet(arquivo_final, engine="pyarrow", index=False)
    print(f"\nExtração concluída com sucesso! Base final estruturada gerada em:\n{arquivo_final}")

if __name__ == "__main__":
    executar_rpa_paginacao()
    extrair_detalhes_contratuais()

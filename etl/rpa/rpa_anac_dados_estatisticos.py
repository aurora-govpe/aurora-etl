import asyncio
import datetime
import re
import traceback
from pathlib import Path
from urllib.parse import unquote, urlparse
from playwright.async_api import async_playwright

# =============================================================================
# CONFIGURAÇÕES E MAPEAMENTO DE DIRETÓRIOS (PROJETO AURORAPRIME)
# =============================================================================
DATA_DIR = Path(r"C:\dados\dados brutos\aurora\aeroportuaria")
DATA_DIR.mkdir(parents=True, exist_ok=True)

LOG_FILE = DATA_DIR / "log_rpa_anac.txt"
URL_ANAC = "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/dados-estatisticos"

ANO_INICIAL = 2015
ANO_ATUAL = datetime.date.today().year
ANO_ANTERIOR = ANO_ATUAL - 1
ANOS_ATUALIZACAO_OBRIGATORIA = {str(ANO_ANTERIOR), str(ANO_ATUAL)}
ANOS_ALVO = [str(ano) for ano in range(ANO_INICIAL, ANO_ATUAL + 1)]


def log(mensagem: str):
    """Grava o log com data/hora no console e no arquivo de log."""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    linha = f"[{timestamp}] {mensagem}"
    print(linha)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(linha + "\n")


def sanitizar_nome_arquivo(nome: str) -> str:
    """Remove caracteres inválidos para nomes de arquivos no Windows."""
    nome_limpo = unquote(nome).strip()
    nome_limpo = re.sub(r'[<>:"/\\|?*]', "_", nome_limpo)
    return nome_limpo


def ano_ja_existe_na_pasta(pasta: Path, ano: str) -> list[Path]:
    """
    Verifica se já existe algum arquivo referente ao ano na pasta de destino
    (ignorando o arquivo de log e arquivos vazios de 0 bytes).
    """
    arquivos_encontrados = []
    for arquivo in pasta.iterdir():
        if arquivo.is_file() and arquivo.name != LOG_FILE.name:
            if ano in arquivo.name and arquivo.stat().st_size > 0:
                arquivos_encontrados.append(arquivo)
    return arquivos_encontrados


async def fechar_banner_cookies(page):
    """Fecha o banner de cookies do gov.br caso apareça na tela."""
    try:
        btn_cookies = page.locator(
            "button.accept-all, button:has-text('Aceitar cookies'), button:has-text('Rejeitar cookies')"
        ).first
        if await btn_cookies.count() > 0 and await btn_cookies.is_visible():
            await btn_cookies.click(timeout=3000)
            await asyncio.sleep(0.5)
    except Exception:
        pass


async def executar_rpa():
    with open(LOG_FILE, "w", encoding="utf-8") as f:
        f.write("=== INÍCIO DA EXECUÇÃO DO ROBÔ AURORAPRIME (ANAC - INCREMENTAL) ===\n")
        f.write(f"Diretório de Destino: {DATA_DIR}\n")
        f.write(f"Janela de Anos: {ANO_INICIAL} a {ANO_ATUAL}\n")
        f.write(f"Reprocessamento Obrigatório: {ANO_ANTERIOR} e {ANO_ATUAL}\n\n")

    # =========================================================================
    # PRÉ-FILTRAGEM: DEFINE QUAIS ANOS REALMENTE PRECISAM SER BAIXADOS
    # =========================================================================
    anos_para_baixar = []
    log("0. Verificando arquivos existentes na pasta local...")

    for ano in ANOS_ALVO:
        existentes = ano_ja_existe_na_pasta(DATA_DIR, ano)

        if ano in ANOS_ATUALIZACAO_OBRIGATORIA:
            motivo = "atualização obrigatória (ano anterior/corrente)"
            log(f"   🔄 Ano {ano}: marcado para download ({motivo}).")
            anos_para_baixar.append(ano)
        elif not existentes:
            log(f"   🆕 Ano {ano}: não encontrado na pasta. Marcado para download.")
            anos_para_baixar.append(ano)
        else:
            nomes = ", ".join(a.name for a in existentes)
            log(f"   ⏭️ Ano {ano}: já existe na pasta ({nomes}). Pulando.")

    if not anos_para_baixar:
        log("✅ Todos os anos já estão atualizados localmente. Nenhum download necessário.")
        return

    async with async_playwright() as p:
        try:
            log("1. Inicializando navegador Chromium...")
            browser = await p.chromium.launch(headless=False)
            context = await browser.new_context(
                accept_downloads=True,
                user_agent=(
                    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/128.0.0.0 Safari/537.36"
                ),
            )
            page = await context.new_page()

            log(f"2. Acessando portal da ANAC: {URL_ANAC}")
            await page.goto(URL_ANAC, wait_until="domcontentloaded", timeout=60000)
            await page.wait_for_load_state("networkidle", timeout=30000)
            await fechar_banner_cookies(page)

            # =================================================================
            # LOCALIZA EXATAMENTE A TABELA DA SEÇÃO "Base de dados subdividida por ano"
            # =================================================================
            log("3. Localizando a seção 'Base de dados subdividida por ano'...")
            titulo_secao = page.locator("text='Base de dados subdividida por ano'").first
            await titulo_secao.wait_for(state="visible", timeout=20000)
            await titulo_secao.scroll_into_view_if_needed()

            tabela_anos = titulo_secao.locator("xpath=following::table[1]")
            await tabela_anos.wait_for(state="visible", timeout=15000)
            log("   ✅ Tabela de anos localizada com sucesso!")

            total_baixados = 0

            # =================================================================
            # ITERA SOMENTE PELOS ANOS FILTRADOS (FALTANTES + ANTERIOR + CORRENTE)
            # =================================================================
            for ano in anos_para_baixar:
                log("--------------------------------------------------")
                log(f"4. Baixando ano {ano}...")

                link_ano = tabela_anos.locator(f"a:text-is('{ano}')").first

                if await link_ano.count() == 0:
                    log(f"   ⚠️ Link para o ano {ano} não encontrado na tabela.")
                    continue

                url_completa = await link_ano.evaluate("el => el.href")
                if not url_completa:
                    log(f"   ⚠️ O elemento '{ano}' não possui URL válida.")
                    continue

                log(f"   🔗 URL identificada: {url_completa}")

                nome_bruto = Path(urlparse(url_completa).path).name
                if not nome_bruto or "." not in nome_bruto:
                    nome_bruto = f"dados_estatisticos_anac_{ano}.zip"

                nome_arquivo = sanitizar_nome_arquivo(nome_bruto)
                caminho_destino = DATA_DIR / nome_arquivo

                sucesso = False

                # Estratégia 1: Clique nativo interceptando o download do navegador
                try:
                    log(f"   🖱️ Clicando no link '{ano}' para iniciar download...")
                    async with page.expect_download(timeout=45000) as download_info:
                        await link_ano.click()
                    download = await download_info.value
                    nome_sugerido = sanitizar_nome_arquivo(download.suggested_filename or nome_arquivo)
                    caminho_destino = DATA_DIR / nome_sugerido
                    await download.save_as(str(caminho_destino))
                    sucesso = True
                except Exception as e_click:
                    log(f"   ⚠️ Clique nativo não disparou download automático ({e_click}). Tentando via requisição direta...")

                # Estratégia 2 (Fallback): Download direto via sessão do Playwright
                if not sucesso:
                    try:
                        response = await context.request.get(url_completa, timeout=60000)
                        if response.ok:
                            caminho_destino.write_bytes(await response.body())
                            sucesso = True
                        else:
                            log(f"   ❌ Erro HTTP {response.status} ao baixar {url_completa}")
                    except Exception as e_req:
                        log(f"   ❌ Falha na requisição direta para {ano}: {e_req}")

                if sucesso and caminho_destino.exists():
                    tamanho_mb = caminho_destino.stat().st_size / (1024 * 1024)
                    log(f"   ✅ Arquivo salvo/atualizado: {caminho_destino} ({tamanho_mb:.2f} MB)")
                    total_baixados += 1

                await asyncio.sleep(1)

            log("==================================================")
            log("🎉 EXECUÇÃO INCREMENTAL CONCLUÍDA!")
            log(f"📦 Arquivos baixados/atualizados nesta execução: {total_baixados}")
            log(f"📁 Pasta destino: {DATA_DIR}")
            log("==================================================")

            await browser.close()

        except Exception as e:
            log(f"💥 [ERRO CRÍTICO]: {str(e)}")
            log(traceback.format_exc())


if __name__ == "__main__":
    asyncio.run(executar_rpa())

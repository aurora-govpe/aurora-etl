import csv
import json
import time
from pathlib import Path
import pandas as pd
import requests

# 1. Definição do diretório e arquivos
pasta = Path(r"C:\dados\dados brutos\aurora\incentivos")
arquivo_origem = pasta / "Beneficios_27102026.csv"
arquivo_destino = pasta / "Beneficios_com_Municipios.csv"
arquivo_cache = pasta / "cache_municipios.json"

# 2. Leitura do arquivo CSV tratando o cabeçalho duplo
linhas = []
with open(arquivo_origem, mode='r', encoding='utf-8-sig', errors='replace') as f:
    leitor = csv.reader(f, delimiter=';')
    for row in leitor:
        row_limpa = [col.strip() for col in row if col.strip() != '']
        if row_limpa:
            linhas.append(row_limpa)

cabecalho = linhas[1]
dados = linhas[2:]
df = pd.DataFrame(dados, columns=cabecalho)

# 3. Carregar progresso anterior, se houver
if arquivo_cache.exists():
    with open(arquivo_cache, 'r', encoding='utf-8') as f:
        cache = json.load(f)
else:
    cache = {}

# 4. Filtrar CNPJs únicos pendentes
cnpjs_unicos = df['CNPJ'].str.zfill(14).unique()
pendentes = [cnpj for cnpj in cnpjs_unicos if cnpj not in cache]
print(f"Total de registros na base: {len(df)}")
print(f"CNPJs únicos: {len(cnpjs_unicos)} | Em cache: {len(cache)} | Pendentes: {len(pendentes)}")

# 5. Consulta à BrasilAPI com persistência automática
for i, cnpj in enumerate(pendentes, start=1):
    url = f"https://brasilapi.com.br/api/cnpj/v1/{cnpj}"
    try:
        resp = requests.get(url, timeout=10)
        if resp.status_code == 200:
            dados_cnpj = resp.json()
            cache[cnpj] = {
                'Municipio': dados_cnpj.get('municipio', ''),
                'UF': dados_cnpj.get('uf', ''),
                'Bairro': dados_cnpj.get('bairro', ''),
                'CEP': dados_cnpj.get('cep', '')
            }
        elif resp.status_code == 404:
            cache[cnpj] = {'Municipio': 'Não encontrado', 'UF': '', 'Bairro': '', 'CEP': ''}
        else:
            cache[cnpj] = {'Municipio': f'Erro {resp.status_code}', 'UF': '', 'Bairro': '', 'CEP': ''}
    except Exception:
        cache[cnpj] = {'Municipio': 'Erro de conexão', 'UF': '', 'Bairro': '', 'CEP': ''}

    # Salva o progresso a cada 50 consultas e no final
    if i % 50 == 0 or i == len(pendentes):
        with open(arquivo_cache, 'w', encoding='utf-8') as f:
            json.dump(cache, f, ensure_ascii=False, indent=2)
        print(f"Progresso: {i}/{len(pendentes)} consultados...")

    time.sleep(0.25)  # Intervalo para respeitar o limite de requisições

# 6. Adicionar colunas ao DataFrame e salvar o arquivo final
cnpj_serie = df['CNPJ'].str.zfill(14)
df['Municipio'] = cnpj_serie.map(lambda c: cache.get(c, {}).get('Municipio', ''))
df['UF'] = cnpj_serie.map(lambda c: cache.get(c, {}).get('UF', ''))
df['Bairro'] = cnpj_serie.map(lambda c: cache.get(c, {}).get('Bairro', ''))
df['CEP'] = cnpj_serie.map(lambda c: cache.get(c, {}).get('CEP', ''))

df.to_csv(arquivo_destino, sep=';', index=False, encoding='utf-8-sig')
print(f"\nConcluído com sucesso! Arquivo gerado em:\n{arquivo_destino}")

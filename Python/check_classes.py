with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

import re
# Vamos extrair do CSS de FarolPE as classes: .top, .topr, .tag, .pbody, .answer, .tb1, table, .grid2, .fig, .cols, .qa, .gl, .fontes
css = re.search(r'<style>(.*?)</style>', content, re.DOTALL).group(1)

# Procurar definicoes de classes no CSS
classes = ['.top', '.topr', '.tag', '.pbody', '.answer', '.tb1', '.grid2', '.fig', '.cols', '.qa', '.gl', '.fontes', 'table']
for cl in classes:
    matches = re.findall(rf'({re.escape(cl)}[^{{]*\{{[^}}]*\}})', css)
    for m in matches[:3]:
        print(m[:150])

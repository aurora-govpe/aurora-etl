with open(r'C:\Users\hugo.vasconcelos\Documents\projetos\apresentacoes\NT\editor.html', 'r', encoding='utf-8') as f:
    html = f.read()

import re
m = re.search(r'<header class="editor-topbar.*?</header>', html, flags=re.DOTALL)
print('Header encontrado?', m is not None)
if m:
    print('Tamanho do header:', len(m.group(0)))

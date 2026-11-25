with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

# Imprimir a estrutura das páginas (IDs, tags principais, classes)
import re
pages = re.findall(r'<section class="page"[^>]*>.*?</section>', content, flags=re.DOTALL)
print(f'Total de páginas encontradas: {len(pages)}')

for i, p in enumerate(pages):
    # remover base64 para visualizar
    clean_p = re.sub(r'data:image/[^;]+;base64,[A-Za-z0-9+/=]+', '[BASE64_IMG]', p)
    print(f'=== PÁGINA {i+1} (Tamanho: {len(p)}) ===')
    print(clean_p[:1800])
    print('\n' + '='*50 + '\n')

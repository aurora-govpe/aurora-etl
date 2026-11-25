with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

import re
pages = re.findall(r'<section class="page"[^>]*>.*?</section>', content, flags=re.DOTALL)
clean_p3 = re.sub(r'data:image/[^;]+;base64,[A-Za-z0-9+/=]+', '[BASE64_IMG]', pages[2])
print('=== PÁGINA 3 COMPLETA ===')
print(clean_p3)

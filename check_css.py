with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

style_start = content.find('<style>')
style_end = content.find('</style>')
print('CSS length:', style_end - style_start)

# Procurar regras de @media print e .page no CSS
css = content[style_start:style_end]
for line in css.split('\n'):
    if '@media' in line or '.page' in line or 'page-break' in line or '@page' in line or 'navigation' in line:
        print(line[:200])

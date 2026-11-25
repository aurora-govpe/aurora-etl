with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

idx = content.find('<body')
print('Index of <body:', idx)
if idx != -1:
    print(content[idx:idx+4000])

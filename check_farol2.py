with open(r'c:\Users\hugo.vasconcelos\Downloads\FarolPE - Síntese da Governadora - PNADc (3T26).html', 'r', encoding='utf-8') as f:
    content = f.read()

idx = content.find('<body')
# Pular a imagem base64
img_end = content.find('>', idx + 100)
print('Após imagem:', content[img_end:img_end+5000])

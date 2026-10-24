secretarias <- tibble(
  nome = c(
    "Companhia Estadual de Habitação e Obras",
    "Pernambuco Participações e Investimentos S/A",
    "Agência de Desenvolvimento Econômico de Pernambuco S/A",
    "Secretaria de Defesa Social de Pernambuco",
    "Secretaria de Desenvolvimento Agrário, Agricultura, Pecuária e Pesca",
    "Secretaria de Administração Penitenciária e Ressocialização de Pernambuco",
    "Secretaria de Ciência, Tecnologia e Inovação de Pernambuco",
    "Secretaria de Cultura de Pernambuco",
    "Secretaria de Desenvolvimento Urbano e Habitação de Pernambuco",
    "Secretaria de Educação de Pernambuco",
    "Secretaria de Esportes de Pernambuco",
    "Secretaria de Meio Ambiente e Sustentabilidade de Pernambuco",
    "Secretaria de Saúde de Pernambuco",
    "Secretaria de Turismo e Lazer de Pernambuco",
    "Secretaria de Mobilidade e Infraestrutura de Pernambuco",
    "Secretaria de Recursos Hídricos e Saneamento de Pernambuco",
    "Secretaria de Assistência Social, Combate à Fome e Políticas sobre Drogas"
  ),
  sigla = c(
    "CEHAB", "PERPART", "ADEPE", "SDS", "SDAPP", "SEAP", "SECTI", "SECULT", "SEDUH",
    "SEE", "SEES", "SEMAS", "SES", "SETUR",
    "SEMOBI", "SRHS", "SAS"
  ),
  cor = c(
    "#27AE60", "#27AE60", # "CEHAB", "PERPART",
    "#6F2235", # SDEC: 
    "#22356F", # SDS: azul marinho (segurança)
    "#8EAD44", #"SDAPP": Secretaria de Desenvolvimento Agrário, Agricultura, Pecuária e Pesca 
    "#E67E22", # SEAP: laranja (transformação)
    "#8E44AD", # SECTI: roxo (inovação)
    "#C0392B", # SECULT: vermelho escuro (cultura)
    "#27AE60", # SEDUH: verde (desenvolvimento urbano)
    "#F1C40F", # SEE: amarelo (educação)
    "#3498DB", # SEES: azul claro (esportes)
    "#2ECC71", # SEMAS: verde claro (meio ambiente)
    "#E74C3C", # SES: vermelho (saúde)
    "#1ABC9C", # SETUR: turquesa (turismo)
    "#639162", # SEMOBI: cinza (mobilidade/infraestrutura)
    "#00A8B5", # SRHS: azul água (recursos hídricos e saneamento)
    "#A8B500"  # SAS: azul água (Secretaria de Assistência Social)
  )
) %>% arrange(sigla)

status_cores <- tibble(
  status = c(
    "Em formulação",
    "Em elaboração de projeto",
    "A licitar",
    "Em licitação",
    "A iniciar",
    "Em execução",
    "Concluída",
    "Não informado",
    "Paralisada"
  ),
  cor = c(
    "#8E44AD",
    "#D35400",
    "#2C3E50",
    "#16A085",
    "#F39C12",
    "#27AE60",
    "#2980B9",
    "#7F8C8D",
    "#FF5555"
  )
)

cores_status <- setNames(
  status_cores$cor,
  status_cores$status
)


# 1) Como tibble
eixos_cores <- tibble(
  eixo = c(
    "Conhecimento e inovação",
    "Desenvolvimento sustentável",
    "Saúde e qualidade de vida",
    "Segurança e cidadania"
  ),
  cor  = c("#377EB8", "#4DAF4A", "#800080", "#FF7F00")
)

# 2) Como named vector, útil para scales ou paletas no ggplot2, leaflet etc.
cores_eixo <- setNames(
  c("#377EB8", "#4DAF4A", "#800080", "#FF7F00"),
  c(
    "Conhecimento e inovação",
    "Desenvolvimento sustentável",
    "Saúde e qualidade de vida",
    "Segurança e cidadania"
  )
)

secretarias_tags <- tibble(
  sigla      = c(
    "CEHAB", "PERPART", "ADEPE", "SEMOBI", "SRHS",   "SEE",    "SES",    "SEDUH",
    "SDS",    "SEAP",   "SDAPP",  "SEES",   "SECTI",
    "SECULT", "SEMAS",  "SETUR", "SAS"
  ),
  secretaria = c(
    "Companhia Estadual de Habitação e Obras",
    "Pernambuco Participações e Investimentos S/A",
    "Agência de Desenvolvimento Econômico de Pernambuco S/A",
    "Mobilidade e Infraestrutura de Pernambuco",
    "Recursos Hídricos e Saneamento de Pernambuco",
    "Educação de Pernambuco",
    "Saúde de Pernambuco",
    "Desenvolvimento Urbano e Habitação de Pernambuco",
    "Defesa Social de Pernambuco",
    "Administração Penitenciária e Ressocialização de Pernambuco",
    "Desenvolvimento Agrário, Agricultura, Pecuária e Pesca",
    "Esportes de Pernambuco",
    "Ciência, Tecnologia e Inovação de Pernambuco",
    "Cultura de Pernambuco",
    "Meio Ambiente e Sustentabilidade de Pernambuco",
    "Turismo e Lazer de Pernambuco",
    "Secretaria de Assistência Social, Combate à Fome e Políticas sobre Drogas"
  ),
  tag = c(
    "Infraestrutura & Ambiente",          # CEHAB
    "Infraestrutura & Ambiente",          # PERPART
    "Desenvolvimento Econômico & Inovação",# ADEPE
    rep("Infraestrutura & Ambiente", 2),  # SEMOBI, SRHS
    rep("Serviços Sociais", 2),           # SEE, SES
    "Infraestrutura & Ambiente",          # SEDUH
    rep("Segurança & Justiça", 2),        # SDS, SEAP
    "Desenvolvimento Econômico & Inovação",# SDAPP
    "Serviços Sociais",                   # SEES
    "Desenvolvimento Econômico & Inovação",# SECTI
    "Serviços Sociais",                   # SECULT
    "Infraestrutura & Ambiente",          # SEMAS
    "Infraestrutura & Ambiente",           # SETUR
    "Serviços Sociais"                   # SAS
  ),
  icon = c(
    "fa-solid fa-road",  # SEDUH
    "fa-solid fa-road",  # SEDUH
    "fa-solid fa-rocket",# ADPE
    # Infraestrutura & Ambiente
    "fa-solid fa-road",  # SEMOBI
    "fa-solid fa-road",  # SRHS
    
    # Serviços Sociais
    "fa-solid fa-users", # SEE
    "fa-solid fa-users", # SES
    
    # Infraestrutura & Ambiente
    "fa-solid fa-road",  # SEDUH
    
    # Segurança & Justiça
    "fa-solid fa-shield",# SDS
    "fa-solid fa-shield",# SEAP
    
    # Desenvolvimento Econômico & Inovação
    "fa-solid fa-rocket",# SDAPP
    
    # Serviços Sociais
    "fa-solid fa-users", # SEES
    
    # Desenvolvimento Econômico & Inovação
    "fa-solid fa-rocket",# SECTI
    
    # Serviços Sociais
    "fa-solid fa-users", # SECULT
    
    # Infraestrutura & Ambiente
    "fa-solid fa-road",  # SEMAS
    "fa-solid fa-road",   # SETUR
    "fa-solid fa-users" # SAS
  )
)
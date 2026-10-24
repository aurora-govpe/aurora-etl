library(tidyverse)
library(furrr)
library(plug)

contratos_plug_temp <- plug_download_base(
  #base_name = 'VW_InfoContratuais_View_Exp'
  'VW_InfoContratuais_View_Exp'
  )

medicoes_plug_raw <- plug_download_base(base_name = 'VW_Medicoes_View_Exp')

base_contratos_raw <- plug_download_base(base_name = 'Contratos_VIEW')

#avancos_plug_raw <- plug_download_base(base_name = 'VW_InfoBM_View_Exp')

#avancos_plug_raw %>% glimpse()

contratos_plug_raw <- contratos_plug_temp %>% left_join(
  base_contratos_raw %>% 
    mutate(avanco_fisico_plug = str_replace(AvancoFisico, ",", ".")) %>% 
    mutate(avanco_fisico_plug = as.numeric(avanco_fisico_plug)/100) %>% 
    select(ID, avanco_fisico_plug),
  by = join_by(ID)
)

#contratos_plug_raw %>% glimpse()

medicoes_plug <- medicoes_plug_raw %>% 
  mutate(`Boletim da Medição (Nº do BM)` = as.integer(`Boletim da Medição (Nº do BM)`)) %>% 
  mutate(`Data Inicial` = as_date(dmy_hms(`Data Inicial`))) %>% 
  mutate(`Data Final` = as_date(dmy_hms(`Data Final`))) %>% 
  mutate(`Valor Medido (R$)` = as.numeric(str_replace(`Valor Medido (R$)`, ",", "."))) %>% 
  mutate(`Valor Medido Reajustado (R$)` = as.numeric(str_replace(`Valor Medido Reajustado (R$)`, ",", "."))) %>% 
  mutate(`Valor Medido Total (R$)` = `Valor Medido (R$)` + `Valor Medido Reajustado (R$)`, .after = `Valor Medido Reajustado (R$)`) %>% 
  mutate(`Motivo da Reprovação` = if_else(`Motivo da Reprovação` == "", NA_character_, `Motivo da Reprovação`))

#contratos_plug_raw %>% glimpse()
#medicoes_plug_raw %>% glimpse()

contratos_plug <- contratos_plug_raw %>% 
  select(contrato_plug = `Nº do Contrato`,
         contrato_tipo_plug = `Tipo de Contrato`,
         contrato_classe_plug = `Classificação do Contrato`,
         situacao_plug = `Situação do Contrato`,
         data_ordem_de_servico = `Data de A.S.`, 
         contratada = Contratada, 
         consorcio = Consórcio, 
         extensao_contrato = `Extensão KM`, 
         protocolo_sei = `Protocolo SEI`, 
         contratos_efisco = `Contrato Efisco`, 
         contrato_tipo = `Tipo de Contrato`, 
         valor_inicial = `Valor Inicial (R$)`, 
         valor_aditado = `Valor Aditado (R$)`, 
         valor_planilha_orcamentaria = `Valor da PO (R$)`, 
         codigos_rodovias = Rodovias,
         avanco_fisico_plug,
         codigos_sre = `Código SRE`) %>% 
  mutate(across(where(is.character), ~ if_else(.x == "", NA_character_, .x))) %>% 
  mutate(extensao_contrato = as.numeric(str_replace(extensao_contrato, ",", "."))) %>% 
  mutate(valor_inicial = as.numeric(str_replace(valor_inicial, ",", "."))) %>% 
  mutate(valor_aditado = as.numeric(str_replace(valor_aditado, ",", "."))) %>% 
  mutate(valor_planilha_orcamentaria = as.numeric(str_replace(valor_planilha_orcamentaria, ",", "."))) %>% 
  mutate(data_ordem_de_servico = as_date(dmy_hms(data_ordem_de_servico))) %>% 
  left_join(
    medicoes_plug %>% group_by(contrato_plug = Contrato) %>% 
      filter(`Status da Medição` == "Medição Aprovada") %>% 
      summarise(valor_medido = sum(`Valor Medido (R$)`, na.rm = TRUE),
                valor_reajuste = sum(`Valor Medido Reajustado (R$)`, na.rm = TRUE),
                  ),
    by = "contrato_plug"
  ) %>% 
  relocate(c(valor_medido, valor_reajuste), .after = valor_planilha_orcamentaria) 

#contratos_plug %>% glimpse()

write_rds(contratos_plug, "data/mulungu/contratos_plug.rds")  

write_rds(medicoes_plug, "data/mulungu/medicoes_plug.rds")  





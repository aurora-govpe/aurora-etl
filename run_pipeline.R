# ==============================================================================
# ENTRY POINT PRINCIPAL: aurora-etl
# ==============================================================================
# Executa todos os pipelines da plataforma Aurora, lendo de:
#   Windows: C:/dados/dados brutos/aurora
#   Linux:   /srv/dados_brutos/aurora
# E gravando as bases consolidadas para os apps em:
#   Windows: C:/dados/aurora
#   Linux:   /srv/dados/aurora
# ==============================================================================

# Carrega resolucao central de caminhos
source("R/paths.R")

message(">>> Diretorio Raw (Entrada): ", DIR_RAW_AURORA)
message(">>> Diretorio Out (Saída Apps): ", DIR_OUT_AURORA)

# Executa rotina unificada
source("etl/ETL_AUTO.R")

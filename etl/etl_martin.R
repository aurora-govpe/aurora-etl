cli::cli_alert_success("Atualizando Martin.")

system(" ssh -p 7654 hugo@10.238.112.127 update_martin.sh")

# system(" ssh -p 7654 juliabarreto@10.238.112.127 update_martin.sh")

cli::cli_alert_success("Martin Atualizado!")

# /usr/local/bin/update_martin.sh

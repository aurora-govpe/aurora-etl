# ==============================================================================
# LOG LOCAL DO ETL (somente R base, sem dependencias externas)
# ==============================================================================
# Grava em arquivo (logs/etl_AAAA-MM-DD.log) e espelha no console.
# Pasta configuravel via variavel de ambiente AURORA_LOG_DIR.
# A pasta logs/ e os arquivos *.log ja estao no .gitignore.
#
# Uso:
#   source("R/logger.R")
#   log_info("mensagem")
#   log_warn("mensagem")
#   log_error("mensagem")
#   log_step("nome da etapa", { ...codigo... })   # mede tempo e captura erros
# ==============================================================================

.log_env <- new.env(parent = emptyenv())
.log_env$file <- NULL
.log_env$con  <- NULL

#' Inicializa o log do dia. Chamado automaticamente no primeiro registro.
log_init <- function(dir = Sys.getenv("AURORA_LOG_DIR", "logs")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .log_env$file <- file.path(dir, paste0("etl_", format(Sys.Date(), "%Y-%m-%d"), ".log"))
  invisible(.log_env$file)
}

#' Escreve uma linha no log: [data hora] [NIVEL] mensagem
#' Com a captura ativa (log_capture_start), o sink ja grava no arquivo tudo
#' o que for impresso no console; sem ela, grava direto no arquivo.
log_msg <- function(level, ...) {
  if (is.null(.log_env$file)) log_init()
  txt <- paste0(..., collapse = "")
  linha <- sprintf("[%s] [%-5s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, txt)
  cat(linha, "\n", sep = "")
  if (is.null(.log_env$con)) {
    tryCatch(
      cat(linha, "\n", sep = "", file = .log_env$file, append = TRUE),
      error = function(e) message("Falha ao gravar no log: ", conditionMessage(e))
    )
  }
  invisible(linha)
}

#' Passa a gravar no arquivo de log TODA a saida do console (cat, print, etc.).
#' As linhas de cat/print entram sem data/hora; as de log_* entram com.
log_capture_start <- function() {
  if (!is.null(.log_env$con)) return(invisible(NULL))
  if (is.null(.log_env$file)) log_init()
  .log_env$con <- file(.log_env$file, open = "a", encoding = "UTF-8")
  sink(.log_env$con, split = TRUE)
  invisible(NULL)
}

#' Encerra a captura iniciada por log_capture_start.
log_capture_stop <- function() {
  if (is.null(.log_env$con)) return(invisible(NULL))
  sink()
  close(.log_env$con)
  .log_env$con <- NULL
  invisible(NULL)
}

log_info  <- function(...) log_msg("INFO",  ...)
log_warn  <- function(...) log_msg("WARN",  ...)
log_error <- function(...) log_msg("ERROR", ...)

#' Executa uma etapa registrando inicio, fim, duracao, warnings e erros.
#' Em caso de erro, registra e (por padrao) relanca o erro.
#' Com stop_on_error = FALSE, devolve FALSE e deixa o pipeline continuar.
log_step <- function(nome, expr, stop_on_error = TRUE) {
  log_info("INICIO  ", nome)
  t0 <- Sys.time()
  ok <- TRUE
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        log_warn(nome, " | ", conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        log_msg("MSG", nome, " | ", trimws(conditionMessage(m), which = "right"))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      ok <<- FALSE
      log_error(nome, " | ", conditionMessage(e))
      if (stop_on_error) stop(e)
    }
  )
  dur <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (ok) log_info("FIM     ", nome, " (", dur, "s)") else log_error("FALHOU  ", nome, " (", dur, "s)")
  invisible(ok)
}

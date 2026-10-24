## -----------------------------------------------------------------------------
## source para o etl iniciativas 
## -----------------------------------------------------------------------------







## -----------------------------------------------------------------------------
## Funções anteriores já existentes 
## -----------------------------------------------------------------------------
make_card_iniciativa <- function(
    ordem, secretaria, fonte, eixo_estrategico, iniciativa_estrategica, produto, local, status, data_de_inicio, 
    data_de_termino, percent_execucao, valor_total, observacoes, cor
) {
  # garanta que 'status' é string (evita armadilhas do switch com factor)
  status <- as.character(status)
  
  titulo <- htmltools::tags$div(
    local,
    style = "display: flex; font-size: 20px; font-weight: 700; margin-bottom: 10px;"
  )
  
  font_size <- 13
  
  valor <- if (!is.na(status)) {
    switch(
      status,
      "Em formulação"            = vialactea::tag_svg("Em formulação",            color="#8E44AD", font="Jost, sans-serif", fontsize=font_size),
      "Em elaboração de projeto" = vialactea::tag_svg("Em elaboração de projeto", color="#D35400", font="Jost, sans-serif", fontsize=font_size),
      "A licitar"                = vialactea::tag_svg("A licitar",                color="#2C3E50", font="Jost, sans-serif", fontsize=font_size),
      "Em licitação"             = vialactea::tag_svg("Em licitação",             color="#16A085", font="Jost, sans-serif", fontsize=font_size),
      "A iniciar"                = vialactea::tag_svg("A iniciar",                color="#F39C12", font="Jost, sans-serif", fontsize=font_size),
      "Em execução"              = vialactea::tag_svg("Em execução",              color="#27AE60", font="Jost, sans-serif", fontsize=font_size),
      "Concluída"                = vialactea::tag_svg("Concluída",                color="#2980B9", font="Jost, sans-serif", fontsize=font_size),
      "Não informado"            = vialactea::tag_svg("Não informado",            color="#7F8C8D", font="Jost, sans-serif", fontsize=font_size),
      "Atrasado"                 = vialactea::tag_svg("Atrasado",                 color="#E74C3C", font="Jost, sans-serif", fontsize=font_size),
      NULL
    )
  }
  
  body_list <- tagList(
    htmltools::tags$div(
      style = 'font-size: .75rem; display: flex; flex-direction: row; column-gap: 10px; padding-bottom:10px',
      make_span(label = "Produto", value = produto, class = "card-produto-value", label_color = cor),
      make_span(label = "Iniciativa", value = iniciativa_estrategica, class = "card-iniciativa-value", label_color = cor)
    ),
    htmltools::tags$div(
      style = 'font-size: .75rem; display: flex; flex-direction: row; column-gap: 10px; align-items: stretch; padding-bottom:10px',
      make_span(label = "Eixo", value = eixo_estrategico, label_color = cor),
      make_span(label = "Início", value = data_de_inicio, label_color = cor),
      make_span(label = "Conclusão", value = data_de_termino, label_color = cor)
    ),
    htmltools::tags$p(
      style = "padding-bottom:10px",
      htmltools::HTML(badge_svg("Secretaria", secretaria, "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      htmltools::HTML(badge_svg("Preenchimento", fonte, "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      htmltools::HTML(badge_svg("Execução", if_else(is.na(percent_execucao), "S/I", paste0(round(100*percent_execucao), "%")), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      htmltools::HTML(badge_svg("Valor", if_else(is.na(valor_total), "S/I", vialactea::comprss(valor_total)), "white", font = "Jost,sans-serif", fontsize = 12, height = 12)),
      valor
    )
  )
  
  if (observacoes != "S/I" & !is.na(observacoes)) {
    body_list <- tagList(
      body_list,
      htmltools::tags$div(
        style = 'font-size: .75rem; display: flex; flex-direction: row; column-gap: 10px; align-items: stretch; padding-bottom:10px',
        make_span(label = "Observação", value = observacoes, class = "card-observacoes-value", label_color = cor)
      )
    )
  }
  
  if (local != "S/I" & !is.na(local)) 
    body_list <- tagList(titulo, body_list)
  
  # ----- RODAPÉ SEM LOCALE DO SISTEMA -----
  meses_pt <- c("janeiro","fevereiro","março","abril","maio","junho",
                "julho","agosto","setembro","outubro","novembro","dezembro")
  hoje <- lubridate::today(tzone = Sys.timezone())
  rodape <- sprintf(
    "Atualizado em %d de %s de %d às %s.",
    lubridate::day(hoje),
    meses_pt[lubridate::month(hoje)],
    lubridate::year(hoje),
    format(lubridate::now(tzone = Sys.timezone()), "%H:%M:%S")
  )
  # ----------------------------------------
  
  bslib::value_box(
    class = glue::glue("iniciativa-cards card-{ordem}"),
    style = "margin:5px;",
    title = NULL,
    body_list,
    htmltools::tags$p(
      style = "font-weight: 300; font-size: 90%; color: black;",
      htmltools::HTML(rodape)
    ),
    full_screen = FALSE,
    theme = bslib::value_box_theme(bg = paste0(cor, "20"))
  ) |>
    as.character() |>
    htmltools::HTML()
}

make_clean_label <- function(nome, status, valor, inicio, termino) {
  
  # Tratamento de NAs para não quebrar a string
  nome_str    <- if (is.na(nome) || nome == "") "Informação da Iniciativa" else nome
  status_str  <- if (is.na(status) || status == "") "Não informado" else status
  inicio_str  <- if (is.na(inicio) || inicio == "") "S/I" else inicio
  termino_str <- if (is.na(termino) || termino == "") "S/I" else termino
  
  # Formatação do Valor (Se for NA, coloca S/I)
  if (is.na(valor)) {
    val_fmt <- "S/I"
  } else {
    # Formata como moeda brasileira (Ex: 1500.5 -> 1.500,50)
    val_fmt <- paste0("R$ ", format(as.numeric(valor), big.mark = ".", decimal.mark = ",", nsmall = 0, scientific = FALSE))
  }
  
  # Monta o HTML usando sprintf
  html_label <- sprintf(
    "
    <div style='color:#333; background:#fff; border-radius:6px; min-width:280px; font-family:Jost, sans-serif; box-shadow:0 4px 12px rgba(0,0,0,.15);'>
        <div style='padding:8px; background:#1a3c6e; color:#fff; font-weight:700; font-size:12px; border-radius:4px 4px 0 0;'>%s</div>
        <div style='padding:10px; font-size:12px; display:flex; flex-direction:column; gap:8px;'>
            <div style='display:flex; justify-content:space-between;'>
                <span><b>Status:</b><br>%s</span>
                <span style='text-align:right;'><b>Valor:</b><br><span style='color:#10b981; font-weight:700;'>%s</span></span>
            </div>
            <div style='display:flex; justify-content:space-between; border-top:1px solid #eee; padding-top:8px;'>
                <span><b>Início:</b><br>%s</span>
                <span style='text-align:right;'><b>Término:</b><br>%s</span>
            </div>
        </div>
    </div>
    ",
    nome_str, status_str, val_fmt, inicio_str, termino_str
  )
  
  # Remove quebras de linha extras para deixar o JSON menorzinho
  return(stringr::str_squish(html_label))
}

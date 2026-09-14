library(purrr)
library(tibble)
library(ggplot2)
library(dplyr)

#' Ciclo PPDAC (Wild & Pfannkuch 1999)
#'
#' @param destaque vetor com as etapas a destacar; NULL destaca todas
#' @param perguntas mostrar a pergunta-guia dentro de cada nó
#' @param base tamanho base do texto
ppdac_plot <- function(destaque = NULL, perguntas = TRUE, base = 4.6) {

  accent      <- "#1c6e8c"
  accent_dark <- "#144f64"
  neutro      <- "#e6e0d3"
  neutro_bord <- "#d3ccbc"
  neutro_txt  <- "#8a8578"
  papel       <- "#faf7f0"

  etapas <- tibble::tibble(
    id       = c("Problema", "Plano", "Dados", "Análise", "Conclusão"),
    pergunta = c("o que eu\nquero saber?", "que dados, e\ncomo coletar?",
                 "o que eu de\nfato tenho?", "que modelo\ncorresponde?",
                 "o que posso\nafirmar?"),
    ang      = seq(90, 90 - 4 * 72, by = -72) * pi / 180
  ) |>
    mutate(
      x  = 1.32 * cos(ang), y = sin(ang),
      on = if (is.null(destaque)) TRUE else id %in% destaque,
      cor_fill = ifelse(on, accent, neutro),
      cor_bord = ifelse(on, accent_dark, neutro_bord),
      cor_txt  = ifelse(on, "white", neutro_txt)
    )

  r_no <- 0.37
  t <- seq(0, 2 * pi, length.out = 121)
  circulos <- purrr::pmap_dfr(
    list(etapas$id, etapas$x, etapas$y, etapas$cor_fill, etapas$cor_bord),
    function(id, x, y, f, b) tibble::tibble(
      id = id, cx = x + r_no * cos(t), cy = y + r_no * sin(t),
      cor_fill = f, cor_bord = b)
  )

  n <- nrow(etapas)
  setas <- tibble::tibble(
    x0 = etapas$x, y0 = etapas$y,
    x1 = etapas$x[c(2:n, 1)], y1 = etapas$y[c(2:n, 1)]
  ) |>
    mutate(dx = x1 - x0, dy = y1 - y0, L = sqrt(dx^2 + dy^2),
           xa = x0 + dx / L * (r_no + .05), ya = y0 + dy / L * (r_no + .05),
           xb = x1 - dx / L * (r_no + .12), yb = y1 - dy / L * (r_no + .12))

  dy_id <- if (perguntas) 0.14 else 0

  ggplot() +
    geom_curve(data = setas, aes(x = xa, y = ya, xend = xb, yend = yb),
               curvature = -0.22, linewidth = 0.9, colour = accent_dark,
               alpha = .55, lineend = "round",
               arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
    geom_polygon(data = circulos, aes(cx, cy, group = id),
                 fill = circulos$cor_fill, colour = circulos$cor_bord,
                 linewidth = 0.6) +
    geom_text(data = etapas, aes(x, y + dy_id, label = id),
              colour = etapas$cor_txt, fontface = "bold", size = base * 0.80) +
    {if (perguntas)
      geom_text(data = etapas, aes(x, y - 0.13, label = pergunta),
                colour = etapas$cor_txt, size = base * 0.52, lineheight = 0.95)
     else NULL} +
    coord_equal(clip = "off") +
    theme_void() +
    theme(plot.background  = element_rect(fill = papel, colour = NA),
          panel.background = element_rect(fill = papel, colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}

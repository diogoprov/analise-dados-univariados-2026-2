library(ggplot2)
library(dplyr)
library(patchwork)

#' Mapa dos 52 segmentos de estrada do conjunto RoadKills
#'
#' As coordenadas do arquivo estão em metros, num sistema projetado que a
#' fonte não documenta. Por isso o mapa é desenhado no sistema do próprio
#' conjunto, com os eixos em quilômetros e escala 1:1 — a geometria (um
#' trecho reto de 25,5 km dividido em segmentos de 500 m) é fiel, e nenhuma
#' afirmação é feita sobre onde isso fica no globo.
mapa_roadkills <- function(RK, base = 14) {

  azul   <- "#1c6e8c"; azul_esc <- "#144f64"
  terra  <- "#a15e3f"; papel    <- "#faf7f0"
  cinza  <- "#4a5568"

  d <- RK |>
    mutate(x_km = (X - min(X)) / 1000,
           y_km = (Y - min(Y)) / 1000,
           parque_km = D.PARK / 1000)

  fundo <- theme_minimal(base_size = base) +
    theme(plot.background  = element_rect(fill = papel, colour = NA),
          panel.background = element_rect(fill = papel, colour = NA),
          plot.title = element_text(size = base, face = "bold", colour = azul_esc),
          legend.position = "right")

  p_mapa <- ggplot(d, aes(x_km, y_km)) +
    geom_path(colour = "#cfc7b6", linewidth = 1.6) +
    geom_point(aes(colour = TOT.N, size = TOT.N)) +
    annotate("text", x = mean(range(d$x_km)), y = max(d$y_km) + 1.9,
             label = "parque", colour = terra, fontface = "bold", size = base * .30) +
    annotate("segment", x = mean(range(d$x_km)), xend = mean(range(d$x_km)),
             y = max(d$y_km) + 1.3, yend = max(d$y_km) + .4,
             colour = terra, linewidth = .9,
             arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.4, 5.5)) +
    scale_x_continuous(breaks = NULL) +
    coord_equal(clip = "off") +
    guides(colour = "none", size = "none") +
    labs(title = "o trecho", x = NULL, y = "km ao longo da estrada") +
    fundo

  p_grad <- ggplot(d, aes(parque_km, TOT.N)) +
    geom_point(aes(colour = TOT.N, size = TOT.N), show.legend = FALSE) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = terra, linewidth = 1) +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.4, 5.5)) +
    labs(title = "mortalidade cai com a distância ao parque",
         x = "distância ao parque (km)", y = "anfíbios mortos no segmento") +
    fundo

  p_mapa + p_grad + plot_layout(widths = c(1, 1.9))
}

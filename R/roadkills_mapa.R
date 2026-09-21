library(ggplot2)
library(dplyr)
library(patchwork)

#' Mapa dos 52 segmentos de estrada do conjunto RoadKills
#'
#' Os dados vêm de um estudo de dois anos na IP2, trecho Portalegre–Monforte,
#' sul de Portugal (26 km, 54 percursos entre 1995 e 1997), publicado por
#' Ascensão & Mira (2005), ICOET 2005, p. 641
#' (https://escholarship.org/uc/item/8r07z6nf) e reanalisado em Zuur et al.
#' (2009), cap. 16 (doi:10.1007/978-0-387-87458-6_16). O capítulo diz 27 km e
#' < 10 000 veículos/dia; a fonte primária diz 26 km e ~5 000 veículos/dia, e
#' 52 segmentos de 500 m dão exatamente 26 km.
#' A estrada foi dividida em segmentos de
#' 500 m e cada animal encontrado morto foi atribuído ao ponto médio do seu
#' segmento; `X` e `Y` são essas coordenadas. O capítulo as chama de
#' coordenadas UTM, mas os valores não correspondem ao fuso UTM de Portugal,
#' então o mapa é desenhado no sistema do próprio conjunto, em quilômetros.
#'
#' O eixo leste–oeste é exagerado de propósito: o trecho tem ~2 km de largura
#' para ~25 km de comprimento e, em escala 1:1, vira uma linha reta. Zuur et
#' al. fazem a mesma distorção na Fig. 16.6 pela mesma razão.
mapa_roadkills <- function(RK, base = 14) {

  azul   <- "#1c6e8c"; azul_esc <- "#144f64"
  terra  <- "#a15e3f"; papel    <- "#faf7f0"

  d <- RK |>
    mutate(x_km = (X - min(X)) / 1000,
           y_km = (Y - min(Y)) / 1000,
           parque_km = D.PARK / 1000)

  fundo <- theme_minimal(base_size = base) +
    theme(plot.background  = element_rect(fill = papel, colour = NA),
          panel.background = element_rect(fill = papel, colour = NA),
          plot.title = element_text(size = base, face = "bold", colour = azul_esc),
          legend.position = "none")

  x_meio <- mean(range(d$x_km))
  y_max  <- max(d$y_km); y_min <- min(d$y_km)

  p_mapa <- ggplot(d, aes(x_km, y_km)) +
    geom_path(colour = "#cfc7b6", linewidth = 1.6) +
    geom_point(aes(colour = TOT.N, size = TOT.N)) +
    annotate("text", x = x_meio, y = y_max + 3.2, size = base * .30,
             label = "Portalegre", colour = terra, fontface = "bold") +
    annotate("text", x = x_meio, y = y_max + 2.2, size = base * .25,
             label = "Serra de São Mamede", colour = terra) +
    annotate("segment", x = x_meio, xend = x_meio,
             y = y_max + 1.6, yend = y_max + .5,
             colour = terra, linewidth = .9,
             arrow = arrow(length = unit(.18, "cm"), type = "closed")) +
    annotate("text", x = x_meio, y = y_min - 1.9, size = base * .30,
             label = "Monforte", colour = terra, fontface = "bold") +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.4, 5.5)) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(expand = expansion(mult = .13)) +
    coord_cartesian(clip = "off") +
    labs(title = "o trecho da IP2", x = NULL, y = "km, sentido norte-sul") +
    fundo

  p_grad <- ggplot(d, aes(parque_km, TOT.N)) +
    geom_point(aes(colour = TOT.N, size = TOT.N)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = terra, linewidth = 1) +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.4, 5.5)) +
    labs(title = "mortalidade cai com a distância ao parque",
         x = "distância ao parque, ao longo da estrada (km)",
         y = "anfíbios mortos no segmento") +
    fundo

  p_mapa + p_grad + plot_layout(widths = c(1, 1.9))
}

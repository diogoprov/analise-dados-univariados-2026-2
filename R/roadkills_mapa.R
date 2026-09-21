library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)

#' Mapa dos 52 segmentos de estrada do conjunto RoadKills
#'
#' Os dados vêm de um estudo de dois anos na IP2, trecho Portalegre–Monforte,
#' sul de Portugal (26 km, 54 percursos entre 1995 e 1997), publicado por
#' Ascensão & Mira (2005), ICOET 2005, p. 641
#' (https://escholarship.org/uc/item/8r07z6nf) e reanalisado em Zuur et al.
#' (2009), cap. 16 (doi:10.1007/978-0-387-87458-6_16). A estrada foi dividida
#' em segmentos de 500 m e cada animal encontrado morto foi atribuído ao ponto
#' médio do seu segmento; `X` e `Y` são essas coordenadas.
#'
#' O capítulo chama `X`/`Y` de "UTM coordinates" sem dar o sistema, e os
#' valores não correspondem ao fuso UTM de Portugal. São coordenadas do
#' **Datum Lisboa / Hayford-Gauss Militar (EPSG:20790)**. Três verificações
#' independentes, feitas com a transformação para WGS84:
#'   - o segmento 1 cai a 1,8 km do centro de Portalegre e a 1,3 km do limite
#'     sudoeste do Parque Natural da Serra de São Mamede;
#'   - o segmento 52 cai a 0,6 km de Monforte;
#'   - a distância em linha reta de cada segmento ao limite do parque
#'     correlaciona 0,988 com a coluna `D.PARK` do próprio arquivo, que é a
#'     mesma distância medida ao longo da estrada.
#'
#' Camadas de contexto em dados/roadkills_contexto.gpkg:
#'   parque  — Parque Natural da Serra de São Mamede, (c) colaboradores do
#'             OpenStreetMap, ODbL (relação 544420; área 559,5 km2)
#'   paises  — Portugal e Espanha, Natural Earth 1:50m, domínio público
#'   cidades — Portalegre e Monforte, Nominatim/OpenStreetMap, ODbL

caminho_contexto <- function() {
  for (p in c("dados/roadkills_contexto.gpkg", "../dados/roadkills_contexto.gpkg"))
    if (file.exists(p)) return(p)
  stop("roadkills_contexto.gpkg não encontrado", call. = FALSE)
}

mapa_roadkills <- function(RK, base = 14, gpkg = caminho_contexto()) {

  azul  <- "#1c6e8c"; azul_esc <- "#144f64"
  terra <- "#a15e3f"; papel    <- "#faf7f0"
  verde <- "#527238"; verde_cl <- "#d8e3d0"

  pts <- RK |>
    mutate(parque_km = D.PARK / 1000) |>
    st_as_sf(coords = c("X", "Y"), crs = 20790) |>
    st_transform(4326)

  parque  <- st_read(gpkg, "parque",  quiet = TRUE)
  paises  <- st_read(gpkg, "paises",  quiet = TRUE)
  cidades <- st_read(gpkg, "cidades", quiet = TRUE)

  cx <- st_coordinates(cidades)
  # o enquadramento tem de caber a estrada E o parque, senão o parque some
  bb <- st_bbox(st_buffer(st_union(st_as_sfc(st_bbox(pts)),
                                   st_as_sfc(st_bbox(parque))), 0.04))
  bb["ymin"] <- bb["ymin"] - 0.025

  fundo <- theme_minimal(base_size = base) +
    theme(plot.background  = element_rect(fill = papel, colour = NA),
          panel.background = element_rect(fill = papel, colour = NA),
          plot.title = element_text(size = base, face = "bold", colour = azul_esc),
          legend.position = "none")

  # barra de escala: 5 km convertidos em graus de longitude nesta latitude
  lat0  <- mean(c(bb["ymin"], bb["ymax"]))
  dx5km <- 5 / (111.320 * cos(lat0 * pi / 180))
  x0    <- bb["xmin"] + 0.014; y0 <- bb["ymax"] - 0.030

  p_mapa <- ggplot() +
    geom_sf(data = parque, fill = verde_cl, colour = verde, linewidth = .5) +
    geom_sf(data = pts, aes(colour = TOT.N, size = TOT.N)) +
    geom_sf(data = cidades, shape = 22, fill = "white", colour = terra,
            size = 2.4, stroke = 1) +
    annotate("text", x = cx[1, 1] + .012, y = cx[1, 2], hjust = 0,
             label = "Portalegre", colour = terra, fontface = "bold",
             size = base * .26) +
    annotate("text", x = cx[2, 1] + .014, y = cx[2, 2] + .002, hjust = 0,
             label = "Monforte", colour = terra, fontface = "bold",
             size = base * .26) +
    annotate("text", x = st_bbox(parque)["xmax"] - .02,
             y = st_bbox(parque)["ymax"] - .055, hjust = 1,
             label = "Parque Natural da\nSerra de São Mamede",
             colour = verde, size = base * .20, lineheight = .95) +
    annotate("segment", x = x0, xend = x0 + dx5km, y = y0, yend = y0,
             colour = azul_esc, linewidth = 1) +
    annotate("text", x = x0 + dx5km / 2, y = y0 - .018, label = "5 km",
             colour = azul_esc, size = base * .20) +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.2, 5.2)) +
    scale_x_continuous(breaks = seq(-7.6, -7.0, by = .2)) +
    scale_y_continuous(breaks = seq(39.0, 39.6, by = .2)) +
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
             expand = FALSE) +
    labs(title = "onde isso fica", x = NULL, y = NULL) +
    fundo +
    theme(axis.text = element_text(size = base * .62, colour = "#5f5a4e"),
          panel.grid = element_line(colour = "#e8e1d2"))

  p_inset <- ggplot() +
    geom_sf(data = paises, fill = "#ece5d6", colour = "#b9b1a0", linewidth = .3) +
    annotate("rect", xmin = bb["xmin"], xmax = bb["xmax"],
             ymin = bb["ymin"], ymax = bb["ymax"],
             fill = NA, colour = terra, linewidth = .7) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = papel, colour = "#b9b1a0"))

  p_grad <- ggplot(pts, aes(parque_km, TOT.N)) +
    geom_point(aes(colour = TOT.N, size = TOT.N)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = terra, linewidth = 1) +
    scale_colour_gradient(low = "#7f9fae", high = azul_esc) +
    scale_size_continuous(range = c(1.2, 5.2)) +
    labs(title = "mortalidade cai com a distância ao parque",
         x = "distância ao parque, ao longo da estrada (km)",
         y = "anfíbios mortos no segmento") +
    fundo

  (p_mapa + inset_element(p_inset, left = .60, bottom = .02,
                          right = 1.00, top = .26, align_to = "panel")) +
    p_grad + plot_layout(widths = c(.85, 1.35))
}

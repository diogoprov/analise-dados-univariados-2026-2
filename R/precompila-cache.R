# Pré-compilação dos modelos brms usados nos slides
# ------------------------------------------------------------------
# Rode UMA VEZ, a partir da raiz do repositório:
#
#     Rscript R/precompila-cache.R
#
# Ele comeca conferindo que as chamadas brm() daqui sao identicas as dos
# slides, e para se houver divergencia.
#
# Ele ajusta os quatro modelos brms dos Encontros 2, 8 e 9 e grava os
# .rds em cache/. Depois disso, `quarto render` carrega os ajustes do
# cache em vez de recompilar o Stan.
#
# As chamadas brm() abaixo são cópia literal das que aparecem nos slides.
# Se você mudar uma fórmula no slide, mude aqui também — ou apague o .rds
# correspondente. O argumento file_refit = "on_change" protege contra
# esquecimento: o brms reajusta se o modelo mudou.
# ------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositório (onde está o _quarto.yml).")
}

source("R/confere_cache.R")
message("Conferindo se as chamadas batem com as dos slides...")
confere_cache()

suppressPackageStartupMessages({
  library(tidyverse)
  library(palmerpenguins)
  library(brms)
})

dir.create("cache", showWarnings = FALSE)
options(mc.cores = min(4, parallel::detectCores()))

cronometra <- function(rotulo, expr) {
  t0 <- Sys.time()
  message("\n== ", rotulo, " ...")
  out <- force(expr)
  message("   pronto em ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
  out
}

# ---- Encontro 2 ---------------------------------------------------
pinguins <- penguins |> drop_na(bill_length_mm, body_mass_g, species, sex)

cronometra("E2 — massa ~ comprimento do bico (gaussiano)", {
  brm(body_mass_g ~ bill_length_mm, data = pinguins,
      chains = 4, iter = 2000, seed = 2026,
      file = "cache/e02_bico", file_refit = "on_change")
})

# ---- Encontro 8 ---------------------------------------------------
rikz <- read.table("dados/RIKZ.txt", header = TRUE, row.names = 1) |>
  mutate(Beach = factor(Beach))

cronometra("E8 — riqueza ~ NAP + (1|praia), binomial negativa", {
  brm(Richness ~ NAP + (1 | Beach),
      family = negbinomial(), data = rikz,
      chains = 4, iter = 2000, seed = 2026,
      file = "cache/e08_rikz_nb", file_refit = "on_change")
})

# ---- Encontro 9 ---------------------------------------------------
RK <- read.delim("dados/RoadKills.txt")

cronometra("E9 — atropelamentos ~ distância ao parque", {
  brm(TOT.N ~ D.PARK, family = negbinomial(), data = RK,
      file = "cache/e09_a", file_refit = "on_change")
})

cronometra("E9 — + distância a corpos d'água", {
  brm(TOT.N ~ D.PARK + L.WAT.C, family = negbinomial(), data = RK,
      file = "cache/e09_b", file_refit = "on_change")
})

# ---- Conferência --------------------------------------------------
esperados <- c("e02_bico", "e08_rikz_nb", "e09_a", "e09_b")
existe <- file.exists(file.path("cache", paste0(esperados, ".rds")))

message("\n---------------------------------------------")
for (i in seq_along(esperados)) {
  message(sprintf("  %-14s %s", esperados[i], if (existe[i]) "OK" else "FALTANDO"))
}
if (all(existe)) {
  message("\nCache completo. Pode rodar `quarto render`.")
} else {
  message("\nAlgum modelo não gravou. Veja os erros acima.")
}

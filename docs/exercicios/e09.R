# Encontro 9 — Prática de referência
# ---------------------------------------------------------------------
# Conjunto de modelos definido a priori, tabela de AICc e diagnóstico do
# melhor apoiado.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS, warn.conflicts = FALSE)   # MASS::select mascara dplyr::select
  library(DHARMa)
})
theme_set(theme_minimal(base_size = 14))

RK <- read.delim(dados("RoadKills.txt"))
RK$D.PARK_km <- RK$D.PARK / 1000

# 1. As hipóteses, em palavras -----------------------------------------
# H0  nulo        — a mortalidade não varia com nada que medimos
# H1  parque      — cai com a distância à área-fonte de anfíbios
# H2  habitat     — depende da composição do habitat ao redor da estrada
# H3  água        — depende da presença de corpos d'água próximos
# H4  parque+água — as duas coisas, sem interação
#
# TAKE-HOME: escrever em português ANTES de escrever em R é o que separa
# um conjunto de modelos de um garimpo. Se a hipótese não cabe numa frase
# biológica, ela não é hipótese — é combinação de colunas.

# 2. Traduzir cada uma em um modelo ------------------------------------
modelos <- list(
  nulo        = glm.nb(TOT.N ~ 1, data = RK),
  parque      = glm.nb(TOT.N ~ D.PARK_km, data = RK),
  habitat     = glm.nb(TOT.N ~ OPEN.L + MONT.S, data = RK),
  agua        = glm.nb(TOT.N ~ L.WAT.C + D.WAT.RES, data = RK),
  parque_agua = glm.nb(TOT.N ~ D.PARK_km + L.WAT.C, data = RK)
)

# TAKE-HOME: o nulo entra sempre. Sem ele você não sabe se o conjunto
# inteiro explica alguma coisa — só qual dos seus modelos é menos ruim.

# 3. A tabela, à mão ---------------------------------------------------
aicc <- function(m) {
  K <- length(coef(m)) + 1          # +1 pelo parâmetro de dispersão da NB
  n <- nobs(m)
  AIC(m) + 2 * K * (K + 1) / (n - K - 1)
}

tab <- tibble(
  modelo = names(modelos),
  K      = purrr::map_dbl(modelos, \(m) length(coef(m)) + 1),
  AICc   = purrr::map_dbl(modelos, aicc)
) |>
  arrange(AICc) |>
  mutate(delta    = AICc - min(AICc),
         vero_rel = exp(-0.5 * delta),
         peso     = vero_rel / sum(vero_rel))

tab |> mutate(across(c(AICc, delta), \(x) round(x, 1)),
              peso = round(peso, 3)) |>
  dplyr::select(modelo, K, AICc, delta, peso)

# Razão de evidência entre o primeiro e o segundo:
round(tab$peso[1] / tab$peso[2], 1)

# TAKE-HOME: K inclui o parâmetro de dispersão — esquecer isso desloca a
# tabela inteira. E os limiares de delta de Burnham & Anderson valem
# "particularly useful for NESTED models" e supõem independência: em
# delineamento agrupado, como o do Encontro 8, a régua não se aplica.

# 4. O parágrafo de resultados -----------------------------------------
# Modelo, para o aluno adaptar:
#
#   "Comparamos cinco modelos definidos a priori por AICc (Tabela 1). O
#    modelo XXX teve o maior peso de Akaike (w = 0,XX), YY vezes mais
#    plausível que o segundo colocado. A distância ao parque foi o
#    preditor mais bem apoiado, com coeficiente de -0,116 por km
#    (IC 95 % ...), isto é, uma queda de XX % na mortalidade esperada a
#    cada quilômetro de afastamento."
#
# TAKE-HOME: reporte a tabela COMPLETA (K, AICc, delta, peso para todos os
# modelos), não só o vencedor — Gerstner et al. (2017) e Burnham &
# Anderson. Publicar só o melhor esconde justamente a incerteza de
# seleção que o método existe para quantificar.

# 5. Diagnosticar o mais bem apoiado -----------------------------------
melhor <- modelos[[tab$modelo[1]]]
set.seed(2026)
res <- simulateResiduals(melhor, n = 1000)
plot(res)
testDispersion(res, plot = FALSE)

# TAKE-HOME: AICc compara modelos ENTRE SI; não diz se algum deles serve.
# O melhor de cinco modelos ruins continua ruim. Seleção não substitui
# diagnóstico — e esta é a ordem certa: selecionar, depois diagnosticar.

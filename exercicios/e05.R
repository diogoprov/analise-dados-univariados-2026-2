# Encontro 5 — Prática de referência
# ---------------------------------------------------------------------
# GLM binomial e Poisson: ajuste, tradução de escala, curva e diagnóstico
# rápido. O item 5 exige o pacote dagitty.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_minimal(base_size = 14))

lagartos <- read_tsv(dados("camila_cauda_lagartos.txt"), show_col_types = FALSE) |>
  drop_na(SVL, Tail_state, Sex) |> mutate(Sex = factor(Sex))
RK <- read.delim(dados("RoadKills.txt"))
RK$D.PARK_km <- RK$D.PARK / 1000

# 1. Ajustar o GLM -----------------------------------------------------
m_bin  <- glm(Tail_state ~ SVL, family = binomial, data = lagartos)
m_pois <- glm(TOT.N ~ D.PARK_km, family = poisson, data = RK)
summary(m_bin)

# TAKE-HOME: `family` não é um detalhe técnico — é a afirmação sobre COMO
# os dados foram gerados. Trocar de família é trocar de hipótese.

# 2. Traduzir para a escala da resposta --------------------------------
# Binomial: coeficiente em log-odds; exp() dá a razão de chances.
round(exp(coef(m_bin)), 3)

# Poisson: coeficiente em log da contagem esperada.
round(exp(coef(m_pois)), 3)

# Efeito de 10 mm de SVL na probabilidade, avaliado na média:
svl_medio <- mean(lagartos$SVL)
p <- predict(m_bin, newdata = data.frame(SVL = c(svl_medio, svl_medio + 10)),
             type = "response")
round(c(no_medio = p[1], dez_mm_maior = p[2], diferenca = diff(p)), 3)

# TAKE-HOME: em GLM o coeficiente NÃO está na escala da resposta. Uma razão
# de chances de 1,05 pode ser irrelevante ou enorme dependendo de onde na
# curva você está — por isso se reporta o efeito avaliado num valor
# concreto, não só o exp(beta).

# 3. Curva ajustada com banda ------------------------------------------
nv <- tibble(SVL = seq(min(lagartos$SVL), max(lagartos$SVL), length.out = 120))
pr <- predict(m_bin, newdata = nv, type = "link", se.fit = TRUE)
nv <- nv |> mutate(p    = plogis(pr$fit),
                   lo   = plogis(pr$fit - 1.96 * pr$se.fit),
                   hi   = plogis(pr$fit + 1.96 * pr$se.fit))

print(
  ggplot(lagartos, aes(SVL, Tail_state)) +
    geom_point(alpha = .4, colour = "#1c6e8c") +
    geom_ribbon(data = nv, aes(y = p, ymin = lo, ymax = hi), alpha = .2) +
    geom_line(data = nv, aes(y = p), colour = "#c08a3e", linewidth = 1) +
    labs(x = "SVL (mm)", y = "P(cauda autotomizada)")
)

# TAKE-HOME: a banda é construída na escala do LINK e só depois convertida
# pela inversa. Fazer o contrário produz intervalos que passam de 1 ou
# ficam abaixo de 0 — erro comum e visível.

# 4. Deviance residual sobre graus de liberdade ------------------------
c(bin  = round(deviance(m_bin)  / df.residual(m_bin), 2),
  pois = round(deviance(m_pois) / df.residual(m_pois), 2))

# TAKE-HOME: para a Poisson, razão perto de 1 é o esperado; muito acima é
# sobredispersão. Para a binomial com resposta 0/1 (Bernoulli) essa razão
# NÃO diagnostica nada — não há dispersão livre para estimar. É o Encontro
# 6 inteiro.

# 5. O DAG antes das covariáveis ---------------------------------------
if (requireNamespace("dagitty", quietly = TRUE)) {
  g <- dagitty::dagitty('dag {
    Precipitacao -> Area
    Precipitacao -> Riqueza
    Area -> Riqueza
    Area -> Deteccao
    Riqueza -> Deteccao
  }')
  print(dagitty::adjustmentSets(g, exposure = "Area", outcome = "Riqueza"))
} else {
  message('instale dagitty: install.packages("dagitty")')
}

# TAKE-HOME: o DAG diz o que INCLUIR, e a resposta quase nunca é "tudo".
# Controlar por uma variável que está no caminho causal (ou por um
# colisor) introduz viés em vez de removê-lo.

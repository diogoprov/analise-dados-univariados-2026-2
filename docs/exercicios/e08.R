# Encontro 8 — Prática de referência
# ---------------------------------------------------------------------
# GLMM, diagnóstico por grupo, R2 de Nakagawa e a ponte bayesiana.
#
# ATENÇÃO ao item 4: a chamada brm() é IDÊNTICA à do slide e aponta para o
# mesmo cache (cache/e08_rikz_pois). Se você mudar a fórmula aqui, mude
# também no slide e em R/precompila-cache.R — o brms, no padrão
# file_refit = "never", carrega o .rds sem conferir a fórmula.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(DHARMa)
  library(performance)
})
theme_set(theme_minimal(base_size = 14))

rikz <- read.table(dados("RIKZ.txt"), header = TRUE, row.names = 1) |>
  mutate(Beach = factor(Beach))

# 1. GLMM com a família adequada ---------------------------------------
m_pois <- glmer(Richness ~ NAP + (1 | Beach), family = poisson, data = rikz)
summary(m_pois)

# TAKE-HOME: riqueza é contagem, então Poisson com ligação log — não
# transformação log da resposta. Transformar a resposta muda a pergunta e
# não resolve a relação média–variância.

# 2. Diagnóstico, inclusive por grupo ----------------------------------
set.seed(2026)
res <- simulateResiduals(m_pois, n = 1000)
plot(res)
testDispersion(res, plot = FALSE)
plotResiduals(res, form = rikz$Beach)

# TAKE-HOME: `plotResiduals(form = grupo)` é o que mais rende em modelo
# misto. Resíduo ainda estruturado DENTRO de um grupo significa que falta
# algo na estrutura aleatória — e isso o gráfico global não mostra.

# 3. R2 marginal e condicional -----------------------------------------
r2_nakagawa(m_pois)

# TAKE-HOME: marginal = só os efeitos fixos; condicional = fixos e
# aleatórios juntos. A diferença entre os dois é exatamente quanto o
# agrupamento explica — número que vale reportar, e quase ninguém reporta.

# 4. O mesmo modelo em brms --------------------------------------------
if (requireNamespace("brms", quietly = TRUE)) {
  library(brms)
  m_bayes <- brm(Richness ~ NAP + (1 | Beach), family = poisson(), data = rikz,
                 chains = 4, iter = 2000, seed = 2026,
                 file = "cache/e08_rikz_pois")
  print(summary(m_bayes))
  print(pp_check(m_bayes, ndraws = 100))
} else {
  message('instale brms: install.packages("brms")')
}

# TAKE-HOME: a fórmula é a MESMA do glmer(). Uma notação serve para lm,
# lmer, glmmTMB e brm — é por isso que o curso usa brms e não rstanarm.
# Com o cache gravado, isto carrega em segundos; sem ele, compila Stan.

# 5. Comparar as estimativas -------------------------------------------
if (exists("m_bayes")) {
  tibble(
    fonte = c("glmer (máxima verossimilhança)", "brms (posterior)"),
    NAP   = c(fixef(m_pois)[["NAP"]], fixef(m_bayes)["NAP", "Estimate"])
  ) |> mutate(NAP = round(NAP, 3)) |> print()
}

# TAKE-HOME: com priors fracamente informativas e dados suficientes, as
# duas estimativas praticamente coincidem — o que é o ponto pedagógico.
# Quando NÃO coincidem, quase sempre é porque um parâmetro está mal
# identificado, e o MCMC mostra isso (R-hat alto, ESS baixo) enquanto a
# máxima verossimilhança devolve uma estimativa pontual de aparência
# inocente. Foi o que aconteceu com a binomial negativa neste mesmo
# conjunto: shape da ordem de 10^18.

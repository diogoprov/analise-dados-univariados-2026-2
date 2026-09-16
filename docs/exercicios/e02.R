# Encontro 2 — Prática de referência
# ---------------------------------------------------------------------
# Os enunciados do slide pedem "nos SEUS dados". Este script faz os mesmos
# cinco passos nos dados do curso, para rodar em sala. O aluno troca o
# conjunto e a fórmula; a sequência é a mesma. O item 6 é extra: não está
# no deck, e existe para sustentar com simulação o que o item 4 afirma.
#
# Rode a partir da raiz do repositório (ou de dentro de exercicios/).
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(palmerpenguins)
})
theme_set(theme_minimal(base_size = 14))

pinguins <- penguins |> drop_na(bill_length_mm, body_mass_g, species, sex)

# 1. Ajuste um lm() ----------------------------------------------------
m <- lm(body_mass_g ~ bill_length_mm + sex, data = pinguins)
summary(m)

# TAKE-HOME: o mesmo lm() serve para o que a tradição chama de teste t,
# ANOVA e regressão. O que muda entre eles é só a matriz de delineamento.

# 2. A matriz de delineamento -----------------------------------------
X <- model.matrix(m)
head(X, 4)
colnames(X)

# (Intercept)    — a massa esperada quando bill_length_mm = 0 e sex = female
# bill_length_mm — coluna contínua, entra com o valor observado
# sexmale        — indicadora 0/1: 1 quando o indivíduo é macho

# TAKE-HOME: cada coluna de X é uma pergunta feita aos dados. Se você não
# consegue dizer em uma frase o que a coluna significa, não consegue
# interpretar o coeficiente dela.

# 3. Coeficientes com unidade -----------------------------------------
round(coef(m), 1)

# bill_length_mm: cada milímetro a mais de bico corresponde a X g a mais
#                 de massa esperada, mantido o sexo.
# sexmale:        machos pesam, em média, Y g a mais que fêmeas de mesmo
#                 comprimento de bico.

# TAKE-HOME: coeficiente sem unidade não é resultado, é número. "Mantido o
# sexo" não é detalhe de redação: é o que o modelo de fato afirma.

# 4. Diagnóstico -------------------------------------------------------
op <- par(mfrow = c(2, 2)); plot(m); par(op)

# TAKE-HOME: o que se examina é o RESÍDUO, não a resposta. A normalidade
# exigida é a do resíduo — mal-entendido mais caro da estatística aplicada.

# 5. Trocar o nível de referência --------------------------------------
pinguins2 <- pinguins |> mutate(sex = relevel(factor(sex), ref = "male"))
m2 <- lm(body_mass_g ~ bill_length_mm + sex, data = pinguins2)

c(coef_original = coef(m)[["sexmale"]], coef_trocado = coef(m2)[["sexfemale"]])
c(logLik_original = as.numeric(logLik(m)), logLik_trocado = as.numeric(logLik(m2)))
all.equal(fitted(m), fitted(m2), check.attributes = FALSE)

# TAKE-HOME: o nível de referência muda o que os coeficientes DIZEM, não o
# ajuste. Verossimilhança e valores preditos são idênticos. Escolher a
# referência é decisão de comunicação, não de estatística.

# 6. A premissa se checa no gráfico, não no teste -----------------------
# Mesma violação, n diferente. A distribuição é sempre a mesma gama, com
# assimetria 2/sqrt(6) = 0,82; só o tamanho da amostra muda.
set.seed(2026)

gera <- function(n) rgamma(n, shape = 6, rate = 1)

prop_rejeita <- function(n, vezes = 400) {
  p <- map_dbl(seq_len(vezes), \(i) shapiro.test(gera(n))$p.value)
  mean(p < 0.05)
}

tibble(n = c(20, 50, 200, 1000, 5000)) |>
  mutate(rejeita = map_dbl(n, prop_rejeita))

# TAKE-HOME: com n = 20 o shapiro.test() diz que está tudo bem na maioria
# das vezes; de n = 200 em diante ele nunca deixa passar. Mesma violação,
# veredito oposto — o que o teste mede é o seu n, não o tamanho do desvio.
# Kozak & Piepho (2018) e Shatz (2024); a versão interativa está em
# teste-de-normalidade.qmd, que roda no navegador.

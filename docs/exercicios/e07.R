# Encontro 7 — Prática de referência
# ---------------------------------------------------------------------
# Efeitos aleatórios: estrutura, erros-padrão, ICC e inclinação aleatória.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
})
theme_set(theme_minimal(base_size = 14))

rikz <- read.table(dados("RIKZ.txt"), header = TRUE, row.names = 1) |>
  mutate(Beach = factor(Beach))

# 1. A estrutura de agrupamento ----------------------------------------
rikz |> count(Beach) |> summarise(praias = n(), amostras = sum(n),
                                  por_praia = paste(range(n), collapse = "–"))

# Cinco amostras dentro de cada uma de nove praias  ->  (1 | Beach)
#
# TAKE-HOME: a fórmula do efeito aleatório sai do DELINEAMENTO, não do
# ajuste. Escreva-a olhando para como os dados foram coletados, antes de
# olhar para qualquer resultado.

# 2. Com e sem o efeito aleatório --------------------------------------
m_lm  <- lm(Richness ~ NAP, data = rikz)
m_mix <- lmer(Richness ~ NAP + (1 | Beach), data = rikz, REML = TRUE)

tibble(
  modelo = c("lm (ignora praia)", "lmer (1 | Beach)"),
  estimativa_NAP = c(coef(m_lm)[["NAP"]], fixef(m_mix)[["NAP"]]),
  erro_padrao    = c(summary(m_lm)$coefficients["NAP", "Std. Error"],
                     summary(m_mix)$coefficients["NAP", "Std. Error"])
) |> mutate(across(where(is.numeric), \(x) round(x, 3)))

# TAKE-HOME: o erro-padrão do lm é otimista porque ele conta 45
# observações independentes quando há 9 praias. Pseudorreplicação
# (Hurlbert 1984) não é erro de cálculo — é contar informação que não
# existe.

# 3. Correlação intraclasse --------------------------------------------
vc <- as.data.frame(VarCorr(m_mix))
v_praia <- vc$vcov[vc$grp == "Beach"]
v_resid <- vc$vcov[vc$grp == "Residual"]
round(c(var_praia = v_praia, var_residual = v_resid,
        ICC = v_praia / (v_praia + v_resid)), 3)

# TAKE-HOME: o ICC é a fração da variação que está ENTRE praias. Perto de
# zero, o agrupamento pouco importa; alto, duas amostras da mesma praia
# são quase uma observação só. É a medida de quanto você teria errado
# ignorando a estrutura.

# 4. Inclinação aleatória ----------------------------------------------
m_slope <- lmer(Richness ~ NAP + (NAP | Beach), data = rikz, REML = TRUE)
print(VarCorr(m_slope), comp = "Variance")

# Convergiu? o que o lme4 avisou?
m_slope@optinfo$conv$lme4$messages

# TAKE-HOME: `(1 | praia)` diz que as praias partem de níveis diferentes;
# `(NAP | praia)` diz que a RESPOSTA ao NAP também difere entre praias.
# São a mesma ideia aplicada a coeficientes diferentes — não dois métodos.
# Com 5 amostras por praia há pouca informação para estimar a inclinação
# de cada uma: correlação ±1 ou variância zero é sinal de estrutura
# aleatória complexa demais para o delineamento, não de erro de código.

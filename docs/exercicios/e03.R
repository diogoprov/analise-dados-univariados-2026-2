# Encontro 3 — Prática de referência
# ---------------------------------------------------------------------
# Dois fatores e interação, nos dados do curso. Mesma numeração do slide.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(palmerpenguins)
  library(car)
  library(emmeans)
})
theme_set(theme_minimal(base_size = 14))

pinguins <- penguins |>
  drop_na(body_mass_g, species, sex, flipper_length_mm) |>
  mutate(species = factor(species), sex = factor(sex))

# 1. Dois fatores e interação ------------------------------------------
m_int <- lm(body_mass_g ~ species * sex, data = pinguins)
m_adi <- lm(body_mass_g ~ species + sex, data = pinguins)
summary(m_int)

# TAKE-HOME: interação não é "um termo a mais". É afirmar que o efeito de
# um fator DEPENDE do nível do outro — uma hipótese biológica distinta.

# 2. Interação vale a pena? --------------------------------------------
anova(m_adi, m_int)

# TAKE-HOME: comparar modelos aninhados por anova() responde "o termo extra
# melhora o ajuste mais do que o esperado por acaso?". Não responde "a
# interação existe na natureza".

# 3. As retas são paralelas? -------------------------------------------
print(
  pinguins |>
    ggplot(aes(sex, body_mass_g, colour = species, group = species)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    labs(x = NULL, y = "Massa (g)", colour = "Espécie")
)

# TAKE-HOME: paralelas = sem interação. O gráfico de médias por grupo diz
# em dois segundos o que a tabela leva um parágrafo para dizer — e mostra
# o TAMANHO do efeito, que o valor de p esconde.

# 4. Somas de quadrados II e III ---------------------------------------
Anova(m_int, type = 2)
Anova(m_int, type = 3)

# Para o tipo III fazer sentido, o contraste precisa ser de soma zero:
op <- options(contrasts = c("contr.sum", "contr.poly"))
m_int_sum <- lm(body_mass_g ~ species * sex, data = pinguins)
Anova(m_int_sum, type = 3)
options(op)   # devolve o padrão, senão vale para o resto da sessão

# TAKE-HOME: com delineamento balanceado e sem interação, II e III
# coincidem. Diferiram? Então o desbalanceamento ou a interação estão
# fazendo trabalho — e você precisa dizer qual tipo usou e por quê.

# 5. Só o contraste que interessa --------------------------------------
emmeans(m_int, pairwise ~ sex | species)

# TAKE-HOME: `emmeans` compara tudo contra tudo se você deixar. A pergunta
# biológica é que decide quais contrastes pedir — não o software.

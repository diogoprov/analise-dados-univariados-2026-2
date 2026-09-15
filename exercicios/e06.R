# Encontro 6 — Prática de referência
# ---------------------------------------------------------------------
# Diagnóstico com DHARMa, sobredispersão e excesso de zeros.
# O DHARMa simula: a semente fixa deixa a aula reprodutível.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages({
  library(tidyverse)
  library(DHARMa)
  library(glmmTMB)
})
theme_set(theme_minimal(base_size = 14))

RK <- read.delim(dados("RoadKills.txt"))
parasitas <- read_tsv(dados("Planilha_Pentastomida.txt"), show_col_types = FALSE) |>
  rename(id = 1) |>
  drop_na(Raillietiella_mottae, CRC, Sexo, Especie)

m_rk <- glm(TOT.N ~ D.PARK, family = poisson, data = RK)

# 1. Resíduos quantílicos simulados ------------------------------------
set.seed(2026)
res <- simulateResiduals(m_rk, n = 1000)
plot(res)

# TAKE-HOME: o DHARMa transforma resíduo de qualquer família numa escala
# uniforme. Por isso o mesmo par de gráficos serve para Poisson, binomial
# negativa, binomial e modelo misto — e por isso substitui o `plot()` da
# base, que só faz sentido no caso gaussiano.

# 2. Dispersão, zeros e outliers ---------------------------------------
testDispersion(res, plot = FALSE)
testZeroInflation(res, plot = FALSE)
testOutliers(res, plot = FALSE)

# TAKE-HOME: são testes com valor de p, não impressão visual. Mas veja a
# ORDEM: testar zeros antes de resolver a sobredispersão costuma acusar
# excesso de zeros que a binomial negativa sozinha explica.

# 3. Se houver sobredispersão, a binomial negativa ---------------------
m_nb <- MASS::glm.nb(TOT.N ~ D.PARK, data = RK)

comparacao <- tibble(
  modelo = c("Poisson", "Binomial negativa"),
  razao_deviance = c(deviance(m_rk) / df.residual(m_rk),
                     deviance(m_nb) / df.residual(m_nb)),
  erro_padrao_D.PARK = c(summary(m_rk)$coefficients["D.PARK", "Std. Error"],
                         summary(m_nb)$coefficients["D.PARK", "Std. Error"]),
  AIC = c(AIC(m_rk), AIC(m_nb))
)
comparacao |> mutate(across(where(is.numeric), \(x) signif(x, 3)))

# TAKE-HOME: a estimativa pontual quase não muda; o ERRO-PADRÃO muda muito.
# Ignorar sobredispersão não enviesa o coeficiente — produz intervalos
# estreitos demais e valores de p pequenos demais. É por isso que importa.
#
# E lembre do Burnham & Anderson (secao 2.5, p. 68): razao acima de ~4
# costuma indicar ESTRUTURA faltando, não só variância extra. Antes de
# trocar de família, pergunte que covariável ou agrupamento ficou de fora.

# 4. Zeros que sobram depois da NB -------------------------------------
set.seed(2026)
res_nb <- simulateResiduals(m_nb, n = 1000)
testZeroInflation(res_nb, plot = FALSE)

# Exemplo com zero-inflação de fato (dados de parasitas):
ziNB <- glmmTMB(Raillietiella_mottae ~ CRC + Sexo * Especie,
                ziformula = ~ ., data = parasitas, family = nbinom2)
summary(ziNB)$coefficients$zi

# TAKE-HOME: a parte `zi` está em log-odds de ser zero ESTRUTURAL — um
# hospedeiro que não pode ter o parasita. Sinais opostos entre as duas
# tabelas não são contradição: uma diz quantos parasitas há onde há, a
# outra diz onde não pode haver. Só ajuste ZI se você souber dizer, em
# biologia, o que é o zero estrutural no seu sistema.

# 5. O parágrafo de métodos --------------------------------------------
# Modelo, para o aluno adaptar (estrutura Davis & Kay 2023):
#
#   "Ajustamos um GLM com distribuição binomial negativa e ligação log
#    (MASS::glm.nb), depois que um modelo de Poisson apresentou
#    sobredispersão (deviance residual / gl = 7,8). O diagnóstico usou
#    resíduos quantílicos simulados (DHARMa, 1000 simulações, semente
#    2026); não houve evidência de dispersão residual (p = ...) nem de
#    excesso de zeros (p = ...)."
#
# TAKE-HOME: o diagnóstico que NÃO acusou problema também se relata. É o
# que permite ao leitor confiar no resto — e é o item que mais falta nos
# artigos de ecologia.

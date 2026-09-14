# Testa, fora do Quarto, os blocos hoje marcados `eval: false` que DEPENDEM de
# pacotes externos (DHARMa, MuMIn, dagitty, performance, ordinal).
#
# Objetivo: descobrir quais rodam de verdade nesta máquina, e quanto demoram,
# ANTES de ligá-los nos slides — assim não descobrimos um erro no meio de um
# `quarto render` de dez decks.
#
# Uso, a partir da raiz do repositório:
#
#     Rscript R/testa-eval-false.R
#
# Ele não altera nenhum .qmd. Só imprime um relatório.
# ------------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositório (onde está o _quarto.yml).")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(purrr)
})

# ---- 1. os pacotes estão instalados? -----------------------------------
pacotes <- c("DHARMa", "MuMIn", "dagitty", "performance",
             "ordinal", "glmmTMB", "lme4", "MASS", "brms")

estado <- map_dfr(pacotes, function(p) {
  tem <- requireNamespace(p, quietly = TRUE)
  tibble(pacote = p,
         instalado = tem,
         versao = if (tem) as.character(utils::packageVersion(p)) else NA_character_)
})

message("\n=== Pacotes ===")
pwalk(estado, function(pacote, instalado, versao) {
  message(sprintf("  %-12s %s", pacote, if (instalado) versao else "NAO INSTALADO"))
})

faltando <- estado$pacote[!estado$instalado]
if (length(faltando)) {
  message("\n  Para instalar o que falta:")
  message('  install.packages(c("', paste(faltando, collapse = '", "'), '"))')
}

# ---- 2. objetos de que os blocos dependem ------------------------------
# Reproduz o setup de cada deck. Os caminhos aqui são a partir da raiz;
# nos slides são "../dados/..." porque o Quarto renderiza de dentro de slides/.

message("\n=== Preparando os objetos ===")

suppressPackageStartupMessages({
  library(lme4); library(glmmTMB); library(ordinal)
})

# E2
library(palmerpenguins)
pinguins <- penguins |> drop_na(bill_length_mm, body_mass_g, species, sex)
m_mult   <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = pinguins)

# E5 / E6
RK        <- read.delim("dados/RoadKills.txt")
RK$D.PARK_km <- RK$D.PARK / 1000
m_rk      <- glm(TOT.N ~ D.PARK, family = poisson, data = RK)

parasitas <- read_tsv("dados/Planilha_Pentastomida.txt", show_col_types = FALSE) |>
  rename(id = 1) |>
  drop_na(Raillietiella_mottae, CRC, Sexo, Especie)

ziP  <- glmmTMB(Raillietiella_mottae ~ CRC + Sexo * Especie,
                ziformula = ~ ., data = parasitas, family = poisson)

mela <- read_delim("dados/NA.csv", delim = ";", show_col_types = FALSE) |>
  mutate(Black = factor(Black, ordered = TRUE),
         Treatment = factor(Treatment), Time = factor(Time),
         Animal = factor(Animal))
m_ord <- clm(Black ~ Treatment + Time, data = mela, link = "logit")

# E8
rikz  <- read.table("dados/RIKZ.txt", header = TRUE, row.names = 1) |>
  mutate(Beach = factor(Beach))
m_nb  <- glmmTMB(Richness ~ NAP + (1 | Beach), family = nbinom2, data = rikz)

# E9
modelos <- list(
  nulo        = MASS::glm.nb(TOT.N ~ 1, data = RK),
  parque      = MASS::glm.nb(TOT.N ~ D.PARK_km, data = RK),
  habitat     = MASS::glm.nb(TOT.N ~ OPEN.L + MONT.S, data = RK),
  agua        = MASS::glm.nb(TOT.N ~ L.WAT.C + D.WAT.RES, data = RK),
  parque_agua = MASS::glm.nb(TOT.N ~ D.PARK_km + L.WAT.C, data = RK)
)

message("  ok")

# ---- 3. os blocos candidatos -------------------------------------------
# A chave é "<deck> linha <n> — <título do slide>", para eu saber exatamente
# onde mexer. Cada bloco é uma função sem argumento.

set.seed(2026)   # o DHARMa simula: sem semente, cada render dá um gráfico

candidatos <- list(

  "e02:740 — Versão moderna (performance::check_model)" = function() {
    print(performance::check_model(m_mult))
  },

  "e05:514 — Na prática (dagitty::adjustmentSets)" = function() {
    g <- dagitty::dagitty('dag {
      Precipitacao -> Area
      Precipitacao -> Riqueza
      Area -> Riqueza
      Area -> Deteccao
      Riqueza -> Deteccao
    }')
    print(dagitty::adjustmentSets(g, exposure = "Area", outcome = "Riqueza"))
  },

  "e06:170 — Com o DHARMa (m_rk)" = function() {
    res <- DHARMa::simulateResiduals(m_rk, n = 1000)
    plot(res)
    print(DHARMa::testDispersion(res, plot = FALSE))
    print(DHARMa::testZeroInflation(res, plot = FALSE))
    print(DHARMa::testOutliers(res, plot = FALSE))
  },

  "e06:443 — A rota robusta (DHARMa em ziP)" = function() {
    res <- DHARMa::simulateResiduals(ziP, n = 1000)
    print(DHARMa::testZeroInflation(res, plot = FALSE))
  },

  "e06:548 — Com medidas repetidas (clmm)" = function() {
    m_ordm <- clmm(Black ~ Treatment * Time + (1 | Animal),
                   data = mela, link = "logit")
    print(summary(m_ordm))
  },

  "e06:568 — Verificando a premissa (nominal_test)" = function() {
    print(nominal_test(m_ord))
  },

  "e08:201 — Diagnóstico (DHARMa em m_nb)" = function() {
    res <- DHARMa::simulateResiduals(m_nb, n = 1000)
    plot(res)
    print(DHARMa::testDispersion(res, plot = FALSE))
    print(DHARMa::testZeroInflation(res, plot = FALSE))
    DHARMa::plotResiduals(res, form = rikz$Beach)
  },

  "e08:366 — Priors (brms::get_prior)" = function() {
    print(brms::get_prior(Richness ~ NAP + (1 | Beach),
                          family = poisson(), data = rikz))
  },

  "e09:418 — Model averaging (MuMIn::model.avg)" = function() {
    med <- MuMIn::model.avg(modelos, fit = TRUE)
    print(summary(med))
    print(confint(med))
  },

  "e09:458 — O garimpo (MuMIn::dredge, 256 modelos)" = function() {
    global <- MASS::glm.nb(TOT.N ~ D.PARK_km + OPEN.L + MONT.S + L.WAT.C +
                             D.WAT.RES + URBAN + N.PATCH + P.EDGE, data = RK)
    options(na.action = "na.fail")        # o dredge exige
    print(head(MuMIn::dredge(global), 5))
  }
)

# ---- 4. roda cada um, isolado ------------------------------------------
# Os gráficos vão para um arquivo temporário: o bloco executa de verdade,
# mas nada abre na tela.

message("\n=== Rodando ===")
pdf(file.path(tempdir(), "testa-eval-false.pdf"))
na_saida <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))

resultado <- imap_dfr(candidatos, function(f, nome) {
  t0 <- Sys.time()
  erro <- NULL
  saida <- tryCatch(na_saida(f()),
                    error = function(e) { erro <<- conditionMessage(e); character(0) })
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  %-52s %s  (%.1f s)", substr(nome, 1, 52),
                  if (is.null(erro)) "OK    " else "ERRO  ", seg))
  if (!is.null(erro)) message("      ", erro)
  tibble(bloco = nome, ok = is.null(erro), segundos = round(seg, 1),
         linhas_de_saida = length(saida),
         erro = if (is.null(erro)) NA_character_ else erro)
})
invisible(dev.off())

# ---- 5. relatório -------------------------------------------------------
message("\n=========================================================")
message("RESUMO — cole isto na conversa\n")
resultado |>
  mutate(status = if_else(ok, "OK", "ERRO")) |>
  select(bloco, status, segundos, linhas_de_saida, erro) |>
  as.data.frame() |>
  print(right = FALSE, row.names = FALSE)

message("\nTempo total: ", round(sum(resultado$segundos), 1), " s")
if (any(!resultado$ok)) {
  message("Blocos com erro ficam como eval: false. Os que passaram eu ligo nos slides.")
} else {
  message("Todos passaram.")
}

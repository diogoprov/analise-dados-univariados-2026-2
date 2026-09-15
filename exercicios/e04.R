# Encontro 4 — Prática de referência
# ---------------------------------------------------------------------
# Escolher a distribuição a partir dos dados, não do hábito.
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}
dados <- function(f) file.path("dados", f)

suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_minimal(base_size = 14))

penta <- read_tsv(dados("Planilha_Pentastomida.txt"), show_col_types = FALSE) |>
  rename(id = 1) |>
  drop_na(Raillietiella_mottae, CRC, Especie) |>
  mutate(Especie = factor(Especie))

y <- penta$Raillietiella_mottae

# 1. Classificar a resposta --------------------------------------------
c(discreta   = all(y == floor(y)),
  minimo     = min(y),
  maximo     = max(y),
  media      = round(mean(y), 2),
  variancia  = round(var(y), 2),
  prop_zeros = round(mean(y == 0), 2))

# TAKE-HOME: três perguntas resolvem a escolha antes de qualquer teste —
# a resposta é discreta? é limitada por baixo? quantos zeros tem? Aqui:
# contagem, mínimo zero, e uma proporção de zeros que já pede atenção.

# 2. Variância contra média por classe ---------------------------------
print(
  penta |>
    mutate(classe = cut(CRC, breaks = 6)) |>
    group_by(classe) |>
    summarise(media = mean(Raillietiella_mottae),
              variancia = var(Raillietiella_mottae), n = n(), .groups = "drop") |>
    ggplot(aes(media, variancia)) +
    geom_point(size = 3, colour = "#1c6e8c") +
    geom_abline(slope = 1, intercept = 0, colour = "#c08a3e", linetype = 2) +
    labs(x = "Média da classe", y = "Variância da classe")
)

# TAKE-HOME: a linha tracejada é variância = média, a assinatura da
# Poisson. Pontos acima dela são sobredispersão, e ela aparece ANTES de
# ajustar qualquer modelo — basta agrupar e olhar.

# 3. Simular sob duas candidatas ---------------------------------------
set.seed(2026)
print(
  bind_rows(
    tibble(valor = y,                         fonte = "Observado"),
    tibble(valor = rpois(length(y), mean(y)), fonte = "Poisson"),
    tibble(valor = rnbinom(length(y), mu = mean(y), size = .3),
           fonte = "Binomial negativa")
  ) |>
    ggplot(aes(valor)) +
    geom_bar(fill = "#1c6e8c") +
    facet_wrap(~fonte) +
    coord_cartesian(xlim = c(0, 8)) +
    labs(x = NULL, y = NULL)
)

# TAKE-HOME: simular sob cada hipótese e comparar com o observado é o mesmo
# raciocínio do pp_check() bayesiano do Encontro 8, feito à mão. Se o
# modelo não consegue gerar dados parecidos com os seus, ele não descreve
# os seus dados.

# 4. A frase que vai para o trabalho final -----------------------------
# Modelo de frase, para o aluno adaptar:
#
#   "A resposta é uma contagem de parasitas por hospedeiro, com mínimo
#    zero e proporção de zeros de XX %. A variância cresce mais rápido
#    que a média entre classes de tamanho do hospedeiro, o que descarta a
#    Poisson; adotamos a binomial negativa, reavaliada por diagnóstico de
#    resíduos quantílicos no Encontro 6."
#
# TAKE-HOME: este é o item 3 do checklist de Davis & Kay (2023) — método
# estatístico com justificativa. Escrever a frase agora poupa o parágrafo
# de métodos depois, e obriga a decidir antes de rodar.

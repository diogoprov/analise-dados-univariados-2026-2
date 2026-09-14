# Por que performance::check_model() não desenha nada dentro do Quarto?
#
# O chunk do Encontro 2 executa sem erro e não produz figura. O `_quarto.yml`
# tem `warning: false` e `message: false`, então qualquer explicação que o
# performance tenha dado foi suprimida. Aqui as condições são CAPTURADAS em
# vez de suprimidas, e o PNG é medido em bytes — a verificação que faltou.
#
# Uso, a partir da raiz do repositório:
#
#     Rscript R/diagnostica-check-model.R
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(palmerpenguins)
  library(performance)
})

message("performance ", utils::packageVersion("performance"),
        " | see ", utils::packageVersion("see"),
        " | R ", getRversion())

pinguins <- penguins |> drop_na(bill_length_mm, body_mass_g, species, sex)
m_mult   <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = pinguins)

# Três formas de invocar. A primeira é a que está no slide.
formas <- list(
  "auto-print (como no slide)" = function() check_model(m_mult),
  "print() explícito"          = function() print(check_model(m_mult)),
  "plot() explícito"           = function() plot(check_model(m_mult))
)

roda <- function(f, rotulo) {
  arq <- tempfile(fileext = ".png")
  png(arq, width = 1200, height = 900, res = 150)

  avisos <- character(0)
  msgs   <- character(0)
  erro   <- NULL

  withCallingHandlers(
    tryCatch(f(), error = function(e) erro <<- conditionMessage(e)),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { msgs   <<- c(msgs,   conditionMessage(m)); invokeRestart("muffleMessage") }
  )

  invisible(dev.off())
  bytes <- if (file.exists(arq)) file.size(arq) else 0L

  message("\n--- ", rotulo)
  message("    png: ", bytes, " bytes  ",
          if (bytes > 20000) "(desenhou)" else "(NAO desenhou)")
  if (!is.null(erro))      message("    ERRO : ", erro)
  if (length(avisos)) walk(avisos, ~ message("    AVISO: ", trimws(.x)))
  if (length(msgs))   walk(msgs,   ~ message("    MSG  : ", trimws(.x)))

  tibble(forma = rotulo, bytes = bytes,
         erro = erro %||% NA_character_,
         avisos = if (length(avisos)) paste(trimws(avisos), collapse = " | ") else NA_character_,
         mensagens = if (length(msgs)) paste(trimws(msgs), collapse = " | ") else NA_character_)
}

message("\n=== check_model() ===")
res <- imap_dfr(formas, ~ roda(.x, .y))

# Controle: um gráfico trivial no mesmo dispositivo, para confirmar que o
# problema é o check_model() e não o PNG.
arq <- tempfile(fileext = ".png")
png(arq, width = 1200, height = 900, res = 150)
print(ggplot(pinguins, aes(bill_length_mm, body_mass_g)) + geom_point())
invisible(dev.off())
message("\n--- controle (ggplot simples)")
message("    png: ", file.size(arq), " bytes")

message("\n=========================================================")
message("RESUMO — cole isto na conversa\n")
res |> as.data.frame() |> print(right = FALSE, row.names = FALSE)

# Verifica que os oito scripts de exercicios/ rodam de ponta a ponta.
# ---------------------------------------------------------------------
# Cada script roda em ambiente próprio, dentro de tryCatch, com os
# gráficos indo para um PDF temporário. O relatório traz, além de OK/ERRO,
# o NÚMERO DE PÁGINAS do PDF — porque "não deu erro" não é "produziu
# figura", lição que já custou três ciclos de render nesta disciplina.
#
# Uso, a partir da raiz do repositório:
#
#     Rscript R/testa-exercicios.R
# ---------------------------------------------------------------------

if (!file.exists("_quarto.yml")) {
  stop("Rode a partir da raiz do repositorio (onde esta o _quarto.yml).")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(purrr)
})

scripts <- sort(list.files("exercicios", pattern = "^e\\d\\d\\.R$", full.names = TRUE))
if (!length(scripts)) stop("Nenhum script encontrado em exercicios/.")

conta_paginas <- function(arq) {
  if (!file.exists(arq)) return(0L)
  n <- length(grep("/Type\\s*/Page[^s]", readLines(arq, warn = FALSE)))
  if (n == 0L && file.size(arq) > 5000) NA_integer_ else n
}

# Um script sem chamada de gráfico deve mesmo marcar zero figura. Só é
# problema quando ele desenha no papel e não desenha na prática.
desenha_algo <- function(caminho) {
  fonte <- paste(readLines(caminho, warn = FALSE), collapse = "\n")
  any(purrr::map_lgl(c("ggplot(", "plot(", "pp_check(", "plotResiduals("),
                     \(p) grepl(p, fonte, fixed = TRUE)))
}

roda <- function(caminho) {
  pdf_tmp <- tempfile(fileext = ".pdf")
  pdf(pdf_tmp)
  t0   <- Sys.time()
  erro <- NULL
  avisos <- character(0)

  withCallingHandlers(
    tryCatch(
      # ambiente próprio, e print.eval = TRUE para reproduzir o que o
      # console faz: sem isso, ggplot e summary() no topo do script não
      # imprimem, e o teste reporta zero figura num script correto
      source(caminho, local = new.env(parent = globalenv()),
             echo = FALSE, print.eval = TRUE),
      error = function(e) erro <<- conditionMessage(e)
    ),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) invokeRestart("muffleMessage")
  )

  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  invisible(dev.off())

  pg <- conta_paginas(pdf_tmp)
  message(sprintf("  %-18s %s  %5.1f s   figuras: %s",
                  basename(caminho),
                  if (is.null(erro)) "OK   " else "ERRO ",
                  seg,
                  ifelse(is.na(pg), "?", pg)))
  if (!is.null(erro)) message("      ERRO : ", erro)
  if (length(avisos)) walk(head(avisos, 3), ~ message("      aviso: ", trimws(.x)))

  tibble(script = basename(caminho),
         status = if (is.null(erro)) "OK" else "ERRO",
         segundos = round(seg, 1),
         figuras = pg,
         esperava_figura = desenha_algo(caminho),
         aviso1 = if (length(avisos)) substr(trimws(avisos[1]), 1, 60) else NA_character_,
         erro = erro %||% NA_character_)
}

message("\n=== Rodando os scripts de exercicios/ ===\n")
res <- map_dfr(scripts, roda)

message("\n=========================================================")
message("RESUMO — cole isto na conversa\n")
res |> dplyr::select(script, status, segundos, figuras, esperava_figura, aviso1, erro) |>
  as.data.frame() |> print(right = FALSE, row.names = FALSE)

message("\nTempo total: ", round(sum(res$segundos), 1), " s")
falhas <- res |> filter(status == "ERRO")
mudos  <- res |> filter(esperava_figura, figuras == 0)
if (nrow(falhas)) {
  message("Com erro: ", paste(falhas$script, collapse = ", "))
} else if (nrow(mudos)) {
  message("Rodaram, mas tem chamada de grafico e nao desenharam: ",
          paste(mudos$script, collapse = ", "))
} else {
  message("Todos rodaram. Todo script com chamada de grafico desenhou; ",
          "os demais nao tem grafico e marcam zero corretamente.")
}

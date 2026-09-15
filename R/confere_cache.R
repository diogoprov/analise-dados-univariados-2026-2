# Confere se as chamadas brm() do precompila-cache.R são idênticas às que
# aparecem nos slides. Importa porque o argumento file= do brms, no padrão
# (file_refit = "never"), carrega o .rds sem verificar se a fórmula mudou:
# uma divergência faria a aula exibir a saída do modelo errado, em silêncio.
#
# Uso:  source("R/confere_cache.R"); confere_cache()

extrai_brm <- function(caminho) {
  linhas <- readLines(caminho, warn = FALSE, encoding = "UTF-8")
  txt <- paste(linhas, collapse = "\n")
  inicios <- gregexpr("brm\\(", txt, fixed = FALSE)[[1]]
  if (inicios[1] == -1) return(character(0))
  chars <- strsplit(txt, "")[[1]]
  vapply(inicios, function(i) {
    nivel <- 0L
    j <- i + 3L
    repeat {
      if (j > length(chars)) break
      if (chars[j] == "(") nivel <- nivel + 1L
      if (chars[j] == ")") {
        nivel <- nivel - 1L
        if (nivel == 0L) break
      }
      j <- j + 1L
    }
    substr(txt, i, j)
  }, character(1))
}

normaliza <- function(x) {
  x <- gsub("\\s+", " ", x)
  x <- gsub('"../cache/', '"cache/', x, fixed = TRUE)   # o slide roda de slides/
  x <- gsub(",\\s*file_refit\\s*=\\s*[^,)]+", "", x)     # só o script usa
  trimws(x)
}

confere_cache <- function(script = "R/precompila-cache.R",
                          decks = c("slides/e02.qmd", "slides/e08.qmd",
                                    "slides/e09.qmd",
                                    "exercicios/e08.R")) {
  ref <- normaliza(extrai_brm(script))
  ok <- TRUE
  for (d in decks) {
    for (ch in extrai_brm(d)) {
      if (!grepl("file =", ch, fixed = TRUE)) next
      if (grepl("...", ch, fixed = TRUE)) next          # bloco ilustrativo
      arquivo <- sub('.*file = "([^"]+)".*', "\\1", gsub("\n", " ", ch))
      bate <- normaliza(ch) %in% ref
      ok <- ok && bate
      message(sprintf("  %-8s %-16s %s", if (bate) "OK" else "DIVERGE", d, arquivo))
      if (!bate) message("      slide : ", normaliza(ch))
    }
  }
  if (!ok) {
    stop("Divergencia entre o script e os slides. Corrija antes de ajustar ",
         "os modelos, senao o cache vai guardar o modelo errado.",
         call. = FALSE)
  }
  message("  todas as chamadas batem")
  invisible(TRUE)
}

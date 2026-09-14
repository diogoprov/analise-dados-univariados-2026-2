# Análise de Dados Univariados — PPGBA/UFMS, 2026/2

Materiais da disciplina **Análise de Dados Univariados**, oferecida no formato
intensivo de **21 a 25 de setembro de 2026** no Programa de Pós-Graduação em
Biologia Animal da UFMS.

Site: <https://provetelab.org/analise-dados-univariados-2026-2/>

## Estrutura

```
.
├── _quarto.yml             configuração do site
├── index.qmd               página inicial + cronograma
├── programa.qmd            ementa, objetivos, avaliação, bibliografia
├── instalacao.qmd          preparação do ambiente (R, RStudio, pacotes)
├── trabalho-final.qmd      template do trabalho final
├── encontros/              uma página por encontro (E1–E10)
├── slides/                 slides revealjs em Quarto (.qmd)
├── dados/                  conjuntos de dados usados nas aulas
├── cache/                  objetos brms compilados (não versionado)
└── docs/                   site renderizado — servido pelo GitHub Pages
```

## Como renderizar

```bash
quarto render          # site inteiro -> docs/
quarto render slides/e01.qmd   # um deck só
quarto preview         # servidor local com recarga automática
```

O GitHub Pages serve o conteúdo de `docs/` no branch `main`. O fluxo é o mesmo
do site do laboratório: editar `.qmd` → `quarto render` → commit → push.

## Licença

Material didático sob [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt-br).
Código sob licença MIT.

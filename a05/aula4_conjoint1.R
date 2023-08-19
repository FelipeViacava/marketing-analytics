# Analise de Preferencia Conjunta - Conjoint Analysis

da <- readxl::read_excel("chocolate.xlsx")

m <- lm(preferencia ~ ., data = da[,2:18])

# utilidade total ----

tabela_efeitos <- m |>
  broom::tidy() |>
  # coloca zero onde é NA
  tidyr::replace_na(list(estimate = 0)) |>
  # adiciona o tipo
  dplyr::mutate(tipo = c(
    "i",
    rep("sabor", 4),
    rep("formato", 4),
    rep("recheio", 6),
    rep("preco", 2)
  )) |>
  # tira o intercepto
  dplyr::filter(tipo != "i")

utilidade_total <- tabela_total |>
  # calcula o maximo menos o minimo
  dplyr::group_by(tipo) |>
  dplyr::summarise(parcial = diff(range(estimate))) |>
  # normaliza
  dplyr::mutate(importancia = parcial / sum(parcial)) |>
  dplyr::arrange(dplyr::desc(importancia))

utilidade_total

# utilidade parcial ----

utilidade_parcial <- tabela_efeitos |>
  dplyr::group_by(tipo) |>
  dplyr::mutate(parcial = (estimate - min(estimate)) * 10) |>
  dplyr::ungroup()

# pergunta 3

tabela_efeitos |>
  dplyr::filter(term %in% c("x1", "x10", "x6")) |>
  dplyr::summarise(s = sum(estimate))
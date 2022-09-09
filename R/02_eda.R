##### IMPORTAR DADOS LIMPOS #####

# Ler dados a partir do arquivo exportado no script '01_data.R'
dados_limpos <- readr::read_rds("data/clean_data.rds")

##### DESCREVER DESFECHO E EXPOSIÇÃO #####

# Dar uma olhada na estrutura
dados_limpos |>
    dplyr::glimpse()

# Checar incidência de transtorno bipolar aos 22 anos
dados_limpos |>
    dplyr::count(bd_any)

# Checar incidência de transtorno bipolar agrupada pela variável de uso
# de maconha
dados_limpos |>
    dplyr::filter(!is.na(maconha)) |>
    dplyr::group_by(maconha) |>
    dplyr::count(bd_any)


##### DATA VIS ######

set.seed(123)
pval_maconha <- chisq.test(x = dados_limpos$maconha,
                           y = dados_limpos$bd_any,
                           correct = TRUE)$p.value

dados_limpos |>
    dplyr::filter(!is.na(maconha)) |>
    dplyr::group_by(maconha) |>
    dplyr::count(bd_any) |>
    dplyr::mutate(pct = n/sum(n)) |>
    ggplot2::ggplot(ggplot2::aes(fill = bd_any, x = maconha, y = n)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct)),
                       position = ggplot2::position_fill(vjust = 1.02),
                       size = 7) +
    ggplot2::annotate(
        "text",
        x = 2,
        y = 0.5,
        label = paste0("p-value: ", signif(pval_maconha, 3)),
        size = 8
    ) +
    ggplot2::theme_minimal(base_size = 22) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(x = "Lifetime cannabis use at 18 years old",
                  fill = "BD at 22 years old",
                  y = "% of subjects") +
    ggplot2::theme(legend.position = "top")

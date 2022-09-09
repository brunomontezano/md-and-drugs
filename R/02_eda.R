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

# Criar função para checar incidência de TB estratificado por alguma variável
contar_tb_estratificado <- function(dados, variavel) {
    dados |>
        dplyr::filter(!is.na(.data[[variavel]])) |>
        dplyr::group_by(.data[[variavel]]) |>
        dplyr::count(.data[["bd_any"]])
}

##### VALORES AUSENTES #####

# Checar quantidade de missings em cada variável através de visualização
DataExplorer::plot_missing(dados_limpos)

# Mesma coisa em texto
dados_limpos |>
    purrr::map_chr(\(x) stringr::str_c(as.character(mean(is.na(
        x
    )) * 100), "%"))

# Checar incidência de transtorno bipolar agrupada pela variável de uso
# de maconha
dados_limpos |>
    contar_tb_estratificado("maconha")

# Isso pode ser feito para as outras variáveis
dados_limpos |>
    dplyr::select(-c(bd1_22, bd2_22)) |>
    purrr::map(contar_tb_estratificado)

##### DATA VIS ######

set.seed(123)
pval_maconha <- chisq.test(x = dados_limpos$maconha,
                           y = dados_limpos$bd_any,
                           correct = TRUE)$p.value

dados_limpos |>
    contar_tb_estratificado("maconha") |>
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

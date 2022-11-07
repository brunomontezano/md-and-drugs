##### IMPORTAR DADOS LIMPOS #####

# Colocar uma seed
set.seed(123)
dados_limpos <- readr::read_rds("data/clean_data.rds")

set.seed(123)
receita <- recipes::recipe(
    bd_any ~ maconha + cocaina + sexo + cordapele + renda_por_1000 +
        ja_apanhou,
                           data = dados_limpos) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors())

dados_preprocessados <- receita |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

log_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("glm")

fluxo <- workflows::workflow() |>
    workflows::add_model(log_mod) |>
    workflows::add_recipe(receita)

fluxo

log_fit <- fluxo |>
    parsnip::fit(data = dados_limpos)

set.seed(123)
intervalos <- exp(confint(log_fit |> hardhat::extract_fit_engine())) |>
    tibble::as_tibble(rownames = "term") |>
    janitor::clean_names() |>
    dplyr::slice(-1)

broom::tidy(log_fit) |>
    dplyr::mutate(or = exp(estimate))

tb_plot <- broom::tidy(log_fit) |>
    dplyr::slice(-1) |>
    dplyr::left_join(intervalos, by = "term") |>
    dplyr::transmute(
        variavel = term,
        or = exp(estimate),
        lower_int = x2_5_percent,
        upper_int = x97_5_percent,
        pvalue = p.value
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = or, y = variavel,
                 label = paste0(
                     as.character(format(or, digits = 2)),
                     " (",
                     as.character(format(lower_int, digits = 2)),
                     "-",
                     as.character(format(upper_int, digits = 2)),
                     ")"
                     )) +
    ggplot2::geom_point(size = 5, color = "tomato3") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower_int, xmax = upper_int),
                            color = "tomato3",
                            size = 0.8) +
    ggplot2::geom_vline(xintercept = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 7.5)) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::labs(x = "Adjusted odds ratio",
                  y = "Variables") +
    ggplot2::scale_y_discrete(labels = c("Cocaine lifetime use",
                                         "Skin color (white)",
                                         "Beaten by parents",
                                         "Cannabis lifetime use",
                                         "Family income (by R$ 1000)",
                                         "Sex (male)")) +
    ggplot2::geom_text(size = 6, hjust = -0.2, vjust = -0.5)

plot_tb <- tb_plot + ggplot2::labs(title = "Bipolar disorder",
                                   y = "",
                                   x = "") +
    ggplot2::theme_minimal(12) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_text(size = 15)
                   )
plot_th <- thplot + ggplot2::labs(title = "Mood disorder",
                                   y = "",
                                  x = "") +
    ggplot2::theme_minimal(12) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

library(patchwork)

plot_tb + plot_th + patchwork::plot_annotation(
    "Adjusted odds ratio for each outcome",
    theme = ggplot2::theme(title = ggplot2::element_text(size = 14))
)


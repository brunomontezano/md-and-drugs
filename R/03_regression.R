##### IMPORTAR DADOS LIMPOS #####

# Colocar uma seed
set.seed(123)

dados_limpos <- readr::read_rds("data/clean_data.rds")


receita <- recipes::recipe(bd_any ~ maconha + sexo + consultou_psi,
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

intervalos <- exp(confint(log_fit |> hardhat::extract_fit_engine())) |>
    tibble::as_tibble(rownames = "term") |>
    janitor::clean_names() |>
    dplyr::slice(-1)

broom::tidy(log_fit) |>
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
    ggplot2::aes(x = or, y = variavel) +
    ggplot2::geom_point(size = 5, color = "royalblue") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower_int, xmax = upper_int),
                            color = "royalblue",
                            size = 0.8) +
    ggplot2::geom_vline(xintercept = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 3)) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::labs(x = "Odds ratio",
                  y = "Feature") +
    ggplot2::scale_y_discrete(labels = c("Mental health treatment",
                                         "Cannabis use",
                                         "Sex (male)"))

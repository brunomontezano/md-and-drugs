banco <- haven::read_sav("data/banco_QI_15_07.sav")

banco |>
    dplyr::count(sexonovo)

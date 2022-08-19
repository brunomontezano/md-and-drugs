library(dplyr)
library(haven)

#Carregando dados

coorte_teste <- read_sav(file.choose())

#Selecionando variáveis

vars <- c("sexonovo", "kc05", "kc06", "kc07", "kc08", "kc09",
          "kc10", "kc11", "kc12", "kc13", "kc14", "kc15",
          "kc16", "kc17", "l063", "l064", "l065", "l066",
          "l067", "l068", "l069", "l070", "l071", "l072",
          "l076", "l074", "l081", "l082", "l084", "l085",
          "l086", "l087", "l088", "l089", "l090", "l091",
          "l092", "l093", "l094", "l083")

coorte_teste_md <- coorte_teste |>
    select(
        any_of(vars)
    )

#Diagnóstico de Transtorno Depressivo Maior

#Major Depressive Disorder is characterized by one or more Major Depressive Episodes
#(at least 2 weeks of depressed mood or loss of interest
#accompanied by at least four additional symptoms of depression).

coorte_teste_md <- coorte_teste_md |>
    mutate(
        epdep_22 = if_else(
            l063 == 1 | l064 == 1,
            if_else(
                l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 3,
                1, 0
            ), if_else(
                l063 == 1 & l064 == 1,
                if_else (
                    l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 3,
                    1, 0
                ), 0
            )
        )
    )

#Diagnóstico de Transtorno Bipolar I

#is characterized by one or more Manic or Mixed Episodes,
#usually accompanied by Major Depressive Episodes.

    #Episódio Hipomaníaco

coorte_teste_md <- coorte_teste_md |>
    mutate(
        ephip_pa_22 = if_else(
            (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
            if_else(
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                if_else(
                    l093 & l094 == 0,
                    1, 0), if_else(
                        l081 == 0,
                        if_else(
                            l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                            if_else(
                                l093 & l094 == 0,
                                1, 0), 0
                            )
                        )
                    )
                )
            )
        )
    )

#Diagnóstico de Transtorno Bipolar II

#is characterized by one or more Major Depressive Episodes
#accompanied by at least one Hypomanic Episode.





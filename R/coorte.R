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

    #Episódio de Transtorno Depressivo Maior

coorte_teste_md <- coorte_teste_md |>
    mutate(
        epdep_22 = if_else(
            l063 == 1 & l064 == 1,
            if_else(
                l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 3,
                1, 0
            ), if_else(
                l063 == 1 | l064 == 1,
                if_else (
                    l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 4,
                    1, 0
                ), 0
            )
        )
    )

#Diagnóstico de Transtorno Bipolar I

#is characterized by one or more Manic or Mixed Episodes,
#usually accompanied by Major Depressive Episodes.

#Episódio Maníaco Passado

coorte_teste_md <- coorte_teste_md |>
    mutate(epman_pa_22 = if_else(
# Se D1a ou D2a e ausência de sintomatologia atual
        (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
        if_else(
# Se D1a = "Sim"
            l081 == 1,
            if_else(
# Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
# Se D4 é cotada como "Sim"
                if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0), 0),
            if_else(
# Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
# Se D4 é cotada como "Sim"
                if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0), 0)
        ), 0
    ))

#Episódio Maníaco Atual

coorte_teste_md <- coorte_teste_md |>
    mutate(epman_at_22 = if_else(
# Se D1a ou D2a e presença de sintomatologia atual
        (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
        if_else(
# Se D1a = "Sim"
            l081 == 1,
            if_else(
# Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
# Se D4 é cotada como "Sim"
                if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0), 0),
            if_else(
# Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
# Se D4 é cotada como "Sim"
                if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0), 0)
        ), 0
    ))

#Diagnóstico de Transtorno Bipolar II

#is characterized by one or more Major Depressive Episodes
#accompanied by at least one Hypomanic Episode.

    #Episódio Hipomaníaco Passado

coorte_teste_md <- coorte_teste_md |>
    mutate(ephip_pa_22 = if_else(
        (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
#Presença de D1a ou D2a e ausência de sintomatologia atual
        if_else(
# Se D1a = "Sim"
            l081 == 1,
            if_else(
# Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
# Se D4 é cotada como "Não"
                if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0), 0 ),
            if_else(
# Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
# Se D4 é cotada como "Não"
                if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0), 0)
        ), 0
    ))

#Episódio Hipomaníaco Atual

coorte_teste_md <- coorte_teste_md |>
    mutate(ephip_at_22 = if_else(
        (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
#Presença de D1a ou D2a e presença de sintomatologia atual
        if_else(
            l082 == 1, # Testando se D1b = "Sim"
            if_else(
# Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
# Se D4 é cotada como "Não"
                if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0), 0),
            if_else(
# Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
# Se D4 é cotada como "Sim"
                if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        0, 1), 0)
            ), 0
    ))








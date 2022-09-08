library(dplyr)
library(haven)

#Carregando dados

coorte_teste <- haven::read_sav("data/banco_QI_15_07.sav")

coorte_teste <- coorte_teste |>
    dplyr::filter(!is.na(l063))

# Selecting variables

vars <- c("sexonovo", "kc05", "kc06", "kc07", "kc08", "kc09",
          "kc10", "kc11", "kc12", "kc13", "kc14", "kc15",
          "kc16", "kc17", "l063", "l064", "l065", "l066",
          "l067", "l068", "l069", "l070", "l071", "l072",
          "l076", "l074", "l081", "l082", "l084", "l085",
          "l086", "l087", "l088", "l089", "l090", "l091",
          "l092", "l093", "l094", "l083", "kmini1", "kmini2",
          "kmini3", "kmini4", "kmini5", "kmini6", "kmini7",
          "kmini8", "kmini9", "kmini19", "kmini20", "kmini21",
          "kmini22", "kmini23", "kmini24", "kmini25", "kmini26",
          "kmini27", "kmini28", "kmini29", "kmini30", "kmini31",
          "kmini32")

da <- coorte_teste |>
    dplyr::select(dplyr::any_of(vars))

# Current Major Depressive Disorder Episode (18y.)

da <- da |>
    dplyr::mutate(
        eptdm_18 = dplyr::if_else(
            # Se A1 e A2 = "Sim"
            kmini1 == 1 & kmini2 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em A3
                kmini3 + kmini4 + kmini5 + kmini6 + kmini7 + kmini8 + kmini9 >= 3,
                1, 0,
                missing = 0
            ), dplyr::if_else(
                # Se A1 ou A2 = "Sim"
                kmini1 == 1 | kmini2 == 1,
                dplyr::if_else (
                    # Se há pelo menos 4 "Sim" em A3
                    kmini3 + kmini4 + kmini5 + kmini6 + kmini7 + kmini8 + kmini9 >= 4,
                    1, 0, missing = 0
                ), 0,
                missing = 0
            )
        )
    )

# Current Major Depressive Disorder Episode (22y.)

da <- da |>
    dplyr::mutate(
        eptdm_22 = dplyr::if_else(
            # Se A1 e A2 = "Sim"
            l063 == 1 & l064 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em A3
                l065 + l066 + l067 + l068 + l069 + l070 + l071 >= 3,
                1, 0,
                missing = 0
            ), dplyr::if_else(
                # Se A1 ou A2 = "Sim"
                l063 == 1 | l064 == 1,
                dplyr::if_else (
                    # Se há pelo menos 4 "Sim" em A3
                    l065 + l066 + l067 + l068 + l069 + l070 + l071 >= 4,
                    1, 0,
                    missing = 0
                ), 0,
                missing = 0
            )
        )
    )

# Past Maniac Episode (18y.)

da <- da |>
    dplyr::mutate(epman_pa_18 = dplyr::if_else(
        # Se D1a ou D2a = "Sim" e ausência de sintomatologia atual
        (kmini19 == 1 | kmini21 == 1) & (kmini20 == 0 & kmini22 == 0),
        dplyr::if_else(
            # Se D1a = "Sim"
            kmini19 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((kmini30 == 1 & kmini32 >= 1) | kmini31 == 1,
                        1, 0
                        ), 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((kmini30 == 1 & kmini32 >= 1) | kmini31 == 1,
                        1, 0), 0)
        ), 0,
        missing = 0
    ))

# Past Maniac Episode (22y.)

da <- da |>
    dplyr::mutate(epman_pa_22 = dplyr::if_else(
        # Se D1a ou D2a = "Sim" e ausência de sintomatologia atual
        (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
        dplyr::if_else(
            # Se D1a = "Sim"
            l081 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0, missing = 0), 0, missing = 0)
        ), 0,
                missing = 0
    ))

# Current Maniac Episode (18y.)

da <- da |>
    dplyr::mutate(epman_at_18 = dplyr::if_else(
        # Se D1a ou D2a e presença de sintomatologia atual
        (kmini19 == 1 | kmini21 == 1) & (kmini20 == 1 | kmini22 == 1),
        dplyr::if_else(
            # Se D1a = "Sim"
            kmini19 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((kmini30 == 1 & kmini32 >= 1) | kmini31 == 1,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((kmini30 == 1 & kmini32 >= 1) | kmini31 == 1,
                        1, 0, missing = 0), 0, missing = 0)
        ), 0,
                missing = 0
    ))

# Current Maniac Episode (22y.)

da <- da |>
    dplyr::mutate(epman_at_22 = dplyr::if_else(
        # Se D1a ou D2a e presença de sintomatologia atual
        (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
        dplyr::if_else(
            # Se D1a = "Sim"
            l081 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((l092 == 1 & l094 >= 1) | l093 == 1,
                        1, 0, missing = 0), 0, missing = 0)
        ), 0,
                missing = 0
    ))

# Past Hipomaniac Episode (18y.)

da <- da |>
    dplyr::mutate(ephip_pa_18 = dplyr::if_else(
        (kmini19 == 1 | kmini21 == 1) & (kmini20 == 0 & kmini22 == 0),
        # Presença de D1a ou D2a e ausência de sintomatologia atual
        dplyr::if_else(
            # Se D1a = "Sim"
            kmini19 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                # Se D4 é cotada como "Não"
                dplyr::if_else((kmini30 == 0 | kmini32 >= 0) & kmini31 == 0,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                # Se D4 é cotada como "Não"
                dplyr::if_else((kmini30 == 0 | kmini32 >= 0) & kmini31 == 0,
                        1, 0, missing = 0), 0, missing = 0)
        ), 0,
                missing = 0
    ))

# Past Hipomaniac Episode (22y.)

da <- da |>
    dplyr::mutate(ephip_pa_22 = dplyr::if_else(
        (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
        # Presença de D1a ou D2a e ausência de sintomatologia atual
        dplyr::if_else(
            # Se D1a = "Sim"
            l081 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                # Se D4 é cotada como "Não"
                dplyr::if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                # Se D4 é cotada como "Não"
                dplyr::if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0, missing = 0), 0, missing = 0)
        ), 0,
        missing = 0
    ))

# Current Hipomaniac Episode (18y.)

da <- da |>
    dplyr::mutate(ephip_at_18 = dplyr::if_else(
        (kmini19 == 1 | kmini21 == 1) & (kmini20 == 1 | kmini22 == 1),
        # Presença de D1a ou D2a e presença de sintomatologia atual
        dplyr::if_else(
            l082 == 1, # Testando se D1b = "Sim"
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                # Se D4 é cotada como "Não"
                dplyr::if_else((kmini30 == 0 | kmini32 >= 0) & kmini31 == 0,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((kmini30 == 0 | kmini32 >= 0) & kmini31 == 0,
                        0, 1), 0, missing = 0)
        ), 0,
        missing = 0
    ))

# Current Hipomaniac Episode (22y.)

da <- da |>
    dplyr::mutate(ephip_at_22 = dplyr::if_else(
        (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
        # Presença de D1a ou D2a e presença de sintomatologia atual
        dplyr::if_else(
            l082 == 1, # Testando se D1b = "Sim"
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                # Se D4 é cotada como "Não"
                dplyr::if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        1, 0, missing = 0), 0, missing = 0),
            dplyr::if_else(
                # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                # Se D4 é cotada como "Sim"
                dplyr::if_else((l092 == 0 | l094 >= 0) & l093 == 0,
                        0, 1), 0, missing = 0)
            ), 0,
        missing = 0
    ))

# Depressive Disorder Episode Diagnosis

# Mini (DSM-IV):

# Major Depressive Disorder is characterized by one or more Major Depressive Episodes
# (at least 2 weeks of depressed mood or loss of interest
# accompanied by at least four additional symptoms of depression).

# DSM-IV:

# A.Presence of a single Major Depressive Episode

# B.The Major Depressive Episode is not better accounted for by Schizoaffective
# Disorder and is not superimposed on Schizophrenia, Schizophreniform
# Disorder, Delusional Disorder, or Psychotic Disorder Not Otherwise Specified

# C. There has never been a Manic Episode, a Mixed Episode or a Hypomanic Episode

#da <- da |>
#    mutate(
#        diagtdm_22 = if_else(
#            (eptdm_22 == 1) & (epman_pa_22 + epman_at_22 + ephip_at_22 + ephip_pa_22 == 0),
#            1, 0)
#        )

# Excluding people with BD1 or BD2 at 18 years old

da_without_bd_at_18 <-
da |>
    dplyr::mutate(
        bd_at_18 = dplyr::if_else(
            # BD type I at 18 yo
            (epman_pa_18 == 1 | epman_at_18 == 1) |
                # BD type II at 18 yo
                ((ephip_pa_18 == 1 | ephip_at_18 == 1) & eptdm_18 == 1), 1, 0,
            missing = 0)
        ) |>
    dplyr::filter(bd_at_18 == 0)

# Bipolar I Disorder Diagnosis

# Mini (DSM-IV):

# Is characterized by one or more Manic or Mixed Episodes,
#usually accompanied by Major Depressive Episodes.

# DSM-IV: (Single Manic Episode)

# A. Presence of only one Manic Episode and no past Major Depressive Episodes.

# B. The Manic Episode is not better accounted for by Schizoaffective
# Disorder and is not superimposed on Schizophrenia, Schizophreniform
# Disorder, Delusional Disorder, or Psychotic Disorder Not Otherwise
# Specified.

#da_22 <- da_22 |>

da_without_bd_at_18 <- da_without_bd_at_18 |>
    dplyr::mutate(
        bd1_22 = dplyr::if_else(
                epman_at_22 == 1 | epman_pa_22 == 1,
                1, 0,
                missing = 0),
        bd2_22 = dplyr::if_else(
            (bd1_22 == 0) &
                ((eptdm_18 == 1 | eptdm_22 == 1) &
                     (ephip_pa_18 == 1 | ephip_pa_22 == 1 | ephip_at_18 == 1 |
                          ephip_at_22 == 1)
                 ), 1, 0,
            missing = 0
        ),
        bd_any = relevel(as.factor(dplyr::if_else(bd1_22 == 1 | bd2_22 == 1, "Yes", "No")), ref = "No"
            ))

da_without_bd_at_18 |>
    dplyr::count(bd_any)

# mania_18 == 1 | (hipo_18 == 1 & dep_18 == 1)

# Checar missings nas variáveis de episódio

da_without_bd_at_18 |>
    dplyr::select(dplyr::matches("^ep.*$")) |>
    purrr::map(\(x) table(x, useNA = "always"))

# Bipolar II Disorder Diagnosis

# Mini (DSM-IV):

#is characterized by one or more Major Depressive Episodes
#accompanied by at least one Hypomanic Episode.

# DSM-IV:

# A. Presence (or history) of one or more Major Depressive Episodes

# B. Presence (or history) of at least one Hypomanic Episode

# C. There has never been a Manic Episode or a Mixed Episode

# D. The mood symptoms in Criteria A and B are not better accounted for
# by Schizoaffective Disorder and are not superimposed on Schizophrenia,
# Schizophreniform Disorder, Delusional Disorder, or Psychotic Disorder
# Not Otherwise Specified.

# E. The symptoms cause clinically significant distress or impairment in
# social, occupational, or other important areas of functioning.

# tem que ter caso de tdm e hipo mas nao de mania

## PARA VERIFICAR NA BASE JÁ FILTRADA O DIAGNÓSTICO DE BD (tanto 1 quanto 2):
## Neste código, podemos fazer algo do tipo
## SE (MANIA_22 == 1 | ((HIPO_18 == 1 OU HIPO_22 == 1) E (EPDEP_22 == 1 OU EPDEP_18 == 1))), SIM
## Assim acima daria certo pois como tu filtrou lá em cima a base,
## Não teria o risco de alguém fechar por HIPO_18 == 1 E EPDEP_18 == 1,
## essa pessoa já teria caído fora
## Se quisesse pegar apenas BD2, seria tirar essa primeira parte de MANIA_22 == 1

## Tem que ser checado alguns NA que podem estar atrapalhando
## a lógica das tuas condicionais.

#da_22 <- da_22 |>
#    mutate(
#        diagbd2_22 = if_else(
#            ((eptdm_18 == 1 & ephip_at_22 == 1) | (eptdm_22 == 1 & ephip_at_18 == 1) |
#                (eptdm_18 == 1 & ephip_pa_22 == 1) | (eptdm_22 == 1 & ephip_pa_18 == 1)) &
#                (epman_pa_18 == 0 & epman_at_18 == 0 & epman_pa_22 == 0 & epman_at_22 == 0), 1, 0)
#        )

## ANALYSIS

teste <- da_without_bd_at_18 |>
    dplyr::mutate(
        dplyr::across(dplyr::starts_with("kc"),
                      \(x) dplyr::case_when(
                          x %in% 1:5 ~ "Yes",
                          x == 0 ~ "No",
                          x == 9 ~ NA_character_,
                          TRUE ~ NA_character_
                      ))
    )

set.seed(123)
chisq.test(as.factor(da_without_bd_at_18$bd_any), da_without_bd_at_18$kc05, simulate.p.value = TRUE)

teste |>
    dplyr::group_by(kc05) |>
    dplyr::count(bd_any)

janitor::tabyl(teste, bd_any, kc05)
descr::CrossTable(teste$kc05, teste$bd_any, expected = TRUE, prop.c = TRUE,
                  prop.chisq = TRUE, prop.t = TRUE)


ggplot2::ggplot(teste |> dplyr::filter(!is.na(kc05)), ggplot2::aes(x = kc05, fill = bd_any)) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Uso de maconha ao longo da vida",
                  y = "Proporção",
                  fill = "TB aos 22 anos")

mod <- stats::glm(bd_any ~ as.factor(kc05), family = stats::binomial, data = teste)

mod |>
    summary()

mod$coefficients |> exp()


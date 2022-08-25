library(dplyr)
library(haven)

#Carregando dados

coorte_teste <- read_sav(file.choose())

# Selecting variables

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

# Major Depressive Disorder Episode

coorte_teste_md <- coorte_teste_md |>
    mutate(
        eptdm_22 = if_else(
# Se A1 e A2 = "Sim"
            l063 == 1 & l064 == 1,
            if_else(
# Se há pelo menos 3 "Sim" em A3
                l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 3,
                1, 0
            ), if_else(
# Se A1 ou A2 = "Sim"
                l063 == 1 | l064 == 1,
                if_else (
# Se há pelo menos 4 "Sim" em A3
                    l065 + l066 + l067 + l068 + l069 + l070 + l071 + l072 + l074 >= 4,
                    1, 0
                ), 0
            )
        )
    )

# Past Maniac Episode

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

# Episódio Maníaco Atual

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

# Past Hipomaniac Episode

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

# Episódio Hipomaníaco Atual

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

coorte_teste_md <- coorte_teste_md |>
    mutate(
        diagtdm_22 = if_else(
            (eptdm_22 == 1) & (epman_pa_22 + epman_at_22 + ephip_at_22 + ephip_pa_22 == 0),
            1, 0)
        )


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

coorte_teste_md <- coorte_teste_md |>
    mutate(
        diagbd1_22 = if_else(
            epman_at_22 == 1 | epman_pa_22 == 1,
            1, 0)
        )

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

coorte_teste_md <- coorte_teste_md |>
    mutate(
        diagbd2_22 = if_else(
            (ephip_at_22 == 1 | ephip_pa_22 == 1) &
                (eptdm_22 == 1) &
                (epman_at_22 == 0 & epman_pa_22 == 0),
            1, 0)
    )

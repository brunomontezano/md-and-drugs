library(dplyr)
library(haven)

#Carregando dados

coorte_teste <- read_sav(file.choose())

#Selecionando variáveis

vars <- c("sexonovo", "kc05", "kc06", "kc07", "kc08", "kc09",
          "kc10", "kc11", "kc12", "kc13", "kc14", "kc15",
          "kc16", "kc17", "l063", "l064", "l065", "l066",
          "l067", "l068", "l069", "l070", "l071", "l071",
          "l072", "l076", "l074")

coorte_teste_md <- coorte_teste |>
    select(
        any_of(vars)
    )

#Criando coluna para diagnóstico dos TH

coorte_teste_md <- coorte_teste_md |>
    mutate(
        tdm = 0,
        tb = 0
    )

#Diagnóstico de TDM

#Major Depressive Disorder is characterized by one or more Major Depressive Episodes
#(at least 2 weeks of depressed mood or loss of interest
#accompanied by at least four additional symptoms of depression).

teste_tdm <- coorte_teste_md

teste_tdm %>%
    if(l063 == 1 | l064 == 1) {
        tdm <- 1
    } else {
        tdm <- 0
    }

#Diagnóstico de Transtorno Bipolar I

#is characterized by one or more Manic or Mixed Episodes,
#usually accompanied by Major Depressive Episodes.

#Diagnóstico de Transtorno Bipolar II

#is characterized by one or more Major Depressive Episodes
#accompanied by at least one Hypomanic Episode.

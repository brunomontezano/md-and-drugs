##### IMPORTAÇÃO #####

# Ler dados a partir do arquivo em sav
dados <- haven::read_sav("data/banco_QI_15_07.sav")

##### SELECIONAR AMOSTRA E VARIÁVEIS #####

# Selecionar variáveis de interesse, como:
# Sexo, variáveis relacionadas ao uso de substâncias ao longo da vida,
# variáveis para diagnóstico de episódios de humor aos 18 e aos 22 anos
variaveis <- c(
    "sexonovo",
    # Sexo do recém-nascido (arrumada em agosto de 2015)
    "kc05",
    # Uso pessoal de maconha
    "kc06",
    # Uso pessoal de cocaína cheirada
    "kc07",
    # Uso pessoal de comprimidos para dormir ou ficar calmo
    "kc08",
    # Uso pessoal de comprimidos para ficar chapado ou ligado
    "kc09",
    # Uso pessoal de cocaína cheirada (mesma variável?)
    "kc10",
    # Uso pessoal de lança perfume ou loló
    "kc11",
    # Uso pessoal de heroína
    "kc12",
    # Uso pessoal de ecstasy
    "kc13",
    # Uso pessoal de pitico (maconha com crack)
    "kc14",
    # Uso pessoal de crack
    "kc15",
    # Uso pessoal de LSD ou ácido
    "kc16",
    # Uso pessoal de cola de sapateiro
    "kc17",
    # Uso pessoal de outra droga
    "l063",
    # Triste, desanimado ou deprimido (22 anos)
    "l064",
    # Sem gosto por nada, perda de interesse (22 anos)
    "l065",
    # Vontade de comer ou peso mudou muito (22 anos)
    "l066",
    # Teve problema com sono (22 anos)
    "l067",
    # Falou ou movimentou mais devagar ou agitou-se (22 anos)
    "l068",
    # Sentiu-se cansado ou sem energia (22 anos)
    "l069",
    # Sentiu-se sem valor ou culpado (22 anos)
    "l070",
    # Teve dificuldade de concentração ou tomada de decisão (22 anos)
    "l071",
    # Pensamentos ruins, que seria melhor morrer (22 anos)
    "l072",
    # Teve estes mesmos problemas em outros momentos por 15 dias ou mais (22 anos)
    "l076",
    # Quis fazer mal a si mesmo (22 anos)
    "l074",
    # Pensando na vida, estudos, trabalho, casa, família e amigos, problemas (22 anos)
    "l081",
    # Tão cheio de energia, pra cima, alegre demais que causou problema (22 anos)
    "l082",
    # Atualmente, sente-se alegre demais ou cheio de energia (22 anos)
    "l084",
    # Atualmente, sente-se sempre irritado (22 anos)
    "l085",
    # Teve sensação que era alguém muito importante (22 anos)
    "l086",
    # Menos necessidade de sono (22 anos)
    "l087",
    # Falou rápido ou sem parar (22 anos)
    "l088",
    # Pensamentos corriam muito rápido (22 anos)
    "l089",
    # Tão distraído que qualquer interrupção fazia perder (22 anos)
    "l090",
    # Agitado que os outros preocupavam-se (22 anos)
    "l091",
    # Queria fazer coisas que pareciam boas/tentadoras que não pensava (22 anos)
    "l092",
    # Comportamentos/sentimentos duraram uma semana ou mais? (22 anos)
    "l093",
    # Alguma vez foi hospitalizado pelos problemas? (22 anos)
    "l094",
    # Pensando nos estudos, família, trabalho, casa, problemas atrapalharam? (22 anos)
    "l083",
    # Alguma vez na vida, ficou vários dias, muito irritado sem ter motivo? (22 anos)
    "kmini1",
    # Últimos 15 dias, sentiu-se triste, desanimado, deprimido ou irritado (18 anos)
    "kmini2",
    # Últimos 15 dias, sentiu que não tinha mais gosto por nada (18 anos)
    "kmini3",
    # Vontade de comer ou peso mudou (18 anos)
    "kmini4",
    # Teve problema de sono (18 anos)
    "kmini5",
    # Falou ou se movimentou mais devagar ou ficou mais agitado (18 anos)
    "kmini6",
    # Sentiu-se cansado ou sem energia (18 anos)
    "kmini7",
    # Sentiu-se culpado ou sem valor (18 anos)
    "kmini8",
    # Teve dificuldade de concentrar ou tomar decisão (18 anos)
    "kmini9",
    # Teve várias vezes pensamentos ruins como pensar em morrer ou fazer mal a si mesmo (22 anos)
    "kmini19",
    # Alguma vez na vida sentiu tão cheio de energia que causou problema (18 anos)
    "kmini20",
    # Atualmente, sente-se alegre demais ou cheio de energia? (18 anos)
    "kmini21",
    # Alguma vez na vida ficou vários dias irritado sem motivo? (18 anos)
    "kmini22",
    # Atualmente te sente sempre irritado? (18 anos)
    "kmini23",
    # Teve sensação que era alguem muito importante ou podia fazer qualquer coisa (18 anos)
    "kmini24",
    # Menor necessidade de sono (18 anos)
    "kmini25",
    # Falou sem parar ou rápido que as pessoas não entendiam (18 anos)
    "kmini26",
    # Pensamento corria tão rápido que não acompanhava (18 anos)
    "kmini27",
    # Ficou distraída que qualquer interrupção te fazia esquecer (18 anos)
    "kmini28",
    # Tão agitado que os outros se preocupavam (18 anos)
    "kmini29",
    # Queria tanto fazer algo que não pensava nos riscos/problemas (18 anos)
    "kmini30",
    # Comportamentos/sentimentos duraram uma semana ou mais (18 anos)
    "kmini31",
    # Alguma vez foi hospitalizado pelos problemas? (18 anos)
    "kmini32"
    # Pensando na vida geral, como problemas prejudicaram (18 anos)
)

# Manter apenas avaliações com coleta nos módulos A (ep. depressivo) e
# D (ep. maníaco ou hipomaníaco) na avaliação aos 22 anos;
# Manter apenas as variáveis presentes no vetor acima descrito
dados_avaliados_aos_22 <- dados |>
    dplyr::filter(!is.na(l063) & !is.na(l081)) |>
    dplyr::select(dplyr::any_of(variaveis))

##### DIAGNOSTICAR EPISÓDIOS DE HUMOR #####

dados_avaliados_aos_22 <- dados_avaliados_aos_22 |>
    dplyr::mutate(
        # Episódio depressivo aos 18 anos
        eptdm_18 = dplyr::if_else(
            # Se A1 e A2 = "Sim"
            kmini1 == 1 & kmini2 == 1,
            dplyr::if_else(
                # Se há pelo menos 3 "Sim" em A3
                kmini3 + kmini4 + kmini5 + kmini6 + kmini7 + kmini8 + kmini9 >= 3,
                1,
                0,
                missing = 0
            ),
            dplyr::if_else(
                # Se A1 ou A2 = "Sim"
                kmini1 == 1 | kmini2 == 1,
                dplyr::if_else (
                    # Se há pelo menos 4 "Sim" em A3
                    kmini3 + kmini4 + kmini5 + kmini6 + kmini7 + kmini8 + kmini9 >= 4,
                    1,
                    0,
                    missing = 0
                ),
                0,
                missing = 0
            )
        ),
        # Episódio depressivo aos 22 anos
        eptdm_22 = dplyr::if_else(
            # Se A1 e A2 = "Sim"
            l063 == 1 & l064 == 1,
            dplyr::if_else(# Se há pelo menos 3 "Sim" em A3
                l065 + l066 + l067 + l068 + l069 + l070 + l071 >= 3,
                1, 0,
                missing = 0),
            dplyr::if_else(
                # Se A1 ou A2 = "Sim"
                l063 == 1 | l064 == 1,
                dplyr::if_else (# Se há pelo menos 4 "Sim" em A3
                    l065 + l066 + l067 + l068 + l069 + l070 + l071 >= 4,
                    1, 0,
                    missing = 0),
                0,
                missing = 0
            )
        ),
        # Episódio maníaco passado aos 18 anos
        epman_pa_18 = dplyr::if_else(
            # Se D1a ou D2a = "Sim" e ausência de sintomatologia atual
            (kmini19 == 1 |
                 kmini21 == 1) & (kmini20 == 0 & kmini22 == 0),
            dplyr::if_else(
                # Se D1a = "Sim"
                kmini19 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((kmini30 == 1 &
                                        kmini32 >= 1) |
                                       kmini31 == 1,
                                   1, 0),
                    0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((kmini30 == 1 &
                                        kmini32 >= 1) |
                                       kmini31 == 1,
                                   1, 0),
                    0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio maníaco passado aos 22 anos
        epman_pa_22 = dplyr::if_else(
            # Se D1a ou D2a = "Sim" e ausência de sintomatologia atual
            (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
            dplyr::if_else(
                # Se D1a = "Sim"
                l081 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((l092 == 1 &
                                        l094 >= 1) | l093 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((l092 == 1 &
                                        l094 >= 1) | l093 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio maníaco atual aos 18 anos
        epman_at_18 = dplyr::if_else(
            # Se D1a ou D2a e presença de sintomatologia atual
            (kmini19 == 1 |
                 kmini21 == 1) & (kmini20 == 1 | kmini22 == 1),
            dplyr::if_else(
                # Se D1a = "Sim"
                kmini19 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((kmini30 == 1 &
                                        kmini32 >= 1) |
                                       kmini31 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((kmini30 == 1 &
                                        kmini32 >= 1) |
                                       kmini31 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio maníaco atual aos 22 anos
        epman_at_22 = dplyr::if_else(
            # Se D1a ou D2a e presença de sintomatologia atual
            (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
            dplyr::if_else(
                # Se D1a = "Sim"
                l081 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((l092 == 1 &
                                        l094 >= 1) | l093 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((l092 == 1 &
                                        l094 >= 1) | l093 == 1,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio hipomaníaco passado aos 18 anos
        ephip_pa_18 = dplyr::if_else(
            (kmini19 == 1 | kmini21 == 1) & (kmini20 == 0 & kmini22 == 0),
            # Presença de D1a ou D2a e ausência de sintomatologia atual
            dplyr::if_else(
                # Se D1a = "Sim"
                kmini19 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((kmini30 == 0 |
                                        kmini32 >= 0) &
                                       kmini31 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((kmini30 == 0 |
                                        kmini32 >= 0) &
                                       kmini31 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio hipomaníaco passado aos 22 anos
        ephip_pa_22 = dplyr::if_else(
            (l081 == 1 | l083 == 1) & (l082 == 0 & l084 == 0),
            # Presença de D1a ou D2a e ausência de sintomatologia atual
            dplyr::if_else(
                # Se D1a = "Sim"
                l081 == 1,
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((l092 == 0 |
                                        l094 >= 0) & l093 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((l092 == 0 |
                                        l094 >= 0) & l093 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio hipomaníaco atual aos 18 anos
        ephip_at_18 = dplyr::if_else(
            (kmini19 == 1 | kmini21 == 1) & (kmini20 == 1 | kmini22 == 1),
            # Presença de D1a ou D2a e presença de sintomatologia atual
            dplyr::if_else(
                l082 == 1,
                # Testando se D1b = "Sim"
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 3,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((kmini30 == 0 |
                                        kmini32 >= 0) &
                                       kmini31 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    kmini23 + kmini24 + kmini25 + kmini26 + kmini27 + kmini28 + kmini29 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((kmini30 == 0 |
                                        kmini32 >= 0) &
                                       kmini31 == 0,
                                   0, 1),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        ),
        # Episódio hipomaníaco atual aos 22 anos
        ephip_at_22 = dplyr::if_else(
            (l081 == 1 | l083 == 1) & (l082 == 1 | l084 == 1),
            # Presença de D1a ou D2a e presença de sintomatologia atual
            dplyr::if_else(
                l082 == 1,
                # Testando se D1b = "Sim"
                dplyr::if_else(
                    # Se há pelo menos 3 "Sim" em D3 como D1a = "Sim"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 3,
                    # Se D4 é cotada como "Não"
                    dplyr::if_else((l092 == 0 |
                                        l094 >= 0) & l093 == 0,
                                   1, 0, missing = 0),
                    0,
                    missing = 0
                ),
                dplyr::if_else(
                    # Se há pelo menos 4 "Sim" em D3 como D1a = "Não"
                    l085 + l086 + l087 + l088 + l089 + l090 + l091 >= 4,
                    # Se D4 é cotada como "Sim"
                    dplyr::if_else((l092 == 0 |
                                        l094 >= 0) & l093 == 0,
                                   0, 1),
                    0,
                    missing = 0
                )
            ),
            0,
            missing = 0
        )
    )

##### REMOVER SUJEITOS QUE FECHARAM DIAGNÓSTICO DE TRANSTORNO BIPOLAR AOS 18 ANOS #####

# Nesta etapa, vamos remover todos os sujeitos que fecharam seu diagnóstico de
# transtorno bipolar aos 18 anos (apenas com episódios avaliados nos 18 anos) e
# removê-los, dado que o objetivo desta análise é avaliar a incidência aos 22
# anos, assim como verificar os preditores aos 18 anos para o desenvolvimento
# do transtorno
dados_sem_tb_aos_18 <-
    dados_avaliados_aos_22 |>
    dplyr::mutate(tb_aos_18 = dplyr::if_else(
        # Transtorno bipolar tipo I aos 18 anos
        (epman_pa_18 == 1 | epman_at_18 == 1) |
            # Transtorno bipolar tipo II aos 18 anos
            ((ephip_pa_18 == 1 |
                  ephip_at_18 == 1) & eptdm_18 == 1),
        1,
        0,
        missing = 0
    )) |>
    dplyr::filter(tb_aos_18 == 0) |>  # Manter sujeitos que não fecharam TB aos 18 anos
    dplyr::select(-dplyr::matches("^(km|l0).*$")) # Remover variáveis da MINI

##### CRIAR DIAGNÓSTICOS DE TRANSTORNO BIPOLAR AOS 22 ANOS #####

dados_com_diagnostico <- dados_sem_tb_aos_18 |>
    dplyr::mutate(
        # Transtorno bipolar tipo I aos 22 anos
        bd1_22 = as.factor(
            dplyr::if_else(epman_at_22 == 1 | epman_pa_22 == 1,
                           "Yes", "No",
                           missing = "No")
        ),
        # Transtorno bipolar tipo II aos 22 anos
        bd2_22 = as.factor(dplyr::if_else((bd1_22 == 0) &
                                              ((eptdm_18 == 1 |
                                                    eptdm_22 == 1) &
                                                   (
                                                       ephip_pa_18 == 1 | ephip_pa_22 == 1 | ephip_at_18 == 1 |
                                                           ephip_at_22 == 1
                                                   )
                                              ), "Yes", "No",
                                          missing = "No")),
        # Diagnóstico de transtorno bipolar (tipo I ou tipo II) aos 22 anos
        bd_any = relevel(as.factor(
            dplyr::if_else(bd1_22 == "Yes" |
                               bd2_22 == "Yes", "Yes", "No")
        ), ref = "No"),
        # "No" como categoria de referência para regressão
        # Recodificar as variáveis de substâncias para dicotômica
        dplyr::across(dplyr::starts_with("kc"),
                      \(x) as.factor(
                          dplyr::case_when(
                              x %in% 1:5 ~ "Yes",
                              x == 0 ~ "No",
                              x == 9 ~ NA_character_,
                              TRUE ~ NA_character_
                          )
                      )),
        sexonovo = as.factor(
            dplyr::case_when(
                sexonovo == 1 ~ "Male",
                sexonovo == 2 ~ "Female",
                TRUE ~ NA_character_
            )
        )
    ) |>
    # Remove variáveis dos episódios e do TB aos 18 anos
    dplyr::select(-dplyr::matches("^ep.*$"), -tb_aos_18) |>
    # Renomear algumas variáveis
    dplyr::rename(
        maconha = kc05,
        cocaina = kc06,
        comp_dormir = kc07,
        comp_ligado = kc08,
        cocaina2 = kc09,
        lanca_perfume = kc10,
        heroina = kc11,
        ecstasy = kc12,
        maconha_com_crack = kc13,
        crack = kc14,
        lsd = kc15,
        cola = kc16,
        outra_droga = kc17,
        sexo = sexonovo
    )

##### EXPORTAR DADOS LIMPOS #####

readr::write_rds(x = dados_com_diagnostico,
                 file = "data/clean_data.rds",
                 compress = "none")

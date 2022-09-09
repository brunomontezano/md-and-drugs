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

# Checar incidência de transtorno bipolar agrupada pela variável de uso
# de maconha
dados_limpos |>
    dplyr::filter(!is.na(maconha)) |>
    dplyr::group_by(maconha) |>
    dplyr::count(bd_any)


##### DATA VIS ######


##### IMPORTAR DADOS LIMPOS #####

# Ler dados a partir do arquivo exportado no script '01_data.R'
dados_limpos <- readr::read_rds("data/clean_data.rds")

# Dar uma olhada na estrutura
dados_limpos |>
    dplyr::glimpse()

pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
    } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g), simulate.p.value = TRUE)$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

dados_limpos <- readr::read_rds("data/clean_data.rds")

table1::label(dados_limpos$maconha) <- "Lifetime cannabis use"
table1::label(dados_limpos$cocaina) <- "Lifetime cocaine use"
table1::label(dados_limpos$sexo) <- "Sex"
table1::label(dados_limpos$cordapele) <- "Skin color"
table1::label(dados_limpos$renda_por_1000) <- "Family income"
table1::label(dados_limpos$ja_apanhou) <- "Beaten by parents"

table1::units(dados_limpos$renda_por_1000) <- "by R$ 1000"
table1::units(dados_limpos$maconha) <- "assessed at 18 years old"
table1::units(dados_limpos$cocaina) <- "assessed at 18 years old"
table1::units(dados_limpos$ja_apanhou) <- "assessed at 11 years old"

tabela_1 <- table1::table1(
    x = ~ maconha + cocaina + sexo + cordapele + renda_por_1000 +
        ja_apanhou | bd_any,
    data = dados_limpos |>
        dplyr::mutate(
            bd_any = dplyr::if_else(bd_any == "Yes",
                                    "Bipolar disorder",
                                    "General population")
        ),
    overall = FALSE,
    extra.col = list(`p-value` = pvalue)
)

tabela_1

# Restructure object
x <- attr(tabela_1, "obj")$contents
names(x) <- lapply(x, function(x){rownames(x)[[1L]]})
x <- lapply(x, function(x){x[-1L, ]})

# Use apa_table()
papaja::apa_table(x,
                  caption = "Descriptive table of participants with bipolar disorder and general population at 18-year evaluation.")


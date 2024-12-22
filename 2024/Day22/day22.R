#!/usr/bin/env r

# Titre : day22.R
# Auteur : Tanguy

##############
##  DAY 22  ##
##############


# Import data ------------------------------------------------------------------

secrets <- readLines(file.path("2024", "Day22", "secrets.txt"))
secrets_example <- readLines(file.path("2024", "Day22", "secrets_example.txt"))
secrets_example2 <- readLines(file.path("2024", "Day22", "secrets_example2.txt"))


# Déclaration fonction ---------------------------------------------------------

to_base2 <- function(n) {
    if (n == 0L) return(0L)
    if (n == 1L) return(1L)
    s <- NULL
    l <- floor(log(n) / log(2L)) + 1L
    for (j in 2L ** (rev(seq_len(l) - 1L))) {
        if (j <= n) {
            n <- n - j
            s <- c(1L, s)
        } else {
            s <- c(0L, s)
        }
    }
    return(s)
}

to_base10 <- function(s) {
    n <- sum(s * 2L ** (seq_along(s) - 1L))
    return(n)
}

xor_gate <- function(a, b) {
    delta_l <- length(a) - length(b)
    if (delta_l > 0L) b <- c(b, rep(0L, delta_l))
    if (delta_l < 0L) a <- c(a, rep(0L, -delta_l))

    return(xor(a, b))
}

mix <- function(a, n) {
    if (n > 0L) return(xor_gate(a, c(rep(0L, n), a)))
    if (n < 0L) return(xor_gate(a, a[-seq_len(-n)]))
}

prune <- function(a) {
    return(a[seq_len(min(length(a), 24L))])
}

process <- function(x) {
    next_secret <- x |>
        mix(6L) |>
        prune() |>
        mix(-5L) |>
        prune() |>
        mix(11L) |>
        prune()
    return(next_secret)
}

get_price_seq <- function(x, n) {
    prices <- x %% 10L
    x <- to_base2(x)
    for (k in seq_len(n)) {
        x <- process(x)
        prices <- c(prices, to_base10(x) %% 10L)
    }
    return(list(last_secret = to_base10(x), prices = prices))
}

solve_day22_part1 <- function(input_secrets) {
    data_secrets <- as.integer(input_secrets)
    sum_secrets <- data_secrets |>
        lapply(get_price_seq, n = 2000L) |>
        lapply(`[[`, "last_secret") |>
        do.call(what = c) |>
        sum()
    return(sum_secrets)
}

compute_prices_by_changes <- function(prices) {
    big_prices <- array(data = 0L, dim = rep(19L, 4L))

    for (price_series in prices) {
        one_price <- array(data = -1L, dim = rep(19L, 4L))
        changes <- diff(price_series) + 10L
        for (k in seq_len(length(price_series) - 4L)) {
            val <- one_price[changes[k],
                             changes[k + 1L],
                             changes[k + 2L],
                             changes[k + 3L]]
            if (val == -1L) {
                one_price[changes[k],
                          changes[k + 1L],
                          changes[k + 2L],
                          changes[k + 3L]] <- price_series[k + 4L]
            }
        }
        one_price[one_price == -1L] <- 0L
        big_prices <- big_prices + one_price
    }
    return(big_prices)
}

solve_day22_part2 <- function(input_secrets) {
    data_secrets <- as.integer(input_secrets)
    prices <- data_secrets |>
        lapply(get_price_seq, n = 2000L) |>
        lapply(`[[`, "prices")
    big_prices <- compute_prices_by_changes(prices)
    return(max(big_prices))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day22_part1(secrets_example)
solve_day22_part1(secrets)


## Part 2 ----------------------------------------------------------------------

solve_day22_part2(secrets_example2)
solve_day22_part2(secrets)

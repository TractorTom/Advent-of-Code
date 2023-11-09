# Titre : day11.R
# Auteur : Tanguy

##############
##  DAY 11  ##
##############


# Import data ------------------------------------------------------------------

monkey <- readLines("./2022/Day11/monkeys.txt")
monkey_example <- readLines("./2022/Day11/monkeys_example.txt")


# Déclaration fonction ---------------------------------------------------------

function_factory <- function(op, val) {
    if (op == "+") {
        val <- as.numeric(val)
        return(function(x) {
            return(x + val)
        })
    } else if (op == "*") {
        if (val == "old") {
            return(function(x) {
                return(x * x)
            })
        } else {
            val <- as.numeric(val)
            return(function(x) {
                return(x * val)
            })
        }
    }
}

compute_rules <- function(data_monkey) {
    item <- list()
    operation <- list()
    test <- c()
    test_false <- c()
    test_true <- c()

    nb_monkey <- (length(data_monkey) + 1) / 7
    for (monkey_k in seq_len(nb_monkey)) {
        index <- (monkey_k - 1) * 7
        item <- c(
            item, 
            list(substr(data_monkey[index + 2], 19, 50) |>
                     strsplit(", ") |>
                     unlist() |>
                     as.numeric())
        )
        operation <- c(operation, list(function_factory(
            op = substr(data_monkey[index + 3], 24, 24),
            val = substr(data_monkey[index + 3], 26, 50)
        )))
        test <- c(test, substr(data_monkey[index + 4], 22, 50) |> as.numeric())
        test_true <- c(test_true, substr(data_monkey[index + 5], 30, 50) |> as.numeric() + 1)
        test_false <- c(test_false, substr(data_monkey[index + 6], 31, 50) |> as.numeric() + 1)
    }

    return(list(
        op = operation, item = item,
        test_val = test, true = test_true,
        false = test_false, nb_monkey = nb_monkey
    ))
}

compute_list_item <- function(data_info) {
    items <- data.frame(
        id = seq_along(data_info$item |> do.call(what = c)),
        val = data_info$item |> do.call(what = c)
    )
    for (k in seq_len(data_info$nb_monkey)) {
        items <- cbind(items, items$val %% data_info$test_val[k])
    }
    colnames(items) <- c("id", "val", paste0("test", seq_len(data_info$nb_monkey)))
    items <- as.matrix(items |> dplyr::select(-c(id, val)))
    return(items)
}

solve_day11_part1 <- function(data_monkey) {
    # Initialisation
    info <- compute_rules(data_monkey)
    count_p <- rep(0, info$nb_monkey)
    round <- 0

    # Traitement
    while (round < 20) {
        for (k in seq_len(info$nb_monkey)) {
            if (!is.null(info$item[[k]])) {
                index <- (k - 1) * 7
                count_p[k] <- count_p[k] + length(info$item[[k]])

                for (item in info$item[[k]]) {
                    wl <- info$op[[k]](item) %/% 3

                    if (wl %% info$test_val[k] == 0) {
                        info$item[[info$true[k]]] <- c(info$item[[info$true[k]]], wl)
                    } else {
                        info$item[[info$false[k]]] <- c(info$item[[info$false[k]]], wl)
                    }
                }
                info$item[k] <- list(NULL)
            }
        }

        round <- round + 1
    }

    return((count_p |> sort(decreasing = TRUE))[1:2] |> prod())
}

solve_day11_part2 <- function(data_monkey) {
    # Initialisation
    info <- compute_rules(data_monkey)
    items <- compute_list_item(data_info = info)
    count_p <- rep(0, info$nb_monkey)
    round <- 0

    index <- 1
    for (k in seq_len(info$nb_monkey)) {
        info$item[[k]] <- seq(index, index + length(info$item[[k]]) - 1)
        index <- index + length(info$item[[k]])
    }

    # Traitement
    while (round < 10000) {
        for (k in seq_len(info$nb_monkey)) {
            if (!is.null(info$item[[k]])) {
                index <- (k - 1) * 7
                count_p[k] <- count_p[k] + length(info$item[[k]])

                items[info$item[[k]], ] <- apply(
                    X = items[info$item[[k]], , drop = FALSE],
                    MARGIN = 1,
                    FUN = \(x) info$op[[k]](x) %% info$test_val
                ) |> t()

                for (item in info$item[[k]]) {
                    # wl <- op(item)# %/% 3

                    if (items[item, k] == 0) {
                        info$item[[info$true[k]]] <- c(info$item[[info$true[k]]], item)
                    } else {
                        info$item[[info$false[k]]] <- c(info$item[[info$false[k]]], item)
                    }
                }
                info$item[k] <- list(NULL)
            }
        }

        round <- round + 1
    }

    return((count_p |> sort(decreasing = TRUE))[1:2] |> prod())
}


# Execution --------------------------------------------------------------------

solve_day11_part1(monkey_example)
solve_day11_part1(monkey)

solve_day11_part2(monkey_example)
solve_day11_part2(monkey)

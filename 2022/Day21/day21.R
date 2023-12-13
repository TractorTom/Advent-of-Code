# Titre : day21.R
# Auteur : Tanguy

##############
##  DAY 21  ##
##############


# Import data ------------------------------------------------------------------

monkey <- readLines("./2022/Day21/monkey.txt")
monkey_example <- readLines("./2022/Day21/monkey_example.txt")


# Déclaration fonction ---------------------------------------------------------

traitement_p1 <- function(riddle) {
    riddle |>
        strsplit(split = ": ") |>
        do.call(what = rbind) |>
        as.data.frame() |>
        dplyr::rename(nom = 1, formule = 2) |>
        dplyr::mutate(
            is.nb = dplyr::case_when(
                !is.na(as.numeric(formule)) ~ TRUE,
                TRUE ~ FALSE
            ),
            res = dplyr::case_when(
                is.nb ~ as.numeric(formule),
                TRUE ~ NA_real_
            ),
            not_done = TRUE
        )
}

op_fraction <- function(op, fr1, fr2) {
    num_1 <- fr1[1]
    denom_1 <- fr1[2]
    num_2 <- fr2[1]
    denom_2 <- fr2[2]

    if (op == "+") {
        num <- num_1 * denom_2 + num_2 * denom_1
        denom <- denom_1 * denom_2
    } else if (op == "-") {
        num <- num_1 * denom_2 - num_2 * denom_1
        denom <- denom_1 * denom_2
    } else if (op == "*") {
        num <- num_1 * num_2
        denom <- denom_1 * denom_2
    } else if (op == "/") {
        if (denom_1 * num_2 == 0) stop("On ne peut diviser par zero.")

        num <- num_1 * denom_2
        denom <- num_2 * denom_1
    }

    pgcd <- gmp::gcd(num, denom)
    num <- num %/% pgcd
    denom <- denom %/% pgcd
    return(c(num, denom))
}

op_complex <- function(op, val1, val2) {
    if (op == "+") {
        return(c(
            op_fraction("+", val1[1:2], val2[1:2]),
            op_fraction("+", val1[3:4], val2[3:4])
        ))
    } else if (op == "-") {
        return(c(
            op_fraction("-", val1[1:2], val2[1:2]),
            op_fraction("-", val1[3:4], val2[3:4])
        ))
    } else if (op == "*") {
        return(c(
            op_fraction(
                "-",
                op_fraction("*", val1[1:2], val2[1:2]),
                op_fraction("*", val1[3:4], val2[3:4])
            ),
            op_fraction(
                "+",
                op_fraction("*", val1[1:2], val2[3:4]),
                op_fraction("*", val1[3:4], val2[1:2])
            )
        ))
    } else if (op == "/") {
        if (val2[3] != 0) stop("On ne peut diviser par un nombre complexe.")
        return(c(
            op_fraction("/", val1[1:2], val2[1:2]),
            op_fraction("/", val1[3:4], val2[1:2])
        ))
    }
    stop("L'opération est inconnue.")
}

traitement_p2 <- function(riddle) {
    riddle <- riddle |>
        strsplit(split = ": ") |>
        do.call(what = rbind) |>
        as.data.frame() |>
        dplyr::rename(nom = 1, formule = 2) |>
        dplyr::mutate(
            formule = dplyr::case_when(
                nom == "root" ~ gsub(
                    x = formule, pattern = "+",
                    replacement = "-", fixed = TRUE
                ),
                TRUE ~ formule
            ),
            is.nb = !grepl(
                x = formule,
                pattern = c("\\+", "\\-", "\\*", "\\/") |>
                    paste0(collapse = "|")
            ),
            to_do = TRUE,
            is.nb_left = is.nb,
            is.nb_right = is.nb,
            left = formule |> strsplit(" ") |> vapply(FUN.VALUE = character(1), 
                                                      FUN = base::`[`, 1),
            right = formule |> strsplit(" ") |> vapply(FUN.VALUE = character(1), 
                                                       FUN = base::`[`, 3),
            op = formule |> strsplit(" ") |> vapply(FUN.VALUE = character(1), 
                                                    FUN = base::`[`, 2),
            value = dplyr::case_when(
                nom == "humn" ~ list(c(0, 1, 1, 1)),
                TRUE ~ list(c(rep(NA_real_, 4)))
            )
        )

    for (monk in seq_len(nrow(riddle))) {
        if (riddle$is.nb[monk] && riddle$nom[monk] != "humn") {
            val <- as.numeric(riddle$formule[monk])
            riddle$value[monk] <- list(c(val, 1, 0, 1))
        }
    }

    return(riddle)
}

solve_day21_part1 <- function(riddle) {
    riddle <- traitement_p1(riddle)
    nb <- nrow(riddle)

    while (isFALSE(riddle$is.nb[riddle$nom == "root"])) {
        for (k in seq_len(nb)) {
            if (riddle$is.nb[k] && riddle$not_done[k]) {
                nomk <- riddle$nom[k]
                val <- riddle$res[k]

                riddle <- riddle |>
                    dplyr::mutate(
                        formule = gsub(
                            x = formule,
                            pattern = nomk,
                            replacement = val
                        )
                    )
                riddle$not_done[k] <- FALSE
            }
        }

        for (k in seq_len(nb)) {
            if (riddle$not_done[k]
                && class(try(expr = eval(parse(text = riddle$formule[k])),
                             silent = TRUE)) != "try-error") {
                riddle$res[k] <- eval(parse(text = riddle$formule[k]))
                riddle$is.nb[k] <- TRUE
            }
        }
    }

    return(riddle[riddle$nom == "root", "res"])
}

solve_day21_part2 <- function(riddle) {
    riddle <- riddle |> traitement_p2()

    while (!riddle$is.nb[riddle$nom == "root"]) {
        monkey_to_do <- riddle$nom[riddle$to_do & riddle$is.nb]
        for (monk in monkey_to_do) {
            riddle$is.nb_left[riddle$left == monk] <- TRUE
            riddle$is.nb_right[riddle$right == monk] <- TRUE
        }
        riddle$to_do[riddle$to_do & riddle$is.nb] <- FALSE

        index_to_compute <- which(!riddle$is.nb)
        for (index_monk in index_to_compute) {
            if (all(riddle[index_monk, c("is.nb_left", "is.nb_right")])) {
                l_monkey <- riddle[index_monk, "left"]
                r_monkey <- riddle[index_monk, "right"]
                op <- riddle[index_monk, "op"]

                left_val <- riddle$value[riddle$nom == l_monkey] |> unlist()
                right_val <- riddle$value[riddle$nom == r_monkey] |> unlist()

                val <- op_complex(op, left_val, right_val)
                riddle$value[index_monk] <- list(val)
                riddle$is.nb[index_monk] <- TRUE
            }
        }
    }
    root <- riddle$value[riddle$nom == "root"] |> unlist()
    return(-(root[1] * root[4]) %/% (root[2] * root[3]))
}

# Execution --------------------------------------------------------------------

solve_day21_part1(riddle = monkey_example)
solve_day21_part1(riddle = monkey) |> dput()

solve_day21_part2(riddle = monkey_example)
solve_day21_part2(riddle = monkey) |> dput()

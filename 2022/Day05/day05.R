# Titre : day05.R
# Auteur : Tanguy

##############
##  DAY 05  ##
##############


# Import data -------------------------------------------------------------

cargo_crane_example <- readLines("./2022/Day05/cargo_crane_example.txt")
cargo_crane <- readLines("./2022/Day05/cargo_crane.txt")


# Déclaration fonction ----------------------------------------------------

create_crane_stack <- function(data_cargo_crane) {
    nb_col <- (nchar(data_cargo_crane[1]) + 1) / 4
    nb_row <- which(data_cargo_crane == "") - 2
    index_col <- 2 + (seq_len(nb_col) - 1) * 4

    return(
        data_cargo_crane[seq_len(nb_row)] |>
            strsplit("") |>
            do.call(what = rbind) |>
            base::`[`(, index_col) |>
            split(rep(seq_len(nb_col), each = nb_row)) |>
            lapply(FUN = \(x) x[x != " "])
    )
}

create_list_moves <- function(data_cargo_crane) {
    first_row <- which(data_cargo_crane == "") + 1
    return(
        data_cargo_crane[seq(first_row, length(data_cargo_crane))] |>
            as.data.frame() |>
            tidyr::separate(
                col = 1, sep = " ", convert = TRUE,
                into = c("instruction", "nb", NA, "col1", NA, "col2")
            )
    )
}


move_p1 <- function(data_crane_stack, nb, col1, col2) {
    one_move <- function(data_crane_stack, col1, col2) {
        val <- data_crane_stack[[col1]][1]
        data_crane_stack[[col1]] <- data_crane_stack[[col1]][-1]
        data_crane_stack[[col2]] <- c(val, data_crane_stack[[col2]])
        return(data_crane_stack)
    }

    if (nb == 0) {
        return(data_crane_stack)
    }
    return(
        data_crane_stack |>
            one_move(col1, col2) |>
            move_p1(nb - 1, col1, col2)
    )
}

move_p2 <- function(data_crane_stack, nb, col1, col2) {
    index <- seq_len(nb)
    val <- data_crane_stack[[col1]][index]

    data_crane_stack[[col1]] <- data_crane_stack[[col1]][-index]
    data_crane_stack[[col2]] <- c(val, data_crane_stack[[col2]])

    return(data_crane_stack)
}

solve_day05_part1 <- function(data_cargo_crane) {
    crane_stack <- create_crane_stack(data_cargo_crane)
    moves <- create_list_moves(data_cargo_crane)

    for (k in seq_len(nrow(moves))) {
        crane_stack <- crane_stack |>
            move_p1(
                nb = moves[k, "nb"],
                col1 = moves[k, "col1"],
                col2 = moves[k, "col2"]
            )
    }

    return(sapply(crane_stack, base::`[[`, 1) |> paste(collapse = ""))
}

solve_day05_part2 <- function(data_cargo_crane) {
    crane_stack <- create_crane_stack(data_cargo_crane)
    moves <- create_list_moves(data_cargo_crane)

    for (k in seq_len(nrow(moves))) {
        crane_stack <- crane_stack |>
            move_p2(
                nb = moves[k, "nb"],
                col1 = moves[k, "col1"],
                col2 = moves[k, "col2"]
            )
    }

    return(sapply(crane_stack, base::`[[`, 1) |> paste(collapse = ""))
}


# Execution ---------------------------------------------------------------

solve_day05_part1(cargo_crane_example)
solve_day05_part1(cargo_crane)

solve_day05_part2(cargo_crane_example)
solve_day05_part2(cargo_crane)

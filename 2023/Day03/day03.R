# Titre : day03.R
# Auteur : Tanguy

##############
##  DAY 03  ##
##############


# Import data -------------------------------------------------------------

schematic <- readLines("./2023/Day03/engine_schematic.txt")
schematic_example <- readLines("./2023/Day03/engine_schematic_example.txt")


# Déclaration fonction ----------------------------------------------------

init <- function(data_schematic) {
    n <- length(data_schematic)

    return(
        data_schematic |>
            strsplit("") |>
            unlist() |>
            matrix(nrow = n) |>
            t() |>
            rbind(".", ... = _, ".") |>
            cbind(".", ... = _, ".")
    )
}

get_adj <- function(data_schematic, line, col) {
    return(data_schematic[(line - 1):(line + 1), (col - 1):(col + 1)])
}

solve_day03_part1 <- function(data_games) {

    data_schematic <- init(data_games)
    n <- ncol(data_schematic) - 2

    somme <- 0
    number <- 0
    adjacent_number <- FALSE

    for (line in seq_len(n) + 1) {
        for (col in seq_len(n) + 1) {

            value <- data_schematic[line, col]

            if (value %in% 0:9) {

                value <- as.integer(value)
                number <- number * 10 + value
                adjacent_number <-
                    adjacent_number ||
                    any(!get_adj(data_schematic, line, col) %in% c(0:9, "."))

            } else {

                if (adjacent_number) {
                    somme <- somme + number
                }
                adjacent_number <- FALSE
                number <- 0
            }
        }

        if (adjacent_number) {
            somme <- somme + number
        }
        adjacent_number <- FALSE
        number <- 0
    }

    return(somme)
}

count_gears <- function(data_schematic, line, col) {

    count <- 0

    for (i in -1:1) {
        new_number <- TRUE
        for (j in -1:1) {
            value <- data_schematic[line + i, col + j]
            if (value %in% 0:9) {
                if (new_number) {
                    new_number <- FALSE
                    count <- count + 1
                }
            } else {
                new_number <- TRUE
            }
        }
    }

    return(count)
}

find_number <- function(data_schematic, line, col) {

    k <- 0
    while (data_schematic[line, col + k] %in% 0:9) {
        k <- k - 1
    }
    min_k <- k + 1

    k <- 0
    while (data_schematic[line, col + k] %in% 0:9) {
        k <- k + 1
    }
    max_k <- k - 1

    return(paste0(data_schematic[line, col + min_k:max_k], collapse = "") |>
               as.integer())
}

sum_gears <- function(data_schematic, line, col) {

    somme <- 1

    for (i in -1:1) {
        new_number <- TRUE
        for (j in -1:1) {
            value <- data_schematic[line + i, col + j]
            if (value %in% 0:9) {
                if (new_number) {
                    new_number <- FALSE
                    somme <- somme * find_number(data_schematic,
                                                 line + i, col + j)
                }
            } else {
                new_number <- TRUE
            }
        }
    }

    return(somme)
}

solve_day03_part2 <- function(data_schematic) {

    data_schematic <- init(data_schematic)
    n <- ncol(data_schematic) - 2

    somme <- 0

    for (line in seq_len(n) + 1) {
        for (col in seq_len(n) + 1) {

            value <- data_schematic[line, col]
            if (value == "*") {
                count <- count_gears(data_schematic, line, col)

                if (count > 1) {
                    somme <- somme + sum_gears(data_schematic, line, col)
                }
            }
        }
    }

    return(somme)
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day03_part1(schematic_example)
solve_day03_part1(schematic)


## Part 2 ------------------------------------------------------------------

solve_day03_part2(schematic_example)
solve_day03_part2(schematic)

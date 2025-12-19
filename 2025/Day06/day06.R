#!/usr/bin/env r

# Titre : day06.R
# Auteur : Tanguy

##############
##  DAY 06  ##
##############

# Import data ------------------------------------------------------------------

worksheet <- readLines(file.path("2025", "Day06", "problems.txt"))
worksheet_example <- readLines(file.path("2025", "Day06",
                                         "problems_example.txt"))


# Déclaration fonction ---------------------------------------------------------

read_programs <- function(raw_worksheet) {
    programs <- raw_worksheet |>
        strsplit(split = " ", fixed = TRUE) |>
        lapply(FUN = \(x) x[nzchar(x)]) |>
        do.call(what = rbind)
    return(programs)
}

solve_programs <- function(programs) {
    values <- programs[-nrow(programs), ] |> apply(FUN = as.double, 2L)
    multiplicative_problems <- programs[nrow(programs), ] == "*"
    values[, multiplicative_problems] <- log(values[, multiplicative_problems])
    values <- colSums(values)
    values[multiplicative_problems] <- round(exp(values[multiplicative_problems]))
    grand_total <- sum(values)

    return(grand_total)
}

solve_day06_part1 <- function(raw_worksheet) {
    programs <- read_programs(raw_worksheet)
    grand_total <- solve_programs(programs)
    return(grand_total)
}


# Execution --------------------------------------------------------------------

read_cephalopod_programs <- function(raw_worksheet) {
    nb_elt <- length(raw_worksheet)
    raw_programs <- raw_worksheet |>
        paste0(strrep(" ", max(nchar(raw_worksheet)) - nchar(raw_worksheet))) |>
        strsplit(split = "", fixed = TRUE)

    operator <- raw_programs[[nb_elt]]

    raw_programs <- do.call(what = rbind, raw_programs[-nb_elt])
    raw_programs[raw_programs == " "] <- ""
    raw_programs <- raw_programs |>
        apply(2L, paste0, collapse = "") |>
        as.double()

    return(list(numbers = raw_programs, operator = operator))
}

solve_day06_part2 <- function(raw_worksheet) {
    programs <- read_cephalopod_programs(raw_worksheet)
    operator <- programs$operator
    numbers <- programs$numbers

    grand_total <- 0.0
    id <- c(which(operator != " "), length(operator) + 2L)
    for (k in seq_len(length(id) - 1L)) {
        if (operator[id[k]] == "+") {
            value <- sum(numbers[id[k]:(id[k + 1L] - 2L)])
        } else if (operator[id[k]] == "*") {
            value <- prod(numbers[id[k]:(id[k + 1L] - 2L)])
        }
        grand_total <- grand_total + value
    }
    return(grand_total)
}


## Part 1 ----------------------------------------------------------------------

solve_day06_part1(worksheet_example)
solve_day06_part1(worksheet)

## Part 2 ----------------------------------------------------------------------

solve_day06_part2(worksheet_example)
solve_day06_part2(worksheet)

#!/usr/bin/env r

# Titre : day03.R
# Auteur : Tanguy

##############
##  DAY 03  ##
##############


# Import data ------------------------------------------------------------------

banks <- readLines(file.path("2025", "Day03", "joltage_rating.txt"))
banks_example <- readLines(file.path("2025", "Day03",
                                     "joltage_rating_example.txt"))


# Déclaration fonction ---------------------------------------------------------

init <- function(data_banks) {
    data_banks <- data_banks |>
        strsplit(split = "", fixed = TRUE) |>
        lapply(FUN = as.integer)
    return(data_banks)
}

find_highest_rating_wo_n <- function(bank, n) {
    index <- which.max(head(bank, length(bank) - n))
    return(index)
}

# bank is a vector of integer (from 0 to 9)
get_largest_joltage <- function(bank, digits = 2L) {
    joltage <- 0.0
    for (id_digit in rev(seq_len(digits) - 1L)) {
        index_hr <- find_highest_rating_wo_n(bank, id_digit)
        joltage <- joltage * 10.0 + bank[index_hr]
        bank <- bank[-seq_len(index_hr)]
    }
    return(joltage)
}

solve_day03_part1 <- function(data_banks) {
    output_joltage <- vapply(
        X = init(data_banks),
        FUN = get_largest_joltage,
        FUN.VALUE = double(1L)
    )
    return(sum(output_joltage))
}

solve_day03_part2 <- function(data_banks) {
    output_joltage <- vapply(
        X = init(data_banks),
        FUN = get_largest_joltage,
        digits = 12L,
        FUN.VALUE = double(1L)
    )
    return(sum(output_joltage))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day03_part1(banks_example)
solve_day03_part1(banks)

## Part 2 ----------------------------------------------------------------------

solve_day03_part2(banks_example)
solve_day03_part2(banks)

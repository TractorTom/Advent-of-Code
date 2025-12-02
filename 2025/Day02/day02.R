#!/usr/bin/env r

# Titre : day02.R
# Auteur : Tanguy

##############
##  DAY 02  ##
##############


# Import data ------------------------------------------------------------------

ID_ranges <- readLines(file.path("2025", "Day02", "ID.txt"))
ID_ranges_example <- readLines(file.path("2025", "Day02", "ID_example.txt"))


# Déclaration fonction ---------------------------------------------------------

read_ranges <- function(raw_ranges) {
    raw_ranges <- raw_ranges |>
        strsplit(split = ",", fixed = TRUE) |>
        do.call(what = c) |>
        strsplit(split = "-", fixed = TRUE)
    ranges <- NULL
    for (range_k in raw_ranges) {
        mini <- as.numeric(range_k[1L])
        maxi <- as.numeric(range_k[2L])
        if (nchar(mini) + 1L == nchar(maxi)) {
            ranges <- rbind(
                ranges,
                c(mini, 10L ** nchar(mini) - 1L),
                c(10L ** nchar(mini), maxi)
            )
        } else if (nchar(mini) == nchar(maxi)) {
            ranges <- rbind(ranges, c(mini, maxi))
        } else {
            stop("case not covered", call. = FALSE)
        }
    }
    return(ranges)
}

get_div <- function(digits) {
    if (digits %in% c(2L, 3L, 5L, 7L)) return(digits)
    if (digits == 1L) return(NULL)
    if (digits == 4L) return(2L)
    if (digits == 6L) return(c(2L, 3L))
    if (digits == 8L) return(2L)
    if (digits == 9L) return(3L)
    if (digits == 10L) return(c(2L, 5L))
    stop("digits not taken into account.", call. = FALSE)
}

repeat_number <- function(pattern, divisor) {
    len_pattern <- floor(log10(pattern)) + 1L
    number <- pattern * (1L - 10L ** (len_pattern * divisor)) / (1L - 10L ** (len_pattern))
    return(number)
}

compute_repeating_number <- function(mini, maxi, part1 = TRUE) {
    digits <- floor(log10(mini)) + 1L
    if (part1) {
        divisors <- 2L
    } else {
        divisors <- get_div(digits)
    }
    repeating_numbers <- NULL
    for (divisor in divisors) {
        len_pattern <- digits %/% divisor
        all_pattern <- seq(mini %/% 10L ** (digits - len_pattern),
                           maxi %/% 10L ** (digits - len_pattern))
        repeating_numbers <- c(repeating_numbers, repeat_number(all_pattern, divisor))
    }
    output <- repeating_numbers[repeating_numbers >= mini & repeating_numbers <= maxi]
    return(unique(output))
}

solve_day02_part1 <- function(raw_ranges) {
    ranges <- read_ranges(raw_ranges)
    repeating_number <- NULL
    for (k in seq_len(nrow(ranges))) {
        repeating_number <- c(
            repeating_number,
            compute_repeating_number(ranges[k, 1L], ranges[k, 2L], part1 = TRUE)
        )
    }
    return(sum(unique(repeating_number)))
}

solve_day02_part2 <- function(raw_ranges) {
    ranges <- read_ranges(raw_ranges)
    repeating_number <- NULL
    for (k in seq_len(nrow(ranges))) {
        print(k)
        print(ranges[k, ])
        print(compute_repeating_number(ranges[k, 1L], ranges[k, 2L], part1 = FALSE))
        repeating_number <- c(
            repeating_number,
            compute_repeating_number(ranges[k, 1L], ranges[k, 2L], part1 = FALSE)
        )
    }
    return(sum(unique(repeating_number)))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day02_part1(ID_ranges_example)
solve_day02_part1(ID_ranges)

## Part 2 ----------------------------------------------------------------------

solve_day02_part2(ID_ranges_example)
solve_day02_part2(ID_ranges)

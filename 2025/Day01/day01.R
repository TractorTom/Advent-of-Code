#!/usr/bin/env r

# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############

# Import data ------------------------------------------------------------------

rotations <- readLines(file.path("2025", "Day01", "rotations.txt"))
rotations_example <- readLines(file.path("2025", "Day01",
                                         "rotations_example.txt"))


# Déclaration fonction ---------------------------------------------------------

solve_day01_part1 <- function(data_rotations, offset = 50L) {
    direction <- substr(data_rotations, 1L, 1L)
    abs_value <- as.integer(substr(data_rotations, 2L, 50L))
    value <- ifelse(direction == "R", abs_value, -abs_value)
    positions <- cumsum(c(50L, value))
    password <- (positions %% 100L) == 0L
    return(sum(password))
}

solve_day01_part2 <- function(data_rotations, offset = 50L) {
    direction <- substr(data_rotations, 1L, 1L)
    abs_value <- as.integer(substr(data_rotations, 2L, 50L))
    value <- ifelse(direction == "R", abs_value, -abs_value)
    positions <- cumsum(c(50L, value))
    domain_circle <- diff(positions %/% 100L)
    password <- abs(domain_circle) +
        (direction == "L" & (positions[-1L] %% 100L) == 0L) -
        (direction == "L" & (positions[-length(data_rotations)] %% 100L) == 0L)

    return(sum(password))
}

# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day01_part1(rotations_example)
solve_day01_part1(rotations)

## Part 2 ----------------------------------------------------------------------

solve_day01_part2(rotations_example)
solve_day01_part2(rotations)

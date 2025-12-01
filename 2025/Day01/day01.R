#!/usr/bin/env r

# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############

# Import data ------------------------------------------------------------------

rotations <- readLines(file.path("2025", "Day01", "rotations.txt"))
rotations_example <- readLines(file.path("2025", "Day01", "rotations_example.txt"))


# Déclaration fonction ---------------------------------------------------------

solve_day01_part1 <- function(data_rotations, offset = 50) {
    direction <- substr(data_rotations, 1, 1)
    abs_value <- as.integer(substr(data_rotations, 2, 50))
    value <- ifelse(direction == "R", abs_value, -abs_value)
    positions <- cumsum(c(50, value))
    points <- (positions %% 100) == 0
    return(sum(points))
}

solve_day01_part2 <- function(data_rotations, offset = 50) {
    direction <- substr(data_rotations, 1, 1)
    abs_value <- as.integer(substr(data_rotations, 2, 50))
    value <- ifelse(direction == "R", abs_value, -abs_value)
    positions <- cumsum(c(50, value))
    domain_circle <- diff(positions %/% 100)
    points <- abs(domain_circle) +
        (direction == "L" & (positions[-1] %% 100) == 0) -
        (direction == "L" & (positions[-length(data_rotations)] %% 100) == 0)

    return(sum(points))
}

# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day01_part1(rotations_example)
solve_day01_part1(rotations)

## Part 2 ----------------------------------------------------------------------

solve_day01_part2(rotations_example)
solve_day01_part2(rotations)

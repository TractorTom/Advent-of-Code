#!/usr/bin/env r

# Titre : day11.R
# Auteur : Tanguy

##############
##  DAY 11  ##
##############


# Import data ------------------------------------------------------------------

stones <- readLines(file.path("2024", "Day11", "stones.txt"))
stones_example <- readLines(file.path("2024", "Day11", "stones_example.txt"))
stones_example2 <- readLines(file.path("2024", "Day11", "stones_example2.txt"))


# Déclaration fonction ---------------------------------------------------------

n_digits <- function(n) {
    return(floor(log(n) / log(10.0)) + 1L)
}

separate <- function(n) {
    middle <- 10L ** (n_digits(n) %/% 2L)
    return(c(n %/% middle, n %% middle))
}

next_stone <- function(stone) {
    if (stone == 0L) return(1L)
    l <- n_digits(stone)
    if (l %% 2L == 0L) return(separate(stone))
    return(stone * 2024L)
}

count_stones <- function(data_stones, steps) {

    values <- data_stones
    count <- rep(1.0, length(data_stones))

    for (nb_step in seq_len(steps)) {
        new_values <- new_count <- NULL
        for (id_stone in seq_along(values)) {
            next_values <- next_stone(values[id_stone])
            for (next_value in next_values) {
                if (next_value %in% new_values) {
                    pos <- which(new_values == next_value)
                    new_count[pos] <- new_count[pos] + count[id_stone]
                } else {
                    new_values <- append(next_value, new_values)
                    new_count <- append(count[id_stone], new_count)
                }
            }
        }
        values <- new_values
        count <- new_count
    }

    return(sum(count))
}

solve_day11_part1 <- function(data_stones) {
    data_stones <- data_stones |>
        strsplit(split = " ", fixed = TRUE) |>
        unlist() |>
        as.numeric()
    count <- count_stones(data_stones, steps = 25L)
    return(count)
}

solve_day11_part2 <- function(data_stones) {
    data_stones <- data_stones |>
        strsplit(split = " ", fixed = TRUE) |>
        unlist() |>
        as.numeric()
    count <- count_stones(data_stones, steps = 75L)
    return(count)
}

# Execution --------------------------------------------------------------------

withr::with_options(new = list(digits = 22L), code = {

    ## Part 1 ------------------------------------------------------------------

    solve_day11_part1(stones_example2)
    solve_day11_part1(stones)


    ## Part 2 ------------------------------------------------------------------

    solve_day11_part2(stones)

})

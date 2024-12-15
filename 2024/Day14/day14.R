#!/usr/bin/env r

# Titre : day14.R
# Auteur : Tanguy

##############
##  DAY 14  ##
##############


# Import data ------------------------------------------------------------------

robots <- readLines(file.path("2024", "Day14", "robots.txt"))
robots_example <- readLines(file.path("2024", "Day14", "robots_example.txt"))


# Déclaration fonction ---------------------------------------------------------

source("R/utils.R")

read_robots_data <- function(input_robots) {
    pattern <- "^p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)$"
    proto <- list(x_init = integer(),
                  y_init = integer(),
                  vx = integer(),
                  vy = integer())
    data_robots <- input_robots[input_robots != ""] |>
        extract_regex(pattern, proto)
    data_robots[["x"]] <- data_robots[["x_init"]] + 1L
    data_robots[["y"]] <- data_robots[["y_init"]] + 1L
    return(data_robots)
}

visualize <- function(data_robots, dimensions) {
    map <- matrix(".", nrow = dimensions[[2L]], ncol = dimensions[[1L]])
    for (k in seq_len(nrow(data_robots))) {
        map[data_robots[k, "y"], data_robots[k, "x"]] <- "#"
    }
    return(map)
}

teleport <- function(data_robots, dimensions, seconds) {
    data_robots[["x"]] <- (data_robots[["x_init"]] + data_robots[["vx"]] * seconds) %% dimensions[[1L]] + 1L
    data_robots[["y"]] <- (data_robots[["y_init"]] + data_robots[["vy"]] * seconds) %% dimensions[[2L]] + 1L

    return(data_robots)
}

solve_day14_part1 <- function(input_robots, dimensions) {
    data_robots <- read_robots_data(input_robots) |>
        teleport(dimensions, 100L)

    safety_factor <- prod(
        sum(data_robots[["x"]] <= dimensions[[1L]] %/% 2L
            & data_robots[["y"]] <= dimensions[[2L]] %/% 2L),
        sum(data_robots[["x"]] <= dimensions[[1L]] %/% 2L
            & data_robots[["y"]] >= 2L + dimensions[[2L]] %/% 2L),
        sum(data_robots[["x"]] >= 2L + dimensions[[1L]] %/% 2L
            & data_robots[["y"]] <= dimensions[[2L]] %/% 2L),
        sum(data_robots[["x"]] >= 2L + dimensions[[1L]] %/% 2L
            & data_robots[["y"]] >= 2L + dimensions[[2L]] %/% 2L)
    )

    return(safety_factor)
}

solve_day14_part2 <- function(input_robots, dimensions) {

    data_robots <- read_robots_data(input_robots)

    all_density <- NULL
    for (seconds in seq_len(prod(dimensions))) {
        data_robots <- data_robots |>
            teleport(dimensions, seconds)
        density_seconds <- (sum(data_robots[["y"]] + 2L * data_robots[["x"]] <= 100L) +
                                sum(data_robots[["y"]] - 2L * data_robots[["x"]] <= -100L)) / nrow(data_robots)
        all_density <- c(all_density, density_seconds)
    }
    names(all_density) <- seq_len(prod(dimensions))
    all_density <- sort(all_density)

    k <- 1L
    cond <- FALSE
    while (!cond) {
        seconds <- as.integer(names(all_density)[k])
        data_robots |>
            teleport(dimensions, seconds) |>
            visualize(dimensions) |>
            apply(MARGIN = 1L, FUN = paste0, collapse = "") |>
            cat(sep = "\n")
        print(seconds)
        cond <- tolower(readline()) %in% c("yes", "y", "ok", "o", "oui")
        k <- k + 1L
    }

    return(seconds)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day14_part1(robots_example, c(11L, 7L))
solve_day14_part1(robots, c(101L, 103L))


## Part 2 ----------------------------------------------------------------------

solve_day14_part2(robots, c(101L, 103L))

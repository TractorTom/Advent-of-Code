#!/usr/bin/env r

# Titre : day12.R
# Auteur : Tanguy

##############
##  DAY 12  ##
##############


# Import data ------------------------------------------------------------------

garden_plots <- readLines(file.path("2024", "Day12", "garden.txt"))
garden_plots_example <- readLines(file.path("2024", "Day12",
                                            "garden_example.txt"))
garden_plots_example2 <- readLines(file.path("2024", "Day12",
                                             "garden_example2.txt"))
garden_plots_example3 <- readLines(file.path("2024", "Day12",
                                             "garden_example3.txt"))
garden_plots_example4 <- readLines(file.path("2024", "Day12",
                                             "garden_example4.txt"))
garden_plots_example5 <- readLines(file.path("2024", "Day12",
                                             "garden_example5.txt"))


# Déclaration fonction ---------------------------------------------------------

create_map <- function(data_garden) {
    data_garden <- data_garden |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind) |>
        rbind(".", ... = _, ".") |>
        cbind(".", ... = _, ".")

    return(data_garden)
}

get_position_int <- function(pos, n) {
    return(pos[[1L]] + n * (pos[[2L]] - 1L))
}

get_position_vect <- function(pos, n) {
    return(c(row = 1L + (pos - 1L) %% n, col = 1L + (pos - 1L) %/% n))
}

get_neighboors <- function(pos, n) {
    return(c(pos - 1L, pos + 1L, pos - n, pos + n))
}

count_perimeters <- function(pos, map) {
    neighboors <- get_neighboors(pos, nrow(map))
    nb_sides <- sum(map[neighboors] != map[[pos]])
    return(nb_sides)
}

count_corners <- function(pos, map) {
    plant <- map[[pos]]

    pos_v <- get_position_vect(pos, nrow(map))
    zoom_map <- map[pos_v[[1L]] + -1L:1L, pos_v[[2L]] + -1L:1L]
    zoom_map[zoom_map != plant] <- "0"
    zoom_map[zoom_map == plant] <- "1"
    zoom_map <- as.integer(zoom_map)

    nb_corners <- 4L - 2L * sum(zoom_map[2L * 1L:4]) +
        2L * zoom_map[[2L]] * zoom_map[[4L]] +
        2L * zoom_map[[4L]] * zoom_map[[8L]] +
        2L * zoom_map[[2L]] * zoom_map[[6L]] +
        2L * zoom_map[[6L]] * zoom_map[[8L]] -
        zoom_map[[1L]] * zoom_map[[2L]] * zoom_map[[4L]] -
        zoom_map[[4L]] * zoom_map[[7L]] * zoom_map[[8L]] -
        zoom_map[[6L]] * zoom_map[[8L]] * zoom_map[[9L]] -
        zoom_map[[2L]] * zoom_map[[3L]] * zoom_map[[6L]]

    return(nb_corners)
}

get_garden <- function(pos, map) {
    plant <- map[[pos]]
    garden <- NULL
    to_check <- pos
    while (length(to_check) > 0L) {
        current <- to_check[[1L]]
        to_check <- to_check[-1L]
        garden <- c(garden, current)
        neighboors <- get_neighboors(current, nrow(map))
        new_to_check <- neighboors[!(neighboors %in% c(to_check, garden))
                                   & map[neighboors] == plant]
        to_check <- c(to_check, new_to_check)
    }
    return(garden)
}

compute_total_price <- function(data_garden, perimeter_function) {

    map <- create_map(data_garden)
    to_check <- seq_along(map)
    total_price <- 0L

    while (length(to_check) > 0L) {
        current <- to_check[[1L]]
        to_check <- to_check[-1L]
        if (map[[current]] != ".") {
            garden <- get_garden(current, map)
            perimeter <- vapply(
                X = garden,
                FUN = perimeter_function,
                map = map,
                FUN.VALUE = numeric(1L)
            ) |>
                sum()
            area <- length(garden)
            total_price <- total_price + area * perimeter
            to_check <- setdiff(to_check, garden)
        }
    }

    return(total_price)
}

solve_day12_part1 <- function(data_garden) {
    total_price <- compute_total_price(data_garden, count_perimeters)
    return(total_price)
}

solve_day12_part2 <- function(data_garden) {
    total_price <- compute_total_price(data_garden, count_corners)
    return(total_price)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day12_part1(garden_plots_example)
solve_day12_part1(garden_plots_example2)
solve_day12_part1(garden_plots_example3)
solve_day12_part1(garden_plots)


## Part 2 ----------------------------------------------------------------------

solve_day12_part2(garden_plots_example)
solve_day12_part2(garden_plots_example4)
solve_day12_part2(garden_plots_example5)
solve_day12_part2(garden_plots_example3)
solve_day12_part2(garden_plots)

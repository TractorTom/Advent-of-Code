#!/usr/bin/env r

# Titre : day14.R
# Auteur : Tanguy

##############
##  DAY 14  ##
##############


# Import data ------------------------------------------------------------------

robots <- readLines(file.path("2024", "Day14", "data.txt"))
robots_example <- readLines(file.path("2024", "Day14", "data_example.txt"))


# Déclaration fonction ---------------------------------------------------------

pre_treatment <- function(input_robots) {
    data_robots <- input_robots |>
        strsplit(split = " ", fixed = TRUE) |>
        lapply(substr, start = 3L, stop = 50L) |>
        lapply(\(x) x |> strsplit(split = ",", fixed = TRUE) |> unlist() |> as.numeric()) |>
        do.call(what = rbind) |>
        as.data.frame()
    data_robots[["x"]] <- data_robots[["V1"]] + 1L
    data_robots[["y"]] <- data_robots[["V2"]] + 1L

    return(data_robots)
}

visualize <- function(data_robots, dimensions) {
    map <- matrix(".", nrow = dimensions[[2L]], ncol = dimensions[[1L]])
    for (k in seq_len(nrow(data_robots))) {
        map[data_robots[k, "y"], data_robots[k, "x"]] <- "#"
    }
    return(map)
}

update_position <- function(data_robots, dimensions, seconds) {
    data_robots[["x"]] <- (data_robots[["V1"]] + data_robots[["V3"]] * seconds) %% dimensions[[1L]] + 1L
    data_robots[["y"]] <- (data_robots[["V2"]] + data_robots[["V4"]] * seconds) %% dimensions[[2L]] + 1L

    return(data_robots)
}

solve_day14_part1 <- function(input_robots, dimensions) {
    data_robots <- pre_treatment(input_robots) |>
        update_position(dimensions, 100L)

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

    data_robots <- pre_treatment(input_robots)

    all_density <- NULL
    for (seconds in seq_len(prod(dimensions))) {
        data_robots <- data_robots |>
            update_position(dimensions, seconds)
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
            update_position(dimensions, seconds) |>
            visualize(dimensions) |>
            apply(MARGIN = 1L, FUN = paste0, collapse = "") |>
            cat(sep = "\n")
        cond <- tolower(readline()) %in% c("yes", "y", "ok", "o", "oui")
        k <- k + 1L
    }

    return(seconds)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day14_part1(robots_example, c(7L, 11L))
solve_day14_part1(robots, c(101L, 103L))


## Part 2 ----------------------------------------------------------------------

solve_day14_part2(robots, c(101L, 103L))

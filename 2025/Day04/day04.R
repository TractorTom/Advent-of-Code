#!/usr/bin/env r

# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############


# Import data ------------------------------------------------------------------

rolls_map <- readLines(file.path("2025", "Day04", "rolls_map.txt"))
rolls_map_example <- readLines(file.path("2025", "Day04", "rolls_map_example.txt"))


# Déclaration fonction ---------------------------------------------------------

read_rolls_map <- function(raw_map) {
    map <- raw_map |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind) |>
        rbind(".", ... = _, ".") |>
        cbind(".", ... = _, ".")
    return(map)
}

get_neighboors_indexex <- function(id, n_row) {
    indexes <- c(
        id - n_row - 1L, id - 1L, id + n_row - 1L,
        id - n_row, id + n_row,
        id - n_row + 1L, id + 1L, id + n_row + 1L
    )
    return(indexes)
}

compute_rolls_adjacent_matrix <- function(map) {
    mat <- rep(0L, prod(dim(map)))

    neighboors_indexex <- get_neighboors_indexex(which(map == "@"), nrow(map))
    for (k in neighboors_indexex) {
        mat[k] <- mat[k] + 1L
    }
    return(mat)
}

forklifts_process <- function(map, part2 = FALSE) {
    mat_adj <- compute_rolls_adjacent_matrix(map)
    list_id <- which(map == "@" & mat_adj < 4L)
    accessed_rolls <- length(list_id)
    if (part2) {
        while (length(list_id) > 0L) {
            map[list_id] <- "."

            neighboors_indexex <- get_neighboors_indexex(list_id, nrow(map))
            for (k in neighboors_indexex) {
                mat_adj[k] <- mat_adj[k] - 1L
            }

            list_id <- which(map == "@" & mat_adj < 4L)
            accessed_rolls <- accessed_rolls + length(list_id)
        }
    }
    return(accessed_rolls)
}

solve_day04_part1 <- function(raw_map) {
    map <- read_rolls_map(raw_map)
    return(forklifts_process(map, part2 = FALSE))
}

solve_day04_part2 <- function(raw_map) {
    map <- read_rolls_map(raw_map)
    return(forklifts_process(map, part2 = TRUE))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day04_part1(rolls_map_example)
solve_day04_part1(rolls_map)

## Part 2 ----------------------------------------------------------------------

solve_day04_part2(rolls_map_example)
solve_day04_part2(rolls_map)

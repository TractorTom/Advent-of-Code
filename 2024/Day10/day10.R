#!/usr/bin/env r

# Titre : day10.R
# Auteur : Tanguy

##############
##  DAY 10  ##
##############


# Import data ------------------------------------------------------------------

map <- readLines(file.path("2024", "Day10", "lava.txt"))
map_example <- readLines(file.path("2024", "Day10", "lava_example.txt"))
map_example2 <- readLines(file.path("2024", "Day10", "lava_example2.txt"))
map_example3 <- readLines(file.path("2024", "Day10", "lava_example3.txt"))
map_example4 <- readLines(file.path("2024", "Day10", "lava_example4.txt"))
map_example5 <- readLines(file.path("2024", "Day10", "lava_example5.txt"))
map_example6 <- readLines(file.path("2024", "Day10", "lava_example6.txt"))
map_example7 <- readLines(file.path("2024", "Day10", "lava_example7.txt"))


# Déclaration fonction ---------------------------------------------------------

preprocessing <- function(data_map) {
    lava_matrix <- data_map |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind) |>
        rbind(".", ... = _, ".") |>
        cbind(".", ... = _, ".")
    lava_matrix[lava_matrix == "."] <- -1L
    lava_matrix <- matrix(as.numeric(lava_matrix), nrow = nrow(lava_matrix))
    return(lava_matrix)
}

matrix_passage <- matrix(c(0L, 1L, 1L, 0L, 0L, -1L, -1L, 0L), nrow = 2L)

get_scope <- function(pos, lava_matrix) {
    val <- lava_matrix[pos[[1L]], pos[[2L]]]
    if (val == 9L) return(pos)
    neighbors <- matrix_passage + pos
    all_path <- NULL
    for (k in seq_len(ncol(neighbors))) {
        val_neighboor <- lava_matrix[neighbors[1L, k], neighbors[2L, k]]
        if (val_neighboor == val + 1L) {
            all_path <- rbind(all_path,
                              get_scope(neighbors[, k], lava_matrix))
        }
    }
    return(all_path)
}

compute_trailhead <- function(lava_matrix, part = 1L) {
    positions <- which(lava_matrix == 0L, arr.ind = TRUE)
    scopes <- apply(
        X = positions,
        MARGIN = 1L,
        FUN = get_scope,
        lava_matrix = lava_matrix,
        simplify = FALSE
    )
    if (part == 1L) scopes <- scopes |> lapply(unique)
    nb_trailhead <- scopes |> lapply(nrow) |> unlist() |> sum()
    return(nb_trailhead)
}

solve_day10_part1 <- function(data_map) {
    lava_matrix <- preprocessing(data_map)
    sum_trailhead <- compute_trailhead(lava_matrix, part = 1L)
    return(sum_trailhead)
}

solve_day10_part2 <- function(data_map) {
    lava_matrix <- preprocessing(data_map)
    sum_trailhead <- compute_trailhead(lava_matrix, part = 2L)
    return(sum_trailhead)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day10_part1(map_example)
solve_day10_part1(map_example2)
solve_day10_part1(map_example3)
solve_day10_part1(map_example4)
solve_day10_part1(map_example5)
solve_day10_part1(map)


## Part 2 ----------------------------------------------------------------------

solve_day10_part2(map_example6)
solve_day10_part2(map_example3)
solve_day10_part2(map_example7)
solve_day10_part2(map_example5)
solve_day10_part2(map)

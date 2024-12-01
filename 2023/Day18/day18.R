# Titre : day18.R
# Auteur : Tanguy

##############
##  DAY 18  ##
##############


# Import data ------------------------------------------------------------------

lagoon <- read.table("./2023/Day18/lagoon.txt")
lagoon_example <- read.table("./2023/Day18/lagoon_example.txt", sep =  " ")


# Déclaration fonction ---------------------------------------------------------

find_vertices <- function(data_lagoon) {
    x <- data_lagoon$V2 * vapply(X = data_lagoon$V1,
                                 FUN.VALUE = integer(1),
                                 FUN = switch,
                                 "R" = 1L, "L" = -1L, "D" =, "U" = 0L)
    y <- data_lagoon$V2 * sapply(X = data_lagoon$V1,
                                 FUN.VALUE = integer(1),
                                 FUN = switch,
                                 "D" = 1L, "U" = -1L, "R" =, "L" = 0L)

    return(data.frame(cumsum(x), cumsum(y)))
}

compute_volume <- function(vertices) {
    cross_vertices <- cbind(vertices[-1, ], vertices[-nrow(vertices), ])
    crossed <- cross_vertices[, 1] * cross_vertices[, 4] -
        cross_vertices[, 2] * cross_vertices[, 3]
    return(abs(sum(crossed) / 2))
}

solve_day18_part1 <- function(data_lagoon) {
    vertices <- find_vertices(data_lagoon)
    return(compute_volume(vertices) + sum(data_lagoon$V2) / 2 + 1)
}

solve_day18_part2 <- function(data_lagoon) {
    data_lagoon$V1 <- substr(data_lagoon$V3, start = 8, stop = 8) |>
        vapply(FUN = switch,
               FUN.VALUE = character(1),
               "0" = "R", "1" = "D", "2" = "L", "3" = "U")
    data_lagoon$V2 <- substr(data_lagoon$V3, start = 3, stop = 7) |>
        strtoi(16L) |>
        as.double()

    vertices <- find_vertices(data_lagoon)
    return(compute_volume(vertices) + sum(data_lagoon$V2) / 2 + 1)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day18_part1(lagoon_example)
solve_day18_part1(lagoon)


## Part 2 ----------------------------------------------------------------------

solve_day18_part2(lagoon_example)
solve_day18_part2(lagoon) |> dput()

# Titre : day11.R
# Auteur : Tanguy

##############
##  DAY 11  ##
##############


# Import data ------------------------------------------------------------------

galaxy <- readLines("./2023/Day11/galaxy_map.txt")
galaxy_example <- readLines("./2023/Day11/galaxy_map_example.txt")


# Déclaration fonction ---------------------------------------------------------

init <- function(data_galaxy) {

    data_galaxy <- strsplit(data_galaxy, split = "") |> do.call(what = rbind)

    nb_galaxy <- sum(data_galaxy != ".")
    data_galaxy[data_galaxy != "."] <- seq_len(nb_galaxy)

    return(data_galaxy)
}

find_galaxy <- function(nb, data_galaxy) {

    n_row <- nrow(data_galaxy)
    pos <- which(data_galaxy == nb)
    x <- ((pos - 1) %% n_row) + 1
    y <- ((pos - 1) %/% n_row) + 1

    return(c(x, y))
}

get_all_dist <- function(nb_expansion, data_galaxy) {

    nb_galaxy <- sum(data_galaxy != ".")
    empty_rows <- which(apply(X = data_galaxy,
                              MARGIN = 1,
                              FUN = \(x) sum(x != ".") == 0))
    empty_cols <- which(apply(X = data_galaxy,
                              MARGIN = 2,
                              FUN = \(x) sum(x != ".") == 0))

    data_distance <- vapply(X = seq_len(nb_galaxy),
                            FUN = find_galaxy, data_galaxy,
                            FUN.VALUE = numeric(2))

    somme <- 0
    for (gk in seq_len(nb_galaxy - 1) + 1) {
        pos_gk <- data_distance[, gk]
        for (gj in seq_len(gk - 1)) {
            pos_gj <- data_distance[, gj]

            expansion <- (nb_expansion - 1) *
                (sum(empty_rows %in% seq(pos_gk[1], pos_gj[1])) +
                     sum(empty_cols %in% seq(pos_gk[2], pos_gj[2])))

            distance <- abs(pos_gk[1] - pos_gj[1]) +
                abs(pos_gk[2] - pos_gj[2]) +
                expansion

            somme <- somme + distance
        }
    }
    return(somme)
}

solve_day11_part1 <- function(data_galaxy) {
    data_galaxy <- init(data_galaxy)
    return(get_all_dist(nb_expansion = 2, data_galaxy))
}

solve_day11_part2 <- function(data_galaxy) {
    data_galaxy <- init(data_galaxy)
    return(get_all_dist(nb_expansion = 1000000, data_galaxy))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day11_part1(galaxy_example)
solve_day11_part1(galaxy)


## Part 2 ----------------------------------------------------------------------

solve_day11_part2(galaxy_example)
solve_day11_part2(galaxy)

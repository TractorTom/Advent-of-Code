# Titre : day20.R
# Auteur : Tanguy

##############
##  DAY 20  ##
##############

options(digits = 22)

# Import data ------------------------------------------------------------------

coordinates <- read.table("./2022/Day20/grove's_coordinates.txt")
coordinates_example <- read.table("./2022/Day20/grove's_coordinates_example.txt")


# Déclaration fonction ---------------------------------------------------------

get_n_suiv <- function(i, suiv, n) {
    n <- n %% (length(suiv) - 1)

    ind <- i
    for (k in seq_len(n)) {
        ind <- suiv[ind]
    }
    return(ind)
}

mix <- function(data_coordinates, n) {
    index_suivant <- seq_along(data_coordinates) + 1
    index_suivant[length(data_coordinates)] <- 1

    for (step in seq_len(n)) {
        for (k in seq_along(data_coordinates)) {
            index_suivant_k <- index_suivant[k]
            dep <- data_coordinates[k]
            s <- get_n_suiv(k, index_suivant, dep)

            if (s != k) {
                prec_k <- which(index_suivant == k)
                index_suivant[prec_k] <- index_suivant_k
                index_suivant[k] <- index_suivant[s]
                index_suivant[s] <- k
            }
        }
    }

    return(index_suivant)
}

get_grove_coordinate <- function(data_coordinates, index_suivant) {
    nb <- length(data_coordinates)
    mixed_file <- NULL
    ind <- index_suivant[which(data_coordinates == 0)]

    for (k in seq_len(nb)) {
        mixed_file <- c(mixed_file, data_coordinates[ind])
        ind <- index_suivant[ind]
    }

    return(mixed_file[((1000 - 1) %% nb + 1)] + mixed_file[((2000 - 1) %% nb + 1)] + mixed_file[((3000 - 1) %% nb + 1)])
}

solve_day20_part1 <- function(data_coordinates) {
    data_coordinates <- data_coordinates[, 1]
    index_suivant <- mix(data_coordinates, 1)
    return(get_grove_coordinate(data_coordinates, index_suivant))
}


solve_day20_part2 <- function(data_coordinates) {
    data_coordinates <- data_coordinates[, 1] * 811589153
    index_suivant <- mix(data_coordinates, 10)
    return(get_grove_coordinate(data_coordinates, index_suivant))
}


# Execution --------------------------------------------------------------------

solve_day20_part1(coordinates_example)
solve_day20_part1(coordinates)

solve_day20_part2(coordinates_example)
solve_day20_part2(coordinates)

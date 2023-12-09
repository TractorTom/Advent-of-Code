# Titre : day17.R
# Auteur : Tanguy

##############
##  DAY 17  ##
##############


# Import data ------------------------------------------------------------------

jet_pattern <- readLines("./2022/Day17/jet_pattern.txt")
jet_pattern_example <- readLines("./2022/Day17/jet_pattern_example.txt")


# Déclaration fonction ---------------------------------------------------------

# coord est la coordonnee du point en bas a gauche [y, x]
decale_bas <- function(playground, rock, coord) {
    if (coord[1] == 1) {
        return(FALSE)
    }

    for (y in seq_len(nrow(rock))) {
        for (x in seq_len(ncol(rock))) {
            if (rock[y, x] == "#"
                && playground[y + coord[1] - 1 - 1, x + coord[2] - 1] == "#") {
                return(FALSE)
            }
        }
    }

    return(TRUE)
}

# coord est la coordonnee du point en bas a gauche [y, x]
decale_gauche <- function(playground, rock, coord) {
    if (coord[2] == 1) {
        return(FALSE)
    }

    for (y in seq_len(nrow(rock))) {
        for (x in seq_len(ncol(rock))) {
            if (rock[y, x] == "#"
                && playground[y + coord[1] - 1, x + coord[2] - 1 - 1] == "#") {
                return(FALSE)
            }
        }
    }

    return(TRUE)
}

# coord est la coordonnee du point en bas a gauche [y, x]
decale_droite <- function(playground, rock, coord) {
    if (coord[2] + ncol(rock) - 1 == 7) {
        return(FALSE)
    }

    for (y in seq_len(nrow(rock))) {
        for (x in seq_len(ncol(rock))) {
            if (rock[y, x] == "#"
                && playground[y + coord[1] - 1, x + coord[2] - 1 + 1] == "#") {
                return(FALSE)
            }
        }
    }

    return(TRUE)
}

get_height <- function(data_jet, tot_rock) {
    nb_rock <- 0
    kinds_rock <- list(
        matrix(c("#", "#", "#", "#"), nrow = 1),
        matrix(c(".", "#", ".", "#", "#", "#", ".", "#", "."), nrow = 3),
        matrix(c("#", "#", "#", ".", ".", "#", ".", ".", "#"), nrow = 3,
               byrow = TRUE),
        matrix(c("#", "#", "#", "#"), ncol = 1),
        matrix(c("#", "#", "#", "#"), ncol = 2)
    )
    height <- 0
    playground <- matrix(".", ncol = 7, nrow = tot_rock * 3 + 10)
    index <- -1

    while (nb_rock < tot_rock) {
        rock <- kinds_rock[[(nb_rock %% 5) + 1]]
        coord <- c(height + 5, 3)

        while (decale_bas(playground, rock, coord)) {
            # Décalage vertical
            coord[1] <- coord[1] - 1

            # Décalage horizontal
            index <- index + 1
            sbl <- substr(x = data_jet,
                          start = (index %% nchar(data_jet)) + 1,
                          stop = (index %% nchar(data_jet)) + 1)

            if (sbl == "<" && decale_gauche(playground, rock, coord)) {
                coord[2] <- coord[2] - 1
            } else if (sbl == ">" && decale_droite(playground, rock, coord)) {
                coord[2] <- coord[2] + 1
            }
        }

        for (y in seq_len(nrow(rock))) {
            for (x in seq_len(ncol(rock))) {
                if (rock[y, x] == "#") {
                    playground[y + coord[1] - 1, x + coord[2] - 1] <- "#"
                }
            }
        }

        height <- max(height, coord[1] + nrow(rock) - 1)
        nb_rock <- nb_rock + 1
    }

    return(height)
}

get_freq <- function(data_jet) {
    nb_rock <- 0
    kinds_rock <- list(
        matrix(c("#", "#", "#", "#"), nrow = 1),
        matrix(c(".", "#", ".", "#", "#", "#", ".", "#", "."), nrow = 3),
        matrix(c("#", "#", "#", ".", ".", "#", ".", ".", "#"), nrow = 3,
               byrow = TRUE),
        matrix(c("#", "#", "#", "#"), ncol = 1),
        matrix(c("#", "#", "#", "#"), ncol = 2)
    )
    height <- 0
    playground <- matrix(".", ncol = 7, nrow = 10)
    index <- -1

    historique <- array(0, dim = c(length(kinds_rock), nchar(data_jet), 3))

    while (TRUE) {
        rock <- kinds_rock[[(nb_rock %% 5) + 1]]
        coord <- c(height + 5, 3)

        while (decale_bas(playground, rock, coord)) {
            # Décalage vertical
            coord[1] <- coord[1] - 1

            # Décalage horizontal
            index <- index + 1
            sbl <- substr(x = data_jet,
                          start = (index %% nchar(data_jet)) + 1,
                          stop = (index %% nchar(data_jet)) + 1)

            if (sbl == "<" && decale_gauche(playground, rock, coord)) {
                coord[2] <- coord[2] - 1
            } else if (sbl == ">" && decale_droite(playground, rock, coord)) {
                coord[2] <- coord[2] + 1
            }
        }

        for (y in seq_len(nrow(rock))) {
            for (x in seq_len(ncol(rock))) {
                if (rock[y, x] == "#") {
                    playground[y + coord[1] - 1, x + coord[2] - 1] <- "#"
                }
            }
        }

        height <- max(height, coord[1] + nrow(rock) - 1)
        nb_rock <- nb_rock + 1

        rock_i <- ((nb_rock - 1) %% 5) + 1
        index_i <- ((index - 1) %% nchar(data_jet)) + 1

        if (historique[rock_i, index_i, 1] == 0) {
            historique[rock_i, index_i, ] <- c(nb_rock, nb_rock, nb_rock)
        } else {
            periode <- historique[rock_i, index_i, 1]
            last <- historique[rock_i, index_i, 2]
            init <- historique[rock_i, index_i, 3]

            if (nb_rock - last == periode) {
                return(c(init, periode))
            } else {
                historique[rock_i, index_i, ] <-
                    c(nb_rock - last, nb_rock, last)
            }
        }

        if (nrow(playground) - height < 10) {
            playground <- rbind(
                playground,
                matrix(".", ncol = 7, nrow = nrow(playground))
            )
        }
    }
}

get_height2 <- function(data_jet, val) {
    res <- get_freq(data_jet)

    init <- res[1]
    periode <- res[2]
    reste <- (val - init) %% periode
    nb_periode <- (val - init) %/% periode

    height_init <- get_height(data_jet, init)
    height_periode <- get_height(data_jet, init + periode) - height_init
    heigh_end <- get_height(
        data_jet = data_jet,
        tot_rock = init + periode + reste
    ) - height_init - height_periode

    return(height_init + height_periode * nb_periode + heigh_end)
}

solve_day17_part1 <- function(data_jet) {
    return(get_height(data_jet, 2022))
}

solve_day17_part2 <- function(data_jet) {
    return(get_height2(data_jet, 1000000000000))
}


# Execution --------------------------------------------------------------------

solve_day17_part1(jet_pattern_example)
solve_day17_part1(jet_pattern)

solve_day17_part2(jet_pattern_example) |> dput()
solve_day17_part2(jet_pattern) |> dput()

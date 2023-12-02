# Titre : day11.R
# Auteur : Tanguy

##############
##  DAY 11  ##
##############

###### IMPORT DATA ######

energy_level <- readLines("./2021/Day11/energy_level.txt")
energy_level_example <- readLines("./2021/Day11/energy_level_example.txt")

###### TRAITEMENT DATA ######

energy_level_example <- matrix(as.numeric(do.call(rbind, strsplit(energy_level_example, ""))),
    nrow = length(energy_level_example)
)
energy_level <- matrix(as.numeric(do.call(rbind, strsplit(energy_level, ""))),
    nrow = length(energy_level)
)

###### DECLARATION FONCTION ######

compute_voisins <- function(x, y, dim_x = 10, dim_y = 10) {
    liste_voisins <- list()
    for (i in max(x - 1, 1):min(x + 1, dim_x)) {
        for (j in max(y - 1, 1):min(y + 1, dim_y)) {
            if (j != y || x != i) {
                liste_voisins <- c(liste_voisins, list(c(i, j)))
            }
        }
    }

    return(liste_voisins)
}

flashes <- function(dataEnergy) {
    dataEnergy <- dataEnergy + 1
    after_flash <- dataEnergy
    new_voisin <- sum(after_flash > 9)
    while (new_voisin > 0) {
        for (x in 1:10) {
            for (y in 1:10) {
                if (dataEnergy[x, y] > 9) {
                    voisins <- compute_voisins(x, y)
                    for (voisin in voisins) {
                        if (after_flash[voisin[1], voisin[2]] != 0) after_flash[voisin[1], voisin[2]] <- after_flash[voisin[1], voisin[2]] + 1
                    }
                    after_flash[x, y] <- 0
                }
            }
        }
        new_voisin <- sum(after_flash > 9)
        dataEnergy <- after_flash
    }

    return(after_flash)
}

solve_day11_part1 <- function(dataEnergy, n = 100) {
    nbr_0 <- 0
    for (k in seq_len(n)) {
        dataEnergy <- flashes(dataEnergy)
        val <- sum(dataEnergy == 0)
        nbr_0 <- nbr_0 + val
    }
    return(nbr_0)
}

solve_day11_part2 <- function(dataEnergy) {
    nbr_0 <- 0
    k <- 0
    while (any(dataEnergy != 0)) {
        dataEnergy <- flashes(dataEnergy)
        k <- k + 1
    }
    return(k)
}

###### EXECUTION ######

solve_day11_part1(energy_level_example)
solve_day11_part1(energy_level)

solve_day11_part2(energy_level_example)
solve_day11_part2(energy_level)

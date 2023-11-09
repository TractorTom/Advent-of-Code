# Titre : day09.R
# Auteur : Tanguy

##############
##  DAY 09  ##
##############

###### IMPORT DATA ######

heightmap <- readLines("./2021/Day09/heightmap.txt")
heightmap_example <- readLines("./2021/Day09/heightmap_example.txt")

###### TRAITEMENT DATA ######

heightmap_example <- matrix(as.numeric(do.call(rbind, strsplit(heightmap_example, ""))),
    nrow = length(heightmap_example)
)
heightmap <- matrix(as.numeric(do.call(rbind, strsplit(heightmap, ""))),
    nrow = length(heightmap)
)

###### DECLARATION FONCTION ######

compute_voisins <- function(x, y, dim_x, dim_y) {
    liste_voisins <- list()
    for (i in max(x - 1, 1):min(x + 1, dim_x)) {
        if (i != x) {
            liste_voisins <- c(liste_voisins, list(c(i, y)))
        }
    }
    for (j in max(y - 1, 1):min(y + 1, dim_y)) {
        if (j != y) {
            liste_voisins <- c(liste_voisins, list(c(x, j)))
        }
    }
    return(liste_voisins)
}

is_low_point <- function(dataHeightmap, x, y) {
    dim_x <- dim(dataHeightmap)[1]
    dim_y <- dim(dataHeightmap)[2]

    liste_voisins <- compute_voisins(x, y, dim_x, dim_y)
    for (voisin in liste_voisins) {
        if (dataHeightmap[x, y] >= dataHeightmap[voisin[1], voisin[2]]) {
            return(FALSE)
        }
    }
    return(TRUE)
}

find_low_points <- function(dataHeightmap) {
    dim_x <- dim(dataHeightmap)[1]
    dim_y <- dim(dataHeightmap)[2]

    low_points <- c()
    for (x in 1:dim_x) {
        for (y in 1:dim_y) {
            if (is_low_point(dataHeightmap, x, y)) {
                low_points <- c(low_points, list(c(x, y)))
            }
        }
    }
    return(low_points)
}

solve_day09_part1 <- function(dataHeightmap) {
    low_points <- find_low_points(dataHeightmap)
    somme <- do.call(sum, lapply(low_points, FUN = function(index) dataHeightmap[index[1], index[2]]))
    return(length(low_points) + somme)
}

compute_bassin <- function(dataHeightmap, x, y) {
    dim_x <- dim(dataHeightmap)[1]
    dim_y <- dim(dataHeightmap)[2]
    bassin_point <- list()
    new_point <- list(c(x, y))

    while (length(new_point) != 0) {
        bassin_point <- c(bassin_point, new_point)
        temp <- new_point
        new_point <- list()

        for (point in temp) {
            list_voisin <- compute_voisins(point[1], point[2], dim_x, dim_y)
            for (voisin in list_voisin) {
                if (dataHeightmap[voisin[1], voisin[2]] != 9 &&
                    dataHeightmap[voisin[1], voisin[2]] > dataHeightmap[point[1], point[2]] &&
                    (!list(voisin) %in% bassin_point) &&
                    (!list(voisin) %in% new_point)) {
                    new_point <- c(new_point, list(voisin))
                }
            }
        }
    }
    return(length(bassin_point))
}

solve_day09_part2 <- function(dataHeightmap) {
    low_points <- find_low_points(dataHeightmap)
    size_all_bassin <- c()

    for (low_point in low_points) {
        x <- low_point[1]
        y <- low_point[2]
        size_all_bassin <- c(size_all_bassin, compute_bassin(dataHeightmap, x, y))
    }

    big_bassins <- sort(size_all_bassin, decreasing = TRUE)
    return(big_bassins[1] * big_bassins[2] * big_bassins[3])
}

###### EXECUTION ######

solve_day09_part1(heightmap_example)
solve_day09_part1(heightmap)

solve_day09_part2(heightmap_example)
solve_day09_part2(heightmap)

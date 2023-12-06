# Titre : day18.R
# Auteur : Tanguy

##############
##  DAY 18  ##
##############


# Import data ------------------------------------------------------------------

lava_droplet <- read.table("./2022/Day18/lava_droplet.txt", sep = ",")
lava_droplet_example <- read.table(
    file = "./2022/Day18/lava_droplet_example.txt",
    sep = ","
)


# Déclaration fonction ---------------------------------------------------------

get_surface <- function(data_droplet) {
    data_droplet <- as.matrix(data_droplet)
    count <- 0

    for (k in seq_len(nrow(data_droplet))) {
        sub <- sweep(data_droplet, 2, data_droplet[k, ])
        val <- 6 - apply(sub, MARGIN = 1, \(x) (sum(abs(x)) == 1)) |> sum()

        count <- count + val
    }
    return(count)
}

init_cartography <- function(data_droplet) {
    min_x <- min(data_droplet$V1)
    min_y <- min(data_droplet$V2)
    min_z <- min(data_droplet$V2)

    map <- array(0, dim = c(
        max(data_droplet$V1) - min_x + 1,
        max(data_droplet$V2) - min_y + 1,
        max(data_droplet$V3) - min_z + 1
    ) + 2)

    for (cube in seq_len(nrow(data_droplet))) {
        map[
            data_droplet[cube, 1] - min_x + 1 + 1,
            data_droplet[cube, 2] - min_y + 1 + 1,
            data_droplet[cube, 3] - min_z + 1 + 1
        ] <- 1
    }

    return(map)
}

color_space <- function(starting, val, map) {
    pile <- list(starting)
    while (length(pile) > 0) {

        pos <- pile[[1]]
        pile <- pile[-1]

        x <- pos[1]
        y <- pos[2]
        z <- pos[3]

        if (map[x, y, z] == 0) {
            map[x, y, z] <- val

            if (x > 1) pile <- c(pile, list(c(x - 1, y, z)))
            if (y > 1) pile <- c(pile, list(c(x, y - 1, z)))
            if (z > 1) pile <- c(pile, list(c(x, y, z - 1)))
            if (x < dim(map)[1]) pile <- c(pile, list(c(x + 1, y, z)))
            if (y < dim(map)[2]) pile <- c(pile, list(c(x, y + 1, z)))
            if (z < dim(map)[3]) pile <- c(pile, list(c(x, y, z + 1)))
        }
    }

    return(map)
}

solve_day18_part1 <- function(data_droplet) {
    get_surface(data_droplet)
}

solve_day18_part2 <- function(data_droplet) {
    map <- init_cartography(data_droplet)
    map <- color_space(rep(1, 3), 2, map)

    map[map == 0] <- 1

    data_droplet_int <- map |>
        reshape2::melt() |>
        subset(value == 1) |>
        dplyr::select(-value) |>
        dplyr::rename(V1 = 1, V2 = 2, V3 = 3)

    return(get_surface(data_droplet_int))
}


# Execution --------------------------------------------------------------------

solve_day18_part1(lava_droplet_example)
solve_day18_part1(lava_droplet)

solve_day18_part2(lava_droplet_example)
solve_day18_part2(lava_droplet)

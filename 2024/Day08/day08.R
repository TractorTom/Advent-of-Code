#!/usr/bin/env r

# Titre : day08.R
# Auteur : Tanguy

##############
##  DAY 08  ##
##############


# Import data ------------------------------------------------------------------

antenna <- readLines(file.path("2024", "Day08", "antenna.txt"))
antenna_example <- readLines(file.path("2024", "Day08", "antenna_example.txt"))


# Déclaration fonction ---------------------------------------------------------

apply_data_care <- function(data_antenna) {
    matrix_antenna <- data_antenna |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind)
    return(matrix_antenna)
}

get_list_antenna <- function(data_antenna) {
    antenna <- unique(data_antenna[data_antenna != "."])
    return(antenna)
}

create_map_of_antinodes <- function(data_antenna, part) {

    matrix_antenna <- apply_data_care(data_antenna)
    all_antennas <- get_list_antenna(matrix_antenna)
    d <- dim(matrix_antenna)[[1L]]
    antinodes <- matrix_antenna

    for (a in all_antennas) {
        positions <- which(matrix_antenna == a)
        positions <- cbind(1L + ((positions - 1L) %% d),
                           1L + ((positions - 1L) %/% d))
        for (id_A in 2L:nrow(positions)) {
            A <- positions[id_A, ]
            for (id_B in seq_len(id_A - 1L)) {
                B <- positions[id_B, ]
                AB <- B - A
                C <- B
                D <- A

                if (part == 1L) {
                    C <- C + AB
                    D <- D - AB
                    if (all(C > 0L & C <= d)) {
                        antinodes[C[[1L]], C[[2L]]] <- "#"
                    }
                    if (all(D > 0L & D <= d)) {
                        antinodes[D[[1L]], D[[2L]]] <- "#"
                    }
                } else if (part == 2L) {
                    while (all(C > 0L & C <= d)) {
                        antinodes[C[[1L]], C[[2L]]] <- "#"
                        C <- C + AB
                    }
                    while (all(D > 0L & D <= d)) {
                        antinodes[D[[1L]], D[[2L]]] <- "#"
                        D <- D - AB
                    }
                }
            }
        }
    }

    return(sum(antinodes == "#"))
}

solve_day08_part1 <- function(data_antenna) {
    create_map_of_antinodes(data_antenna, part = 1L)
}

solve_day08_part2 <- function(data_antenna) {
    create_map_of_antinodes(data_antenna, part = 2L)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day08_part1(antenna_example)
solve_day08_part1(antenna)


## Part 2 ----------------------------------------------------------------------

solve_day08_part2(antenna_example)
solve_day08_part2(antenna)

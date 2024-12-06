#!/usr/bin/env r

# Titre : day06.R
# Auteur : Tanguy

##############
##  DAY 06  ##
##############


# Import data ------------------------------------------------------------------

map <- readLines(file.path("2024", "Day06", "map.txt"))
map_example <- readLines(file.path("2024", "Day06", "map_example.txt"))


# Déclaration fonction ---------------------------------------------------------

initialize_map <- function(data_map) {
    map <- data_map |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind)

    map[map == "^"] <- 0L
    map[map == "."] <- 0L
    map[map == "#"] <- -1L
    map <- map |>
        rbind(-2L, ... = _, -2L) |>
        cbind(-2L, ... = _, -2L) |>
        as.numeric() |>
        matrix(nrow = length(data_map) + 2L)

    return(map)
}

initial_position <- function(data_map) {
    map <- data_map |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind)
    position <- 1L + c((which(map == "^") - 1L) %% nrow(map),
                       (which(map == "^") - 1L) %/% nrow(map))
    return(position + 1L)
}

move_position <- function(position, direction, steps = 1L) {
    next_positions <- position + steps * switch(direction,
                                                up = c(-1L, 0L),
                                                down = c(1L, 0L),
                                                left = c(0L, -1L),
                                                right = c(0L, 1L))
    return(next_positions)
}

next_dir <- c("right", "left", "up", "down")
names(next_dir) <- c("up", "down", "left", "right")

browse_map <- function(map, start) {

    dir_map <- array(data = rep(map, 4L), dim = c(nrow(map), ncol(map), 4L))
    dimnames(dir_map) <- list(NULL, NULL, c("up", "down", "left", "right"))

    nb_elt <- dim(dir_map)[[1L]]

    pos <- start
    dir <- "up"

    next_pos <- move_position(pos, dir)
    val <- dir_map[next_pos[[1L]], next_pos[[2L]], dir]

    while (val %in% c(-1L, 0L)) {
        if (val == -1L) {
            dir <- next_dir[dir]
            next_pos <- move_position(pos, dir)
            val <- dir_map[next_pos[[1L]], next_pos[[2L]], dir]
        } else {
            # Il n'y a rien devant moi, je peux foncer !
            pos_1 <- switch(dir,
                            up = pos[[1L]]:1L,
                            down = pos[[1L]]:nb_elt,
                            left = pos[[1L]],
                            right = pos[[1L]])
            pos_2 <- switch(dir,
                            up = pos[[2L]],
                            down = pos[[2L]],
                            left = pos[[2L]]:1L,
                            right = pos[[2L]]:nb_elt)

            next_values <- dir_map[pos_1, pos_2, dir]
            steps <- which(next_values != 0L)[[1L]] - 1L
            pos_1 <- pos_1[seq_len(min(steps, length(pos_1)))]
            pos_2 <- pos_2[seq_len(min(steps, length(pos_2)))]

            dir_map[pos_1, pos_2, dir] <- dir_map[pos_1, pos_2, dir] + 1L
            pos <- move_position(pos, dir, steps - 1L)

            next_pos <- move_position(pos, dir)
            val <- dir_map[next_pos[[1L]], next_pos[[2L]], dir]
        }
    }
    return(list(dir_map, success = val == -2L))
}

solve_day06_part1 <- function(data_map) {
    starting_position <- initial_position(data_map)
    map <- initialize_map(data_map)
    output_map <- browse_map(map, starting_position)

    superposition <- pmax(output_map[[1L]][, , 1L],
                          output_map[[1L]][, , 2L],
                          output_map[[1L]][, , 3L],
                          output_map[[1L]][, , 4L])
    count_move <- sum(superposition > 0L)

    return(count_move)
}

solve_day06_part2 <- function(data_map, limit = 1L) {
    starting_position <- initial_position(data_map)
    map <- initialize_map(data_map)

    output_map <- browse_map(map, starting_position)
    superposition <- pmax(output_map[[1L]][, , 1L],
                          output_map[[1L]][, , 2L],
                          output_map[[1L]][, , 3L],
                          output_map[[1L]][, , 4L])
    positions <- which(superposition > 0L)
    positions <- cbind(1L + ((positions - 1L) %% nrow(map)),
                       1L + ((positions - 1L) %/% nrow(map)))

    nb_loop <- 0L
    for (id_pos in seq_len(nrow(positions))) {
        testing_map <- map
        testing_map[positions[id_pos, 1L], positions[id_pos, 2L]] <- -1L
        output_map <- browse_map(testing_map, starting_position)

        if (!output_map[[2L]]) {
            nb_loop <- nb_loop + 1L
        }
    }

    return(nb_loop)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day06_part1(map_example)
solve_day06_part1(map)


## Part 2 ----------------------------------------------------------------------

solve_day06_part2(map_example)
solve_day06_part2(map)

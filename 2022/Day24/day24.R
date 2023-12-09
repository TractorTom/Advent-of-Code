# Titre : day24.R
# Auteur : Tanguy

## Pb à résoudre :
#   - cerains noeux sont répétés lors de leur évaluation (méthode 1 et 2)
#   - la seconde méthode sevrait être plus rapide !

##############
##  DAY 24  ##
##############

# Import data ------------------------------------------------------------------

map_valley <- readLines("./2022/Day24/valley_map.txt")
map_valley_example <- readLines("./2022/Day24/valley_map_example.txt")
map_valley_example2 <- readLines("./2022/Day24/valley_map_example2.txt")

# Déclaration fonction ---------------------------------------------------------

pgcd <- function(x, y) {
    if (y == 0) {
        return(x)
    } else {
        return(pgcd(y, x %% y))
    }
}

ppcm <- function(x, y) {
    return(x * y %/% pgcd(x, y))
}

modulo_vectorise <- function(indexs, pos, all_step) {
    (function(steps) {
        aux <- (function(step) (pos + step - indexs) %% length(indexs) == 0) |>
            Vectorize()
        return(aux(steps))
    })(all_step)
}

get_blizzard_position <- function(data_valley) {
    data_valley <- data_valley |> strsplit("")
    blizzard_position <- NULL
    ordre <- c("<", ">", "^", "v")

    for (row in seq_along(data_valley)) {
        for (col in seq_along(data_valley[[row]])) {
            val <- data_valley[[row]][col]
            if (val %in% ordre) {
                blizzard_position <- rbind(
                    blizzard_position,
                    cbind(
                        row - 1,
                        col - 1,
                        c("left", "right", "up", "down")[which(val == ordre)]
                    )
                )
            }
        }
    }

    blizzard_position <- blizzard_position |>
        as.data.frame() |>
        dplyr::rename(x = 2, y = 1, direction = 3) |>
        dplyr::mutate(
            x = as.integer(x),
            y = as.integer(y)
        )

    return(blizzard_position)
}

get_blizzard_map <- function(data_valley) {
    blizzard_position <- get_blizzard_position(data_valley)

    dim_y <- length(data_valley) - 2
    dim_x <- nchar(data_valley[1]) - 2

    max_step <- ppcm(dim_y, dim_x)
    map <- array(FALSE, dim = c(dim_y, dim_x, max_step))

    for (blizzard in seq_len(nrow(blizzard_position))) {
        y <- blizzard_position$y[blizzard]
        x <- blizzard_position$x[blizzard]
        direction <- blizzard_position$direction[blizzard]

        if (direction == "up") {
            map[, x, ] <- map[, x, ] |
                modulo_vectorise(seq_len(dim_y),
                                 pos = y,
                                 all_step = -(seq_len(max_step)))
        } else if (direction == "down") {
            map[, x, ] <- map[, x, ] |
                modulo_vectorise(seq_len(dim_y),
                                 pos = y,
                                 all_step = seq_len(max_step))
        } else if (direction == "left") {
            map[y, , ] <- map[y, , ] |
                modulo_vectorise(seq_len(dim_x),
                                 pos = x,
                                 all_step = -(seq_len(max_step)))
        } else if (direction == "right") {
            map[y, , ] <- map[y, , ] |
                modulo_vectorise(seq_len(dim_x),
                                 pos = x,
                                 all_step = seq_len(max_step))
        }
    }

    return(map)
}

insert <- function(elt, listo) {
    if (length(listo) == 0) {
        return(list(elt))
    }

    index <- 1
    while (index <= length(listo) && listo[[index]][4] < elt[4]) {
        index <- index + 1
    }

    if (index > length(listo)) {
        return(c(listo, list(elt)))
    }
    return(c(listo[seq_len(index - 1)], list(elt), listo[index:length(listo)]))
}

search_path <- function(blizzard_map, start, end, step) {
    dim_y <- dim(blizzard_map)[1]
    dim_x <- dim(blizzard_map)[2]
    max_step <- dim(blizzard_map)[3]

    list_etat <- list(
        list(c(
            y = start[1],
            x = start[2],
            step = step,
            dist = abs(1 - end[2]) + abs(1 - end[1])
        )), list()
    )

    while (length(list_etat[[1]]) + length(list_etat[[2]]) > 0) {
        if (length(list_etat[[1]]) == 0) {
            etat <- list_etat[[2]][[1]]
            list_etat <- list(list_etat[[2]][-1], list())
        } else {
            etat <- list_etat[[1]][[1]]
            list_etat[[1]] <- list_etat[[1]][-1]
        }

        y <- etat[1]
        x <- etat[2]
        step <- etat[3]

        if (y == end[1] && x == end[2]) {
            return(step + 1)
        }

        if (y == 0) {
            if (!blizzard_map[y + 1, x, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y + 1, x, step + 1,
                        abs(x - end[2]) + abs(y + 1 - end[1])
                    )
                )
                blizzard_map[y + 1, x, (step %% max_step) + 1] <- TRUE
            }

            list_etat[[2]] <- insert(
                listo = list_etat[[2]],
                elt = c(
                    y, x, step + 1,
                    abs(x - end[2]) + abs(y - end[1])
                )
            )
        } else if (y == dim_y + 1) {
            if (!blizzard_map[y - 1, x, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y - 1, x, step + 1,
                        abs(x - end[2]) + abs(y - 1 - end[1])
                    )
                )
                blizzard_map[y - 1, x, (step %% max_step) + 1] <- TRUE
            }

            list_etat[[2]] <- insert(
                listo = list_etat[[2]],
                elt = c(
                    y, x, step + 1,
                    abs(x - end[2]) + abs(y - end[1])
                )
            )
        } else {
            if (y > 1 && !blizzard_map[y - 1, x, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y - 1, x, step + 1,
                        abs(x - end[2]) + abs(y - 1 - end[1])
                    )
                )

                blizzard_map[y - 1, x, (step %% max_step) + 1] <- TRUE
            }

            if (x > 1 && !blizzard_map[y, x - 1, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y, x - 1, step + 1,
                        abs(x - 1 - end[2]) + abs(y - end[1])
                    )
                )

                blizzard_map[y, x - 1, (step %% max_step) + 1] <- TRUE
            }

            if (y < dim_y && !blizzard_map[y + 1, x, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y + 1, x, step + 1,
                        abs(x - end[2]) + abs(y + 1 - end[1])
                    )
                )

                blizzard_map[y + 1, x, (step %% max_step) + 1] <- TRUE
            }

            if (x < dim_x && !blizzard_map[y, x + 1, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y, x + 1, step + 1,
                        abs(x + 1 - end[2]) + abs(y - end[1])
                    )
                )

                blizzard_map[y, x + 1, (step %% max_step) + 1] <- TRUE
            }

            if (!blizzard_map[y, x, (step %% max_step) + 1]) {
                list_etat[[2]] <- insert(
                    listo = list_etat[[2]],
                    elt = c(
                        y, x, step + 1,
                        abs(x - end[2]) + abs(y - end[1])
                    )
                )

                blizzard_map[y, x, (step %% max_step) + 1] <- TRUE
            }
        }
    }

    stop("Il n'y a pas de solutions.")
}

solve_day24_part1 <- function(data_valley) {
    blizzard_map <- get_blizzard_map(data_valley)

    dim_y <- dim(blizzard_map)[1]
    dim_x <- dim(blizzard_map)[2]

    return(search_path(
        blizzard_map = blizzard_map,
        start = c(0, 1), end = c(dim_y, dim_x), step = 0
    ))
}

solve_day24_part2 <- function(data_valley) {
    blizzard_map <- get_blizzard_map(data_valley)

    dim_y <- dim(blizzard_map)[1]
    dim_x <- dim(blizzard_map)[2]

    step <- search_path(
        blizzard_map = blizzard_map,
        start = c(0, 1), end = c(dim_y, dim_x), step = 0
    )
    step <- search_path(
        blizzard_map = blizzard_map,
        start = c(dim_y + 1, dim_x), end = c(1, 1), step = step
    )
    return(search_path(
        blizzard_map = blizzard_map,
        start = c(0, 1), end = c(dim_y, dim_x), step = step
    ))
}

# Execution --------------------------------------------------------------------

solve_day24_part1(map_valley_example)
solve_day24_part1(map_valley_example2)
solve_day24_part1(map_valley)

solve_day24_part2(map_valley_example)
solve_day24_part2(map_valley_example2)
solve_day24_part2(map_valley)

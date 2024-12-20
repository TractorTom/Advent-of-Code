#!/usr/bin/env r

# Titre : day20.R
# Auteur : Tanguy

##############
##  DAY 20  ##
##############


# Import data ------------------------------------------------------------------

racetrack <- readLines(file.path("2024", "Day20", "racetrack.txt"))
racetrack_example <- readLines(file.path("2024", "Day20", "racetrack_example.txt"))


# Déclaration fonction ---------------------------------------------------------

walk <- function(pos, dir, n, p) {
    if (dir == "<" && pos > n) return(pos - n)
    if (dir == ">" && pos < n * p - n + 1L) return(pos + n)
    if (dir == "^" && ((pos - 1L) %% n > 0L)) return(pos - 1L)
    if (dir == "v" && (pos %% n > 0L)) return(pos + 1L)
    return(NULL)
}

get_neighboors <- function(pos, n) {
    neighboors <- lapply(
        X = c("<", ">", "^", "v"),
        FUN = walk, pos = pos, n = n, p = n
    ) |>
        unlist()
    return(neighboors)
}

get_2_neighboors <- function(pos, n) {
    neighboors <- lapply(
        X = c("<", ">", "^", "v"),
        FUN = \(dir) {
            pos |>
                walk(dir = dir, n = n, p = n) |>
                walk(dir = dir, n = n, p = n)
        }
    ) |>
        unlist()
    return(neighboors)
}

create_map_distance <- function(input_racetrack) {

    map <- input_racetrack |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind) |>
        rbind("#", ... = _, "#") |>
        cbind("#", ... = _, "#")

    n <- nrow(map)
    map_distance <- matrix(Inf, nrow = n, ncol = n)

    positions <- which(map == "E")
    step <- 0L
    while (length(positions) > 0L) {
        map_distance[positions] <- step
        neighboors <- NULL
        for (pos in positions) {
            neighboors_n <- get_neighboors(pos, n)
            not_visited <- map[neighboors_n] != "#" & is.infinite(map_distance[neighboors_n])
            neighboors <- c(neighboors, neighboors_n[not_visited])
        }
        step <- step + 1L
        positions <- neighboors
    }

    return(map_distance)
}

dist_manathan <- function(x, y, n) {
    return(abs((x %% n) - (y %% n)) + abs((x %/% n) - (y %/% n)))
}

get_position_vect <- function(pos, n) {
    return(c(row = 1L + (pos - 1L) %% n, col = 1L + (pos - 1L) %/% n))
}

solve_day20_part1 <- function(input_racetrack) {
    map_distance <- create_map_distance(input_racetrack)
    n <- nrow(map_distance)

    maxi <- max(map_distance[is.finite(map_distance)])
    position <- which(map_distance == maxi)
    cheat <- NULL

    while (length(position) > 0L) {
        distance <- map_distance[position]
        neighboors_2_times <- get_2_neighboors(position, n)
        for (neighboor in neighboors_2_times) {
            if (map_distance[neighboor] < distance - 2L) {
                cheat <- c(cheat, distance - map_distance[neighboor] - 2L)
            }
        }
        neighboors <- get_neighboors(position, n)
        position <- neighboors[which(map_distance[neighboors] == distance - 1L)]
    }

    return(sum(cheat >= 100L))
}

find_cheat <- function(position, map_distance) {
    n <- nrow(map_distance)
    p <- ncol(map_distance)
    max_step <- 20L

    position_vect <- get_position_vect(position, n)
    row <- position_vect[["row"]]
    col <- position_vect[["col"]]

    losange <- lapply(
        X = max(1L, row - max_step):min(n, row + max_step),
        FUN = \(r) {
            m <- col - max_step + abs(row - r)
            M <- col + max_step - abs(row - r)
            r + p * (max(m, 1L):min(M, p) - 1L)
        }
    ) |>
        unlist()
    losange <- losange[is.finite(map_distance[losange])]

    dist <- map_distance[losange]
    steps <- dist_manathan(position, losange, n)

    cheat <- map_distance[position] - dist - steps
    return(cheat[cheat > 0L])
}

solve_day20_part2 <- function(input_racetrack) {
    map_distance <- create_map_distance(input_racetrack)
    positions <- which(is.finite(map_distance))

    cheat <- integer(length(positions))
    names(cheat) <- seq_along(positions)
    for (pos in positions) {
        new_cheat <- table(find_cheat(pos, map_distance))
        cheat[names(new_cheat)] <- cheat[names(new_cheat)]  + new_cheat
    }

    return(sum(cheat[-(1L:99L)]))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day20_part1(racetrack)


## Part 2 ----------------------------------------------------------------------

solve_day20_part2(racetrack)

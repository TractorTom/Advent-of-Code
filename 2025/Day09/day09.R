#!/usr/bin/env r

# Titre : day09.R
# Auteur : Tanguy

##############
##  DAY 09  ##
##############


# Import data ------------------------------------------------------------------

theater_floor <- readLines(file.path("2025", "Day09", "floors.txt"))
theater_floor_example <- readLines(file.path("2025", "Day09",
                                             "floors_example.txt"))


# Déclaration fonction ---------------------------------------------------------

# Execution --------------------------------------------------------------------

read_tiles <- function(raw_floor) {
    read.table(
        text = raw_floor,
        sep = ",",
        colClasses = c("double", "double"),
        col.names = c("x", "y")
    )
}

compute_distance <- function(tiles) {
    nb_points <- nrow(tiles)

    x <- matrix(tiles[, 1L], ncol = nb_points, nrow = nb_points)
    y <- matrix(tiles[, 2L], ncol = nb_points, nrow = nb_points)

    distances <- (abs(x - t(x)) + 1.0) * (abs(y - t(y)) + 1.0)
    return(distances)
}

solve_day09_part1 <- function(raw_floor) {
    tiles <- read_tiles(raw_floor)
    d <- compute_distance(tiles)
    return(max(d))
}

compute_horizontals <- function(tiles) {
    if (tiles[1L, 1L] == tiles[2L, 1L]) {
        tiles <- tiles[c(2L:nrow(tiles), 1L), ]
    }
    horizontals <- rbind(
        matrix(tiles[, 1L], nrow = 2L),
        matrix(tiles[, 2L], nrow = 2L)
    )
    horizontals <- rbind(
        apply(FUN = range, horizontals[1L:2L, ], MARGIN = 2L),
        horizontals[3L, ]
    )
    return(horizontals)
}

compute_verticals <- function(tiles) {
    if (tiles[1L, 2L] == tiles[2L, 2L]) {
        tiles <- tiles[c(2L:nrow(tiles), 1L), ]
    }
    verticals <- rbind(
        matrix(tiles[, 2L], nrow = 2L),
        matrix(tiles[, 1L], nrow = 2L)
    )
    verticals <- rbind(
        apply(FUN = range, verticals[1L:2L, ], MARGIN = 2L),
        verticals[3L, ]
    )
    return(verticals)
}

is_whole <- function(p1, p2, horizontals, verticals) {
    m2 <- min(c(p1["y"], p2["y"]))
    m1 <- min(c(p1["x"], p2["x"]))
    M2 <- max(c(p1["y"], p2["y"]))
    M1 <- max(c(p1["x"], p2["x"]))

    cond_x <- horizontals[3L, ] > m2 &
        horizontals[3L, ] < M2 &
        horizontals[2L, ] > m1 &
        horizontals[1L, ] < M1

    if (any(cond_x)) {
        return(FALSE)
    }

    cond_y <- verticals[3L, ] > m1 &
        verticals[3L, ] < M1 &
        verticals[2L, ] > m2 &
        verticals[1L, ] < M2

    return(!any(cond_y))
}

is_inside <- function(point, verticals) {
    left_lines <- verticals[3L, ] < point["x"] &
        verticals[1L, ] < point["y"] &
        verticals[2L, ] > point["y"]

    return(sum(left_lines) %% 2L == 1L)
}

solve_day09_part2 <- function(raw_floor) {
    positions <- as.matrix(read_tiles(raw_floor))
    nb_points <- nrow(positions)

    id1 <- rep(seq_len(nb_points), times = nb_points)
    id2 <- rep(seq_len(nb_points), each = nb_points)

    distances <- data.frame(
        point_A = id1,
        point_B = id2,
        distance = as.double(compute_distance(positions))
    )
    distances <- distances[id1 < id2, ]

    horizontals <- compute_horizontals(positions)
    verticals <- compute_verticals(positions)

    distances <- distances[order(distances$distance, decreasing = TRUE), ]

    for (k in seq_len(nrow(distances))) {
        p1 <- positions[distances$point_A[k], ]
        p2 <- positions[distances$point_B[k], ]
        cond1 <- is_whole(
            p1 = p1,
            p2 = p2,
            horizontals = horizontals,
            verticals = verticals
        )
        if (cond1) {
            cond2 <- is_inside(
                point = c(
                    x = mean(c(p1["x"], p2["x"])),
                    y = mean(c(p1["y"], p2["y"]))
                ) + 0.1,
                verticals = verticals
            )
            if (cond2) {
                return(distances[k, "distance"])
            }
        }
    }
    stop("Not found !", call. = FALSE)
}


## Part 1 ----------------------------------------------------------------------

solve_day09_part1(theater_floor_example)
solve_day09_part1(theater_floor)

## Part 2 ----------------------------------------------------------------------

solve_day09_part2(theater_floor_example)
solve_day09_part2(theater_floor)

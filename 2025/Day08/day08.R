#!/usr/bin/env r

# Titre : day08.R
# Auteur : Tanguy

##############
##  DAY 08  ##
##############

# Import data ------------------------------------------------------------------

positions <- readLines(file.path("2025", "Day08", "boxes_positions.txt"))
positions_example <- readLines(file.path("2025", "Day08",
                                         "boxes_positions_example.txt"))


# Déclaration fonction ---------------------------------------------------------

read_junction <- function(raw_positions) {
    boxes <- read.table(
        text = raw_positions,
        sep = ",",
        colClasses = c("double", "double", "double")
    )
    nb_points <- nrow(boxes)

    x <- matrix(boxes[, 1L], ncol = nb_points, nrow = nb_points)
    y <- matrix(boxes[, 2L], ncol = nb_points, nrow = nb_points)
    z <- matrix(boxes[, 3L], ncol = nb_points, nrow = nb_points)

    distances <- sqrt((x - t(x)) ** 2L + (y - t(y)) ** 2L + (z - t(z)) ** 2L)

    id1 <- rep(seq_len(nb_points), times = nb_points)
    id2 <- rep(seq_len(nb_points), each = nb_points)

    junctions_distances <- data.frame(
        point_A = id1,
        point_B = id2,
        distance = as.double(distances)
    )

    junctions_distances <- junctions_distances[id1 < id2, ]

    return(list(distances = junctions_distances, boxes = boxes))
}

get_cycles <- function(junctions) {
    junctions[, 1L] <- as.character(junctions[, 1L])
    junctions[, 2L] <- as.character(junctions[, 2L])

    vertices <- unique(c(junctions[, 1L], junctions[, 2L]))
    vertice_colors <- seq_along(vertices)
    names(vertice_colors) <- vertices

    k <- 0L
    while (any(vertice_colors > 1L) && k < nrow(junctions)) {
        k <- k + 1L
        c1 <- vertice_colors[junctions[k, 1L]]
        c2 <- vertice_colors[junctions[k, 2L]]

        if (c1 != c2) {
            vertice_colors[vertice_colors %in% max(c1, c2)] <- min(c1, c2)
        }
    }

    return(list(cycles = vertice_colors, index = k))
}

solve_day08_part1 <- function(raw_positions, max_pairs = 10L) {
    distances <- read_junction(raw_positions)$distances

    sorted_index <- head(order(distances[, "distance"]), max_pairs)
    distances <- distances[sorted_index, 1L:2L]
    cycles <- get_cycles(distances)$cycles

    return(prod(head(sort(table(cycles), decreasing = TRUE), 3L)))
}

solve_day08_part2 <- function(raw_positions) {
    junctions <- read_junction(raw_positions)
    distances <- junctions$distances

    distances <- distances[order(distances[, "distance"]), ]
    index <- get_cycles(distances)$index

    return(prod(
        junctions$boxes[distances[index, 1L], 1L],
        junctions$boxes[distances[index, 2L], 1L]
    ))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day08_part1(positions_example, max_pairs = 10L)
solve_day08_part1(positions, max_pairs = 1000L)

## Part 2 ----------------------------------------------------------------------

solve_day08_part2(positions_example)
solve_day08_part2(positions)

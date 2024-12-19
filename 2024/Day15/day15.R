#!/usr/bin/env r

# Titre : day15.R
# Auteur : Tanguy

##############
##  DAY 15  ##
##############


# Import data ------------------------------------------------------------------

lanternfish <- readLines(file.path("2024", "Day15", "woes.txt"))
lanternfish_example <- readLines(file.path("2024", "Day15", "woes_example.txt"))
lanternfish_example2 <- readLines(file.path("2024", "Day15", "woes_example2.txt"))
lanternfish_example3 <- readLines(file.path("2024", "Day15", "woes_example3.txt"))


# Déclaration fonction ---------------------------------------------------------

read_lanternfish <- function(input_lanternfish) {
    k <- which(input_lanternfish == "")
    map <- strsplit(x = input_lanternfish[seq_len(k - 1L)],
                    split = "",
                    fixed = TRUE) |>
        do.call(what = rbind)
    instr <- strsplit(x = input_lanternfish[(k + 1L):length(input_lanternfish)],
                      split = "",
                      fixed = TRUE) |>
        unlist()
    return(list(map = map, instructions = instr))
}

extend_map <- function(map) {
    new_map <- NULL
    for (col in seq_len(ncol(map))) {
        colonne <- map[, col]
        O <- which(colonne == "O")
        diese <- which(colonne == "#")
        arobase <- which(colonne == "@")
        if (length(c(O, diese)) > 0L) {
            colonne[O] <- "["
            colonne[diese] <- "#"
            colonne[arobase] <- "@"
            new_map <- cbind(new_map, colonne)
            colonne[O] <- "]"
            colonne[arobase] <- "."
            new_map <- cbind(new_map, colonne)
        } else {
            new_map <- cbind(new_map, colonne)
        }
    }
    return(new_map)
}

walk <- function(pos, dir, n, p) {
    if (dir == "<" && pos > n) return(pos - n)
    if (dir == ">" && pos < n * p - n + 1L) return(pos + n)
    if (dir == "^" && ((pos - 1L) %% n > 0L)) return(pos - 1L)
    if (dir == "v" && (pos %% n > 0L)) return(pos + 1L)
    return(NULL)
}

get_neighboors_inline <- function(pos, dir, n, p) {
    if (is.null(pos)) return(NULL)
    next_position <- walk(pos, dir, n, p)
    return(c(next_position, get_neighboors_inline(next_position, dir, n, p)))
}

get_block <- function(pos, dir, map) {

    next_position <- walk(pos, dir, nrow(map), ncol(map))
    val <- map[next_position]

    if (val == "#") {
        return(NULL)
    } else if (val == ".") {
        return(pos)
    } else if (val == "[") {
        right_pos <- walk(next_position, ">", nrow(map), ncol(map))
        right_block <- get_block(pos = right_pos, dir, map)
        left_block <- get_block(pos = next_position, dir, map)
    } else if (val == "]") {
        left_pos <- walk(next_position, "<", nrow(map), ncol(map))
        left_block <- get_block(left_pos, dir, map)
        right_block <- get_block(next_position, dir, map)
    }

    if (is.null(right_block) || is.null(left_block)) return(NULL)
    return(c(left_block, right_block, pos))
}

update_map <- function(pos, dir, map, part = 1L) {
    n <- nrow(map)
    p <- ncol(map)
    if (part == 1L || dir %in% c("<", ">")) {
        position_inline <- get_neighboors_inline(pos, dir, n, p)
        boxes <- map[position_inline]
        limit <- which.max(boxes %in% c(".", "#"))
        if (boxes[limit] == ".") {
            map[position_inline[seq_len(limit)]] <- map[c(pos, position_inline[seq_len(limit - 1L)])]
            map[pos] <- "."
        }
    } else {
        block <- get_block(pos, dir, map)
        if (!is.null(block)) {
            n_map <- map
            map[block] <- "."
            map[block + 2L * (dir == "v") - 1L] <- n_map[block]
        }
    }
    return(map)
}

compute_score <- function(map) {
    score <- 0L
    for (row in seq_len(nrow(map))) {
        for (col in seq_len(ncol(map))) {
            if (map[row, col] %in% c("O", "[")) {
                score <- score + (row  - 1L) * 100L + (col - 1L)
            }
        }
    }
    return(score)
}

solve_day15_part1 <- function(input_lanternfish) {
    data_warehouse <- read_lanternfish(input_lanternfish)
    map <- data_warehouse[["map"]]
    for (direction in data_warehouse[["instructions"]]) {
        pos <- which(map == "@")
        map <- update_map(pos = pos, dir = direction, map = map, part = 1L)
    }
    return(compute_score(map))
}

solve_day15_part2 <- function(input_lanternfish) {
    data_warehouse <- read_lanternfish(input_lanternfish)
    map <- data_warehouse[["map"]] |> extend_map()
    for (direction in data_warehouse[["instructions"]]) {
        pos <- which(map == "@")
        map <- update_map(pos = pos, dir = direction, map = map, part = 2L)
    }
    return(compute_score(map))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day15_part1(lanternfish_example)
solve_day15_part1(lanternfish_example2)
solve_day15_part1(lanternfish)


## Part 2 ----------------------------------------------------------------------

solve_day15_part2(lanternfish_example)
solve_day15_part2(lanternfish)

#!/usr/bin/env r

# Titre : day11.R
# Auteur : Tanguy

##############
##  DAY 11  ##
##############


# Import data ------------------------------------------------------------------

rack <- readLines(file.path("2025", "Day11", "server_rack.txt"))
rack_example <- readLines(file.path("2025", "Day11", "server_rack_example.txt"))
rack_example2 <- readLines(file.path("2025", "Day11",
                                     "server_rack_example2.txt"))


# Déclaration fonction ---------------------------------------------------------

# Execution --------------------------------------------------------------------

init <- function(raw_connections) {
    connections <- strsplit(raw_connections, split = ": ", fixed = TRUE)
    devices <- lapply(connections, "[", 1L)
    names(connections) <- devices
    connections <- lapply(connections, "[", 2L) |>
        lapply(strsplit, " ") |>
        lapply(unlist)
    return(connections)
}

count_paths <- function(connections, from, to) {
    counter <- rep(NA_integer_, length(connections))
    visited <- rep(FALSE, length(connections))

    names(counter) <- names(visited) <- names(connections)

    counter[to] <- 1.0
    visited[to] <- TRUE

    if (to != "out") {
        counter["out"] <- 0.0
        visited["out"] <- TRUE
    }

    while (!visited[from]) {
        for (device in names(which(!visited))) {
            outputs <- connections[[device]]
            if (all(visited[outputs])) {
                counter[device] <- sum(counter[outputs])
                visited[device] <- TRUE
            }
        }
    }
    return(counter[from])
}

solve_day11_part1 <- function(raw_connections) {
    connections <- init(raw_connections)
    nb_paths <- count_paths(connections, from = "you", to = "out")
    return(nb_paths)
}

solve_day11_part2 <- function(raw_connections) {
    connections <- init(raw_connections)
    nb_paths <- count_paths(connections, "svr", "fft") *
        count_paths(connections, "fft", "dac") *
        count_paths(connections, "dac", "out")
    return(nb_paths)
}


## Part 1 ----------------------------------------------------------------------

solve_day11_part1(rack_example)
solve_day11_part1(rack)

## Part 2 ----------------------------------------------------------------------

solve_day11_part2(rack_example2)
solve_day11_part2(rack)

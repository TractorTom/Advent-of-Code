#!/usr/bin/env r

# Titre : day07.R
# Auteur : Tanguy

##############
##  DAY 07  ##
##############

# Import data ------------------------------------------------------------------

diagrams <- readLines(file.path("2025", "Day07", "diagrams.txt"))
diagrams_example <- readLines(file.path("2025", "Day07",
                                        "diagrams_example.txt"))


# Déclaration fonction ---------------------------------------------------------

count_splits <- function(counter, splitter_line) {
    nc <- double(length(counter))
    splitter <- splitter_line == "^"

    left_beam <- c(tail(splitter, -1L), FALSE)
    right_beam <- c(FALSE, head(splitter, -1L))

    nc[!splitter] <- counter[!splitter]
    nc[right_beam] <- nc[right_beam] + counter[splitter]
    nc[left_beam] <- nc[left_beam] + as.integer(counter[splitter] > 0L)

    return(nc)
}

solve_day07_part1 <- function(raw_diagrams) {
    tachyon_manifold <- strsplit(raw_diagrams, split = "", fixed = TRUE)
    header <- tachyon_manifold[[1L]]
    l <- length(tachyon_manifold)
    L <- length(header)
    tachyon_manifold <- tachyon_manifold[seq(from = 1L, to = l, by = 2L)]
    incoming_beam <- rep(0L, L)
    incoming_beam[header == "S"] <- 1L
    nb_splits <- Reduce(
        f = count_splits,
        x = tachyon_manifold[-1L],
        init = incoming_beam
    )
    return(sum(nb_splits) - 1L)
}

count_quantum_timelines <- function(counter, splitter_line) {
    nc <- double(length(counter))
    splitter <- splitter_line == "^"
    nc[!splitter] <- counter[!splitter]
    nc[splitter] <- counter[c(FALSE, head(splitter, -1L))] +
        counter[c(tail(splitter, -1L), FALSE)]
    return(nc)
}

solve_day07_part2 <- function(raw_diagrams) {
    tachyon_manifold <- strsplit(raw_diagrams, split = "", fixed = TRUE)
    header <- tachyon_manifold[[1L]]
    odd_indexes <- seq(from = 1L, to = length(tachyon_manifold), by = 2L)
    tachyon_manifold <- tachyon_manifold[odd_indexes]
    timelines <- Reduce(
        f = count_quantum_timelines,
        x = rev(tachyon_manifold),
        init = rep(1.0, length(header))
    )
    timelines <- timelines[header == "S"]
    return(timelines)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day07_part1(diagrams_example)
solve_day07_part1(diagrams)

## Part 2 ----------------------------------------------------------------------

solve_day07_part2(diagrams_example)
solve_day07_part2(diagrams)

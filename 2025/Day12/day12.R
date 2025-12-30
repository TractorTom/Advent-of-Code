#!/usr/bin/env r

# Titre : day12.R
# Auteur : Tanguy

##############
##  DAY 12  ##
##############


# Import data ------------------------------------------------------------------

presents <- readLines(file.path("2025", "Day12", "presents_situation.txt"))


# Déclaration fonction ---------------------------------------------------------

read_summary <- function(raw_regions) {
    sep <- min(grep(raw_regions, pattern = "x", fixed = TRUE))

    sections <- raw_regions[sep:length(raw_regions)] |>
        strsplit(": ", fixed = TRUE)

    nb_of_shapes <- lapply(sections, "[", 2L) |>
        as.character() |>
        strsplit(" ", fixed = TRUE) |>
        lapply(as.integer) |>
        lapply(sum) |>
        as.integer()

    areas <- lapply(sections, "[", 1L) |>
        as.character() |>
        strsplit("x", fixed = TRUE) |>
        lapply(as.integer) |>
        lapply(prod) |>
        as.integer()

    return(list(quantity = nb_of_shapes, areas = areas))
}

solve_day12_part1 <- function(raw_regions) {
    regions <- read_summary(raw_regions)
    return(sum(regions$areas >= regions$quantity * 9L))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day12_part1(presents)

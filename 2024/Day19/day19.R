#!/usr/bin/env r

# Titre : day19.R
# Auteur : Tanguy

##############
##  DAY 19  ##
##############


# Import data ------------------------------------------------------------------

onsen <- readLines(file.path("2024", "Day19", "onsen.txt"))
onsen_example <- readLines(file.path("2024", "Day19", "onsen_example.txt"))


# Déclaration fonction ---------------------------------------------------------

read_towel <- function(input_onsen) {
    stripes <- strsplit(input_onsen[1], ", ", fixed = TRUE) |> unlist()
    designs <- input_onsen[-(1:2)]
    return(list(patterns = stripes, designs = designs))
}

count_different_ways <- function(towel, patterns) {
    if (nchar(towel) == 0) {
        return(1)
    }
    if (!is.null(grand_possible[[towel]]) && !is.na(grand_possible[[towel]])) {
        return(grand_possible[[towel]])
    }

    sol <- 0
    for (pattern in patterns) {
        if (startsWith(towel, pattern)) {
            solution <- is_possible(substr(towel, nchar(pattern) + 1, 100), patterns = patterns)
            sol <- sol + solution
        }
    }
    grand_possible[[towel]] <<- sol
    return(sol)
}

solve_day19_part1 <- function(input_onsen) {
    data_towel <- read_towel(input_onsen)

    stripes <- data_towel$patterns
    pattern <- paste0("^(", paste0(stripes, collapse = "|"), ")*$")
    designs <- data_towel$designs

    possible_designs <- grepl(
        pattern = pattern,
        x = designs,
        perl = FALSE,
        fixed = FALSE
    ) |> sum()

    return(possible_designs)
}

solve_day19_part2 <- function(input_onsen){
    data_towel <- read_towel(input_onsen)
    grand_possible <<- hashtab()
    s <- 0
    for (design in data_towel$designs) {
        s <- s + count_different_ways(design, data_towel$patterns)
    }
    return(s)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day19_part1(onsen_example)
solve_day19_part1(onsen)


## Part 2 ----------------------------------------------------------------------

solve_day19_part2(onsen_example)
solve_day19_part2(onsen)

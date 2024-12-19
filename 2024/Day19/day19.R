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
    stripes <- strsplit(input_onsen[[1L]], ", ", fixed = TRUE) |> unlist()
    designs <- input_onsen[-(1L:2L)]
    return(list(patterns = stripes, designs = designs))
}

count_different_ways <- function(design, patterns, cache) {
    if (nchar(design) == 0L) {
        return(1.0)
    }
    if (!is.null(cache[[design]])
        && !is.na(cache[[design]])) {
        return(cache[[design]])
    }

    nb_solutions <- 0.0
    for (pattern in patterns) {
        if (startsWith(design, pattern)) {
            solution <- count_different_ways(
                design = substr(design, nchar(pattern) + 1L, 100L),
                patterns = patterns,
                cache = cache
            )
            nb_solutions <- nb_solutions + solution
        }
    }
    cache[[design]] <- nb_solutions
    return(nb_solutions)
}

solve_day19_part1 <- function(input_onsen) {
    data_towel <- read_towel(input_onsen)

    stripes <- data_towel[["patterns"]]
    pattern <- paste0("^(", paste0(stripes, collapse = "|"), ")*$")
    designs <- data_towel[["designs"]]

    possible_designs <- grepl(
        pattern = pattern,
        x = designs,
        perl = FALSE,
        fixed = FALSE
    ) |> sum()

    return(possible_designs)
}

solve_day19_part2 <- function(input_onsen) {
    data_towel <- read_towel(input_onsen)
    cache <- hashtab()
    nb_solutions <- 0.0
    for (design in data_towel[["designs"]]) {
        nb_solutions <- nb_solutions + count_different_ways(
            design = design,
            patterns = data_towel[["patterns"]],
            cache = cache
        )
    }
    return(nb_solutions)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day19_part1(onsen_example)
solve_day19_part1(onsen)


## Part 2 ----------------------------------------------------------------------

solve_day19_part2(onsen_example)
solve_day19_part2(onsen)

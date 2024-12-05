#!/usr/bin/env r

# Titre : day05.R
# Auteur : Tanguy

##############
##  DAY 05  ##
##############

# Import data ------------------------------------------------------------------

manual <- readLines(file.path("2024", "Day05", "manual.txt"))
manual_example <- readLines(file.path("2024", "Day05", "manual_example.txt"))


# Déclaration fonction ---------------------------------------------------------

pre_treatment <- function(data_manual) {
    sep <- which(data_manual == "")
    rules <- data_manual[seq_len(sep - 1L)] |>
        strsplit(split = "|", fixed = TRUE) |>
        do.call(what = rbind) |>
        as.data.frame()
    updates <- data_manual[(sep + 1L):(length(data_manual))] |>
        strsplit(split = ",", fixed = TRUE)

    return(list(rules = rules, updates = updates))
}

is_sorted <- function(x, rules) {
    rules <- rules[rules[["V1"]] %in% x & rules[["V2"]] %in% x, ]
    positions <- seq_along(x)
    names(positions) <- x
    condition <- all(positions[rules[["V1"]]] < positions[rules[["V2"]]])
    return(condition)
}

solve_day05_part1 <- function(data_manual) {

    l_manual <- pre_treatment(data_manual)
    rules <- l_manual[["rules"]]
    updates <- l_manual[["updates"]]

    output <- 0L
    for (pages in updates) {
        if (is_sorted(pages, rules)) {
            output <-  output + as.numeric(pages[(length(pages) + 1L) / 2L])
        }
    }
    return(output)
}

rearrange <- function(x, rules) {
    positions <- seq_along(x)
    names(positions) <- x
    rules <- rules[rules[["V1"]] %in% x & rules[["V2"]] %in% x, ]
    while (length(x) > 1L) {
        never_first <- setdiff(x, rules[["V1"]])
        never_second <- setdiff(x, rules[["V2"]])
        rules <- rules[rules[["V1"]] != never_second
                       & rules[["V2"]] != never_first, ]
        x <- x[!x %in% c(never_first, never_second)]
    }
    return(x)
}

solve_day05_part2 <- function(data_manual) {
    l_manual <- pre_treatment(data_manual)
    rules <- l_manual[["rules"]]
    updates <- l_manual[["updates"]]

    output <- 0L
    for (pages in updates) {
        if (!is_sorted(pages, rules)) {
            output <- output + as.numeric(rearrange(pages, rules))
        }
    }
    return(output)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day05_part1(manual_example)
solve_day05_part1(manual)


## Part 2 ----------------------------------------------------------------------

solve_day05_part2(manual_example)
solve_day05_part2(manual)

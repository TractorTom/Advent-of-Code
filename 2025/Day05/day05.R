#!/usr/bin/env r

# Titre : day05.R
# Auteur : Tanguy

##############
##  DAY 05  ##
##############


# Import data ------------------------------------------------------------------

ingredients_table <- readLines(file.path(
    "2025", "Day05", "fresh_ingredients.txt"
))
ingredients_table_example <- readLines(file.path(
    "2025", "Day05", "fresh_ingredients_example.txt"
))


# Déclaration fonction ---------------------------------------------------------

extract_ID <- function(raw_ID) {
    sep <- which(!nzchar(raw_ID))
    ranges <- strsplit(raw_ID[seq_len(sep - 1L)], split = "-", fixed = TRUE) |>
        lapply(as.numeric) |>
        do.call(what = rbind) |>
        as.data.frame()
    ingredients <- as.numeric(raw_ID[-seq_len(sep)])
    return(list(ranges = ranges, ingredients = ingredients))
}

solve_day05_part11 <- function(raw_ID) {
    ID <- extract_ID(raw_ID)

    expr <- paste("x >=", ID$ranges$V1,
                  "& x <=", ID$ranges$V2) |>
        paste(collapse = ") | (") |>
        paste0("(", ... = _, ")")
    all_fresh <- eval(
        expr = parse(text = expr),
        envir = list2env(list(x = ID$ingredients))
    )
    return(sum(all_fresh))
}


solve_day05_part2 <- function(raw_ID) {
    ID <- extract_ID(raw_ID)
    ranges <- ID$ranges[order(ID$ranges$V1), ]

    # Sorting
    ranges <- ranges[order(ranges$V1), ]

    # Borne sup
    ranges$max <- cummax(ranges$V2)

    # Filtering
    ranges <- ranges[ranges$V2 == ranges$max, ]

    # Different groups
    separator <- ranges$V1[-1L] - ranges$max[-nrow(ranges)] > 1L

    # Computing endpoints
    endpoint1 <- c(TRUE, separator) * ranges$V1
    endpoint2 <- c(separator, TRUE) * ranges$V2

    return(sum(endpoint2[endpoint2 != 0L] - endpoint1[endpoint1 != 0L] + 1L))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day05_part1(ingredients_table_example)
solve_day05_part1(ingredients_table)

## Part 2 ----------------------------------------------------------------------

solve_day05_part2(ingredients_table_example)
solve_day05_part2(ingredients_table)

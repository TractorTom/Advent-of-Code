#!/usr/bin/env r

# Titre : day07.R
# Auteur : Tanguy

##############
##  DAY 07  ##
##############


# Import data ------------------------------------------------------------------

equations <- readLines(file.path("2024", "Day07", "equations.txt"))
equations_example <- readLines(file.path("2024", "Day07",
                                         "equations_example.txt"))


# Déclaration fonction ---------------------------------------------------------

extract_terms <- function(equation) {
    terms <- equation |>
        strsplit(split = ": | ", perl = TRUE, fixed = FALSE) |>
        unlist() |>
        as.numeric()
    return(terms)
}

concatenate <- function(a, b) {
    return(a * 10L ** (floor(log10(b)) + 1L) + b)
}

all_possible <- function(funs, res, values) {
    l <- length(values)
    if (l == 1L) {
        return(values)
    }
    possible_n_1 <- all_possible(funs, res, values[-l])
    possible_n_1 <- possible_n_1[possible_n_1 <= res]
    output <- vapply(
        X = funs,
        FUN = do.call,
        list(possible_n_1, values[[l]]),
        FUN.VALUE = numeric(length(possible_n_1))
    ) |>
        as.numeric()
    return(output)
}

count_valid <- function(data_equations, funs) {
    calibration_results <- 0L
    for (equations in data_equations) {
        out <- extract_terms(equations)
        res <- out[[1L]]
        test_values <- out[-1L]
        if (res %in% all_possible(funs = funs, res, test_values)) {
            calibration_results <- calibration_results + res
        }
    }
    return(calibration_results)
}

solve_day07_part1 <- function(data_equations) {
    out <- count_valid(data_equations, list(`+`, `*`))
    return(out)
}

solve_day07_part2 <- function(data_equations) {
    out <- count_valid(data_equations, list(`+`, concatenate, `*`))
    return(out)
}


# Execution --------------------------------------------------------------------

withr::with_options(new = list(digits = 22L), code = {

    ## Part 1 ------------------------------------------------------------------

    solve_day07_part1(equations_example) |> print()
    solve_day07_part1(equations) |> print()


    ## Part 2 ------------------------------------------------------------------

    solve_day07_part2(equations_example) |> print()
    solve_day07_part2(equations) |> print()

})

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

all_possible <- function(res, values) {
    l <- length(values)
    if (l == 1L) {
        return(values)
    }
    out <- all_possible(res, values[-l])
    out <- out[out <= res]
    return(c(values[[l]] * out, values[[l]] + out))
}

all_possible2 <- function(res, values) {
    l <- length(values)
    if (l == 1L) {
        return(values)
    }
    out <- all_possible2(res, values[-l])
    out <- out[out <= res]
    return(c(values[[l]] * out,
             values[[l]] + out,
             as.numeric(paste0(out, values[[l]]))))
}

count_valid <- function(data_equations, fun_possible) {
    calibration_results <- 0L
    for (equations in data_equations) {
        out <- equations |>
            strsplit(": ", fixed = TRUE) |>
            unlist() |>
            strsplit(" ", fixed = TRUE) |>
            unlist() |>
            as.numeric()
        res <- out[[1L]]
        test_values <- out[-1L]
        if (res %in% fun_possible(res, test_values)) {
            calibration_results <- calibration_results + res
        }
    }
    return(calibration_results)
}

solve_day07_part1 <- function(data_equations) {
    out <- count_valid(data_equations, all_possible)
    return(out)
}

solve_day07_part2 <- function(data_equations) {
    out <- count_valid(data_equations, all_possible2)
    return(out)
}


# Execution --------------------------------------------------------------------

withr::with_options(new = list(digits = 22), code = {

    ## Part 1 ------------------------------------------------------------------

    solve_day07_part1(equations_example) |> print()
    solve_day07_part1(equations) |> print()


    ## Part 2 ------------------------------------------------------------------

    solve_day07_part2(equations_example) |> print()
    solve_day07_part2(equations) |> print()

})

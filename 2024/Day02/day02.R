# Titre : day02.R
# Auteur : Tanguy

##############
##  DAY 02  ##
##############


# Import data ------------------------------------------------------------------

reports <- readLines(file.path("2024", "Day02", "report.txt"))
reports_example <- readLines(file.path("2024", "Day02", "report_example.txt"))


# Déclaration fonction ---------------------------------------------------------

treat_as_list <- function(data_reports) {
    output <- data_reports |>
        strsplit(split = " ", fixed = TRUE) |>
        lapply(FUN = as.numeric)
    return(output)
}

is_safe <- function(report) {
    d_report <- diff(report)
    safe <- all(d_report %in% -(1L:3L)) || all(d_report %in% 1L:3L)
    return(safe)
}

is_safe2 <- function(report) {
    safe <- is_safe(report) || lapply(X = -seq_along(report),
                                      FUN = \(a) report[a]) |>
        lapply(is_safe) |>
        unlist() |>
        any()
    return(safe)
}

solve_day02_part1 <- function(data_reports) {
    output <- data_reports |>
        treat_as_list() |>
        lapply(FUN = is_safe) |>
        unlist() |>
        sum()
    return(output)
}

solve_day02_part2 <- function(data_reports) {
    output <- data_reports |>
        treat_as_list() |>
        lapply(FUN = is_safe2) |>
        unlist() |>
        sum()
    return(output)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day02_part1(reports_example)
solve_day02_part1(reports)


## Part 2 ----------------------------------------------------------------------

solve_day02_part2(reports_example)
solve_day02_part2(reports)

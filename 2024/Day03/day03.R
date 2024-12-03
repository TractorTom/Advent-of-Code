# Titre : day03.R
# Auteur : Tanguy

##############
##  DAY 03  ##
##############


# Import data ------------------------------------------------------------------

memory <- readLines(file.path("2024", "Day03", "memory.txt"))
memory_example <- readLines(file.path("2024", "Day03", "memory_example.txt"))


# Déclaration fonction ---------------------------------------------------------

get_do <- function(x) {
    return(gregexpr("do()", x, fixed = TRUE)[[1L]])
}

get_dont <- function(x) {
    return(gregexpr("don't()", x, fixed = TRUE)[[1L]])
}

extract_regex <- function(x, pattern) {
    return(regmatches(x = x, m = gregexpr(pattern = pattern, text = x)))
}

solve_day03_part1 <- function(data_memory) {
    mul <- data_memory |>
        paste0(collapse = "") |>
        extract_regex(pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)") |>
        unlist() |>
        extract_regex(pattern = "\\d+")
    corrupted_memory <- vapply(X = mul,
                               FUN = \(x) prod(as.integer(x)),
                               FUN.VALUE = numeric(1L)) |>
        sum()
    return(corrupted_memory)
}

extract_enabled <- function(x) {

    do <- get_do(x)
    don <- get_dont(x)

    if (-1L %in% don) return(x)

    b1 <- 1L
    b2 <- don[[1L]] - 1L
    out <- substr(x, b1, b2)

    while (b2 < nchar(x) && length(do[do > b2]) > 0L) {
        b1 <- min(do[do > b2]) + 4L
        b2 <- min(don[don > b1]) - 1L
        val <- substr(x, b1, b2)
        out <- c(out, val)
    }

    return(out)
}

solve_day03_part2 <- function(data_memory) {
    corrupted_memory <- data_memory |>
        paste0(collapse = "") |>
        vapply(FUN = solve_day03_part1, FUN.VALUE = numeric(1L)) |>
        sum()
    return(corrupted_memory)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day03_part1(memory_example)
solve_day03_part1(memory)


## Part 2 ----------------------------------------------------------------------

solve_day03_part2(memory_example)
solve_day03_part2(memory)

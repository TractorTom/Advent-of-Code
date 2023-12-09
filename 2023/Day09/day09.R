# Titre : day09.R
# Auteur : Tanguy

##############
##  DAY 09  ##
##############


# Import data ------------------------------------------------------------------

history <- readLines("./2023/Day09/OASIS_history.txt")
history_example <- readLines("./2023/Day09/OASIS_history_example.txt")


# Déclaration fonction ---------------------------------------------------------

init <- function(data_history) {
    data_history <- data_history |> strsplit(" ") |> lapply(FUN = as.numeric)
    return(data_history)
}

predict_next_val <- function(one_history) {

    values_diff <- one_history
    index_diff <- 0

    while (length(unique(values_diff)) > 1) {
        values_diff <- diff(values_diff)
        index_diff <- index_diff + 1
    }

    out <- diffinv(x = c(values_diff, values_diff[1]),
                   differences = index_diff,
                   xi = one_history[seq_len(index_diff)])

    return(out[length(out)])
}

solve_day09_part1 <- function(data_history) {
    data_history <- init(data_history)
    return(vapply(X = data_history,
                  FUN = predict_next_val,
                  FUN.VALUE = numeric(1)) |> sum())
}

solve_day09_part2 <- function(data_history) {
    data_history <- init(data_history) |> lapply(FUN = rev)
    return(vapply(X = data_history,
                  FUN = predict_next_val,
                  FUN.VALUE = numeric(1)) |> sum())
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day09_part1(history_example)
solve_day09_part1(history)


## Part 2 ----------------------------------------------------------------------

solve_day09_part2(history_example)
solve_day09_part2(history)

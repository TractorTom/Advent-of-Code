# Titre : day02.R
# Auteur : Tanguy

##############
##  DAY 02  ##
##############


# Import data -------------------------------------------------------------

games <- readLines("./2023/Day02/record.txt")
games_example <- readLines("./2023/Day02/record_example.txt")


# Déclaration fonction ----------------------------------------------------

input <- c("red" = 12, "green" = 13, "blue" = 14)

get_sum_id <- function(bags) {
    return(all(input[bags$V2] >= bags$V1))
}

get_power <- function(bags) {
    return(prod(bags$V1))
}

aggregate_bags <- function(bags) {
    summary_tab <- bags |>
        strsplit(" ") |>
        do.call(what = rbind) |>
        as.data.frame()

    summary_tab$V1 <- as.integer(summary_tab$V1)

    return(aggregate(V1 ~ V2, data = summary_tab, FUN = max))
}

init <- function(data_games) {
    return(data_games |>
               strsplit(": ") |>
               vapply(FUN.VALUE = character(1), FUN = `[`, 2) |>
               strsplit("\\; |\\, ") |>
               lapply(FUN = aggregate_bags))
}

solve_day02_part1 <- function(data_games) {
    summarized_bags <- init(data_games)
    return(
        sum(seq_along(data_games)
            * vapply(X = summarized_bags,
                     FUN = get_sum_id,
                     FUN.VALUE = logical(1)))
    )
}

solve_day02_part2 <- function(data_games) {
    summarized_bags <- init(data_games)
    return(sum(vapply(X = summarized_bags,
                      FUN = get_power,
                      FUN.VALUE = numeric(1))))
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day02_part1(games_example)
solve_day02_part1(games)


## Part 2 ------------------------------------------------------------------

solve_day02_part2(games_example)
solve_day02_part2(games)

# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############


# Import data -------------------------------------------------------------

scratchcards <- readLines("./2023/Day04/scratchcards.txt")
scratchcards_example <- readLines("./2023/Day04/scratchcards_example.txt")


# Déclaration fonction ----------------------------------------------------

init <- function(data_scratchcards) {
    return(data_scratchcards |>
               strsplit(split = ": ") |>
               vapply(FUN = `[`, FUN.VALUE = character(1), 2) |>
               strsplit(" | ", fixed = TRUE))
}

solve_day04_part1 <- function(data_scratchcards) {
    data_scratchcards <- init(data_scratchcards)

    somme <- 0
    for (k in seq_along(data_scratchcards)) {

        v1 <- data_scratchcards[[k]][1] |> strsplit(" ") |> unlist()
        v2 <- data_scratchcards[[k]][2] |> strsplit(" ") |> unlist()

        v1 <- v1[v1 != ""]
        v2 <- v2[v2 != ""]

        val <- 2 ** (sum(v2 %in% v1) - 1)

        if (val >= 1) {
            somme <- somme + val
        }
    }

    return(somme)
}

solve_day04_part2 <- function(data_scratchcards) {

    data_scratchcards <- init(data_scratchcards)

    weight <- rep(1, 2 * length(data_scratchcards))
    for (k in seq_along(data_scratchcards)) {

        v1 <- data_scratchcards[[k]][1] |> strsplit(" ") |> unlist()
        v2 <- data_scratchcards[[k]][2] |> strsplit(" ") |> unlist()

        v1 <- v1[v1 != ""]
        v2 <- v2[v2 != ""]

        match_number <- sum(v2 %in% v1)

        if (match_number > 0) {
            weight[k + seq_len(match_number)] <-
                weight[k + seq_len(match_number)] + weight[k]
        }
    }

    return(sum(weight[seq_along(data_scratchcards)]))
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day04_part1(scratchcards_example)
solve_day04_part1(scratchcards)


## Part 2 ------------------------------------------------------------------

solve_day04_part2(scratchcards_example)
solve_day04_part2(scratchcards)

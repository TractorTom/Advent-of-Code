#!/usr/bin/env r

# Titre : day13.R
# Auteur : Tanguy

##############
##  DAY 13  ##
##############


# Import data ------------------------------------------------------------------

claws <- readLines(file.path("2024", "Day13", "claws.txt"))
claws_example <- readLines(file.path("2024", "Day13", "claws_example.txt"))


# Déclaration fonction ---------------------------------------------------------

source(file.path("R", "utils.R"))

read_claws_data <- function(input_claws) {
    pattern <- "^(?:Button [AB]|Prize): X[+=](\\d+), Y[+=](\\d+)$"
    proto <- list(X = integer(), Y = integer())
    equations <- input_claws[input_claws != ""] |>
        extract_regex(pattern, proto) |>
        as.matrix()
    return(equations)
}

solve_equation <- function(mat) {
    if (det(mat) == 0L) return(NULL)
    return((solve(mat) * det(mat)) |> round())
}

count_token <- function(data_claw, offset = 0L) {
    tokens <- 0L
    for (id_claw in (seq_len(nrow(data_claw) %/% 3L) - 1L)) {
        A <- t(data_claw[3L * id_claw + 1L:2L, ])
        Y <- offset + data_claw[3L * id_claw + 3L, ]
        A_1 <- solve_equation(A)
        d <- round(det(A))
        if (!is.null(A_1)) {
            new_tokens <- c(3L, 1L) %*% A_1 %*% Y
            if (new_tokens %% d == 0L) {
                tokens <- tokens + round(new_tokens %/% d)
            }
        }
    }
    return(tokens)
}

solve_day13_part1 <- function(input_claws) {
    data_claw <- read_claws_data(input_claws)
    tokens <- count_token(data_claw)
    return(tokens)
}

solve_day13_part2 <- function(input_claws) {
    data_claw <- read_claws_data(input_claws)
    tokens <- count_token(data_claw, offset = 10000000000000.0)
    return(tokens)
}

# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day13_part1(claws_example)
solve_day13_part1(claws)


## Part 2 ----------------------------------------------------------------------

solve_day13_part2(claws)

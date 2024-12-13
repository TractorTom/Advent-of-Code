#!/usr/bin/env r

# Titre : day13.R
# Auteur : Tanguy

##############
##  DAY 13  ##
##############


# Import data ------------------------------------------------------------------

claws <- read.table(file.path("2024", "Day13", "claws.txt"))
claws_example <- read.table(file.path("2024", "Day13", "claws_example.txt"))


# Déclaration fonction ---------------------------------------------------------

# Résolution du système
# u * xa + v + xb = x
# u * ya + v + yb = y
solve_equation <- function(xa, xb, x, ya, yb, y) {

    v <- (x * ya - y * xa) %/% (xb * ya - yb * xa)
    u <- (x - v * xb) %/% xa

    if ((x * ya - y * xa) %% (xb * ya - yb * xa) == 0L
        && (x - v * xb) %% xa == 0L) {
        return(c(u, v))
    }
    return(NULL)
}

count_token <- function(data_claw, offset = 0L) {
    tokens <- 0L
    for (id_claw in (seq_len(nrow(data_claw) %/% 3L) - 1L)) {
        equations <- as.list(c(data_claw[1L:3L + 3L * id_claw, 1L],
                               data_claw[1L:3L + 3L * id_claw, 2L]) +
                                 c(0L, 0L, offset, 0L, 0L, offset))
        new_tokens <- do.call(solve_equation, equations)
        tokens <- tokens + sum(c(3L, 1L) * new_tokens)
    }
    return(tokens)
}

solve_day13_part1 <- function(data_claw) {
    tokens <- count_token(data_claw)
    return(tokens)
}

solve_day13_part2 <- function(data_claw) {
    tokens <- count_token(data_claw, offset = 10000000000000)
    return(tokens)
}

# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day13_part1(claws_example)
solve_day13_part1(claws)


## Part 2 ----------------------------------------------------------------------

solve_day13_part2(claws)

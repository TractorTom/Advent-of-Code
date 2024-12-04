# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############


# Import data ------------------------------------------------------------------

words <- readLines(file.path("2024", "Day04", "words.txt"))
words_example <- readLines(file.path("2024", "Day04", "words_example.txt"))


# Déclaration fonction ---------------------------------------------------------

get_diagonal <- function(x, k) {
    diagonal <- paste0(x[row(x) == col(x) + k], collapse = "")
    return(diagonal)
}

get_anti_diagonal <- function(x, k) {
    x <- x[rev(seq_len(nrow(x))), ]
    anti_diagonal <- paste0(x[row(x) == col(x) + k], collapse = "")
    return(anti_diagonal)
}

get_horizontal <- function(x, k) {
    horizontal <- paste0(x[k, ], collapse = "")
    return(horizontal)
}

get_vertical <- function(x, k) {
    vertical <- paste0(x[, k], collapse = "")
    return(vertical)
}

count_xmas <- function(x) {
    xmas_occurencies <- gregexpr(
        pattern = "(?=XMAS|SAMX)",
        text = x,
        perl = TRUE
    ) |>
        unlist()
    nb_xmas <- length(xmas_occurencies[xmas_occurencies != -1L])
    return(nb_xmas)
}

solve_day04_part1 <- function(data_words) {

    words_matrix <- data_words |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind)

    m_dim <- min(dim(words_matrix)) - 1L

    diagonals <- lapply(
        X = -m_dim:m_dim,
        FUN = get_diagonal,
        x = words_matrix
    )
    anti_diagonals <- lapply(
        X = -m_dim:m_dim,
        FUN = get_anti_diagonal,
        x = words_matrix
    )
    horizontal <- lapply(
        X = seq_len(nrow(words_matrix)),
        FUN = get_horizontal,
        x = words_matrix
    )
    vertical <- lapply(
        X = seq_len(nrow(words_matrix)),
        FUN = get_vertical,
        x = words_matrix
    )

    count <- lapply(
        X = c(diagonals, anti_diagonals, vertical, horizontal),
        FUN = count_xmas
    ) |>
        as.numeric() |>
        sum()
    return(count)
}

solve_day04_part2 <- function(data_words) {

    words_matrix <- data_words |>
        strsplit(split = "", fixed = TRUE) |>
        do.call(what = rbind)

    count_x_mas <- 0L
    for (row_k in seq_len(nrow(words_matrix) - 2L)) {
        for (col_k in seq_len(ncol(words_matrix) - 2L)) {
            mini_matrix <- words_matrix[row_k:(row_k + 2L), col_k:(col_k + 2L)]
            diagonal <- paste0(diag(mini_matrix), collapse = "")
            anti_diagonal <- paste0(diag(mini_matrix[3L:1L, ]), collapse = "")

            if (diagonal %in% c("MAS", "SAM")
                && anti_diagonal %in% c("MAS", "SAM")) {
                count_x_mas <- count_x_mas + 1L
            }
        }
    }
    return(count_x_mas)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day04_part1(words_example)
solve_day04_part1(words)


## Part 2 ----------------------------------------------------------------------

solve_day04_part2(words_example)
solve_day04_part2(words)

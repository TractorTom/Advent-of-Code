# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############

###### IMPORT PACKAGE######

library("magrittr")

###### IMPORT DATA ######

set_of_board_example <- readLines(file.path("2021", "Day04", "board_example.txt"))
set_of_board <- readLines(file.path("2021", "Day04", "board.txt"))

###### TRAITEMENT DATA ######

traitement <- function(data_board) {
    list_nums <- as.numeric(strsplit(data_board[1], split = ",")[[1]])
    index_1 <- 3
    list_boards <- list()

    for (index_2 in 3:length(data_board)) {
        if (data_board[index_2] == "") {
            board <- data_board[index_1:(index_2 - 1)] %>%
                strsplit(split = " ") %>%
                lapply(FUN = as.numeric) %>%
                lapply(FUN = function(line) line[!is.na(line)]) %>%
                do.call(what = rbind)
            index_1 <- index_2 + 1
            list_boards <- c(list_boards, list(board))
        }
    }

    board <- data_board[index_1:length(data_board)] %>%
        strsplit(split = " ") %>%
        lapply(FUN = as.numeric) %>%
        lapply(FUN = function(line) line[!is.na(line)]) %>%
        do.call(what = rbind)

    list_boards <- c(list_boards, list(board))

    return(list(
        nums = list_nums,
        boards = list_boards
    ))
}

set_of_board <- traitement(set_of_board)
set_of_board_example <- traitement(set_of_board_example)

###### DECLARATION FONCTION ######

solve_day04_part1 <- function(list_board) {
    completed_board <- list_board$board

    for (k in seq_along(list_board$num)) {
        num <- list_board$num[k]
        for (i in seq_along(completed_board)) {
            completed_board[[i]][completed_board[[i]] == num] <- -1
            board <- completed_board[[i]]

            row_sum <- rowSums(board)
            col_sum <- colSums(board)
            if (any(-5 == c(row_sum, col_sum))) {
                return(sum(board[board > 0]) * num)
            }
        }
    }
    stop("L'algorithme aurait dû finir.")
}

solve_day04_part2 <- function(list_board) {
    completed_board <- list_board$board
    index <- seq_along(completed_board)

    k <- 1
    while (length(index) > 1) {
        num <- list_board$num[k]
        index_temp <- index
        for (i in index_temp) {
            board <- completed_board[[i]]
            completed_board[[i]][board == num] <- NA

            for (col in seq_len(ncol(board))) {
                if (all(is.na(board[, col]))) index <- index[index != i]
            }
            for (lig in seq_len(nrow(board))) {
                if (all(is.na(board[lig, ]))) index <- index[index != i]
            }
        }
        k <- k + 1
    }

    last_board <- completed_board[[index]]
    test_aligne <- 0 %in% c(
        colSums(last_board, na.rm = TRUE),
        rowSums(last_board, na.rm = TRUE)
    )

    while (!test_aligne && k <= length(list_board$num)) {
        num <- list_board$num[k]
        last_board[last_board == num] <- NA
        test_aligne <- 0 %in% c(
            colSums(last_board, na.rm = TRUE),
            rowSums(last_board, na.rm = TRUE)
        )
        k <- k + 1
    }

    return(sum(last_board, na.rm = TRUE) * num)
}

###### EXECUTION ######

solve_day04_part1(list_board = set_of_board_example)
solve_day04_part1(list_board = set_of_board)

solve_day04_part1_bis(list_board = set_of_board_example)
solve_day04_part1_bis(list_board = set_of_board)

solve_day04_part2(list_board = set_of_board_example)
solve_day04_part2(list_board = set_of_board)

microbenchmark::microbenchmark(
    v1 =
        solve_day04_part1(list_board = set_of_board),
    v2 = solve_day04_part1_bis(list_board = set_of_board)
)

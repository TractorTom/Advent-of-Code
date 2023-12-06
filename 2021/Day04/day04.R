# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############

###### IMPORT PACKAGE######

library(magrittr)

###### IMPORT DATA ######

set_of_board_example <- readLines("./2021/Day04/board_example.txt")
set_of_board <- readLines("./2021/Day04/board.txt")

###### TRAITEMENT DATA ######

traitement <- function(dataBoard) {
    list_nums <- as.numeric(strsplit(dataBoard[1], split = ",")[[1]])
    index_1 <- 3
    list_boards <- list()

    for (index_2 in 3:length(dataBoard)) {
        if (dataBoard[index_2] == "") {
            board <- dataBoard[index_1:(index_2 - 1)] %>%
                strsplit(split = " ") %>%
                lapply(FUN = as.numeric) %>%
                lapply(FUN = function(line) line[!is.na(line)]) %>%
                do.call(what = rbind)
            index_1 <- index_2 + 1
            list_boards <- c(list_boards, list(board))
        }
    }

    board <- dataBoard[index_1:length(dataBoard)] %>%
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

solve_day04_part1 <- function(listBoard) {
    completedBoard <- listBoard$board

    for (k in seq_along(listBoard$num)) {
        num <- listBoard$num[k]
        for (i in seq_along(completedBoard)) {
            completedBoard[[i]][completedBoard[[i]] == num] <- NA
            board <- completedBoard[[i]]

            for (col in 1:5) {
                if (all(is.na(board[, col]))) {
                    return(sum(board, na.rm = TRUE) * num)
                }
            }
            for (lig in 1:5) {
                if (all(is.na(board[lig, ]))) {
                    return(sum(board, na.rm = TRUE) * num)
                }
            }
        }
    }
    stop("L'algorithme aurait dû finir.")
}

solve_day04_part2 <- function(listBoard) {
    completedBoard <- listBoard$board
    index <- seq_along(completedBoard)

    k <- 1
    while (length(index) > 1) {
        num <- listBoard$num[k]
        index_temp <- index
        for (i in index_temp) {
            board <- completedBoard[[i]]
            completedBoard[[i]][board == num] <- NA

            for (col in seq_len(ncol(board))) {
                if (all(is.na(board[, col]))) index <- index[index != i]
            }
            for (lig in seq_len(nrow(board))) {
                if (all(is.na(board[lig, ]))) index <- index[index != i]
            }
        }
        k <- k + 1
    }

    lastBoard <- completedBoard[[index]]
    test_aligne <- 0 %in% c(
        apply(lastBoard, 2, sum, na.rm = TRUE),
        apply(lastBoard, 1, sum, na.rm = TRUE)
    )

    while (!test_aligne && k <= length(listBoard$num)) {
        num <- listBoard$num[k]
        lastBoard[lastBoard == num] <- NA
        test_aligne <- 0 %in% c(
            apply(lastBoard, 1, sum, na.rm = TRUE),
            apply(lastBoard, 2, sum, na.rm = TRUE)
        )
        k <- k + 1
    }

    return(sum(lastBoard, na.rm = TRUE) * num)
}

solve_day04_part1(listBoard = set_of_board_example)
solve_day04_part1(listBoard = set_of_board)

solve_day04_part2(listBoard = set_of_board_example)
solve_day04_part2(listBoard = set_of_board)

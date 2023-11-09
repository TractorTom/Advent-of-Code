# Titre : day02.R
# Auteur : Tanguy

##############
##  DAY 02  ##
##############


# Import data -------------------------------------------------------------

result_game <- read.table("./2022/Day02/game_rock_paper_scissors.txt")
result_game_example <- read.table("./2022/Day02/game_rock_paper_scissors_example.txt")


# Déclaration fonction ----------------------------------------------------

hands <- c("Pierre", "Papier", "Ciseaux")

translate_table <- data.frame(
    hand = hands,
    abc_encryption = LETTERS[1:3],
    xyz_encryption = LETTERS[24:26],
    score_result = 0:2,
    score = 1:3
)

compute_winner <- (function(hand1, hand2) {
    difference <- which(hands == hand1) - which(hands == hand2)
    return((difference + 1) %% 3)
}) |> Vectorize(USE.NAMES = FALSE)

solve_day02_part1 <- function(data_game) {
    data_game |>
        dplyr::rename(
            elf_choice = 1,
            my_choice = 2
        ) |>
        merge(
            y = translate_table[, c("hand", "abc_encryption")],
            by.x = "elf_choice", by.y = "abc_encryption"
        ) |>
        merge(
            y = translate_table[, c("hand", "xyz_encryption")],
            by.x = "my_choice", by.y = "xyz_encryption"
        ) |>
        dplyr::rename(my_hand = hand.y, elf_hand = hand.x) |>
        merge(
            y = translate_table[, c("hand", "score")],
            by.x = "my_hand", by.y = "hand"
        ) |>
        dplyr::mutate(score = compute_winner(my_hand, elf_hand) * 3 + score) |>
        dplyr::pull(score) |>
        sum()
}

solve_day02_part2 <- function(data_game) {
    data_game |>
        dplyr::rename(
            elf_choice = 1,
            enc_result = 2
        ) |>
        merge(
            y = translate_table[, c("xyz_encryption", "score_result")],
            by.x = "enc_result", by.y = "xyz_encryption"
        ) |>
        merge(
            y = translate_table[, c("abc_encryption", "score_result")],
            by.x = "elf_choice", by.y = "abc_encryption"
        ) |>
        dplyr::rename(score_hand = score_result.y, score_result = score_result.x) |>
        dplyr::mutate(score = score_result * 3 + (score_hand + score_result - 1) %% 3 + 1) |>
        dplyr::pull(score) |>
        sum()
}


# Execution ---------------------------------------------------------------

solve_day02_part1(result_game_example)
solve_day02_part1(result_game)

solve_day02_part2(result_game_example)
solve_day02_part2(result_game)

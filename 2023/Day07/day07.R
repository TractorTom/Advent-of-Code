# Titre : day07.R
# Auteur : Tanguy

##############
##  DAY 07  ##
##############


# Import data ------------------------------------------------------------------

hands <- read.table("./2023/Day07/poker_hands.txt")
hands_example <- read.table("./2023/Day07/poker_hands_example.txt")


# Déclaration fonction ---------------------------------------------------------

type <- function(hand) {
    all_cards <- strsplit(hand, split = "") |> unlist()
    repartition <- table(all_cards)

    if (length(unique(all_cards)) == 1) {
        return(7)
    } else if (length(unique(all_cards)) == 2) {
        if (all(repartition %in% c(1, 4))) {
            return(6)
        } else if (all(repartition %in% c(2, 3))) {
            return(5)
        }
    } else if (max(repartition) == 3) {
        return(4)
    } else if (length(unique(all_cards)) == 3) {
        return(3)
    } else if (length(unique(all_cards)) == 4) {
        return(2)
    } else {
        return(1)
    }
}

rename_cards <- function(data_cards, J_value) {
    data_cards$renaming_hand <- data_cards$V1 |>
        gsub(pattern = "T", replacement = "a") |>
        gsub(pattern = "J", replacement = J_value) |>
        gsub(pattern = "Q", replacement = "c") |>
        gsub(pattern = "K", replacement = "d") |>
        gsub(pattern = "A", replacement = "e")

    return(data_cards)
}

sort_cards <- function(data_cards, J_value) {
    data_cards <- rename_cards(data_cards, J_value)

    hands <- data_cards$renaming_hand
    scores <- data_cards$rank
    data_cards <- data_cards[order(scores, hands), ]

    return(data_cards)
}

solve_day07_part1 <- function(data_cards) {
    data_cards$rank <- sapply(data_cards$V1, type)
    data_cards <- sort_cards(data_cards, J_value = "b")

    return(sum(seq_len(nrow(data_cards)) * data_cards$V2))
}

type_p2 <- function(hand) {

    type_record <- type(hand)
    if (grepl("J", hand)) {

        all_cards <- strsplit(hand, split = "") |> unlist()

        for (val in all_cards) {
            if (val != "J") {
                new_card <- hand |> gsub(pattern = "J", replacement = val)
                type_n <- type(new_card)

                if (type_n > type_record) {
                    type_record <- type_n
                }
            }
        }
    }

    return(type_record)
}

solve_day07_part2 <- function(data_cards) {
    data_cards$rank <- sapply(data_cards$V1, type_p2)
    data_cards <- sort_cards(data_cards, J_value = "1")

    return(sum(seq_len(nrow(data_cards)) * data_cards$V2))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day07_part1(hands_example)
solve_day07_part1(hands)


## Part 2 ----------------------------------------------------------------------

solve_day07_part2(hands_example)
solve_day07_part2(hands)

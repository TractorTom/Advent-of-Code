# Titre : day07.R
# Auteur : Tanguy

##############
##  DAY 07  ##
##############

###### IMPORT DATA ######

position_crab_example <- readLines(
    con = "./2021/Day07/horizontal_position_example.txt"
)
position_crab <- readLines("./2021/Day07/horizontal_position.txt")

###### TRAITEMENT DATAS######

position_crab_example <- as.numeric(strsplit(
    x = position_crab_example,
    split = ","
)[[1]])
position_crab <- as.numeric(strsplit(x = position_crab, split = ",")[[1]])

###### DECLARATION FONCTIONS ######

score <- function(val) {
    return(val * (val + 1) / 2)
}

solve_day07_part1 <- function(data_position) {
    index <- median(data_position)
    return(sum(abs(data_position - index)))
}

solve_day07_part2 <- function(data_position) {
    return(min(
        sum(score(abs(data_position - floor(mean(data_position))))),
        sum(score(abs(data_position - floor(mean(data_position)) - 1)))
    ))
}

###### EXECUTION ######

solve_day07_part1(data_position = position_crab_example)
solve_day07_part1(data_position = position_crab)

solve_day07_part2(data_position = position_crab_example)
solve_day07_part2(data_position = position_crab)

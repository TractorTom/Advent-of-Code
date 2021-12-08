#Titre : day07.R
#Auteur : Tanguy

##############
##  DAY 07  ##
##############

######IMPORT DATA######

position_crab_example <- readLines("./2021/Day07/horizontal_position_example.txt")
position_crab <- readLines("./2021/Day07/horizontal_position.txt")

######TRAITEMENT DATAS######

position_crab_example <- as.numeric(strsplit(position_crab_example, split = ",")[[1]])
position_crab <- as.numeric(strsplit(position_crab, split = ",")[[1]])

######DECLARATION FONCTIONS######

score <- function(val){
    return(val * (val + 1) / 2)
}

solve_day07_part1 <- function(dataPosition){
    index <- median(dataPosition)
    return(sum(abs(dataPosition - index)))
}

solve_day07_part2 <- function(dataPosition){
    return(min(sum(score(abs(dataPosition - floor(mean(dataPosition))))), 
               sum(score(abs(dataPosition - floor(mean(dataPosition)) - 1)))
               )
    )
}

######EXECUTION######

solve_day07_part1(dataPosition = position_crab_example)
solve_day07_part1(dataPosition = position_crab)

solve_day07_part2(dataPosition = position_crab_example)
solve_day07_part2(dataPosition = position_crab)

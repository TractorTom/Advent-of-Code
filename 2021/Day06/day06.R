#Titre : day06.R
#Auteur : Tanguy

##############
##  DAY 06  ##
##############

options(digits = 20)

######IMPORT DATA######

internal_time_lanternfish_example <- readLines("./2021/Day06/internal_time_example.txt")
internal_time_lanternfish <- readLines("./2021/Day06/internal_time.txt")

######DECLARATION FONCTIONS######

count_fish <- function(number_fish, day){
    
    number_fish <- as.numeric(strsplit(number_fish, ",")[[1]])
    dataFish <- matrix(0, ncol = 9, nrow = day + 1)
    
    for (k in number_fish){
        dataFish[1, k + 1] <-  dataFish[1, k + 1] + 1
    }
    
    for (jour in 2:(day + 1)){ 
        dataFish[jour, 8 + 1] <- dataFish[jour - 1, 0 +1]
        dataFish[jour, 7 + 1] <- dataFish[jour - 1, 8 +1]
        dataFish[jour, 6 + 1] <- dataFish[jour - 1, 7 + 1] + dataFish[jour - 1, 0 + 1]
        for (timer in 0:5){
            dataFish[jour, timer + 1] <- dataFish[jour - 1, timer + 2]
        }
    }
    return(sum(dataFish[day + 1, ]))
}

######EXECUTION######

count_fish(internal_time_lanternfish_example, 80)
count_fish(internal_time_lanternfish, 80)

count_fish(internal_time_lanternfish_example, 256)
count_fish(internal_time_lanternfish, 256)

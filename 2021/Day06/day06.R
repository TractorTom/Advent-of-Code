# Titre : day06.R
# Auteur : Tanguy

##############
##  DAY 06  ##
##############

###### IMPORT DATA ######

time_lanternfish_example <- readLines(
    con = "./2021/Day06/internal_time_example.txt"
)
time_lanternfish <- readLines("./2021/Day06/internal_time.txt")

###### DECLARATION FONCTIONS ######

count_fish <- function(number_fish, day) {
    number_fish <- as.numeric(strsplit(number_fish, ",")[[1]])
    data_fish <- matrix(0, ncol = 9, nrow = day + 1)

    for (k in number_fish) {
        data_fish[1, k + 1] <- data_fish[1, k + 1] + 1
    }

    for (jour in 2:(day + 1)) {
        data_fish[jour, 8 + 1] <- data_fish[jour - 1, 0 + 1]
        data_fish[jour, 7 + 1] <- data_fish[jour - 1, 8 + 1]
        data_fish[jour, 6 + 1] <- data_fish[jour - 1, 7 + 1] +
            data_fish[jour - 1, 0 + 1]
        for (timer in 0:5) {
            data_fish[jour, timer + 1] <- data_fish[jour - 1, timer + 2]
        }
    }
    return(sum(data_fish[day + 1, ]))
}

###### EXECUTION ######

count_fish(time_lanternfish_example, 80)
count_fish(time_lanternfish, 80)

count_fish(time_lanternfish_example, 256)
count_fish(time_lanternfish, 256) |> 
    dput()

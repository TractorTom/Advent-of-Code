# Titre : day02.R
# Auteur : Tanguy

##############
##  DAY 02  ##
##############

###### IMPORT DATA ######

course_example <- read.table("./2021/Day02/planned_course_example.txt")
course <- read.table("./2021/Day02/planned_course.txt")

###### DECLARATION FONCTION ######

solve_day02_part1 <- function(dataPlan) {
    forward <- sum(as.double(dataPlan[dataPlan$V1 == "forward", "V2"]))
    up <- sum(as.double(dataPlan[dataPlan$V1 == "up", "V2"]))
    down <- sum(as.double(dataPlan[dataPlan$V1 == "down", "V2"]))

    depth <- down - up
    return(forward * depth)
}

solve_day02_part2 <- function(dataPlan) {
    forward <- sum(as.double(dataPlan[dataPlan$V1 == "forward", "V2"]))
    depth <- 0
    aim <- 0

    list_index_forward <- (seq_len(nrow(dataPlan)))[dataPlan$V1 == "forward"]

    for (k in seq_len(list_index_forward)) {
        index <- list_index_forward[k]
        if (index != 1) {
            if (k == 1) {
                index_prec <- 1
            } else {
                index_prec <- list_index_forward[k - 1]
            }

            if (index - index_prec > 1) {
                sub_data <- dataPlan[index_prec:index, ]
                up <- sum(as.double(sub_data[sub_data$V1 == "up", "V2"]))
                down <- sum(as.double(sub_data[sub_data$V1 == "down", "V2"]))
                aim <- aim + down - up
            }

            depth <- depth + aim * dataPlan[index, "V2"]
        }
    }

    return(depth * forward)
}

###### EXECUTION ######

solve_day02_part1(dataPlan = course_example)
solve_day02_part1(dataPlan = course)

solve_day02_part2(dataPlan = course_example)
solve_day02_part2(dataPlan = course)

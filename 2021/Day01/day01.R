# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############

###### IMPORT DATA ######

report <- read.table("./2021/Day01/sonar_report.txt")[, 1]
report_example <- read.table("./2021/Day01/sonar_report_example.txt")[, 1]

###### DECLARATION FONCTION ######

count_increases <- function(serie) {
    return(sum(serie[-1] - serie[-length(serie)] > 0))
}

compute_sliding_window <- function(serie) {
    return(serie[-c(length(serie), length(serie) - 1)] +
        serie[-c(length(serie), 1)] +
        serie[-(1:2)])
}

###### EXECUTION ######

count_increases(serie = report_example)
count_increases(serie = report)

count_increases(compute_sliding_window(serie = report_example))
count_increases(compute_sliding_window(serie = report))

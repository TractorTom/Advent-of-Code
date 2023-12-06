# Titre : day03.R
# Auteur : Tanguy

##############
##  DAY 03  ##
##############

###### IMPORT DATA ######

diagnostic_report_example <- readLines(
    con = "./2021/Day03/diagnostic_report_example.txt"
)
diagnostic_report <- readLines("./2021/Day03/diagnostic_report.txt")

###### TRAITEMENT DATA ######

diagnostic_report_example <- t(matrix(
    data = as.numeric(do.call(
        what = c,
        args = strsplit(diagnostic_report_example, split = "")
    )),
    ncol = length(diagnostic_report_example)
))

diagnostic_report <- t(matrix(
    data = as.numeric(do.call(
        what = c,
        args = strsplit(diagnostic_report, split = "")
    )),
    ncol = length(diagnostic_report)
))

###### DECLARATION FONCTION ######

solve_day03_part1 <- function(dataDiagnostic) {
    gamma <- apply(dataDiagnostic, 2, mean) >= .5
    epsilon <- 1 - gamma

    gamma <- sum(gamma * 2**((length(gamma) - 1):0))
    epsilon <- sum(epsilon * 2**((length(epsilon) - 1):0))

    return(gamma * epsilon)
}

solve_day03_part2 <- function(dataDiagnostic) {
    dataTemp <- dataDiagnostic
    k <- 1
    while (!is.null(dim(dataTemp)) && sum(!duplicated(dataTemp)) != 1) {
        val_mod <- (mean(dataTemp[, k]) >= .5)
        dataTemp <- dataTemp[dataTemp[, k] == val_mod, ]
        k <- k + 1
    }
    if (is.null(dim(dataTemp))) {
        O2_generator <- sum(dataTemp * 2**((length(dataTemp) - 1):0))
    } else {
        O2_generator <- sum(dataTemp[1, ] * 2**((length(dataTemp[1, ]) - 1):0))
    }

    dataTemp <- dataDiagnostic
    k <- 1
    while (!is.null(dim(dataTemp)) && sum(!duplicated(dataTemp)) != 1) {
        val_non_mod <- 1 - (mean(dataTemp[, k]) >= .5)
        if (!val_non_mod %in% dataTemp[, k]) val_non_mod <- 1 - val_non_mod
        dataTemp <- dataTemp[dataTemp[, k] == val_non_mod, ]
        k <- k + 1
    }
    if (is.null(dim(dataTemp))) {
        CO2_scrubber <- sum(dataTemp * 2**((length(dataTemp) - 1):0))
    } else {
        CO2_scrubber <- sum(dataTemp[1, ] * 2**((length(dataTemp[1, ]) - 1):0))
    }

    return(O2_generator * CO2_scrubber)
}

###### EXECUTION ######

solve_day03_part1(dataDiagnostic = diagnostic_report_example)
solve_day03_part1(dataDiagnostic = diagnostic_report)

solve_day03_part2(dataDiagnostic = diagnostic_report_example)
solve_day03_part2(dataDiagnostic = diagnostic_report)

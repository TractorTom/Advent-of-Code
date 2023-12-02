# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############


# Import data -------------------------------------------------------------

calibration <- readLines("./2023/Day01/calibration.txt")
calibration_example <- readLines("./2023/Day01/calibration_example.txt")
calibration_example2 <- readLines("./2023/Day01/calibration_example2.txt")


# Déclaration fonction ----------------------------------------------------

str_rev <- function(x) {
    if (length(x) == 0) return(x)
    return(c(
        strsplit(x[1], "")[[1]] |>
            rev() |>
            paste0(collapse = ""),
        str_rev(x[-1])
    ))
}

number_letter <- c("one", "two", "three", "four", "five",
                   "six", "seven", "eight", "nine")
n <- max(nchar(number_letter))
str_number_letter <- str_rev(number_letter)

get_first_digit_p1 <- function(encrypted_calibration) {
    for (index in seq_len(nchar(encrypted_calibration))) {
        if (substr(encrypted_calibration, index, index) %in% 1:9) {
            return(as.integer(substr(encrypted_calibration, index, index)))
        }
    }
    stop("No digits")
}

get_first_digit_p2 <- function(encrypted_calibration, v) {
    i <- 1
    while (i <= nchar(encrypted_calibration)) {
        j <- 0
        digit <- substr(encrypted_calibration, i, i)
        if (digit %in% 1:9) {
            return(as.integer(digit))
        }
        while (j <= n) {
            digit <- substr(encrypted_calibration, i, i + j)
            if (digit %in% v) {
                return(which(digit == v))
            } else {
                j <- j + 1
            }
        }
        i <- i + 1
    }
    stop("No digits")
}

solve_day01_part1 <- function(data_calibration) {
    s <- 0
    for (encrypted_calibration in data_calibration) {
        n1 <- get_first_digit_p1(encrypted_calibration)
        n2 <- get_first_digit_p1(str_rev(encrypted_calibration))
        s <- s + n1 * 10 + n2
    }
    return(s)
}

solve_day01_part2 <- function(data_calibration) {
    s <- 0
    for (encrypted_calibration in data_calibration) {
        n1 <- get_first_digit_p2(encrypted_calibration, number_letter)
        n2 <- get_first_digit_p2(
            str_rev(encrypted_calibration),
            str_number_letter
        )
        s <- s + n1 * 10 + n2
    }
    return(s)
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day01_part1(calibration_example)
solve_day01_part1(calibration)

## Part 2 ------------------------------------------------------------------

solve_day01_part2(calibration_example2)
solve_day01_part2(calibration)

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

get_first_digit_p1 <- function(k) {
    for (i in seq_len(nchar(k))) {
        if (substr(k, i, i) %in% 1:9) {
            return(as.integer(substr(k, i, i)))
        }
    }
    stop("No digits")
}

get_last_digit_p1 <- function(k) {
    for (i in rev(seq_len(nchar(k)))) {
        if (substr(k, i, i) %in% 1:9) {
            return(as.integer(substr(k, i, i)))
        }
    }
    stop("No digits")
}

solve_day01_part1 <- function(data_calibration) {
    s <- 0
    for (k in data_calibration) {
        n1 <- get_first_digit_p1(k)
        n2 <- get_last_digit_p1(k)
        s <- s + n1 * 10 + n2
    }
    return(s)
}


v <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
n <- max(nchar(v))

get_first_digit_p2 <- function(k) {
    i <- 1
    while (i <= nchar(k)) {
        j <- 0
        while (j <= n && i + j <= nchar(k)) {
            digit <- substr(k, i, i + j)
            if (digit %in% v) {
                return(which(digit == v))
            } else if (digit  %in% 1:9) {
                return(as.integer(digit))
            } else {
                j <- j + 1
            }
        }
        i <- i + 1
    }
    stop("No digits")
}

get_last_digit_p2 <- function(k) {
    i <- nchar(k)
    while (i >= 1) {
        j <- 0
        while (j <= n && i + j <= nchar(k)) {
            digit <- substr(k, i, i + j)
            if (digit %in% v) {
                return(which(digit == v))
            } else if (digit  %in% 1:9) {
                return(as.integer(digit))
            } else {
                j <- j + 1
            }
        }
        i <- i - 1
    }
    stop("No digits")
}

solve_day01_part2 <- function(data_calibration) {
    
    s <- 0
    for (k in data_calibration) {
        n1 <- get_first_digit_p2(k)
        n2 <- get_last_digit_p2(k)
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
solve_day01_part2(calibration |> rep(10))

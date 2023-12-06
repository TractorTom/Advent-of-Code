# Titre : day06.R
# Auteur : Tanguy

##############
##  DAY 06  ##
##############


# Import data -------------------------------------------------------------

race <- readLines("./2023/Day06/race.txt") |> 
    read.table(text = _, row.names = 1L)
race_example <- readLines("./2023/Day06/race_example.txt") |> 
    read.table(text = _, row.names = 1L)


# Déclaration fonction ----------------------------------------------------

get_charge <- function(time, distance) {
    # Polynôme de degré 2
    a <- 1
    b <- -time
    c <- distance
    
    delta <- b**2 - 4*a*c
    
    ch1 <- (-b - sqrt(delta)) / 2
    ch2 <- (-b + sqrt(delta)) / 2
    
    return(ceiling(ch2 - 1) - floor(ch1 + 1) + 1)
}

# time <- paste0(Time, collapse = "") |> as.integer()
# distance <- paste0(Distance, collapse = "") |> as.numeric()


solve_day06_part1 <- function(data_race) {
    sapply(seq_len(ncol(data_race)), 
           FUN = \(i) get_charge(data_race["Time", i], data_race["Distance", i])) |> prod()
}

solve_day06_part2 <- function(data_race) {
    time <- data_race["Time", , drop = TRUE] |> 
        paste0(collapse = "") |> 
        as.numeric()
    distance <- data_race["Distance", , drop = TRUE] |> 
        paste0(collapse = "") |> 
        as.numeric()
    return(get_charge(time, distance))
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day06_part1(race_example)
solve_day06_part1(race)


## Part 2 ------------------------------------------------------------------

solve_day06_part2(race_example)
solve_day06_part2(race)

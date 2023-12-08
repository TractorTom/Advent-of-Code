# Titre : day08.R
# Auteur : Tanguy

##############
##  DAY 08  ##
##############


# Import network ---------------------------------------------------------------

network <- readLines("./2023/Day08/network.txt")
network_example <- readLines("./2023/Day08/network_example.txt")
network_example2 <- readLines("./2023/Day08/network_example2.txt")

options(digits = 22)


# Déclaration fonction ---------------------------------------------------------

traitement <- function(data_network) {
    instructions <- data_network[1] |> strsplit("") |> unlist()

    map <- read.table(text = data_network[c(-1, -2)])
    map$V2 <- NULL
    map$V3 <- substr(map$V3, start = 2, stop = 4)
    map$V4 <- substr(map$V4, start = 1, stop = 3)
    rownames(map) <- map$V1

    return(list(instructions = instructions, map = map))
}

pgcd <- function(a, b) {
    if (b == 0) {
        return(a)
    } else {
        return(pgcd(b, a %% b))
    }
}
ppcm <- function(a, b) a * b / (pgcd(a, b))

get_cycle <- function(starting, data_networks) {

    instructions <- data_networks$instructions
    map <- data_networks$map

    v_end <- NULL
    v_step <- NULL
    v_index <- NULL

    step <- 0
    pos <- starting
    last_step <- 0
    index <- 1

    while (sum((v_end == pos) & (v_index == index)) < 2) {

        index <- step %% length(instructions) + 1
        next_instruction <- (instructions[index] == "R") + 1
        pos <- map[pos, next_instruction + 1]
        step <- step + 1

        if (substr(pos, 3, 3) == "Z") {
            v_end <- c(v_end, pos)
            v_step <- c(v_step, step - last_step)
            last_step <- step
            v_index <- c(v_index, index)
        }
    }
    # Ici, le cycle est très particulier :
    # --> init = cycle = cte
    return(v_step[1])
}

solve_day08_part1 <- function(data_network) {
    data_network <- data_network |> traitement()
    return(get_cycle("AAA", data_network))
}

solve_day08_part2 <- function(data_network) {
    data_network <- data_network |> traitement()
    map <- data_network$map
    starting_point <- map$V1[(map$V1 |> substr(3, 3)) == "A"]
    return(starting_point |>
               sapply(FUN = get_cycle, data_network) |>
               Reduce(f = ppcm))
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day08_part1(network_example)
solve_day08_part1(network)


## Part 2 ------------------------------------------------------------------

solve_day08_part2(network_example2)
solve_day08_part2(network)

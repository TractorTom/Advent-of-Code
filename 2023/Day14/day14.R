# Titre : day14.R
# Auteur : Tanguy

##############
##  DAY 14  ##
##############


# Import data ------------------------------------------------------------------

platform <- readLines("./2023/Day14/platform.txt")
platform_example <- readLines("./2023/Day14/platform_example.txt")


# Déclaration fonction ---------------------------------------------------------

move_one_north <- function(data_platform, row, col) {
    if (data_platform[row, col] == "O") {
        k <- row - 1
        while (k > 0 && data_platform[k, col] == ".") {
            k <- k - 1
        }
        data_platform[row, col] <- "."
        data_platform[k + 1, col] <- "O"
    }
    return(data_platform)
}
move_north <- function(data_platform) {
    for (col in seq_len(ncol(data_platform))) {
        for (row in seq_len(nrow(data_platform))) {
            data_platform <- move_one_north(data_platform, row, col)
        }
    }
    return(data_platform)
}
move_west <- function(data_platform) {
    return(t(move_north(t(data_platform))))
}
move_south <- function(data_platform) {
    data_platform <- data_platform[rev(seq_len(nrow(data_platform))), ] |>
        move_north()
    return(data_platform[rev(seq_len(nrow(data_platform))), ])
}
move_east <- function(data_platform) {
    data_platform <- data_platform[, rev(seq_len(ncol(data_platform)))] |>
        move_west()
    return(data_platform[, rev(seq_len(ncol(data_platform)))])
}

move_cycle <- function(data_platform) {
    return(data_platform |>
               move_north() |>
               move_west() |>
               move_south() |>
               move_east())
}

count_load <- function(data_platform) {
    return(
        sum(rev(seq_len(nrow(data_platform))) * apply(
            X = data_platform,
            MARGIN = 1,
            FUN = \(x) sum(x == "O")
        ))
    )
}

rpz <- function(data_platform) {
    return(paste0(data_platform, collapse = ""))
}

compute_periodicity <- function(data_platform, nb_cycle) {

    load_memory <- NULL
    load_memory[rpz(data_platform)] <- count_load(data_platform)
    actual_cycle <- 0
    cond <- TRUE

    while (cond) {
        data_platform <- move_cycle(data_platform)
        rpz_k <- rpz(data_platform)
        actual_cycle <- actual_cycle + 1
        if (!is.na(load_memory[rpz_k])) {
            cond <- FALSE
            first_cycle <- which(names(load_memory) == rpz_k) - 1
            #-1 car on a ajouté l'état initial au début
            periodicity <- actual_cycle - first_cycle
        } else {
            load_memory[rpz_k] <- count_load(data_platform)
        }
    }
    last <- (nb_cycle - first_cycle) %% periodicity

    return(load_memory[first_cycle + last + 1])
}

solve_day14_part1 <- function(data_platform) {
    data_platform <- data_platform |>
        strsplit(split = "") |>
        do.call(what = rbind)

    somme <- count_load(data_platform |> move_north())

    return(somme)
}

solve_day14_part2 <- function(data_platform) {
    data_platform <- data_platform |>
        strsplit(split = "") |>
        do.call(what = rbind)

    somme <- compute_periodicity(data_platform, nb_cycle = 1000000000) |>
        as.numeric()
    return(somme)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day14_part1(platform_example)
solve_day14_part1(platform)


## Part 2 ----------------------------------------------------------------------

solve_day14_part2(platform_example)
solve_day14_part2(platform)

# Titre : day08.R
# Auteur : Tanguy

##############
##  DAY 08  ##
##############

###### IMPORT DATA ######

segments <- read.table("./2021/Day08/random_segments.txt")
segments_example <- read.table("./2021/Day08/random_segments_example.txt")

###### DECLARATION FONCTION ######

without <- function(x, wo) {
    x_str <- strsplit(x, "")[[1]]
    wo_str <- strsplit(wo, "")[[1]]
    return(x_str[!x_str %in% wo_str])
}

index_contained <- function(liste_chaine,
                            pattern,
                            is_deco = FALSE,
                            same = FALSE) {
    if (!is_deco) pattern <- strsplit(pattern, "")[[1]]
    return(vapply(
        X = liste_chaine, 
        FUN = function(code) {
            if (same) {
                return(all(pattern %in% strsplit(code, "")[[1]]) &&
                           length(pattern) == nchar(code))
            }
            return(all(pattern %in% strsplit(code, "")[[1]]))
        }, 
        FUN.VALUE = logical(1)
    ))
}

compute_motif <- function(signal) {
    motif <- data.frame(a = as.character(signal), b = rep(0, 10))

    motif[nchar(motif$a) == 2, "b"] <- 1
    motif[nchar(motif$a) == 3, "b"] <- 7
    motif[nchar(motif$a) == 4, "b"] <- 4
    motif[nchar(motif$a) == 7, "b"] <- 8

    motif[index_contained(liste_chaine = motif$a,
                          pattern = motif[motif$b == 4, "a"]) &
              motif$b == 0, "b"] <- 9
    motif[index_contained(liste_chaine = motif$a,
                          pattern = motif[motif$b == 1, "a"]) &
              motif$b == 0 &
              nchar(motif$a) == 5, "b"] <- 3

    trois_sans_un <- without(
        x = motif[motif$b == 3, "a"],
        wo = motif[motif$b == 1, "a"]
    )

    motif[index_contained(
        liste_chaine = motif$a,
        pattern = trois_sans_un, is_deco = TRUE
    ) & motif$b == 0 & nchar(motif$a) == 6, "b"] <- 6

    huit_sans_six <- without(
        x = motif[motif$b == 8, "a"],
        wo = motif[motif$b == 6, "a"]
    )

    motif[index_contained(liste_chaine = motif$a,
                          pattern = huit_sans_six, is_deco = TRUE) &
              motif$b == 0 & nchar(motif$a) == 5, "b"] <- 2

    motif[nchar(motif$a) == 5 & motif$b == 0, "b"] <- 5

    return(motif)
}

dispatch <- function(k) {
    if (k == 2)  return(1)
    if (k == 3)  return(3)
    if (k == 4)  return(2)
    if (k == 7)  return(4)
    stop("Error: wrong k")
}

solve_day08_part1 <- function(data_segment) {
    count <- c(0, 0, 0, 0)
    for (col in 12:15) {
        for (line in seq_len(nrow(data_segment))) {
            n <- nchar(data_segment[line, col])
            if (n %in% c(2:4, 7)) {
                index <- dispatch(n)
                count[index] <- count[index] + 1
            }
        }
    }
    return(sum(count))
}

solve_day08_part2 <- function(data_segment) {
    somme <- 0
    for (line in seq_len(nrow(data_segment))) {
        motif <- compute_motif(data_segment[line, 1:10])
        number <- 0
        for (col in 12:15) {
            chiffre <- motif[index_contained(
                liste_chaine = motif$a,
                pattern = data_segment[line, col],
                same = TRUE
            ), "b"]
            number <- 10 * number + chiffre
        }
        somme <- somme + number
    }
    return(somme)
}

###### EXECUTION ######

solve_day08_part1(data_segment = segments_example)
solve_day08_part1(data_segment = segments)

solve_day08_part2(data_segment = segments_example)
solve_day08_part2(data_segment = segments)

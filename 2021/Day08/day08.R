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

index_contained <- function(liste_chaine, pattern, is.deco = FALSE, same = FALSE) {
    if (!is.deco) pattern <- strsplit(pattern, "")[[1]]
    return(sapply(liste_chaine, FUN = function(code) {
        if (same) {
            return(all(pattern %in% strsplit(code, "")[[1]]) && length(pattern) == nchar(code))
        }
        return(all(pattern %in% strsplit(code, "")[[1]]))
    }))
}

compute_motif <- function(signal) {
    motif <- data.frame(a = as.character(signal), b = rep(0, 10))
    
    motif[nchar(motif$a) == 2, "b"] <- 1
    motif[nchar(motif$a) == 3, "b"] <- 7
    motif[nchar(motif$a) == 4, "b"] <- 4
    motif[nchar(motif$a) == 7, "b"] <- 8
    
    motif[index_contained(liste_chaine = motif$a, pattern = motif[motif$b == 4, "a"]) &
              motif$b == 0, "b"] <- 9
    motif[index_contained(liste_chaine = motif$a, pattern = motif[motif$b == 1, "a"]) &
              motif$b == 0 && nchar(motif$a) == 5, "b"] <- 3
    trois_sans_un <- without(x = motif[motif$b == 3, "a"], wo = motif[motif$b == 1, "a"])
    motif[index_contained(liste_chaine = motif$a, pattern = trois_sans_un, is.deco = TRUE) &
              motif$b == 0 && nchar(motif$a) == 6, "b"] <- 6
    huit_sans_six <- without(x = motif[motif$b == 8, "a"], wo = motif[motif$b == 6, "a"])
    motif[index_contained(liste_chaine = motif$a, pattern = huit_sans_six, is.deco = TRUE) &
              motif$b == 0 && nchar(motif$a) == 5, "b"] <- 2
    
    motif[nchar(motif$a) == 5 && motif$b == 0, "b"] <- 5
    
    return(motif)
}

solve_day08_part1 <- function(dataSegment) {
    count <- c(0, 0, 0, 0)
    for (col in 12:15) {
        for (line in seq_len(nrow(dataSegment))) {
            if (nchar(dataSegment[line, col]) == 2) count[1] <- count[1] + 1
            if (nchar(dataSegment[line, col]) == 3) count[3] <- count[3] + 1
            if (nchar(dataSegment[line, col]) == 4) count[2] <- count[2] + 1
            if (nchar(dataSegment[line, col]) == 7) count[4] <- count[4] + 1
        }
    }
    return(sum(count))
}

solve_day08_part2 <- function(dataSegment) {
    somme <- 0
    for (line in seq_len(nrow(dataSegment))) {
        motif <- compute_motif(dataSegment[line, 1:10])
        number <- 0
        for (col in 12:15) {
            chiffre <- motif[index_contained(
                liste_chaine = motif$a,
                pattern = dataSegment[line, col],
                same = TRUE
            ), "b"]
            number <- 10 * number + chiffre
        }
        somme <- somme + number
    }
    return(somme)
}

###### EXECUTION ######

solve_day08_part1(dataSegment = segments_example)
solve_day08_part1(dataSegment = segments)

solve_day08_part2(dataSegment = segments_example)
solve_day08_part2(dataSegment = segments)

# Titre : day12.R
# Auteur : Tanguy

##############
##  DAY 12  ##
##############

###### IMPORT DATA ######

map_remaining_caves <- readLines(con = "./2021/Day12/map_remaining_caves.txt")
map_remaining_caves_S_example <- readLines(
    con = "./2021/Day12/map_remaining_caves_example.txt"
)
map_remaining_caves_M_example <- readLines(
    con = "./2021/Day12/map_remaining_caves_slightly_larger_example.txt"
)
map_remaining_caves_L_example <- readLines(
    con = "./2021/Day12/map_remaining_caves_even_larger_example.txt"
)

###### TRAITEMENT dataMap######

traitement <- function(dataMap) {
    liste_arretes <- unlist(strsplit(dataMap, "-"))
    liste_sommets <- unique(liste_arretes)
    arbre <- list(sommets = liste_sommets)

    for (num_arrete in seq_along(dataMap)) {
        sommet1 <- liste_arretes[2 * num_arrete - 1]
        sommet2 <- liste_arretes[2 * num_arrete]

        if (sommet1 != "end" && sommet2 != "start") {
            arbre[[sommet1]] <- c(arbre[[sommet1]], sommet2)
        }

        if (sommet2 != "end" && sommet1 != "start") {
            arbre[[sommet2]] <- c(arbre[[sommet2]], sommet1)
        }
    }

    return(arbre)
}

map_remaining_caves_S_example <- traitement(map_remaining_caves_S_example)
map_remaining_caves_M_example <- traitement(map_remaining_caves_M_example)
map_remaining_caves_L_example <- traitement(map_remaining_caves_L_example)
map_remaining_caves <- traitement(map_remaining_caves)


###### DECLARATION FONCTION ######

count_paths <- function(map, part1, part2) {
    aux_rec <- function(sommet, prin_sommets, comp_sommets) {
        if (sommet == "end") {
            return(1)
        }

        if (tolower(sommet) == sommet) {
            prin_sommets2 <- prin_sommets[prin_sommets != sommet]
            if (sommet %in% prin_sommets) {
                comp_sommets2 <- comp_sommets
            } else if (sommet %in% comp_sommets) {
                comp_sommets2 <- NULL
            } else {
                stop("On a mal traité un cas ?")
            }
        } else {
            prin_sommets2 <- prin_sommets
            comp_sommets2 <- comp_sommets
        }

        dests <- map[[sommet]]
        dests <- dests[dests %in% c(prin_sommets2, comp_sommets2)]

        somme_path <- 0
        for (sommet_dest in dests) {
            somme_path <- somme_path + aux_rec(
                sommet = sommet_dest,
                prin_sommets = prin_sommets2,
                comp_sommets = comp_sommets2
            )
        }

        return(somme_path)
    }

    if (part1) {
        return(aux_rec(sommet = "start",
                       prin_sommets = map$sommet,
                       comp_sommets = NULL))
    } else if (part2) {
        return(aux_rec(
            sommet = "start",
            prin_sommets = map$sommet,
            comp_sommets = map$sommets[tolower(map$sommets) == map$sommets]
        ))
    } else {
        stop("Le troisième cas maudit...")
    }
}

solve_day12_part1 <- function(dataMap) {
    return(count_paths(dataMap, part1 = TRUE, part2 = FALSE))
}
solve_day12_part2 <- function(dataMap) {
    return(count_paths(dataMap, part1 = FALSE, part2 = TRUE))
}


###### EXECUTION ######

solve_day12_part1(map_remaining_caves_S_example)
solve_day12_part1(map_remaining_caves_M_example)
solve_day12_part1(map_remaining_caves_L_example)
solve_day12_part1(map_remaining_caves)

solve_day12_part2(map_remaining_caves_S_example)
solve_day12_part2(map_remaining_caves_M_example)
solve_day12_part2(map_remaining_caves_L_example)
solve_day12_part2(map_remaining_caves)

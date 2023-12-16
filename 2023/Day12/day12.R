# Titre : day12.R
# Auteur : Tanguy

##############
##  DAY 12  ##
##############


# Import data ------------------------------------------------------------------

unknown_spring <- readLines(con = "./2023/Day12/condition_records.txt")
unknown_spring_example <- readLines(
    con = "./2023/Day12/condition_records_example.txt"
)


# Déclaration fonction ---------------------------------------------------------

count_arrangements <- function(spring, contiguous_group) {

    spring <- paste0(".", spring, ".")

    #3 dimensions :
    #   - pos : la positions à laquelle on se trouve dans le spring
    #   - groups : le nombre de groupe que l'on a passé (groupe de # validé par l'input)
    #   - len : le nombre de # que l'on a en réserve

    # je met +1 pour le 0 (même si on sera obligé de faire index + 1 car pas d'index 0 en R)
    state <- array(data = 0,
                   dim = c(nchar(spring) + 1,
                           length(contiguous_group) + 1,
                           max(contiguous_group) + 1))

    # Maintenant il faut écrire les fonctions de passage
    # Si spring[pos] = # --> alors len devient len + 1 et state[pos + 1, groups, len + 1] = state[pos + 1, groups, len]
    # Si spring[pos] = ? --> alors on a 2 choix :
    #   - si len == 0 ou len == contiguous_group[groups + 1] alors on peut le mettre en .
    #       - state[pos + 1, groups + 1, len = 0] = state[pos, groups + 1, 0] + state[pos, groups + 1, contiguous_group[groups + 1]]
    #   - si len < contiguous_group[groups + 1] on peut le mettre en #
    #       - si len == contiguous_group[groups + 1] - 1 alors state[pos + 1, groups + 1, len = 0] = state[pos, groups, len]
    #       - si len < contiguous_group[groups + 1] - 1 alors state[pos + 1, groups, len + 1] = state[pos, groups, len]
    # Si spring[pos] = .
    #   - si len == 0 ou len == contiguous_group[groups + 1] alors on peut le mettre en .
    #       - state[pos + 1, groups + 1, len = 0] = state[pos, groups + 1, 0] + state[pos, groups + 1, contiguous_group[groups + 1]]
    #
    # A chaque fois, le sinon = 0
    # chaque fois qu'on fait appel à contiguous_group[group + 1] c'est pour group + 1 <= length(contiguous_group)

    # Le fonction suivantes sont très intéressantes mais malheureusement trop lentes... donc inutilisées
    f <- function(state, x, y, z) {
        return(state[x + 1, y + 1, z + 1])
    }

    `f<-` <- function(state, x, y, z, value) {
        state[x + 1, y + 1, z + 1] <- value + state[x + 1, y + 1, z + 1]
        return(state)
    }
    # Donc on ajoute +1 partout pour coller avec les indices de l'array

    state[0 +1, 0 +1, 0 +1] <- 1

    for (pos in 1:(nchar(spring))) {
        val <- substr(spring, pos, pos)

        if (val %in% c(".", "?")) {
            state[pos +1, , 0 +1] <- state[pos - 1 +1, , 0 +1]
        }
        for (groups in 0:length(contiguous_group)) {

            if (val %in% c(".", "?") && groups > 0) {
                state[pos +1, groups +1, 0 +1] <-
                    state[pos +1, groups +1, 0 +1] +
                    state[pos - 1 +1, groups - 1 +1, contiguous_group[groups] +1]
            }

            if (val %in% c("#", "?") && groups < length(contiguous_group)) {
                for (len in seq_len(contiguous_group[groups + 1]) - 1) {
                    state[pos +1, groups +1, len + 1 +1] <-
                        state[pos - 1 +1, groups +1, len +1]
                }
            }
        }
    }

    return(state[nchar(spring) +1, length(contiguous_group) +1, 0 +1])
}

solve_day12_part1 <- function(data_spring) {

    data_spring <- data_spring |>
        strsplit(split = " ") |>
        do.call(what = rbind)

    somme <- 0
    for (k in seq_len(nrow(data_spring))) {
        spring_k <- data_spring[k, 1]
        contiguous_group_k <- data_spring[k, 2] |>
            strsplit(split = ",") |>
            unlist() |>
            as.numeric()
        somme <- somme + count_arrangements(spring_k, contiguous_group_k)
    }
    return(somme)
}

solve_day12_part2 <- function(data_spring) {
    data_spring <- data_spring |>
        strsplit(split = " ") |>
        do.call(what = rbind)

    somme <- 0
    for (k in seq_len(nrow(data_spring))) {
        spring_k <- paste0(rep(data_spring[k, 1], 5), collapse = "?")
        contiguous_group_k <- data_spring[k, 2] |>
            rep(times = 5) |>
            paste0(collapse = ",") |>
            strsplit(split = ",") |>
            unlist() |>
            as.numeric()
        somme <- somme + count_arrangements(spring_k, contiguous_group_k)
    }
    return(somme)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day12_part1(unknown_spring_example)
solve_day12_part1(unknown_spring)


## Part 2 ----------------------------------------------------------------------

solve_day12_part2(unknown_spring_example)
solve_day12_part2(unknown_spring) |> dput()

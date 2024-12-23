#!/usr/bin/env r

# Titre : day23.R
# Auteur : Tanguy

##############
##  DAY 23  ##
##############


# Import data ------------------------------------------------------------------

lan <- read.table(file.path("2024", "Day23", "lan.txt"), sep = "-")
lan_example <- read.table(file.path("2024", "Day23", "lan_example.txt"),
                          sep = "-")


# Déclaration fonction ---------------------------------------------------------

create_link_matrix <- function(data_lan) {
    all_computers <- unique(c(data_lan[["V1"]], data_lan[["V2"]]))

    link_matrix <- matrix(
        FALSE,
        nrow = length(all_computers),
        ncol = length(all_computers),
        dimnames = list(all_computers, all_computers)
    )

    for (k in seq_len(nrow(data_lan))) {
        link_matrix[data_lan[k, 1L], data_lan[k, 2L]] <- 1L
        link_matrix[data_lan[k, 2L], data_lan[k, 1L]] <- 1L
    }
    return(link_matrix)
}

solve_day23_part1 <- function(data_lan) {

    link_matrix <- create_link_matrix(data_lan)
    trio <- list()

    while (nrow(link_matrix) > 2L) {
        computer <- rownames(link_matrix)[[1L]]
        connected <- which(link_matrix[1L, ] == 1L) |> names()
        for (other_cmp in connected) {
            connected_2 <- which(link_matrix[1L, ] == link_matrix[other_cmp, ]
                                 & link_matrix[1L, ] == 1L) |>
                names()
            trio <- c(trio, lapply(connected_2, c, computer, other_cmp))
        }
        link_matrix <- link_matrix[-1L, -1L]
    }

    trio <- trio |> lapply(sort) |> do.call(what = rbind) |> as.data.frame()
    trio <- trio[!duplicated(trio), ]
    nb_trio <- trio |> subset(startsWith(V1, "t")
                              | startsWith(V2, "t")
                              | startsWith(V3, "t")) |>
        nrow()
    return(nb_trio)
}

get_connected <- function(group, link_matrix) {
    all_connected <- link_matrix[group, , drop = FALSE] |>
        apply(MARGIN = 2L, FUN = all) |>
        which() |>
        names() |>
        setdiff(group)
    return(all_connected)
}

find_bigest_group <- function(computer, link_matrix) {
    connected <- get_connected(computer, link_matrix)
    network <- link_matrix[connected, connected]
    while (sum(network == 0L) > nrow(network)) {
        to_remove <- network |> apply(MARGIN = 2L, FUN = sum) |> which.min()
        network <- network[-to_remove, -to_remove]
    }
    return(nrow(network) + 1L)
}

solve_day23_part2 <- function(data_lan) {
    all_computers <- unique(c(data_lan[["V1"]], data_lan[["V2"]]))
    link_matrix <- create_link_matrix(data_lan)
    max_group <- vapply(
        X = all_computers,
        FUN = find_bigest_group,
        link_matrix = link_matrix,
        FUN.VALUE = integer(1L)
    )
    solution <- all_computers[which(max_group == max(max_group))] |>
        sort() |>
        paste0(collapse = ",")
    return(solution)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day23_part1(lan_example)
solve_day23_part1(lan)


## Part 2 ----------------------------------------------------------------------

solve_day23_part2(lan_example)
solve_day23_part2(lan)

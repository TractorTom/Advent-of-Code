#!/usr/bin/env r

# Titre : day09.R
# Auteur : Tanguy

##############
##  DAY 09  ##
##############


# Import data ------------------------------------------------------------------

disk <- readLines(file.path("2024", "Day09", "disk.txt"))
disk_example <- readLines(file.path("2024", "Day09", "disk_example.txt"))


# Déclaration fonction ---------------------------------------------------------

get_space_disk <- function(data_disk) {
    space_disk <- data_disk |>
        strsplit(split = "", fixed = TRUE) |>
        unlist() |>
        as.integer()
    return(space_disk)
}

get_initial_id_number <- function(space_disk) {
    id_number <- rep(c(1L, -1L), times = length(space_disk) %/% 2L)
    if (length(space_disk) %% 2L == 1L) {
        id_number <- c(id_number, 1L)
    }
    id_number[id_number == 1L] <- seq_along(id_number[id_number == 1L]) - 1L
    return(id_number)
}

get_disk_blocks_representation <- function(space_disk, id_number) {
    rep(id_number, space_disk)
}

compacting_disk <- function(disk_blocks) {
    nb_empty_block <- sum(disk_blocks == -1L)
    full_block <- disk_blocks[disk_blocks != -1L]
    disk_blocks[disk_blocks == -1L] <- rev(full_block)[seq_len(nb_empty_block)]
    compact_disk <- disk_blocks[seq_along(full_block)]
    return(compact_disk)
}

compute_filesystem_checksum <- function(compact_disk) {
    compact_disk[compact_disk == -1L] <- 0L
    return(sum(compact_disk * (seq_along(compact_disk) - 1L)))
}

solve_day09_part1 <- function(data_disk) {
    space_disk <- get_space_disk(data_disk)
    id_number <- get_initial_id_number(space_disk)
    output <- space_disk |>
        get_disk_blocks_representation(id_number) |>
        compacting_disk() |>
        compute_filesystem_checksum()
    return(output)
}

solve_day09_part2 <- function(data_disk) {

    space_disk <- get_space_disk(data_disk)
    id_number <- get_initial_id_number(space_disk)

    id_value <- max(id_number)
    while (id_value > 0L) {
        pos_id_value <- which.max(id_number == id_value)
        nb_rep <- space_disk[pos_id_value]
        solutions <- which(id_number == -1L & space_disk >= nb_rep)
        solutions <- solutions[solutions < pos_id_value]
        if (length(solutions) > 0L) {
            solution <- min(solutions)
            diff <- space_disk[solution] - nb_rep
            id_number[pos_id_value] <- -1L
            space_disk <- c(space_disk[seq_len(solution - 1L)],
                            nb_rep, if (diff > 0L) diff,
                            space_disk[(solution + 1L):length(space_disk)])
            id_number <- c(id_number[seq_len(solution - 1L)],
                           id_value, if (diff > 0L) -1L,
                           id_number[(solution + 1L):length(id_number)])
        }
        id_value <- id_value - 1L
    }

    checksum <- get_disk_blocks_representation(space_disk, id_number) |>
        compute_filesystem_checksum()
    return(checksum)
}


# Execution --------------------------------------------------------------------

withr::with_options(new = list(digits = 22L), code = {

    ## Part 1 ------------------------------------------------------------------

    solve_day09_part1(disk_example) |> print()
    solve_day09_part1(disk) |> print()


    ## Part 2 ------------------------------------------------------------------

    solve_day09_part2(disk_example) |> print()
    solve_day09_part2(disk) |> print()
})

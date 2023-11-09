# Titre : day23.R
# Auteur : Tanguy

##############
##  DAY 23  ##
##############


# Import data ------------------------------------------------------------------

grove <- readLines("./2022/Day23/grove.txt")
grove_example <- readLines("./2022/Day23/grove_example.txt")
grove_example2 <- readLines("./2022/Day23/grove_example2.txt")


# Déclaration fonction ---------------------------------------------------------

is_in_list <- function(item, liste) {
    for (element in liste) if (all(item == element)) {
        return(TRUE)
    }
    return(FALSE)
}

choose_propose <- function(id, tab, round) {
    x <- tab$x[id]
    y <- tab$y[id]

    dire <- list(
        c(y - 1, x),
        c(y + 1, x),
        c(y, x - 1),
        c(y, x + 1)
    )

    v <- c(
        sum((tab$x %in% c(x - 1, x, x + 1)) && (tab$y == (y - 1))) == 0,
        sum((tab$x %in% c(x - 1, x, x + 1)) && (tab$y == (y + 1))) == 0,
        sum((tab$y %in% c(y - 1, y, y + 1)) && (tab$x == (x - 1))) == 0,
        sum((tab$y %in% c(y - 1, y, y + 1)) && (tab$x == (x + 1))) == 0
    )

    if (all(v)) {
        return(c(NA, NA))
    }

    v_round <- ((round - 1) %% 4) + 1
    v <- c(v[v_round:4], v[seq_len(v_round - 1)], TRUE)
    dire <- c(dire[v_round:4], dire[seq_len(v_round - 1)], list(c(NA, NA)))

    return(dire[[min((1:5)[v])]])
}

display_map <- function(map) {
    mat <- matrix(".",
        ncol = max(map$x) - min(map$x) + 1,
        nrow = max(map$y) - min(map$y) + 1
    )

    for (id in seq_len(nrow(map))) {
        if (id < 10) {
            mat[map[id, "y"] - min(map$y) + 1, map[id, "x"] - min(map$x) + 1] <- id
        } else {
            mat[map[id, "y"] - min(map$y) + 1, map[id, "x"] - min(map$x) + 1] <- "#"
        }
    }

    invisible(apply(mat, MARGIN = 1, FUN = \(x) print(paste0(x, collapse = ""))))
}

get_pos <- function(data_grove) {
    data_grove <- data_grove |> strsplit("")
    pos <- c()

    for (elf in seq_along(data_grove)) {
        if ("#" %in% data_grove[[elf]]) {
            pos <- rbind(pos, cbind(elf, which(data_grove[[elf]] == "#")))
        }
    }
    pos <- pos |>
        as.data.frame() |>
        dplyr::rename(x = 2, y = 1) |>
        dplyr::mutate(
            id = dplyr::row_number(),
            propose_y = NA,
            propose_x = NA,
            new_y = NA,
            new_x = NA
        )
    return(pos)
}

search <- function(elves, limit) {
    round <- 1
    while (round <= limit) {
        # Part 1 round
        for (id in seq_len(nrow(elves))) {
            if (id == 1) {
                a <- choose_propose(id = id, tab = elves, round = round)
            }
            elves[id, c("propose_y", "propose_x")] <- choose_propose(id = id, tab = elves, round = round)
        }

        # Part 2
        elves$new_y <- elves$y
        elves$new_x <- elves$x
        for (id in seq_len(nrow(elves))) {
            x <- elves$propose_x[id]
            y <- elves$propose_y[id]

            if (sum((elves$propose_x == x) && (elves$propose_y == y), na.rm = TRUE) == 1 && all(!is.na(c(x, y)))) {
                elves[id, c("new_y", "new_x")] <- elves[id, c("propose_y", "propose_x")]
            }
        }

        if (all(elves$y - min(elves$y) == elves$new_y - min(elves$new_y)) &&
            all(elves$x - min(elves$x) == elves$new_x - min(elves$new_x))) {
            square <- (max(elves$x) - min(elves$x) + 1) * (max(elves$y) - min(elves$y) + 1)
            if (limit == 10) {
                return(square - nrow(elves))
            } else {
                return(round)
            }
        }

        elves$propose_x <- NA
        elves$propose_y <- NA
        elves$y <- elves$new_y
        elves$x <- elves$new_x

        round <- round + 1
    }

    square <- (max(elves$x) - min(elves$x) + 1) * (max(elves$y) - min(elves$y) + 1)
    if (limit == 10) {
        return(square - nrow(elves))
    } else {
        return(round)
    }
}

solve_day23_part1 <- function(data_grove) {
    elves <- data_grove |> get_pos()
    return(search(elves, 10))
}

solve_day23_part2 <- function(data_grove) {
    elves <- data_grove |> get_pos()
    return(search(elves, Inf))
}

# Execution --------------------------------------------------------------------

solve_day23_part1(grove_example)
solve_day23_part1(grove_example2)
solve_day23_part1(grove)

solve_day23_part2(grove_example)
solve_day23_part2(grove_example2)
solve_day23_part2(grove)

# Titre : day13.R
# Auteur : Tanguy

##############
##  DAY 13  ##
##############


# Import data ------------------------------------------------------------------

mirrors <- readLines("./2023/Day13/mirrors.txt")
mirrors_example <- readLines("./2023/Day13/mirrors_example.txt")


# Déclaration fonction ---------------------------------------------------------

init <- function(data_mirrors) {
    k <- 1
    list_mirrors <- list()
    while (k <= length(data_mirrors)) {
        mat <- NULL
        while (k <= length(data_mirrors) && data_mirrors[k] != "") {
            mat <- rbind(mat, strsplit(data_mirrors[k], split = "") |> unlist())
            k <- k + 1
        }
        list_mirrors <- c(list_mirrors, list(mat))
        k <- k + 1
    }
    return(list_mirrors)
}

find_col_reflection <- function(mat) {
    for (vertical_line in seq_len(ncol(mat) - 1)) {
        range <- min(vertical_line, ncol(mat) - vertical_line)
        row <- 1
        cond <- TRUE

        while (cond && row <= nrow(mat)) {
            for (index_col in seq_len(range)) {
                v_left <- mat[row, vertical_line - index_col + 1]
                v_right <- mat[row, vertical_line + index_col]
                if (v_left != v_right) {
                    cond <- FALSE
                }
            }
            row <- row + 1
        }

        if (cond) {
            return(vertical_line)
        }
    }
    return(0)
}

find_col_reflection2 <- function(mat) {
    for (vertical_line in 1:(ncol(mat) - 1)) {
        range <- min(vertical_line, ncol(mat) - vertical_line)
        row <- 1
        count_smudge <- 0

        for (row in seq_len(nrow(mat))) {
            for (index_col in seq_len(range)) {
                v_left <- mat[row, vertical_line - index_col + 1]
                v_right <- mat[row, vertical_line + index_col]
                if (v_left != v_right) {
                    count_smudge <- count_smudge + 1
                }
            }
        }

        if (count_smudge == 1) {
            return(vertical_line)
        }
    }
    return(0)
}

summarise_note <- function(data_mirrors, fun) {
    somme <- 0
    for (mat in data_mirrors) {
        somme <- somme + fun(mat)
        somme <- somme + 100 * fun(t(mat))
    }
    return(somme)
}

solve_day13_part1 <- function(data_mirrors) {
    list_map_mirrors <- data_mirrors |> init()
    return(summarise_note(list_map_mirrors, fun = find_col_reflection))
}

solve_day13_part2 <- function(data_mirrors) {
    list_map_mirrors <- data_mirrors |> init()
    return(summarise_note(list_map_mirrors, fun = find_col_reflection2))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day13_part1(mirrors_example)
solve_day13_part1(mirrors)


## Part 2 ----------------------------------------------------------------------

solve_day13_part2(mirrors_example)
solve_day13_part2(mirrors)

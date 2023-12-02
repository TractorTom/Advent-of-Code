# Titre : day08.R
# Auteur : Tanguy

##############
##  DAY 08  ##
##############


# Import data ------------------------------------------------------------------

forest <- readLines("./2022/Day08/forest.txt")
forest_example <- readLines("./2022/Day08/forest_example.txt")


# Déclaration fonction ---------------------------------------------------------

complete_matrix <- function(matrix_forest) {
    n <- nrow(matrix_forest)
    mat <- matrix(FALSE, nrow = n, ncol = n)
    mat[seq_len(n), c(1, n)] <- TRUE
    mat[c(1, n), seq_len(n)] <- TRUE

    for (i in 2:(n - 1)) {
        # horizontal vers droite
        col <- 2
        maxi <- matrix_forest[i, 1]
        while (col <= n) {
            if (matrix_forest[i, col] > maxi) {
                mat[i, col] <- TRUE
                maxi <- matrix_forest[i, col]
            }
            col <- col + 1
        }

        # horizontal vers gauche
        col <- n - 1
        maxi <- matrix_forest[i, n]
        while (col > 0) {
            if (matrix_forest[i, col] > maxi) {
                mat[i, col] <- TRUE
                maxi <- matrix_forest[i, col]
            }
            col <- col - 1
        }

        # vertical vers bas
        row <- 2
        maxi <- matrix_forest[1, i]
        while (row <= n) {
            if (matrix_forest[row, i] > maxi) {
                mat[row, i] <- TRUE
                maxi <- matrix_forest[row, i]
            }
            row <- row + 1
        }

        # vertical vers haut
        row <- n - 1
        maxi <- matrix_forest[n, i]
        while (row > 0) {
            if (matrix_forest[row, i] > maxi) {
                mat[row, i] <- TRUE
                maxi <- matrix_forest[row, i]
            }
            row <- row - 1
        }
    }

    return(sum(mat))
}

traitement <- function(data_forest) {
    data_forest <- data_forest |>
        strsplit("") |>
        do.call(what = rbind)
    matrix_forest <- matrix(as.numeric(data_forest), nrow = nrow(data_forest))
    return(matrix_forest)
}

get_viewing_distance <- function(val, v) {
    if (length(v) == 0) {
        return(0)
    }
    return(min(which(c(v, val) >= val), length(v)))
}

get_scenic_score <- function(matrix_forest, x, y) {
    n <- nrow(matrix_forest)
    if (any(c(x, y) %in% c(1, n))) {
        return(0)
    }

    height <- matrix_forest[x, y]
    all_viewing_distance <- c(
        get_viewing_distance(v = matrix_forest[(x + 1):n, y], val = height),
        get_viewing_distance(v = matrix_forest[(x - 1):1, y], val = height),
        get_viewing_distance(v = matrix_forest[x, (y + 1):n], val = height),
        get_viewing_distance(v = matrix_forest[x, (y - 1):1], val = height)
    )

    return(prod(all_viewing_distance))
}

get_best_view <- function(matrix_forest) {
    n <- nrow(matrix_forest)
    vapply(
        X = seq_len(n),
        FUN = function(x1) {
            vapply(X = seq_len(n),
                   FUN = get_scenic_score,
                   matrix_forest = matrix_forest, x = x1,
                   FUN.VALUE = numeric(1))
        },
        FUN.VALUE = numeric(n)
    ) |> max()
}


# Execution --------------------------------------------------------------------

forest_example |>
    traitement() |>
    complete_matrix()
forest |>
    traitement() |>
    complete_matrix()

forest_example |>
    traitement() |>
    get_best_view()
forest |>
    traitement() |>
    get_best_view()

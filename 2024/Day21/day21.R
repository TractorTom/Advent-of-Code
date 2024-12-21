#!/usr/bin/env r

# Titre : day21.R
# Auteur : Tanguy

##############
##  DAY 21  ##
##############


# Import data ------------------------------------------------------------------

keypad <- readLines(file.path("2024", "Day21", "keypad.txt"))
keypad_example <- readLines(file.path("2024", "Day21", "keypad_example.txt"))


# Déclaration fonction ---------------------------------------------------------

numeric_keyboard <- list(
    "0" = c(4L, 2L),
    "A" = c(4L, 3L),
    "1" = c(3L, 1L),
    "2" = c(3L, 2L),
    "3" = c(3L, 3L),
    "4" = c(2L, 1L),
    "5" = c(2L, 2L),
    "6" = c(2L, 3L),
    "7" = c(1L, 1L),
    "8" = c(1L, 2L),
    "9" = c(1L, 3L)
)

directional_keyboard <- list(
    "^" = c(1L, 2L),
    "A" = c(1L, 3L),
    "<" = c(2L, 1L),
    "v" = c(2L, 2L),
    ">" = c(2L, 3L)
)

get_num_position <- function(num) {
    return(numeric_keyboard[[num]])
}

get_dir_position <- function(num) {
    return(directional_keyboard[[num]])
}

type <- function(from, to, fun, type = "num") {
    if (from == to) return(list("A"))

    pos_from <- fun(from)
    pos_to <- fun(to)

    from_to <- pos_to - pos_from

    vertical <-   rep(ifelse(from_to[[1L]] > 0L, "v", "^"), abs(from_to[[1L]]))
    horizontal <- rep(ifelse(from_to[[2L]] > 0L, ">", "<"), abs(from_to[[2L]]))

    if (length(horizontal) == 0L) return(list(c(vertical, "A")))
    if (length(vertical) == 0L) return(list(c(horizontal, "A")))

    if ((type == "num" && pos_from[[2L]] == 1L && pos_to[[1L]] == 4L)
        || (type == "dir" && pos_from[[2L]] == 1L && pos_to[[1L]] == 1L))
        return(list(c(horizontal, vertical, "A")))
    if ((type == "num" && pos_to[[2L]] == 1L && pos_from[[1L]] == 4L)
        || (type == "dir" && pos_to[[2L]] == 1L && pos_from[[1L]] == 1L))
        return(list(c(vertical, horizontal, "A")))

    return(list(c(horizontal, vertical, "A"), c(vertical, horizontal, "A")))
}

get_one_length <- function(buttons_seq, step, matrix_lengths) {
    count <- 0.0
    buttons_seq <- c("A", buttons_seq)
    for (j in seq_len(length(buttons_seq) - 1L)) {
        count <- count + matrix_lengths[buttons_seq[j], buttons_seq[j + 1L]]
    }
    return(count)
}

fill_dynamic_lengths <- function(step) {

    directions <- c("<", ">", "^", "v", "A")
    if (step == 0L) {
        dynamic_lengths <- array(NA, dim = c(5L, 5L, 25L),
                                 dimnames = list(directions, directions))
        return(dynamic_lengths)
    }
    dynamic_lengths <- fill_dynamic_lengths(step - 1L)
    if (step == 1L) {
        dynamic_lengths[, , 1L][] <- Map(
            f = \(x, y) type(x, y, get_dir_position, "dir") |> lengths() |> min(),
            x = rep(directions, times = 5L),
            y = rep(directions, each = 5L)
        ) |>
            unlist()
        return(dynamic_lengths)
    }

    dynamic_lengths[, , step][] <- Map(
        f = \(x, y) {
            lapply(
                X = type(x, y, get_dir_position, "dir"),
                FUN = get_one_length,
                step = step,
                matrix_lengths = dynamic_lengths[, , step - 1L]
            ) |>
                unlist() |>
                min()
        },
        x = rep(directions, times = 5L),
        y = rep(directions, each = 5L)
    ) |>
        unlist()

    return(dynamic_lengths)
}

dynamic_lengths <- fill_dynamic_lengths(25L)

add_elt <- function(liste, elt) {
    if (length(liste) == 0L) return(list(elt))
    return(lapply(liste, c, elt))
}

get_shortest_numeric <- function(code) {
    code <- c("A", strsplit(code, split = "", fixed = TRUE) |> unlist())
    solutions <- list()
    for (k in seq_len(length(code) - 1L)) {
        directions <- type(code[k], code[k + 1L], get_num_position, type = "num")
        solutions <- lapply(directions, add_elt, liste = solutions) |> do.call(what = c)
    }
    return(solutions)
}

get_numeric_part <- function(code) {
    numeric_part <- gsub(
        x = code,
        pattern = "A",
        replacement = "",
        fixed = TRUE
    ) |> as.integer()
    return(numeric_part)
}

get_shortest_length <- function(code, step) {
    buttons_seq <- code |> get_shortest_numeric()
    shortest_length <- lapply(
        X = buttons_seq,
        FUN = get_one_length,
        step = step,
        matrix_length = dynamic_lengths[, , step - 1L]
    ) |>
        unlist() |>
        min()
    return(shortest_length)
}

get_complexity <- function(input_keypad, nb_robot) {
    complexity <- 0L
    for (code in input_keypad) {
        length_shortest_seq <- get_shortest_length(code, step = nb_robot + 1L)
        numeric_part <- get_numeric_part(code)
        complexity <- complexity + length_shortest_seq * numeric_part
    }
    return(complexity)
}

solve_day21_part1 <- function(input_keypad) {
    return(get_complexity(input_keypad, nb_robot = 2L))
}

solve_day21_part2 <- function(input_keypad) {
    return(get_complexity(input_keypad, nb_robot = 25L))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day21_part1(keypad_example)
solve_day21_part1(keypad)


## Part 2 ----------------------------------------------------------------------

solve_day21_part2(keypad)

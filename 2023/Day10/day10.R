# Titre : day10.R
# Auteur : Tanguy

##############
##  DAY 10  ##
##############


# Import data ------------------------------------------------------------------

maze <- readLines("./2023/Day10/pipe_maze.txt")
maze_example <- readLines("./2023/Day10/pipe_maze_example.txt")
maze_example2 <- readLines("./2023/Day10/pipe_maze_example2.txt")
maze_example3 <- readLines("./2023/Day10/pipe_maze_example3.txt")
maze_example4 <- readLines("./2023/Day10/pipe_maze_example4.txt")
maze_example5 <- readLines("./2023/Day10/pipe_maze_example5.txt")


# Déclaration fonction ---------------------------------------------------------

init <- function(data_maze) {
    n_col <- nchar(data_maze[1])
    n_row <- length(data_maze)
    return(data_maze |>
               strsplit(split = "") |>
               unlist() |>
               matrix(nrow = n_row, ncol = n_col, byrow = TRUE) |>
               rbind(".", ... = _, ".") |>
               cbind(".", ... = _, "."))
}

next_move <- function(last_move, pipe) {

    if (pipe == "-") {
        if (last_move == "left") return("left")
        if (last_move == "right") return("right")
    } else if (pipe == "|") {
        if (last_move == "up") return("up")
        if (last_move == "down") return("down")
    } else if (pipe == "L") {
        if (last_move == "left") return("up")
        if (last_move == "down") return("right")
    } else if (pipe == "J") {
        if (last_move == "down") return("left")
        if (last_move == "right") return("up")
    } else if (pipe == "7") {
        if (last_move == "right") return("down")
        if (last_move == "up") return("left")
    } else if (pipe == "F") {
        if (last_move == "left") return("down")
        if (last_move == "up") return("right")
    }

    stop("You are blocked.")
}

next_pos <- function(last_pos, move) {

    x <- last_pos[1]
    y <- last_pos[2]

    if (move == "up") return(c(x - 1, y))
    if (move == "down") return(c(x + 1, y))
    if (move == "left") return(c(x, y - 1))
    if (move == "right") return(c(x, y + 1))

    stop("Wrong move")
}

find_start <- function(data_maze) {

    n_row <- nrow(data_maze)
    S_pos <- which(data_maze == "S")
    x_init <- ((S_pos - 1) %% n_row) + 1
    y_init <- ((S_pos - 1) %/% n_row) + 1

    return(c(x_init, y_init))
}

find_all_direction <- function(pos, data_maze) {

    x <- pos[1]
    y <- pos[2]

    out <- NULL

    if (data_maze[x - 1, y] %in% c("|", "7", "F")) out <- c(out, "up")
    if (data_maze[x + 1, y] %in% c("|", "L", "J")) out <- c(out, "down")
    if (data_maze[x, y - 1] %in% c("-", "L", "F")) out <- c(out, "left")
    if (data_maze[x, y + 1] %in% c("-", "7", "J")) out <- c(out, "right")

    return(out)
}

find_pipe <- function(directions) {
    if (all(directions %in% c("right", "left"))) return("-")
    if (all(directions %in% c("up", "down"))) return("|")
    if (all(directions %in% c("up", "right"))) return("L")
    if (all(directions %in% c("up", "left"))) return("J")
    if (all(directions %in% c("down", "left"))) return("7")
    if (all(directions %in% c("down", "right"))) return("F")
}

fill_matrix <- function(data_maze) {

    pos_init <- find_start(data_maze)

    all_directions <- find_all_direction(pos_init, data_maze)
    move <- all_directions[1]
    pos <- next_pos(last_pos = pos_init, move)

    mat <- matrix(0, nrow = nrow(data_maze), ncol = ncol(data_maze))
    mat[pos[1], pos[2]] <- 1

    while (any(pos != pos_init)) {
        move <- next_move(
            last_move = move,
            pipe = data_maze[pos[1], pos[2]]
        )
        pos <- next_pos(last_pos = pos, move)
        mat[pos[1], pos[2]] <- 1
    }

    return(mat)
}

count_tiles <- function(data_maze, mat) {

    pos_init <- find_start(data_maze)
    all_directions <- find_all_direction(pos_init, data_maze)
    data_maze[pos_init[1], pos_init[2]] <- find_pipe(all_directions)
    somme <- 0

    for (row in seq_len(nrow(data_maze))) {
        nb_inside <- 0
        nb_front <- 0
        last <- ""
        for (col in seq_len(ncol(data_maze))) {
            if (mat[row, col] == 1) {
                val <- data_maze[row, col]
                condition <- ((val %in% c("|", "F", "L"))
                              || (val == "7" && last == "F")
                              || (val == "J" && last == "L"))
                if (condition) {
                    nb_front <- nb_front + 1
                    last <- val
                }

            } else if (nb_front %% 2 == 1) {
                nb_inside <- nb_inside + 1
            }

            if (nb_front %% 2 == 0) {
                somme <- somme + nb_inside
                nb_inside <- 0
            }
        }
    }

    return(somme)
}

solve_day06_part1 <- function(data_maze) {
    data_maze <- init(data_maze)
    mat <- fill_matrix(data_maze)
    return(sum(mat) / 2)
}

solve_day06_part2 <- function(data_maze) {
    data_maze <- init(data_maze)
    mat <- fill_matrix(data_maze)
    return(count_tiles(data_maze, mat))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day06_part1(maze_example)
solve_day06_part1(maze_example2)
solve_day06_part1(maze)


## Part 2 ----------------------------------------------------------------------

solve_day06_part2(maze_example3)
solve_day06_part2(maze_example4)
solve_day06_part2(maze_example5)
solve_day06_part2(maze)

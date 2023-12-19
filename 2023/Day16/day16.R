# Titre : day16.R
# Auteur : Tanguy

##############
##  DAY 16  ##
##############


# Import data ------------------------------------------------------------------

contraption <- readLines("./2023/Day16/contraption.txt")
contraption_example <- readLines("./2023/Day16/contraption_example.txt")


# Déclaration fonction ---------------------------------------------------------

count_tiles <- function(data_contraption, first_situation) {
    n <- nrow(data_contraption)
    energized_field <- lapply(X = 1:4,
                              FUN = \(x) {
                                  matrix(data = 0,
                                         ncol = n,
                                         nrow = n)
                              })
    names(energized_field) <- c("up", "down", "left", "right")
    stack <- list(first_situation)

    while (length(stack) > 0) {

        situation <- stack[[1]]
        stack <- stack[-1]

        x <- situation[[1]]
        y <- situation[[2]]
        direction <- situation[[3]]

        new_x <- x + switch(direction,
                            "right" = 0,
                            "left" = 0,
                            "up" = -1,
                            "down" = 1)
        new_y <- y + switch(direction,
                            "right" = 1,
                            "left" = -1,
                            "up" = 0,
                            "down" = 0)

        if (new_x > 0 && new_x <= n && new_y > 0 && new_y <= n
            && (energized_field[[direction]][new_x, new_y] == 0)) {

            energized_field[[direction]][new_x, new_y] <- 1
            tile <- data_contraption[new_x, new_y]

            if (tile == ".") {
                stack <- c(stack, list(list(new_x, new_y, direction)))
            } else if (tile == "/") {
                new_direction <- switch(direction,
                                        "right" = "up",
                                        "left" = "down",
                                        "up" = "right",
                                        "down" = "left")
                stack <- c(stack, list(list(new_x, new_y, new_direction)))
            } else if (tile == "\\") {
                new_direction <- switch(direction,
                                        "left" = "up",
                                        "right" = "down",
                                        "up" = "left",
                                        "down" = "right")
                stack <- c(stack, list(list(new_x, new_y, new_direction)))
            } else if (tile == "-") {
                if (direction %in% c("left", "right")) {
                    stack <- c(stack, list(list(new_x, new_y, direction)))
                } else {
                    stack <- c(stack,
                               list(list(new_x, new_y, "left")),
                               list(list(new_x, new_y, "right")))
                }
            } else if (tile == "|") {
                if (direction %in% c("up", "down")) {
                    stack <- c(stack, list(list(new_x, new_y, direction)))
                } else {
                    stack <- c(stack,
                               list(list(new_x, new_y, "up")),
                               list(list(new_x, new_y, "down")))
                }
            }
        }
    }

    return(do.call(what = pmax, energized_field) |> sum())
}

solve_day16_part1 <- function(data_contraption) {

    data_contraption <- data_contraption |>
        strsplit(split = "") |>
        do.call(what = rbind)

    return(count_tiles(data_contraption, list(1, 0, "right")))
}

solve_day16_part2 <- function(data_contraption) {

    data_contraption <- data_contraption |>
        strsplit(split = "") |>
        do.call(what = rbind)

    n <- nrow(data_contraption)

    record <- 0
    for (k in seq_len(n)) {
        nb_right <- count_tiles(data_contraption, list(k, 0, "right"))
        nb_downward <- count_tiles(data_contraption, list(0, k, "down"))
        nb_left <- count_tiles(data_contraption, list(k, n + 1, "left"))
        nb_upward <- count_tiles(data_contraption, list(n + 1, k, "up"))
        record <- max(record, nb_left, nb_right, nb_downward, nb_upward)
    }
    return(record)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day16_part1(contraption_example)
solve_day16_part1(contraption)


## Part 2 ----------------------------------------------------------------------

solve_day16_part2(contraption_example)
solve_day16_part2(contraption)

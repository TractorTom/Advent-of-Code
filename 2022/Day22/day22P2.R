# Titre : dayXX.R
# Auteur : Tanguy

##############
##  DAY XX  ##
##############


# Import data ------------------------------------------------------------------

# datam <- read.table("./2022/DayXX/data.txt")
# datax_example <- read.table("./2022/DayXX/data_example.txt")

datam <- readLines("./2022/Day22/data.txt")
datax_example <- readLines("./2022/Day22/data_example.txt")


# Déclaration fonction ---------------------------------------------------------

new_direction <- function(direction, instruction) {
    ordre <- c("right", "down", "left", "up")
    if (instruction == "R") {
        return(ordre[(which(ordre == direction) %% 4) + 1])
    } else if (instruction == "L") {
        return(ordre[((which(ordre == direction) - 2) %% 4) + 1])
    }
    stop("Il y a une erreur de direction.")
}

# posiction est codée sur 4 élément : la position dans le cube, le numéro du cube et la direction
# nb est la dimension du cube / carré
change_cube <- function(position, nb) {
    y <- position[[1]]
    x <- position[[2]]
    cube <- position[[3]]
    dire <- position[[4]]

    if (cube == 1) {
        if (dire == "left") {
            return(list(nb - y + 1, 1, 4, "right"))
        }
        if (dire == "down") {
            return(list(1, x, 3, "down"))
        }
        if (dire == "up") {
            return(list(x, 1, 6, "right"))
        }
        if (dire == "right") {
            return(list(y, 1, 2, "right"))
        }
    } else if (cube == 2) {
        if (dire == "left") {
            return(list(y, nb, 1, "left"))
        }
        if (dire == "down") {
            return(list(x, nb, 3, "left"))
        }
        if (dire == "up") {
            return(list(nb, x, 6, "up"))
        }
        if (dire == "right") {
            return(list(nb - y + 1, nb, 5, "left"))
        }
    } else if (cube == 3) {
        if (dire == "left") {
            return(list(1, y, 4, "down"))
        }
        if (dire == "down") {
            return(list(1, x, 5, "down"))
        }
        if (dire == "up") {
            return(list(nb, x, 1, "up"))
        }
        if (dire == "right") {
            return(list(nb, y, 2, "up"))
        }
    } else if (cube == 4) {
        if (dire == "left") {
            return(list(nb - y + 1, 1, 1, "right"))
        }
        if (dire == "down") {
            return(list(1, x, 6, "down"))
        }
        if (dire == "up") {
            return(list(x, 1, 3, "right"))
        }
        if (dire == "right") {
            return(list(y, 1, 5, "right"))
        }
    } else if (cube == 5) {
        if (dire == "left") {
            return(list(y, nb, 4, "left"))
        }
        if (dire == "down") {
            return(list(x, nb, 6, "left"))
        }
        if (dire == "up") {
            return(list(nb, x, 3, "up"))
        }
        if (dire == "right") {
            return(list(nb - y + 1, nb, 2, "left"))
        }
    } else if (cube == 6) {
        if (dire == "left") {
            return(list(1, y, 1, "down"))
        }
        if (dire == "down") {
            return(list(1, x, 2, "down"))
        }
        if (dire == "up") {
            return(list(nb, x, 4, "up"))
        }
        if (dire == "right") {
            return(list(nb, y, 5, "up"))
        }
    }

    stop("Il y a une erreur qq part !")
}

one_move <- function(all_cubes, position, shift) {
    # cat("--------Inside one_move_function--------\n")
    k <- 0
    direction <- position[[4]]
    # cat("Direction : ", direction, "\n")

    if (direction == "right") {
        while (k < shift) {
            # On sort du cube
            if (position[[2]] == 50) {
                new_position <- change_cube(position, 50)
                if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                    return(position)
                } else {
                    return(one_move(all_cubes, new_position, shift - k - 1))
                }
            } else {
                new_position <- position
                new_position[[2]] <- new_position[[2]] + 1
            }

            if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                return(position)
            }
            position <- new_position
            k <- k + 1
        }
    } else if (direction == "left") {
        while (k < shift) {
            # On sort du cube
            if (position[[2]] == 1) {
                new_position <- change_cube(position, 50)
                if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                    return(position)
                } else {
                    return(one_move(all_cubes, new_position, shift - k - 1))
                }
            } else {
                new_position <- position
                new_position[[2]] <- new_position[[2]] - 1
            }

            if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                return(position)
            }
            position <- new_position
            k <- k + 1
        }
    } else if (direction == "down") {
        while (k < shift) {
            # On sort du cube
            if (position[[1]] == 50) {
                new_position <- change_cube(position, 50)
                if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                    return(position)
                } else {
                    return(one_move(all_cubes, new_position, shift - k - 1))
                }
            } else {
                new_position <- position
                new_position[[1]] <- new_position[[1]] + 1
            }

            if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                return(position)
            }
            position <- new_position
            k <- k + 1
        }
    } else if (direction == "up") {
        while (k < shift) {
            # On sort du cube
            if (position[[1]] == 1) {
                new_position <- change_cube(position, 50)
                if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                    return(position)
                } else {
                    return(one_move(all_cubes, new_position, shift - k - 1))
                }
            } else {
                new_position <- position
                new_position[[1]] <- new_position[[1]] - 1
            }

            if (all_cubes[[new_position[[3]]]][new_position[[1]], new_position[[2]]] == "#") {
                return(position)
            }
            position <- new_position
            k <- k + 1
        }
    }

    # cat("--------outside one_move_function--------\n")
    return(position)
}

## Initialise cube

datax <- datam
instr <- datax[length(datax)]
map <- datax[-c((length(datax) - 1):length(datax))]
map <- gsub(x = map, pattern = " ", replacement = "O")
nb_col <- max(nchar(map))
for (k in seq_along(map)) {
    map[k] <- paste0(map[k], strrep("O", nb_col - nchar(map[k])))
}
map <- strsplit(map, split = "") |> do.call(what = rbind)

list_cubes <- list(
    map[1:50, 51:100],
    map[1:50, 101:150],
    map[51:100, 51:100],
    map[101:150, 1:50],
    map[101:150, 51:100],
    map[151:200, 1:50]
)


# Initialise instr

index <- 0
index2 <- 1
dire <- "R"
list_instr <- NULL
while (index < nchar(instr)) {
    if (index > 1) {
        dire <- substr(instr, index, index)
        index2 <- index + 1
    }
    while (index2 < nchar(instr) && substr(instr, index2, index2) %in% 0:9) {
        index2 <- index2 + 1
    }
    if (index2 == nchar(instr)) {
        val <- substr(instr, index + 1, index2)
    } else {
        val <- substr(instr, index + 1, index2 - 1)
    }

    list_instr <- rbind(list_instr, c(dire, val))
    index <- index2
}

list_instr <- list_instr |>
    as.data.frame() |>
    dplyr::mutate(V2 = as.numeric(V2))



# Action !


min_pos <- min(which(list_cubes[[1]][1, ] == "."))
pos <- list(1, min_pos, 1, "right")
print(pos)

tab_pos <- c(pos[[1]], pos[[2]], pos[[3]])

for (k in seq_len(nrow(list_instr))) {
    # cat("\n\n")
    # print(k)
    # print(list_instr[k, ])
    # cat("Actual direction : ", pos[[4]], "\n")
    if (k > 1) {
        pos[[4]] <- new_direction(direction = pos[[4]], instruction = list_instr$V1[k])
        # cat("New direction : ", pos[[4]], "\n")
    }
    # cat("Déplacement : ", list_instr$V2[k], "\n")
    pos <- one_move(all_cubes = list_cubes, position = pos, shift = list_instr$V2[k])

    # cat("Nouvelle position : ", pos[[1]], pos[[2]], ", cube : ", pos[[3]], ", direction", pos[[4]], "\n")

    tab_pos <- rbind(tab_pos, c(pos[[1]], pos[[2]], pos[[3]]))
    # print(pos)
    # cat("New position : ", pos, "\n")
}

ordre <- c("right", "down", "left", "up")
print(dire)
print(pos)

y <- pos[[1]]
x <- pos[[2]] + 100

print(1000 * y + 4 * x + which(ordre == pos[[4]]) - 1)

# 130090 #too high
# 78160 #too high
# 28160 #too low











solve_dayXX_part1 <- function(data_cpu) {

}

solve_dayXX_part2 <- function(data_cpu) {

}

# Execution --------------------------------------------------------------------

solve_dayXX_part1(datax_example)
solve_dayXX_part1(datam)

solve_dayXX_part2(datax_example)
solve_dayXX_part2(datam)

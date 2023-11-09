# Titre : day09.R
# Auteur : Tanguy

##############
##  DAY 09  ##
##############


# Import data ------------------------------------------------------------------

instruction <- read.table("./2022/Day09/instruction.txt")
instruction_example <- read.table("./2022/Day09/instruction_example.txt")
instruction_example2 <- read.table("./2022/Day09/instruction_example2.txt")


# Déclaration fonction ---------------------------------------------------------

move <- function(direction) {
    if (direction == "L") {
        return(c(0, -1))
    } else if (direction == "R") {
        return(c(0, 1))
    } else if (direction == "U") {
        return(c(-1, 0))
    } else if (direction == "D") {
        return(c(1, 0))
    }
}

follow <- function(head, tail) {
    if (any(abs(head - tail) > 1)) {
        x.head <- head[1]
        y.head <- head[2]
        x.tail <- tail[1]
        y.tail <- tail[2]

        if (x.head == x.tail) {
            return(c(0, (y.head - y.tail) / 2))
        }
        if (y.head == y.tail) {
            return(c((x.head - x.tail) / 2, 0))
        }
        if (x.head > x.tail && y.head > y.tail) {
            return(c(1, 1))
        }
        if (x.head > x.tail && y.head < y.tail) {
            return(c(1, -1))
        }
        if (x.head < x.tail && y.head > y.tail) {
            return(c(-1, 1))
        }
        if (x.head < x.tail && y.head < y.tail) {
            return(c(-1, -1))
        }
    }
    return(c(0, 0))
}

monitor_movement <- function(data_instruction, node_nb = 2) {
    dim_max <- data_instruction |>
        dplyr::group_by(V1) |>
        dplyr::summarise(tot = sum(V2)) |>
        dplyr::pull(tot) |>
        max()

    all_nodes <- data.frame(x = rep(dim_max, node_nb), y = rep(dim_max, node_nb))
    passage <- matrix(0, nrow = 2 * dim_max, ncol = 2 * dim_max)

    passage[all_nodes[1, ]$x, all_nodes[1, ]$y] <- 1

    for (k in seq_len(nrow(data_instruction))) {
        instr <- data_instruction[k, ]
        print(k)

        for (i in seq_len(instr$V2)) {
            all_nodes[1, ] <- all_nodes[1, ] + move(instr$V1)

            for (node in 1 + seq_len(node_nb - 1)) {
                all_nodes[node, ] <- all_nodes[node, ] +
                    follow(
                        head = all_nodes[node - 1, ],
                        tail = all_nodes[node, ]
                    )
            }

            passage[all_nodes[node_nb, "x"], all_nodes[node_nb, "y"]] <- 1
        }
    }

    return(sum(passage))
}

solve_day09_part1 <- function(data_instruction) {
    monitor_movement(data_instruction, 2)
}

solve_day09_part2 <- function(data_instruction) {
    monitor_movement(data_instruction, 10)
}


# Execution --------------------------------------------------------------------

solve_day09_part1(instruction_example)
solve_day09_part1(instruction)

solve_day09_part2(instruction_example)
solve_day09_part2(instruction_example2)
solve_day09_part2(instruction)

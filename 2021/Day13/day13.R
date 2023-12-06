# Titre : day13.R
# Auteur : Tanguy

##############
##  DAY 13  ##
##############

###### IMPORT DATA ######

page1 <- readLines("./2021/Day13/instructions.txt")
page1_example <- readLines("./2021/Day13/instructions_example.txt")

###### TRAITEMENT dataPage1######

traitement <- function(dataPage1) {
    output_table <- NULL

    index <- 1
    line <- dataPage1[index]

    while (line != "") {
        output_table <- rbind(
            output_table,
            1 + as.numeric(unlist(strsplit(line, ",")))
        )
        index <- index + 1
        line <- dataPage1[index]
    }
    index <- index + 1

    output_fold <- dataPage1[index:length(dataPage1)]

    output_mat <- matrix(
        data = ".",
        nrow = max(output_table[, 2]),
        ncol = max(output_table[, 1])
    )
    for (index_b in seq_len(nrow(output_table))) {
        output_mat[output_table[index_b, 2], output_table[index_b, 1]] <- "*"
    }


    output_list <- list(
        table = output_table,
        matrice = output_mat,
        fold = output_fold
    )
    return(output_list)
}

page1 <- traitement(page1)
page1_example <- traitement(page1_example)


###### DECLARATION FONCTION ######

fold <- function(table, instruction) {
    name_dim <- substr(instruction, 12, 12)
    val <- as.numeric(substr(instruction, 14, nchar(instruction))) + 1
    #+ 1 car on commence à (1, 1) et pas à (0, 0)

    new_table <- table

    if (name_dim == "x") {
        for (id_x in (val + 1):(dim(table)[2])) {
            for (id_y in 1:(dim(table)[1])) {
                if (table[id_y, 2 * val - id_x] == ".") {
                    new_table[id_y, 2 * val - id_x] <- table[id_y, id_x]
                }
                if (table[id_y, val] != ".") {
                    stop("il y a une croix sur une ligne")
                }
            }
        }
        new_table <- new_table[, 1:(val - 1)]
    } else if (name_dim == "y") {
        for (id_y in (val + 1):(dim(table)[1])) {
            for (id_x in 1:(dim(table)[2])) {
                if (table[2 * val - id_y, id_x] == ".") {
                    new_table[2 * val - id_y, id_x] <- table[id_y, id_x]
                }
                if (table[val, id_x] != ".") {
                    stop("il y a une croix sur une ligne")
                }
            }
        }
        new_table <- new_table[1:(val - 1), ]
    } else {
        stop("Il y a une erreur.")
    }

    return(new_table)
}

display <- function(table) {
    cat(paste0(apply(table, MARGIN = 1, paste0, collapse = ""),
        collapse = "\n"
    ))
    return()
}

solve_day13_part1 <- function(dataPage1) {
    first_instruction <- dataPage1$fold[1]
    new_table <- fold(table = dataPage1$mat, instruction = first_instruction)
    return(sum(new_table == "*"))
}

solve_day13_part2 <- function(dataPage1) {
    new_table <- dataPage1$mat
    for (instr in dataPage1$fold) {
        new_table <- fold(table = new_table, instruction = instr)
    }
    display(new_table)
    return()
}

###### EXECUTION ######

solve_day13_part1(page1_example)
solve_day13_part1(page1)

solve_day13_part2(page1_example)
solve_day13_part2(page1)

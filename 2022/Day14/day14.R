# Titre : day14.R
# Auteur : Tanguy

##############
##  DAY 14  ##
##############


# Import data ------------------------------------------------------------------

solid_rock <- readLines("./2022/Day14/solid_rock.txt")
solid_rock_example <- readLines("./2022/Day14/solid_rock_example.txt")


# Déclaration fonction ---------------------------------------------------------

get_min_max <- function(data_rock) {
    
    all_coor <- c(Inf, -Inf, Inf, -Inf)
    for (line in data_rock) {
        new_coor <- line |> strsplit(split = " -> ") |> unlist() |> 
            strsplit(split = ",")
        
        all_coor[1] <- min(all_coor[1], sapply(new_coor, "[[", 1) |> as.numeric())
        all_coor[2] <- max(all_coor[2], sapply(new_coor, "[[", 1) |> as.numeric())
        all_coor[3] <- min(all_coor[3], sapply(new_coor, "[[", 2) |> as.numeric())
        all_coor[4] <- max(all_coor[4], sapply(new_coor, "[[", 2) |> as.numeric())
    }
    
    return(all_coor)
}

add_one_path_rock <- function(cave_map, bord, dim_min) {
    
    new_coor <- bord |> 
        strsplit(split = " -> ") |> 
        unlist() |> 
        strsplit(split = ",") |> 
        do.call(what = rbind)
    
    new_coor[, 1] <- as.numeric(new_coor[, 1])
    new_coor[, 2] <- as.numeric(new_coor[, 2])
    
    for (intersection in seq_len(nrow(new_coor) - 1)) {
        row_vect <- new_coor[intersection, 2]:new_coor[intersection + 1, 2] + 1
        col_vect <- new_coor[intersection, 1]:new_coor[intersection + 1, 1] - dim_min + 1
        cave_map[row_vect, col_vect] <- "#"
    }
    
    return(cave_map)
}

add_all_rock_path <- function(cave_map, data_rock, dim_min) {
    for (bord in data_rock) {
        cave_map <- cave_map |> add_one_path_rock(bord = bord, dim_min = dim_min)
    }
    return(cave_map)
}

add_one_sable <- function(cave_map, col_500) {
    
    if (cave_map[1, col_500] != ".") return("full")
    row <- 1
    col <- col_500
    
    cond <- TRUE
    while ((row < nrow(cave_map)) && cond) {
        row <- row + 1
        if (cave_map[row, col] == ".") {
            col <- col
        } else if (cave_map[row, col - 1] == ".") {
            col <- col - 1
        } else if (cave_map[row, col + 1] == ".") {
            col <- col + 1
        } else {
            cond <- FALSE
            row <- row - 1
        }
    }
    
    return(c(row, col))
}

solve_day14_part1 <- function(data_rock) {
    
    dimension <- get_min_max(data_rock)
    dimension[1] <- dimension[1] - 1
    dimension[2] <- dimension[2] + 1
    
    map <- matrix(".", nrow = 1 + dimension[4] - min(dimension[3], 0), 
                  ncol = 1 + dimension[2] - dimension[1]) |> 
        add_all_rock_path(data_rock = data_rock, dim_min = dimension[1])
    
    map <- rbind(map, ".")
    
    cond <- TRUE
    index <- 0
    
    while (cond) {
        coor <- add_one_sable(map, col_500 = 500 - dimension[1] + 1)
        if (coor[1] == nrow(map)) {
            cond <- FALSE
        } else {
            map[coor[1], coor[2]] <- "+"
            index <- index + 1
        }
    }
    
    return(index)
}

solve_day14_part2 <- function(data_rock) {
    
    dimension <- get_min_max(data_rock)
    
    y_range <- dimension[4] + 2
    dimension[1] <- min(dimension[1], 500 - y_range)
    dimension[2] <- max(dimension[2], 500 + y_range)
    
    map <- matrix(".", nrow = 1 + dimension[4] - min(dimension[3], 0), 
                  ncol = 1 + dimension[2] - dimension[1]) |> 
        add_all_rock_path(data_rock = data_rock, dim_min = dimension[1])
    
    map <- rbind(map, ".", "#")
    
    cond <- TRUE
    index <- 0
    
    while (cond) {
        coor <- add_one_sable(map, col_500 = 500 - dimension[1] + 1)
        if (coor[1] == "full") {
            cond <- FALSE
        } else {
            map[coor[1], coor[2]] <- "+"
            index <- index + 1
        }
    }
    
    return(index)
}


# Execution --------------------------------------------------------------------

solve_day14_part1(solid_rock_example)
solve_day14_part1(solid_rock)

solve_day14_part2(solid_rock_example)
solve_day14_part2(solid_rock)

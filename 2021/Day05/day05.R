#Titre : day05.R
#Auteur : Tanguy

##############
##  DAY 05  ##
##############

######IMPORT DATA######

hydrothermal_vents_example <- readLines("./2021/Day05/hydrothermal_vents_example.txt")
hydrothermal_vents <- readLines("./2021/Day05/hydrothermal_vents.txt")

######DECLARATION FONCTIONS######

createEmptyMap <- function(dataVents){
    min_x <- 0
    min_y <- 0
    max_x <- 0
    max_y <- 0
    
    for (k in 1: length(dataVents)){
        
        direction <- strsplit(dataVents[k], " -> ")[[1]]
        coord_1 <- as.numeric(strsplit(direction[1], ",")[[1]])
        coord_2 <- as.numeric(strsplit(direction[2], ",")[[1]])
        
        min_x <- min(coord_1[1], coord_2[1], min_x)
        min_y <- min(coord_1[2], coord_2[2], min_y)
        max_x <- max(coord_1[1], coord_2[1], max_x)
        max_y <- max(coord_1[2], coord_2[2], max_y)
    }
    
    return(list(map = matrix(0, nrow = max_x - min_x + 1, ncol = max_y - min_y + 1), 
                min = c(min_x, min_y) ,
                max = c(max_x, max_y)))
}

solve_day05_part1 <- function(dataVents){
    map <- createEmptyMap(dataVents)
    
    for (k in 1: length(dataVents)){
        
        direction <- strsplit(dataVents[k], " -> ")[[1]]
        coord_1 <- as.numeric(strsplit(direction[1], ",")[[1]])
        coord_2 <- as.numeric(strsplit(direction[2], ",")[[1]])
        
        if (coord_1[1] == coord_2[1] | coord_1[2] == coord_2[2]){
            for (val_x in coord_1[1]:coord_2[1] - map$min[1] + 1){
                for (val_y in coord_1[2]:coord_2[2] - map$min[2] + 1){
                    map$map[val_x, val_y] <- map$map[val_x, val_y] + 1
                }
            }
        }
    }
    return(sum(map$map > 1))
}

solve_day05_part2 <- function(dataVents){
    map <- createEmptyMap(dataVents)
    
    for (k in 1: length(dataVents)){
        
        direction <- strsplit(dataVents[k], " -> ")[[1]]
        coord_1 <- as.numeric(strsplit(direction[1], ",")[[1]])
        coord_2 <- as.numeric(strsplit(direction[2], ",")[[1]])
        
        if (coord_1[1] == coord_2[1] | coord_1[2] == coord_2[2]){
            for (val_x in coord_1[1]:coord_2[1] - map$min[1] + 1){
                for (val_y in coord_1[2]:coord_2[2] - map$min[2] + 1){
                    map$map[val_x, val_y] <- map$map[val_x, val_y] + 1
                }
            }
        } else {
            vx <- coord_1[1]:coord_2[1] - map$min[1] + 1
            vy <- coord_1[2]:coord_2[2] - map$min[2] + 1
            if (length(vx) != length(vy)){
                stop("Le vecteur n'est pas lisible.")
            }
            for (k in 1:length(vx)){
                val_x <- vx[k]
                val_y <- vy[k]
                map$map[val_x, val_y] <- map$map[val_x, val_y] + 1
            }
        }
    }
    return(sum(map$map > 1))
}

######EXECUTION######

solve_day05_part1(dataVents = hydrothermal_vents_example)
solve_day05_part1(dataVents = hydrothermal_vents)

solve_day05_part2(dataVents = hydrothermal_vents_example)
solve_day05_part2(dataVents = hydrothermal_vents)

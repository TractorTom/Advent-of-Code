# Titre : day24.R
# Auteur : Tanguy

##############
##  DAY 24  ##
##############

# Import data ------------------------------------------------------------------

map_valley <- readLines("./2022/Day24/valley_map.txt")
map_valley_example <- readLines("./2022/Day24/valley_map_example.txt")
map_valley_example2 <- readLines("./2022/Day24/valley_map_example2.txt")


# Déclaration fonction ---------------------------------------------------------

ppcm <- function(x, y) {
    return(x * y %/% schoolmath::gcd(x, y))
}

get_blizzard_position <- function(data_valley) {
    
    data_valley <- data_valley |> strsplit("")
    blizzard_position <- c()
    ordre <- c("<", ">", "^", "v")
    
    for (row in 1:length(data_valley)) {
        for (col in 1:(length(data_valley[[row]]))) {
            val <- data_valley[[row]][col]
            if (val %in% ordre) {
                blizzard_position <- rbind(blizzard_position, cbind(row - 1, col - 1, c("left", "right", "up", "down")[which(val == ordre)]))
            }
        }
    }
    
    blizzard_position <- blizzard_position |>
        as.data.frame() |>
        dplyr::rename(x = 2, y = 1, direction = 3) |>
        dplyr::mutate(
            x = as.integer(x),
            y = as.integer(y)
        )
    
    return(blizzard_position)
}

get_next_position <- function(y, x, direction, dim_y, dim_x) {
    
    if (direction == "up") {
        return(list(y = (y - 2) %% dim_y + 1, x = x, direction = "up"))
    } else if (direction == "down") {
        return(list(y = y %% dim_y + 1, x = x, direction = "down"))
    }  else if (direction == "left") {
        return(list(y = y, x = (x - 2) %% dim_x + 1, direction = "left"))
    } else if (direction == "right") {
        return(list(y = y, x = x %% dim_x + 1, direction = "right"))
    }
    
    stop("Il y a une erreur !")
}

get_next_blizzard_position <- function(blizzard_position, dim_y, dim_x) {
    return(purrr::pmap_df(blizzard_position, .f = get_next_position,
                          dim_y = dim_y, dim_x = dim_x))
}

get_blizzard_map <- function(data_valley) {
    blizzard_position <- get_blizzard_position(data_valley)
    
    dim_y <- length(data_valley) - 2
    dim_x <- nchar(data_valley[1]) - 2
    
    max_step <- ppcm(dim_y, dim_x)
    mat <- array(FALSE, dim = c(dim_y, dim_x, max_step))
    
    for (step in 1:max_step) {
        blizzard_position <- get_next_blizzard_position(blizzard_position = blizzard_position, dim_y, dim_x)
        for (blizzard in 1:nrow(blizzard_position)) {
            mat[blizzard_position$y[blizzard], blizzard_position$x[blizzard], step] <- TRUE
        }
    }
    
    return(mat)
}

insert <- function(elt, listo) {
    if (length(listo) == 0) return(list(elt))
    
    index <- 1
    while (index <= length(listo) && listo[[index]][4] < elt[4]) {
        index <- index + 1
    }
    
    if (index > length(listo)) return(c(listo, list(elt)))
    return(c(listo[seq_len(index - 1)], list(elt), listo[index:length(listo)]))
}

search_path <- function(blizzard_map, dim_y, dim_x, start, end, step) {
    
    max_step <- ppcm(dim_y, dim_x)
    
    list_etat <- list(c(
        y = start[1], x = start[2],
        step = step,
        dist = abs(1 - end[2]) + abs(1 - end[1]))
    )
    
    rec <- Inf
    
    while (length(list_etat) > 0) {
        
        etat <- list_etat[[1]]
        list_etat <- list_etat[-1]
        
        y <- etat[1]
        x <- etat[2]
        step <- etat[3]
        
        if (step <= rec) {
            
            if (y == end[1] && x == end[2]) {
                print(step)
                rec <- step
            }
            
            if (y == 0) {
                
                if (!blizzard_map[y + 1, x, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y + 1, x, step + 1,
                                                abs(x - end[2]) + abs(y + 1 - end[1])
                                        ))
                }
                
                list_etat <- insert(listo = list_etat,
                                    elt = c(y, x, step + 1,
                                            abs(x - end[2]) + abs(y - end[1])
                                    ))
                
            } else if (y == dim_y + 1) {
                
                if (!blizzard_map[y - 1, x, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y - 1, x, step + 1,
                                                abs(x - end[2]) + abs(y - 1 - end[1])
                                        ))
                }
                
                list_etat <- insert(listo = list_etat,
                                    elt = c(y, x, step + 1,
                                            abs(x - end[2]) + abs(y - end[1])
                                    ))
                
            } else {
                
                blizzard_map[y, x, ((step - 1) %% max_step) + 1] <- TRUE
                
                if (y > 1 && !blizzard_map[y - 1, x, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y - 1, x, step + 1,
                                                abs(x - end[2]) + abs(y - 1 - end[1])
                                        ))
                }
                
                if (x > 1 && !blizzard_map[y, x - 1, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y, x - 1, step + 1,
                                                abs(x - 1 - end[2]) + abs(y - end[1])
                                        ))
                }
                
                if (y < dim_y && !blizzard_map[y + 1, x, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y + 1, x, step + 1,
                                                abs(x - end[2]) + abs(y + 1 - end[1])
                                        ))
                }
                
                if (x < dim_x && !blizzard_map[y, x + 1, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y, x + 1, step + 1,
                                                abs(x + 1 - end[2]) + abs(y - end[1])
                                        ))
                }
                
                if (!blizzard_map[y, x, (step  %% max_step) + 1]) {
                    list_etat <- insert(listo = list_etat,
                                        elt = c(y, x, step + 1,
                                                abs(x - end[2]) + abs(y - end[1])
                                        ))
                }
            }
        }
    }
    
    return(rec + 1)
}

solve_day24_part1 <- function(data_valley) {
    
    blizzard_map <- get_blizzard_map(data_valley)
    
    dim_y <- length(data_valley) - 2
    dim_x <- nchar(data_valley[1]) - 2
    
    return(search_path(blizzard_map = blizzard_map, 
                       dim_y = dim_y, dim_x = dim_x, 
                       start = c(0, 1), end = c(dim_y, dim_x), step = 0))
}

solve_day24_part2 <- function(data_valley) {
    
    blizzard_map <- get_blizzard_map(data_valley)
    
    dim_y <- length(data_valley) - 2
    dim_x <- nchar(data_valley[1]) - 2
    
    step <- search_path(blizzard_map = blizzard_map, 
                        dim_y = dim_y, dim_x = dim_x, 
                        start = c(0, 1), end = c(dim_y, dim_x), step = 0)
    step <- search_path(blizzard_map = blizzard_map, 
                        dim_y = dim_y, dim_x = dim_x, 
                        start = c(dim_y + 1, dim_x), end = c(1, 1), step = step)
    return(search_path(blizzard_map = blizzard_map, 
                       dim_y = dim_y, dim_x = dim_x, 
                       start = c(0, 1), end = c(dim_y, dim_x), step = step))
}

# Execution --------------------------------------------------------------------

solve_day24_part1(map_valley_example)
solve_day24_part1(map_valley_example2)
solve_day24_part1(map_valley)

solve_day24_part2(map_valley_example)
solve_day24_part2(map_valley_example2)
solve_day24_part2(map_valley)

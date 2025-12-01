#!/usr/bin/env r

# Titre : day18.R
# Auteur : Tanguy

##############
##  DAY 18  ##
##############


# Import data ------------------------------------------------------------------

datax <- readLines(file.path("2024", "Day18", "data.txt"))
datax_example <- readLines(file.path("2024", "Day18", "data_example.txt"))
# datax_example2 <- readLines(file.path("2024", "Day18", "data_example2.txt"))
# datax_example3 <- readLines(file.path("2024", "Day18", "data_example3.txt"))


# Déclaration fonction ---------------------------------------------------------

get_position_int <- function(pos, n) {
    return(pos[[1L]] + n * (pos[[2L]] - 1L))
}

get_position_vect <- function(pos, n) {
    return(c(row = 1L + (pos - 1L) %% n, col = 1L + (pos - 1L) %/% n))
}

get_neighboors <- function(pos, n) {
    return(c(pos - 1L, pos + 1L, pos - n, pos + n))
}

insert <- function(liste, elt) {
    # print(liste)
    # print(elt)
    k <- 1
    while (k <= length(liste) && liste[[k]][3] < elt[3]) {
        k <- k + 1
    }
    if (k == 1) return(c(list(elt), liste))
    if (k == (length(liste) + 1)) return(c(liste, list(elt)))
    return(c(liste[1:(k - 1)], list(elt), liste[k:length(liste)]))
}
#
# {
# n <- 71
# # n <- 7
# datam <- datax |>
#     strsplit(",") |>
#     lapply(\(x) as.numeric(x) + 1)
#
# datar <- matrix(".", nrow = n, ncol = n)
# # for (pos in datam[1:1024]) {
# #     datar[pos[2], pos[1]] <- "#"
# # }
#
# datar <- datar |>
#     rbind("#", ... = _, "#") |>
#     cbind("#", ... = _, "#")
#
# block <- NULL
# for (col in 1:ncol(datar)) {
#     for (row in 1:nrow(datar)) {
#         if (datar[row, col] == "#") {
#             block <- c(block, row + n * (col - 1L))
#         }
#     }
# }
#
# start <- n + 2 + 2
# end <- (n + 2) ** 2 - (n + 2) - 1
# datal <- datar
#
# k <- 1
# cond<- TRUE
# while (k <= length(datam) && cond) {
#     print(k)
#     print(datam[[k]] - 1)
#     datal[datam[[k]][2] + 1,datam[[k]][1] + 1] <- "#"
#
#     datar <- datal
#     datar[datar == "#"] <- -1
#     datar[datar == "."] <- Inf
#     datar <- as.numeric(datar)
#
#     list_pos <- list(c(start, 0,
#                        sum(get_position_vect(end, n + 2) - get_position_vect(start, n + 2))))
#
#     step <- 0
#     best <- Inf
#
#     while (best == Inf && length(list_pos) > 0) {
#         elt <- list_pos[[1]]
#         list_pos <- list_pos[-1]
#         # cat("\n\nelt : ")
#         # print(elt)
#
#         pos <- elt[1]
#         step <- elt[2]
#
#         if (pos == end) {
#             if (step < best) best <- step
#             print("trouvé")
#         }
#
#         neighboors <- get_neighboors(pos, n + 2)
#         # cat("voisins :")
#         # print(neighboors)
#         for (neighboor in neighboors) {
#             # cat("ce voisin :")
#             # print(neighboor)
#             # cat("sa valeur")
#             # print(datar[neighboor])
#             if (step + 1 < min(datar[neighboor], best)) {
#                 datar[neighboor] <- step + 1
#                 list_pos <- insert(list_pos, c(neighboor, step + 1, sum(get_position_vect(end, n + 2) - get_position_vect(neighboor, n + 2))))
#                 # print(list_pos)
#             }
#         }
#     }
#     cond <- best < Inf
#     k <- k + 1
# }
# print(k - 1)
# #239 not right
# #250 too high
# }

dir.create("2024/Day18/img", recursive = TRUE)

n <- 71
next_col <- 3
color_grid <- matrix(0L, ncol = n, nrow = n) |>
    rbind(1L, ... = _, 2L) |>
    cbind(2L, ... = _, 1L)

list_blocks <- datax |>
    strsplit(",") |>
    lapply(rev) |>
    lapply(as.integer) |>
    lapply(`+`, 2)

for (id_block in seq_along(list_blocks)) {
    block <- list_blocks[[id_block]]
    neighboors <- color_grid[block[1] + (-1:1), block[2] + (-1:1)]
    col_neighboors <- neighboors[neighboors != 0] |> unique() |> sort()
    if (all(1L:2L %in% col_neighboors)) {
        print("ici")
        print(id_block)
        print(rev(block - 2))
        color_grid[color_grid %in% col_neighboors] <- col_neighboors[1L]
        color_grid[block[1], block[2]] <- col_neighboors[1L]
        # print(image(color_grid))
        break
    } else if (length(col_neighboors) > 0) {
        color_grid[color_grid %in% col_neighboors] <- col_neighboors[1L]
        color_grid[block[1], block[2]] <- col_neighboors[1L]
    } else {
        color_grid[block[1], block[2]] <- next_col
        next_col <- next_col + 1L
    }
    png(filename = paste0("2024/Day18/img/img_", sprintf("%04d", id_block), ".png"))
    print(image(color_grid))
    dev.off()
}

# Create GIF
library("magick")

## list file names and read in
imgs <- list.files("2024/Day18/img", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 20)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "2024/Day18/img/datax.gif")

# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

# solve_day18_part1(datax_example)
# solve_day18_part1(datax)


## Part 2 ----------------------------------------------------------------------

# solve_day18_part2(datax_example)
# solve_day18_part2(datax)

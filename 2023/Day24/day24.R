# Titre : day24.R
# Auteur : Tanguy

##############
##  DAY 24  ##
##############

# Import data ------------------------------------------------------------------

trajectories <- readLines("./2023/Day24/trajectories.txt")
trajectories_example <- readLines("./2023/Day24/trajectories_example.txt")


# Déclaration fonction ---------------------------------------------------------

get_a_b <- function(k, data_trajectories) {
    y0 <- data_trajectories[k, 2]
    x0 <- data_trajectories[k, 1]
    vy <- data_trajectories[k, 5]
    vx <- data_trajectories[k, 4]
    return(c(vy / vx, y0 - vy / vx * x0))
}

init <- function(data_trajectories) {
    data_trajectories <- data_trajectories |>
        gsub(pattern = "@", replacement = ",") |>
        read.table(text = _, sep = ",")

    data_trajectories <- cbind(
        data_trajectories,
        t(sapply(seq_len(nrow(data_trajectories)), get_a_b, data_trajectories))
    )

    colnames(data_trajectories) <- c("x", "y", "z", "vx", "vy", "vz", "a", "b")

    return(data_trajectories)
}

find_intersect <- function(hail_i, hail_j, mini, maxi) {

    if (hail_i[1, 7] == hail_j[1, 7]) return(FALSE)

    x_int <- (hail_i[1, 8] - hail_j[1, 8]) / (hail_j[1, 7] - hail_i[1, 7])
    y_int <- hail_i[1, 7] * x_int + hail_i[1, 8]

    time_crashed_i <- ((x_int - hail_i[1, 1]) / hail_i[1, 4])
    time_crashed_j <- ((x_int - hail_j[1, 1]) / hail_j[1, 4])

    return(x_int >= mini
           && y_int >= mini
           && x_int <= maxi
           && y_int <= maxi
           && time_crashed_j >= 0
           && time_crashed_i >= 0)
}

solve_day24_part1 <- function(data_trajectories, mini, maxi) {
    data_trajectories <- init(data_trajectories)
    count <- 0

    for (i in 2:nrow(data_trajectories)) {
        hail_i <- data_trajectories[i, ]
        for (j in seq_len(i - 1)) {
            hail_j <- data_trajectories[j, ]
            count <- count + find_intersect(hail_i, hail_j, mini, maxi)
        }
    }
    return(count)
}

solve_day24_part2 <- function(data_trajectories) {
    data_trajectories <- init(data_trajectories)

    data_x_y <- data_trajectories
    data_x_y$c0 <- data_x_y$x
    data_x_y$c1 <- data_x_y$vy
    data_x_y$c2 <- -data_x_y$y
    data_x_y$c3 <- -data_x_y$vx
    data_x_y$c5 <- data_x_y$y * data_x_y$vx - data_x_y$x * data_x_y$vy

    data_x_z <- data_trajectories
    data_x_z$c0 <- data_x_z$x
    data_x_z$c1 <- data_x_z$vz
    data_x_z$c2 <- -data_x_z$z
    data_x_z$c3 <- -data_x_z$vx
    data_x_z$c5 <- data_x_z$z * data_x_z$vx - data_x_z$x * data_x_z$vz

    mod_x_y <- lm(formula = -c5 ~ c0 + c1 + c2 + c3, data = data_x_y)
    mod_x_z <- lm(formula = -c5 ~ c0 + c1 + c2 + c3, data = data_x_z)

    return(as.numeric(mod_x_y$coefficients["c1"]
                      + mod_x_y$coefficients["c3"]
                      + mod_x_z$coefficients["c3"]))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day24_part1(trajectories_example, 7, 27)
solve_day24_part1(trajectories, 200000000000000, 400000000000000)


## Part 2 ----------------------------------------------------------------------

solve_day24_part2(trajectories_example) |> dput()
solve_day24_part2(trajectories) |> dput()

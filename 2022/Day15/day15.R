# Titre : day15.R
# Auteur : Tanguy

##############
##  DAY 15  ##
##############


# Import data ------------------------------------------------------------------

beacons <- read.table("./2022/Day15/beacons.txt")
beacons_example <- read.table("./2022/Day15/beacons_example.txt")


# Déclaration fonction ---------------------------------------------------------

traitement <- function(data_beacons) {
    data_beacons <- data_beacons |>
        dplyr::mutate(
            sx = substr(V3, 3, nchar(V3) - 1) |> as.numeric(),
            sy = substr(V4, 3, nchar(V4) - 1) |> as.numeric(),
            bx = substr(V9, 3, nchar(V9) - 1) |> as.numeric(),
            by = substr(V10, 3, nchar(V10)) |> as.numeric(),
            distance = abs(sx - bx) + abs(sy - by)
        )
    return(data_beacons)
}

get_one_frontier <- function(sensor) {
    sx <- sensor$sx
    sy <- sensor$sy
    distance <- sensor$distance

    data.frame(
        x = sx + c(
            seq(-distance, distance + 1),
            seq(-distance - 1, distance)
        ),
        y = sy + c(
            seq(1, distance + 1), seq(distance, 0),
            -seq(0, distance + 1), -seq(distance, 1)
        )
    )
}

solve_day15_part1 <- function(data_beacons, line) {
    data_beacons <- data_beacons |> traitement()
    pos_beacon <- data_beacons |>
        subset(by == line) |>
        dplyr::pull(bx) |>
        unique()

    maxi <- max(data_beacons$sx + data_beacons$distance)
    mini <- min(data_beacons$sx - data_beacons$distance)
    v <- logical(maxi - mini + 1)

    for (k in seq_len(nrow(data_beacons))) {
        dist <- data_beacons[k, ]$distance
        depassement <- dist - abs(data_beacons[k, ]$sy - line)
        if (depassement >= 0) {
            sx <- data_beacons[k, ]$sx
            v[seq(-depassement, depassement) + sx - mini + 1] <- TRUE
        }
    }

    v[pos_beacon - mini + 1] <- FALSE
    return(sum(v))
}

solve_day15_part2 <- function(data_beacons, borne) {
    data_beacons <- data_beacons |> traitement()
    fr_temp <- data.frame()
    idx_sensor <- 1

    while (idx_sensor <= nrow(data_beacons) && any(dim(fr_temp) == 0)) {
        fr <- get_one_frontier(data_beacons[idx_sensor, ])
        fr_temp <- fr |> subset((x >= 0)
                                & (x <= borne)
                                & (y >= 0)
                                & (y <= borne))
        idx_sensor2 <- 1

        while (idx_sensor2 <= nrow(data_beacons) && all(dim(fr_temp) > 0)) {
            if (idx_sensor2 != idx_sensor) {
                dist_i <- data_beacons[idx_sensor2, "distance"]
                fr_temp <- fr_temp |>
                    subset(
                        (abs(x - data_beacons[idx_sensor2, ]$sx) +
                             abs(y - data_beacons[idx_sensor2, ]$sy)) > dist_i
                    )
            }
            idx_sensor2 <- idx_sensor2 + 1
        }

        if (all(dim(fr_temp) != 0)) {
            return(4000000 * fr_temp$x + fr_temp$y)
        }
        idx_sensor <- idx_sensor + 1
    }
    stop("Il n'y a pas de solution.")
}


# Execution --------------------------------------------------------------------

solve_day15_part1(beacons_example, 10)
solve_day15_part1(beacons, 2000000)

solve_day15_part2(beacons_example, 20)
solve_day15_part2(beacons, 4000000) |> dput()

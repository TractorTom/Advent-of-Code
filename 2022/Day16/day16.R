# Titre : day16.R
# Auteur : Tanguy

##############
##  DAY 16  ##
##############


# Import data ------------------------------------------------------------------

valves <- readLines("./2022/Day16/valves.txt")
valves_example <- readLines("./2022/Day16/valves_example.txt")


# Déclaration fonction ---------------------------------------------------------

traitement <- function(data_valves) {
    return(data_valves |>
        strsplit("; ") |>
        do.call(what = rbind) |>
        as.data.frame() |>
        dplyr::mutate(
            num = seq_len(dplyr::n()),
            origin = substr(V1, 7, 8),
            flow = substr(V1, 24, nchar(V1)) |> as.numeric(),
            out = substr(V2, 23, nchar(V2)) |>
                gsub(pattern = " ", replacement = "")
        ))
}

get_neighborhood <- function(data_valves) {
    val <- data_valves$num
    names(val) <- data_valves$origin

    neighborhood <- matrix(
        data = Inf,
        nrow = nrow(data_valves),
        ncol = nrow(data_valves)
    )
    colnames(neighborhood) <- data_valves$origin
    rownames(neighborhood) <- data_valves$origin

    for (valve in seq_len(nrow(data_valves))) {
        neighbors <- data_valves[valve, "out"] |>
            strsplit(",") |>
            unlist()
        neighborhood[valve, val[neighbors]] <- val[neighbors]
    }

    return(neighborhood)
}

get_neighbors <- function(valve, neighborhood) {
    neighbors <- neighborhood[valve, ] |> unique()
    return(neighbors[neighbors != Inf])
}

get_distance <- function(neighborhood) {
    distance <- neighborhood != Inf
    distance[!distance] <- Inf

    for (valve in seq_len(nrow(distance))) {
        distance[valve, valve] <- 0
    }

    for (step in seq_len(nrow(distance))) {
        for (valve in seq_len(nrow(distance))) {
            neighbors <- get_neighbors(valve, neighborhood)
            distance[valve, ] <- c(
                list(distance[valve, ]),
                lapply(neighbors, \(x) 1 + distance[, x])
            ) |>
                purrr::reduce(.f = pmin)
        }
    }

    return(distance)
}

one_person <- function(flows, distance, pos, score, closed, time) {
    if (time <= 0 || length(closed) == 0) {
        return(score)
    }

    score_tot <- score

    for (k in closed) {
        distance_k <- distance[pos, k]
        score_k <- (time - distance_k - 1) * flows[k]

        if (score_k > 0) {
            score_tot <- max(
                score_tot,
                one_person(flows, distance,
                    pos = k, score = score + score_k,
                    closed = closed[closed != k], time = time - distance_k - 1
                )
            )
        }
    }

    return(score_tot)
}

human_and_elephant <- function(flows, distance, start, closed, time) {
    sep <- length(closed) %/% 2

    all_perm <- combn(closed, sep)
    score <- 0

    for (k in seq_len(ncol(all_perm))) {
        score_1 <- one_person(
            flows = flows,
            distance = distance,
            pos = start,
            score = 0,
            closed = all_perm[, k],
            time = time
        )
        score_2 <- one_person(
            flows = flows,
            distance = distance,
            pos = start,
            score = 0,
            closed = closed[!closed %in% all_perm[, k]],
            time = time
        )

        score <- max(score, score_1 + score_2)
    }

    return(score)
}

solve_day16_part1 <- function(data_valves) {
    data_valves <- traitement(data_valves)
    neighborhood <- get_neighborhood(data_valves)
    distance <- get_distance(neighborhood)
    starting <- which(data_valves$origin == "AA")
    closed <- seq_len(nrow(data_valves))[data_valves$flow != 0]

    return(one_person(data_valves$flow, distance,
        pos = starting, score = 0,
        closed = closed, time = 30
    ))
}

solve_day16_part2 <- function(data_valves) {
    data_valves <- traitement(data_valves)
    neighborhood <- get_neighborhood(data_valves)
    distance <- get_distance(neighborhood)
    starting <- which(data_valves$origin == "AA")
    closed <- seq_len(nrow(data_valves))[data_valves$flow != 0]

    return(human_and_elephant(data_valves$flow, distance,
        start = starting,
        closed = closed, time = 26
    ))
}


# Execution --------------------------------------------------------------------

solve_day16_part1(valves_example)
solve_day16_part1(valves)

solve_day16_part2(valves_example)
solve_day16_part2(valves)

# Titre  <- read.table(text = " day05.R
# Auteur  <- read.table(text = " Tanguy

##############
##  DAY 05  ##
##############


# Import data -------------------------------------------------------------

maps <- readLines("./2023/Day05/maps.txt")
maps_example <- readLines("./2023/Day05/maps_example.txt")


# Déclaration fonction ----------------------------------------------------

init <- function(data_maps) {
    out <- list(seeds = maps[2] |>
                    read.delim(text = _, sep = " ", header = FALSE) |>
                    as.numeric())
    k <- 4
    index_start <- 4

    while (k <= length(data_maps)) {
        val <- data_maps[k]
        if (val == "") {

            name <- data_maps[index_start] |>
                strsplit(split = " map:") |>
                unlist()
            tab <- read.table(text = data_maps[(index_start + 1):(k - 1)])
            out[[name]] <- tab

            index_start <- k + 1
            k <- k + 3
        } else {
            k <- k + 1
        }
    }
    name <- data_maps[index_start] |>
        strsplit(split = " map:") |>
        unlist()
    tab <- read.table(text = data_maps[(index_start + 1):length(data_maps)])
    out[[name]] <- tab

    return(out)
}

next_category <- function(val, convert_table) {
    for (h in seq_len(nrow(convert_table))) {
        if ((val >= convert_table[h, 2])
            && (val <= convert_table[h, 2] + convert_table[h, 3])) {
            incr <- val - convert_table[h, 2]
            return(convert_table[h, 1] + incr)
        }
    }
    return(val)
}

solve_day05_part1 <- function(data_maps) {

    res <- init(data_maps)
    v_loc <- NULL

    for (seed in res$seeds) {
        location <- seed |>
            next_category(res$seed_to_soil) |>
            next_category(res$soil_to_fertilizer) |>
            next_category(res$fertilizer_to_water) |>
            next_category(res$water_to_light) |>
            next_category(res$light_to_temperature) |>
            next_category(res$temperature_to_humidity) |>
            next_category(res$humidity_to_location)
        v_loc <- c(location, v_loc)
    }

    return(min(v_loc))
}

# Ce qui se trouve dans int1 et int2
intersect_intervals <- function(int1, int2) {
    if ((int1[2] < int2[1]) || (int2[2] < int1[1])) {
        return(NULL)
    }
    return(c(
        max(int1[1], int2[1]),
        min(int1[2], int2[2])
    ))
}

# Ce qui se trouve dans int1 et pas dans int2
setdiff_intervals <- function(int1, int2) {
    out <- list()
    if (int1[1] < int2[1]) {
        out <- c(out, list(c(int1[1], min(int2[1] - 1, int1[2]))))
    }
    if (int1[2] > int2[2]) {
        out <- c(out, list(c(max(int2[2] + 1, int1[1]), int1[2])))
    }
    return(out)
}

next_category_intervals <- function(list_intervals, convert_table) {
    out <- list()
    remaining <- list_intervals
    for (h in seq_len(nrow(convert_table))) {

        interval_dom <- c(convert_table[h, 2],
                          convert_table[h, 2] + convert_table[h, 3] - 1)
        translation <- convert_table[h, 1] - convert_table[h, 2]

        new_remaining <- NULL
        for (interval in remaining) {
            out1 <- intersect_intervals(interval, interval_dom)
            if (length(out1) > 0) {
                out <- c(out, list(out1 + translation))
            }
            remain1 <- setdiff_intervals(interval, interval_dom)
            if (length(remain1) > 0) {
                new_remaining <- c(new_remaining, remain1)
            }
        }

        remaining <- new_remaining
    }
    return(c(out, remaining))
}

solve_day05_part2 <- function(data_maps) {

    res <- init(data_maps)

    seeds_list <- list()
    for (k in 1:(length(res$seeds) / 2)) {
        val <- c(res$seeds[2 * k - 1],
                 res$seeds[2 * k - 1] + res$seeds[2 * k] - 1)
        seeds_list <- c(seeds_list, list(val))
    }

    loc <- seeds_list |>
        next_category_intervals(res$seed_to_soil) |>
        next_category_intervals(res$soil_to_fertilizer) |>
        next_category_intervals(res$fertilizer_to_water) |>
        next_category_intervals(res$water_to_light) |>
        next_category_intervals(res$light_to_temperature) |>
        next_category_intervals(res$temperature_to_humidity) |>
        next_category_intervals(res$humidity_to_location)

    return(do.call(rbind, loc)[, 1] |> min())
}


# Execution ---------------------------------------------------------------

## Part 1 ------------------------------------------------------------------

solve_day05_part1(maps_example)
solve_day05_part1(maps)


## Part 2 ------------------------------------------------------------------

solve_day05_part2(maps_example)
solve_day05_part2(maps)

# Titre : day10.R
# Auteur : Tanguy

##############
##  DAY 10  ##
##############


# Import data ------------------------------------------------------------------

cpu <- readLines("./2022/Day10/cpu.txt")
cpu_example <- readLines("./2022/Day10/cpu_example.txt")
cpu_example2 <- readLines("./2022/Day10/cpu_example2.txt")


# Déclaration fonction ---------------------------------------------------------

compute_value <- function(data_cpu) {
    data_cpu |>
        strsplit(split = " ") |>
        do.call(what = rbind) |>
        as.data.frame() |>
        dplyr::rename(instr = 1, add_value = 2) |>
        dplyr::mutate(
            add_value = dplyr::case_when(
                add_value == "noop" ~ "0",
                TRUE ~ add_value
            ) |> as.numeric(),
            value = cumsum(add_value) + 1,
            instant_value = value - add_value,
            add_cycle = 1 + (instr == "addx"),
            cycle = cumsum(add_cycle)
        )
}

solve_day10_part1 <- function(data_cpu) {
    data_cpu |>
        compute_value() |>
        dplyr::mutate(
            strength = dplyr::case_when(
                (cycle == 20 || (cycle - 20) %% 40 == 0) ~ cycle * instant_value,
                (instr == "addx") &
                    ((cycle - 1) == 20 ||
                        (cycle - 20 - 1) %% 40 == 0) ~ (cycle - 1) * instant_value,
                TRUE ~ 0
            )
        ) |>
        dplyr::pull(strength) |>
        sum()
}

solve_day10_part2 <- function(data_cpu) {
    data_cpu |>
        compute_value() |>
        dplyr::mutate(
            min_sprite = instant_value,
            max_sprite = instant_value + 2,
            index_pixel = ((cycle - 1) %% 40 + 1),
            index_pixel_1 = ((cycle - 1 - 1) %% 40 + 1),
            crt_pixel = dplyr::case_when(
                index_pixel >= min_sprite && index_pixel <= max_sprite ~ "#",
                TRUE ~ "."
            ),
            crt_pixel = dplyr::case_when(
                (instr == "addx") &
                    (index_pixel_1 >= min_sprite) &
                    (index_pixel_1 <= max_sprite) ~ paste0("#", crt_pixel),
                instr == "addx" ~ paste0(".", crt_pixel),
                TRUE ~ crt_pixel
            )
        ) |>
        dplyr::pull(crt_pixel) |>
        paste0(collapse = "") |>
        strsplit(split = "") |>
        unlist() |>
        matrix(ncol = 40, byrow = TRUE) |>
        apply(MARGIN = 1, \(x) {
            x |>
                paste0(collapse = "") |>
                print()
        })
    return(NULL)
}


# Execution --------------------------------------------------------------------

solve_day10_part1(cpu_example)
solve_day10_part1(cpu_example2)
solve_day10_part1(cpu)

solve_day10_part2(cpu_example)
solve_day10_part2(cpu_example2)
solve_day10_part2(cpu)

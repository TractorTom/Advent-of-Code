# Titre : day03.R
# Auteur : Tanguy

##############
##  DAY 03  ##
##############


# Import data -------------------------------------------------------------

rucksack <- read.table("./2022/Day03/rucksack.txt")
rucksack_example <- read.table("./2022/Day03/rucksack_example.txt")


# Déclaration fonction ----------------------------------------------------

priority <- data.frame(letter = c(letters, LETTERS), 
                       priority = 1:52)

solve_day03_part1 <- function(data_rucksack) {
    data_rucksack |> 
        dplyr::rename(content = 1) |> 
        dplyr::mutate(
            first_compartment = substr(content, 1, nchar(content) / 2), 
            second_compartment = substr(content, 1 + nchar(content) / 2, nchar(content)), 
            common_item = purrr::map2_chr(first_compartment, second_compartment, 
                                          \(x, y) intersect(strsplit(x, "") |> unlist(), 
                                                            strsplit(y, "") |> unlist()))
        ) |> 
        merge(y = priority, by.x = "common_item", by.y = "letter") |> 
        dplyr::pull(priority) |> 
        sum()
}

solve_day03_part2 <- function(data_rucksack) {
    data_rucksack |> 
        dplyr::rename(content = 1) |> 
        dplyr::mutate(group = rep(1:(dplyr::n() / 3), each = 3)) |> 
        dplyr::group_by(group) |> 
        dplyr::summarise(common_item = Reduce(f = intersect, x = strsplit(content, ""))) |> 
        merge(y = priority, by.x = "common_item", by.y = "letter") |> 
        dplyr::pull(priority) |> 
        sum()
}


# Execution ---------------------------------------------------------------

solve_day03_part1(rucksack_example)
solve_day03_part1(rucksack)

solve_day03_part2(rucksack_example)
solve_day03_part2(rucksack)

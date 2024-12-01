# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############


# Import data ------------------------------------------------------------------

id <- read.table("./2024/Day01/location_id.txt")
id_example <- read.table("./2024/Day01/location_id_example.txt")


# Déclaration fonction ---------------------------------------------------------

solve_day01_part1 <- function(id_table) {
    return(sum(abs(sort(id_table$V1) - sort(id_table$V2))))
}

compute_occurences_score <- function(value, right_list) {
    return(sum(value == right_list))
}

solve_day01_part2 <- function(id_table) {
    occurences_scores <- vapply(
        X = id_table$V1,
        FUN = compute_occurences_score,
        right_list = id_table$V2,
        FUN.VALUE = integer(1L)
    )
    return(sum(id_table$V1 * occurences_scores))
}

solve_day01_part2_bis <- function(id_table) {
    occurences_scores <- table(id_table$V2)[as.character(id_table$V1)]
    return(sum(id_table$V1 * occurences_scores, na.rm = TRUE))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day01_part1(id_example)
solve_day01_part1(id)


## Part 2 ----------------------------------------------------------------------

solve_day01_part2(id_example)
solve_day01_part2(id)

# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############


# Import data ------------------------------------------------------------------

calorie <- readLines("./2022/Day01/calorie.txt")
calorie_example <- readLines("./2022/Day01/calorie_example.txt")


# Déclaration fonction ---------------------------------------------------------

traitement_calorie <- function(tab) {
    tab |>
        as.data.frame() |>
        dplyr::rename(calorie = 1) |>
        dplyr::mutate(
            elf_id = cumsum(calorie == "") + 1,
            calorie_val = ifelse(calorie == "", 0, as.numeric(calorie))
        ) |>
        dplyr::group_by(elf_id) |>
        dplyr::summarise(somme_calorie = sum(calorie_val)) |>
        dplyr::pull(somme_calorie) |>
        sort(decreasing = TRUE)
}

table_somme_calorie_example <- traitement_calorie(calorie_example)
table_somme_calorie <- traitement_calorie(calorie)


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

table_somme_calorie_example[1]
table_somme_calorie[1]


## Part 2 ----------------------------------------------------------------------

table_somme_calorie_example[1:3] |> sum()
table_somme_calorie[1:3] |> sum()

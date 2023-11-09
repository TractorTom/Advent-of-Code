# Titre : day04.R
# Auteur : Tanguy

##############
##  DAY 04  ##
##############


# Import data -------------------------------------------------------------

assignments <- read.table("./2022/Day04/assignments.txt", sep = ",")
assignments_example <- read.table("./2022/Day04/assignments_example.txt", sep = ",")


# Déclaration fonction ----------------------------------------------------

prepare_data <- function(section_assignments) {
    section_assignments |>
        tidyr::separate(V1, sep = "-", into = c("borne_inf_1", "borne_sup_1"), convert = TRUE) |>
        tidyr::separate(V2, sep = "-", into = c("borne_inf_2", "borne_sup_2"), convert = TRUE) |>
        dplyr::mutate(
            fully_contains = ((borne_inf_1 <= borne_inf_2) && (borne_sup_1 >= borne_sup_2)) |
                ((borne_inf_2 <= borne_inf_1) && (borne_sup_2 >= borne_sup_1)),
            overlaps = fully_contains || (borne_inf_1 >= borne_inf_2 && borne_inf_1 <= borne_sup_2) |
                (borne_inf_2 >= borne_inf_1 && borne_inf_2 <= borne_sup_1)
        )
}


# Execution ---------------------------------------------------------------

assignments_example |>
    prepare_data() |>
    dplyr::pull(fully_contains) |>
    sum()
assignments |>
    prepare_data() |>
    dplyr::pull(fully_contains) |>
    sum()

assignments_example |>
    prepare_data() |>
    dplyr::pull(overlaps) |>
    sum()
assignments |>
    prepare_data() |>
    dplyr::pull(overlaps) |>
    sum()

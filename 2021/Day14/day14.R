#Titre : day14.R
#Auteur : Tanguy

##############
##  DAY 14  ##
##############

######OPTIONS######

options(digits = 20)

######IMPORT DATA######

polymer <- readLines("./2021/Day14/polymer.txt")
polymer_example <- readLines("./2021/Day14/polymer_example.txt")

######TRAITEMENT dataPolymer######

traitement <- function(dataPolymer) {

    rules <- data.frame(do.call(
        what = rbind,
        args = strsplit(dataPolymer[3:length(dataPolymer)], " -> ")
    ))
    colnames(rules) <- c("from", "to")

    translation <- data.frame(motif = rules$from, count = 0)

    template <- unlist(strsplit(dataPolymer[1], ""))

    for (index in 2:length(template)) {
        motif_temp <- paste0(template[c(index - 1, index)], collapse = "")
        translation[translation$motif == motif_temp, "count"] <-
            translation[translation$motif == motif_temp, "count"] + 1
    }

    return(list(translation = translation, rules = rules, template = template))
}

polymer <- traitement(polymer)
polymer_example <- traitement(polymer_example)

######DECLARATION FONCTION######

polymerize_brut <- function(chain, rules) {
    new_line <- chain[1]
    for (index in 2:length(chain)) {
        extract <- paste0(chain[c(index - 1, index)], collapse = "")
        adding_char <- rules[rules[, 1] == extract, 2]
        new_line <- c(new_line, adding_char, chain[index])
    }
    return(new_line)
}

polymerize_thoughful <- function(translation, rules) {
    new_table <- translation
    new_table$count <- 0
    for (mot in translation$motif[translation$count != 0]) {

        p <- rules[rules[, 1] == mot, 2]

        new_mot <- paste0(substr(mot, 1, 1), p)
        new_table[new_table$motif == new_mot, "count"] <-
            new_table[new_table$motif == new_mot, "count"] +
            translation[translation$motif == mot, "count"]

        new_mot <- paste0(p, substr(mot, 2, 2))
        new_table[new_table$motif == new_mot, "count"] <-
            new_table[new_table$motif == new_mot, "count"] +
            translation[translation$motif == mot, "count"]

    }

    return(new_table)
}

occurrence_in_translation <- function(translate_tab, first_char, last_char) {
    list_char <- unique(unlist(strsplit(
        x = paste0(translate_tab$motif, collapse = ""),
        split = ""
    )))
    occ <- rep(0, length(list_char))
    names(occ) <- list_char

    for (index in seq_len(nrow(translate_tab))) {
        decompo_char1 <- substr(translate_tab[index, "motif"], 1, 1)
        decompo_char2 <- substr(translate_tab[index, "motif"], 2, 2)

        occ[decompo_char1] <- occ[decompo_char1] + translate_tab[index, "count"]
        occ[decompo_char2] <- occ[decompo_char2] + translate_tab[index, "count"]
    }

    occ[first_char] <- occ[first_char] + 1
    occ[last_char] <- occ[last_char] + 1

    return(occ / 2)
}

solve_day14_part1 <- function(dataPolymer, nb_step = 10) {
    chain <- dataPolymer$template
    for (step in 1:nb_step) {
        chain <- polymerize_brut(chain, rules = dataPolymer$rules)
    }
    occurrence <- table(chain)
    return(max(occurrence) - min(occurrence))
}

solve_day14_part2 <- function(dataPolymer, nb_step = 40) {

    translation <- dataPolymer$translation
    for (step in 1:nb_step) {
        translation <- polymerize_thoughful(
            translation = translation,
            rules = dataPolymer$rules
        )
    }
    occurrence <- occurrence_in_translation(
        translate_tab = translation,
        first_char = dataPolymer$template[1],
        last_char = dataPolymer$template[length(dataPolymer$template)]
    )
    return(max(occurrence) - min(occurrence))
}


######EXECUTION######

solve_day14_part1(polymer_example)
solve_day14_part1(polymer)

solve_day14_part2(polymer_example)
solve_day14_part2(polymer)

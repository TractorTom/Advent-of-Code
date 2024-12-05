# Titre : day05.R
# Auteur : Tanguy

##############
##  DAY 05  ##
##############


# Import data ------------------------------------------------------------------

manual <- readLines(file.path("2024", "Day05", "manual.txt"))
manual_example <- readLines(file.path("2024", "Day05", "manual_example.txt"))


# Déclaration fonction ---------------------------------------------------------

pre_treatment <- function(data_manual) {
    sep <- which(data_manual == "")
    rules <- data_manual[seq_len(sep - 1L)] |>
        strsplit(split = "|", fixed = TRUE) |>
        lapply(as.numeric) |>
        do.call(what = rbind) |>
        as.data.frame()
    updates <- data_manual[(sep + 1L):(length(data_manual))]

    return(list(rules = rules, updates = updates))
}

is_sorted <- function(x, rules) {
    for (j in seq_len(nrow(rules))) {
        pos1 <- which(x == rules[j, "V1"])
        pos2 <- which(x == rules[j, "V2"])
        if (pos1 > pos2) {
            return(FALSE)
        }
    }
    return(TRUE)
}

solve_day05_part1 <- function(data_manual) {

    l_manual <- pre_treatment(data_manual)
    rules <- l_manual[["rules"]]
    updates <- l_manual[["updates"]]

    output <- 0L
    for (u in updates) {
        pages <- strsplit(u, split = ",", fixed = TRUE) |>
            unlist() |>
            as.numeric()
        rrules <- rules |> subset(V1 %in% pages & V2 %in% pages)
        cond <- is_sorted(pages, rrules)
        if (cond) {
            output <-  output + pages[(length(pages) + 1L) / 2L]
        }
    }
    return(output)
}

rearrange <- function(x, rules) {
    out <- NULL
    while (length(x) > 1L) {
        never_second <- setdiff(x, rules[["V2"]])
        out <- c(out, never_second)
        rules <- rules |> subset(V1 != never_second)
        x <- x[x != never_second]
    }
    return(c(out, x))
}

solve_day05_part2 <- function(data_manual) {
    l_manual <- pre_treatment(data_manual)
    rules <- l_manual[["rules"]]
    updates <- l_manual[["updates"]]

    output <- 0L
    for (u in updates) {
        pages <- strsplit(u, split = ",", fixed = TRUE) |>
            unlist() |>
            as.numeric()
        rrules <- rules |> subset(V1 %in% pages & V2 %in% pages)
        cond <- is_sorted(pages, rrules)
        if (!cond) {
            pages <- rearrange(pages, rrules)
            output <- output + pages[(length(pages) + 1L) / 2L]
        }
    }
    return(output)
}



# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day05_part1(manual_example)
solve_day05_part1(manual)


## Part 2 ----------------------------------------------------------------------

solve_day05_part2(manual_example)
solve_day05_part2(manual)

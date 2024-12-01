# Titre : day19.R
# Auteur : Tanguy

##############
##  DAY 19  ##
##############


# Import data ------------------------------------------------------------------

system <- readLines("./2023/Day19/system.txt")
system_example <- readLines("./2023/Day19/system_example.txt")


# Déclaration fonction ---------------------------------------------------------

init <- function(data_systems) {
    k <- 1
    while (data_systems[k] !=  "") {
        k <- k + 1
    }
    workflows <- data_systems[seq_len(k - 1)]

    ratings <- data_systems[(k + 1):length(data_systems)] |>
        gsub(pattern = "{", replacement = "list(", fixed = TRUE) |>
        gsub(pattern = "}", replacement = ")", fixed = TRUE)

    ratings <- lapply(X = ratings, FUN = \(x) x |> parse(text = _) |> eval())
    workflows <- list()
    for (wf in data_systems[seq_len(k - 1)]) {
        wf <- strsplit(wf, split = "{", fixed = TRUE) |> unlist()
        wf_name <- wf[1]
        wf <- wf[2]
        wf <- substr(x = wf, start = 1, stop = nchar(wf) - 1)
        wf <- strsplit(wf, split = ",") |> unlist() |> list()
        names(wf) <- wf_name
        workflows <- c(workflows, wf)
    }

    return(list(ratings, workflows))
}

apply_one_workflow <- function(part, workflow) {
    for (k in workflow) {
        cond <- strsplit(k, ":") |> unlist()
        if (length(cond) == 2
            && eval(parse(text = cond[1]), envir = list2env(part))) {
            return(cond[2])
        } else if (length(cond) == 1) {
            return(cond)
        }
    }
}

apply_worflows_chain <- function(part, workflows) {
    wf_1 <- workflows[["in"]]
    out <- apply_one_workflow(part, wf_1)
    while (!(out %in% c("A", "R"))) {
        out <- apply_one_workflow(part, workflows[[out]])
    }
    return(out)
}

solve_day19_part1 <- function(data_system) {
    data_system <- init(data_system)

    ratings <- data_system[[1]]
    workflows_system <- data_system[[2]]

    somme <- 0
    for (part_k in ratings) {
        out <-  apply_worflows_chain(part_k, workflows_system)
        if (out == "A") somme <- somme + part_k |> as.numeric() |> sum()
    }
    return(somme)
}

solve_day19_part2 <- function(data_system) {
    data_system <- init(data_system)
    workflows <- data_system[[2]]

    aux_rec <- function(part, wf_name) {

        if (wf_name == "A") {
            return(part |>
                       vapply(FUN = length, FUN.VALUE = integer(1)) |>
                       prod())
        }
        if (wf_name == "R") return(0)

        somme <- 0
        wf <- workflows[[wf_name]]

        for (k in wf) {
            cond <- strsplit(k, ":") |> unlist()

            if (length(cond) == 2) {

                val_cond <- eval(parse(text = cond[1]), envir = list2env(part))
                arg <- cond[1] |>
                    strsplit(split = "<") |>
                    unlist() |>
                    strsplit(split = ">") |>
                    unlist() |>
                    base::`[`(1)

                part2 <- part
                part2[[arg]] <- part2[[arg]][val_cond]
                somme <- somme + aux_rec(part2, cond[2])
                part[[arg]] <- part[[arg]][!val_cond]


            } else if (length(cond) == 1) {
                somme <- somme + aux_rec(part, cond)
            }
        }
        return(somme)
    }
    return(aux_rec(list(x = 1:4000, m = 1:4000, a = 1:4000, s = 1:4000), "in"))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day19_part1(system_example)
solve_day19_part1(system)


## Part 2 ----------------------------------------------------------------------

solve_day19_part2(system_example) |> dput()
solve_day19_part2(system) |> dput()

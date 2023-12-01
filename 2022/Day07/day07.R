# Titre : day07.R
# Auteur : Tanguy

##############
##  DAY 07  ##
##############


# Import data ------------------------------------------------------------------

command <- readLines("./2022/Day07/command.txt")
command_example <- readLines("./2022/Day07/command_example.txt")


# Déclaration fonction ---------------------------------------------------------

get_index_cd <- function(data_command) {
    index_cd <- NULL
    for (k in seq_along(data_command)) {
        instr <- strsplit(data_command[k], " ") |> unlist()
        if (instr[1] == "$" && instr[2] == "cd" && instr[3] != "..") {
            index_cd <- c(index_cd, k)
        }
    }
    return(index_cd)
}

get_dir_content <- function(extr_command) {
    compte <- 1
    index <- 2
    while (index <= length(extr_command) && compte > 0) {
        instr <- strsplit(extr_command[index], " ") |> unlist()
        if (instr[1] == "$" && instr[2] == "cd" && instr[3] == "..") {
            compte <- compte - 1
        } else if (instr[1] == "$" && instr[2] == "cd") {
            compte <- compte + 1
        }
        index <- index + 1
    }

    return(extr_command[seq_len(index)])
}

count_repository_size <- function(dir_content) {
    all_size <- sapply(dir_content, FUN = function(instr) {
        first_part <- (strsplit(instr, " ") |> unlist())[1]
        if (!first_part %in% c("$", "dir")) {
            return(as.numeric(first_part))
        }
        return(0)
    })

    return(sum(all_size, na.rm = TRUE))
}

get_all_size <- function(data_command) {
    all_cd <- get_index_cd(data_command)
    all_size <- sapply(all_cd, FUN = function(i) {
        data_command[seq(i, length(data_command))] |>
            get_dir_content() |>
            count_repository_size()
    })
    return(all_size)
}

solve_day07_part1 <- function(data_command) {
    all_size <- get_all_size(data_command)
    return(all_size[all_size <= 100000] |> sum())
}

solve_day07_part2 <- function(data_command) {
    all_size <- get_all_size(data_command)
    return(all_size[all_size >= (30000000 - (70000000 - max(all_size)))] |> min())
}


# Execution --------------------------------------------------------------------

solve_day07_part1(command_example)
solve_day07_part1(command)

solve_day07_part2(command_example)
solve_day07_part2(command)

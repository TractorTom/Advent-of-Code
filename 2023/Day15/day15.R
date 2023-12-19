# Titre : day15.R
# Auteur : Tanguy

##############
##  DAY 15  ##
##############


# Import data ------------------------------------------------------------------

lens_library <- readLines("./2023/Day15/lens_library.txt")
lens_library_example <- readLines("./2023/Day15/lens_library_example.txt")


# Déclaration fonction ---------------------------------------------------------

hash <- function(txt) {
    return(
        c(0, utf8ToInt(txt)) |>
            Reduce(f = \(x, y) return(((x + y) * 17) %% 256))
    )
}

solve_day15_part1 <- function(data_lens) {
    data_lens <- strsplit(data_lens, ",") |> unlist()
    return(vapply(X = data_lens, FUN = hash, FUN.VALUE = numeric(1)) |> sum())
}

solve_day15_part2 <- function(data_lens) {

    data_lens <- strsplit(data_lens, ",") |> unlist()
    light_line <- lapply(1:256, \(x) NULL)

    for (elt in data_lens) {
        step <- elt |>
            strsplit(split = "=") |>
            unlist() |>
            strsplit(split = "-") |>
            unlist()

        label <- step[1]
        box <- hash(label)
        content <- light_line[[box + 1]]
        pos <- which(label == names(content))

        if (grepl("=", elt)) {
            focal_length <- step[2] |> as.integer()
            if (length(pos) == 1) {
                light_line[[box + 1]][pos] <- focal_length
            } else {
                light_line[[box + 1]] <- c(content, focal_length)
                names(light_line[[box + 1]])[length(content) + 1] <- label
            }
        } else if (grepl("-", elt)) {
            if (length(pos) == 1) {
                light_line[[box + 1]] <- content[-pos]
            }
        }
    }

    focusing_power <- sapply(
        X = seq_along(light_line),
        FUN = \(i) sum(i * light_line[[i]] * seq_along(light_line[[i]]))
    ) |> sum()

    return(focusing_power)
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day15_part1(lens_library_example)
solve_day15_part1(lens_library)


## Part 2 ----------------------------------------------------------------------

solve_day15_part2(lens_library_example)
solve_day15_part2(lens_library)

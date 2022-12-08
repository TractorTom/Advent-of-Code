# Titre : day06.R
# Auteur : Tanguy

##############
##  DAY 06  ##
##############


# Import data -------------------------------------------------------------

buffer <- readLines("./2022/Day06/buffer.txt")
buffer_example1 <- readLines("./2022/Day06/buffer_example1.txt")
buffer_example2 <- readLines("./2022/Day06/buffer_example2.txt")
buffer_example3 <- readLines("./2022/Day06/buffer_example3.txt")
buffer_example4 <- readLines("./2022/Day06/buffer_example4.txt")
buffer_example5 <- readLines("./2022/Day06/buffer_example5.txt")


# Déclaration fonction ----------------------------------------------------

pre_treatment <- function(data_buffer) {
    return(data_buffer |> strsplit(split = "") |> unlist())
}

find_first_marker <- function(data_buffer, length_marker = 4) {
    
    index <- 0
    test_marker <- c()
    while (length(unique(test_marker)) != length_marker) {
        index <- index + 1
        test_marker <- data_buffer[index:(index + length_marker - 1)]
    }
    return(index + length_marker - 1)
}


# Execution ---------------------------------------------------------------

# Part1
buffer_example1 |> pre_treatment() |> find_first_marker(length_marker = 4)
buffer_example2 |> pre_treatment() |> find_first_marker(length_marker = 4)
buffer_example3 |> pre_treatment() |> find_first_marker(length_marker = 4)
buffer_example4 |> pre_treatment() |> find_first_marker(length_marker = 4)
buffer_example5 |> pre_treatment() |> find_first_marker(length_marker = 4)
buffer |> pre_treatment() |> find_first_marker(length_marker = 4)

# Part2
buffer_example1 |> pre_treatment() |> find_first_marker(length_marker = 14)
buffer_example2 |> pre_treatment() |> find_first_marker(length_marker = 14)
buffer_example3 |> pre_treatment() |> find_first_marker(length_marker = 14)
buffer_example4 |> pre_treatment() |> find_first_marker(length_marker = 14)
buffer_example5 |> pre_treatment() |> find_first_marker(length_marker = 14)
buffer |> pre_treatment() |> find_first_marker(length_marker = 14)

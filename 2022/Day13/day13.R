# Titre : day13.R
# Auteur : Tanguy

##############
##  DAY 13  ##
##############


# Import data ------------------------------------------------------------------

signal <- readLines("./2022/Day13/distress_signal.txt")
signal_example <- readLines("./2022/Day13/distress_signal_example.txt")


# Déclaration fonction ---------------------------------------------------------

to_list <- function(data_signal) {
    data_signal |> 
        gsub(pattern = "[", replacement = "list(", fixed = TRUE) |> 
        gsub(pattern = "]", replacement = ")", fixed = TRUE) |>
        parse(text = _) |>
        eval()
}

compare <- function(signal_a, signal_b) {
    
    if ((is.null(signal_a) || (is.list(signal_a) && length(signal_a) == 0)) &&
        (is.null(signal_b) || (is.list(signal_b) && length(signal_b) == 0))) {
        return(2)
    } else if (is.null(signal_a) || (is.list(signal_a) && length(signal_a) == 0)) {
        return(1)
    } else if (is.null(signal_b) || (is.list(signal_b) && length(signal_b) == 0)) {
        return(0)
    } 
    
    if (!is.list(signal_a) && !is.list(signal_b)) {
        return((as.numeric(signal_a) <= as.numeric(signal_b)) + (as.numeric(signal_a) == as.numeric(signal_b)))
    } else if (!is.list(signal_a)) {
        return(compare(list(signal_a), signal_b))
    } else if (!is.list(signal_b)) {
        return(compare(signal_a, list(signal_b)))
    }
    
    for (index in seq_len(min(length(signal_a), length(signal_b)))) {
        res <- compare(signal_a[[index]], signal_b[[index]])
        if (res != 2) {
            return(res)
        }
    }
    
    return((length(signal_a) <= length(signal_b)) + (length(signal_a) == length(signal_b)))
}

solve_day13_part1 <- function(data_signal) {
    
    n <- (length(data_signal) + 1) %/% 3
    compte <- 0
    
    for (index in (seq_len(n) - 1)) {
        signal_1 <- data_signal[3 * index + 1] |> to_list()
        signal_2 <- data_signal[3 * index + 2] |> to_list()
        res <- compare(signal_1, signal_2)
        
        if (res == 1) {
            compte <- compte + (index + 1)
        }
    }
    return(compte)
}

solve_day13_part2 <- function(data_signal) {
    
    n <- (length(data_signal) + 1) %/% 3
    
    signal_deb <- "[[2]]" |> to_list()
    signal_fin <- "[[6]]" |> to_list()
    compte_deb <- 0
    compte_fin <- 0
    
    for (index in seq_len(length(data_signal))) {
        
        if (index %% 3 != 0) {
            signal_k <- data_signal[index] |> to_list()
            
            res <- compare(signal_deb, signal_k)
            compte_deb <- compte_deb + (res == 0)
            
            res <- compare(signal_k, signal_fin)
            compte_fin <- compte_fin + (res > 0)
        }
        
    }
    return((2 + compte_fin) * (1 + compte_deb))
}

# Execution --------------------------------------------------------------------

solve_day13_part1(signal_example)
solve_day13_part1(signal)

solve_day13_part2(signal_example)
solve_day13_part2(signal)

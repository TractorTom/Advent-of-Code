# Titre : day25.R
# Auteur : Tanguy

##############
##  DAY 25  ##
##############


# Import data ------------------------------------------------------------------

snafu <- read.table("./2022/Day25/snafu.txt")
snafu_example <- read.table("./2022/Day25/snafu_example.txt")

# Déclaration fonction ---------------------------------------------------------

convert_bob_10 <- (function(b_bob) {
    p5 <- 1
    somme <- 0
    for (k in 1:nchar(b_bob)) {
        v_bob <- b_bob |> substr(nchar(b_bob) - k + 1, nchar(b_bob) - k + 1)
        if (v_bob == "-") v_bob <- -1
        if (v_bob == "=") v_bob <- -2
        v10 <- as.numeric(v_bob) * p5
        somme <- somme + v10
        
        
        p5 <- p5 * 5
    }
    
    return(somme)
}) |> base::Vectorize()

convert_10_bob <- function(b10) {
    p5 <- 1
    b_bob <- ""
    while (b10 != 0) {
        v_bob <- (b10 %% (p5 * 5)) %/% p5
        b10 <- b10 - v_bob * p5
        if (v_bob > 2) {
            v_bob <- v_bob - 5
            b10 <- b10 + (p5 * 5) 
        }
        
        if (v_bob == -1) v_bob <- "-"
        if (v_bob == -2) v_bob <- "="
        b_bob <- paste0(v_bob, b_bob)
        p5 <- 5 * p5
    }
    return(b_bob)
}

solve_day25_part1 <- function(data_snafu) {
    sum_fuel <- data_snafu$V1 |> convert_bob_10() |> sum()
    return(sum_fuel |> convert_10_bob())
    
}

# Execution --------------------------------------------------------------------

solve_day25_part1(snafu_example)
solve_day25_part1(snafu)

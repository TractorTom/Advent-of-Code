# Titre : day21.R
# Auteur : Tanguy

##############
##  DAY 21  ##
##############

options(digits = 22)

# Import data ------------------------------------------------------------------

monkey <- readLines("./2022/Day21/monkey.txt")
monkey_example <- readLines("./2022/Day21/monkey_example.txt")


# Déclaration fonction ---------------------------------------------------------

traitement_p1 <- function(data_monkey) {
    data_monkey |> strsplit(split = ": ") |> do.call(what = rbind) |> 
        as.data.frame() |> 
        dplyr::rename(nom = 1, formule = 2) |> 
        dplyr::mutate(
            is.nb = dplyr::case_when(
                !is.na(as.numeric(formule)) ~ TRUE, 
                TRUE ~ FALSE), 
            res = dplyr::case_when(
                is.nb ~ as.numeric(formule), 
                TRUE ~ NA_real_), 
            not_done = TRUE)
}

op_fraction <- function(op, fr1, fr2) {
    num_1 <- fr1[1]
    denom_1 <- fr1[2]
    num_2 <- fr2[1]
    denom_2 <- fr2[2]
    
    if (op == "+") {
        num <- num_1 * denom_2 + num_2 * denom_1
        denom <- denom_1 * denom_2
    } else if (op == "-") {
        num <- num_1 * denom_2 - num_2 * denom_1
        denom <- denom_1 * denom_2
    } else if (op == "*") {
        num <- num_1 * num_2
        denom <- denom_1 * denom_2
    } else if (op == "/") {
        if (denom_1 * num_2 == 0) stop("On ne peut diviser par zero.")
        
        num <- num_1 * denom_2
        denom <- num_2 * denom_1
    }
    
    pgcd <- gmp::gcd(num, denom)
    num <- num %/% pgcd
    denom <- denom %/% pgcd
    return(c(num, denom))
}

op_complex <- function(op, val1, val2) {
    if (op == "+") {
        return(c(op_fraction("+", val1[1:2], val2[1:2]), 
                 op_fraction("+", val1[3:4], val2[3:4])))
    } else if (op == "-") {
        return(c(op_fraction("-", val1[1:2], val2[1:2]), 
                 op_fraction("-", val1[3:4], val2[3:4])))
    } else if (op == "*") {
        return(c(op_fraction("-", 
                             op_fraction("*", val1[1:2], val2[1:2]), 
                             op_fraction("*", val1[3:4], val2[3:4])), 
                 op_fraction("+", 
                             op_fraction("*", val1[1:2], val2[3:4]), 
                             op_fraction("*", val1[3:4], val2[1:2])))
        )
    } else if (op == "/") {
        if (val2[3] != 0) stop("On ne peut diviser par un nombre complexe.")
        return(c(op_fraction("/", val1[1:2], val2[1:2]), 
                 op_fraction("/", val1[3:4], val2[1:2]))
        )
    } 
    stop("L'opération est inconnue.")
}

traitement_p2 <- function(data_monkey) {
    data_monkey <- data_monkey |> 
        strsplit(split = ": ") |> 
        do.call(what = rbind) |> 
        as.data.frame() |> 
        dplyr::rename(nom = 1, formule = 2) |> 
        dplyr::mutate(
            formule = dplyr::case_when(
                nom == "root" ~ gsub(x = formule, pattern = "+", 
                                     replacement = "-", fixed = TRUE), 
                TRUE ~ formule),
            is.nb = !grepl(x = formule, pattern = c("\\+", "\\-", "\\*", "\\/") |> 
                               paste0(collapse = "|")), 
            to_do = TRUE, 
            is.nb_left = is.nb, 
            is.nb_right = is.nb, 
            left = formule |> strsplit(" ") |> sapply(FUN = base::`[`, 1), 
            right = formule |> strsplit(" ") |> sapply(FUN = base::`[`, 3), 
            op = formule |> strsplit(" ") |> sapply(FUN = base::`[`, 2), 
            value = dplyr::case_when(
                nom == "humn" ~ list(c(0, 1, 1, 1)), 
                TRUE ~ list(c(rep(NA_real_, 4)))
            )
        )
    
    for (monk in seq_len(nrow(data_monkey))) {
        if (data_monkey$is.nb[monk] && data_monkey$nom[monk] != "humn") {
            val <- as.numeric(data_monkey$formule[monk])
            data_monkey$value[monk] <- list(c(val, 1, 0, 1))
        }
    }
    
    return(data_monkey)
}

solve_day21_part1 <- function(data_monkey) {
    data_monkey <- traitement_p1(data_monkey)
    nb <- nrow(data_monkey)
    
    while (isFALSE(data_monkey$is.nb[data_monkey$nom == "root"])) {
        for (k in 1:nb) {
            if (data_monkey$is.nb[k] && data_monkey$not_done[k]) {
                
                nomk <- data_monkey$nom[k]
                val <- data_monkey$res[k]
                
                data_monkey <- data_monkey |> 
                    dplyr::mutate(
                        formule = gsub(x = formule, pattern = nomk, replacement = val)
                    )
                data_monkey$not_done[k] <- FALSE
            }
        }
        
        for (k in 1:nb) {
            if (data_monkey$not_done[k] && class(try(eval(parse(text = data_monkey$formule[k])), silent = TRUE)) != "try-error") {
                data_monkey$res[k] <- eval(parse(text = data_monkey$formule[k]))
                data_monkey$is.nb[k] <- TRUE
            }
        }
    }
    
    return(data_monkey[data_monkey$nom == "root", "res"])
}

solve_day21_part2 <- function(data_monkey) {
    
    data_monkey <- data_monkey |> traitement_p2()
    
    while (!data_monkey$is.nb[data_monkey$nom == "root"]) {
        monkey_to_do <- data_monkey$nom[data_monkey$to_do & data_monkey$is.nb]
        for (monk in monkey_to_do) {
            data_monkey$is.nb_left[data_monkey$left == monk] <- TRUE
            data_monkey$is.nb_right[data_monkey$right == monk] <- TRUE
        }
        data_monkey$to_do[data_monkey$to_do & data_monkey$is.nb] <- FALSE
        
        index_to_compute <- which(!data_monkey$is.nb)
        for (index_monk in index_to_compute) {
            if (all(data_monkey[index_monk, c("is.nb_left", "is.nb_right")])) {
                
                left_monkey <- data_monkey[index_monk, "left"]
                right_monkey <- data_monkey[index_monk, "right"]
                op <- data_monkey[index_monk, "op"]
                
                left_val <- data_monkey$value[data_monkey$nom == left_monkey] |> unlist()
                right_val <- data_monkey$value[data_monkey$nom == right_monkey] |> unlist()
                
                val <- op_complex(op, left_val, right_val)
                data_monkey$value[index_monk] <- list(val)
                data_monkey$is.nb[index_monk] <- TRUE
            }
        }
    }
    root <- data_monkey$value[data_monkey$nom == "root"] |> unlist()
    return(-(root[1] * root[4]) %/% (root[2] * root[3]))
}

# Execution --------------------------------------------------------------------

solve_day21_part1(monkey_example)
solve_day21_part1(monkey)

solve_day21_part2(monkey_example)
solve_day21_part2(monkey)

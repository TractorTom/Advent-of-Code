#!/usr/bin/env r

# Titre : day17.R
# Auteur : Tanguy

##############
##  DAY 17  ##
##############


# Import data ------------------------------------------------------------------

computer <- readLines(file.path("2024", "Day17", "computer.txt"))
computer_example <- readLines(file.path("2024", "Day17", "computer_example.txt"))
computer_example2 <- readLines(file.path("2024", "Day17", "computer_example2.txt"))


# Déclaration fonction ---------------------------------------------------------

read_computer_data <- function(input_computer) {
    data_computer <- input_computer[input_computer != ""] |>
        strsplit(split = ": ", fixed = TRUE) |>
        lapply(\(x) {
            strsplit(x = x[[2L]], split = ",", fixed = TRUE) |>
                unlist() |>
                as.integer()
        })
    registers <- data_computer[1L:3L]
    names(registers) <- c("A", "B", "C")
    return(list(registers = registers, instructions = data_computer[[4L]]))
}

get_combo_value <- function(operand, registers) {
    if (operand >= 0L && operand <= 3L) return(operand)
    if (operand <= 6L) return(registers[[operand - 3L]])
    stop("Wrong operand")
}

to_base2 <- function(n) {
    if (n == 0L) return(0L)
    if (n == 1L) return(1L)
    s <- NULL
    l <- floor(log(n) / log(2L)) + 1L
    for (j in 2L ** (rev(seq_len(l) - 1L))) {
        if (j <= n) {
            n <- n - j
            s <- c(s, 1L)
        } else {
            s <- c(s, 0L)
        }
    }
    return(s)
}

to_base10 <- function(s) {
    n <- 0L
    for (j in seq_along(s)) {
        n <- n + s[j] * 2L ** (length(s) - j)
    }
    return(n)
}

xor_gate <- function(a, b) {
    s_a <- to_base2(a)
    s_b <- to_base2(b)
    delta_l <- length(s_a) - length(s_b)
    if (delta_l > 0L) s_b <- c(rep(0L, delta_l), s_b)
    if (delta_l < 0L) s_a <- c(rep(0L, -delta_l), s_a)

    return(to_base10(xor(s_a, s_b)))
}

adv <- function(operand, registers) {
    division <- floor(registers[["A"]] / 2L ** get_combo_value(operand, registers))
    registers[["A"]] <- division
    return(registers)
}

bxl <- function(operand, registers) {
    XOR <- xor_gate(registers[["B"]], operand)
    registers[["B"]] <- XOR
    return(registers)
}

bst <- function(operand, registers) {
    registers[["B"]] <- get_combo_value(operand, registers) %% 8L
    return(registers)
}

jnz <- function(nb_instr, operand, registers) {
    if (registers[["B"]] == 0L) {
        return(nb_instr + 2L)
    } else {
        return(operand + 1L)
    }
}

bxc <- function(registers) {
    XOR <- xor_gate(registers[["B"]], registers[["C"]])
    registers[["B"]] <- XOR
    return(registers)
}

out <- function(operand, registers) {
    value <- get_combo_value(operand, registers) %% 8L
    return(value)
}

bdv <- function(operand, registers) {
    division <- floor(registers[["A"]] / 2L ** get_combo_value(operand, registers))
    registers[["B"]] <- division
    return(registers)
}

cdv <- function(operand, registers) {
    division <- floor(registers[["A"]] / 2L ** get_combo_value(operand, registers))
    registers[["C"]] <- division
    return(registers)
}

apply_instructions <- function(instructions, registers) {
    index <- 1L
    output <- NULL
    while (index <= length(instructions)) {
        opcode <- instructions[[index]]
        operand <- instructions[[index + 1L]]
        if (opcode == 3L) {
            index <- jnz(index, operand, registers) - 2L
        } else if (opcode == 5L) {
            output <- c(output, out(operand, registers))
        } else {
            registers <- switch(
                EXPR = as.character(opcode),
                "0" = adv(operand, registers),
                "1" = bxl(operand, registers),
                "2" = bst(operand, registers),
                "4" = bxc(registers),
                "6" = bdv(operand, registers),
                "7" = cdv(operand, registers)
            )
        }
        index <- index + 2L
    }
    return(output)
}

solve_day17_part1 <- function(input_computer) {
    data_computer <- read_computer_data(input_computer)
    output <- apply_instructions(instructions = data_computer[["instructions"]],
                                 registers = data_computer[["registers"]])
    return(paste(output, collapse = ","))
}

solve_day17_part2 <- function(input_computer) {

    data_computer <- read_computer_data(input_computer)
    instructions <- data_computer[["instructions"]]
    registers <- data_computer[["registers"]]

    registers_A <- 0L
    for (k in rev(seq_along(instructions) - 1L)) {
        new_registers_A <- NULL
        for (register_A in registers_A) {
            for (j in 0L:7L) {
                registers[["A"]] <- register_A + j * 8L ** k
                output <- apply_instructions(instructions, registers)
                len <- length(instructions) - k
                if (all(rev(output)[seq_len(len)] == rev(instructions)[seq_len(len)])) {
                    new_registers_A <- c(new_registers_A, register_A + j * 8L ** k)
                }
            }
        }
        registers_A <- new_registers_A
    }
    return(min(registers_A))
}


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day17_part1(computer_example)
solve_day17_part1(computer)


## Part 2 ----------------------------------------------------------------------

solve_day17_part2(computer)

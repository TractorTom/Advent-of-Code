#Titre : day10.R
#Auteur : Tanguy

##############
##  DAY 10  ##
##############

######IMPORT DATA######

subsystem <- readLines("./2021/Day10/navigation_subsystem.txt")
subsystem_example <- readLines("./2021/Day10/navigation_subsystem_example.txt")

######DECLARATION FONCTION######

score_first_error <- function(line){
    
    opened_chunks <- c()
    ligne <- strsplit(line, split = "")[[1]]
    
    #Cas des lignes avec de problèmes de syntaxe interne
    for (chunk in ligne){
        if (chunk %in% c("<", "(", "{", "[")) opened_chunks <- c(chunk, opened_chunks)
        else if (length(opened_chunks) == 0) return(chunk)
        else if (chunk == ">" & opened_chunks[1] != "<")  return(25137)
        else if (chunk == ")" & opened_chunks[1] != "(")  return(3)
        else if (chunk == "]" & opened_chunks[1] != "[")  return(57)
        else if (chunk == "}" & opened_chunks[1] != "{")  return(1197)
        else opened_chunks <- opened_chunks[-1]
    }
    
    #Cas des lignes avec de problèmes de terminaison
    return(0)
}

score_last_error <- function(line){
    
    opened_chunks <- c()
    ligne <- strsplit(line, split = "")[[1]]
    
    #Cas des lignes avec de problèmes de syntaxe interne
    for (chunk in ligne){
        if (chunk %in% c("<", "(", "{", "[")) opened_chunks <- c(chunk, opened_chunks)
        else if (length(opened_chunks) == 0) return(chunk)
        else if (chunk == ">" & opened_chunks[1] != "<")  return(0)
        else if (chunk == ")" & opened_chunks[1] != "(")  return(0)
        else if (chunk == "]" & opened_chunks[1] != "[")  return(0)
        else if (chunk == "}" & opened_chunks[1] != "{")  return(0)
        else opened_chunks <- opened_chunks[-1]
    }
    
    #Cas d'une ligne sans erreur
    if (length(opened_chunks) == 0) return(0)
    
    #Cas des lignes avec de problèmes de terminaison
    score <- 0
    for (chunk in opened_chunks){
        if (chunk == "(") score <- score * 5 + 1
        else if (chunk == "[") score <- score * 5 + 2
        else if (chunk == "{") score <- score * 5 + 3
        else if (chunk == "<") score <- score * 5 + 4
        else stop("ERREUR")
    }
    
    return(score)
}

solve_day10_part1 <- function(dataSubsystem){
    score <- 0
    for (lig in 1:length(dataSubsystem)){
        score <- score + score_first_error(dataSubsystem[lig])
    }
    return(score)
}

solve_day10_part2 <- function(dataSubsystem){
    score <- c()
    for (lig in 1:length(dataSubsystem)){
        score <- c(score, score_last_error(dataSubsystem[lig]))
    }
    score <- score[score != 0]
    return(median(score))
}

######EXECUTION######

solve_day10_part1(subsystem_example)
solve_day10_part1(subsystem)

solve_day10_part2(subsystem_example)
solve_day10_part2(subsystem)

#!/usr/bin/env python

# Titre : day01.R
# Auteur : Tanguy

##############
##  DAY 01  ##
##############


# Chargement bibliothèques -----------------------------------------------------

import pandas as pd


# Import data ------------------------------------------------------------------

location_example = pd.read_csv(
    "./2024/Day01/location_id_example.txt", sep=" ", header=None
)
location = pd.read_csv("./2024/Day01/location_id.txt", sep=" ", header=None)


# Déclaration fonction ---------------------------------------------------------


def solve_day01_part1(location_table):
    return sum(
        abs(
            location_table[0].sort_values().reset_index(drop=True)
            - location_table[3].sort_values().reset_index(drop=True)
        )
    )


def solve_day01_part2(location_table):
    occurences = location_table[3].value_counts()
    return sum([k * occurences[k] for k in location_table[0] if k in occurences.index])


# Execution --------------------------------------------------------------------

## Part 1 ----------------------------------------------------------------------

solve_day01_part1(location_example)
solve_day01_part1(location)


## Part 2 ----------------------------------------------------------------------

solve_day01_part2(location_example)
solve_day01_part2(location)

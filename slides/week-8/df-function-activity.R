
library(tidyverse)
library(palmerpenguins)

# Recreate the table() function in R
# Specify which group to get marginal proportions for

# Well, let's start by looking at what table() gives us...

## One categorical variable
table(penguins$species)

## Notice that the table is wide...
## So the default should be to pivot wider

## Two categorical variables
table(penguins$species, penguins$island)

## Notice the first variable is on the rows and the second variable is on the columns

# Let's give this a go

penguins |> 
  count(species, island)

## We need a pivot...

penguins |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n)

## How to fill the NAs with 0s?

penguins |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n, values_fill = 0)

# Okay, how do we generalize this?

df |> 
  count(var1, var2) |> 
  pivot_wider(names_from = var2, values_from = n, values_fill = 0)

## Maybe we should be more specific about what variables go where...

df |> 
  count(row_var, col_var) |> 
  pivot_wider(names_from = col_var, values_from = n, values_fill = 0)

# Let's put it into a function!

tidy_table <- function(df, row_var, col_var){
  df |> 
    count(row_var, col_var) |> 
    pivot_wider(names_from = col_var, values_from = n, values_fill = 0)
}

# Let's give it a try! 

tidy_table(penguins, species, island)

# Error in `count()`:
#   ! Must group by variables found in `.data`.
# Column `row_var` is not found.
# Column `col_var` is not found.

## What seems to be happening?!?!

# Lazy evaluation: R delays evaluating arguments — it keeps them as expressions.
# 
# {{ }}: Takes advantage of that delay to “splice” the user’s expression into tidyverse code at the right time and place.
# 
# So yes — you’re totally right!
#   You need {{ }} because lazy evaluation doesn’t automatically insert the variable name where you need it; it just stores it for later use.


# What do we need to do?

tidy_table <- function(df, row_var, col_var){
  df |> 
    count({{ row_var }}, {{ col_var }}) |> 
    pivot_wider(names_from = {{ col_var }}, values_from = n, values_fill = 0)
}

tidy_table(penguins, species, island)

# What if we wanted to use quoted variable names?

tidy_table(penguins, "species", "island")

# Error in `pivot_wider()`:
#   ! Can't select columns that don't exist.
# ✖ Column `island` doesn't exist.
# Run `rlang::last_trace()` to see where the error occurred.

# Soooo, count and pivot_wider expect unquoted variable names
# If we want to use quotes, we need to use some helper functions

quote_table <- function(df, row_var, col_var){
  df |> 
    count(
      pick(
        all_of(
          c(row_var, col_var)
          )
        )
      ) |> 
    pivot_wider(names_from = all_of(col_var), values_from = n, values_fill = 0)
}

# We should use pick() here instead of across() because this is akin to group_by + summarize()
# where pick() would apply the group_by() step to the entire dataset

quote_table(penguins, "species", "sex")


# Okay, now lets' recreate the prop.table() function

table(penguins$species, penguins$island) |> 
  prop.table()

table(penguins$species, penguins$island) |> 
  prop.table(margin = 1)

# Let's add some proportions to our table!

## These are proportions for the entire table
penguins |> 
  count(species, island) |> 
  mutate(prop = n / sum(n)) 

## I want proportions within each group

penguins |> 
  count(species, island) |> 
  group_by(species) |> 
  mutate(prop = n / sum(n)) 

## Notice that there is still a grouping variable?

penguins |> 
  count(species, island) |> 
  group_by(species) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

# Now what about pivoting? We have two variables with values...
## Only want the proportions...

penguins |> 
  count(species, island) |> 
  group_by(species) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  pivot_wider(id_cols = species, names_from = island, values_from = prop, values_fill = 0)

# Generalize this! 

df |> 
  count(row_var, col_var) |> 
  group_by(col_var) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  pivot_wider(id_cols = row_var, 
              names_from = col_var,
              values_from = prop, 
              values_fill = 0)

# Put it into a function

my_table <- function(df, row_var, col_var, margin = NULL){
  
  df |> 
    count({{ row_var }}, {{ col_var }}) |> 
    group_by({{ col_var }}) |> 
    mutate(prop = n / sum(n)) |> 
    ungroup() |> 
    select(-n) |> 
    pivot_wider(id_cols = {{ row_var }}, 
                names_from = {{ col_var }},
                values_from = prop, 
                values_fill = 0)
  
}

my_table(penguins, species, island)

## In prop.table() when no margin is specified the proportions are joint (not conditional)
## margin = 1 is conditional on the ROWS
## margin = 2 is conditional on the COLUMNS

my_table <- function(df, row_var, col_var, margin = NULL){
  
  # Default to joint proportions
  if(is.null(margin)){
    df |> 
    count({{ row_var }}, {{ col_var }}) |> 
    mutate(prop = n / sum(n)) |> 
    ungroup() |> 
    select(-n) |> 
    pivot_wider(id_cols = {{ row_var }}, 
                names_from = {{ col_var }},
                values_from = prop, 
                values_fill = 0)
  }
  else if(margin == "row"){
    df |> 
      count({{ row_var }}, {{ col_var }}) |> 
      group_by({{ row_var }}) |> 
      mutate(prop = n / sum(n)) |> 
      ungroup() |> 
      select(-n) |> 
      pivot_wider(id_cols = {{ row_var }}, 
                  names_from = {{ col_var }},
                  values_from = prop, 
                  values_fill = 0) |> 
      print()
  }
  else{
    df |> 
      count({{ row_var }}, {{ col_var }}) |> 
      group_by({{ col_var }}) |> 
      mutate(prop = n / sum(n)) |> 
      ungroup() |> 
      select(-n) |> 
      pivot_wider(id_cols = {{ row_var }}, 
                  names_from = {{ col_var }},
                  values_from = prop, 
                  values_fill = 0)
  }
  
}

my_table(penguins, species, island)
my_table(penguins, species, island, margin = "row")
my_table(penguins, species, island, margin = "col")

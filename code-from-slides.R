library(tidyverse)

# You need to save the function first before using it
# if you edit the function you need to re-save it and re-run following functions or else the original function will take place
# if you download multiple functions or load different packages you have to be careful with which functions are being used
# and if there are any conflicts with how those other packages use that function
# conflicted package that forces you to say you are using X function from X package 
# ℹ Use the conflicted package to force all conflicts to become errors!

say_hello <- function(input_name) {
  hello_output <- str_glue("Hello, {input_name}!")
  return(hello_output)
}
say_hello(input_name = "class")

hello_world <- say_hello("world!")
print(hello_world)


# Defining functions ----

hyp <- function(side_a, side_b) {
  a <- side_a^2
  b <- side_b^2
  h <- sqrt(a + b)
  return(h)
}
hyp(3, 4) # this is automatically adding 3 to side a and 4 to side b
hyp(9, 12)

hyp <- function(side_a, side_b = NULL) { # null means nothing -- does not exist
  a <- side_a^2
  b <- ifelse(is.null(side_b), a, side_b^2) # if side B is null then use side A again otherwise sqr side b
  h <- sqrt(a + b)
  return(h)
}
hyp(9, 9)
hyp(9)

# Checking arguments for robustness
hyp <- function(side_a, side_b) {
if (is.numeric(side_a) & is.numeric(side_b)) {
  a <- side_a^2
  b <- ifelse(is.null(side_b), a, side_b^2)
  h <- sqrt(a + b)
  return(h)
} else {
  return("Both sides must be numeric values")
}
}
hyp(9, 9)
hyp("9", "8")

# Using if, else, else if
x <- 1 #try w/ 0, -1
if (x == 0) {
  paste(x, "is exactly 0")
} else if (x < 0) {
  paste(x, "is negative")
} else {
  paste(x, "is positive")
} 

# "Wrapper" functions
mean_na <- function(x) mean(x, na.rm = T)
sd_na <- function(x) sd(x, na.rm = T)
se_na <- function(x) {
  x <- discard(x, is.na) #discard is like "filter if not" for vectors
  sd(x)/sqrt(length(x))
}

summary_stats_na <- function(x) {
  # Functions can call previously defined functions
  list(mean = mean_na(x), sd = sd_na(x), se = se_na(x))
}
data <- c(4, 6, 9, 12, 5, 6, NA)
summary_stats_na(data)

# Piped expressions can be saved as functions
avg_height_mass <- . %>% # if you start a piped expression with a period, that can be stored as a function
  select(height, mass) %>% 
  summarize(mean_ht = mean_na(height), mean_ms = mean_na(mass))
# selects height and mass from a data frame itll 

starwars %>% avg_height_mass
starwars %>% group_by(species) %>% avg_height_mass

# Across() -----

# Mutate
ds <- starwars %>% select(-where(is.list))
ds %>% mutate(across(everything(), as.character)) 
ds %>% mutate(across(c("height", "mass"), round)) 
ds %>% mutate(across(where(is.numeric), round, .names = "{.col}_rounded"), .after = name) # creating new columns of numeric columns where they are rounded
ds %>% mutate(across(where(is.numeric), ~ .x *100)) # multiplying every numeric column by 100
ds %>% mutate(across(ends_with("color"), ~ str_remove_all(.x, "[:punct:]"))) # str remove all can remove things like punctuation
  # the first argument of it is the thing youre removing from and then telling it what you want to remove
  # uses .x as the argument

# Summarize
fx_list <- list(mean = mean_na, sd = sd_na, se = se_na)
ds %>% summarize(across(height:mass, ~ mean(.x, na.rm = T))) # itll run it on height and mass and will put each one respectively in .x
ds %>% summarize(across(height:mass, mean_na)) 
  # the semicolon is saying like select these two columns; if you use a comma in between then it wouldnt work 
  # if there were other columns in between height and mass then it would also be selecting those in between but
  # in this case its only height and mass so not an issue

ds %>% summarize(across(where(is.numeric), fx_list))
ds %>% summarize(across(where(is.numeric), fx_list, .names = "{.fn}_{.col}"))

# Make a summary stats function using across

summary_stats <- function(df, var_select_string) {
  na_mean <- function(x) mean(x, na.rm = T)
  na_sd <- function(x) sd(x, na.rm = T)
  fx_list <- list(M = na_mean, SD = na_sd)
  df  %>% summarize(across(contains(var_select_string) & where(is.numeric), fx_list))
}
ds %>% summary_stats("height")
ds %>% summary_stats("mass")
ds %>% group_by(species) %>% filter(n() > 2) %>% summary_stats(c("height","mass"))

# For loops ----
# key word, something in parenthesis and something in curly brackets
for (i in c(1, 2, 3, 4, 5)) {
  # "i" will take on each value of the vector 1:5
  print(paste("This is loop iteration", i))
}

days_of_the_week <- c("Tues", "Wed", "Mon", "Thur", "Sun", "Fri" )
for (i in days_of_the_week) {
  print(paste("Today is", i))
}

# Use loops to run analyses
mtcars #built-in R dataset
predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
cor_output <- vector() #empty vector to store results
for (p in predictors) {
  #Calculate each predictor's correlation with 'mpg'
  cor_output[p] <- cor(mtcars['mpg'], mtcars[p])
} # p is each one in predictors so first p will be "cyl" then "hp" then "drat"
cor_output

# Linear regression repetitive code
lm(mpg ~ cyl, data = mtcars)
lm(mpg ~ disp, data = mtcars)
lm(mpg ~ hp, data = mtcars)

predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
for (p in predictors) {
  f <- as.formula(str_glue("mpg ~ {p}")) # Convert a string to a formula
  res <- lm(f, data = mtcars)
  print(p) #Need to explicitly print things within loops so that they display
  print(summary(res))
}

# for loops vs map
values_to_be_squared <- 1:5
for(value in values_to_be_squared) {
  print(value^2) # the "value" piece is something that can be changed to whatever
}
# will do the loop 5 times

map_int(values_to_be_squared, ~ .x^2)
# get the same output as you do in loops but with map you have more control about how you want it to output
# in for loop you need to decide where you want to store it
# for loop might be preferred if you need to loop over multiple things at once, this may be more flexible



# Map() ----
days_of_the_week <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
map(days_of_the_week, toupper) #Default output is a list
map_chr(days_of_the_week, toupper) #Use map_* to specify the output type
map_chr(days_of_the_week, ~ str_glue("Day: {.x}")) #Use in-line fns as in across()
map_lgl(days_of_the_week, ~ str_detect(.x, "M"))


# Use map to run a series of analyses
predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
cor_output <- map(predictors, ~ cor(mtcars['mpg'], mtcars[.x]))
cor_output

# Use set_names to make mapped outputs more readable
cor_output %>% set_names(predictors)

# Mapping linear regression formula
predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
output <- map(predictors, ~ lm(as.formula(str_glue("mpg ~ {.x}")), data = mtcars))
output %>% set_names(predictors)

# Custom functions make it easier to map
predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
mpg_lm <- function(p) {
  f <- as.formula(str_glue("mpg ~ {p}"))
  res <- lm(f, data = mtcars)
  res$call <- f
  return(res)
}
results <- map(predictors, mpg_lm) # returns list of model results
map(results, summary) # runs summary on each item in the list

# What if we wanted all combinations?
preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
all_combos <- expand.grid(preds, preds) %>% filter(Var1 != Var2)
all_combos

# Revise fx to take two inputs
mpg_lm2 <- function(criterion, predictor) {
  f <- as.formula(str_glue("{criterion} ~ {predictor}"))
  res <- lm(f, data = mtcars)
  res$call <- f
  return(res)
}
# Use map2 to provide two inputs
results <- map2(all_combos$Var1, all_combos$Var2, mpg_lm2)
map(results, summary)

library(broom) #for model output 'tidy' function
mpg_lm_tidy <- function(criterion, predictor) {
  f <- as.formula(str_glue("{criterion} ~ {predictor}"))
  res <- lm(f, data = mtcars)
  res <- tidy(res) %>% mutate(criterion = criterion, predictor = predictor, .before = 1)
  return(res)
}

map2_dfr(all_combos$Var1, all_combos$Var2, mpg_lm_tidy)

# Mapping on split datasets
ds <- starwars %>% mutate(species = factor(
  ifelse(species %in% c("Human", "Droid"), species, "Other")))

height_mass_lm <- function(df) {
  df %>% drop_na(height, mass) %>% 
    lm(mass ~ height, data = .) %>% 
    tidy %>% mutate(species = df$species[1], .before = 1)}

split_ds <- split(ds, ds$species) #base R fx
map_dfr(split_ds, height_mass_lm)

# Writing split datasets to file
ds <- starwars %>% mutate(species = factor(
  ifelse(species %in% c("Human", "Droid"), species, "Other")))

write_species <- function(df) {
  fname <- str_glue("data_species/{df$species[1]}.csv")
  df %>% write_csv(file = fname) }

split_ds <- split(ds, ds$species) 
walk(split_ds, write_species) #Walk is map w/o returning data


                                                
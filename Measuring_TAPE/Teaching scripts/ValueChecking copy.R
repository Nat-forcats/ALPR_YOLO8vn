##################
# VALUE CHECKING #
##################

# This uses data from the provider study, but you can pull R data to test.
# Goal was to remove state hot encoded columns that weren't needed.
# But you can use this loop to check cols for any value





# Import the updated version of the data with one license per row

full_df <- read.csv('/Users/natajahroberts/Desktop/TAPE_CLEAN_states_disaggregated.csv', 
                    stringsAsFactors = T)
glimpse(full_df)

# Selecting columns -- removing individual scale questions and some aggregates
tib <- tibble(full_df[, c(2:72, 130:139, 144:188)])
glimpse(tib)


########################
### Checking for zeros #    <--- code you can use to check a col for any value!
########################
# establishing a target
target <- 1

####### SKIP THIS IF YOU DIDNT RUN THE FIRST SET OF DATA ##
# testing on original columns that we removed 
# because they only had 0s
result <- any(df$VI == target)

# Print the result
print(result)
# output: false


### checking the original data 
# to make sure the function is working

# subsetting so we can loop through a whole df
df_subset <- df[,143:203] # pulled from above
glimpse(df_subset) # only contains states, great!

for (i in 1:ncol(df_subset)) {
  result <- any(df_subset[i] == target)
  print(result)
}

# to see side by side we need to save the results and merge with subset
check_values <- c()

for (i in 1:ncol(df_subset)) {
  result <- any(df_subset[i] == target)
  
  check_values <- append(check_values, result)
}

# testing
check_values

confirm <- tibble(check_values, colnames(df_subset))
view(confirm) # FALSE means there are no 1s in the column
# a quick scan will show that the loop is working
# if you compare with states listed above
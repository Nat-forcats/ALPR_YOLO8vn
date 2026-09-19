
### LOOP TO ADD DIFF SCORE ####

# This code was needed because we expanded provider study data
# so each row represented a license, not a provider
# For providers with multiple licenses, we needed their highest
# and lowest policy scores to calc a diff score
# but the values were in multiple rows.
# This takes chunks of rows, calcs the diff, and then rebuilds the table.



# NOTE: if you mess up, just reset results <- c()


# Function plan:
# select a subset of the table based on Subject
# use that chunk of rows within the loop
# get the highest and lowest scores for each column and create diff column
# save chunk to tibble outside of loop and iterate through next chunks

# confirming our i value
range(temp_tib2$Subject)
# 124 is the actual # of subj

# empty tibble for output
results <- tibble()


for (i in 1:124) {                           
  hold_subj <- temp_tib2[temp_tib2$Subject == i,]
  
  # create new scores for each group of rows and then bind rows
  # GIPT HEALTHCARE DIFF
  #find higest val in GIPT_healthcare, save val
  health_HI <- max(hold_subj$GIPT_healthcare)
  
  # find lowest val in GIPT_healthcare, save val
  health_LO <- min(hold_subj$GIPT_healthcare)
  
  # create GIPT_health_DIFF 
  hold_subj$GIPT_health_DIFF <- (health_HI - health_LO)
  
  
  # GIPT TOTAL DIFF
  #find higest val in GIPT_healthcare, save val
  total_HI <- max(hold_subj$GIPT_healthcare)
  
  # find lowest val in GIPT_healthcare, save val
  total_LO <- min(hold_subj$GIPT_healthcare)
  
  # create GIPT_health_DIFF 
  hold_subj$GIPT_total_DIFF <- (total_HI - total_LO)
  
  
  
  # HEALTHCARE DIFF
  #find higest val in GIPT_healthcare, save val
  health_HI <- max(hold_subj$GIPT_healthcare)
  
  # find lowest val in GIPT_healthcare, save val
  health_LO <- min(hold_subj$GIPT_healthcare)
  
  # create GIPT_health_DIFF 
  hold_subj$GIPT_health_DIFF <- (health_HI - health_LO)
  
  # binding this group with its new columns to the empty results tib
  results <- bind_rows(results, hold_subj)
  
  # as it iterates, it will add new groups to the first one, so cols are same
  
}

view(results)

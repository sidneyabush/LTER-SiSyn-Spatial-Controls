##rbfi 

#df is a dataframe of a discharge file for one site
#this was in a loop where each RBFI value integrates flashiness across discharge for the entire period of record

# Calculate daily changes in discharge
df <- df %>%
  arrange(Date) %>%
  mutate(dQ = Q - lag(Q),  # Daily change in discharge
         abs_dQ = abs(dQ)) %>%              # Absolute change in discharge
  filter(!is.na(abs_dQ))  # Remove NA values (first row)

# Calculate the total discharge over the period
total_discharge <- sum(df$Q)

# Calculate the Richards-Baker Flashiness Index
RBFI <- sum(df$abs_dQ) / total_discharge
library(readr)
library(dplyr)
data_cpep <- read_csv("data_cpep_mice.csv")

# rename variables for requirement of the app
data_cpep <- data_cpep %>% rename(
"ID" = "subject",
"Visit" = "visit",
"Time" = "time",
"Measurement" = "result"
)
# remove first two columns
data_cpep <- data_cpep[,-c(1,2)]
# write the data to a csv file
write.csv(data_cpep, "data_cpep.csv", row.names = FALSE)

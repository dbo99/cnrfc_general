### Function Definitions ###

#####################################
### year and month classifications ##
#####################################

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}


### HEFS output's second row is just all "SQIN", so read in, drop 2nd row, and add filename column
read_plus <- function(flnm) {
  read_csv(flnm)[-1,] %>%
    mutate(filename = flnm)}

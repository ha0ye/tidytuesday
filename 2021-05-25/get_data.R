library(tidytuesdayR)

# download the data
# dat <- tt_load("2021-05-25")
# saveRDS(dat$drivers, file = "2021-05-25/drivers.RDS")
# saveRDS(dat$records, file = "2021-05-25/records.RDS")

drivers <- readRDS("2021-05-25/drivers.RDS")
records <- readRDS("2021-05-25/records.RDS")

# What is drivers$position ("Player’s current leader board position") - 
#   based on total number of records?

plot(position ~ total, data = drivers)
## yep, looks monotonic: higher # of records --> lower numerical position (higher ranking)

# check that all of the player names in `drivers` match player names in `records`
setequal(unique(drivers$player), 
         unique(records$player))
# players in the `drivers` table but not in the `records` table
setdiff(unique(drivers$player), 
        unique(records$player))
# players in the `records` table but not in the `drivers` table
setdiff(unique(records$player), 
        unique(drivers$player))

print(drivers[drivers$player == "Christian C",], n = Inf)

missing_players <- setdiff(unique(drivers$player), 
                           unique(records$player))
unique(drivers[drivers$player %in% missing_players, "position"])
# positions for the drivers without entries in the `records` table
# because these positions are not 1, they are not world records, and
# are therefore not recorded in `records`

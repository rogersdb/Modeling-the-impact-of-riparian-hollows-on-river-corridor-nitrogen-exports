require(lubridate)
require(baytrends)
require(dplyr)
require(ggplot2)

########################################### READ DATA FILES ##########################################
# H1D_out1 extracts all of the data from the first HYDRUS simulation that are necessary for
# the second HYDRUS simulation
H1D_out1 <- read.table("C:/Tutorial/PondingDepth_M1B1/T_Level.out", skip = 9, header = FALSE,
                       nrows = length(readLines("C:/Tutorial/PondingDepth_M1B1/T_Level.out")) - 10)
T_Level.out_header <- c("Time", "rTop", "rRoot", "vTop", "vRoot", "vBot", "sum(rTop)", "sum(rRoot)",
                        "sum(vTop)", "sum(vRoot)", "sum(vBot)", "hTop", "hRoot", "hBot", "RunOff",
                        "sum(RunOff)", "Volume", "sum(Infil)", "sum(Evap)", "TLevel", "Cum(WTrans)", "SnowLayer")
colnames(H1D_out1) <- T_Level.out_header

# wl provides the absolute elevation of the water table
# wl needs to have the following header: "DateTime" "WL.elev.m"
# "DateTime" should use the order [month-day-year-hour-minute] using any common formatting
wl <- read.csv("C:/Tutorial/wl_M1B1.csv", header = TRUE)

# stage provides the river stage
# stage needs to have the following header: "DateTime" "Stage.m"
# "DateTime" should use the order [month-day-year-hour-minute] using any common formatting
stage <- read.csv("C:/Tutorial/stage_PH.csv", header = TRUE)


################################### DEFINE VARIABLES AND FOLDER PATHS #################################
# year: year being simulated
# start: first minute of simulation (minute is minute of year) (for reference, there are 1440 minutes in a day)
# length: length of simulation in minutes
# steps: number of time steps (should be simulation length / time step length, but this may need to be adjusted)
# elev: absolute elevation of ground surface or datum of interest in meters
# out: file path of desired output location
# dist: river distance from point in river where stage is measured to point of interest
# grad_river: river gradient

# This example is for M1B1 during March 2016
year <- 2016
start <- 87840
length <- 44640
steps <- 2972
elev <- 2759.96
dist <- 64
grad_river <- 0.0024
ice_free <- "2016-04-11"
out <- "C:/Tutorial/InfilRate_M1B1/ATMOSPH.IN"

#################### DETERMINE PERIODS OF PONDING DUE TO BANKFULL OVERFLOW OR RAINFALL ####################
# select subset of wl data for year of interest
wl <- subset(wl, year(mdy_hm(wl$DateTime)) == year)

# calculate and create a column for minute of year based on DateTime column
wl$MOY <- yday(mdy_hm(wl$DateTime)) * 1440 +
  hour(mdy_hm(wl$DateTime)) * 60 +
  minute(mdy_hm(wl$DateTime)
  )

# select subset of wl data for desired simulation period
wl <- subset(wl, MOY >= start & MOY <= start + length)

# make sure "MOY" is in ascending order
wl <- wl[order(wl$MOY),]

# calculate river stage at point of interest
stage$stage_poi <- stage$Stage.m - dist * grad_river

# calculate river stage elevation relative to ground surface elevation at point of interest
stage$stage_rel <- stage$stage_poi - elev

# round river stage data to nearest 15 minutes so that it is comparable to groundwater level data
stage$DateTime <- round_date(mdy_hm(stage$DateTime), "15mins")

# remove any duplicate DateTime entries due to rounding
stage <- stage[!duplicated(stage$DateTime),]

# make sure wl DateTime column is same format as stage DateTime column
# this seems inefficient, but it is necessary for merging 
wl$DateTime <- mdy_hm(wl$DateTime)

# merge stage data and groundwater level data
dat <- merge(wl, stage, by = "DateTime", all.x = TRUE)

# replace negative head values with 0
H1D_out1$hTop <- replace(H1D_out1$hTop, which(H1D_out1$hTop < 0), 0)

# calculate minute of year
H1D_out1$MOY <- H1D_out1$Time + start

# round minute of year to nearest multiple of 15 for comparison to wl and stage
H1D_out1$MOY <- round(H1D_out1$MOY / 15) * 15

# remove any duplicate MOY entries due to rounding
H1D_out1 <- H1D_out1[!duplicated(H1D_out1$MOY),]

# extract necessary variables (MOY and surface head) from H1D_out1 and merge into dat
tmp <- H1D_out1[,c("MOY", "hTop")]
dat <- merge(dat, tmp, by = "MOY", all.x = TRUE)

# hTop contains positive head values when there is ponding due to rain, as calculated by HYDRUS
# if there is no ponding by rain, hTop is 0
# the final step (below) is to add bankfull overflow ponding
dat$hTop <- with(dat, ifelse(stage_rel > 0 & DateTime > ymd(ice_free), stage_rel, hTop))

######################### GENERATE ATMOSPH.IN FILE FOR INPUT INTO SECOND HYDRUS SIMULATION ##############################
#initialize data frame
atm.in <- data.frame(matrix(nrow = nrow(dat), ncol = 0))

# populate data frame with time, pressure head at surface (calculated by first HYDRUS simulation),
# and groundwater elevation
atm.in$Time.min <- dat$MOY - start
atm.in$hT <- dat$hTop
atm.in$hB <- dat$WL.elev.m - elev + 5

# define new columns required for HYDRUS input
# any column that is 0 will not be used to force the model, it will be calculated by the model
atm.in$Precip.m.min <- 0  # rainfall rate
atm.in$ET.m.min <- 0      # potential ET rate
atm.in$rRoot <- 0         # recharge by roots
atm.in$hCritA <- -1000    # minimum pressure head at surface
atm.in$rB <- 0            # recharge at bottom of domain
atm.in$RootDepth <- 0     # root depth

# if there is ponding (hT > 0) and groundwater upwelling (hB > 5) set head at bottom of domain
# to be equal to head at top of domain for purposes on convergence
atm.in$hB <- with(atm.in, ifelse(hT > 0 & hB > 5, atm.in$hT + 5, atm.in$hB))

# remove the first time step, since HYDRUS will use an initial condition for time 0 rather than
# a boundary condition for time 0
atm.in <- atm.in[-1,]

# ensure the columns are in the order required by HYDRUS
atm.in <- atm.in[,c("Time.min", "Precip.m.min", "ET.m.min", "rRoot", "hCritA", "rB", "hB", "hT", "RootDepth")]

# define standard header and footer for HYDRUS ATMOSPH.IN file
header <- paste("Pcp_File_Version=4
*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************
  MaxAL                    (MaxAL = number of atmospheric hydrusa-records)", steps,
                "DailyVar  SinusVar  lLay  lBCCycles lInterc lDummy  lDummy  lDummy  lDummy  lDummy
f       f       f       f       f       f       f       f       f       f
hCritS                 (max. allowed pressure head at the soil surface)
100
tAtm        Prec       rSoil       rRoot      hCritA          rB          hB          hT    RootDepth",
                sep = "\n")
footer <- "end*** END OF INPUT FILE 'ATMOSPH.IN' ******************************"

writeLines(header, con = out)
write.table(atm.in, file = out, append = TRUE, col.names = FALSE, row.names = FALSE, sep = "\t")
write(footer, file = out, append = TRUE)


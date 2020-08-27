require(lubridate)
require(baytrends)
require(dplyr)

########################################### READ DATA FILES ##########################################
# atm provides precipitation and ET rates in m/min
# atm needs to have the following header: "Year" "MOY" "Precip.m.min" "ET.m.min"
# "MOY" is minute of year and should be specified in the time steps that will be used in HYDRUS
atm <- read.csv("C:/Tutorial/atm_M1B1.csv", header = TRUE)

# wl provides the absolute elevation of the water table
# wl needs to have the following header: "DateTime" "WL.elev.m"
# "DateTime" should use the order [month-day-year-hour-minute] using any common formatting
wl <- read.csv("C:/Tutorial/wl_M1B1.csv", header = TRUE)

#################################### DEFINE VARIABLES AND FILE PATHS ##################################
# year: year being simulated
# start: first minute of simulation (minute is minute of year)
# length: length of simulation in minutes
# steps: number of time steps (should be simulation length / time step length, but this may need to be adjusted)
# elev: absolute elevation of ground surface or datum of interest in meters
# precip.scale: scaling factor to multiply precipitation rate by 
# out: file path of first HYDRUS simulation folder

# This example is for M1B1 during March 2016
year <- 2016
start <- 87840
length <- 44640
steps <- 2972
elev <- 2759.96
precip.scale <- 2
out <- "C:/Tutorial/PondingDepth_M1B1/ATMOSPH.IN"

######################### GENERATE ATMOSPH.IN FILE FOR INPUT INTO FIRST HYDRUS SIMULATION ##############################
# select subset of atm data for year of interest and for desired simulation period
tmp <- subset(atm, Year == year & MOY >= start & MOY <= start + length)

# define new column "Time.min" which provides the minute of the simulation starting from 0 
tmp$Time.min <- tmp$MOY - start

# extract only variables needed for HYDRUS input
dat <- tmp[,c("Time.min", "Precip.m.min", "ET.m.min")]

# make sure "Time.min" is in ascending order starting from 0
dat <- dat[order(dat$Time.min),]

# multiply precipitation by a scaling factor if desired
dat$Precip.m.min <- dat$Precip.m.min * precip.scale

# define new columns required for HYDRUS input
# any column that is 0 will not be used to force the model, it will be calculated by the model
dat$hT <- 0           # head at top (surface) of domain
dat$rRoot <- 0        # recharge by roots
dat$hCritA <- -1000   # minimum pressure head at surface
dat$rB <- 0           # recharge at bottom of domain
dat$hB <- 0           # head at bottom of domain
dat$RootDepth <- 0    # root depth

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

# assign the water level measurements to the head boundary at the bottom of the domain
# if wl and dat do not have the same number of rows, you will need to manually correct this
dat$hB <- wl$WL.elev.m - elev + 5

# remove the first time step, since HYDRUS will use an initial condition for time 0 rather than
# a boundary condition for time 0
dat <- dat[-1,]

# ensure the columns are in the order required by HYDRUS
dat <- dat[,c("Time.min", "Precip.m.min", "ET.m.min", "rRoot", "hCritA", "rB", "hB", "hT", "RootDepth")]

# define standard header and footer for HYDRUS ATMOSPH.IN file
header <- paste("Pcp_File_Version=4
*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************
  MaxAL                    (MaxAL = number of atmospheric data-records)", steps,
"DailyVar  SinusVar  lLay  lBCCycles lInterc lDummy  lDummy  lDummy  lDummy  lDummy
f       f       f       f       f       f       f       f       f       f
hCritS                 (max. allowed pressure head at the soil surface)
100
tAtm        Prec       rSoil       rRoot      hCritA          rB          hB          hT    RootDepth",
sep = "\n")
footer <- "end*** END OF INPUT FILE 'ATMOSPH.IN' ******************************"

writeLines(header, con = out)
write.table(dat, file = out, append = TRUE, col.names = FALSE, row.names = FALSE, sep = "\t")
write(footer, file = out, append = TRUE)


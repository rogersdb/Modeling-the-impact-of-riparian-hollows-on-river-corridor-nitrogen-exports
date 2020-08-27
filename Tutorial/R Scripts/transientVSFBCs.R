require(lubridate)
require(baytrends)
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
H1D_out2 <- read.table("C:/Tutorial/InfilRate_M1B1/T_Level.out", skip = 9, header = FALSE,
                       nrows = length(readLines("C:/Tutorial/InfilRate_M1B1/T_Level.out")) - 10)
colnames(H1D_out2) <- T_Level.out_header

# wl provides the absolute elevation of the water table
# wl needs to have the following header: "DateTime" "WL.elev.m"
# "DateTime" should use the order [month-day-year-hour-minute] using any common formatting
wl <- read.csv("C:/Tutorial/wl_M1B1.csv", header = TRUE)

# stage provides the river stage
# stage needs to have the following header: "DateTime" "Stage.m"
# "DateTime" should use the order [month-day-year-hour-minute] using any common formatting
stage <- read.csv("C:/Tutorial/stage_PH.csv", header = TRUE)

# gradient provides the lateral groundwater gradient
# gradient needs to have the following header: "DateTime" "gradient"
gradient <- read.csv("C:/Tutorial/gradient.csv", header = TRUE)

# load snowmelt data
load("C:/Tutorial/DailyETQ.RData")

################################### DEFINE VARIABLES AND FOLDER PATHS #################################

# year: year being simulated
# start: first minute of simulation (minute is minute of year) (for reference, there are 1440 minutes in a day)
# length: length of simulation in minutes
# steps: number of time steps (should be simulation length / time step length, but this may need to be adjusted)
# elev: absolute elevation of ground surface or datum of interest in meters
# dist: river distance from point in river where stage is measured to point of interest
# grad_river: river gradient
# model_width: width (lateral) of model domain
# ice_free: date river becomes unconfined from ice
# out: file path of desired output location

# This example is for M1B1 during March 2016
year <- 2016
start <- 87840
length <- 44640
steps <- 2972
elev <- 2759.96
dist <- 64
grad_river <- 0.0024
model_width <- 2
ice_free <- "2016-04-11"
out.indicator <- "C:/Tutorial/RTBC_ind.csv"
out.bcvs <- "C:/Tutorial/MIN3P/ERRH2D.bcvs"

####################### CALCULATE HEAD BCS FOR LEFT AND RIGHT SIDES OF DOMAIN ########################
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

wl$bc1 <- wl$WL.elev.m - elev + 5

wl <- merge(wl, gradient, by = "DateTime", all.x = TRUE)

wl$gradient <- fillMissing(wl$gradient, span = 5, max.fill = 2000)

wl$bc2 <- wl$bc1 + model_width * wl$gradient

####################### CALCULATE PERIODS OF BANKFULL OVERFLOW ######################
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

######################### GET PONDING DEPTH AND INFILTRATION DUE SOLELY TO RAINFALL ############################
# calculate minute of year
H1D_out1$MOY <- H1D_out1$Time + start

# round minute of year to nearest multiple of 15 for comparison to wl and stage
H1D_out1$MOY <- round(H1D_out1$MOY / 15) * 15

# remove any duplicate MOY entries due to rounding
H1D_out1 <- H1D_out1[!duplicated(H1D_out1$MOY),]

# replace negative head values with 0
H1D_out1$hTop <- replace(H1D_out1$hTop, which(H1D_out1$hTop < 0), 0)

# extract necessary variables (MOY and surface head) from H1D_out1 and merge into dat
tmp <- H1D_out1[,c("MOY", "vTop", "hTop")]
colnames(tmp) <- c("MOY", "vTop1", "hTop1")
dat <- merge(dat, tmp, by = "MOY", all.x = TRUE)

##################### GET INFILTRATION RATE FROM SURFACE FLUX DUE TO RAIN OR BANKFULL OVERFLOW #####################
# calculate minute of year
H1D_out2$MOY <- H1D_out2$Time + start

# round minute of year to nearest multiple of 15 for comparison to wl and stage
H1D_out2$MOY <- round(H1D_out2$MOY / 15) * 15

# remove any duplicate MOY entries due to rounding
H1D_out2 <- H1D_out2[!duplicated(H1D_out2$MOY),]

# extract necessary variables (MOY and surface head) from H1D_out2 and merge into dat
tmp <- H1D_out2[,c("MOY", "vTop")]
colnames(tmp) <- c("MOY", "VTop2")
dat <- merge(dat, tmp, by = "MOY", all.x = TRUE)

# hTop1 contains positive head values when there is ponding due to rain, as calculated by HYDRUS
# if there is no ponding by rain, hTop1 is 0
# dedault TYPE of surface flux to 0 for no surface flux
dat$type <- 0
# set TYPE to 2 if there is surface flux due to rainfall
dat$type <- replace(dat$type, dat$hTop1 > 0 , 2)

# the final step (below) is to add bankfull overflow ponding to hTop1
dat$hTop1 <- with(dat, ifelse(stage_rel > 0 & DateTime > ymd(ice_free), stage_rel, hTop1))
# also change TYPE to 1 for ponding due to bankfull overflow
dat$type <- replace(dat$type, dat$stage_rel > 0 & dat$DateTime > ymd(ice_free), 1)

# if there is ponding due to rain or bankfull overflow, apply infiltration rate calcualted in second HYDRUS model
dat$bc3 <- with(dat, ifelse(type > 0, vTop2 / -60, 0))


# compare current boundary conditions and mergeETQ to determine when there is no ponding due
# to rainfall or bankull overflow BUT there is snowmelt infiltration
# convert daily total (mm) to appropriate units (m/s) and insert into boundary conditions
# also set TYPE to 3 if there is infiltration due to snowmelt

mergeETQ$DailyMelt.mm <- mergeETQ$SnowRainMelt - mergeETQ$Rain
mergeETQ <- mergeETQ[!is.na(mergeETQ$DailyMelt.mm),]

for (m in 1:nrow(mergeETQ)) {
  if (mergeETQ$DailyMelt.mm[m] > 0) {
    dat$bc3 <- replace(dat$bc3,
                        which(year(as.POSIXct(dat$DateTime, format = "%Y-%m-%d %H:%M:%S")) == mergeETQ[m, "Year"] &
                                     yday(as.POSIXct(dat$DateTime, format = "%Y-%m-%d %H:%M:%S")) == mergeETQ[m, "DOY"] &
                                           dat$bc3 == 0),
                       mergeETQ$DailyMelt.mm[m] / 1000 / 86400) # convert from mm to m/s
    dat$type <- replace(dat$type,
                       which(year(as.POSIXct(dat$DateTime, format = "%Y-%m-%d %H:%M:%S")) == mergeETQ[m, "Year"] &
                               yday(as.POSIXct(dat$DateTime, format = "%Y-%m-%d %H:%M:%S")) == mergeETQ[m, "DOY"] &
                               dat$bc3 == mergeETQ$DailyMelt.mm[m] / 1000 / 86400),
                       3) # surface flux ID for snowmelt
  }
  else {
    next
  }
}

# compare first HYDRUS model and current boundary conditions to determine when there is no ponding due
# to rainfall or bankfull overflow or infiltration due to snowmelt, but there IS infiltration due to rainfall
# also change TYPE to 2 if there is infiltration due to rainfall
dat$bc3 <- with(dat, ifelse(vTop1 < 0 & bc3 <= 0, vTop1 / -60, bc3))
dat$type <- replace(dat$type, dat$vTop1 < 0 & dat$bc3 == dat$vTop1 / -60, 2)

########################################### WRITE FILES ###########################################
# calculate model day starting from 0; these are the time units used by MIN3P
dat$ModelDay <- (dat$MOY - dat$MOY[1]) / 1440

# write INDICATOR file, which indicates which geochemical boundary condition should be applied for each time step
# the INDICATOR variable for this simuation is TYPE
write.csv(dat[,c("ModelDay", "type")], file = out.indicator, row.names = FALSE)

# extract varibles necessary for MIN3P .bcvs file
dat <- dat[, c("ModelDay", "bc1", "bc2", "bc3")]

# reduce to 30-minute time step 
dat <- dat[-seq(0, nrow(dat), 2),]

# write .bcvs file
write.table(dat, file = out.bcvs, col.names = FALSE, row.names = FALSE)

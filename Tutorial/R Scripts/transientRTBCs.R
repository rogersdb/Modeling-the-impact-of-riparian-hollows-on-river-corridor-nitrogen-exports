
########################################### READ DATA FILES ##########################################
# ind is a file that provides the type of geochemical composition at each time step
# type needs to have the following header: "ModelDay" "type"
ind <- read.csv("C:/Tutorial/RTBC_ind.csv", header = TRUE)

################################### DEFINE VARIABLES AND FOLDER PATHS #################################
# out: file path of desired output location
out <- "C:/Tutorial/MIN3P/rtbc.txt"

################################### DETERMINE TRANSITION PERIODS BETWEEN BCS #################################
# when there is no surface flux, assign current geochemical type to geochemical type of most
# recent surface flux event; before running these lines, the current geochemical type should be 0
if (ind$type[1] == 0) {
  ind$type[1] <- 3
}

for(m in 1:nrow(ind)) {
  if (ind[m, "type"] == 0) {
    ind[m, "type"] <- ind[m - 1, "type"]
  }
}

# determine each time the boundary condition changes
for (m in 1:nrow(ind)) {
  if (m == 1) {
    df <- data.frame(list(as.POSIXct(ind[m, "ModelDay"], format = "%Y-%m-%d %H:%M:%S"),
                          ind[m, "type"]))
    colnames(df) <- c("ModelDay", "Transition to:")
  }
  else if (!(ind[m, "type"] == ind[m - 1, "type"])) {
    df[nrow(df) + 1, ] <- list(as.POSIXct(ind[m, "ModelDay"], format = "%Y-%m-%d %H:%M:%S"),
                               ind[m, "type"])
  }
  else if (m == nrow(ind)) {
    return(df)
  }
  else {
    next
  }
}

for (m in 1:nrow(ind)) {
  if (m == 1) {
    df <- ind[m,]
    colnames(df) <- c("ModelDay", "Transition to:")
  }
  else if (!(ind[m, "type"] == ind[m - 1, "type"])) {
    df[nrow(df) + 1, ] <- ind[m,]
  }
  else if (m == nrow(ind)) {
    return(df)
  }
  else {
    next
  }
}

################################# WRITE REACTIVE TRANSPORT BCS BLOCKS ################################
# define RTBC initial condition block header
# only necessary change may be number of zones
block15.header <- "
! --------------------------------------------------------------------------
! INITIAL CONDITION - REACTIVE TRANSPORT (DATA BLOCK 15)
! --------------------------------------------------------------------------
'initial condition - reactive transport'                                    
1                             ;number of zones
"                              

# define generic block closer
block.closer <- "
'done'
"

# define geochemical composition of initial condition
initial.condition <- "
! --------------------------------------------------------------------------
'number and name of zone'
1
'inflow'

'boundary type'
'third'

'concentration input'
4.00d-01   ;'ch2o'
5.00d-02    ;'no3-1'
5.40d+01    ;'co3-2'
2.80d-03    ;'n2(aq)'
7.65        ;'h+1'
4.00d-01    ;'o2(aq)'
2.00d-04    ;'nh4+1'
7.90d+01    ;'ca+2'
5.00d-02    ;'fe+2'
5.00d-03    ;'fe+3'
1.00d-02    ;'hs-1'
1.00d+01   ;'so4-2'

'mineral input'
5.0d-02	.true.	'constant'   	;phi, minequil, type  	{SOM_ch2o}
1.0d-10	1.0d-14	0.0   			;phi_min, k_eff, sat	{SOM_ch2o}
7.0d-03	.true.	'constant'   	;phi, minequil, type  	{pyrite}
1.0d-10	1.0d-12	0.0  			;phi_min, k_eff, sat	{pyrite}

'extent of zone'
0.0 2.0 0.0 1.0 0.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# define RTBC boundary conditions condition block header
# only necessary change may be number of zones
block16.header <- "
! --------------------------------------------------------------------------
! BOUNDARY CONDITIONS - REACTIVE TRANSPORT (DATA BLOCK 16)
! --------------------------------------------------------------------------
'boundary conditions - reactive transport'
3						;number of zones
"

# define geochemical composition
bankfull.overflow <- "
! --------------------------------------------------------------------------
'number and name of zone'
3
'hollow surface'

'boundary type'
'third'

'concentration input'
1.00d-01   ;'ch2o'
2.00d-01    ;'no3-1'
1.70d+01    ;'co3-2'
5.00d-05    ;'n2(aq)'	
8.0         ;'h+1'	
1.00d+01    ;'o2(aq)'	
2.30d-04    ;'nh4+1'	
3.00d+00    ;'ca+2'	
2.00d-02    ;'fe+2'	
2.00d-03    ;'fe+3'	
1.00d-04    ;'hs-1'	
2.00d+01    ;'so4-2'

'mineral input'
5.0d-02	.true.	'constant'   	;phi, minequil, type  	{SOM_ch2o}
1.0d-10	1.0d-14	0.0   			;phi_min, k_eff, sat	{SOM_ch2o}
7.0d-03	.true.	'constant'   	;phi, minequil, type  	{pyrite}
1.0d-10	1.0d-12	0.0  			;phi_min, k_eff, sat	{pyrite}

'extent of zone'
0.0 2.0 0.0 1.0 5.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# define geochemical composition
rainfall <- "
! --------------------------------------------------------------------------
'number and name of zone'
3
'hollow surface'

'boundary type'
'third'

'concentration input'
3.00d-01   ;'ch2o'
6.50d-01    ;'no3-1'
3.00d+01    ;'co3-2'
1.35d-01    ;'n2(aq)'
5.3         ;'h+1'
8.64d+00    ;'o2(aq)'
1.80d-03    ;'nh4+1'
1.60d-01    ;'ca+2'
5.00d-03    ;'fe+2'
5.00d-04    ;'fe+3'
1.00d-04    ;'hs-1'
3.00d-01   ;'so4-2'

'mineral input'
1.0d-10	.true.	'constant'   	;phi, minequil, type  	{SOM_ch2o}
1.0d-10	1.0d-14	0.0   			;phi_min, k_eff, sat	{SOM_ch2o}
1.0d-10	.true.	'constant'   	;phi, minequil, type  	{pyrite}
1.0d-10	1.0d-12	0.0  			;phi_min, k_eff, sat	{pyrite}

'extent of zone'
0.0 2.0 0.0 1.0 5.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# define geochemical composition
snowmelt <- "
! --------------------------------------------------------------------------
'number and name of zone'
3
'hollow surface'

'boundary type'
'third'

'concentration input'
3.00d-01   ;'ch2o'
4.39d-01    ;'no3-1'
3.00d+01    ;'co3-2'
1.35d-01    ;'n2(aq)'
5.3         ;'h+1'
8.64d+00    ;'o2(aq)'
5.90d-04    ;'nh4+1'
6.70d-02    ;'ca+2'
5.00d-03    ;'fe+2'
5.00d-04    ;'fe+3'
1.00d-04    ;'hs-1'
1.54d-01   ;'so4-2'

'mineral input'
1.0d-10	.true.	'constant'   	;phi, minequil, type  	{SOM_ch2o}
1.0d-10	1.0d-14	0.0   			;phi_min, k_eff, sat	{SOM_ch2o}
1.0d-10	.true.	'constant'   	;phi, minequil, type  	{pyrite}
1.0d-10	1.0d-12	0.0  			;phi_min, k_eff, sat	{pyrite}

'extent of zone'
0.0 2.0 0.0 1.0 5.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# define geochemical composition
groundwater <- "
! --------------------------------------------------------------------------
'number and name of zone'
1
'inflow'

'boundary type'
'third'

'concentration input'
4.00d-01   ;'ch2o'
5.00d-02    ;'no3-1'
5.40d+01    ;'co3-2'
2.80d-03    ;'n2(aq)'
7.65        ;'h+1'
4.00d-01    ;'o2(aq)'
2.00d-04    ;'nh4+1'
7.90d+01    ;'ca+2'
5.00d-02    ;'fe+2'
5.00d-03    ;'fe+3'
1.00d-02    ;'hs-1'
1.00d+01   ;'so4-2'

'mineral input'
5.0d-02	.true.	'constant'   	;phi, minequil, type  	{SOM_ch2o}
1.0d-10	1.0d-14	0.0   			;phi_min, k_eff, sat	{SOM_ch2o}
7.0d-03	.true.	'constant'   	;phi, minequil, type  	{pyrite}
1.0d-10	1.0d-12	0.0  			;phi_min, k_eff, sat	{pyrite}

'extent of zone'
0.0 0.0 0.0 1.0 0.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# define outflow boundary condition
# can also be a geochemical composition like the previous BCs
outflow <- "
! --------------------------------------------------------------------------
'number and name of zone'
2
'outflow'

'boundary type'
'second'

'extent of zone'
2.0 2.0 0.0 1.0 0.0 5.0

'end of zone'
! --------------------------------------------------------------------------
"

# write transient boundary conditions to text file
# the contents of this text file will need to be copied and pasted into .dat file

write(paste(block15.header, initial.condition, block.closer,
            block16.header, groundwater, outflow, bankfull.overflow),
      file = out)

write(paste("! --------------------------------------------------------------------------
'update boundary conditions'
", nrow(df) - 1,
"                   ;number of target read times", sep = ""), file = out, append = TRUE)

for (m in 2:nrow(df)) {
  if (m == 2) {
    write(paste(df$ModelDay[m],
                "             ;target read times"), file = out,
          append = TRUE)
  }
  else {
    write(df$ModelDay[m], file = out, append = TRUE)
  }
}

# change the following loop based on number of geochemical types for the 
# transient boundary condition; make sure variable names are associated with
# correct type number (as provided in indicator file)
for (m in 2:nrow(df)) {
  write(paste("
! --------------------------------------------------------------------------
! target read time ", m - 1, "
'start of target read time input'
", m - 1, "
", if (df[m, "Transition to:"] == 1) {
  bankfull.overflow
}
else if (df[m, "Transition to:"] == 2) {
  rainfall
}
else {
  snowmelt
}, "
'end of target read time input'", sep = ""), file = out, append = TRUE)
}

write("
'done'", file = out, append = TRUE)

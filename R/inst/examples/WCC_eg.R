# WCC example
# Compartment 1:
#  Number of trees in stratum:
#  Total number of trees in all (12) plots = 146
#  Area of all plots = 12 * 0.02 ha = 0.24 ha
#  Average number of trees per hectare = 146/0.24 = 608.33
#  Number of trees in compartment = 608.33 * 5.6 ha (net area) = 3406
#  Mean (quadratic) dbh:
#    The number of trees in each centimetre dbh class in the stratum (based on
# adding together the values in each row) is shown in the right-most column of
# Section 2 of the data collection form (Figure A4.1). To calculate the mean
# (quadratic) dbh, multiply the number of trees in each class by the square of
# the dbh for each class. Next, add the results together for each class and
# divide by the number of trees measured. For the current example, this
# calculation gives a mean "squared" dbh of 629.27. The square root of this
# value gives the mean (quadratic) dbh: sqrt(629.27) = 25.1 cm

dbh = 25.1

# To calculate mean tree volume at a later stage the mean (quadratic) dbh was converted
# into the mean basal area using the equation in Section 4.1.5.

Mean_basal_area = (pi *25.12^2)/40000

library(WoodlandCarbonCode)
tariff_vol_area(vol, dbh, sigma_vol = NA, sigma_dbh = NA)

# Stand/stratum tariff number:
#  The stand tariff number is the rounded down mean of the single tree tariff numbers of
# each height sample tree recorded in Section 3 of the data collection form. The tariff
# number for each tree is calculated by entering the dbh and height measurements of each
# tree into Equation 3, described in Section 4.1.5. The parameters a1-a3 in the equation
# differ for each species. In this case, the parameters for Scots pine were selected. The
# results of each calculation should be rounded to the nearest whole tariff number and
# were entered in to the right-most column of Section 3 of the data collection form. The
# next step was to add all of the individual tariff numbers for the stratum together, which
# gave a total of 582
# 582 ÷ 24 = 24.2 rounded down to a stand tariff number of 24

eg$Tariffno <- conifer_tariff(spcode = eg$Spcode, height = eg$Height..m., dbh = eg$dbh..cm.)
eg$Tariffno_0dp <- round(eg$Tariffno, 0)
sum(eg$Tariffno_0dp)
floor(mean(eg$Tariffno_0dp))

tariff <- 24

a2 <- 0.315049301 * (tariff - 0.138763302)
  a1 <- (0.0360541 * tariff) - (a2 * 0.118288)


# Mean merchantable tree volume:
#   The mean tree volume is calculated using Equation 5 in Section 4.1.5 using the stand
# tariff number and the mean basal area (from mean quadratic dbh).
# Mean merchantable tree volume = a1 + (a2 * 0.049) = 0.344 m3
# where:
a1 = (0.0360541 * 24) - (a2 * 0.118288)
a2 = 0.315049301 * (24 - 0.138763302)

merchtreevol(dbh = 24, tariff = 24)

# Mean total stem volume:
#   To calculate the mean total stem volume, including volume above 7 cm top diameter,
# the mean merchantable volume is multiplied by the factor given in Table 4.1.9 (page 40)
# for the appropriate mean (quadratic) dbh:
#   Mean total stem volume = 0.344 * 1.01 = 0.34744 m3 (unrounded).






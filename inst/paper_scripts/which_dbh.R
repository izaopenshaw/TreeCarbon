# =========== Which DBH ======================

i = c(1:nrow(trees))
tree <- trees[c(i),]
nom <- paste(genus[i], species[i])
method = "IPCC2"

DBH         <- fc_agc_error(name =nom, dbh = trees$DBH[i],        height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBH_Manual  <- fc_agc_error(name =nom, dbh = trees$DBH_Manual[i], height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHsec      <- fc_agc_error(name =nom, dbh = trees$DBHsec[i],     height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHsec_100  <- fc_agc_error(name =nom, dbh = trees$DBHsec_100[i], height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHqsm      <- fc_agc_error(name =nom, dbh = trees$DBHqsm[i],     height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHqsm_100  <- fc_agc_error(name =nom, dbh = trees$DBHqsm_100[i], height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHcyl      <- fc_agc_error(name =nom, dbh = trees$DBHcyl[i],     height = height[i], height_timber = height_timber[i], type = type[i], method = method)
DBHcyl_100  <- fc_agc_error(name =nom, dbh = trees$DBHcyl_100[i], height = height[i], height_timber = height_timber[i], type = type[i], method = method)


# Write
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Wakehurst_analysis")
write.csv(DBH,        "DBH.csv",        row.names = FALSE)
write.csv(DBH_Manual, "DBH_Manual.csv", row.names = FALSE)
write.csv(DBHsec,     "DBHsec.csv",     row.names = FALSE)
write.csv(DBHsec_100, "DBHsec_100.csv", row.names = FALSE)
write.csv(DBHqsm,     "DBHqsm.csv",     row.names = FALSE)
write.csv(DBHqsm_100, "DBHqsm_100.csv", row.names = FALSE)
write.csv(DBHcyl,     "DBHcyl.csv",     row.names = FALSE)
write.csv(DBHcyl_100, "DBHcyl_100.csv", row.names = FALSE)


DBHs <- rbind(DBH, DBH_Manual, DBHsec, DBHsec_100, DBHqsm, DBHqsm_100, DBHcyl, DBHcyl_100)
DBH_measure <- c("DBH", "DBH_Manual", "DBHsec", "DBHsec_100", "DBHqsm", "DBHqsm_100", "DBHcyl", "DBHcyl_100")
DBHs <- cbind(DBH_measure, DBHs)

trees[i,]
trees[i,]$tariff


trees$wc_tariff <- NA
trees$spcode <- lookupcode(trees$Name, trees$Classification)

for (i in 1:nrow(trees)){
  tree <- trees[c(i),]
  nom <- paste(genus[i], species[i])

  tree.tariff <- tariffs(trees$spcode$code[i], height[i], trees$DBH[i], type[i], height_timber=height_timber[i]) #, re_h = re_h, re_dbh = re_dbh, re = re

  #wc <- fc_agc_error(nom, trees$DBH[i], height[i], height_timber = height_timber[i], type = type[i], method = method, re_dbh = re_dbh, re_h = re_h, re = re, rich_output = FALSE)
  trees$wc_tariff <- tree.tariff[1]
}

treeout <- cbind(tree, wc)

# NOTE: cbind creates duplicate column names. ALL$AGC_WCC_t returns the FIRST match,
# which is df_raw$AGC_WCC_t (pre-calculated WCC in the CSV). Use wcc_df$AGC_WCC_t
# for fc_agc_error output and results$WCC_C_t for allometries.

ALL <- cbind(trees, wcc_df)

plot(ALL$agc, ALL$AGC_WCC_t)

# Diagnostic Checks
fc_agc_vals <- wcc_df$AGC_WCC_t # from fc_agc_error
allom_vals  <- results$WCC_C_t  # from allometries()
js_vals <- df_raw$agc           # measured/TLS carbon from CSV

# Comparison df: all carbon sources side-by-side
comparison_df <- data.frame(
  fc_agc_error_AGC = wcc_df$AGC_WCC_t,   # fc_agc_error (with uncertainty)
  allometries_WCC  = results$WCC_C_t,    # allometries WCC carbon
  csv_AGC_WCC_t    = df_raw$AGC_WCC_t,   # pre-calc WCC in CSV (= ALL$AGC_WCC_t)
  measured_agc     = df_raw$agc          # measured/TLS carbon
)
head(comparison_df)

# allometries vs fc_agc_error
plot(fc_agc_vals, allom_vals, xlab = "fc_agc_error AGC_WCC_t", ylab = "allometries WCC_C_t",
     main = "allometries vs fc_agc_error")
abline(0, 1, col = "red", lty = 2)

# J's (measured) vs fc_agc_error
plot(fc_agc_vals, js_vals, xlab = "fc_agc_error AGC_WCC_t", ylab = "measured agc")
abline(0, 1, col = "red", lty = 2)

# CSV WCC vs measured (ALL$AGC_WCC_t = df_raw$AGC_WCC_t, first in cbind)
plot(ALL$AGC_WCC_t, ALL$agc, xlab = "CSV AGC_WCC_t", ylab = "measured agc")
abline(0, 1, col = "red", lty = 2)


# tariffs
plot(wcc_df$tariff, df_raw$tariff)

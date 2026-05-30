# Tests for fc_plot_stratum_carbon() — FC_WCC plot Methods B/C office work

test_that("fc_plot_stratum_carbon returns structure and positive carbon for oak sample", {
  plot_list <- list(
    p1 = data.frame(
      Genus = "Quercus", Species = "robur", DBH = 40, Height = 18,
      Classification = "broadleaf", stringsAsFactors = FALSE
    ),
    p2 = data.frame(
      Genus = "Quercus", Species = "robur", DBH = 45, Height = 20,
      Classification = "broadleaf", stringsAsFactors = FALSE
    )
  )
  plot_areas_ha <- c(p1 = 0.02, p2 = 0.02)
  stratum_area_ha <- 0.5
  res <- fc_plot_stratum_carbon(
    plot_list, plot_areas_ha, stratum_area_ha,
    type = "broadleaf", seed = 42L
  )
  expect_true(is.numeric(res$carbon_t))
  expect_true(res$carbon_t > 0)
  expect_true(is.numeric(res$n_stratum) && res$n_stratum > 0)
  expect_true(is.numeric(res$trees_per_ha))
  expect_s3_class(res$by_species, "data.frame")
  expect_true(nrow(res$by_species) >= 1L)
  expect_true(all(c("Species", "N_sample", "QMD_cm", "Mean_tariff", "Carbon_t") %in%
                    names(res$by_species)))
})

test_that("fc_plot_stratum_carbon errors when plot_areas_ha missing a plot name", {
  plot_list <- list(a = data.frame(
    Genus = "Quercus", Species = "robur", DBH = 40, Height = 18,
    Classification = "broadleaf", stringsAsFactors = FALSE
  ))
  expect_error(
    fc_plot_stratum_carbon(plot_list, c(b = 0.01), 1, type = "broadleaf"),
    "missing entries"
  )
})

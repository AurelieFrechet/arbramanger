test_that("nom_latin", {
  expect_equal(nom_latin("genre", "espece", "variete"),
  "<i>Genre espece variete </i>")
})

test_that("description_popup", {
  expect_equal(description_popup("nom_commun", "genre", "espece", "variete"),
               "nom_commun <br> <i>Genre espece variete </i>")
})

# test_that("croquis", {

# })

test_that("description_beaulieue", {
  df_beaulieu <- read_excel("data/arboretum/donnees_arboretum.xlsx")
  expect_no_error(description_beaulieu(id_svg = "sapin_blanc", df_beaulieu = df_beaulieu))
})

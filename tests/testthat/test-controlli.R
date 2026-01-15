context("Funzioni di controllo di dati in input")


# --- Valori anomali --- #
testthat::test_that("outlier GESD - caso base", {
  x <- c(10, 11, 9, 10, 12, 11, 10, 45)

  res <- outlier_gesd(x, significance = 0.95)

  # struttura output
  expect_type(res, "list")
  expect_named(res, c("data", "result"))

  # data.frame corretto
  expect_s3_class(res$data, "data.frame")
  expect_named(res$data, c("id", "I", "R", "lambda", "outlier"))

  # 45 deve essere identificato come outlier
  expect_true(45 %in% res$data$I[res$data$outlier])

  # messaggio descrittivo
  expect_match(res$result, "valore anomalo")
})

testthat::test_that("outlier GESD - nessun outlier", {
  x <- c(10, 11, 9, 10, 12, 11, 10, 9)

  res <- outlier_gesd(x)

  expect_false(any(res$data$outlier))
  expect_equal(res$result, "nessun valore anomalo.")
})

testthat::test_that("outlier GESD - più outlier", {
  x <- c(10, 11, 9, 10, 12, 50, 45, 10, 11)

  res <- outlier_gesd(x, significance = 0.95)

  out_vals <- res$data$I[res$data$outlier]

  expect_true(all(c(45, 50) %in% out_vals))
  expect_match(res$result, "possibili valori anomali")
})

testthat::test_that("outlier GESD - controlli input", {
  expect_error(outlier_gesd("a"))
  expect_error(outlier_gesd(c(1, 2, 3, 4))) # meno di 5
  expect_error(outlier_gesd(1:10, significance = 0.8))
  expect_error(outlier_gesd(1:10, significance = 1))
  expect_error(outlier_gesd(1:10, m = 0))
  expect_error(outlier_gesd(1:10, m = 20))
})

# Results from  Tietjen and Moore (August 1972),
# Some Grubbs-Type Statistics for the Detection of Outliers,
# Technometrics, 14(3), pp. 583-597. Also available at
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm
testthat::test_that("outlier GESD - Tietjen and Moore (August 1972)", {

  res <- outlier_gesd(uranium_cps, significance = 0.99)

  expect_equal(sum(res$data$outlier), 1)
  expect_equal(res$result, "245.57 è un possibile valore anomalo.")
})

# Results from UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4:
# Detection and treatment of outliers. Section 4.3.2.
testthat::test_that("outlier GESD - UNI ISO 16269-4:2019", {

  res <- outlier_gesd(uniiso_16269_4_432, significance = 0.95)

  expect_equal(sum(res$data$outlier), 2)
  expect_equal(res$result, "12.6, 5.8 sono possibili valori anomali.")
})

testthat::test_that("outlier GESD - deviazione standard nulla", {
  expect_error(outlier_gesd(rep(10, 6)))
})

# --- Normalità --- #
testthat::test_that("normalità Shapiro - dati normali", {
  set.seed(123)
  x <- rnorm(100)

  res <- normalita_shapiro(x)

  expect_type(res, "list")
  expect_named(res, c("W", "pvalue", "alpha", "result"))

  expect_true(res$pvalue > 0.05)
  expect_match(res$result, "compatibili con una distribuzione normale")
})

testthat::test_that("normalità Shapiro - dati non normali", {
  set.seed(123)
  x <- rexp(100)

  res <- normalita_shapiro(x, significance = 0.95)

  expect_true(res$pvalue <= 0.05)
  expect_match(res$result, "non sono compatibili con una distribuzione normale")
})

testthat::test_that("normalità Shapiro - alpha personalizzato", {
  set.seed(123)
  x <- rnorm(50)

  res <- normalita_shapiro(x, significance = 0.90)

  expect_equal(res$alpha, 0.10)
})

# Results from An analysis of variance test for normality (complete samples),
#'  Biometrika (1965), 52, 3 and 2, p. 591.
#'  Section 4 - Examples, pag. 606, Example 1.
testthat::test_that("normalità Shapiro - Shapiro and Wilk (Biometrika 1965)", {
  res <- normalita_shapiro(shapirotest_reference, significance = 0.95)

  expect_equal(res$alpha, 0.05)
  expect_lt(res$pvalue, 0.01)
  expect_equal(res$result, "i valori non sono compatibili con una distribuzione normale.")
})

testthat::test_that("normalità Shapiro - controlli input", {
  expect_error(normalita_shapiro("a"))
  expect_error(normalita_shapiro(1:2))        # < 3
  expect_error(normalita_shapiro(1:6000))     # > 5000
  expect_error(normalita_shapiro(1:10, alpha = 0))
  expect_error(normalita_shapiro(1:10, alpha = 1))
})

# --- Bande di dispersione robuste --- #
test_that("banda_robusta calcola correttamente mediana e MAD", {
  x <- c(10, 11, 9, 10, 12, 11, 10)

  res <- banda_robusta(x, k = 2)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("mediana", "mad", "k", "lower", "upper") %in% names(res)))

  expect_equal(res$mediana, median(x))
  expect_equal(res$mad, mad(x, constant = 1.4826))
  expect_equal(res$k, 2)

  expect_equal(res$lower, res$mediana - 2 * res$mad)
  expect_equal(res$upper, res$mediana + 2 * res$mad)
})

test_that("banda_robusta gestisce NA correttamente", {
  x <- c(10, 11, 9, 10, 12, NA, NA)

  res <- banda_robusta(x, k = 3, na.rm = TRUE)

  expect_equal(res$mediana, median(x, na.rm = TRUE))
})

test_that("banda_robusta fallisce con input non valido", {
  expect_error(banda_robusta("a"))
  expect_error(banda_robusta(1:4))              # meno di 5 valori
  expect_error(banda_robusta(1:10, k = 0))
  expect_error(banda_robusta(1:10, k = -1))
})

test_that("banda_robusta fallisce con MAD nulla", {
  expect_error(banda_robusta(rep(10, 6)))
})
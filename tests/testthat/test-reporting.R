context("Funzioni di formattazione e reporting")

# --- Cifre significative --- #
test_that("format_signif restituisce le cifre significative corrette", {
  x <- c(0, 1, 1.234, 10, 0.00456, NA)
  res <- format_signif(x, 2)
  expect_equal(res, c("0.0", "1.0", "1.2", "10", "0.0046", NA_character_))
})

test_that("format_signif gestisce input non numerici e n non valido", {
  expect_error(format_signif("abc", 2), "x deve essere numerico")
  expect_error(format_signif(1:3, 0), "n deve essere un numero intero positivo")
})

# --- Requisiti soddisfatti sì o no --- #
test_that("check_requisito_dt verifica correttamente i requisiti", {
  risultati <- c("10", "15", "5.5", "abc", "20")
  req_min <- c(5, 10, NA, 0, 18)
  req_max <- 15

  out <- check_requisito_dt(risultati, req_max, req_min)
  expect_equal(out, c(TRUE, TRUE, TRUE, NA, FALSE))
})

test_that("check_requisito_dt gestisce input non validi", {
  expect_error(check_requisito_dt(1:5, 10, 5), "risultato_txt deve essere character")
  expect_error(check_requisito_dt(c("1"), "10", 5), "requisito_max e requisito_min devono essere numerici")
})

# --- Formattazione testuale del risultato --- #
test_that("format_risultato produce stringhe corrette", {
  s1 <- format_risultato(req1 = 10, req2 = 20, value = 15, udm = "mg/L", esito = TRUE)
  expect_true(grepl("requisito: 10 - 20", s1))
  expect_true(grepl("risultato: 15", s1))
  expect_true(grepl("\u2705", s1))  # ✅

  s2 <- format_risultato(req1 = NA, req2 = 5, value = 4.8, udm = "mg/L", esito = FALSE)
  expect_true(grepl("risultato: 4.8", s2))
  expect_true(grepl("\u274C", s2))  # ❌

  s3 <- format_risultato(req1 = NA, req2 = NA, value = 7.2, udm = "mg/L", esito = NA)
  expect_equal(s3, "risultato: 7.2 mg/L")
})

# --- Prima lettera maiuscola --- #
test_that("capitalizza mette maiuscola la prima lettera", {
  expect_equal(capitalizza("mappa"), "Mappa")
  expect_equal(capitalizza("test"), "Test")
  expect_equal(capitalizza("r è figo"), "R è figo")
  expect_equal(capitalizza("xyz"), "Xyz")
})


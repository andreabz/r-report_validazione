context("Funzioni per il calcolo dei parametri prestazionali")


# --- Deviazione standard --- #
testthat::test_that("deviazione standard", {
  # valore formattato
  expect_equal(dev_std(c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)), "0.15")
  # valore numerico
  expect_equal(
    dev_std(c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1), sigfigs = "numeric"),
    0.147196
  )

  # errori
  expect_error(dev_std(c(10.1, 9.9, 10.0)))
  expect_error(dev_std(c("10.1", "9.9", "10.0", "10.2", "9.8", "10.1")))
  expect_error(dev_std(c(10.1, 9.9, 10.0, 10.2, NA, NA)))
})

# --- Limite di ripetibilità --- #
testthat::test_that("limite di ripetibilità", {
  valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)

  # valore formattato
  expect_equal(limite_ripetibilita(valori), "0.54")

  # valore numerico
  expect_equal(
    limite_ripetibilita(valori, sigfigs = "numeric"),
    qt(0.975, 5) * sqrt(2) * sd(valori)
  )

  # confidence diverso
  expect_equal(
    limite_ripetibilita(valori, confidence = 0.90, sigfigs = "numeric"),
    qt(0.95, 5) * sqrt(2) * sd(valori)
  )

  # errori
  expect_error(limite_ripetibilita(c(1, 2, 3)))
  expect_error(limite_ripetibilita("a"))
  expect_error(limite_ripetibilita(valori, confidence = 0.7))
  expect_error(limite_ripetibilita(valori, sigfigs = -1))
})

# --- Coefficiente di variazione percentuale --- #
testthat::test_that("coefficiente di variazione percentuale", {
  valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)

  # valore formattato
  expect_equal(cv_percento(valori), "1.5")

  # valore numerico
  expect_equal(
    cv_percento(valori, sigfigs = "numeric"),
    sd(valori) / mean(valori) * 100
  )

  # media zero
  expect_error(cv_percento(rep(0, 6)))

  # errori
  expect_error(cv_percento(c(1, 2, 3)))
  expect_error(cv_percento("a"))
  expect_error(cv_percento(valori, sigfigs = 0))
})

# --- Media aritmetica --- #
testthat::test_that("media aritmetica", {
  valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)

  # valore formattato
  expect_equal(media_aritmetica(valori), "10")

  # valore numerico
  expect_equal(
    media_aritmetica(valori, sigfigs = "numeric"),
    mean(valori)
  )

  # NA ammessi
  expect_equal(
    media_aritmetica(c(valori, NA), sigfigs = "numeric"),
    mean(valori)
  )

  # errori
  expect_error(media_aritmetica(c(1, 2, 3)))
  expect_error(media_aritmetica("a"))
  expect_error(media_aritmetica(valori, sigfigs = -2))
})

# --- Intervallo di confidenza della media --- #
testthat::test_that("intervallo di confidenza della media", {
  valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)

  n <- length(valori)
  s <- sd(valori)
  se <- s / sqrt(n)
  tcrit <- qt(0.975, df = n - 1)

  # ampiezza numerica
  expect_equal(
    ic_media(valori, sigfigs = "numeric"),
    tcrit * se
  )

  # output formattato
  expect_match(ic_media(valori), "–")

  # confidence diverso
  expect_equal(
    ic_media(valori, confidence = 0.90, sigfigs = "numeric"),
    qt(0.95, df = n - 1) * se
  )

  # errori
  expect_error(ic_media(c(1, 2, 3)))
  expect_error(ic_media("a"))
  expect_error(ic_media(valori, confidence = 0.7))
  expect_error(ic_media(valori, sigfigs = 0))
})

# --- Intervalli di lavoro --- #
testthat::test_that("intervallo data.table", {
  dt <- data.table::data.table(
    parametro = c(
      "Intervallo di lavoro",
      "Intervallo di lavoro",
      "Intervallo di linearità"
    ),
    minimo = c(1, 2, 0.5),
    massimo = c(10, 8, 5),
    udm = c("mg/kg", "mg/kg", "mg/kg")
  )

  expect_equal(
    intervallo_dt(dt, "lavoro"),
    "1–10 mg/kg"
  )

  expect_equal(
    intervallo_dt(dt, "linearità"),
    "0.5–5 mg/kg"
  )

  # tipo assente
  expect_true(is.na(intervallo_dt(dt[1], "linearità")))

  # errori
  expect_error(intervallo_dt(as.data.frame(dt), "lavoro"))
  expect_error(intervallo_dt(dt, "altro"))
})

# --- Incertezza di Horwitz-Thompson --- #
testthat::test_that("horwitz_thompson", {
  # Horwitz (concentrazione alta)
  res <- horwitz_thompson(1, unit = "mg/kg")
  expect_equal(res$method, "Horwitz")
  expect_equal(res$RSD |> round(3), 0.159)
  expect_equal(res$U |> round(3), 0.318)

  # Thompson (concentrazione bassa)
  res_low <- horwitz_thompson(50, unit = "pg/g")
  expect_equal(res_low$method, "Thompson")
  expect_equal(res_low$RSD, 0.22)
  expect_equal(res_low$U, 22)

  # vettore
  res_vec <- horwitz_thompson(c(0.1, 1, 10), unit = "ug/l")
  expect_equal(nrow(res_vec), 3)

  # unità non supportata
  expect_error(horwitz_thompson(1, unit = "ppm"))
  expect_error(horwitz_thompson(1, unit = "ppb"))

  # concentrazione negativa o zero
  expect_error(horwitz_thompson(0, unit = "mg/kg"))
  expect_error(horwitz_thompson(-1, unit = "mg/kg"))

  # lunghezza unit incoerente
  expect_error(horwitz_thompson(c(1, 2), unit = c("mg/kg", "ug/kg", "ng/kg")))
})

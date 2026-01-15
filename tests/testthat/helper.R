library(here)
source(here("R/controlli.R"))
source(here("R/prestazioni.R"))
source(here("R/reporting.R"))

#' Mass spectrometer measurements on a Uranium isotope
#'
#' A dataset containing the results expressed counts per seconds for mass spectrometer measurements on a Uranium isotope.
#' The variable are as follows:
#'
#' @format a vector with 8 numerical values:
#'
#' @name uranium_cps
#' @docType data
#' @author Gary L. Tietjen
#' @author Roger H. Moore
#' @source Some Grubbs-Type Statistics for the Detection of Several Outliers.
#'  Technometrics, 14(3), 1972, pp. 583-597.
#'  Also available at
#'  \url{https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm}.
#' @keywords data

uranium_cps <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)

#' Observations from UNI ISO 16269-4:2019 - Section 4.3.2
#'
#' A dataset containing 20 observations for outliers detection.
#' The variable are as follows:
#'
#' @format a vector with 20 numerical values:
#'
#' @name uniiso_16269_4_432
#' @docType data
#' @author ISO/TC 69 - Applications of statistical methods
#' @source UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4:
#'  Detection and treatment of outliers. Section 4.3.2.
#'  \url{https://store.uni.com/uni-iso-16269-4-2019}.
#' @keywords data

uniiso_16269_4_432 <- c(
  -2.21,
  -1.84,
  -0.95,
  -0.91,
  -0.36,
  -0.19,
  -0.11,
  -0.10,
  0.18,
  0.30,
  0.43,
  0.51,
  0.64,
  0.67,
  0.93,
  1.22,
  1.35,
  1.73,
  5.80,
  12.60
)

#' Shapiro-Wilk test data
#'
#' A dataset with 11 values of men weights expressed in pounds.
#' The dataset is provided for testing the results of \code{fct_shapiro}.
#'
#' @format a vector with 11 numerical elements.
#'
#' @name shapiro_reference
#' @docType data
#' @author S. S. Shapiro
#' @author M. B. Wilk
#' @source An analysis of variance test for normality (complete samples),
#'  Biometrika (1965), 52, 3 and 2, p. 591.
#'  Section 4 - Examples, pag. 606, Example 1.
#'  \url{http://links.jstor.org/sici?sici=0006-3444%28196512%2952%3A3%2F4%3C591%3AAAOVTF%3E2.0.CO%3B2-B}
#' @keywords data

shapirotest_reference <- c(148, 154, 158, 160, 161, 162, 166, 170, 182, 195, 236)

# Dataset di prova per grafico
dt_test <- data.table(
  matrice = "terreno",
  analita = "A",
  livello = c(10, 10, 10, 10, 10, 20, 20, 20, 20, 20),
  valore_misurato = c(9.8, 10.1, 10.0, 10.2, 10.0, 19.5, 20.3, 20.1, 20.0, 19.8),
  udm = "ng/kg"
)
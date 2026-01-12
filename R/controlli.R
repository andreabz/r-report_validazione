library(data.table)
# ---- Controlli sui dati ---- #

#' Test GESD per l'individuazione di valori anomali
#'
#' Implementa il test \emph{Generalized Extreme Studentized Deviate (GESD)}
#' per l'identificazione di uno o più valori anomali in un campione univariato.
#' Il metodo rimuove iterativamente l'osservazione più distante dalla media e
#' confronta la statistica del test con una soglia critica dipendente dal livello
#' di significatività scelto.
#'
#' L'implementazione è basata sull'approccio proposto da Rosner (1983) ed è
#' equivalente al test di Rosner per outlier multipli.
#'
#' @param values Vettore numerico contenente i dati da analizzare.
#'   Deve contenere almeno 5 osservazioni.
#' @param significance Livello di significatività del test (default \code{0.95}).
#'   Deve essere compreso tra \code{0.90} (incluso) e \code{1} (escluso).
#' @param m Numero massimo di valori anomali ipotizzabili.
#'   Di default è pari a circa \eqn{2/9} della numerosità del campione.
#'
#' @details
#' Il test GESD procede come segue:
#' \enumerate{
#'   \item calcola media e deviazione standard del campione;
#'   \item individua l'osservazione con la massima deviazione assoluta dalla media;
#'   \item calcola la statistica \eqn{R_l};
#'   \item rimuove l'osservazione più estrema;
#'   \item ripete la procedura fino a \code{m} rimozioni;
#'   \item confronta ciascun valore \eqn{R_l} con il corrispondente valore critico
#'   \eqn{\lambda_l} e determina il numero finale di outlier.
#' }
#'
#' Il test è bilaterale e assume che i dati (al netto degli outlier)
#' seguano approssimativamente una distribuzione normale.
#'
#' @return
#' Una lista con due elementi:
#' \describe{
#'   \item{data}{\code{data.frame} contenente, per ogni iterazione:
#'     \itemize{
#'       \item \code{id}: indice originale dell'osservazione;
#'       \item \code{I}: valore osservato;
#'       \item \code{R}: statistica GESD calcolata;
#'       \item \code{lambda}: valore critico associato;
#'       \item \code{outlier}: indicatore logico (\code{TRUE/FALSE}).
#'     }
#'   }
#'   \item{result}{Stringa descrittiva in italiano che riassume
#'   la presenza o assenza di valori anomali.}
#' }
#'
#' @references
#' Rosner, B. (1983).
#' Percentage Points for a Generalized ESD Many-Outlier Procedure.
#' \emph{Technometrics}, 25(2), 165–172.
#'
#' @examples
#' x <- c(10, 11, 9, 10, 12, 11, 10, 45)
#'
#' res <- outlier_gesd(x, significance = 0.95)
#'
#' res$data
#' res$result
#'
#' @seealso
#' \code{\link[EnvStats]{rosnerTest}}
#'
#' @export
outlier_gesd <- function(
  values,
  significance = 0.95,
  m = round(length(values) * 2 / 9, 0)
) {
  ## --- Controlli ------------------------------------------------------------
  stopifnot(
    is.numeric(values),
    is.vector(values),
    length(values) >= 5,
    is.numeric(significance),
    significance >= 0.90 && significance < 1,
    is.numeric(m),
    m >= 1,
    m <= length(values)
  )

  ## --- Funzione lambda critica ---------------------------------------------
  lambda_l <- function(n, l, signif) {
    alpha <- 1 - signif
    n_l <- n - l
    p <- (1 - alpha / 2)^(1 / n_l)
    t_p <- stats::qt(p, df = n_l - 2)

    ((n_l - 1) * t_p) /
      sqrt((n_l - 2 + t_p^2) * n_l)
  }

  ## --- Testo descrittivo ----------------------------------------------------
  text_result <- function(res) {
    idx <- which(res$outlier)
    if (length(idx) == 0) {
      "nessun valore anomalo."
    } else if (length(idx) == 1) {
      paste(res$I[idx], "è un possibile valore anomalo.")
    } else {
      paste(
        paste(res$I[idx], collapse = ", "),
        "sono possibili valori anomali."
      )
    }
  }

  ## --- Preparazione ---------------------------------------------------------
  n <- length(values)

  # manteniamo l'indice originale
  df <- data.frame(
    id = seq_along(values),
    I = values
  )

  # preallocazione (niente rbind nel loop)
  res <- vector("list", m + 1)

  ## --- Algoritmo GESD -------------------------------------------------------
  for (l in 0:m) {
    x <- df$I
    x_sd <- stats::sd(x)

    # protezione da sd = 0
    if (x_sd == 0) {
      stop("outlier GESD - deviazione standard nulla:
        impossibile calcolare la statistica R")
    }

    x_mean <- mean(x)
    dev <- abs(x - x_mean)
    i_max <- which.max(dev)

    R <- dev[i_max] / x_sd

    res[[l + 1]] <- data.frame(
      id = df$id[i_max],
      I = df$I[i_max],
      R = R,
      l = l
    )

    df <- df[-i_max, , drop = FALSE]
  }

  df_result <- do.call(rbind, res)

  ## --- Lambda e decisione ---------------------------------------------------
  df_result$lambda <- lambda_l(n, df_result$l, significance)

  idx_out <- which(df_result$R > df_result$lambda)
  k <- if (length(idx_out) == 0) 0 else max(idx_out)

  df_result$outlier <- seq_len(nrow(df_result)) <= k

  ## --- Output ---------------------------------------------------------------
  df_result <- df_result[, c("id", "I", "R", "lambda", "outlier")]

  list(
    data = df_result,
    result = text_result(df_result)
  )
}

#' Test di normalità di Shapiro-Wilk
#'
#' Esegue il test di Shapiro-Wilk per verificare la compatibilità di un campione
#' univariato con una distribuzione normale.
#'
#' Il test è valido per campioni di dimensione compresa tra 3 e 5000 osservazioni.
#'
#' @param values Vettore numerico contenente i dati da analizzare.
#' @param significance Livello di significatività del test (default \code{0.95}).
#'
#' @details
#' L'ipotesi nulla del test è che i dati provengano da una distribuzione normale.
#' Un valore di \code{p.value} inferiore o uguale a \code{alpha} porta al rifiuto
#' dell'ipotesi nulla.
#'
#' @return
#' Una lista con i seguenti elementi:
#' \describe{
#'   \item{W}{Valore della statistica di Shapiro-Wilk.}
#'   \item{pvalue}{Valore p associato al test.}
#'   \item{alpha}{Livello di significatività utilizzato.}
#'   \item{result}{Stringa descrittiva in italiano con l'esito del test.}
#' }
#'
#' @examples
#' x <- rnorm(50)
#' normalita_shapiro(x)
#'
#' @seealso
#' \code{\link[stats]{shapiro.test}}
#'
#' @export
normalita_shapiro <- function(values, significance = 0.95) {
  stopifnot(
    is.numeric(values),
    is.vector(values),
    length(values) >= 3,
    length(values) <= 5000,
    is.numeric(significance),
    significance > 0 && significance < 1
  )

  test <- stats::shapiro.test(values)
  alpha <- 1 - significance

  result <- if (test$p.value <= alpha) {
    "i valori non sono compatibili con una distribuzione normale."
  } else {
    "i valori sono compatibili con una distribuzione normale."
  }

  list(
    W = unname(test$statistic),
    pvalue = test$p.value,
    alpha = alpha,
    result = result
  )
}

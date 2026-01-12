library(data.table)
# ---- Parametri prestazionali ---- #

#' Deviazione standard
#'
#' Calcola la deviazione standard \eqn{s} per una serie di risultati
#' ottenuti in condizioni di ripetibilità.
#'
#' @param values Un vettore numerico contenente i risultati di prova.
#'   I valori \code{NA} sono ammessi ma vengono esclusi dal calcolo.
#' @param sigfigs Numero di cifre significative per la formattazione
#'   del risultato. Usare \code{"numeric"} per ottenere il valore numerico
#'   non formattato.
#'
#' @return
#' Un valore numerico se \code{sigfigs = "numeric"}, altrimenti una stringa
#' contenente il limite di ripetibilità formattato.
#'
#' @details
#' Sono richiesti almeno 6 valori numerici non \code{NA}.
#' Il calcolo assume che i risultati siano ottenuti in condizioni
#' di ripetibilità (stesso operatore, stesso strumento, stesso laboratorio,
#' breve intervallo di tempo).
#'
#' @examples
#' valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)
#'
#' dev_std(valori)
#'
#' # Valore numerico non formattato
#' dev_std(valori, sigfigs = "numeric")
#'
#' @export
dev_std <- function(values, sigfigs = 2) {
  if (!is.numeric(values)) {
    stop("I valori devono essere numerici")
  }

  if (sum(!is.na(values)) < 6) {
    stop("Servono almeno 6 valori non NA")
  }

  if (
    !((is.character(sigfigs) && sigfigs == "numeric") ||
      (is.numeric(sigfigs) && length(sigfigs) == 1 && sigfigs > 0))
  ) {
    stop("sigfigs deve essere un numero positivo oppure 'numeric'")
  }

  num_vals <- values[!is.na(values)]
  sr <- sd(num_vals)

  if (sigfigs == "numeric") {
    sr
  } else {
    format_signif(sr, sigfigs)
  }
}

#' Limite di ripetibilità
#'
#' Calcola il limite di ripetibilità \eqn{r} per una serie di risultati
#' ottenuti in condizioni di ripetibilità, nell'ambito della validazione
#' di un metodo di prova.
#'
#' Il limite di ripetibilità è definito come:
#' \deqn{
#' r = t_{(1 + c)/2, \, \nu} \cdot \sqrt{2} \cdot s_r
#' }
#' dove \eqn{s_r} è la deviazione standard di ripetibilità stimata
#' sperimentalmente, \eqn{c} è il livello di confidenza e
#' \eqn{\nu = n - 1} sono i gradi di libertà associati alla stima di
#' \eqn{s_r}.
#'
#' Poiché la deviazione standard di ripetibilità non è nota a priori ma
#' stimata a partire dai dati sperimentali, il fattore di copertura è
#' determinato utilizzando la distribuzione t di Student.
#'
#' Per livelli di confidenza prossimi al 95\% e numerosità sufficientemente
#' elevate, il valore ottenuto è prossimo alla definizione della norma
#' ISO 5725, per cui \eqn{r \approx 2.8 \cdot s_r}.
#'
#' @param values Un vettore numerico contenente i risultati di prova
#'   ottenuti in condizioni di ripetibilità. I valori \code{NA} sono ammessi
#'   ma vengono esclusi dal calcolo.
#' @param confidence Livello di confidenza desiderato per il confronto tra
#'   due risultati singoli. Il valore predefinito \code{0.95} è coerente con
#'   la pratica di validazione secondo ISO 5725.
#' @param sigfigs Numero di cifre significative per la formattazione del
#'   risultato. Usare \code{"numeric"} per ottenere il valore numerico non
#'   formattato.
#'
#' @return
#' Un valore numerico se \code{sigfigs = "numeric"}, altrimenti una stringa
#' contenente il limite di ripetibilità formattato.
#'
#' @details
#' Sono richiesti almeno 6 risultati numerici non \code{NA}.
#' Il calcolo assume che:
#' \itemize{
#'   \item i risultati seguano una distribuzione approssimativamente normale;
#'   \item le prove siano state eseguite in condizioni di ripetibilità
#'   (stesso operatore, stesso strumento, stesso laboratorio e breve
#'   intervallo di tempo);
#'   \item la deviazione standard di ripetibilità sia stimata dai dati
#'   sperimentali.
#' }
#'
#' @references
#' ISO 5725-2:2019. \emph{Accuracy (trueness and precision) of measurement
#' methods and results — Part 2: Basic method for the determination of
#' repeatability and reproducibility of a standard measurement method}.
#'
#' @examples
#' valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)
#'
#' # Limite di ripetibilità (95%)
#' limite_ripetibilita(valori)
#'
#' # Valore numerico non formattato
#' limite_ripetibilita(valori, sigfigs = "numeric")
#'
#' # Livello di confidenza diverso
#' limite_ripetibilita(valori, confidence = 0.90)
#'
#' @export
limite_ripetibilita <- function(values, confidence = 0.95, sigfigs = 2) {
  if (!is.numeric(values)) {
    stop("I valori devono essere numerici")
  }

  if (sum(!is.na(values)) < 6) {
    stop("Servono almeno 6 valori non NA")
  }

  if (confidence < 0.8 || confidence > 0.999) {
    stop("Il livello di confidenza deve essere tra 0.8 e 0.999")
  }

  if (
    !((is.character(sigfigs) && sigfigs == "numeric") ||
      (is.numeric(sigfigs) && length(sigfigs) == 1 && sigfigs > 0))
  ) {
    stop("sigfigs deve essere un numero positivo oppure 'numeric'")
  }

  num_vals <- values[!is.na(values)]
  sr <- sd(num_vals)
  alpha <- (1 + confidence) / 2
  dof <- length(num_vals) - 1
  r <- qt(alpha, dof) * sqrt(2) * sr

  if (sigfigs == "numeric") {
    r
  } else {
    format_signif(r, sigfigs)
  }
}


#' Coefficiente di variazione percentuale (CV%)
#'
#' Calcola il coefficiente di variazione percentuale (CV\%) di una serie
#' di valori numerici, definito come il rapporto tra la deviazione standard
#' e la media dei valori, moltiplicato per 100.
#'
#' @param values Un vettore numerico contenente i valori osservati.
#'   I valori \code{NA} sono ammessi ma vengono esclusi dal calcolo.
#' @param sigfigs Numero di cifre significative per la formattazione
#'   del risultato. Usare \code{"numeric"} per ottenere il valore numerico
#'   non formattato.
#'
#' @return
#' Un valore numerico se \code{sigfigs = "numeric"}, altrimenti una stringa
#' contenente il coefficiente di variazione percentuale formattato.
#'
#' @details
#' Sono richiesti almeno 6 valori numerici non \code{NA}.
#' Il coefficiente di variazione non è definito se la media dei valori è zero.
#'
#' @examples
#' valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)
#'
#' # CV% formattato
#' cv_percento(valori)
#'
#' # CV% numerico
#' cv_percento(valori, sigfigs = "numeric")
#'
#' @export
cv_percento <- function(values, sigfigs = 2) {
  if (!is.numeric(values)) {
    stop("I valori devono essere numerici")
  }

  if (sum(!is.na(values)) < 6) {
    stop("Servono almeno 6 valori non NA")
  }

  if (
    !((is.character(sigfigs) && sigfigs == "numeric") ||
      (is.numeric(sigfigs) && length(sigfigs) == 1 && sigfigs > 0))
  ) {
    stop("sigfigs deve essere un numero positivo oppure 'numeric'")
  }

  num_vals <- values[!is.na(values)]
  mean_val <- mean(num_vals)

  if (mean_val == 0) {
    stop("La media dei valori è zero: CV non definito")
  }

  sd_val <- sd(num_vals)
  cv <- sd_val / mean_val * 100

  if (sigfigs == "numeric") {
    cv
  } else {
    format_signif(cv, sigfigs)
  }
}


#' Calcola l'intervallo minimo-massimo per tipo e unità
#'
#' Questa funzione è ottimizzata per data.table e permette di calcolare
#' gli intervalli di "lavoro" o "linearità" per ogni gruppo.
#'
#' @param dt Un data.table contenente almeno le colonne: parametro, minimo, massimo e l'unità di misura.
#' @param type Tipo di intervallo da calcolare: "lavoro" o "linearità".
#' @param unit_col Nome della colonna contenente l'unità di misura (stringa).
#'
#' @return Una stringa con formato "min–max unità".
#' @export
intervallo_dt <- function(dt, type, unit_col = "udm") {
  if (!is.data.table(dt)) {
    stop("dt deve essere un data.table")
  }
  if (!(type %in% c("lavoro", "linearità"))) {
    stop("type deve essere 'lavoro' o 'linearità'")
  }

  tipo <- if (type == "lavoro") {
    "Intervallo di lavoro"
  } else {
    "Intervallo di linearità"
  }

  # Filtra solo il tipo corretto
  subset_dt <- dt[parametro == tipo]

  if (nrow(subset_dt) == 0) {
    return(NA_character_)
  }

  # Calcola minimo e massimo per il subset
  minimo <- min(subset_dt$minimo, na.rm = TRUE)
  massimo <- max(subset_dt$massimo, na.rm = TRUE)

  # Restituisci la stringa con unità della prima riga (assumendo omogenea)
  paste0(minimo, "–", massimo, " ", subset_dt[[unit_col]][1])
}

#' Media aritmetica
#'
#' Calcola la media aritmetica di una serie di risultati numerici
#' ottenuti in condizioni di ripetibilità.
#'
#' @param values Un vettore numerico contenente i risultati di prova.
#'   I valori \code{NA} sono ammessi ma vengono esclusi dal calcolo.
#' @param sigfigs Numero di cifre significative per la formattazione
#'   del risultato. Usare \code{"numeric"} per ottenere il valore numerico
#'   non formattato.
#'
#' @return
#' Un valore numerico se \code{sigfigs = "numeric"}, altrimenti una stringa
#' contenente la media formattata.
#'
#' @details
#' Sono richiesti almeno 6 valori numerici non \code{NA}.
#'
#' @examples
#' valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)
#'
#' media_aritmetica(valori)
#' media_aritmetica(valori, sigfigs = "numeric")
#'
#' @export
media_aritmetica <- function(values, sigfigs = 2) {
  if (!is.numeric(values)) {
    stop("I valori devono essere numerici")
  }

  if (sum(!is.na(values)) < 6) {
    stop("Servono almeno 6 valori non NA")
  }

  if (
    !((is.character(sigfigs) && sigfigs == "numeric") ||
      (is.numeric(sigfigs) && length(sigfigs) == 1 && sigfigs > 0))
  ) {
    stop("sigfigs deve essere un numero positivo oppure 'numeric'")
  }

  num_vals <- values[!is.na(values)]
  m <- mean(num_vals)

  if (sigfigs == "numeric") {
    m
  } else {
    format_signif(m, sigfigs)
  }
}


#' Intervallo di confidenza della media
#'
#' Calcola l'intervallo di confidenza della media di una serie di risultati
#' numerici, assumendo una distribuzione normale e utilizzando la
#' distribuzione t di Student.
#'
#' L'intervallo è calcolato come:
#' \deqn{
#' \bar{x} \pm t_{(1 + c)/2, \, n-1} \cdot \frac{s}{\sqrt{n}}
#' }
#'
#' @param values Un vettore numerico contenente i risultati di prova.
#'   I valori \code{NA} sono ammessi ma vengono esclusi dal calcolo.
#' @param confidence Livello di confidenza desiderato (default \code{0.95}).
#' @param sigfigs Numero di cifre significative per la formattazione
#'   dei limiti dell'intervallo. Usare \code{"numeric"} per ottenere
#'   i valori numerici non formattati.
#'
#' @return
#' Se \code{sigfigs = "numeric"}, un valore numerico pari all'ampiezza
#' dell'intervallo, \code{t * se}.
#' Altrimenti, una stringa con formato \code{"lower – upper"}.
#'
#' @details
#' Sono richiesti almeno 6 valori numerici non \code{NA}.
#' L'intervallo di confidenza è valido sotto l'assunzione di normalità
#' dei dati o numerosità sufficiente del campione.
#'
#' @examples
#' valori <- c(10.1, 9.9, 10.0, 10.2, 9.8, 10.1)
#'
#' ic_media(valori)
#' ic_media(valori, confidence = 0.90)
#' ic_media(valori, sigfigs = "numeric")
#'
#' @export
ic_media <- function(values, confidence = 0.95, sigfigs = 2) {
  if (!is.numeric(values)) {
    stop("I valori devono essere numerici")
  }

  if (sum(!is.na(values)) < 6) {
    stop("Servono almeno 6 valori non NA")
  }

  if (confidence < 0.8 || confidence > 0.999) {
    stop("Il livello di confidenza deve essere tra 0.8 e 0.999")
  }

  if (
    !((is.character(sigfigs) && sigfigs == "numeric") ||
      (is.numeric(sigfigs) && length(sigfigs) == 1 && sigfigs > 0))
  ) {
    stop("sigfigs deve essere un numero positivo oppure 'numeric'")
  }

  num_vals <- values[!is.na(values)]
  n <- length(num_vals)
  m <- mean(num_vals)
  s <- sd(num_vals)

  alpha <- (1 + confidence) / 2
  t_crit <- qt(alpha, df = n - 1)

  se <- s / sqrt(n)
  ci <- t_crit * se
  lower <- m - ci
  upper <- m + ci

  if (sigfigs == "numeric") {
    ci
  } else {
    paste0(
      format_signif(lower, sigfigs),
      " – ",
      format_signif(upper, sigfigs)
    )
  }
}

# ---- Incertezza ---- #
#' Calcolo automatico dell'incertezza estesa secondo Horwitz–Thompson
#'
#' Questa funzione calcola l'incertezza estesa di un'analisi chimica
#' utilizzando la relazione di Horwitz per concentrazioni superiori a 120 µg/kg
#' e la relazione modificata di Thompson per concentrazioni inferiori a questo valore.
#' Supporta diverse unità di misura, riportando l'incertezza nella stessa unità di input.
#'
#' @param C Numeric. Concentrazione dell'analita. Può essere un singolo valore o un vettore.
#' @param unit Character. Unità di misura della concentrazione. Valori supportati:
#'   \code{"fraction"} (frazione massica, default),
#'   \code{"g/kg"}, \code{"mg/kg"}, \code{"ug/kg"}, \code{"ng/kg"},
#'   \code{"pg/g"}, \code{"ng/g"}, \code{"ug/g"}, \code{"mg/g"}, \code{"g/g"},
#'   \code{"pg/ml"}, \code{"ng/ml"}, \code{"ug/ml"}, \code{"mg/ml"},
#'   \code{"g/l"}, \code{"mg/l"}, \code{"ug/l"}, \code{"ng/l"}, \code{"pg/l"},.
#'   Per le unità liquide si assume densità della soluzione ≈ 1 g/ml.
#' @param k Numeric. Fattore di copertura per l'incertezza estesa (default = 2, circa 95% di confidenza).
#'
#' @return Data frame con colonne:
#' \describe{
#'   \item{C}{Concentrazione in input, nella stessa unità specificata.}
#'   \item{unit}{Unità di misura della concentrazione.}
#'   \item{method}{Metodo utilizzato: \code{"Horwitz"} o \code{"Thompson"}.}
#'   \item{RSD}{Deviazione standard relativa prevista (senza moltiplicare per 100).}
#'   \item{u}{Incertezza standard nella stessa unità della concentrazione.}
#'   \item{U}{Incertezza estesa nella stessa unità della concentrazione.}
#' }
#'
#' @examples
#' # Concentrazione superiore al threshold → Horwitz
#' horwitz_thompson_auto(1, unit = "mg/kg")
#'
#' # Concentrazione molto bassa → Thompson
#' horwitz_thompson_auto(50, unit = "pg/g")
#'
#' # Concentrazione in soluzione acquosa
#' horwitz_thompson_auto(0.5, unit = "ng/mL")
#'
#' # Vettore di concentrazioni
#' horwitz_thompson_auto(c(0.1, 1, 10), unit = "ug/L")
#'
#' @export
horwitz_thompson <- function(C, unit = "fraction", k = 2) {
  if (length(unit) != length(C) && length(unit) != 1) {
    stop("unit deve essere di lunghezza 1 o uguale a C")
  }

  # Vectorize unit se necessario
  if (length(unit) == 1) {
    unit <- rep(unit, length(C))
  }

  # Pulizia dell'unità
  clean_unit <- unit
  clean_unit <- tolower(clean_unit) # minuscolo
  clean_unit <- gsub("\\s+", "", clean_unit) # rimuove spazi
  clean_unit <- gsub("µ", "u", clean_unit) # micro simbolo -> u
  clean_unit <- gsub("μ", "u", clean_unit) # micro mu -> u
  clean_unit <- gsub("^l$", "l", clean_unit) # uniforma L/l
  clean_unit <- gsub("^kg$", "kg", clean_unit)

  # Conversioni in frazione massica/frazioni volumetriche (kg/kg o L/L)
  conv <- c(
    fraction = 1,
    "%" = 1e-2,
    "g/kg" = 1e-3,
    "mg/kg" = 1e-6,
    "ug/kg" = 1e-9,
    "ng/kg" = 1e-12,
    "pg/g" = 1e-12,
    "pg/kg" = 1e-15,
    "ng/g" = 1e-9,
    "ug/g" = 1e-6,
    "mg/g" = 1e-3,
    "g/g" = 1,
    "pg/ml" = 1e-12,
    "ng/ml" = 1e-9,
    "ug/ml" = 1e-6,
    "mg/ml" = 1e-3,
    "g/l" = 1e-3,
    "mg/l" = 1e-6,
    "ug/l" = 1e-9,
    "ng/l" = 1e-12,
    "pg/l" = 1e-15
  )

  # Controllo unità
  if (any(!clean_unit %in% names(conv))) {
    stop("Unità non supportata in input")
  }

  if (any(C <= 0)) {
    stop("La concentrazione C deve essere > 0.")
  }

  C_frac <- C * conv[clean_unit]

  # Threshold per passaggio Horwitz -> Thompson
  threshold <- 1.2e-7

  # Calcolo RSD
  RSD <- ifelse(
    C_frac >= threshold,
    0.02 * C_frac^(-0.15), # Horwitz classico
    0.22 # Thompson modificato
  )

  # Incertezza standard
  u_frac <- RSD * C_frac

  # Incertezza estesa
  U_frac <- k * u_frac

  # Riporta nella stessa unità di input
  u <- u_frac / conv[clean_unit]
  U <- U_frac / conv[clean_unit]

  # Output
  data.frame(
    C = C,
    unit = unit,
    method = ifelse(C_frac >= threshold, "Horwitz", "Thompson"),
    RSD = RSD,
    u = u,
    U = U
  )
}

library(data.table)
library(scales)
library(ggplot2)
# ---- Funzioni di formattazione e reporting ---- #

#' Formatta un numero con un numero fisso di cifre significative
#'
#' Questa funzione formatta un numero (o un vettore numerico) con un numero
#' specificato di cifre significative, restituendo una stringa che mantiene
#' gli zeri finali.
#'
#' @param x Numero o vettore numerico da formattare.
#' @param n Numero intero positivo, numero di cifre significative desiderate.
#' @return Vettore di caratteri con i numeri formattati secondo il numero
#'   di cifre significative, con gli zeri finali preservati.
#' @examples
#' format_signif(0.2002, 3)   # "0.200"
#' format_signif(1234.56, 3)  # "1230"
#' format_signif(c(0.00456, 78.91), 2)  # c("0.0046", "79")
#' @export
format_signif <- function(x, n) {
  if (!is.numeric(x)) {
    stop("x deve essere numerico")
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0) {
    stop("n deve essere un numero intero positivo")
  }

  sapply(x, function(val) {
    if (is.na(val)) {
      return(NA_character_)
    }
    if (val == 0) {
      return(paste0("0.", paste(rep("0", n - 1), collapse = "")))
    }

    rounded <- signif(val, n)

    # numero di cifre intere
    int_digits <- floor(log10(abs(rounded))) + 1

    # decimali necessari per n cifre significative
    decimals <- max(n - int_digits, 0)

    formatC(rounded, format = "f", digits = decimals)
  })
}

#' Verifica di conformità rispetto a requisiti minimi e massimi
#'
#' Verifica riga per riga la conformità di risultati di prova espressi
#' come testo numerico rispetto a requisiti minimi e/o massimi, forniti
#' come colonne di una \code{data.table}.
#'
#' La funzione è progettata per l'uso in contesti di validazione e
#' verifica di metodi e restituisce esclusivamente l'esito logico della
#' conformità, senza produrre output formattati.
#'
#' @param risultato_txt Vettore \code{character} contenente risultati
#'   numerici espressi in formato testuale (es. \code{"10"}, \code{"10.2"}).
#'   Sono accettati esclusivamente numeri in formato decimale con punto.
#' @param requisito_max Vettore numerico contenente il requisito massimo.
#'   Può avere lunghezza 1 o uguale a \code{risultato_txt}.
#' @param requisito_min Vettore numerico contenente il requisito minimo.
#'   Usare \code{NA} se il requisito minimo non è previsto per una riga.
#'
#' @return
#' Un vettore logico contenente:
#' \itemize{
#'   \item \code{TRUE} se il risultato è conforme ai requisiti;
#'   \item \code{FALSE} se non conforme;
#'   \item \code{NA} se il risultato non è un numero testuale valido.
#' }
#'
#' @details
#' La funzione:
#' \itemize{
#'   \item non effettua conversioni automatiche di formato numerico
#'   (ad esempio virgola decimale);
#'   \item considera non validi risultati non esprimibili come numeri
#'   decimali standard;
#'   \item non applica regole decisionali legate all'incertezza di misura.
#' }
#'
#' È pensata per essere utilizzata all'interno di una \code{data.table}
#' per la valutazione riga per riga della conformità.
#'
#' @examples
check_requisito_dt <- function(
  risultato_txt,
  requisito_max,
  requisito_min
) {
  n <- length(risultato_txt)

  if (!is.character(risultato_txt)) {
    stop("risultato_txt deve essere character")
  }

  if (!is.numeric(requisito_max) || !is.numeric(requisito_min)) {
    stop("requisito_max e requisito_min devono essere numerici")
  }

  if (!(length(requisito_max) %in% c(1, n))) {
    stop("requisito_min deve avere lunghezza 1 o uguale a risultato_txt")
  }

  if (!(length(requisito_min) %in% c(1, n))) {
    stop("requisito_min deve avere lunghezza 1 o uguale a risultato_txt")
  }

  # Ricicla se necessario
  requisito_max <- rep_len(requisito_max, n)
  requisito_min <- rep_len(requisito_min, n)

  # Pattern numero decimale pulito
  pattern <- "^-?[0-9]+(\\.[0-9]+)?$"

  valid <- grepl(pattern, risultato_txt)

  out <- rep(NA, n)

  valori <- suppressWarnings(as.numeric(risultato_txt[valid]))

  ok_max <- valori <= requisito_max[valid]
  ok_min <- ifelse(
    is.na(requisito_min[valid]),
    TRUE,
    valori >= requisito_min[valid]
  )

  out[valid] <- ok_max & ok_min

  out
}

#' Formattazione del risultato di prova rispetto ai requisiti
#'
#' Genera una stringa descrittiva del risultato di prova a partire da
#' uno o due requisiti, dal valore misurato e dall'unità di misura.
#' La funzione è pensata per l'uso in report di validazione e restituisce
#' un testo pronto per la presentazione dei risultati.
#'
#' Il comportamento dipende dalla presenza dei requisiti:
#' \itemize{
#'   \item se sono presenti \code{req1} e \code{req2}, il requisito è
#'   espresso come intervallo (\code{req1 - req2});
#'   \item se è presente solo \code{req1}, il requisito è espresso come
#'   limite singolo;
#'   \item se non sono presenti requisiti, viene riportato solo il
#'   risultato misurato.
#' }
#'
#' L'esito della prova (conforme / non conforme) è riportato tramite
#' simboli grafici (tick verde o croce rossa) ed è fornito come input,
#' senza essere ricalcolato dalla funzione.
#'
#' @param req1 Requisito inferiore o requisito singolo. Valore numerico
#'   oppure \code{NA} se non applicabile.
#' @param req2 Requisito superiore. Valore numerico oppure \code{NA}
#'   se non applicabile.
#' @param value Valore misurato della prova.
#' @param udm Unità di misura associata ai requisiti e al risultato
#'   (ad esempio \code{"mg/L"}, \code{"%"}).
#' @param esito Esito della prova. Valore logico:
#'   \code{TRUE} per prova conforme, \code{FALSE} per prova non conforme.
#'
#' @return
#' Una stringa di testo contenente la descrizione del requisito,
#' il risultato misurato e, se applicabile, l'esito della prova.
#'
#' @details
#' La funzione non effettua alcuna valutazione di conformità e non
#' applica criteri decisionali. L'esito deve essere determinato
#' separatamente secondo le regole definite nel piano di validazione
#' o nella procedura di prova.
#'
#' È progettata per essere utilizzata riga per riga, ad esempio tramite
#' \code{mapply()} o all'interno di una \code{data.table}.
#'
#' @examples
#' format_risultato(
#'   req1 = 10,
#'   req2 = 20,
#'   value = 15,
#'   udm = "mg/L",
#'   esito = TRUE
#' )
#'
#' format_risultato(
#'   req1 = 5,
#'   req2 = NA,
#'   value = 4.8,
#'   udm = "mg/L",
#'   esito = FALSE
#' )
#'
#' format_risultato(
#'   req1 = NA,
#'   req2 = NA,
#'   value = 7.2,
#'   udm = "mg/L",
#'   esito = NA
#' )
#'
#' @seealso
#' \code{\link[data.table]{data.table}}
#'
#' @export
format_risultato <- function(req1, req2, value, udm, esito) {
  tick_ok <- "\u2705" # ✅
  tick_no <- "\u274C" # ❌

  # Aggiungi spazio tra valore e unità di misura se non è %
  udm_txt <- ifelse(udm == "%", udm, paste0(" ", udm))

  value_txt <- format_signif(as.numeric(value), 2)

  simbolo_esito <- if (isTRUE(esito)) tick_ok else tick_no

  # Caso 1: requisito 1 e 2 presenti
  if (!is.na(req1) && !is.na(req2)) {
    paste0(
      "requisito: ",
      req1,
      " - ",
      req2,
      udm_txt,
      "; ",
      "risultato: ",
      value_txt,
      udm_txt,
      "; ",
      "esito: ",
      simbolo_esito
    )

    # Caso 2: solo requisito 1 presente
  } else if (is.na(req1) && !is.na(req2)) {
    paste0(
      "requisito: ≤",
      req2,
      udm_txt,
      "; ",
      "risultato: ",
      value_txt,
      udm_txt,
      "; ",
      "esito: ",
      simbolo_esito
    )

    # Caso 3: nessun requisito
  } else {
    paste0(
      "risultato: ",
      value_txt,
      udm_txt
    )
  }
}

#' Capitalizza la prima lettera di una stringa
#'
#' Rende maiuscola solo la prima lettera di una stringa, lasciando il resto invariato.
#' Funziona perfettamente con stringhe già in minuscolo.
#'
#' @param x \code{character(1)} La stringa da capitalizzare
#'
#' @return \code{character(1)} La stringa con la prima lettera maiuscola
#'
#' @examples
#' capitalizza("mappa")         # "Mappa"
#' capitalizza("test")          # "Test"
#' capitalizza("r è figo")      # "R è figo"
#' capitalizza("xyz")           # "Xyz"
#'
#' @export
capitalizza <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' Grafico di dispersione con bande robuste e valori di riferimento
#'
#' Questa funzione genera un grafico di dispersione dei valori misurati per un dato
#' livello di concentrazione, evidenziando:
#' 
#' - I **punti sperimentali** con jitter per evitare sovrapposizioni;
#' - Il **valore medio** per ciascun livello (diamante);
#' - Il **valore atteso o di riferimento** (linea tratteggiata);
#' - **Intervalli di copertura robusti** calcolati come k·MAD dei valori misurati.
#' 
#' La funzione è utile in contesti di **validazione di metodi analitici** (es. report ISO 17025),
#' poiché mostra sia il confronto tra valori misurati e attesi, sia la dispersione robusta dei dati.
#'
#' @param dt `data.table` contenente i dati sperimentali. Deve includere almeno le colonne
#'   indicate in `livello`, `valore` e `udm`.
#' @param livello `character` nome della colonna di `dt` che contiene i livelli di concentrazione attesi.
#' @param valore `character` nome della colonna di `dt` con i valori misurati.
#' @param udm `character` nome della colonna di `dt` che specifica l'unità di misura.
#' @param k `numeric` vettore di uno o due valori di moltiplicatore per il MAD (es. c(2,3)).
#'   Determina la larghezza delle bande robuste. L'ordine dei livelli influisce sullo spessore
#'   delle linee: livelli maggiori hanno linee più sottili per una migliore visibilità.
#'
#' @return Un oggetto `ggplot` contenente:
#' - Punti sperimentali jitterati;
#' - Diamante per il valore medio di ciascun livello;
#' - Linea tratteggiata per il valore atteso;
#' - Bande robuste colorate secondo i valori di `k`.
#'
#' @details
#' - Le bande robuste sono calcolate come `banda_robusta(valore, k)`, funzione robusta basata su MAD.
#' - La legenda distingue le bande (k·MAD), la media e il valore di riferimento.
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   Livello = rep(c(0, 1, 2), each = 5),
#'   Valore = c(0.1, 0.2, 0.15, 0.18, 0.12, 1.0, 1.1, 0.95, 1.05, 1.02, 2.0, 1.9, 2.1, 2.05, 1.98),
#'   UDM = rep("mg/L", 15)
#' )
#' plot_dispersione(dt, livello = "Livello", valore = "Valore", udm = "UDM")
#' }
#' 
#' @export
plot_dispersione <- function(
  dt,
  livello,
  valore,
  udm,
  k = c(2, 3)
) {
  ...
}

plot_dispersione <- function(
  dt,
  livello,
  valore,
  udm,
  k = c(2, 3)
) {

  if (!data.table::is.data.table(dt)) {
    stop("`dt` deve essere un `data.table`")
  }

  if (!all(c(livello, valore, udm) %in% names(dt))) {
    stop("`livello`, `valore` e `udm` devono essere colonne presenti in `dt`")
  }

  if (length(k) > 2) {
    stop("possono essere gestiti solo due livelli di `k · MAD`")
  }

  # riassunti per livello
  riass <- dt[, .(
    media = mean(valore_col, na.rm = TRUE),
    udm = unique(udm_col, na.rm = TRUE)
  ),
  by = .(atteso = livello_col,
         livello = factor(livello_col)),
  env = list(
    valore_col = valore,
    livello_col = livello,
    udm_col = udm
  )
  ]

  udm <- unique(riass$udm) # unità di misura per il grafico
  if (length(udm) != 1) {
    stop("i livelli devono condividere la stessa unità di misura")
  }

  # bande robuste
  k_order <- k[order(k)]
  bande <- lapply(k_order, function(x) {
    dt[, as.data.frame(banda_robusta(valore_col, k = x, na.rm = TRUE)),
      by = .(livello_col),
    env = list(
      valore_col = valore,
      livello_col = livello
    )]
  }) |> rbindlist()

 bande[, livello_col := factor(livello_col), 
  env = list(livello_col = livello)]
 bande[, k := factor(k, levels = sort(unique(k)))]
 # spessore delle linee inversamente proporzionale a k
 bande[, linewidth := (nlevels(k) - as.integer(k) + 1) * 3]
 bande <- bande[order(-k)]

 # colori 
 accent <- "#0f4c81"
 accent_2 <- scales::col_lighter(accent, 50)
 accent_1 <- scales::col_lighter(accent, 20)
  
 k_levels <- levels(bande$k)

 col_map <- setNames(
  c(accent_1, accent_2)[seq_along(k_levels)],
  k_levels
 ) 

  # costruzione grafico
  ggplot2::ggplot(dt, 
    ggplot2::aes(
      x = factor(.data[[livello]]), 
      y = .data[[valore]]
    )
  ) +

    # bande robuste
    ggplot2::geom_linerange(
      data = bande, # riordino per overlay
      ggplot2::aes(
        x = .data[[livello]],
        ymin = lower,
        ymax = upper,
        col = k
      ),
      linewidth = bande$linewidth,
      inherit.aes = FALSE
    ) +

    # punti sperimentali
    ggplot2::geom_jitter(
      width = 0.05,
      height = 0,
      shape = 16,
      size = 3
    ) +

    # media per livello
    ggplot2::geom_point(
      data = riass,
      ggplot2::aes(
        x = .data[[livello]],
        y = media,
        shape = "Media",
      ),
      fill = "#f4f4f4",
      col = accent,
      stroke = 0.6,
      size = 5
    ) +

    # valore atteso
    ggplot2::geom_segment(
      data = riass,
      ggplot2::aes(
        x = as.numeric(.data[[livello]]) - 0.2,
        xend = as.numeric(.data[[livello]]) + 0.2,
        y = atteso,
        yend = atteso,
        linetype = "Valore di riferimento"
      ),
      colour = "#222222",
      linewidth = 0.8
    ) +

    ggplot2::labs(
      x = paste0("Livello di concentrazione atteso (", udm, ")"),
      y = paste0("Concentrazione misurata (", udm, ")"),
      col = "k · MAD"
    ) +
    
    ggplot2::scale_colour_manual(values = col_map) +
    ggplot2::scale_shape_manual(
      name = "",
      values = c("Media" = 23)
    ) +
    ggplot2::scale_linetype_manual(
      name = "",
      values = c("Valore di riferimento" = "dashed"),
      labels = "Valore di\nriferimento"
    ) +

    ggplot2::theme_minimal(base_size = 14) +

    ggplot2::guides(linewidth = "none")
}

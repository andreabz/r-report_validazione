# Report di validazione ISO/IEC 17025 – esempio strutturato

[![Test and Publish](https://github.com/andreabz/r-report_validazione/actions/workflows/deploy.yml/badge.svg)](
https://github.com/andreabz/r-report_validazione/actions/workflows/deploy.yml
)
![renv](https://img.shields.io/badge/R%20deps-renv-blue)

Questo repository mostra un possibile approccio alla **strutturazione del materiale di lavoro**
che porta alla redazione di un rapporto di validazione di un metodo analitico in ambito ISO/IEC 17025.

L'obiettivo non è fornire un metodo operativo, ma **mostrare come organizzare requisiti,
pianificazione, dati e calcoli in modo coerente con il processo reale di validazione**.

👉 **Report pubblicato:**  
https://andreabz.github.io/r-report_validazione/

---

## Obiettivo del repository

- separare concettualmente **requisiti**, **piano prove** e **dati sperimentali**
- rendere i calcoli **riproducibili e rieseguibili**
- facilitare l'aggiornamento del report al variare di:
  - requisiti normativi
  - matrici analizzate
  - dati sperimentali
- fornire un esempio didattico di report generato in modo dinamico

Il report finale non è una fotografia statica, ma il **risultato riproducibile di un processo**.

---

## Cosa questo repository NON è

- ❌ non è un metodo ufficiale
- ❌ non è utilizzabile a fini operativi o di accreditamento
- ❌ non contiene dati reali di laboratorio
- ❌ non sostituisce procedure, istruzioni operative o metodi normati

I dati presenti sono **verosimili ma non reali** e hanno esclusivamente finalità dimostrative.

---

## Struttura del repository

La struttura del repository è pensata per separare chiaramente
codice, dati, contenuti testuali e configurazione del report.

```text
R/
├─ utils.R                 # Funzioni R per calcoli, decisioni e formattazione

data/
├─ condizioni.csv          # Condizioni sperimentali e livelli di prova
├─ requisiti.csv           # Requisiti di accettabilità
├─ risultati.csv           # Dati sperimentali (verosimili, meglio se presi da LIMS)

_includes/
├─ terreno/
│  ├─ parametri_prestazionali.qmd  # Descrizione parametri e requisiti (terreni)
│  └─ piano_prove.qmd              # Piano sperimentale per i terreni
├─ sedimento/
│  ├─ parametri_prestazionali.qmd  # Descrizione parametri e requisiti (sedimenti)
│  └─ piano_prove.qmd              # Piano sperimentale per i sedimenti
├─ news.qmd                # Storico delle modifiche al metodo
├─ riferimenti.qmd         # Riferimenti normativi e bibliografici
├─ sommario.qmd            # Sommario del report

tests/
├─ testthat/               # Test unitari delle funzioni R

www/
├─ report_validazione.css  # Stili grafici del report

report_validazione.qmd     # Documento principale
```

---

## Principi di organizzazione

Il repository è strutturato per riflettere il **processo reale di validazione**:

1. **Requisiti**
   - definiti prima delle prove
   - stabili nel tempo
   - indipendenti dai dati sperimentali

2. **Pianificazione**
   - definizione di matrici, livelli, repliche
   - scelta delle prove necessarie a verificare i requisiti

3. **Dati sperimentali**
   - risultati delle misure
   - unici elementi che cambiano a ogni iterazione

Queste informazioni nascono in momenti diversi e vengono quindi mantenute in **file distinti**.

---

## Riproducibilità

- L'ambiente R è gestito tramite `renv`
- Tutte le elaborazioni sono rieseguite automaticamente a ogni render del report
- Le funzioni di calcolo sono testate formalmente tramite `testthat`

Modificando:
- un file di requisiti,
- un piano di prova,
- o aggiungendo una nuova matrice,

il report si aggiorna in modo coerente senza riscrivere manualmente il documento.

Questo approccio rende esplicito il legame tra decisioni prese a monte, 
dati sperimentali e risultati riportati.

---

## Funzioni di calcolo e test

Le funzioni di calcolo utilizzate nel report sono verificate tramite test unitari formali (`testthat`).

L'obiettivo dei test non è solo individuare errori di codice, ma rendere esplicite le regole statistiche e 
operative che stanno alla base del report di validazione.

I test sono strutturati secondo tre livelli principali.

### 1. Correttezza matematica

Ogni funzione viene verificata confrontando i risultati con:

- formule statistiche esplicite (es. ripetibilità, intervalli di confidenza),
- valori di riferimento tratti dalla letteratura o da norme tecniche,
- casi limite noti.

Questo garantisce che i calcoli implementati corrispondano ai metodi teorici dichiarati.

### 2. Controllo degli input

I test verificano che le funzioni:

- accettino solo dati coerenti con i requisiti del metodo (numero minimo di repliche, 
tipo di dato, range dei parametri),
- intercettino condizioni non valide (deviazione standard nulla, concentrazioni non fisicamente ammissibili,
parametri fuori range),
- restituiscano errori espliciti e informativi.

In questo modo, le regole di applicabilità dei parametri prestazionali non restano implicite, 
ma diventano parte del codice verificato.

### 3. Coerenza dell'output per il reporting

Poiché il report finale simula un documento formale, vengono testati anche:

- la struttura degli output (liste, data.frame, campi attesi),
- i messaggi testuali di interpretazione,
- la formattazione dei risultati e degli esiti di conformità.

Questo assicura che il passaggio da calcolo → interpretazione → report sia stabile e riproducibile.

### Integrazione con la CI

I test vengono eseguiti automaticamente tramite GitHub Actions a ogni `push` sul branch principale.

Il report viene pubblicato solo se:

- l'ambiente R è correttamente ripristinato tramite `renv`,
- tutti i test risultano superati.

In questo modo, il documento pubblicato rappresenta sempre il risultato coerente
di un processo verificato.

## Generazione del report

Il [report finale](https://andreabz.github.io/r-report_validazione/) viene generato con **Quarto** a partire da:

- file di testo descrittivi (`.qmd`)
- dati strutturati (`.csv`)
- funzioni R riutilizzabili

Le sezioni di risultato sono create dinamicamente tramite cicli e `knit_child()`.

---

## Licenza

Questo repository è rilasciato con licenza GPL-3 ed è utilizzabile liberamente
a fini didattici e formativi.
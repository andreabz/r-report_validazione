# Report di validazione ISO/IEC 17025 – esempio strutturato

[![Test and Publish](https://github.com/andreabz/r-report_validazione/actions/workflows/deploy.yml/badge.svg)](https://github.com/andreabz/r-report_validazione/actions/workflows/deploy.yml)
![renv](https://img.shields.io/badge/R%20deps-renv-blue)

Questo repository esplora un possibile modo di **organizzare e rendere riproducibile il materiale di lavoro** che porta alla redazione di un rapporto di validazione di un metodo analitico, in un contesto orientato ai principi della **ISO/IEC 17025**.

L'obiettivo non è proporre un metodo ufficiale o una procedura di accreditamento, ma mostrare come collegare in modo esplicito **requisiti, pianificazione sperimentale, dati, calcoli e reporting**.

**[Report pubblicato](https://andreabz.github.io/r-report_validazione/)**

---

## Il problema

Un rapporto di validazione non è soltanto un documento finale. È il risultato di una sequenza di decisioni:

1. quali prestazioni del metodo devono essere verificate;
2. quali requisiti di accettabilità vengono adottati;
3. come vengono pianificate le prove;
4. quali dati vengono raccolti;
5. quali calcoli e regole decisionali vengono applicati;
6. come i risultati vengono documentati.

In questo progetto queste componenti sono mantenute distinte, ma collegate attraverso codice e dati strutturati.

L'idea centrale è semplice: **il report dovrebbe essere una conseguenza riproducibile del processo di validazione, non una trascrizione manuale dei suoi risultati**.

---

## Cosa contiene

Il repository separa principalmente:

- **requisiti** e criteri di accettabilità;
- **piano delle prove** e condizioni sperimentali;
- **dati sperimentali**;
- funzioni R per **calcoli, decisioni e formattazione**;
- contenuti testuali e riferimenti;
- test automatici delle funzioni;
- documento Quarto che assembla il report finale.

Una struttura semplificata è:

```text
R/
├─ utils.R

data/
├─ condizioni.csv
├─ requisiti.csv
├─ risultati.csv

_includes/
├─ terreno/
├─ sedimento/
├─ news.qmd
├─ riferimenti.qmd
└─ sommario.qmd

tests/
└─ testthat/

www/
└─ report_validazione.css

report_validazione.qmd
```

La separazione dei contenuti permette, per esempio, di modificare requisiti o dati sperimentali senza dover riscrivere manualmente il report.

---

## Riproducibilità e controllo dei calcoli

L'ambiente R è gestito con `renv`, mentre i calcoli sono implementati in funzioni riutilizzabili e verificati con `testthat`.

I test coprono tre aspetti principali:

### Correttezza matematica

I risultati vengono confrontati con formule statistiche esplicite, valori di riferimento e casi limite.

### Validità degli input

Vengono controllate condizioni come numero di repliche, tipologia e intervallo dei dati e parametri non ammissibili. Le condizioni che rendono un calcolo non applicabile vengono quindi esplicitate nel codice.

### Coerenza del reporting

Vengono verificati anche struttura degli output, messaggi interpretativi e informazioni utilizzate per costruire il report.

Questo rende verificabile non solo il calcolo, ma anche il passaggio:

**dati → calcolo → interpretazione → documento**

---

## Continuous integration

I test vengono eseguiti automaticamente tramite **GitHub Actions**.

Il workflow:

1. ripristina l'ambiente R tramite `renv`;
2. esegue i test;
3. genera il report;
4. pubblica il risultato solo quando il processo di verifica è completato con successo.

La continuous integration viene quindi utilizzata come controllo del processo di generazione del documento, non soltanto come controllo del codice.

---

## Generazione del report

Il report viene generato con **Quarto** a partire da:

- file `.qmd` per i contenuti descrittivi;
- file `.csv` per dati e requisiti;
- funzioni R per i calcoli;
- componenti riutilizzabili per le diverse matrici e sezioni.

Le sezioni di risultato vengono costruite dinamicamente, mantenendo separati contenuto, dati e logica di elaborazione.

**[Apri il report](https://andreabz.github.io/r-report_validazione/)**

---

## Cosa questo progetto non è

Questo repository è un **esempio dimostrativo**.

- Non è un metodo analitico ufficiale.
- Non è destinato all'uso operativo o all'accreditamento.
- Non contiene dati reali di laboratorio.
- Non sostituisce metodi normati, procedure o istruzioni operative.

I dati utilizzati sono verosimili ma non reali e hanno esclusivamente finalità dimostrative.

---

## Perché questo progetto

Il punto di interesse non è soltanto automatizzare la produzione di un documento.

È esplorare come **qualità, statistica e riproducibilità** possano essere incorporate nella struttura stessa del lavoro di validazione: requisiti espliciti, dati separati dalla logica, calcoli verificabili e un report che possa essere rigenerato a partire dalle sue fonti.

---

## Licenza

Questo repository è rilasciato con licenza GPL-3 ed è utilizzabile liberamente a fini didattici e formativi.

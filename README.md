# IDA_case_study
Repository for an examplary case study about car manufacturing supply chains as a final project for the course Introduction to Data Analysis with R from the TU Berlin.

# Setup

## Daten
Die Daten finden sich in der [TUB Cloud](https://tubcloud.tu-berlin.de/s/QY3zTYfpam2s5nn)

Die Rohdaten können in einen Ordner "project/Data/" abgelegt werden. Der ist in der ".gitignore" Datei vermerkt um nicht synchronisiert zu werden.
Bitte nicht die Rohdaten in GIT hinzufügen und erst recht nicht pushen. Am besten die Daten direkt aus dem Git Ordner raus lassen.

# Dateistrukturen
  tbd
 
 
# TODO
- [ ] Jeder schaut sich seine Daten an, wie sie struktutriert sind, sie importieren und in einen ordentlichen DF laden

## Case Study
- [ ] relavante Datensätze importieren und mit tidy data aufbereiten und als Dataframe ablegen 
  - [ ] Dataframes aus quelldateien erzeugen
  - [ ] Aus diesen dataframes interessante Daten isolieren
  - [ ] diese Daten aufräumen
  - [ ] dann mergen in einen Dataframe
  - [ ] Dafür Schaubild erstellen und erklären welche Daten wir warum brauchen
- ## Shiny App
    - Alle module mit if_blabla bla install installieren
    - [ ] Deutschlandkarte mit:
      - [ ] Gemeinden als Voronoy Diagram
      - [ ] Werken als Punktmakierungen
      - [ ] Popups wenn man auf Gemeinden klickt
        - Überschrift: Gemeindename
        
            Tabelle: Fahrzeug_id, Zulassungsdatum, Defekten Komponenten, defekten Einzelteile

            Bei Klick auf Beuteil ID soll Kuftlinie des Wegs des Bauteils vom BT Hersteller, zum K hersteller, zur OEM, zur Gemeinde


## Inhaltlich
- Erst alle Lederseite von uns rausfinden, von da sowohl auf Fahrzeuge schließen um am Ende Halter zu informieren, als auch auf Einzelteile schließen um defekte Einzelteile und damit die verknüpften Fahrzeuge zu markieren.

### Datensätze

Beuteil IDs bestehen aus A-B-C-D mit
  - A als Teilenummer
  - B als Herstellernummer
  - C als Werksnummer
  - D als Laufindex

Jedes Fahrzeug besteht aus 4 Komponenten:

Motor ist K1,

**Sitze ist K2**,

Schaltung K3, 

Karosserie K4

Jede Komponente besteht aus mehreren Teilen (T1-T40)

Für uns also interessant: 

| Ordner | Bedeutung | Zuständig |
|--------|-----------|-----------|
| Einzelteil | Welches Einzelteil von welchem Hersteller in welchem Werk wann hergestellt wurde | Dejan |
| Fahrzeug | .... | Lin |
| Geodaten | .... | Jan |
| Komponenten | ... | Julia |
| Logistikverzug | nur relevant für allg. Aufgabe | |
| Zulassungen | ... | Max |

**Ziel**: Zulassung -> Fahrzeugnummer -> Sitze -> Einzelteile -> Werke mit Standort



# Fragen
- [x] Was bedeutet "finaler Datensatz"?  :
  - rdata Datei erzeugen durch in rStudio auf env klicken und speichern klicken, dann landen alle Datensätze im Speicher in eine rdata Datei geschrieben. Da kann man auch selektieren
- [x] "Schadensschwerpunkte"? Ist hier wichtig welcher Schaden oder nur ob überhaupt?
  - A: Nur defekte Ledersitze
- [ ] Sind Fahrzeugdefekte redundant weil eh nur von defekten Komponenten abhängig?


# Aufteilung

## Max

## Lin

## Jan
CSS einbinden für RMarkdown
[SRF Projekt](https://github.com/srfdata/2019-06-party-history)

## Dejan

## Julia


# Fortgeschrittene Datenanalysen mit R

In diesem Repository finden sich die Code Snippets und R Markdown-Dateien für die durchgeführte Analyse (siehe [Supplementory Information](SupplementaryInformation.pdf))und den [Leistungsnachweis](Leistungsnachweis_R.pdf) (Rmd/Papaja). Folgende Schritte wuden dabei durchgeführt:

- Aufbereitung der Daten
- Visuelle Inspektion und Ausschluss von Probanden
- Bayesian multi level analysis (brms)

Die Ergebnisse wurden in einem Bericht zusammengefassst und als [Leistungsnachweis](Leistungsnachweis_R.pdf) des Kurses eingereicht.

## Der Code auf Github:

- [Projekt-Datei](Project.Rmd) umfasst die Aufbereitung der Daten und alle Analysen sowie Kommentare zu den Daten. Diese ist die Grundlage für die Supplementory Information. 
### Skripte
- Funktion für die Berechnung der Stan-Modelle: [getBRMModel](Scripts/getBRMModel.R)
-  Berechnung der Verhaltensmodelle: [Verhaltens - Modelle](Scripts/brms_b.R)
-  Berechnung der Reaktionszeitmodelle: [Reaktionszeiten - Modelle](Scripts/brms_RT.R)
-  Berechnung der Herzfrequenzmodelle: [Herzfrequenz - Modelle](Scripts/brms_RRi.R)

Alle Rechte vorbehalten. 





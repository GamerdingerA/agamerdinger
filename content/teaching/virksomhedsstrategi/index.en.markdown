---
title: Virksomhedsstrategi i et Netværksperspektiv
author: Alexander Gamerdinger
date: '2022-02-14'
slug: []
categories: []
tags: []
description: ''
summary: "Copenhagen Business School | HA i Europæisk Business | 6th Semester"
---
![Abstract purple artwork](course-picture.jpg "Photo by [Sander Weeteling](https://unsplash.com/@sanderweeteling) on [Unsplash](https://unsplash.com/)")

### Kursets formål 
Kurset er et obligatorisk fag for HA i europæisk business på [Copenhagen Business School](https://cbscanvas.instructure.com/courses/22821/modules/items/480509). Kurset introducerer til virksomhedsstrategi i et netværksperspektiv. Virksomheder og deres medarbejdere er afhængige af deres omverden og en helt central måde er at knytte an til omverdenen gennem netværk af forskellig art. Virksomheder konkurrerer og samarbejder med andre organisationer via netværk, som bl.a. giver dem adgang til ressourcer, som de er afhængige af. Kurset udstyrer de studerende med redskaber fra netværksanalyse til at forstå hvordan interne såvel som eksterne netværk indvirker på
virksomhedens strategiske processer. Kurset er inddelt i tre dele. 

1. Første del giver en introduktion til forskellige teorier om ’virksomhedsstrategi i netværksperspektiv’ og eksemplificerer teorierne gennem brug af cases. 
2. Anden del indfører de studerende i metodiske redskaber fra netværksanalysen.
3. Tredje del har fokus på anvendelse af netværksanalyse på konkrete cases og giver de studerende mulighed for at lave en konkret netværksanalyse af et intra- eller inter-organisatorisk netværk. 

Forbindelsen mellem teori og metode i konkrete caseanalyser har prioritet ligesom de praktisk-strategiske implikationer af virksomhedsnetværk løbende vil blive diskuteret gennem kurset.

### Office hours og kontakt
- Alexander Gamerdinger
- E-mail: <aga.ioa@cbs.dk>
- Office hours: Fredag kl. 09:00-10:20 (efter aftale)
- Office: Kilen 3. sal (K3.42)

<!-- Calendly link widget begin -->
<link href="https://assets.calendly.com/assets/external/widget.css" rel="stylesheet">
<script src="https://assets.calendly.com/assets/external/widget.js" type="text/javascript" async></script>
<a href="" onclick="Calendly.initPopupWidget({url: 'https://calendly.com/aga-ioa/30min'});return false;">Book meeting here</a>
<!-- Calendly link widget end -->

---

### Holdundervisning

{{< alert >}}
Alt undervisning er on campus!
{{< /alert >}}

#### Vigtige informationer
- [Se syllabus her](syllabus_2022_1.pdf)
- [Guide: Installing R and Rstuido](setup-r-v01.pdf)
- [Guide: R Resources](setup-r-v01.pdf)

#### Troubleshooting
- Hvordan sletter jeg R og Rstudio rigtigt? [For macOS bruger](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Uninstalling-under-macOS) [For Windows bruger](https://stackoverflow.com/questions/55204017/how-to-uninstall-r-and-rstudio-with-all-packages-settings-and-everything-else)
- Jeg har problemer med at installere en pakke [For macOS bruger](https://cbscanvas.instructure.com/courses/22821/discussion_topics/86081)
- Hvis jeg plotter netværk, åbner Rstduio et nyt vindue. Svar: Det er fordi du har en gammel version af Rstudio. Opdater Rstudio.

Hvis der opstår flere problemer, stil et nyt spørgsmål [her](https://cbscanvas.instructure.com/courses/22821/discussion_topics).

---

### Uge 7: Intro til netværksanalyse og visualisering 
I uge 7, lærer I:
1. hvordan man installerer R og Rstudio
2. hvordan man subsetter datasæt med hjælp af `data.table` og/eller `tidyverse` funktioner
3. hvordan man visualiserer simple netværk med `ggraph` og `igraph` funktioner

#### Readings
- [Ellersgaard & Larsen. (2015). The Danish Elite Network](lektion01/Ellersgaard_Larsen_2015.pdf)
- [Larsen, Ellersgaard & Bernsen (2015). Magteliten: hvordan 423 danskere styrer landet](lektion01/Larsen_Ellersgaard_Bernsen_2015.pdf)
- [Larsen, Ellersgaard & Steinitz (2016). Magtens Atlas: Et kort over netværk i Danmark](lektion01/Steinitz_Ellersgaard_Larsen_2016.pdf)

#### Materiale (online)
- videoerne er desværre for store, og skal derfor ses på [Canvas](https://cbscanvas.instructure.com/courses/22821/modules)
- [lektion01-online-rscript](lektion01/lektion01-v01-virkstrat.R)

#### Materiale (on campus)
- [lektion01-powerpoint](lektion01/lektion01-powerpoint.pptx) eller [lektion01-pdf](lektion01/lektion01-powerpoint.pdf)
- [lektion01-øvelse-rscript](lektion01/lektion01-øvelse.R)
- [lektion01-data](den17-no-nordic-letters.csv)

#### Mine R scripts 
- [lektion01-rscript-Alexander](lektion01/lektion01-rscript_Alexander.R)

---

{{< alert >}}
**To do!** Prøv og svar på spørgsmål, som I kan se i filen [lektion01-øvelse-rscript](lektion01/lektion01-øvelse.R)
{{< /alert >}}


### Uge 8: Sammenhængskraft i netværk
I uge 8 lærer I: 
1. hvordan man udvælger komponenter af et netværk med `igraph/tidyverse`funktioner
2. om forskellige måder at måle sammenhængskraft i netværk (densitet, stilængder, diameter)
3. hvordan man kan visualisere det på en individ- og organisationsniveau med `ggraph` funktioner

#### Readings 
- Kap. 3 side 51-59. Lasse Folke Henriksen, & Christian Waldstrøm Netværksanalyse - en
introduktion (2016, København: Samfundslitteratur) 

#### R resourcer
- [Data manipulation med tidyverse](https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html)
- [How to search for help online](lektion02/searching-for-help-guide.pdf)

#### Materiale (online)
- videoerne vises på [Canvas](https://cbscanvas.instructure.com/courses/22821/modules)
- [video om materiale vi har ikke nået til](https://cbs.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=2eb3b8e5-d6ee-4ebe-9af0-ae4500ee7b4e&start=0)

#### Materiale (on campus)
- [lektion02-øvelse-rscript](lektion02/lektion02-without-code_v2.R)
- [lektion02-powerpoint](lektion02/lektion02-powerpoint.pptx) eller [lektion02-pdf](lektion02/lektion02-powerpoint.pdf)

#### Mine R scripts 
- [lektion02-rscript-Alexander](lektion02/lektion02-code-and-comments.R)
- [lektion01-øvelse-rscript-solutions](lektion01/lektion01-øvesle-solutions.R)

---

{{< alert >}}
**Feedback** Jeg har lavet en Feedback Discussion på [Canvas](https://cbscanvas.instructure.com/courses/22821/discussion_topics/86820). I må gerne give en kommentar om hvordan holdundervisning kan forbedres. Hvis det nuværende system ikke fungerer for jer, så må i endelig skrive. 
{{< /alert >}}

### Uge 9: Centralitetsanalyse 
I uge 9 lærer I: 
1. hvordan man beregner degree (grad), betweenness, closeness (nærhed) og eigenvector centralitet med `igraph` funktioner 
2. hvordan man kan visualisere centrale aktører i et netværk

#### Readings 
- Kap. 3 side 70-76. Lasse Folke Henriksen, & Christian Waldstrøm Netværksanalyse - en
introduktion (2016, København: Samfundslitteratur) 
- [Seidman. (1983). Network structure and minimum degree](lektion03/Seidman_1983.pdf)
- [Freeman. (1979). Centrality in social networks: Conceptual clarification](lektion03/Freeman_1979.pdf)

#### Materiale (online)
- videoerne vises på [Canvas](https://cbscanvas.instructure.com/courses/22821/modules)

#### Materiale (on campus)
- [lektion03-rscript-Alexander](lektion03/lektion03-rscript-Alexander.R)











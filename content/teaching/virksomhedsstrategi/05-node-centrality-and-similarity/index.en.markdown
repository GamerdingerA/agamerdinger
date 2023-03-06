---
title: "Session 5 - Brokerage and assortativity"
author: Alexander Gamerdinger
date: '2023-01-01'
slug: /brokerage-and-assortativity/
categories:
  - R
  - Teaching
tags:
description: 'Session on brokerage and assortativity'
summary: 'This session will focus on two topics. First, we will look at Burts constraint as a measure for brokerage. Second, we will cover assortativity as a measure of homophily in a network.'
---

## Session 5 - Brokerage and assortativity

This session connects to session 4 which introduced analysis tools for the node level. Here, we further investigate the broker role with Burt's constraint as a measure for brokerage. Second, we cover assortativity which measures homophily among nodes - or in other words, the likelihood that nodes with similar properties are connected with each other.

But first, after having set working directory and loaded packages, we load our data set, create a graph object and select the biggest component.




```r
# load data 
den <- read_csv("input/den17-no-nordic-letters.csv")

# subset of corporations that have a valid cvr affiliation
den1 <- 
  den %>% filter(sector %in% "Corporations")

# Let us create a graph using an incidence matrix
# Create the incidence matrix
incidence <- xtabs(formula = ~ name + affiliation, 
                   data = den1, 
                   sparse = TRUE)

# adjacency matrix for chosing corporations
adj_c <- Matrix::t(incidence) %*% incidence

# one-mode graph for corporations
gr <- graph_from_adjacency_matrix(adj_c, mode = "undirected") %>% 
  simplify(remove.multiple = TRUE, remove.loops = TRUE)

# What are the components?
complist <- components(gr)

# Decompose graph
comps <- decompose.graph(gr)

# Create an index
index <- 
  table(complist$membership) %>% 
  as_tibble(.name_repair = make.names) %>% 
  arrange(desc(n)) %>% 
  mutate(X = as.numeric(X)) %>% 
  pull(1)

# Select the largest component
comp1 <- comps[[index[1]]] 
```

## 5.1 Burt's constraint

Burt's constraint is a measure of brokerage. The idea of brokerage suggests that nodes who occupy structural holes in a network have greater access to resources and can exert more influence over other nodes who they are connected to. Higher values of Burt's constraint signify that nodes have less structural opportunities for bridging structural holes in the network. It measures how many direct ties of a node are redundant - the more nodes are redundant, the more constraint a node is.


```r
# constraint - the higher, the less chances for brokerage
constraint(comp1)
```

```
##                                             3C Groups 
##                                            1.00000000 
##                                                   3xN 
##                                            0.86500000 
##                                   5E Byg (Bestyrelse) 
##                                            0.50000000 
##                                                    7N 
##                                            0.60875717 
##                                             A-pressen 
##                                            0.22903249 
##                                 A.P. Moeller - Maersk 
##                                            0.15022555 
##                         A/S DANSK ERHVERVSINVESTERING 
##                                            1.00000000 
##                                     Aalborg Forsyning 
##                                            0.50000000 
##                                 Aalborg Stiftstidende 
##                                            1.00000000 
##                  Aalborg Zoologiske Have (Bestyrelse) 
##                                            0.50000000 
##                       Aarhus Letbane I/S (Bestyrelse) 
##                                            0.37673611 
##                                              Aarsleff 
##                                            0.17929009 
##                                        Actona Company 
##                                            0.71223958 
##                                  Advance (Bestyrelse) 
##                                            0.76085069 
##                                            Advice A/S 
##                                            0.48900000 
##                                       AGRO SUPPLY A/S 
##                                            0.33334908 
##                                  Airteam (Bestyrelse) 
##                                            0.80864198 
##                                     Ajos (Bestyrelse) 
##                                            0.58333333 
##                                         Aker Seafoods 
##                                            0.83812500 
##                                  Alex Andersen OElund 
##                                            0.80864198 
##                                                   ALK 
##                                            0.16599610 
##                                                 Aller 
##                                            0.58694444 
##                                          Allianceplus 
##                                            0.63790295 
##                                            Alm. Brand 
##                                            0.20313480 
##                             ALTOR EQUITY PARTNERS A/S 
##                                            0.75062500 
##                                                  Ambu 
##                                            0.28674745 
##                                   Anders Nielsen & Co 
##                                            0.50000000 
##                                    Andersen & Martini 
##                                            1.00000000 
##                                       Andersen Motors 
##                                            1.00000000 
##                                      Andreas Andresen 
##                                            1.00694444 
##                        Anker Hansen & co.(Bestyrelse) 
##                                            0.75154321 
##                                            AP Pension 
##                                            0.14757553 
##                                 Arbejdernes Landsbank 
##                                            0.12005288 
##                                                 Arkil 
##                                            0.17879656 
##                           ARKITEKTERNES PENSIONSKASSE 
##                                            0.33333333 
##                                              Arkitema 
##                                            1.00000000 
##                                            Arla Foods 
##                                            0.50000000 
##                                Arp-Hansen Hotel Group 
##                                            0.18984876 
##                                             Artpeople 
##                                            1.00000000 
##                                 Autohuset Vestergaard 
##                                            1.00000000 
##                                                   AVK 
##                                            0.26414834 
##                          Axcelfuture (Advisory Board) 
##                                            0.18125164 
##                                               Axellus 
##                                            1.00000000 
##                                    B. Christiansen H. 
##                                            1.00694444 
##                                  B. Nygaard Soerensen 
##                                            0.75154321 
##                              Babcock & Wilcox Voelund 
##                                            1.00000000 
##                                Bagger-Soerensen Group 
##                                            0.40238361 
##                                        Bang & Olufsen 
##                                            0.29332176 
##                                              Bankdata 
##                                            0.15527315 
##                           Bansk Boligbyg (Bestyrelse) 
##                                            0.50000000 
##                                 Barslund (Bestyrelse) 
##                                            0.50000000 
##                                       Bavarian Nordic 
##                                            0.22201666 
##                                        BB Electronics 
##                                            0.34613715 
##                           BEC (Bankernes EDB Central) 
##                                            0.13395091 
##                                            Bech-Bruun 
##                                            0.12995000 
##                           Bech-Bruun (Advisory Board) 
##                                            1.00000000 
##                                      Berlingske Media 
##                                            0.75062500 
##                             Berlingske People (24syv) 
##                                            0.21902500 
##                                            Bestseller 
##                                            0.50000000 
##                                      BGB (Bestyrelse) 
##                                            1.00000000 
##                                            BI Holding 
##                                            0.13540662 
##                           Big Future (Advisroy Board) 
##                                            0.11915799 
##                                      Billund Lufthavn 
##                                            0.33570216 
##                                             BKI Foods 
##                                            0.50000000 
##                                        Bladkompagniet 
##                                            0.41717156 
##                                      Bladt Industries 
##                                            0.19242108 
##                                         Blücher Metal 
##                                            1.00000000 
##                         BOARD ASSURE A/S (Bestyrelse) 
##                                            1.00000000 
##                                             Boconcept 
##                                            0.80444444 
##                                   Bolius (Bestyrelse) 
##                                            1.00000000 
##                                            BPW Finans 
##                                            1.00000000 
##                           Brdr. A. & O. Johansen (AO) 
##                                            0.19335005 
##                                       Brenntag Nordic 
##                                            0.53038580 
##                                             BRFkredit 
##                                            0.34303001 
##                                         Broedr. Ewers 
##                                            0.80864198 
##                                    Broedrene Hartmann 
##                                            0.19396172 
##                                              Broendum 
##                                            0.26016257 
##                                        Bruun & Hjejle 
##                                            0.25000000 
##                                       Bruun Rasmussen 
##                                            0.69926698 
##                       Bryggeriforeningen (Bestyrelse) 
##                                            0.28155207 
##                                                  BWSC 
##                                            0.23494933 
##                                         Bygma Gruppen 
##                                            0.32666667 
##                                             C.W. Obel 
##                                            0.34366312 
##                                              Carl Ras 
##                                            1.00000000 
##                                             Carlsberg 
##                                            0.18545782 
##                          Carlsberg Byen Ejendomme P/S 
##                                            0.69753086 
##                  CAT FORSKNINGS- OG TEKNOLOGIPARK A/S 
##                                            0.23240205 
##                                       Chr. Hansen A/S 
##                                            0.37069444 
##                                     Chr. Olesen Group 
##                                            0.40151235 
##                          City Renovation (Bestyrelse) 
##                                            0.51736111 
##                                       Claus Soerensen 
##                                            1.00000000 
##                                         Clipper Group 
##                                            0.33333333 
##                                         CO-RO Holding 
##                                            0.34186921 
##                                                 Codan 
##                                            0.24803964 
##                                             Coloplast 
##                                            0.12305323 
##                                   Container Centralen 
##                                            0.78472222 
##                                                  Coop 
##                                            0.35006173 
##                                             Coop Amba 
##                                            0.45924576 
##                                     COWI (Bestyrelse) 
##                                            0.12724964 
##                                            D/S Norden 
##                                            0.63281250 
##                                                 DADES 
##                                            0.25061224 
##                                           DAGROFA A/S 
##                                            0.26020493 
##                                                  Daka 
##                                            0.32704392 
##                                              Dan Agro 
##                                            0.39006204 
##                                              Dan Cake 
##                                            0.53680857 
##                                               Danfoss 
##                                            0.33333333 
##                                   DANFOSS - SEMCO A/S 
##                                            0.45033236 
##                                              Danhatch 
##                                            0.32780901 
##                                    Danica Corporation 
##                                            0.40297068 
##                                    Danish Agro (note) 
##                                            0.39006204 
##                                  Danish Air Transport 
##                                            1.00000000 
##                                          Danish Crown 
##                                            0.18047330 
##                                               Dansani 
##                                            0.33333333 
##                                     Dansk Retursystem 
##                                            0.50000000 
##                                     Dansk Supermarked 
##                                            0.30134426 
##                   Dansk Vaekstkapital II (Bestyrelse) 
##                                            0.21662179 
##                               DANSK VAEKSTKAPITAL K/S 
##                                            0.22300497 
##                                           Danske Bank 
##                                            0.26505534 
##                               Danske Bank (Direktion) 
##                                            0.37567130 
##                               Danske Commodities (DC) 
##                                            0.19244709 
##                                  Danske Faerger(note) 
##                                            0.50000000 
##                            Danske Medier Research ApS 
##                                            0.71715561 
##                                           Danske Spil 
##                                            0.17630855 
##                               Davidsens Toemmerhandel 
##                                            1.00000000 
##                                           De 5 gaarde 
##                                            0.31521267 
##                                                 Defco 
##                                            0.50000000 
##                                                  DEIF 
##                                            0.33333333 
##                                  Den Jyske Sparekasse 
##                                            0.22664513 
##                                              Designit 
##                                            1.00000000 
##                                                  DFDS 
##                                            0.21954665 
##                                              DGI byen 
##                                            0.50000000 
##                                      DHI (bestyrelse) 
##                                            0.50000000 
##                                             Diba Bank 
##                                            0.27755797 
##                                       Djurslands Bank 
##                                            0.60085069 
##                                       DKA CAPITAL A/S 
##                                            1.00000000 
##                           DLG (Dansk Landbrugs Grov.) 
##                                            0.32328735 
##                                                   DLH 
##                                            1.00000000 
##                                            DLR Kredit 
##                                            0.09648083 
##                                            Donau Agro 
##                                            0.42534722 
##                                           DONG Energy 
##                                            0.14437678 
##                                                    DR 
##                                            0.24270283 
##                                        DR (Direktion) 
##                                            0.42212686 
##                                            DS Gruppen 
##                                            1.00000000 
##                                                   DSB 
##                                            0.17977016 
##                                                   DSV 
##                                            0.17771215 
##                                            DSV Miljoe 
##                                            0.38618056 
##                                           E 3-Gruppen 
##                                            0.50000000 
##                                                  ECCO 
##                                            0.40238361 
##                                      EG (EDB Gruppen) 
##                                            0.34010000 
##                                            Egetaepper 
##                                            0.44087338 
##                                                Egmont 
##                                            0.14901613 
##                              ENEMAERKE & PETERSEN A/S 
##                                            0.86574074 
##                                        Energi Danmark 
##                                            0.33453125 
##                                            Energi Fyn 
##                                            0.50000000 
##                                            EnergiMidt 
##                                            0.50000000 
##                         ERHVERVSINVEST MANAGEMENT A/S 
##                                            0.28387082 
##                                Erria A/S (bestyrelse) 
##                                            0.33333333 
##                                     Esbjerg Forsyning 
##                                            1.00000000 
##                                                ESVAGT 
##                                            0.63790295 
##                                   European Fertilizer 
##                                            0.80864198 
##                                               F Group 
##                                            0.33673611 
##                                F. Junckers Industrier 
##                                            0.49828532 
##                                        F. SALLING A/S 
##                                            0.68055556 
##                                          F.E. Bording 
##                                            0.30428890 
##                 Factor Insurance Brokers (Bestyrelse) 
##                                            0.37037494 
##  FAGBEVAEGELSENS ERHVERVSINVESTERING A/S (Bestyrelse) 
##                                            0.29363021 
##                                                   FDC 
##                                            1.00000000 
##                                         FF SKAGEN A/S 
##                                            0.76518519 
##                                FIBERTEX NONWOVENS A/S 
##                                            0.40746855 
##                                      FIH Erhvervsbank 
##                                            0.69753086 
##                        Filmby Aarhus (Advisory Board) 
##                                            0.33897569 
##                              FINANSIEL STABILITET A/S 
##                                            0.15731537 
##                                        Fjernvarme Fyn 
##                                            0.30500000 
##                                              FLSmidth 
##                                            0.28573422 
##                                               Flügger 
##                                            0.50000000 
##                                Forca A/S (Bestyrelse) 
##                                            0.46864198 
##                                      Force Technology 
##                                            0.50000000 
##                       FORSIKRINGS-AKTIESELSKABET ALKA 
##                                            0.44090206 
##                                  Forsyning Helsingoer 
##                                            1.00000000 
##                                                  Foss 
##                                            0.19868846 
##                             Fredericia Skibsvaerft El 
##                                            0.50173611 
##                                  Frederiksberg Energi 
##                                            1.00000000 
##                                    Fridthjof Film A/S 
##                                            1.00000000 
##                                  Friland (bestyrelse) 
##                                            0.37415132 
##                                      FRITZ HANSEN A/S 
##                                            0.17650062 
##                                      Fuglsang Holding 
##                                            1.00000000 
##                                   Fyens Stiftstidende 
##                                            0.47221111 
##                                            Gasa Group 
##                                            0.67592593 
##                                 Genan Business & Dev. 
##                                            0.80444444 
##                                                Genmab 
##                                            0.29586916 
##                                                 Genua 
##                                            0.56278345 
##                                         GF Forsikring 
##                                            1.00000000 
##                                             GF Inveco 
##                                            0.83812500 
##                                               Givesco 
##                                            0.53680857 
##                                        Global Connect 
##                                            0.75062500 
##                                     Global Wind Power 
##                                            1.00000000 
##                                         GN Store Nord 
##                                            0.28722994 
##                                       Good Food Group 
##                                            0.33733277 
##                                  Gorrissen Federspiel 
##                                            0.20676443 
##                                     GPV International 
##                                            0.33685061 
##                                             Graakjaer 
##                                            1.00000000 
##                                     Greystone Capital 
##                                            0.50000000 
##                                   Groenbech & Soenner 
##                                            0.28303015 
##                                Groupcare (Bestyrelse) 
##                                            0.48900000 
##                                             Gyldendal 
##                                            0.18332314 
##                                           H. Lundbeck 
##                                            0.13879961 
##                                           H.J. Hansen 
##                                            0.76085069 
##                                     H+H International 
##                                            0.43945312 
##                                               Halberg 
##                                            0.44373521 
##                                        Haldor Topsoee 
##                                            0.13501836 
##                                        Hamlet Protein 
##                                            0.34431276 
##                                       Hans Just Group 
##                                            0.59244083 
##                                     Hansson & Knudsen 
##                                            0.74196511 
##                             Hanstholm Fiskemelsfabrik 
##                                            0.76518519 
##                                         Harald Nyborg 
##                                            1.00000000 
##                                      Harboes Bryggeri 
##                                            0.21742607 
##                                    Have Kommunikation 
##                                            0.42189643 
##                                 HCS Transport & Sped. 
##                                            1.00000000 
##                              Hededanmark (Bestyrelse) 
##                                            1.00000000 
##                                                Hempel 
##                                            0.13770185 
##                             Henning Larsen Architects 
##                                            0.35026286 
##                                        Henry Kjeldsen 
##                                            1.00000000 
##                                      HHM (Bestyrelse) 
##                                            0.80444444 
##                                  Hildebrandt & Brandi 
##                                            0.86500000 
##                                              Hoffmann 
##                                            1.00000000 
##                                             HOFOR A/S 
##                                            0.36257166 
##                        HOFOR Holding P/S (Bestyrelse) 
##                                            0.36257166 
##                          HOFOR Spildevand Holding A/S 
##                                            0.43111111 
##                                         Hornsyld Korn 
##                                            1.00000000 
##                                                Horten 
##                                            0.50000000 
##                                         HusCompagniet 
##                                            0.50000000 
##                                             HV Invest 
##                                            0.68056583 
##                                            Hydra Tech 
##                                            0.45256944 
##                                           IAI Holding 
##                                            0.43945312 
##                                           IBM Danmark 
##                                            0.32455942 
##                                           IC Companys 
##                                            0.34167631 
##                                              IDdesign 
##                                            0.42966049 
##                                                Imerco 
##                                            0.22612916 
##                                   Industriens Pension 
##                                            0.09511041 
##                                           Information 
##                                            0.41713349 
##                                           Inter Primo 
##                                            0.25658928 
##                                            Interbuild 
##                                            0.75308642 
##                                              Interdan 
##                                            1.00000000 
##                                             Intermail 
##                                            0.44472222 
##                                         INTERSTIL A/S 
##                                            0.71223958 
##                     INVESTERINGSFORENINGEN MAJ INVEST 
##                                            0.12574565 
##                                                   ISS 
##                                            0.50000000 
##                              Itelligence (Bestyrelse) 
##                                            0.47026910 
##                                          J. Lauritzen 
##                                            0.21613283 
##                                        Jensen Denmark 
##                                            0.72145275 
##                                   Jensen's Food Group 
##                                            1.00000000 
##                                                Jeudan 
##                                            0.14220909 
##                                 Jeudan II(Bestyrelse) 
##                                            0.43099451 
##                                           JN DATA A/S 
##                                            0.20499336 
##                                          Johannes Fog 
##                                            0.30428890 
##                   Jorcks Ejendomsselskab (bestyrelse) 
##                                            0.54611623 
##                                   Jorton (Bestyrelse) 
##                                            0.84694444 
##                                        Jorton Holding 
##                                            0.84694444 
##                                     JP/Politikens Hus 
##                                            0.24730867 
##                                JYSK FYNSKE MEDIER P/S 
##                                            0.39725309 
##                                          Jysk Holding 
##                                            0.71223958 
##                                    Jysk/Fynsk Kapital 
##                                            0.40597541 
##                                            Jyske Bank 
##                                            0.23332967 
##                                Jyske Bank (Direktion) 
##                                            0.36171276 
##                    Jyske Bank (Group Executive Board) 
##                                            0.41923599 
##                  Jyske Bank (Group Supervisory Board) 
##                                            0.76518519 
##                               K. Nissen International 
##                                            0.19816420 
##                                          KAMSTRUP A/S 
##                                            0.31168530 
##                         Karnov Group (Advisory Board) 
##                                            0.50000000 
##                                      Kemp & Lauritzen 
##                                            0.18461486 
##                                                   KFI 
##                                            0.42234568 
##                                          Kim Johansen 
##                                            0.50000000 
##                                    KIRK & THORSEN A/S 
##                                            0.41358025 
##                                         Kirkbi (LEGO) 
##                                            0.12892766 
##                                              KK-Group 
##                                            0.50000000 
##                                                Klimax 
##                                            1.00000000 
##                                                   KMD 
##                                            0.18117089 
##                                                   KNI 
##                                            0.48352891 
##                                 Koebenhavns Lufthavne 
##                                            0.37216759 
##        Koebenhavns Lufthavns Vaekstkomité (Medlemmer) 
##                                            0.12028784 
##                             Koebstaedernes Forsikring 
##                                            1.00000000 
##                               Kommunikationsforum A/S 
##                                            0.67046875 
##                                            KOMPAN A/S 
##                                            0.50000000 
##                             KONGSKILDE INDUSTRIES A/S 
##                                            0.53074074 
##                      Kontorfaellesskabet i Amaliegade 
##                                            0.23001274 
##             Kontorfaellesskabet paa Sankt Annae Plads 
##                                            0.12690037 
##                              KPC herning (Bestyrelse) 
##                                            1.00000000 
##                                           KPC Holding 
##                                            0.34600646 
##                                    Kristeligt Dagblad 
##                                            0.33333333 
##                                       Kromann Reumert 
##                                            0.16670540 
##                                            Kunde & co 
##                                            1.00000000 
##                                       Kvadrat Holding 
##                                            1.00000000 
##                                      Laan & Spar Bank 
##                                            0.19248877 
##                               LAEGERNES PENSIONSKASSE 
##                                            1.00000000 
##                                     Laerernes Pension 
##                                            0.33333333 
##            Landbrugets Finansieringsbank (Bestyrelse) 
##                                            1.00000000 
##                                                Lastas 
##                                            0.66864198 
##                                           LB Forening 
##                                            1.00000000 
##                                           Lead Agency 
##                                            0.23386012 
##                                              LEGO A/S 
##                                            0.26353328 
##                                          Lemminkainen 
##                                            1.00000000 
##                                        Lemvigh-Müller 
##                                            0.80864198 
##                                            LEO Pharma 
##                                            0.21306224 
##                                         Lett Law Firm 
##                                            0.33333333 
##                                                 Linak 
##                                            1.00000000 
##                                      Linderberg Group 
##                                            0.40729383 
##                                           LINDPRO A/S 
##                                            0.86574074 
##                                   LM byg (Bestyrelse) 
##                                            0.45987654 
##                                           LMB Danmark 
##                                            0.59689449 
##                                Louis Poulsen Lighting 
##                                            0.33376736 
##                                     LR REALKREDIT A/S 
##                                            0.15573852 
##                                 M.Goldschmidt Holding 
##                                            0.22992337 
##                                         Maersk Broker 
##                                            0.22822500 
##                             Maersk Container Industry 
##                                            0.63045953 
##                                    Maersk Olie og Gas 
##                                            0.61392691 
##                                MAJ INVEST HOLDING A/S 
##                                            0.10579859 
##                                                Mannaz 
##                                            0.45987654 
##                                       Marius Pedersen 
##                                            0.29410156 
##                                        Metroselskabet 
##                                            0.33333333 
##                                      Mholding (Matas) 
##                                            0.13703896 
##                                 Microsoft Development 
##                                            1.00000000 
##                                  Miracle (Bestyrelse) 
##                                            1.00000000 
##                                           MLA-Gruppen 
##                                            0.80864198 
##                            Moalem Weitemeyer Bendtsen 
##                                            1.00000000 
##                                           Mols-Linien 
##                                            0.18761322 
##                                     Monberg & Thorsen 
##                                            0.50711066 
##                                       Monjasa Holding 
##                                            0.50000000 
##                                    Movia (Bestyrelse) 
##                                            1.00000000 
## MP PENSION - PENSIONSKASSEN FOR MAGISTRE & PSYKOLOGER 
##                                            1.00000000 
##                                          MT Hoejgaard 
##                                            0.17021019 
##                             Munck Asfalt (Bestyrelse) 
##                                            1.00694444 
##                Munck Forsyningsledninger (Bestyrelse) 
##                                            1.00694444 
##                                         Munck Gruppen 
##                                            0.61111111 
##            NASDAQ OMX Copenhagen (Advisory Committee) 
##                                            0.16919255 
##                                          Naturgas Fyn 
##                                            0.33333333 
##                               Nemlig.com (Bestyrelse) 
##                                            0.38678998 
##                                          Nextwork A/S 
##                                            0.67046875 
##                              Nic. Christiansen Invest 
##                                            0.61111111 
##                             Nielsen & Nielsen Holding 
##                                            0.23312500 
##                                    Nielsen & Noerager 
##                                            0.50000000 
##                                    Niras (Bestyrelse) 
##                                            0.58145833 
##                                         Niras Gruppen 
##                                            0.37486225 
##                               NKT cables (Bestyrelse) 
##                                            1.00000000 
##                                           NKT Holding 
##                                            0.17522824 
##                                    NNE PHARMAPLAN A/S 
##                                            0.28823380 
##                                              NNIT A/S 
##                                            0.15526345 
##                                         Nobia Denmark 
##                                            1.00000000 
##                             Nord. Parfumerivarefabrik 
##                                            1.00000000 
##                                             NORDEA AB 
##                                            0.50000000 
##                                 NORDEA AB (Direktion) 
##                                            1.00000000 
##                             NORDEA FINANS DANMARK A/S 
##                                            0.60616264 
##                                  Nordea Liv & Pension 
##                                            0.33873663 
##                                          Nordic Sugar 
##                                            1.00000000 
##                                        Nordic Tankers 
##                                            1.00000000 
##                                      Nordisk Film A/S 
##                                            0.24500429 
##                         Nordisk Film Distribution A/S 
##                                            0.73090278 
##                                        Nordjyske Bank 
##                                            0.18650609 
##                                               Norisol 
##                                            1.00000000 
##                                           North Media 
##                                            1.00000000 
##                                              NOVO A/S 
##                                            0.14965557 
##                                      Novo Holding A/S 
##                                            0.14480624 
##                          Novo Holding A/S (Direktion) 
##                                            0.16352640 
##                                          Novo Nordisk 
##                                            0.13533465 
##                                             Novozymes 
##                                            0.14801375 
##                                                  NRGi 
##                                            1.00000000 
##                            Nykredit Forsikring (note) 
##                                            0.53103567 
##                     Nykredit Holding A/S (bestyrelse) 
##                                            0.11621238 
##                      Nykredit Realkredit (Bestyrelse) 
##                                            0.13129960 
##                               Odense Staalskibsvaerft 
##                                            0.36606451 
##                ODIN EQUITY PARTNERS MANAGEMENT II K/S 
##                                            0.75062500 
##                                                   OEK 
##                                            0.16957301 
##                             OEresund A/S (Bestyrelse) 
##                                            0.18948817 
##                                                    OK 
##                                            0.19924265 
##                                         Orifarm Group 
##                                            0.35361111 
##                   P. CHRISTENSEN, ODENSE, HOLDING A/S 
##                                            1.00000000 
##                              Pallisgaard (Bestyrelse) 
##                                            1.00000000 
##                                               Pandora 
##                                            0.22617670 
##                                                Pankas 
##                                            0.65703735 
##                          Parken Sport & Entertainment 
##                                            0.50000000 
##                                        PensionDanmark 
##                                            0.12613360 
##                                           Peoplegroup 
##                                            0.50173611 
##                                           Peter Bodum 
##                                            0.38956404 
##                                Peter Justesen Company 
##                                            0.20491645 
##                      Peter Madsen Rederi (Bestyrelse) 
##                                            1.00000000 
##                              PFA Pension (Bestyrelse) 
##                                            0.18367703 
##                                               Philips 
##                                            1.00000000 
##                                    Pindstrup Mosebrug 
##                                            0.67013889 
##                                      PKA (Bestyrelse) 
##                                            0.35666667 
##                                       PKA (direktion) 
##                                            0.69345679 
##                                               Plesner 
##                                            0.34278549 
##                                             Plus Pack 
##                                            0.65668403 
##                                PM energi (Bestyrelse) 
##                                            1.00000000 
##                                POLARIS MANAGEMENT A/S 
##                                            0.29109892 
##                                          Polen Invest 
##                                            0.56299383 
##                                 POUL DUE JENSENS FOND 
##                                            0.50000000 
##                                              PRAS A/S 
##                                            0.12067378 
##                                   Pre-Seed Innovation 
##                                            0.50000000 
##                                       Pressalit Group 
##                                            0.50000000 
##                               Pressens Faellesindkoeb 
##                                            0.41021719 
##                                         Privatsikring 
##                                            0.33333333 
##                              Progressive (Bestyrelse) 
##                                            0.41173611 
##                 PROTEIN- OG OLIEFABRIKKEN SCANOLA A/S 
##                                            0.59689449 
##                                           PWT Holding 
##                                            0.34977372 
##                                       R. Faerch Plast 
##                                            0.31571667 
##                                            Radiometer 
##                                            0.64293686 
##                                      Ramboell Gruppen 
##                                            0.15490574 
##                                              Ratos AB 
##                                            0.29051719 
##                                REALKREDIT DANMARK A/S 
##                                            0.40297068 
##                                     Reckitt Benckiser 
##                                            1.00000000 
##                                             Rema 1000 
##                                            1.00000000 
##                                               Riegens 
##                                            1.00000000 
##                               Ringkjoebing Landbobank 
##                                            0.17318336 
##                                         Ritzau Bureau 
##                                            0.30963855 
##                                       RM Rich. Müller 
##                                            0.20402850 
##                                            RN Holding 
##                                            0.52111111 
##                                Rockwool International 
##                                            0.23706864 
##                                     Roenne & Lundgren 
##                                            0.50000000 
##                            Rohde Nielsen (Bestyrelse) 
##                                            1.00000000 
##                                     Royal Arctic Line 
##                                            1.00000000 
##                                       Royal Greenland 
##                                            0.52111111 
##                                         Royal Unibrew 
##                                            0.12895466 
##                                         Sampension KP 
##                                            0.15305623 
##                                             Sanistaal 
##                                            0.50683594 
##                                       SAS DANMARK A/S 
##                                            1.00000000 
##                                             SAS Group 
##                                            0.29987654 
##                                             Saxo Bank 
##                                            0.27311021 
##                                       Saxo Privatbank 
##                                            0.50000000 
##                                           Scan Office 
##                                            0.28194830 
##                                 Scancom International 
##                                            0.71223958 
##                               Scandi Byg (Bestyrelse) 
##                                            0.86574074 
##                       SCANDINAVIAN PRIVATE EQUITY A/S 
##                                            0.18630506 
##                                            Scanenergi 
##                                            0.33333333 
##                                          Schouw & Co. 
##                                            0.11730174 
##                                   Schur International 
##                                            0.25000000 
##              Scion DTU - Det Groenne Ivaerksaetterhus 
##                                            0.50000000 
##                                  SDA-Koncernen (note) 
##                                            0.16224788 
##                        SDC (Skandinavisk Data Center) 
##                                            0.22969839 
##                                    SE BLUE EQUITY A/S 
##                                            0.68402778 
##                                              SEAS-NVE 
##                                            0.66625000 
##                                           SEB Pension 
##                                            0.50000000 
##                                        Semler Gruppen 
##                                            0.37144819 
##                            Semler Retail (Bestyrelse) 
##                                            0.80864198 
##                                               Siemens 
##                                            1.00000000 
##                                    Siemens Wind Power 
##                                            0.38587281 
##                                               SimCorp 
##                                            0.28392716 
##                                          Sirena Group 
##                                            0.67506944 
##                                 SIVIL (Advisoryboard) 
##                                            1.00000000 
##                                   Sjaellandske Medier 
##                                            0.45076266 
##                                                 Skako 
##                                            0.23201732 
##                                  Skandinavisk Holding 
##                                            0.25338712 
##                                           Skjern Bank 
##                                            0.25000000 
##                                    Slagteriet Broerup 
##                                            1.00000000 
##                      Smith, Hammer, Lassen architects 
##                                            0.28713022 
##                                      Soenderborg Korn 
##                                            0.80864198 
##                                                 Solar 
##                                            0.43425383 
##                                        Sondex Holding 
##                                            1.00000000 
##                                                Sonion 
##                                            0.63021861 
##                                     SOS International 
##                                            1.00000000 
##                                              SP Group 
##                                            0.15031269 
##                                             Spar Nord 
##                                            0.13905689 
##                                   Sparekassen Faaborg 
##                                            0.37566103 
##                                Sparekassen Himmerland 
##                                            0.50000000 
##                               Sparekassen Kronjylland 
##                                            1.00000000 
##                                 Sparekassen Sjaelland 
##                                            0.23806262 
##                                       Sparekassen Thy 
##                                            0.83506944 
##                                Sparekassen Vendsyssel 
##                                            0.37721881 
##                                       SPF-DANMARK P/S 
##                                            0.31680578 
##                                                   SSG 
##                                            0.38118056 
##                                         Stibo Holding 
##                                            0.23474490 
##                                    Studsgaard Holding 
##                                            1.00000000 
##                Sund og Baelt Holding A/S (Bestyrelse) 
##                                            0.16658662 
##                                         SUPERGROS A/S 
##                                            1.00000000 
##                                           SVITZER A/S 
##                                            0.44728395 
##                                       Syd Energi (SE) 
##                                            0.33897569 
##                                               Sydbank 
##                                            0.12703556 
##                   SYDDANSK TEKNOLOGISK INNOVATION A/S 
##                                            0.36091942 
##                                Sygeforsikring Danmark 
##                                            0.50000000 
##                                              Synoptik 
##                                            0.48900000 
##                                            Systematic 
##                                            0.40225765 
##                                     T. Hansen Gruppen 
##                                            1.00000000 
##                                            TDC (note) 
##                                            0.14860447 
##                                TDC (note) (Direktion) 
##                                            0.33980442 
##                  Tegnestuen Mejeriet (Advisory Board) 
##                                            0.49184172 
##                                TELE-POST (Bestyrelse) 
##                                            1.00000000 
##                                                 Terma 
##                                            0.15403048 
##                                  THE SCANDINAVIAN AsP 
##                                            0.72000000 
##                      Thorkil Andersen (Frode Laursen) 
##                                            0.50000000 
##                                              Thornico 
##                                            0.68402778 
##                            Thyholm Murer (Bestyrelse) 
##                                            1.00000000 
##                                              Thykjaer 
##                                            1.00000000 
##                                                 Tican 
##                                            1.00000000 
##                                                Tivoli 
##                                            0.27464570 
##                                        TK Development 
##                                            0.32947531 
##                                          Toms Gruppen 
##                                            0.38678998 
##                                            Topdanmark 
##                                            0.25063492 
##                                       TOTALKREDIT A/S 
##                                            0.12007903 
##                                                  Tryg 
##                                            0.08870099 
##                                TULIP FOOD COMPANY A/S 
##                                            0.20634311 
##                                                   TV2 
##                                            0.21177753 
##                                     Uggerhoej Holding 
##                                            1.00000000 
##                                     Uhrenholt Holding 
##                                            0.31070313 
##                                             Unifeeder 
##                                            0.50000000 
##                              Unisense Environment A/S 
##                                            0.38346065 
##                          Unleash (board of directors) 
##                                            0.30865824 
##                       Unleash (global advisory board) 
##                                            0.30990053 
##                           USTC (United Ship. & Trad.) 
##                                            0.20205099 
##                                    Vald. Birn Holding 
##                                            1.00000000 
##                               Valdemars Slot Gods A/S 
##                                            0.65728395 
##                                        Vandcenter Syd 
##                                            1.00000000 
##                                             VELUX A/S 
##                                            0.17204633 
##                                                 Verdo 
##                                            1.00000000 
##                                                Vestas 
##                                            0.14029322 
##                                    Vestas (Direktion) 
##                                            0.74196511 
##                   Vestas Northern Europe (Bestyrelse) 
##                                            0.49334252 
##                                   Vestergaard Company 
##                                            0.65703735 
##                                         Vestforsyning 
##                                            0.53472222 
##                                             Vestfrost 
##                                            0.47026910 
##                                    Vestjyllands Andel 
##                                            1.00000000 
##                                         Vestjysk Bank 
##                                            0.50000000 
##                                             Via Biler 
##                                            0.80864198 
##                                                Viasat 
##                                            1.00000000 
##                                   Victoria Properties 
##                                            0.65728395 
##                                   Vital Petfood Group 
##                                            0.52111111 
##                                           VKR Holding 
##                                            0.16642930 
##                                     VP Securities A/S 
##                                            0.20196817 
##                                               Welltec 
##                                            0.80444444 
##                       Wicotec Kirkebjerg (Bestyrelse) 
##                                            0.63791960 
##                                                 Widex 
##                                            0.33445325 
##                                        William Demant 
##                                            0.19237319 
##                                 WRIST SHIP SUPPLY A/S 
##                                            0.35562500 
##                                                 Zebra 
##                                            0.17611129 
##                                       Zentropa Folket 
##                                            0.35069444 
##              Zoologisk Have i Koebenhavn (Bestyrelse) 
##                                            0.25000000
```

```r
# make brokerage variable so we can compare it to the other measures
brokerage <- 1- constraint(comp1)

# Let us make a data.frame with tibble() and look at the different measures. 
metrics <- tibble(
  
  name = names(constraint(comp1)),
  brokerage = 1- constraint(comp1),
  betweenness = betweenness(comp1, directed = FALSE),
  degree = degree(comp1, mode = "all") 
  
)

# Let us see who is least constraint?
metrics %>% arrange(desc(brokerage)) 
```

```
## # A tibble: 533 × 4
##    name                                           brokerage betweenness degree
##    <chr>                                              <dbl>       <dbl>  <dbl>
##  1 Tryg                                               0.911      11793.     20
##  2 Industriens Pension                                0.905       6273.     19
##  3 DLR Kredit                                         0.904       8371.     23
##  4 MAJ INVEST HOLDING A/S                             0.894       6080.     18
##  5 Nykredit Holding A/S (bestyrelse)                  0.884       6480.     18
##  6 Schouw & Co.                                       0.883      11209.     17
##  7 Big Future (Advisroy Board)                        0.881       4795.     12
##  8 Arbejdernes Landsbank                              0.880       4092.     17
##  9 TOTALKREDIT A/S                                    0.880       3778.     20
## 10 Koebenhavns Lufthavns Vaekstkomité (Medlemmer)     0.880       3831.     12
## # … with 523 more rows
```

As you can see, Burt's constraint is very similar to betweenness, with certain distinctions. In a sense, Burt's constraint is more local, not taking the whole network structure into account.

Let us visualize Burt's constraint by adding it as a graph attribute.


```r
# Make sure to test if sequence is correct before adding it to a network attribute. 
all.equal(metrics$name, V(comp1)$name) 
```

```
## [1] TRUE
```

```r
# Add to our graph object
V(comp1)$brokerage <- metrics$brokerage

# how to select the 10 most central people?
ten_largest <- metrics %>% 
  arrange(desc(brokerage)) %>% #arranage by brokerage
  slice(1:10) %>% # slice the data frame and look at the first 10 nodes
  pull(name) # extract only the names

# visualize
comp1 %>% 
  ggraph(layout = "mds") +
  geom_edge_link0(color='grey', width=0.6, alpha=0.35) + 
  geom_node_point(aes(size=brokerage), alpha=0.75) + 
  # scalling the size interval of geom node point
  scale_size(range = c(0.01,3)) +
  scale_color_viridis() +
  geom_node_label(aes(filter=name %in% ten_largest, label=name), alpha=0.75, repel=T, size=1) +
  geom_node_point(aes(filter=name %in% ten_largest), color='red', size=.75, alpha=0.75) +
  theme_graph()
```

```
## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/visualization-1.png" width="672" />

## 5.2 Assortativity

Assortativity measures the extend to which nodes with similar properties are connected to each other. The assortativity coefficient ranges between -1 and 1, where 1 indicates that there is a high likelihood of two nodes with the same properties being connected. Networks that have an assortativity of 0 are usually referred to as neutral.

There are three kinds of assortativities we cover in this session:

-   categorical assortativity which measures to what extend nodes with the same categorical properties are connected with each other. This is measured by the function `assortativity_nominal()`

-   continuous assortativity which is used to measure homophily based on a continuous variable with the function `assortativity()`

-   assortativity degree with calculates the if nodes with similar degree scores are connected with each other. This is measured with the function `assortativity_degree()`

### 5.2.1 Continuous assortativity

In order to show examples of calculating the former options, I will create a new variable which measures the gender proportion in an company board. This will create the basis for the calculation of `assortativity()`.


```r
# What is the gender proportion?
den1 %>% count(gender)
```

```
## # A tibble: 4 × 2
##   gender        n
##   <chr>     <int>
## 1 Binominal    30
## 2 Men        6605
## 3 Women      1305
## 4 <NA>         49
```

```r
# Let's aggregate gender proportions for each affiliation (but only look at women and men)
gender <- den1 %>% 
  #filter for women and men only
  filter(gender %in% c("Women", "Men")) %>% 
  #count amount of women and men per affiliation
  count(affiliation, gender) %>% 
  # do something, but group by affiliation first
  group_by(affiliation) %>% 
  # generates the total number of board members per affiliation
  mutate(n_total = sum(n), 
         # generates the share of women and men.
         share = n/(sum(n)))   

# Now, let us focus on one share: the share of men in the dataset. 
Men <- gender %>% filter(gender == "Men")
Women <- gender %>% filter(gender == "Women" & n == n_total)
Women$share = 0

Men <- Men %>% select(affiliation, share)
Women <- Women %>% select(affiliation, share)

# create a common object 
gender1 <- rbind(Men, Women)

# now, just choose those affiliations that are part of the biggest component
gender2 <- 
  gender1 %>% 
  filter(affiliation %in% V(comp1)$name)


# check if they are same than the comp1aph
all.equal(gender2$affiliation, V(comp1)$name)
```

```
## [1] "457 string mismatches"
```

```r
index <- match(V(comp1)$name, gender2$affiliation)
gender2 <- gender2 %>% arrange(factor(affiliation, levels = affiliation[index]))
all.equal(gender2$affiliation, names(V(comp1)))
```

```
## [1] TRUE
```

```r
# Let us add gender share as an attribute to the graph 
V(comp1)$share_men <- gender2$share

# Visualize 
comp1 %>% 
  ggraph(layout = "stress") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=share_men), size=5) + 
  theme_graph() +
  # make a gradient of color from red to blue per gender
  scale_color_gradient2(low='firebrick4', mid='grey80', high='dodgerblue4', midpoint=0.5, na.value='pink') 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/nominal-1.png" width="672" />

```r
# Calculate assortativity for share of men
assortativity(comp1, V(comp1)$share_men, directed=FALSE)
```

```
## [1] 0.1578647
```

In this example, the assortativity of `0.157`tells us that there is a small likelihood that boards with a similar gender proportion are directly connected with each other.

### 5.2.2 Categorical assortativity

Now, we can use the gender proportion variable to create a new categorical variable of gender. It changes the variable to *female dominated*, *gender_balanced* and *male_dominated* depending on their proportions. This is then used to calculate and visualize categorical assortativity.


```r
# let us create a categorical gender variable based on gender2. 
gender3 <- 
  gender2 %>%
  # we are creating a new variable "gender_cat" where we use a case_when() statement
  mutate(gender_cat = case_when(
    # when the share col has a share of males below 0.45, it is female dominated
    share < 0.45 ~ "female_dominated", 
    0.45 < share & share < 0.55 ~ "gender_balanced",
    .default = "male_dominated"))

# let us add it to the graph comp1
all.equal(gender3$affiliation, names(V(comp1)))
```

```
## [1] TRUE
```

```r
V(comp1)$gender_cat <- gender3$gender_cat

comp1 %>% 
  ggraph(layout = "fr") + 
  geom_edge_link0(width=.5, alpha=0.4) + 
  geom_node_point(aes(color=gender_cat), size=3) + 
  scale_color_manual(values = c("salmon1", "olivedrab", "steelblue")) + 
  theme_graph() 
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/cat1-1.png" width="672" />

```r
# Make sure to write as.factor(), so it is recognized as a categorical variable
assortativity_nominal(comp1, as.numeric(as.factor(V(comp1)$gender_cat)), directed = FALSE)
```

```
## [1] 0.0395072
```

With an assortativity of `0.03`, there is a neutral likelihood that nodes with the same gender category `gender_cat` are directly connected with each other. The score is telling us that we cannot expect boards with the same gender proportion to be connected with each other.

## 5.3 Material

-   [05-rscript](05-rscript.R)
-   [05-rscript-blank](05-rscript-blank.R)



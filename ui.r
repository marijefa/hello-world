library(shiny)
library(ggplot2)
library(scales)
library(directlabels)
library(plyr)
library(xlsx)
library(psy)
library(xtable) #om automatisch tabel te laten maken
library(reshape2) #voor omzetten correlatiematrix naar lang
library(knitr)

fluidPage(
  
  titlePanel("ToetsAnalyseTool"),
  
  sidebarPanel(
    conditionalPanel(
      condition = "!output.fileUploaded",
      "Lees de informatie in de tab 'Start' en upload toetsgegevens bij de tab 'Gegevens uploaden' om aan de 
      slag te gaan met de ToetsAnalyseTool. Voorbeeldgegevens kunnen worden gedownload bij de tab 'Start'."),
    conditionalPanel(
      condition = "output.fileUploaded",
      h5(strong("Algemene informatie")),
      textOutput("tekst_ndeelnemers"),
      textOutput("tekst_nvragen"),
      textOutput("tekst_maxscore"),
      textOutput("tekst_gokscore"),
      textOutput("tekst_alfa"),
      h5(strong("Totaalscores")),
      textOutput("tekst_gemscore"),
      textOutput("tekst_sdscore"),
      textOutput("tekst_rangescore"),
      h5(strong("Uitslag")),
      textOutput("tekst_cesuurmethode"),
      textOutput("tekst_cesuur"),
      textOutput("tekst_nslaag"),
      h5(strong("Cijfers")),
      textOutput("tekst_gemcijfer"),
      textOutput("tekst_sdcijfer"),
      textOutput("tekst_rangecijfer"),
      h5(strong("Download resultaten")),
      conditionalPanel(
        condition = "!output.fileDownloadklaar",
        "Als de tabs tot en met 'Per vraag' zijn doorlopen, kunnen de resultaten hier gedownload worden."),
      conditionalPanel(
        condition = "output.fileDownloadklaar",
        "Via onderstaande knoppen kan de download worden gestart van een Excel-bestand met de resultaten 
        en van een pdf met een samenvatting van de belangrijkste resultaten en instellingen.",
      br(),
      br(),
      downloadButton('download_xslx', 'Deelnemer- en vraaggegevens in Excel'),
      br(),
      br(),
      downloadButton("download_pdf", "Overzicht van toetsanalyse als pdf")))
  ),
  
  ####main panel####
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Start",
                         br(),
                         "Welkom bij de ToetsAnalyseTool! Deze tool kan worden gebruikt om toetsgegevens
                         te verwerken en om meer inzicht in toetsresultaten te krijgen. Om aan de slag te gaan
                         begint u met het uploaden van toetsgegevens (onder de tab 'Gegevens uploaden'). Daarna kunt
                         u de daarop volgende tabs doorlopen en waar gewenst instellingen van de analyse aanpassen.
                         Zo kan er gekozen worden tussen verschillende cesuurmethodes, kan worden aangegeven wanneer
                         vragen statistisch gesignaleerd moeten worden en kan de scoring van vragen aangepast worden.
                         Wanneer u alles naar tevredenheid heeft ingesteld, kunt u de resulaten in een Excel-bestand
                         downloaden. Ook kunt u een pdf downloaden met een samenvatting van de belangrijkste
                         resultaten en instellingen.",
                         br(),
                         br(),
                         h5(strong("Voorbeeldgegevens")),
                         "Voordat u met uw eigen toetsgegevens aan de slag gaat, kunt u de tool uitproberen met 
                         onderstaande voorbeeldgegevens. Kies voor een van de drie toetsvormen, klik op de bijbehorende
                          link(s) en sla de .csv-bestanden op op uw computer. Deze kunt u vervolgens uploaden bij de tab 'Gegevens 
                         uploaden'. U kunt de voorbeeldgegevens ook gebruiken als voorbeeld 
                         van hoe u uw gegevens moet vormgeven zodat de tool ermee uit de voeten kan
                         (dit moet precies kloppen; zie tab 'Gegevens uploaden' voor de eisen).",
                         br(),
                         br(),
                         em("Toets gescoord als 0/1 (fout/goed)"),
                         br(),
                         a("Antwoordgegevens", href="https://drive.google.com/uc?export=download&id=1vtMWYWRayqcBuiW6P8zWmix1amf76w2B", target="_blank"),
                         br(),
                         em("Toets met meerkeuzevragen"),
                         br(),
                         a("Antwoordgegevens", href="https://drive.google.com/uc?export=download&id=1d_HxcBgGe6RTpJLt3lZf3EJy9ixzU5EJ", target="_blank"),
                         br(),
                         a("Vraaggegevens", href="https://drive.google.com/uc?export=download&id=1hHAlzud-VcKr3YUTbXx0GYqD7pXtYGz0", target="_blank"),
                         br(),
                         a("Deelnemergegevens (optioneel)", href="https://drive.google.com/uc?export=download&id=1u3WaktDsNkCMS9RgxjBL1PQAOzoOw2ko", target="_blank"),
                         br(),
                         em("Toets met vragen met deelscores"),
                         br(),
                         a("Antwoordgegevens", href="https://drive.google.com/uc?export=download&id=1gDPlKyHxRmPZsw4wMZ6cRF9-In2SKXHE", target="_blank"),
                         br(),
                         a("Vraaggegevens", href="https://drive.google.com/uc?export=download&id=1St-FsmzLQDVszpY5A7m6o8HFw0TZwx8j", target="_blank"),
                         br(),
                         a("Deelnemergegevens (optioneel)", href="https://drive.google.com/uc?export=download&id=1u3WaktDsNkCMS9RgxjBL1PQAOzoOw2ko", target="_blank"),
                         br(),
                         br(),
                         h5(strong("Status van de tool")),
                         "De ToetsAnalyseTool is nog volop in ontwikkeling, dus u kunt nog fouten of errors tegenkomen
                         en al uw feedback en suggesties zijn meer dan welkom. U kunt ons bereiken via ",
                         a("het contacformulier van 10voordeleraar", href="https://www.10voordeleraar.nl/contact", target="_blank"),".
                         Het programmabureau van ",em("10voordeleraar")," is niet verantwoordelijk voor de juistheid van de met
                         de ToetsAnalyseTool verkregen resultaten. Merk verder op dat de tool vooralsnog op een gratis
                         server in de VS draait, wat betekent dat het anonimiseren van de te uploaden toetsgegevens
                         aan te raden is."), 
                tabPanel("Gegevens uploaden",
                         br(),
                         "Voor het gebruik van de analysetool moeten verschillende gegevens over de toets in deze 
                         app geupload worden in de vorm van .csv-bestanden (kies in Excel bij 'Opslaan als' voor
                         'CSV (Comma delimited) (*.csv)'). Het uploaden kan worden gedaan met behulp van de 
                         onderstaande invoerknoppen.",
                         br(),
                         "Er kan worden gekozen voor het uploaden van alleen de antwoordgegevens. In dat geval
                         moeten deze gegevens worden verstekt als nullen en enen (0 voor een fout antwoord en 1
                         voor een goed antwoord). Bij de vraaggegevens wordt er dan voor alle vragen vanuit
                         gegaan dat 1 de maximale score is, dat 1 het goede antwoord is (sleutel is 1), dat de gokkans 0 is (vraagtype KA 
                         (kort antwoord)) en dat alle vragen uit hetzelfde toetsonderdeel komen (gehele toets). 
                         Let op: als er wel degelijk een gokkans is bij vragen, zijn de cesuren waarbij moet
                         worden gecorrigeerd voor de gokscore in dit geval incorrect. Bij geen apart ingevoerde deelnemerggegevens
                         worden alle toetsdeelnemers in een algemene groep geplaatst.",
                         br(),
                         "Wanneer zowel eerstekansers als herkansers aan de toets hebben deelgenomen, verdient 
                         het aanbeveling de analyses voor cesuurbepaling en eventuele wijzigingen in de toets
                         alleen op gegevens van eerstekansers te baseren.",
                         h5(strong("Antwoordgegevens (vereist)")),
                         "Het .csv-bestand met de antwoordgegevens moet bestaan uit de antwoorden van alle toetsdeelnemers
                         op alle toetsvragen. Elke regel (horizontaal) staat voor een toetsdeelnemer en elke kolom
                         (verticaal) voor een toetsvraag. Elke cel bestaat daarmee uit een antwoord van een toetdeelnemer
                         op een toetsvraag. De eerste kolom moet bestaan uit unieke deelnemeraanduidigen (simpelweg
                         nummeren kan ook). De eerste rij moet bestaan uit unieke vraagaanduidingen (ook hier kan
                         genummerd worden, maar er mogen geen spaties of bijzondere tekens worden gebruikt). 
                         Wanneer bij een vraag deelscores worden toegekend, moet het aantal behaalde
                         punten in de cellen worden ingevuld.",
                         fileInput('antwoordgegevens','',accept = c(".csv")),
                         "Hieronder verschijnen ter controle  de eerste drie regels van de geuploade antwoordgegevens 
                         na het selecteren van het bestand (deelnemeraanduidingen zijn onzichtbaar):",
                         br(),
                         tableOutput("tab_antwoordgegevens_head"),
                         h5(strong("Vraaggegevens (optioneel)")),
                         "Het .csv-bestand met de vraaggegevens moet bestaan uit de kenmerken van alle toetsvragen.
                         Elke regel (horizontaal) staat voor een toetsvraag en elke kolom (verticaal) voor een 
                         kenmerk. De eerste kolom moet 'vraagaanduiding' heten (zonder aanhalingstekens) en 
                         bestaan uit de unieke vraagaanduidingen uit de antwoordgegevens.
                         De tweede kolom moet 'maxscore' heten en moet bestaan uit de maximaal te halen scoren op elke vraag (overal 1
                         wanneer alle vragen even zwaar tellen, bij verschillende wegingen van vragen of vragen waarbij deelscores kunnen
                         worden behaald ook waarden groter dan 1). De derde kolom moet 'sleutel' heten en moet bestaan uit de sleutel 
                         voor elke vraag; het antwoord in de antwoordgegevens
                         dat goedgerekend moet worden (bijvoorbeeld A, B, C of D). De vierde kolom moet 'vraagtype' heten en bestaan uit 
                         het vraagtype van elke vraag: KA voor voor een kort (open) antwoord (gokkans 0), DS voor een vraag waarop
                         deelscores kunnen worden gehaald (gokkans 0), MC2 voor 
                         tweekeuzevragen (gokkans 50 procent), MC3 voor driekeuzevragen (gokkans 33 procent) en 
                         MC4 voor vierkeuzevragen (gokkans 25 procent). De vijfdde kolom moet 'onderdeel' heten en bestaan uit het onderdeel van de toets waar de vraag deel van
                         uitmaakt. Hierbij kan elke gewenste benoeming worden gebruikt (bijvoorbeeld 'spelling',
                         'grammatica' en 'literatuur') of, wanneer een uitsplitsing in de analyse niet nodig is,
                         voor de hele toets dezelfde benoeming (bijvoorbeeld 'gehele toets').",
                         fileInput('vraaggegevens', '',accept = c(".csv")),
                         "Hieronder verschijnen ter controle  de eerste drie regels van de geuploade vraaggegevens 
                         na het selecteren van het bestand, of van de uit de antwoordgegevens afgeleide vraaggegevens:",
                         br(),
                         tableOutput("tab_vraaggegevens_head"),
                         "Hieronder verschijnen ter controle  de eerste drie regels van de door het systeem aan de
                         hand van de vraaggegevens nagekeken antwoordgegevens:",
                         br(),
                         tableOutput("tab_accgegevens_head"),
                         h5(strong("Deelnemergegevens (optioneel)")),
                         "Het .csv-bestand met de deelnemergegevens moet de groepsindeling van alle toetsdeelnemers
                         bevatten. De eerste kolom moet 'deelnemer' heten (zonder aanhalingstekens) en 
                         bestaan uit de unieke deelnemeraanduidingen uit de antwoordgegevens. De tweede kolom 
                         moet 'groep' heten en voor elke toetsdeelnemer weergeven tot welke groep deze behoort.",
                         fileInput('deelnemergegevens', '',accept = c(".csv")),
                         "Hieronder verschijnen ter controle  de eerste drie regels van de geuploade deelnemergegevens 
                         na het selecteren van het bestand, of van de uit de antwoordgegevens afgeleide deelnemergegevens:",
                         br(),
                         tableOutput("tab_deelnemergegevens_head")),
                tabPanel("Totaalscores en cesuur",
                         br(),
                         "In het onderstaande ",a("histogram", href="http://www.wisfaq.nl/pagina.asp?nummer=1738", target="_blank")," wordt de verdeling
                         weergegeven van de door toetsdeelnemers behaalde totaalscores op de toets. De streeplijn geeft
                         de cesuur weer: de score die deelnemers tenminste moeten halen om te slagen.
                         Onder het histogram kan gekozen worden tussen verschillende cesuurbepalingsmethodes in het
                         uitklapmenu. Bij de absolute methode en de ",a("methode Cohen-Schotanus", href="http://media.leidenuniv.nl/legacy/cijferberekeningtentamens-versie-050312.pdf", target="_blank")," kan het vereiste percentage 
                         goed worden aangepast, bij de relatieve methode het vereiste 
                         slagingspercentage, bij de", a("methode Hofstee", href="https://www.google.nl/url?sa=t&rct=j&q=&esrc=s&source=web&cd=5&ved=0ahUKEwjEm-S6w8_XAhUSFuwKHfOtA28QFghCMAQ&url=http%3A%2F%2Fwww.cito.nl%2F~%2Fmedia%2Fcito_nl%2Ffiles%2Fonderzoek%2520en%2520wetenschap%2Fpsychometrie_in_de_praktijk%2Fcito_pidp_h13.ashx&usg=AOvVaw0_xSDnpUI92BLAHNPH-JmH", target="_blank")," de bandbreedtes van acceptabele percentages gezakten en cesuren, en er kan ook een eigen cesuur worden ingevoerd.
                         Bij de absolute methode en de methode Cohen-Schotanus wordt gecorrigeerd voor de gokscore.",
                         br(),
                         "Boven elke staaf van een behaalde totaalscore staat het corresponderende cijfer bij de gekozen cesuur. Er worden geen cijfers
                         lager dan 1 toegekend.",
                         h5(strong("Verdeling totaalscores")),
                         textOutput("tekst_slaag"),
                         plotOutput("plot_totaalscore"),
                         br(),
                         fluidRow(
                           column(6,selectInput("cesuurmethode", 
                                                label = "Gewenste cesuurmethode:",
                                                choices = c("absoluut (percentage score)",
                                                            "relatief (slagingspercentage)",
                                                            "Cohen-Schotanus",
                                                            "Hofstee",
                                                            "eigen cesuur opgeven"),
                                                selected = "absoluut (percentage score)"),
                                  conditionalPanel(
                                    condition = "input.cesuurmethode == 'absoluut (percentage score)'",
                                    sliderInput("cesuur_absoluut", 
                                                label = "Vereist percentage van maximaal haalbare score:",
                                                min=0, max=100, value=60)),
                                  conditionalPanel(
                                    condition = "input.cesuurmethode == 'relatief (slagingspercentage)'",
                                    sliderInput("cesuur_relatief", 
                                                label = "Vereist slagingspercentage:",
                                                min=0, max=100, value=75)),
                                  conditionalPanel(
                                    condition = "input.cesuurmethode == 'Cohen-Schotanus'",
                                    sliderInput("cesuur_cohen", 
                                                label = "Vereist percentage goede antwoorden (van score op 95ste percentiel):",
                                                min=0, max=100, value=60)),
                                  conditionalPanel(
                                    condition = "input.cesuurmethode == 'Hofstee'",
                                    sliderInput("cesuur_hofstee_zak", 
                                                label = "Bandbreedte acceptabele percentages gezakten:",
                                                min=0, max=100, value=c(0,50)),
                                    uiOutput("cesuur_hofstee_score")),
                                  conditionalPanel(
                                    condition = "input.cesuurmethode == 'eigen cesuur opgeven'",
                                    numericInput("cesuur_eigen", 
                                                 label = "Eigen cesuur (aantal vragen)", 
                                                 min = 0, max = 1000, value=0))),
                           column(6, selectInput("cijfermethode", 
                                                 label = "Bepaling van onvoldoende cijfers:",
                                                 choices = c("totaalscores niet hoger dan gokscore krijgen 1",
                                                             "zelfde aantal vragen per punt als bij voldoende cijfers"),
                                                 selected = "totaalscores niet hoger dan gokscore krijgen 1"),
                                  numericInput("cijfer_decimalen", 
                                               label = "Aantal decimalen cijfers", 
                                               min = 0, max = 3, value=0))),
                         plotOutput("plot_hofstee", width="50%")
                ),
                tabPanel("Toetsonderdelen",
                         br(),
                         "Wanneer vraaggegevens zijn geupload waarin onderscheid wordt gemaakt tussen 
                         verschillende onderdelen van de toets, worden hier resultaten weergegeven over
                         scores per onderdeel en de samenhang tussen deze onderdelen.",
                         h5(strong("Verdeling scores per onderdeel van de toets")),
                         "In de onderstaande ",a("histogrammen", href="http://www.wisfaq.nl/pagina.asp?nummer=1738", target="_blank"),"wordt 
                         de verdeling weergegeven van de door toetsdeelnemers behaalde score 
                         per toetsonderdeel. Bij elk onderdeel staat ook het gemiddeld behaalde percentage van de maximale score.",
                         plotOutput("plot_deelscores"),
                         h5(strong("Correlaties tussen scores per toetsonderdeel")),
                         "De scores van toetsdeelnemers per toetsonderdeel kunnen aan elkaar gerelateerd worden
                         door middel van ",a("correlaties", href="https://www.tilburguniversity.edu/nl/studenten/studie/colleges/spsshelpdesk/edesk/correlat/", target="_blank"),".
                         In onderstaande grafiek worden deze correlaties weergegeven.",
                         plotOutput("plot_coronderdelen"),
                         h5(strong("Correlaties tussen scores per vraag")),
                         "Ook de scores per vraag kunnen aan elkaar gerelateerd worden met correlaties.
                         Wanneer toetsonderdelen elk een samenhangend geheel vormen, zijn de correlaties tussen vragen
                         binnen die onderdelen (zwarte getallen) hoog. Bij een hoog totaalaantal vragen in 
                         de toets kan deze grafiek moeilijk leesbaar zijn; kies dan 'Deelnemer- en vraaggegevens in Excel' om alle correlaties te zien.",
                         plotOutput("plot_corvragen", height=800)),
                
                tabPanel("Eigenschappen vragen",
                         h5(strong("Overzicht moeilijkheidsgraad en onderscheidend vermogen toetsvragen")),
                         "In onderstaand figuur worden de statistische eigenschappen van de toetsvragen weergegeven.
                         Vragen die in een rood gebied vallen zijn of moeilijk of erg makkelijk voor de 
                         toetsdeelnemers (bijvoorbeeld wanneer minder dan 40% of meer dan 95% heeft de vraag goed heeft), 
                         of hebben een laag onderscheidend vermogen (bijvoorbeeld wanneer de item-rest-correlatie lager dan 0.10 is).",
                         br(),
                         "De relatieve moeilijkheidsgraad van toetsvragen wordt hierbij uitgedrukt in
                         p-waardes: de proportie toetsdeelnemers die een vraag correct heeft beantwoord.
                         Bij vragen met deelscores wordt in plaats hiervan de gemiddeld behaalde proportie van de maximale score weergegeven.
                         Het onderscheidend vermogen van vragen wordt uitgedrukt in item-rest-correlaties:
                         de samenhang tussen de score op een vraag en op de rest van de vragen in de toets. Bij
                         vragen die goed in de toets als geheel passen, is deze samenhang positief (ideaal gezien
                         groter dan 0.30).",
                         br(),
                         br(),
                         fluidRow(
                           column(7,plotOutput("plot_gulliksen")),
                           column(3, br(),
                                  sliderInput("gulliksen_pgrenzen", 
                                              label = "Minimale en maximale p-waarde:",
                                              min=0, max=1, value=c(.4,.95)),
                                  br(),
                                  sliderInput("gulliksen_rirgrens", 
                                              label = "Minimale item-rest-correlatie:",
                                              min=0, max=.4, value=.1))),
                         h5(strong("Noodzakelijk nader te bekijken vragen")),
                         "Bij bepaalde statistische eigenschappen is het inhoudelijk nader bekijken van toetsvragen
                         noodzakelijk. Dit is zo wanneer de p-waarde van een vraag lager is dan de
                         gokkans, want dan hebben deelnemers een vraag minder vaak goed dan op basis van puur gokken.
                         Het verwijderen van de vraag uit de toets kan in dit geval gewenst zijn. Ook bij een negatieve
                         item-rest-correlatie moet nader onderzoek worden verricht, want dan beantwoorden toetsdeelnemers
                         die op de rest van de toets slechter scoren de vraag juist vaker goed. Er kan dan bijvoorbeeld
                         sprake zijn van een sleutelfout of een misleidende vraagstelling.",
                         tableOutput("tab_zekernaderbekijken"),
                         h5(strong("Andere nader te bekijken vragen")),
                         "Ook bij andere vragen die in een rood gebied vallen is inhoudelijk nader bekijken
                         gewenst, al dan niet voor aanknopingspunten voor aanpassingen voor 
                         hergebruik in een volgende toets.",
                         tableOutput("tab_gewenstnaderbekijken")),
                
                tabPanel("Per vraag",
                         br(),
                         "In de onderstaande tabel en grafiek wordt voor de geselecteerde vraag meer informatie gegeven 
                         over hoe vaak elk antwoord gekozen is en hoe dit samenhangt met de score behaald op de 
                         rest van de toetsvragen. Bij goede antwoorden wordt een positieve correlatie ('rirofrar') verwacht
                         en bij foute antwoorden een negatieve correlatie.",
                         "In de grafiek zijn de deelnemers in drie groepen verdeeld op basis van de hoogte
                         van hun totaalscore. Het goede antwoord is gemarkeerd met ruiten en vertoont bij een positieve 
                         item-rest-correlatie een stijgende lijn.",
                         br(),
                         "Onder de gegevens over de antwoorden kan worden gekozen voor een afwijkende scoring van de vraag
                         of van een specifiek antwoord. Deze wijzigingen worden doorgevoerd bij het bepalen van de scores 
                         van deelnemers, de cesuur (de gokscore wordt aangepast aan het aantal goed te rekenen antwoorden),
                         de scores per groep toetsdeelnemers en de betrouwbaarheid. De scores per toetsonderdeel (niet per groep)
                         en de statistische eigenschappen van de vragen zijn gebaseerd op de oorspronkelijke toets zonder wijzigingen.",
                         br(),
                         br(),
                         "LET OP: HERLAAD BIJ ERROR DE TOOL (F5) EN PROBEER OPNIEUW",
                         br(),
                         br(),
                         h5(strong(textOutput("tekst_vraagwijzigingen"))),
                         tableOutput("tab_vraagwijzigingen"),
                         br(),
                         uiOutput("vragenlijst"),
                         fluidRow(
                           column(4, textOutput("tekst_vraagtype"),
                                  br(),
                                  tableOutput("tab_antwoorden")),
                           column(6, plotOutput("plot_item"))),
                         br(),
                         fluidRow(
                           column(4,radioButtons("vraagwijziging","Wijziging in vraag",
                                                 c("meetellen in oorspronkelijke vorm",
                                                   "laten vervallen uit de toets",
                                                   "alle antwoorden goedrekenen",
                                                   "extra antwoord goedrekenen",
                                                   "ander antwoord goedrekenen"),
                                                 "meetellen in oorspronkelijke vorm")),
                           column(4, offset = 1,conditionalPanel(
                             condition = "input.vraagwijziging == 'extra antwoord goedrekenen'|input.vraagwijziging == 'ander antwoord goedrekenen'",
                             uiOutput("goedrekenen"))))),
                tabPanel("Groepen",
                         br(),
                         "Wanneer deelnemergegevens zijn geupload waarin onderscheid wordt gemaakt tussen 
                         verschillende groepen toetsdeelnemers, worden hier resultaten weergegeven over
                         scores per groep.",
                         h5(strong("Overzicht resultaten per groep toetsdeelnemers")),
                         "In de onderstaande tabel wordt weergegeven uit hoeveel toetsdeelnemers elke groep 
                         bestaat en hoeveel procent daarvan slaagt bij de gekozen cesuur. Daarnaast worden
                         de gemiddelde cijfers en totaalscores per groep weergegeven, en als onderscheid 
                         wordt gemaakt tussen verschillende toetsonderdelen, de gemiddelde score per 
                         toetsonderdeel.",
                         tableOutput("tab_groepen"),
                         h5(strong("Verdeling totaalscores per groep toetsdeelnemers")),
                         "In het onderstaande ",a("histogram", href="http://www.wisfaq.nl/pagina.asp?nummer=1738", target="_blank"),"wordt 
                         de verdeling weergegeven van de totaalscores van groepen toetseelnemers. De staven zijn doorschijnend,
                         zodat de verdelingen kunnen worden vergeleken.",
                         plotOutput("plot_groepscores"),
                         h5(strong("Gemiddelde score per toetsonderdeel per groep toetsdeelnemers")),
                         plotOutput("plot_groependeelscores")),
                tabPanel("Betrouwbaarheid",
                         br(),
                         "Met een toets worden verschillen tussen toetsdeelnemers in de beheersing van leerdoelen in kaart gebracht. Toetsscores
                         geven echter niet perfect de verschillen in beheersing weer; zij bevatten ook toevallige meetfouten. Hoe kleiner het aandeel
                         van meetfouten in de scores, hoe ",a("betrouwbaarder", href="https://www.psynip.nl/wp-content/uploads/2016/07/COTAN-Beoordelingssysteem-2010.pdf",target="_blank")," de toets. Bij een betrouwbare toets behalen toetsdeelnemers 
                         (bij gelijkblijvende beheersing) bij hermeting een vergelijkbaar resultaat.",
                         br(),
                         "Een gangbare manier om de betrouwbaarheid van toets te kwantificeren, is met Cronbachs alfa. Bij toetsen op 
                         basis waarvan belangrijke beslissingen op individueel niveau worden genomen, worden hierbij waardes van minimaal 0.80 
                         als voldoende",a("beschouwd", href="https://www.psynip.nl/wp-content/uploads/2016/07/COTAN-Beoordelingssysteem-2010.pdf",target="_blank"),"en waardes van minimaal 0.90 als goed. Bij minder belangrijke beslissingen op 
                         individueel niveau (bijvoorbeeld bij voortgangscontrole) worden waardes van minimaal 0.70 als voldoende en
                         minimaal 0.80 als goed beschouwd. Bij lagere waarden is de betrouwbaarheid onvoldoende en zullen er veel 
                         toetsdeelnemers ten onrechte zakken of slagen.",textOutput("tekst_alfa2"),
                         br(),
                         h5(strong("Betrouwbaarheid bij ander aantal toetsvragen")),
                         "De betrouwbaarheid van een toets neemt toe bij het verlengen van de toets met vergelijkbare vragen en neemt 
                         af bij verkorten. Met de",  
                         a("Spearman-Brown-formule",href="https://en.wikipedia.org/wiki/Spearman%E2%80%93Brown_prediction_formula",target="_blank"),
                         "kan een voorspelling worden gemaakt van de betrouwbaarheid bij een ander aantal vergelijkbare vragen.",textOutput("tekst_sb"),
                         br(),
                         br(),
                         fluidRow(
                           column(4, numericInput("spearbrown_nvragen",
                                                  label = "Aantal vragen voor voorspelling:",
                                                  min = 0, max = 1000, value=0)),
                           column(4, tableOutput("tab_spearbrown"))),
                         h5(strong("Vragen met een negatieve bijdrage aan de betrouwbaarheid")),
                         "Niet alle toetsvragen hoeven een postitieve bijdrage te leveren aan de betrouwbaarheid van een toets. Wanneer een vraag slecht in de toets past, 
                         kan het uit de toets verwijderen van die vraag de betrouwbaarheid van de toets verhogen. Afhankelijk van de inhoud
                         van de vraag en resterende toetsvragen kan dit echter wel de validiteit (inhoudsdekking) van de toets beinvloeden, dus
                         neem deze altijd in overweging voor verwijdering.",
                         br(),
                         br(),
                         tableOutput("tab_alfavragen_slecht"),
                         h5(strong("Vragen met de grootste positieve bijdrage aan de betrouwbaarheid")),
                         "Vragen die goed in de toets passen en waarvan verwijdering uit de toets de betrouwbaarheid zou verlagen,
                         kunnen waardevolle informatie bieden voor de constructie van succesvolle vragen in de toekomst.",
                         br(),
                         br(),
                         tableOutput("tab_alfavragen_goed")
                         )
                )
    )
  )

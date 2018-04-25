library(shiny)
library(ggplot2)

debug <- T

function(input, output) {
  
 antwoordgegevens <- reactive({
   if(debug){cat("antwoordgegevens\n")}
    inFile <- input$antwoordgegevens
    
    if (is.null(inFile))
      return(NULL)
    
    antwoordgegevens <- read.csv(inFile$datapath, header=T, sep=";")
    antwoordgegevens <- data.frame(antwoordgegevens[,-1],row.names=antwoordgegevens[,1])
    antwoordgegevens
  })
  
 output$fileUploaded <- reactive({#om in linkerpanel niets weer te geven als nog niks geupload
   if(debug){cat("output$fileUploaded\n")}
   return(!is.null(antwoordgegevens()))
 })
 outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE) #om in linkerpanel niets weer te geven als nog niks geupload
 
  vraaggegevens <- reactive({
    if(debug){cat("vraaggegevens\n")}
    inFile_vraaggegevens <- input$vraaggegevens
    
    if(is.null(inFile_vraaggegevens)){
      vraaggegevens <- data.frame(vraagaanduiding=colnames(antwoordgegevens()),
                                  maxscore=1,
                                  sleutel=1,
                                  vraagtype="KA",
                                  onderdeel="gehele toets",
                                  gokkans=0)
    }else{
      vraaggegevens <- read.csv(inFile_vraaggegevens$datapath, header=T, sep=";")
      vraaggegevens[vraaggegevens$vraagtype=="KA","gokkans"] <- 0
      vraaggegevens[vraaggegevens$vraagtype=="DS","gokkans"] <- 0
      vraaggegevens[vraaggegevens$vraagtype=="MC2","gokkans"] <- 1/2
      vraaggegevens[vraaggegevens$vraagtype=="MC3","gokkans"] <- 1/3
      vraaggegevens[vraaggegevens$vraagtype=="MC4","gokkans"] <- 1/4
      vraaggegevens[vraaggegevens$vraagtype=="MC5","gokkans"] <- 1/5
      vraaggegevens[vraaggegevens$vraagtype=="MC6","gokkans"] <- 1/6
      vraaggegevens[vraaggegevens$vraagtype=="MC7","gokkans"] <- 1/7
      vraaggegevens[vraaggegevens$vraagtype=="MC8","gokkans"] <- 1/8
    }
    
    vraagwijz <- data.frame(vraagaanduiding=vraaggegevens$vraagaanduiding,
                            vervallen="nee",
                            allesgoedrekenen="nee",
                            extragoedrekenen=NA,
                            andergoedrekenen=NA)
    
    write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)
    
    vraaggegevens
  })
  
  gokscore <- reactive({
    if(debug){cat("gokscore\n")}
    sum(vraaggegevens()$maxscore*vraaggegevens()$gokkans)
  })
  
  accgegevens <- reactive({
    if(debug){cat("accgegevens\n")}
    accgegevens <- data.frame(matrix(NA, ncol=ncol(antwoordgegevens()), nrow=nrow(antwoordgegevens())))
    colnames(accgegevens) <- colnames(antwoordgegevens())
    for(r in 1:ncol(accgegevens)){
      if(vraaggegevens()[vraaggegevens()$vraagaanduiding==colnames(accgegevens)[r],"vraagtype"]!="DS"){
        maxscore <- vraaggegevens()[vraaggegevens()$vraagaanduiding==colnames(accgegevens)[r],"maxscore"]
        sleutel <- vraaggegevens()[vraaggegevens()$vraagaanduiding==colnames(accgegevens)[r],"sleutel"]
        accgegevens[,r] <- maxscore*as.numeric(antwoordgegevens()[,r]==as.character(sleutel))}
      if(vraaggegevens()[vraaggegevens()$vraagaanduiding==colnames(accgegevens)[r],"vraagtype"]=="DS"){
        accgegevens[,r] <- antwoordgegevens()[,r]}
    }
    
    accgegevens[is.na(accgegevens)] <- 0
    
    #cat(mode(accgegevens[,1]),"\n\n")
    #cat(sum(is.na(accgegevens)),"\n\n")
    
    accgegevens
  })
  
  vraagwijzigingen <- reactive({
    if(debug){cat("vraagwijzigingen\n")}
    vraagwijz <- read.csv("vraagwijzigingen.csv",header=T,sep=";")

    vraagwijz$vervallen <- factor(vraagwijz$vervallen, levels=c("nee","ja"))
    vraagwijz$allesgoedrekenen <- factor(vraagwijz$allesgoedrekenen, levels=c("nee","ja"))
    vraagwijz$extragoedrekenen <- factor(vraagwijz$extragoedrekenen, levels=unique(unlist(antwoordgegevens())))
    vraagwijz$andergoedrekenen <- factor(vraagwijz$andergoedrekenen, levels=unique(unlist(antwoordgegevens())))
    
    vraagsel <- strsplit(as.character(input$vraagselectie)," ")[[1]][1]
    if(input$vraagwijziging=="meetellen in oorspronkelijke vorm"){
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"vervallen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"allesgoedrekenen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"extragoedrekenen"] <- NA
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"andergoedrekenen"] <- NA
      write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)}
    if(input$vraagwijziging=="laten vervallen uit de toets"){
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"vervallen"] <- "ja"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"allesgoedrekenen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"extragoedrekenen"] <- NA
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"andergoedrekenen"] <- NA
      write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)}
    if(input$vraagwijziging=="alle antwoorden goedrekenen"){
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"vervallen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"allesgoedrekenen"] <- "ja"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"extragoedrekenen"] <- NA
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"andergoedrekenen"] <- NA
      write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)}
    if(input$vraagwijziging=="extra antwoord goedrekenen"){
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"vervallen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"allesgoedrekenen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"extragoedrekenen"] <- input$goedrekenen
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"andergoedrekenen"] <- NA
      write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)}
    if(input$vraagwijziging=="ander antwoord goedrekenen"){
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"vervallen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"allesgoedrekenen"] <- "nee"
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"extragoedrekenen"] <- NA
      vraagwijz[vraagwijz$vraagaanduiding==vraagsel,"andergoedrekenen"] <- input$goedrekenen
      write.table(vraagwijz,"vraagwijzigingen.csv",sep=";",row.names=F)}
    
    
    vraagwijz
  })
  
  output$fileDownloadklaar <- reactive({#om in linkerpanel niets weer te geven als nog niks geupload
    if(debug){cat("output$fileDownloadklaar\n")}
    return(!is.null(vraagwijzigingen()))
  })
  outputOptions(output, 'fileDownloadklaar', suspendWhenHidden=FALSE) #om in linkerpanel niets weer te geven als nog niks geupload
  
  
  vraagwijzigingen_ja <- reactive({
    if(debug){cat("vraagwijzigingen_ja\n")}
    wijzigingen <- vraagwijzigingen()[vraagwijzigingen()$vervallen=="ja"|vraagwijzigingen()$allesgoedrekenen=="ja"|!is.na(vraagwijzigingen()$extragoedrekenen)|!is.na(vraagwijzigingen()$andergoedrekenen),]
    if(nrow(wijzigingen)==0){return(NULL)}else{wijzigingen}
  })
  
  accgegevens_wijz <- reactive({
    if(debug){cat("accgegevens_wijz\n")}
    acc <- accgegevens()
    
    if(!is.null(input$vraagselectie)){
      for(c in 1:ncol(accgegevens())){
        vraagaanduiding <- colnames(accgegevens())[c]
        maxscore <- as.numeric(as.character(vraaggegevens()[vraaggegevens()$vraagaanduiding==as.character(vraagaanduiding),"maxscore"]))
        if(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"vervallen"]=="ja"){
          acc <- acc[,-which(colnames(acc)==vraagaanduiding)]}
        if(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"allesgoedrekenen"]=="ja"){
          acc[,which(colnames(acc)==vraagaanduiding)] <- maxscore}
        if(!is.na(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"extragoedrekenen"])){
          acc[antwoordgegevens()[,as.character(vraagaanduiding)]==as.character(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"extragoedrekenen"]),as.character(vraagaanduiding)] <- maxscore}
        if(!is.na(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"andergoedrekenen"])){
          acc[,as.character(vraagaanduiding)] <- 0
          acc[antwoordgegevens()[,as.character(vraagaanduiding)]==as.character(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"andergoedrekenen"]),as.character(vraagaanduiding)] <- maxscore}
      }}
    
    acc
  })
  
  vraaggegevens_wijz <- reactive({
    if(debug){cat("vraaggegevens_wijz\n")}
    vraag <- vraaggegevens()
    vraag$gokkans <- as.numeric(as.character(vraag$gokkans))
    vraag$sleutel <- as.character(vraag$sleutel)
    
    if(!is.null(input$vraagselectie)){
      for(c in 1:nrow(vraaggegevens())){
        vraagaanduiding <- vraaggegevens()$vraagaanduiding[c]
        if(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"vervallen"]=="ja"){
          vraag <- vraag[-which(vraag$vraagaanduiding==vraagaanduiding),]}
        if(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"allesgoedrekenen"]=="ja"){
          vraag[vraag$vraagaanduiding==vraagaanduiding,"gokkans"] <- 1
          vraag[vraag$vraagaanduiding==vraagaanduiding,"sleutel"] <- "alle antwoorden"}
        if(!is.na(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"extragoedrekenen"])){
          vraag[vraag$vraagaanduiding==vraagaanduiding,"gokkans"] <- 2*vraag[vraag$vraagaanduiding==vraagaanduiding,"gokkans"]
          vraag[vraag$vraagaanduiding==vraagaanduiding,"sleutel"] <- paste(vraag[vraag$vraagaanduiding==vraagaanduiding,"sleutel"],
                                                                           "&",vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"extragoedrekenen"])}
        if(!is.na(vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"andergoedrekenen"])){
          vraag[vraag$vraagaanduiding==vraagaanduiding,"sleutel"] <- vraagwijzigingen()[vraagwijzigingen()$vraagaanduiding==as.character(vraagaanduiding),"andergoedrekenen"]}
      }}
    
    vraag
  })
  
  gokscore_wijz <- reactive({
    if(debug){cat("gokscore_wijz\n")}
    sum(vraaggegevens_wijz()$maxscore*vraaggegevens_wijz()$gokkans)
  })
  
  deelnemergegevens_incompleet <- reactive({
    if(debug){cat("deelnemergegevens_incompleet\n")}
    if(is.null(input$deelnemergegevens)){
      deelnemergegevens_incompleet <- data.frame(deelnemer=rownames(antwoordgegevens()))
      deelnemergegevens_incompleet$groep <- "alle toetsdeelnemers"
    }else{
      deelnemergegevens_incompleet <- read.csv(input$deelnemergegevens$datapath, header=T, sep=";")
    }
    
    deelnemergegevens_incompleet$totaalscore <- apply(accgegevens(),1,sum)
    
    onderdelen <- unique(vraaggegevens()$onderdeel)
    onderdeelscores <- data.frame(matrix(NA, nrow=nrow(accgegevens()), ncol=length(onderdelen)))
    for(o in 1:length(onderdelen)){
      vragen <- vraaggegevens()[vraaggegevens()$onderdeel==onderdelen[o],"vraagaanduiding"]
      acc_onderdeel <- accgegevens()[,colnames(accgegevens()) %in% vragen]
      onderdeelscores[,o] <- apply(acc_onderdeel,1,sum)
    }
    names(onderdeelscores) <- onderdelen
    
    deelnemergegevens_incompleet <- cbind(deelnemergegevens_incompleet, onderdeelscores)
    
    deelnemergegevens_incompleet
  })
  
  cesuur <- reactive({
    if(debug){cat("cesuur\n")}
    cesuurmethode <- input$cesuurmethode
    
    if(cesuurmethode=="absoluut (percentage score)"){
      cesuur <- ceiling((input$cesuur_absoluut/100)*(sum(vraaggegevens()$maxscore)-gokscore())+gokscore())}
    if(cesuurmethode=="relatief (slagingspercentage)"){
      slaagbijcesuur <- data.frame(cesuur=0:sum(vraaggegevens()$maxscore), slaag=NA)
      for(r in 1:nrow(slaagbijcesuur)){
        slaagbijcesuur$slaag[r] <- nrow(deelnemergegevens_incompleet()[deelnemergegevens_incompleet()$totaalscore>=slaagbijcesuur$cesuur[r],])/nrow(deelnemergegevens_incompleet())}
      cesuur <- max(slaagbijcesuur[slaagbijcesuur$slaag>=(input$cesuur_relatief/100),"cesuur"])}
    if(cesuurmethode=="Cohen-Schotanus"){
      maxscore_cohen <- sort(deelnemergegevens_incompleet()$totaalscore)[ceiling(0.95*length(deelnemergegevens_incompleet()$totaalscore))]
      cesuur <- ceiling(gokscore() + input$cesuur_cohen/100*(maxscore_cohen - gokscore()))}
    if(cesuurmethode=="Hofstee"){
      hofsteegrenzen <- list(ondergrens_vragen=input$cesuur_hofstee_score[1], bovengrens_vragen=input$cesuur_hofstee_score[2], 
                             ondergrens_gezakt=input$cesuur_hofstee_zak[1], bovengrens_gezakt=input$cesuur_hofstee_zak[2])
      
      hofsteedat <- data.frame(totaalscore=c(0:sum(vraaggegevens()$maxscore)), gezakt=NA)
      for(s in 1:nrow(hofsteedat)){
        hofsteedat$gezakt[s] <- sum(deelnemergegevens_incompleet()$totaalscore<hofsteedat$totaalscore[s])/length(deelnemergegevens_incompleet()$totaalscore)}
      
      hofsteelijn_slope <- (hofsteegrenzen$ondergrens_gezakt/100-hofsteegrenzen$bovengrens_gezakt/100)/(hofsteegrenzen$bovengrens_vragen-hofsteegrenzen$ondergrens_vragen)
      hofsteelijn_intercept <- hofsteegrenzen$ondergrens_gezakt/100 - hofsteelijn_slope*hofsteegrenzen$bovengrens_vragen
      
      hofsteedat$hofsteelijn <- hofsteelijn_intercept + hofsteelijn_slope*hofsteedat$totaalscore
      hofsteedat$verschil <- hofsteedat$gezakt-hofsteedat$hofsteelijn
      hofsteedat$verschil_abs <- abs(hofsteedat$verschil)
      
      cesuur <- hofsteedat[hofsteedat$verschil_abs==min(hofsteedat$verschil_abs)[1],"totaalscore"]}
    if(cesuurmethode=="eigen cesuur opgeven"){
      cesuur <- input$cesuur_eigen}
    
    cesuur
  })
  
  deelnemergegevens <- reactive({
    if(debug){cat("deelnemergegevens\n")}
    deelnemergegevens <- deelnemergegevens_incompleet()
    deelnemergegevens$slaag <- "gezakt"
    deelnemergegevens[deelnemergegevens$totaalscore>=cesuur(),"slaag"] <- "geslaagd"
    deelnemergegevens$slaag <- factor(deelnemergegevens$slaag, levels=c("gezakt","geslaagd"))
    
    #bij elke totaalscore een cijfer en percentiel (aparte formule voor onvoldoende en voldoende)
    transformatie <- data.frame(totaalscore=c(0:sum(vraaggegevens()$maxscore)), percentiel=NA)
    for(r in 1:nrow(transformatie)){
      transformatie$percentiel[r] <- nrow(deelnemergegevens_incompleet()[deelnemergegevens_incompleet()$totaalscore<=transformatie$totaalscore[r],])/nrow(deelnemergegevens_incompleet())}
    
    transformatie$cijfer_onvoldoende <- round(5.4999 - 4.5*(transformatie$totaalscore - cesuur())/(gokscore()-cesuur()),input$cijfer_decimalen)
    transformatie$cijfer_voldoende <- round(5.5001 + 4.5*(transformatie$totaalscore - cesuur())/(sum(vraaggegevens()$maxscore) - cesuur()),input$cijfer_decimalen)
    
    if(input$cijfermethode=="totaalscores niet hoger dan gokscore krijgen 1"){
      transformatie[transformatie$totaalscore<cesuur(),"cijfer"] <- transformatie[transformatie$totaalscore<cesuur(),"cijfer_onvoldoende"]
      transformatie[transformatie$totaalscore>=cesuur(),"cijfer"] <- transformatie[transformatie$totaalscore>=cesuur(),"cijfer_voldoende"]
      transformatie[transformatie$totaalscore<gokscore(),"cijfer"] <- 1}
    if(input$cijfermethode=="zelfde aantal vragen per punt als bij voldoende cijfers"){
      transformatie$cijfer <- transformatie$cijfer_voldoende
      transformatie[transformatie$cijfer<1,"cijfer"] <- 1}
    deelnemergegevens$cijfer <- NA
    for(r in 1:nrow(transformatie)){
      deelnemergegevens[deelnemergegevens$totaalscore==transformatie$totaalscore[r],"percentiel"] <- transformatie$percentiel[r]
      deelnemergegevens[deelnemergegevens$totaalscore==transformatie$totaalscore[r],"cijfer"] <- transformatie$cijfer[r]} 
    
    deelnemergegevens
  })
  
  itemgegevens <- reactive({
    if(debug){cat("itemgegevens\n")}
    itemgegevens <- vraaggegevens()
    
    itemgegevens$onderdeelvraag <- paste0(itemgegevens$onderdeel,": ",itemgegevens$vraagaanduiding)
    itemgegevens$p <- apply(accgegevens(),2,mean)/vraaggegevens()$maxscore
    
    restscores <- apply(accgegevens(),2,function(x) deelnemergegevens()$totaalscore - x)
    rirwaardes <- NA
    for(c in 1:ncol(accgegevens())){
      rirwaardes[c] <- cor(accgegevens()[,c],restscores[,c])} 
    itemgegevens$rir <- rirwaardes
    
    itemgegevens$alfa_als_verwijderd <- NA
    for(r in 1:nrow(itemgegevens)){
      itemgegevens$alfa_als_verwijderd[r] <- round(as.numeric(cronbach(accgegevens()[,-r])[3]),2)}
    
    itemgegevens
  })
  
  itemgegevens_commentaar <- reactive({
    if(debug){cat("itemgegevens_commentaar\n")}
    itemgegevens_commentaar <- data.frame(vraagaanduiding=itemgegevens()$vraagaanduiding,
                                          pwaarde=itemgegevens()$p,
                                          itemrestcorrelatie=itemgegevens()$rir)
    
    itemgegevens_commentaar$probleem <- ""
    itemgegevens_commentaar[itemgegevens()$p>=input$gulliksen_pgrenzen[1]&itemgegevens()$p<=input$gulliksen_pgrenzen[2]&itemgegevens()$rir<input$gulliksen_rirgrens,"probleem"] <- "laag onderscheidend vermogen"
    itemgegevens_commentaar[itemgegevens()$p<input$gulliksen_pgrenzen[1]&itemgegevens()$rir>=input$gulliksen_rirgrens,"probleem"] <- "moeilijk"
    itemgegevens_commentaar[itemgegevens()$p<input$gulliksen_pgrenzen[1]&itemgegevens()$rir<input$gulliksen_rirgrens,"probleem"] <- "moeilijk en laag onderscheidend vermogen"
    itemgegevens_commentaar[itemgegevens()$p>input$gulliksen_pgrenzen[2]&itemgegevens()$rir>=input$gulliksen_rirgrens,"probleem"] <- "erg makkelijk"
    itemgegevens_commentaar[itemgegevens()$p>input$gulliksen_pgrenzen[2]&itemgegevens()$rir<input$gulliksen_rirgrens,"probleem"] <- "erg makkelijk en laag onderscheidend vermogen"
    itemgegevens_commentaar[itemgegevens()$p<itemgegevens()$gokkans&itemgegevens()$rir>=0,"probleem"] <- "p-waarde onder de gokkans"
    itemgegevens_commentaar[itemgegevens()$p>=itemgegevens()$gokkans&round(itemgegevens()$rir,2)<0,"probleem"] <- "negatief onderscheidend vermogen"
    itemgegevens_commentaar[itemgegevens()$p<itemgegevens()$gokkans&round(itemgegevens()$rir,2)<0,"probleem"] <- "p-waarde onder de gokkans en negatief onderscheidend vermogen"
    
    itemgegevens_commentaar$vraagplusprob <- paste0(itemgegevens_commentaar$vraagaanduiding," (",itemgegevens_commentaar$probleem,")")
    itemgegevens_commentaar[itemgegevens_commentaar$probleem=="","vraagplusprob"] <- as.character(itemgegevens_commentaar[itemgegevens_commentaar$probleem=="","vraagaanduiding"])
    
    itemgegevens_commentaar$sleutel <- itemgegevens()$sleutel
    
    itemgegevens_commentaar
  })
  
  dat_itemalts <- reactive({
    if(debug){cat("dat_itemalts\n")}
    deelnemergegevens <- deelnemergegevens()
    groepsgrootte  <- length(deelnemergegevens$totaalscore)/3
    knip1_hi <- sort(deelnemergegevens$totaalscore)[groepsgrootte]
    knip1_lo <-  sort(unique(deelnemergegevens$totaalscore))[which(sort(unique(deelnemergegevens$totaalscore))==knip1_hi)-1]
    knip2_hi <- sort(deelnemergegevens$totaalscore)[2*groepsgrootte]
    knip2_lo <-  sort(unique(deelnemergegevens$totaalscore))[which(sort(unique(deelnemergegevens$totaalscore))==knip2_hi)-1]
    sum_hi <-sum((groepsgrootte-sum(deelnemergegevens$totaalscore<=knip1_hi))^2,
                 (groepsgrootte-sum(deelnemergegevens$totaalscore>knip1_hi&deelnemergegevens$totaalscore<=knip2_hi))^2,
                 (groepsgrootte-sum(deelnemergegevens$totaalscore>knip2_hi))^2)
    sum_lo <- sum((groepsgrootte-sum(deelnemergegevens$totaalscore<=knip1_lo))^2,
                  (groepsgrootte-sum(deelnemergegevens$totaalscore>knip1_lo&deelnemergegevens$totaalscore<=knip2_lo))^2,
                  (groepsgrootte-sum(deelnemergegevens$totaalscore>knip2_lo))^2)
    if(sum_hi<=sum_lo){knip1<-knip1_hi;knip2<-knip2_hi}else{knip1<-knip1_lo;knip2<-knip2_lo}
    deelnemergegevens$scoregroep <- NA
    deelnemergegevens[deelnemergegevens$totaalscore<=knip1,"scoregroep"] <- "laag"
    deelnemergegevens[deelnemergegevens$totaalscore>knip1&deelnemergegevens$totaalscore<=knip2,"scoregroep"] <- "middel"
    deelnemergegevens[deelnemergegevens$totaalscore>knip2,"scoregroep"] <- "hoog"
    
    laagstuds <- deelnemergegevens[deelnemergegevens$scoregroep=="laag","deelnemer"]
    middelstuds <- deelnemergegevens[deelnemergegevens$scoregroep=="middel","deelnemer"]
    hoogstuds <- deelnemergegevens[deelnemergegevens$scoregroep=="hoog","deelnemer"]
    alternatiefcodes <- paste0("A",1:nrow(vraaggegevens())) #voor max aantal verschillende antwoorden
    
    dat_itemalts <- data.frame(vraag=rep(itemgegevens()$vraagaanduiding,each=4*3), #dataframe met benodigde levels maken
                               vraagmetinfo=rep(paste0("Vraagtype: ",itemgegevens()$vraagtype,"\np-waarde: ",round(itemgegevens()$p,2),"\nItem-rest-correlatie: ",round(itemgegevens()$rir,2)),each=4*3),
                               scoregroep=c("laag","middel","hoog"),
                               alternatief=rep(alternatiefcodes[1:4],each=3), correct=NA,proportie=NA, antwoord=NA)
    
    for(r in 1:nrow(dat_itemalts)){
      if(itemgegevens()[itemgegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"vraagtype"]=="KA"|
         itemgegevens()[itemgegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"vraagtype"]=="DS"){
        if(dat_itemalts$alternatief[r]=="A1"){dat_itemalts$antwoord[r] <- as.character(names(sort(table(antwoordgegevens()[,dat_itemalts$vraag[r]]),decreasing=T))[1])}
        if(dat_itemalts$alternatief[r]=="A2"){dat_itemalts$antwoord[r] <- as.character(names(sort(table(antwoordgegevens()[,dat_itemalts$vraag[r]]),decreasing=T))[2])}
        if(dat_itemalts$alternatief[r]=="A3"){dat_itemalts$antwoord[r] <- as.character(names(sort(table(antwoordgegevens()[,dat_itemalts$vraag[r]]),decreasing=T))[3])}
        if(dat_itemalts$alternatief[r]=="A4"){dat_itemalts$antwoord[r] <- as.character(names(sort(table(antwoordgegevens()[,dat_itemalts$vraag[r]]),decreasing=T))[4])}
      }else{
        if(dat_itemalts$alternatief[r]=="A1"){dat_itemalts$antwoord[r] <- as.character(sort(unique(antwoordgegevens()[,dat_itemalts$vraag[r]]))[1])}
        if(dat_itemalts$alternatief[r]=="A2"){dat_itemalts$antwoord[r] <- as.character(sort(unique(antwoordgegevens()[,dat_itemalts$vraag[r]]))[2])}
        if(dat_itemalts$alternatief[r]=="A3"){dat_itemalts$antwoord[r] <- as.character(sort(unique(antwoordgegevens()[,dat_itemalts$vraag[r]]))[3])}
        if(dat_itemalts$alternatief[r]=="A4"){dat_itemalts$antwoord[r] <- as.character(sort(unique(antwoordgegevens()[,dat_itemalts$vraag[r]]))[4])}}
      if(dat_itemalts$scoregroep[r]=="laag"){dat_itemalts$proportie[r] <- nrow(antwoordgegevens()[rownames(antwoordgegevens()) %in% laagstuds&antwoordgegevens()[,dat_itemalts$vraag[r]]==dat_itemalts$antwoord[r],])/length(laagstuds)}
      if(dat_itemalts$scoregroep[r]=="middel"){dat_itemalts$proportie[r] <- nrow(antwoordgegevens()[rownames(antwoordgegevens()) %in% middelstuds&antwoordgegevens()[,dat_itemalts$vraag[r]]==dat_itemalts$antwoord[r],])/length(middelstuds)}
      if(dat_itemalts$scoregroep[r]=="hoog"){dat_itemalts$proportie[r] <- nrow(antwoordgegevens()[rownames(antwoordgegevens()) %in% hoogstuds&antwoordgegevens()[,dat_itemalts$vraag[r]]==dat_itemalts$antwoord[r],])/length(hoogstuds)}
      if(!is.na(dat_itemalts$antwoord[r])&dat_itemalts$antwoord[r]==as.character(itemgegevens()[itemgegevens()$vraagaanduiding==dat_itemalts$vraag[r],"sleutel"])){dat_itemalts$correct[r]<-"ja"}else{dat_itemalts$correct[r]<-"nee"}
    }
    
    dat_itemalts$scoregroep <- factor(dat_itemalts$scoregroep, levels = c("laag","middel","hoog"))
    
    dat_itemalts
  })
  
  deelnemergegevens_incompleet_wijz <- reactive({
    if(debug){cat("deelnemergegevens_incompleet_wijz\n")}
    deelnemergegevens_incompleet <- data.frame(deelnemer=deelnemergegevens_incompleet()$deelnemer,
                                               groep=deelnemergegevens_incompleet()$groep)
    deelnemergegevens_incompleet$totaalscore <- apply(accgegevens_wijz(),1,sum)
    
    onderdelen <- unique(vraaggegevens_wijz()$onderdeel)
    onderdeelscores <- data.frame(matrix(NA, nrow=nrow(accgegevens_wijz()), ncol=length(onderdelen)))
    for(o in 1:length(onderdelen)){
      vragen <- vraaggegevens_wijz()[vraaggegevens_wijz()$onderdeel==onderdelen[o],"vraagaanduiding"]
      acc_onderdeel <- accgegevens_wijz()[,colnames(accgegevens_wijz()) %in% vragen]
      onderdeelscores[,o] <- apply(acc_onderdeel,1,sum)
    }
    names(onderdeelscores) <- onderdelen
    
    deelnemergegevens_incompleet <- cbind(deelnemergegevens_incompleet, onderdeelscores)
    
    deelnemergegevens_incompleet
  })
  
  cesuur_wijz <- reactive({
    if(debug){cat("cesuur_wijz\n")}
    cesuurmethode <- input$cesuurmethode
    
    if(cesuurmethode=="absoluut (percentage score)"){
      cesuur <- ceiling((input$cesuur_absoluut/100)*(sum(vraaggegevens_wijz()$maxscore)-gokscore_wijz())+gokscore_wijz())}
    if(cesuurmethode=="relatief (slagingspercentage)"){
      slaagbijcesuur <- data.frame(cesuur=0:sum(vraaggegevens_wijz()$maxscore), slaag=NA)
      for(r in 1:nrow(slaagbijcesuur)){
        slaagbijcesuur$slaag[r] <- nrow(deelnemergegevens_incompleet_wijz()[deelnemergegevens_incompleet_wijz()$totaalscore>=slaagbijcesuur$cesuur[r],])/nrow(deelnemergegevens_incompleet_wijz())}
      cesuur <- max(slaagbijcesuur[slaagbijcesuur$slaag>=(input$cesuur_relatief/100),"cesuur"])}
    if(cesuurmethode=="Cohen-Schotanus"){
      maxscore_cohen <- sort(deelnemergegevens_incompleet_wijz()$totaalscore)[ceiling(0.95*length(deelnemergegevens_incompleet_wijz()$totaalscore))]
      cesuur <- ceiling(gokscore_wijz() + input$cesuur_cohen/100*(maxscore_cohen - gokscore()))}
    if(cesuurmethode=="Hofstee"){
      hofsteegrenzen <- list(ondergrens_vragen=input$cesuur_hofstee_score[1], bovengrens_vragen=input$cesuur_hofstee_score[2], 
                             ondergrens_gezakt=input$cesuur_hofstee_zak[1], bovengrens_gezakt=input$cesuur_hofstee_zak[2])
      
      hofsteedat <- data.frame(totaalscore=c(0:sum(vraaggegevens_wijz()$maxscore)), gezakt=NA)
      for(s in 1:nrow(hofsteedat)){
        hofsteedat$gezakt[s] <- sum(deelnemergegevens_incompleet_wijz()$totaalscore<hofsteedat$totaalscore[s])/length(deelnemergegevens_incompleet_wijz()$totaalscore)}
      
      hofsteelijn_slope <- (hofsteegrenzen$ondergrens_gezakt/100-hofsteegrenzen$bovengrens_gezakt/100)/(hofsteegrenzen$bovengrens_vragen-hofsteegrenzen$ondergrens_vragen)
      hofsteelijn_intercept <- hofsteegrenzen$ondergrens_gezakt/100 - hofsteelijn_slope*hofsteegrenzen$bovengrens_vragen
      
      hofsteedat$hofsteelijn <- hofsteelijn_intercept + hofsteelijn_slope*hofsteedat$totaalscore
      hofsteedat$verschil <- hofsteedat$gezakt-hofsteedat$hofsteelijn
      hofsteedat$verschil_abs <- abs(hofsteedat$verschil)
      
      cesuur <- hofsteedat[hofsteedat$verschil_abs==min(hofsteedat$verschil_abs)[1],"totaalscore"]}
    if(cesuurmethode=="eigen cesuur opgeven"){
      cesuur <- input$cesuur_eigen}
    
    cesuur
  })
  
  deelnemergegevens_wijz <- reactive({
    if(debug){cat("deelnemergegevens_wijz\n")}
    deelnemergegevens <- deelnemergegevens_incompleet_wijz()
    deelnemergegevens$slaag <- "gezakt"
    deelnemergegevens[deelnemergegevens$totaalscore>=cesuur_wijz(),"slaag"] <- "geslaagd"
    deelnemergegevens$slaag <- factor(deelnemergegevens$slaag, levels=c("gezakt","geslaagd"))
    
    #bij elke totaalscore een cijfer en percentiel (aparte formule voor onvoldoende en voldoende)
    transformatie <- data.frame(totaalscore=c(0:sum(vraaggegevens_wijz()$maxscore)), percentiel=NA)
    for(r in 1:nrow(transformatie)){
      transformatie$percentiel[r] <- nrow(deelnemergegevens_incompleet_wijz()[deelnemergegevens_incompleet_wijz()$totaalscore<=transformatie$totaalscore[r],])/nrow(deelnemergegevens_incompleet_wijz())}
    
    transformatie$cijfer_onvoldoende <- round(5.4999 - 4.5*(transformatie$totaalscore - cesuur_wijz())/(gokscore_wijz()-cesuur_wijz()),input$cijfer_decimalen)
    transformatie$cijfer_voldoende <- round(5.5001 + 4.5*(transformatie$totaalscore - cesuur_wijz())/(sum(vraaggegevens_wijz()$maxscore) - cesuur_wijz()),input$cijfer_decimalen)
    
    if(input$cijfermethode=="totaalscores niet hoger dan gokscore krijgen 1"){
      transformatie[transformatie$totaalscore<cesuur_wijz(),"cijfer"] <- transformatie[transformatie$totaalscore<cesuur_wijz(),"cijfer_onvoldoende"]
      transformatie[transformatie$totaalscore>=cesuur_wijz(),"cijfer"] <- transformatie[transformatie$totaalscore>=cesuur_wijz(),"cijfer_voldoende"]
      transformatie[transformatie$totaalscore<gokscore_wijz(),"cijfer"] <- 1}
    if(input$cijfermethode=="zelfde aantal vragen per punt als bij voldoende cijfers"){
      transformatie$cijfer <- transformatie$cijfer_voldoende
      transformatie[transformatie$cijfer<1,"cijfer"] <- 1}
    deelnemergegevens$cijfer <- NA
    for(r in 1:nrow(transformatie)){
      deelnemergegevens[deelnemergegevens$totaalscore==transformatie$totaalscore[r],"percentiel"] <- transformatie$percentiel[r]
      deelnemergegevens[deelnemergegevens$totaalscore==transformatie$totaalscore[r],"cijfer"] <- transformatie$cijfer[r]} 
    
    deelnemergegevens
  })
  
  ####reactive plots####
  
  totaalscore_reactive <- reactive({
    hist <- data.frame(totaalscore=table(deelnemergegevens_wijz()$totaalscore),
                       cijfer=NA, slaag=NA)
    colnames(hist) <- c("totaalscore","freq","cijfer")
    for(r in 1:nrow(hist)){
      hist$cijfer[r] <- unique(deelnemergegevens_wijz()[deelnemergegevens_wijz()$totaalscore==hist$totaalscore[r],"cijfer"])}
    hist$slaag <- "gezakt"
    hist[hist$cijfer>=5.5,"slaag"] <- "geslaagd"
    hist$totaalscore <- as.numeric(as.character(hist$totaalscore))
    
    hist
  })
  
  plot_totaalscore_reactive <- reactive({
    hist <- totaalscore_reactive()
    hist$totaalscore <- as.numeric(as.character(hist$totaalscore))
    hist$slaag <- factor(hist$slaag, levels=c("gezakt","geslaagd"))
    
    if(sum(vraaggegevens_wijz()$maxscore)<25){tickwidth<-2}else{
      if(sum(vraaggegevens_wijz()$maxscore)<=100){tickwidth<-5}else{tickwidth<-10}}
    
    ggplot(data=hist, aes(x=totaalscore, y=freq)) +
      geom_bar(stat="identity", aes(fill=slaag)) +
      geom_text(aes(label=cijfer), vjust=-0.5) +
      geom_vline(xintercept=cesuur_wijz()-0.5, linetype="dashed") +
      scale_x_continuous(limits=c(0,sum(vraaggegevens_wijz()$maxscore)+.5), breaks=seq(0,sum(vraaggegevens_wijz()$maxscore),tickwidth)) +
      scale_y_continuous(limits=c(0,1.1*max(hist$freq)), expand=c(0,0)) +
      #scale_fill_manual(values=c("chartreuse3","firebrick2")) +
      labs(x="Totaalscore op de toets", y="Aantal toetsdeelnemers") +
      theme_bw() +
      theme(legend.title=element_blank())
  })
  
  plot_totaalscore_legbot_reactive <- reactive({
    hist <- totaalscore_reactive()
    hist$totaalscore <- as.numeric(as.character(hist$totaalscore))
    hist$slaag <- factor(hist$slaag, levels=c("gezakt","geslaagd"))
    
    if(sum(vraaggegevens_wijz()$maxscore)<25){tickwidth<-2}else{
      if(sum(vraaggegevens_wijz()$maxscore)<=100){tickwidth<-5}else{tickwidth<-10}}
    
    ggplot(data=hist, aes(x=totaalscore, y=freq)) +
      geom_bar(stat="identity", aes(fill=slaag)) +
      geom_text(aes(label=cijfer), vjust=-0.5, size=3) +
      geom_vline(xintercept=cesuur_wijz()-0.5, linetype="dashed") +
      scale_x_continuous(breaks=seq(0,sum(vraaggegevens_wijz()$maxscore),tickwidth)) +
      scale_y_continuous(limits=c(0,1.1*max(hist$freq)), expand=c(0,0)) +
      #scale_fill_manual(values=c("chartreuse3","firebrick2")) +
      labs(x="Totaalscore op de toets", y="Aantal toetsdeelnemers") +
      theme_bw() +
      theme(legend.title=element_blank(), legend.position="bottom")
  })
  
  plot_hofstee_reactive <- reactive({
    if(input$cesuurmethode=="Hofstee"){
      if(sum(vraaggegevens_wijz()$maxscore)<15){tickwidth<-2}else{if(sum(vraaggegevens_wijz()$maxscore)<25){tickwidth<-5}else{tickwidth<-10}}
      
      hofsteegrenzen <- list(ondergrens_vragen=input$cesuur_hofstee_score[1], bovengrens_vragen=input$cesuur_hofstee_score[2], 
                             ondergrens_gezakt=input$cesuur_hofstee_zak[1], bovengrens_gezakt=input$cesuur_hofstee_zak[2])
      
      hofsteedat <- data.frame(totaalscore=c(0:sum(vraaggegevens_wijz()$maxscore)), gezakt=NA)
      for(s in 1:nrow(hofsteedat)){
        hofsteedat$gezakt[s] <- sum(deelnemergegevens_incompleet_wijz()$totaalscore<hofsteedat$totaalscore[s])/length(deelnemergegevens_incompleet_wijz()$totaalscore)}
      
      hofsteelijn_slope <- (hofsteegrenzen$ondergrens_gezakt/100-hofsteegrenzen$bovengrens_gezakt/100)/(hofsteegrenzen$bovengrens_vragen-hofsteegrenzen$ondergrens_vragen)
      hofsteelijn_intercept <- hofsteegrenzen$ondergrens_gezakt/100 - hofsteelijn_slope*hofsteegrenzen$bovengrens_vragen
      
      hofsteedat$hofsteelijn <- hofsteelijn_intercept + hofsteelijn_slope*hofsteedat$totaalscore
      hofsteedat$verschil <- hofsteedat$gezakt-hofsteedat$hofsteelijn
      hofsteedat$verschil_abs <- abs(hofsteedat$verschil)
      
      cesuur <- hofsteedat[hofsteedat$verschil_abs==min(hofsteedat$verschil_abs)[1],"totaalscore"]
      
      hofsteedat_long <- data.frame(totaalscore=c(rep(hofsteedat$totaalscore,2),hofsteegrenzen$ondergrens_vragen,hofsteegrenzen$bovengrens_vragen,cesuur_wijz()),
                                    soort=c(rep("data",nrow(hofsteedat)),rep("hofsteelijn",nrow(hofsteedat)),rep("hofsteepunten",2),"hofsteecesuur"),
                                    gezakt=c(hofsteedat$gezakt,hofsteedat$hofsteelijn,hofsteegrenzen$bovengrens_gezakt/100,hofsteegrenzen$ondergrens_gezakt/100,NA))
      
      hofsteedat_long[nrow(hofsteedat_long),"gezakt"] <- hofsteedat_long[hofsteedat_long$totaalscore==cesuur_wijz()&hofsteedat_long$soort=="data","gezakt"]
      
      ggplot(data=hofsteedat_long[hofsteedat_long$soort!="hofsteepunten",], aes(x=totaalscore, y=gezakt, group=soort)) +
        geom_line(aes(color=soort)) +
        geom_point(data=hofsteedat_long[hofsteedat_long$soort=="hofsteepunten",]) +
        geom_point(data=hofsteedat_long[hofsteedat_long$soort=="hofsteecesuur",], shape=5) +
        scale_x_continuous(breaks=seq(0,1000,tickwidth), expand=c(0,0)) +
        scale_y_continuous(lab=percent, limits=c(-1,2), breaks=seq(0,1,.1)) +
        coord_cartesian(ylim=c(0,1)) + #zodat de Hofsteelijn helemaal wordt doorgetrokken
        labs(x="Cesuur", y="Gezakte deelnemers") +
        guides(colour=FALSE) +
        theme_bw()
    }else{
      return(NULL)}
  })
  
  plot_deelscores_reactive <- reactive({
    onderdelen <- unique(vraaggegevens()$onderdeel)
    onderdelen_n <- table(factor(vraaggegevens()$onderdeel, levels=unique(vraaggegevens()$onderdeel)))
    onderdelen_maxscore <- onderdelen_n
    for(o in 1:length(onderdelen_maxscore)){
      onderdelen_maxscore[o] <- sum(vraaggegevens()[vraaggegevens()$onderdeel==onderdelen[o],"maxscore"])}
    
    scoresperonderdeel <- data.frame(deelnemer=rep(1:nrow(deelnemergegevens()), times=length(onderdelen)),
                                     onderdeel=rep(onderdelen, each=nrow(deelnemergegevens())),
                                     onderdeel_n=rep(onderdelen_n, each=nrow(deelnemergegevens())),
                                     onderdeel_maxscore=rep(onderdelen_maxscore, each=nrow(deelnemergegevens())),
                                     n=NA, p=NA, freq=NA, onderdeel_gem=NA, onderdeel_tekst=NA)
    for(r in 1:nrow(scoresperonderdeel)){
      scoresperonderdeel$n[r] <- deelnemergegevens()[scoresperonderdeel$deelnemer[r],
                                                     as.character(scoresperonderdeel$onderdeel[r])]
    }
    scoresperonderdeel$p <- scoresperonderdeel$n/scoresperonderdeel$onderdeel_maxscore
    
    for(r in 1:nrow(scoresperonderdeel)){
      scoresperonderdeel$onderdeel_gem[r] <- mean(scoresperonderdeel[scoresperonderdeel$onderdeel==scoresperonderdeel$onderdeel[r],"p"])
      scoresperonderdeel$freq[r] <- nrow(scoresperonderdeel[scoresperonderdeel$onderdeel==scoresperonderdeel$onderdeel[r]&
                                                              scoresperonderdeel$n==scoresperonderdeel$n[r],])
      
    }
    scoresperonderdeel$onderdeel_tekst <- paste0(scoresperonderdeel$onderdeel," (",scoresperonderdeel$onderdeel_n," vragen, gem. ",
                                                 round(100*scoresperonderdeel$onderdeel_gem,0),"%)")
    
    ggplot(data=scoresperonderdeel[!duplicated(scoresperonderdeel[,-1]),], aes(x=n, y=freq)) +
      geom_bar(stat="identity", aes(fill=onderdeel)) +
      #geom_vline(aes(xintercept=onderdeel_gem), linetype="dashed") +
      facet_wrap(~onderdeel_tekst) +
      #scale_x_continuous(label=percent, limits=c(0,1), breaks=seq(0,1,.1)) +
      scale_y_continuous(limits=c(0,1.1*max(scoresperonderdeel[!duplicated(scoresperonderdeel[,-1]),"freq"])), expand=c(0,0)) +
      labs(x="Score op toetsonderdeel", y="Aantal toetsdeelnemers") +
      guides(fill=F) +
      theme_bw()
  })
  
  plot_coronderdelen_reactive <- reactive({
    if(length(unique(vraaggegevens()$onderdeel))>1){
      cors <- round(cor(deelnemergegevens()[,as.character(unique(vraaggegevens()$onderdeel))]),2)
      cors[upper.tri(cors)] <- NA
      cors_lang <- melt(cors)
      
      ggplot(cors_lang, aes(Var2, Var1, fill = value)) +
        geom_tile(color = "white") +
        scale_y_discrete(name="", limits = rev(levels(cors_lang$Var2))) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="correlatie", na.value="white") +
        theme_minimal()+ # minimal theme
        theme(axis.text.x = element_text(angle=90, size=10), axis.text.y=element_text(size=10)) +
        coord_fixed() +
        geom_text(aes(Var2, Var1, label = value), color="black", size=3) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          #legend.position = "bottom",
          legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5))
    }else{
      return(NULL)}
  })
  
  corvragen_reactive <- reactive({
    accgegevens_onderdeel <- accgegevens()
    for(c in 1:ncol(accgegevens_onderdeel)){
      colnames(accgegevens_onderdeel)[c] <- itemgegevens()[itemgegevens()$vraagaanduiding==colnames(accgegevens_onderdeel)[c],"onderdeelvraag"]}
    accgegevens_onderdeel <- accgegevens_onderdeel[,order(colnames(accgegevens_onderdeel))]
    
    cors <- round(cor(accgegevens_onderdeel),2)
    
    cors
  })
  
  plot_corvragen_reactive <- reactive({
    cors <- corvragen_reactive()
    
    cors[upper.tri(cors)] <- NA
    cors_lang <- melt(cors)
    
    cors_lang$onderdeel1 <- sapply(as.character(cors_lang$Var1),function(x) strsplit(x,":")[[1]][1])
    cors_lang$onderdeel2 <- sapply(as.character(cors_lang$Var2),function(x) strsplit(x,":")[[1]][1])
    cors_lang$zelfdeonderdeel <- 0
    cors_lang[cors_lang$onderdeel1==cors_lang$onderdeel2,"zelfdeonderdeel"] <- 1
    
    cors_lang$zelfdeonderdeel <- factor(cors_lang$zelfdeonderdeel)
    
    if(nrow(vraaggegevens())<25){numbersize<-3; textsize<-10}else{numbersize<-2; textsize<-7}
    
    ggplot(cors_lang, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_y_discrete(name="", limits = rev(levels(cors_lang$Var2))) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="correlatie", na.value="white") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle=90, size=textsize), axis.text.y = element_text(size=textsize))+
      coord_fixed() +
      geom_text(aes(Var2, Var1, label = sub('^(-)?0[.]','\\1.',value), color = zelfdeonderdeel), size=numbersize) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
      scale_color_manual(values=c("grey","black")) +
      guides(color=F, fill = guide_colorbar(barwidth = 7, barheight = 1,
                                            title.position = "top", title.hjust = 0.5))
  })
  
  plot_gulliksen_reactive <- reactive({
    ggplot(itemgegevens(), aes(x=p, y=rir)) +
      scale_x_continuous(limits = c(0,1), breaks=seq(0,1,.1), expand=c(0,0)) +
      scale_y_continuous(breaks=seq(-1,1,.1)) +
      annotate("rect", xmin=-Inf, xmax=input$gulliksen_pgrenzen[1], ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
      annotate("rect", xmin=input$gulliksen_pgrenzen[2], xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=input$gulliksen_rirgrens, alpha=0.2, fill="red") +
      geom_point(aes(x=p, y=rir, shape=vraagtype), size=3, alpha=0.7) +
      labs(x="p-waarde", y="item-rest-correlatie (rir)") +
      theme(plot.background = element_rect(fill = "#F5F5F5", color="#F5F5F5")) +
      theme_bw()
  })
  
  tab_groepen_reactive <- reactive({
    tabel <- data.frame(groep=sort(unique(deelnemergegevens_wijz()$groep)),
                        aantal=NA,geslaagd=NA,cijfer=NA,totaalscore=NA)
    
    for(r in 1:nrow(tabel)){
      tabel$geslaagd[r]<-paste0(round(100*nrow(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r]&deelnemergegevens_wijz()$slaag=="geslaagd",])/nrow(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r],]),0),"%")
      tabel$aantal[r]<-nrow(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r],])
      tabel$cijfer[r]<-mean(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r],"cijfer"])
      tabel$totaalscore[r]<-mean(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r],"totaalscore"])}
    
    if(length(unique(vraaggegevens_wijz()$onderdeel))>1){
      onderdelen <- data.frame(matrix(NA,nrow(tabel),length(unique(vraaggegevens_wijz()$onderdeel))))
      colnames(onderdelen) <- unique(vraaggegevens_wijz()$onderdeel)
      tabel <- cbind(tabel,onderdelen)
      for(c in 6:ncol(tabel)){
        for(r in 1:nrow(tabel)){tabel[r,c]<-mean(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==tabel$groep[r],as.character(colnames(tabel)[c])])}}
    }
    
    tabel
  })
  
  plot_groepscores_reactive <- reactive({
    if(sum(vraaggegevens_wijz()$maxscore)<25){tickwidth<-2}else{
      if(sum(vraaggegevens_wijz()$maxscore)<=100){tickwidth<-5}else{tickwidth<-10}}
    
    hist <- data.frame(totaalscore=NA,freq=NA,groep=NA)
    for(g in 1:length(unique(deelnemergegevens_wijz()$groep))){
      temp <- data.frame(totaalscore=table(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==unique(deelnemergegevens_wijz()$groep)[g],"totaalscore"]),
                         groep=unique(deelnemergegevens_wijz()$groep)[g])
      colnames(temp) <- c("totaalscore","freq","groep")
      hist <- rbind(hist,temp)}
    hist <- hist[-1,]
    
    hist$totaalscore <- as.numeric(as.character(hist$totaalscore))
    
    ggplot(hist, aes(x=totaalscore, y=freq)) +
      geom_bar(stat="identity", position="identity", alpha=0.5, aes(fill=groep, color=groep)) +
      geom_vline(xintercept = cesuur_wijz()-0.5, linetype = "dashed") +
      scale_x_continuous(limits = c(0,sum(vraaggegevens()$maxscore)), breaks=seq(0,200,tickwidth)) +
      scale_y_continuous(limits=c(0,1.1*max(hist$freq)), expand=c(0,0)) +
      labs(x="Totaalscore",y="Aantal toetsdeelnemers") +
      theme_bw() + 
      theme(legend.title=element_blank())
  })
  
  plot_groepdeelscores_reactive <- reactive({
    if(length(unique(vraaggegevens_wijz()$onderdeel))>1){
      scores <- data.frame(groep=rep(unique(deelnemergegevens_wijz()$groep), each=length(unique(vraaggegevens_wijz()$onderdeel))+1),
                           onderdeel=c(as.character(unique(vraaggegevens_wijz()$onderdeel)),"totaalscore"),
                           gem=NA, max=NA, p=NA)
      
      for(r in 1:nrow(scores)){
        scores$gem[r] <- mean(deelnemergegevens_wijz()[deelnemergegevens_wijz()$groep==scores$groep[r],as.character(scores$onderdeel[r])])
        scores$max[r] <- sum(vraaggegevens_wijz()[vraaggegevens_wijz()$onderdeel==as.character(scores$onderdeel[r]),"maxscore"])}
      
      scores$p <- scores$gem/scores$max
      scores[scores$onderdeel=="totaalscore","p"] <- scores[scores$onderdeel=="totaalscore","gem"]/sum(vraaggegevens_wijz()$"maxscore")
      
      scores$onderdeel <- factor(scores$onderdeel, levels=rev(c(as.character(unique(vraaggegevens_wijz()$onderdeel)),"totaalscore","totaal")))
      scores[scores$onderdeel=="totaalscore","onderdeel"] <- "totaal"
      
      ggplot(data=scores, aes(x=onderdeel, y=p)) +
        geom_bar(stat="identity", position="dodge", aes(fill=groep)) +
        geom_vline(xintercept=1.5, linetype = "dashed") +
        scale_y_continuous(limits=c(0,1.01), labels=percent, breaks=seq(0,1,.2), expand=c(0,0)) +
        labs(x="Toetsonderdeel", y="Gemiddeld percentage van maximaal te behalen score") +
        theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") +
        coord_flip() +
        guides(color=FALSE)
    }
  })
  
  #####reactive objecten voor xlsx####
  
  deelnemerresultaten <- reactive({
    deelnemerresultaten <- data.frame(deelnemer=deelnemergegevens_wijz()$deelnemer,
                                      groep=deelnemergegevens_wijz()$groep,
                                      resultaat=deelnemergegevens_wijz()$slaag,
                                      cijfer=deelnemergegevens_wijz()$cijfer,
                                      percentiel=round(100*deelnemergegevens_wijz()$percentiel,0))
    
    if(length(unique(vraaggegevens_wijz()$onderdeel))>1){
      deelnemerresultaten <- cbind(deelnemerresultaten, 
                                   deelnemergegevens_wijz()[,as.character(unique(vraaggegevens_wijz()$onderdeel))], 
                                   totaalscore=deelnemergegevens_wijz()$totaalscore,
                                   deelnemergegevens_wijz()[,as.character(unique(vraaggegevens_wijz()$onderdeel))], 
                                   totaalscore=paste0(round(100*deelnemergegevens_wijz()$totaalscore/sum(vraaggegevens_wijz()$maxscore),0),"%"),
                                   totaalscore_metgokscorecorrectie=paste0(round(100*(deelnemergegevens_wijz()$totaalscore-gokscore_wijz())/(sum(vraaggegevens_wijz()$maxscore)-gokscore_wijz()),0),"%"))
      for(c in 6:(5+length(unique(vraaggegevens_wijz()$onderdeel)))){
        deelnemerresultaten[,c+length(unique(vraaggegevens_wijz()$onderdeel))+1] <- paste0(round(100*deelnemerresultaten[,c]/sum(vraaggegevens_wijz()[vraaggegevens_wijz()$onderdeel==colnames(deelnemerresultaten)[c],"maxscore"]),0),"%")}
    }else{
      deelnemerresultaten <- cbind(deelnemerresultaten, 
                                   totaalscore=deelnemergegevens_wijz()$totaalscore,
                                   totaalscore=paste0(round(100*deelnemergegevens_wijz()$totaalscore/sum(vraaggegevens_wijz()$maxscore),0),"%"),
                                   totaalscore_metgokscorecorrectie=paste0(round(100*(deelnemergegevens_wijz()$totaalscore-gokscore_wijz())/(sum(vraaggegevens_wijz()$maxscore)-gokscore_wijz()),0),"%"))}
    
    deelnemerresultaten
  })
  
  vraagstatistieken <- reactive({
    vraagstatistieken <- data.frame(vraagaanduiding=itemgegevens()$vraagaanduiding,
                                    onderdeel=itemgegevens()$onderdeel,
                                    vraagtype=itemgegevens()$vraagtype,
                                    gokkans=itemgegevens()$gokkans,
                                    pwaarde=round(itemgegevens()$p,2),
                                    itemrestcorrelatie=round(itemgegevens()$rir,2),
                                    alfaalsverwijderd=round(itemgegevens()$alfa_als_verwijderd,2),
                                    probleem=itemgegevens_commentaar()$probleem)
    
    vraagstatistieken <- cbind(vraagstatistieken, vraagwijzigingen()[,-1])
    
    vraagstatistieken
  })
  
  ####aanroepen reactive####
  
  ####sidebar####
  
  output$tekst_ndeelnemers <- renderText({
    paste0("Aantal toetsdeelnemers: ",nrow(antwoordgegevens()))
  })
  
  output$tekst_nvragen <- renderText({
    if(ncol(accgegevens_wijz())==ncol(antwoordgegevens())){
      paste0("Aantal toetsvragen: ",ncol(accgegevens_wijz()))}else{
        paste0("Aantal toetsvragen: ",ncol(accgegevens_wijz())," (was ",ncol(antwoordgegevens()),")")}
  })
  
  output$tekst_maxscore <- renderText({
    if(ncol(accgegevens_wijz())==ncol(antwoordgegevens())){
      paste0("Maximaal haalbare score: ",sum(vraaggegevens_wijz()$maxscore))}else{
        paste0("Maximaal haalbare score: ",sum(vraaggegevens_wijz()$maxscore)," (was ",sum(vraaggegevens()$maxscore),")")}
  })
  
  output$tekst_gokscore <- renderText({
    if(round(gokscore_wijz(),2)==round(gokscore(),2)){
      paste0("Gokscore: ",round(gokscore_wijz(),2))}else{
        paste0("Gokscore: ",round(gokscore_wijz(),2)," (was ",round(gokscore(),2),")")}
  })
  
  output$tekst_alfa <- renderText({
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      if(round(as.numeric(cronbach(accgegevens_wijz())[3]),2)==round(as.numeric(cronbach(accgegevens())[3]),2)){
        tekst_alfa <- paste0("Cronbachs alfa: ",round(as.numeric(cronbach(accgegevens_wijz())[3]),2))}else{
          tekst_alfa <- paste0("Cronbachs alfa: ",round(as.numeric(cronbach(accgegevens_wijz())[3]),2)," (was ",round(as.numeric(cronbach(accgegevens())[3]),2),")")}
      tekst_alfa}
  })
  
  output$tekst_gemscore <- renderText({
    if(round(mean(apply(accgegevens_wijz(),1,sum)),2)==round(mean(apply(accgegevens(),1,sum)),2)){
      paste0("Gemiddelde: ",round(mean(apply(accgegevens_wijz(),1,sum)),2))}else{
        paste0("Gemiddelde: ",round(mean(apply(accgegevens_wijz(),1,sum)),2)," (was ",round(mean(apply(accgegevens(),1,sum)),2),")")}
  })
  
  output$tekst_sdscore <- renderText({
    if(round(sd(apply(accgegevens_wijz(),1,sum)),2)==round(sd(apply(accgegevens(),1,sum)),2)){
      paste0("Standaarddeviatie: ",round(sd(apply(accgegevens_wijz(),1,sum)),2))}else{
        paste0("Standaarddeviatie: ",round(sd(apply(accgegevens_wijz(),1,sum)),2)," (was ",round(sd(apply(accgegevens(),1,sum)),2),")")}
  })
  
  output$tekst_rangescore <- renderText({
    if(min(apply(accgegevens_wijz(),1,sum))==min(apply(accgegevens(),1,sum))&max(apply(accgegevens_wijz(),1,sum))==max(apply(accgegevens(),1,sum))){
      paste0("Bereik: ",min(apply(accgegevens_wijz(),1,sum)),"-",max(apply(accgegevens_wijz(),1,sum)))}else{
        paste0("Bereik: ",min(apply(accgegevens_wijz(),1,sum)),"-",max(apply(accgegevens_wijz(),1,sum)),
               " (was ",min(apply(accgegevens(),1,sum)),"-",max(apply(accgegevens(),1,sum)),")")}
  })
  
  output$tekst_gemcijfer <- renderText({
    paste0("Gemiddelde: ",round(mean(deelnemergegevens_wijz()$cijfer),2))
  })
  
  output$tekst_sdcijfer <- renderText({
    paste0("Standaarddeviatie: ",round(sd(deelnemergegevens_wijz()$cijfer),2))
  })
  
  output$tekst_rangecijfer <- renderText({
    paste0("Bereik: ",min(deelnemergegevens_wijz()$cijfer),"-",max(deelnemergegevens_wijz()$cijfer))
  })
  
  output$tekst_cesuurmethode <- renderText({
    paste("Cesuurmethode: ",input$cesuurmethode)
  })
  
  output$tekst_cesuur <- renderText({
    paste0("Cesuur: ",cesuur_wijz()," (",round(100*(cesuur_wijz()-gokscore_wijz())/(sum(vraaggegevens_wijz()$maxscore)-gokscore_wijz()),0),"%)")
  })
  
  output$tekst_nslaag <- renderText({
    paste0("Geslaagd: ",sum(deelnemergegevens_wijz()$slaag=="geslaagd")," van de ",nrow(antwoordgegevens())," (",round(100*sum(deelnemergegevens_wijz()$slaag=="geslaagd")/nrow(deelnemergegevens_wijz()),0),"%)")
  })
  
  output$download_xslx <- downloadHandler(
    filename = function() { paste("toetsanalyse.xlsx")},
    
    content = function(file){
      Results_Workbook <- createWorkbook(type='xlsx')
      sheet.1 <- createSheet(Results_Workbook, sheetName = "Deelnemerresultaten")
      addDataFrame(deelnemerresultaten(), sheet=sheet.1, startRow=1,startColumn=1,row.names=F)
      sheet.2 <- createSheet(Results_Workbook, sheetName = "Vraagstatistieken")
      addDataFrame(vraagstatistieken(), sheet=sheet.2, startRow=1,startColumn=1,row.names=F)
      sheet.3 <- createSheet(Results_Workbook, sheetName = "Correlaties tussen vragen")
      addDataFrame(corvragen_reactive(), sheet=sheet.3, startRow=1,startColumn=1,row.names=T)
      saveWorkbook(Results_Workbook,file)
    })
  
  output$download_pdf <- downloadHandler(
    filename = "Toetsanalyse.pdf",
    content = function(file) {
      out = knit2pdf('app_analysetool_report.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )
  
  ####main panel####
  
  output$tab_antwoordgegevens_head <- renderTable({
    antwoordgegevens()[1:3,]
  })
  
  output$tab_vraaggegevens_head <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    vraaggegevens()[1:3,]
  })
  
  output$tab_accgegevens_head <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    apply(accgegevens()[1:3,],2,as.character)
  })
  
  output$tab_deelnemergegevens_head <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    apply(deelnemergegevens_wijz()[1:3,1:2],2,as.character)
  })
  
  output$tekst_slaag <- renderText({
    paste0("Bij de gekozen cesuur van ",cesuur_wijz()," vragen slagen ",sum(deelnemergegevens_wijz()$slaag=="geslaagd")," van de ",
           nrow(deelnemergegevens_wijz())," deelnemers (",round(100*sum(deelnemergegevens_wijz()$slaag=="geslaagd")/nrow(deelnemergegevens_wijz()),0),"%).")
  })
  
  
  output$plot_totaalscore <- renderPlot({
    plot_totaalscore_reactive()
  })
  
  output$cesuur_hofstee_score = renderUI({
    sliderInput("cesuur_hofstee_score", 
                label = "Bandbreedte acceptabele cesuren (score):",
                min=0, max=sum(vraaggegevens()$maxscore), value=c(0,sum(vraaggegevens()$maxscore)))
  })
  
  output$plot_hofstee <- renderPlot({
    plot_hofstee_reactive()
  })
  
  output$plot_deelscores <- renderPlot({
    plot_deelscores_reactive()
  })
  
  output$plot_coronderdelen <- renderPlot({
    plot_coronderdelen_reactive()
  })
  
  output$plot_corvragen <- renderPlot({
    plot_corvragen_reactive()
  })
  
  output$plot_gulliksen <- renderPlot({
    plot_gulliksen_reactive()
  })
  
  output$tab_zekernaderbekijken <- renderTable({
    selectie <- itemgegevens_commentaar()[itemgegevens_commentaar()$probleem=="p-waarde onder de gokkans"|
                                            itemgegevens_commentaar()$probleem=="negatief onderscheidend vermogen"|
                                            itemgegevens_commentaar()$probleem=="p-waarde onder de gokkans en negatief onderscheidend vermogen",1:4]
    if(nrow(selectie)==0){return(NULL)}else{selectie}
  })
  
  output$tab_gewenstnaderbekijken <- renderTable({
    selectie <- itemgegevens_commentaar()[itemgegevens_commentaar()$probleem=="laag onderscheidend vermogen"|
                                            itemgegevens_commentaar()$probleem=="moeilijk"|
                                            itemgegevens_commentaar()$probleem=="moeilijk en laag onderscheidend vermogen"|
                                            itemgegevens_commentaar()$probleem=="erg makkelijk"|
                                            itemgegevens_commentaar()$probleem=="erg makkelijk en laag onderscheidend vermogen",1:4]
    if(nrow(selectie)==0){return(NULL)}else{selectie}
  })
  
  output$vragenlijst = renderUI({
    selectInput("vraagselectie", label="Selecteer een vraag om te bekijken:", itemgegevens_commentaar()$vraagplusprob, width=400)
  })
  
  output$tekst_vraagtype <- renderText({
    paste0("Vraagtype: ",itemgegevens()[itemgegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"vraagtype"])
  })
  
  output$tab_antwoorden <- renderTable({
    if(itemgegevens()[itemgegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"vraagtype"]=="DS"){
      antwoorden <- data.frame(deelscore=names(table(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]])),
                               perc=paste0(round(100*table(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]])/nrow(antwoordgegevens()),0),"%"),
                               rirofrar=NA)
      
      for(r in 1:nrow(antwoorden)){
        antwoorden$rirofrar[r] <- cor(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]]==as.character(antwoorden$deelscore[r]),
                                      apply(accgegevens()[,-which(colnames(accgegevens())==strsplit(as.character(input$vraagselectie)," ")[[1]][1])],1,sum))
      }
      
      antwoorden}else{
        
        antwoorden <- data.frame(antwoord=names(table(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]])),
                                 sleutel=NA,
                                 perc=paste0(round(100*table(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]])/nrow(antwoordgegevens()),0),"%"),
                                 rirofrar=NA)
        
        for(r in 1:nrow(antwoorden)){
          if(antwoorden$antwoord[r]==as.character(vraaggegevens()[vraaggegevens()$vraagaanduiding==strsplit(as.character(input$vraagselectie)," ")[[1]][1],"sleutel"])){
            antwoorden$sleutel[r] <- "correct"}else{antwoorden$sleutel[r] <- "incorrect"}
          antwoorden$rirofrar[r] <- cor(antwoordgegevens()[,strsplit(as.character(input$vraagselectie)," ")[[1]][1]]==as.character(antwoorden$antwoord[r]),
                                        apply(accgegevens()[,-which(colnames(accgegevens())==strsplit(as.character(input$vraagselectie)," ")[[1]][1])],1,sum))
        }
        
        antwoorden}
  })
  
  output$goedrekenen = renderUI({
    radioButtons("goedrekenen",
                 label="Goed te rekenenen:",
                 sort(unique(antwoordgegevens()[,strsplit(input$vraagselectie," ")[[1]][1]])[unique(antwoordgegevens()[,strsplit(input$vraagselectie," ")[[1]][1]])!=as.character(vraaggegevens()[vraaggegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"sleutel"])]),
                 width=600)
  })
  
  output$plot_item<- renderPlot({
    if(itemgegevens()[itemgegevens()$vraagaanduiding==strsplit(input$vraagselectie," ")[[1]][1],"vraagtype"]=="DS"){
      ggplot(dat_itemalts()[dat_itemalts()$vraag==strsplit(input$vraagselectie," ")[[1]][1]&!is.na(dat_itemalts()$antwoord),],aes(x=scoregroep, y=proportie, group=antwoord, colour=antwoord)) +
        geom_line(size=1.5) +
        scale_color_discrete(name="deelscore") +
        geom_dl(aes(label = antwoord), method = list(dl.trans(x = x + 0.4), "last.points", cex = 1)) +
        geom_dl(aes(label = antwoord), method = list(dl.trans(x = x - 0.4), "first.points", cex = 1)) +
        scale_y_continuous(labels=percent,limits = c(0, 1)) + #percentages op y-as
        labs(x = "Scoregroep deelnemers", y = "Percentage deelnemers met deelscore") +
        theme_bw()
    }else{
      ggplot(dat_itemalts()[dat_itemalts()$vraag==strsplit(input$vraagselectie," ")[[1]][1]&!is.na(dat_itemalts()$antwoord),],aes(x=scoregroep, y=proportie, group=antwoord, colour=antwoord)) +
        geom_line(size=1.5) +
        geom_point(aes(size=correct), shape=18, color="black") +
        geom_dl(aes(label = antwoord), method = list(dl.trans(x = x + 0.4), "last.points", cex = 1)) +
        geom_dl(aes(label = antwoord), method = list(dl.trans(x = x - 0.4), "first.points", cex = 1)) +
        scale_size_manual(values = c(6, 0)) +
        scale_y_continuous(labels=percent,limits = c(0, 1)) + #percentages op y-as
        labs(x = "Scoregroep deelnemers", y = "Percentage deelnemers dat alternatief kiest") +
        theme_bw()}
  })
  
  output$tekst_vraagwijzigingen <- renderText({
    if(is.null(vraagwijzigingen_ja())){return(NULL)}else{"Vragen met wijzigingen"}
  })
  
  output$tab_vraagwijzigingen <- renderTable({
    vraagwijzigingen_ja()
  })
  
  output$plot_groepscores <- renderPlot({
    plot_groepscores_reactive()
  })
  
  output$tab_groepen <- renderTable({
    tab_groepen_reactive()
  })
  
  output$plot_groependeelscores <- renderPlot({
    plot_groepdeelscores_reactive()
  })
  
  output$tekst_alfa2 <- renderText({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      paste0("De Cronbachs alfa van deze toets is ",round(as.numeric(cronbach(accgegevens_wijz())[3]),2),".")}
  })
  
  output$tekst_sb <- renderText({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      alfas <- data.frame(nvragen=0:1000, alfa=NA)
      a <- as.numeric(cronbach(accgegevens_wijz())[3])
      for(r in 1:nrow(alfas)){
        n <- alfas$nvragen[r]/nrow(vraaggegevens_wijz())
        alfas$alfa[r] <- round((n*a)/(1+(n-1)*a),2)}
      min_nvragen_80 <- min(alfas[alfas$alfa>=.80,"nvragen"])
      min_nvragen_90 <- min(alfas[alfas$alfa>=.90,"nvragen"])
      
      paste0("Het minimale aantal vragen dat een voorspelde Cronbachs alfa van 0.80 geeft is ",min_nvragen_80,
             ". Voor een Cronbachs alfa van 0.90 is dat ",min_nvragen_90," vragen.")}
  })
  
  output$tab_spearbrown <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      n <- input$spearbrown_nvragen/nrow(vraaggegevens_wijz())
      a <- as.numeric(cronbach(accgegevens_wijz())[3])
      a_star <- (n*a)/(1+(n-1)*a)
      
      data.frame(toets=c("huidig","voorspeld"),
                 vragen=c(nrow(vraaggegevens_wijz()), input$spearbrown_nvragen),
                 alfa=c(a, a_star))}
  })
  
  output$tab_alfavragen_slecht <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      alfas <- data.frame(vraagaanduiding=vraaggegevens_wijz()$vraagaanduiding, 
                          pwaarde=NA, itemrestcorrelatie=NA, alfa_als_verwijderd=NA)
      for(r in 1:nrow(alfas)){
        alfas$pwaarde[r] <- itemgegevens()[itemgegevens()$vraagaanduiding==alfas$vraagaanduiding[r],"p"]
        alfas$itemrestcorrelatie[r] <- itemgegevens()[itemgegevens()$vraagaanduiding==alfas$vraagaanduiding[r],"rir"]
        alfas$alfa_als_verwijderd[r] <- round(as.numeric(cronbach(accgegevens_wijz()[,-r])[3]),2)}
      slecht <- alfas[alfas$alfa_als_verwijderd>round(as.numeric(cronbach(accgegevens_wijz())[3]),2),]
      if(nrow(slecht)==0){return(NULL)}else{slecht}}
  })
  
  output$tab_alfavragen_goed <- renderTable({
    if(is.null(input$antwoordgegevens)){return(NULL)}
    if(sum(apply(accgegevens(),1,sum))==0){return(NULL)}else{
      alfas <- data.frame(vraagaanduiding=vraaggegevens_wijz()$vraagaanduiding, 
                          pwaarde=NA, itemrestcorrelatie=NA, alfa_als_verwijderd=NA)
      for(r in 1:nrow(alfas)){
        alfas$pwaarde[r] <- itemgegevens()[itemgegevens()$vraagaanduiding==alfas$vraagaanduiding[r],"p"]
        alfas$itemrestcorrelatie[r] <- itemgegevens()[itemgegevens()$vraagaanduiding==alfas$vraagaanduiding[r],"rir"]
        alfas$alfa_als_verwijderd[r] <- round(as.numeric(cronbach(accgegevens_wijz()[,-r])[3]),2)}
      goed <- alfas[alfas$alfa_als_verwijderd<round(as.numeric(cronbach(accgegevens_wijz())[3]),2),]
      if(nrow(goed)>10){goed <- goed[order(goed$alfa_als_verwijderd),]; goed <- goed[1:10,]}
      if(nrow(goed)==0){return(NULL)}else{goed}}
  })
  
}

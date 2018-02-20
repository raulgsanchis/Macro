library(ggplot2)
library(dplyr)
library(latex2exp)

Makrofigur <- setRefClass("Makrofigur", fields = list(modell='vector', modellatr ='list',
                                                      plotvectorend='list', dfmodellres='list',
                                                      ggtyper='list'))

Makrofigur$methods(initialize=function(modellnavn=NULL){

  keynes <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
  islml <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
  islmo <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmocequ.json'))
  adasl <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adascequ.json'))
  adaso <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adasoequ.json'))

  modellatr <<- list(keynes=keynes, islml=islml, islmo=islmo, adasl=adasl, adaso=adaso)

  modell <<- c(modellnavn)

})


Makrofigur$methods(numerisk=function(endvar=NULL ,lukketpar=NULL, openpar=NULL, endrvar = NULL, kat=NULL){

  exoparval <- c(lukketpar, openpar, endrvar)

  #browser()

  plotvectorend <<- list()
  for(mvar in endvar){
    # mvar <- endvar[1]
    endv <- list(eval(parse(text=  modellatr[[modell]][[mvar]]), exoparval))
    plotvectorend <<- append(plotvectorend, endv)
  }

  names(plotvectorend) <<- endvar

  DFmodellres <- data.frame(Iv= endrvar[[1]], plotvectorend) %>% reshape2::melt(id.vars = c("Iv"))

  dfmodellres[[kat]] <<- DFmodellres

})

Makrofigur$methods(grafisknum=function(samlikv=list(x=NULL, y=NULL), dftekst=NULL, manuell=1){

  #browser()

  ggobjnum <- ggplot() + geom_line(data = dfmodellres[[1]], aes(x = Iv, y = value, color = factor(variable))) +
    geom_point(aes(x=samlikv$x, y=samlikv$y)) +
    geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                     xend = samlikv$x, yend = dftekst$xlim[1]), lty = 2) +
    geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                     xend = dftekst$ylim[1], yend = samlikv$y), lty = 2) +
    geom_text(data=dftekst, aes(x, y, label=kurve), color=dftekst$farge)

  #browser()

  ggtyper <<- list(ggobjnum)

})

Makrofigur$methods(grafisknumappend=function(samlikve=list(x=0, y=0),  dftekst=NULL, manuell=1,
                                             tilstand=NULL){

  #browser()

  samlikvedf <- data.frame(x=samlikve$x, y=samlikve$y)

  ggobjnumapp <- ggtyper[[length(ggtyper)]] +
    geom_point(data=samlikvedf,aes(x=x, y=y)) +
    geom_segment(data=samlikvedf,aes(x = x, y = y ,
                                     xend = x, yend =  dftekst$xlim[1]), lty = 2) +
    geom_segment(data=samlikvedf, aes(x = x, y = y ,
                                      xend = dftekst$ylim[1], yend = y), lty = 2) +
    geom_line(data = dfmodellres[[tilstand]],
              aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data=dftekst, aes(x, y, label=kurve), color=dftekst$farge)

  ggtyper <<- append(ggtyper,list(ggobjnumapp))

})

Makrofigur$methods(grafiskstyle=function(labs=list(title=NULL, x=NULL, y=NULL),
                                         skaleringx=NULL,
                                         skaleringy=NULL,
                                         fargelinje=c('black','black'),
                                         figurnr = NULL){

  #browser()
  nrfigur <- c(length(ggtyper), figurnr)[ifelse(is.null(figurnr)==TRUE,1,2)]

  ggobjsty <- ggtyper[[nrfigur]] + labs(title = labs$title, x = labs$x, y = labs$y) +
    scale_x_continuous(labels = skaleringx$label, breaks=skaleringx$breaks, limits=skaleringx$limits) +
    scale_y_continuous(labels = skaleringy$label, breaks=skaleringy$breaks, limits=skaleringy$limits) +
    scale_colour_manual(values =fargelinje) +
  theme_classic() + theme(legend.position="none")

  ggtyper <<- append(ggtyper,list(ggobjsty))

})

## AD åpen flytende kurs
#############################################################################################################
iv <- list(i=2:7.5)
openpar <- eopenpar <- sopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
lukketpar <- elukketpar <- slukketpar <-  c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
flytislmoad <- Makrofigur(modellnavn='islmo')

# Samtidig
flytislmoad$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
flytislmoadtekst <- data.frame(kurve=c('IS-BoP','LM'), farge=c('red', 'red'), x = c(2,7), y = c(190, 170), xlim=100, ylim=2)
flytislmoad$grafisknum(samlikv=list(x=c(4.4), y=c(144)), dftekst=flytislmoadtekst, manuell=1)
flytislmoad$ggtyper[[1]] + coord_flip()

# Skift
elukketpar$P <- 0.75
flytislmoad$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=elukketpar, openpar=eopenpar, endrvar=iv, kat='endringG')
eflytislmoadtekst <- data.frame(kurve=c("IS-BoP'","LM'"), farge=c('red', 'red'), x = c(2,7), y = c(207, 200), xlim=100, ylim=2)
flytislmoad$grafisknumappend(samlikv=list(x=c(3.4), y=c(168)), dftekst=eflytislmoadtekst, manuell=1, tilstand='endringG')
flytislmoad$ggtyper[[2]] + coord_flip()

# Styling
flytislmoad$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(4.4)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(144)),
                         figurnr=1)

samtidigflytislmad <- flytislmoad$ggtyper[[3]]  + coord_flip()


flytislmoad$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(4.4, 3.4)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(144, 168)),
                         figurnr=2)

skiftflytislmad <- flytislmoad$ggtyper[[4]]  + coord_flip()
skiftflytislmad

















## IS-LM åpen flytende kurs
#############################################################################################################
iv <- list(i=2:7.5)
openpar <- eopenpar <- sopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
lukketpar <- elukketpar <- slukketpar <-  c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
flytislmo <- Makrofigur(modellnavn='islmo')

# Samtidig
flytislmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
flytislmotekst <- data.frame(kurve=c('IS-BoP','LM'), farge=c('red', 'red'), x = c(2,7), y = c(200, 170), xlim=100, ylim=2)
flytislmo$grafisknum(samlikv=list(x=c(4.4), y=c(144)), dftekst=flytislmotekst, manuell=1)
flytislmo$ggtyper[[1]] + coord_flip()

# Skift
elukketpar$oG <- 75
flytislmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=elukketpar, openpar=eopenpar, endrvar=iv, kat='endringG')
eflytislmotekst <- data.frame(kurve=c("IS-BoP'",""), farge=c('red', 'red'), x = c(2,3), y = c(220, 220), xlim=100, ylim=2)
flytislmo$grafisknumappend(samlikv=list(x=c(5.75), y=c(158)), dftekst=eflytislmotekst, manuell=1, tilstand='endringG')
flytislmo$ggtyper[[2]] + coord_flip()

# Stabilisering
slukketpar$M <- 79
flytislmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=slukketpar, openpar=sopenpar, endrvar=iv, kat='stabM')
sflytislmotekst <- data.frame(kurve=c("IS-BoP'","LM'"), farge=c('red', 'red'), x = c(2,7), y = c(220, 150), xlim=100, ylim=0)
flytislmo$grafisknumappend(samlikv=list(x=c(6.55), y=c(144)), dftekst=sflytislmotekst, manuell=1, tilstand='stabM')
flytislmo$ggtyper[[3]] + coord_flip()

# Styling
flytislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                       skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(4.4)),
                       skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(144)),
                       figurnr=1)

samtidigflytislm <- flytislmo$ggtyper[[4]]  + coord_flip()
samtidigflytislm

flytislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                       skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(4.4, 5.75)),
                       skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(144,158)),
                       figurnr=2)

skiftflytendeislm <- flytislmo$ggtyper[[5]]  + coord_flip()
skiftflytendeislm

flytislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                       skaleringx=list(label=c(TeX('$i_{0=2}}$'),TeX('$i_{1}}$')), breaks=c(4.4, 5.75)),
                       skaleringy=list(label=c(TeX('$Y_{0=2}}$'),TeX('$Y_{1}}$')), breaks=c(144,158)),
                       figurnr=3)

stabflytendeislm <- flytislmo$ggtyper[[6]]  + coord_flip()
stabflytendeislm


## IS-LM åpen fast kurs
#############################################################################################################
iv <- list(i=2:7.5)
openpar <- eopenpar <- sopenpar <- list(i_s=3.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
lukketpar <- elukketpar <- slukketpar <-  c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10, k =1, Y = 130, m=1, t=0.4))

fastislmo <- Makrofigur(modellnavn='islmo')

# Samtidig
fastislmo$numerisk(endvar=c('FastISC'), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
fastislmotekst <- data.frame(kurve=c('IS','BoP'), farge=c('red', 'red'), x = c(2,openpar$i_s+openpar$rp+0.25), y = c(190, 190), xlim=100, ylim=2)
fastislmo$grafisknum(samlikv=list(x=c(openpar$i_s+openpar$rp), y=c(172)), dftekst=fastislmotekst, manuell=1)
fastislmo$ggtyper[[1]] + coord_flip()

# Skift
eopenpar$i_s <- 3
fastislmo$numerisk(endvar=c('FastISC'), lukketpar=elukketpar, openpar=eopenpar, endrvar=iv, kat='endringP')
efastislmotekst <-  data.frame(kurve=c("IS","BoP'"), farge=c('red', 'red'), x = c(2,eopenpar$i_s+openpar$rp+0.25), y = c(190, 190), xlim=100, ylim=2)
fastislmo$grafisknumappend(samlikv=list(x=c(eopenpar$i_s+openpar$rp), y=c(179)), dftekst=efastislmotekst, manuell=1, tilstand='endringP')
fastislmo$ggtyper[[2]] + coord_flip()

# Stabilisering
slukketpar$oG <- 44
fastislmo$numerisk(endvar=c('FastISC'), lukketpar=slukketpar, openpar=eopenpar, endrvar=iv, kat='stabG')
sfastislmotekst <-  data.frame(kurve=c("IS'","BoP'"), farge=c('red', 'red'), x = c(2,eopenpar$i_s+openpar$rp+0.25), y = c(190, 190), xlim=100, ylim=2)
fastislmo$grafisknumappend(samlikv=list(x=c(eopenpar$i_s+openpar$rp), y=c(179)), dftekst=sfastislmotekst, manuell=1, tilstand='stabG')
fastislmo$ggtyper[[3]] + coord_flip()

# Styling
fastislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(3.75)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(172)),
                         figurnr=1)

samtidigfastislm <- fastislmo$ggtyper[[4]]  + coord_flip() + geom_line(data=data.frame(x=openpar$i_s+openpar$rp, y=110:200), aes(x,y), color ='black', size=0.5)



fastislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(openpar$i_s+openpar$rp, eopenpar$i_s+openpar$rp)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(172, 179)),
                         figurnr=2)

skiftfastislm <- fastislmo$ggtyper[[5]]  + coord_flip() + geom_line(data=data.frame(x=openpar$i_s+openpar$rp, y=110:200), aes(x,y), color ='black', size=0.5) + geom_line(data=data.frame(x=eopenpar$i_s+openpar$rp, y=110:200), aes(x,y), color ='black', size=0.5)
skiftfastislm

fastislmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                       skaleringx=list(label=c(TeX('$i_{0,2}}$'),TeX('$i_{1}}$')), breaks=c(openpar$i_s+openpar$rp, eopenpar$i_s+openpar$rp)),
                       skaleringy=list(label=c(TeX('$Y_{0,2}}$'),TeX('$Y_{1}}$')), breaks=c(172, 179)),
                       figurnr=3)

stabfastislm <- fastislmo$ggtyper[[6]]  + coord_flip() + geom_line(data=data.frame(x=openpar$i_s+openpar$rp, y=110:200), aes(x,y), color ='black', size=0.5) + geom_line(data=data.frame(x=eopenpar$i_s+openpar$rp, y=110:200), aes(x,y), color ='black', size=0.5)



Makrofigur$methods(dftransform=function(){
  paste0('Hello world')
  browser()
  dfmodellres[['init']]
})

fastislmoad <- Makrofigur(modellnavn='islmo')
fastislmoad$numerisk(endvar=c('SEQFastY','IAS'), lukketpar=lukketpar, openpar=openpar, endrvar=iv2, kat='init')
fastislmoadtekst <- data.frame(kurve=c('AS','AD'), farge=c('red','red'), x = c(3,4), y = c(150,150), xlim=c(145,150), ylim=c(3,4))
fastislmoad$grafisknum(samlikv=list(x=3, y=150), dftekst=fastislmoadtekst, manuell=1)

fastislmoad$ggtyper[[1]] + coord_flip()

## AD fast kurs
#############################################################################################################
iv <- list(i=2:6)
iv2 <- list(P=0:30)

openpar <- eopenpar <- sopenpar <- list(i_s=3.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
lukketpar <- elukketpar <- slukketpar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, h = 10,
                                                k =1, Y = 130, m=1, t=0.4,Pe=1, mu = 0.1, l_1=-20,l_2=1, z=1, A= 5,
                                                N=30, alpha = 1, Ac = 2))




# Samtidig
fastislmoad$numerisk(endvar=c('FastISC'), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
fastislmoadtekst <- data.frame(kurve=c('IS','BoP'), farge=c('red', 'red'), x = c(2,openpar$i_s+openpar$rp+0.1), y = c(190, 205), xlim=130, ylim=2)
fastislmoad$grafisknum(samlikv=list(x=c(openpar$i_s+openpar$rp), y=c(172)), dftekst=fastislmoadtekst, manuell=1)
fastislmoad$ggtyper[[1]] + coord_flip()

fastadlmo$numerisk(endvar=c('SEQFastY'), lukketpar=lukketpar, openpar=openpar, endrvar=iv2, kat='init')
fastadlmotekst <- data.frame(kurve=c('IS'), farge=c('red'), x = 1, y = 150, xlim=0, ylim=0)
fastadlmo$grafisknum(samlikv=list(x=c(1), y=c(150)), dftekst=fastadlmotekst, manuell=1)
fastadlmo$ggtyper[[1]] + coord_flip()

# Skift
elukketpar$P <- 0.75
fastislmoad$numerisk(endvar=c('FastISC'), lukketpar=elukketpar, openpar=eopenpar, endrvar=iv, kat='endringP')
efastislmoadtekst <- data.frame(kurve=c("IS'",""), farge=c('red', 'red'), x = c(2,6), y = c(207, 200), xlim=130, ylim=2)
fastislmoad$grafisknumappend(samlikv=list(x=c(openpar$i_s+openpar$rp), y=c(188)), dftekst=efastislmoadtekst, manuell=1, tilstand='endringP')
fastislmoad$ggtyper[[2]] + coord_flip()

# Styling
fastislmoad$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(3.75)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(172)),
                         figurnr=1)

samtidigfastislmad <- fastislmoad$ggtyper[[3]]  + coord_flip() + geom_line(data=data.frame(x=openpar$i_s+openpar$rp, y=130:200), aes(x,y), color ='black', size=0.5)
samtidigfastislmad

fastislmoad$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
                         skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(3.75, 3.75)),
                         skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(172, 188)),
                         figurnr=2)

skiftfastislmad <- fastislmoad$ggtyper[[4]]  + coord_flip() + geom_line(data=data.frame(x=openpar$i_s+openpar$rp, y=130:210), aes(x,y), color ='black', size=0.5)
skiftfastislmad




## AD- AS fast kurs
#############################################################################################################
iv <- list(P=2:4)

openpar <- eopenpar <- sopenpar <- list(i_s=3.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
lukketpar <- elukketpar <- slukketpar <-  c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, h = 10,
                                                 k =1, Y = 130, m=1, t=0.4,Pe=1, mu = 0.1, l_1=-20,l_2=1, z=1, A= 5,
                                                 N=30, alpha = 1, Ac = 2))

fastadlmo <- Makrofigur(modellnavn='adaso')
fastadlmo$numerisk(endvar=c('SEQFastY','SEQFlytY'), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
fastadlmotekst <- data.frame(kurve=c('AS'), farge=c('red'), x = c(3), y = c(150), xlim=c(145,150), ylim=c(3,4))
fastadlmo$grafisknum(samlikv=list(x=3, y=150), dftekst=fastadlmotekst, manuell=1)
fastadlmo$ggtyper[[1]] + coord_flip()


fastadlmo$dfmodellres
fastadlmo$dftransform()

# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# ## IS-LM åpen flytende kurs
# #############################################################################################################
# iv <- list(i=2:7.5)
# vopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# vislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
#                     P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
# evopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# evislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 75, b = 10, oI = 10, T = 50, M= 100,
#                      P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
# svopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# svislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 80,
#                      P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
#
# islmo <- Makrofigur(modellnavn='islmo')
# names(islmo$modellatr$islmo)
# islmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=vislmopar, openpar=vopenpar, endrvar=iv, kat='init')
# islmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=evislmopar, openpar=evopenpar, endrvar=iv, kat='endringG')
# islmo$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=svislmopar, openpar=svopenpar, endrvar=iv, kat='stabM')
#
# islmodftekst <- data.frame(kurve=c('IS-BoP','LM'), farge=c('red', 'red'), x = c(2,7), y = c(189, 170), xlim=100, ylim=0)
#
# eislmodftekst <- data.frame(kurve=c("IS-BoP'","LM"), farge=c('red', 'red'), x = c(2,7), y = c(220, 170), xlim=100, ylim=0)
#
# sislmodftekst <- data.frame(kurve=c("IS-BoP'","LM'"), farge=c('red', 'red'), x = c(2,7), y = c(220, 150), xlim=100, ylim=0)
#
# islmo$grafisknum(samlikv=list(x=c(4.4), y=c(144)), dftekst=islmodftekst, manuell=1)
#
# islmo$ggtyper[[1]]  + coord_flip()
#
# islmo$grafisknumappend(samlikv=list(x=c(5.75), y=c(158)), dftekst=eislmodftekst, manuell=1, tilstand='endringG')
#
# islmo$ggtyper[[2]]  + coord_flip()
#
# islmo$grafisknumappend(samlikv=list(x=c(6.5), y=c(144)), dftekst=sislmodftekst, manuell=1, tilstand='stabM')
#
# islmo$ggtyper[[3]]  + coord_flip()
#
# islmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                    skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(4.4)),
#                    skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(144)),
#                    figurnr=1)
#
# islmo$ggtyper[[4]]  + coord_flip()
#
# islmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                    skaleringx=list(label=c(TeX('$i_{1}}$')), breaks=c(4.4)),
#                    skaleringy=list(label=c(TeX('$Y_{1}}$')), breaks=c(144)),
#                    figurnr=2)
#
# islmo$ggtyper[[5]]  + coord_flip()
#
# islmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                    skaleringx=list(label=c(TeX('$i_{2}}$')), breaks=c(7.25)),
#                    skaleringy=list(label=c(TeX('$Y_{2}}$')), breaks=c(145)),
#                    figurnr=3)
#
# islmo$ggtyper[[6]]  + coord_flip()
#
#
# ## IS-LM åpen fast kurs
# #############################################################################################################
# iv <- list(i=2:7.5)
# openpar <- eopenpar <- sopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# lukketpar <- elukketpar <- slukketpar <-  c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
# fastislmo <- Makrofigur(modellnavn='islmo')
#
# flytislmo$numerisk(endvar=c(c('FastISC')), lukketpar=lukketpar, openpar=openpar, endrvar=iv, kat='init')
#
#
#
#
#
#
#
#
#
#
#
#
# adkurven$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=evislmopar, openpar=evopenpar, endrvar=iv, kat='endringP')
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# ## AD-kurven flytende kurs
# #############################################################################################################
# iv <- list(i=2:7.5)
# vopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# vislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
#                     P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
# evopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# evislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
#                      P=0.75, h = 10, k =1, Y = 130, m=1, t=0.4))
#
# adkurven <- Makrofigur(modellnavn='islmo')
# names(adkurven$modellatr$islmo)
# adkurven$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=vislmopar, openpar=vopenpar, endrvar=iv, kat='init')
# adkurven$numerisk(endvar=c('FlytISCBoP','FlytLMC'), lukketpar=evislmopar, openpar=evopenpar, endrvar=iv, kat='endringP')
#
# islmodftekst <- data.frame(kurve=c('IS-BoP','LM'),
#                            farge=c('red', 'red'),
#                            x = c(2,7),
#                            y = c(189, 170),
#                            xlim=100,
#                            ylim=0)
#
# eislmodftekst <- data.frame(kurve=c("IS-BoP'","LM'"),
#                             farge=c('red', 'red'),
#                             x = c(2,7),
#                             y = c(207, 200),
#                             xlim=100,
#                             ylim=0)
#
# adkurven$grafisknum(samlikv=list(x=c(4.4), y=c(144)), dftekst=islmodftekst, manuell=1)
#
#
# islmo$ggtyper[[1]]  + coord_flip()
#
#
# adkurven$grafisknumappend(samlikv=list(x=c(3.4), y=c(167)), dftekst=eislmodftekst,
#                           manuell=1, tilstand='endringP')
#
#
# adkurven$ggtyper[[2]]  + coord_flip()
#
# adkurven$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                       skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(4.4, 3)),
#                       skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(144,167)),
#                       figurnr=2)
#
# adkurven$ggtyper[[3]]  + coord_flip()
#
#
# ## AD-kurven fast kurs
# #############################################################################################################
# iv <- list(i=2:7.5)
# vopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# vislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10, k =1, Y = 130, m=1, t=0.4))
# evopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
# evislmopar <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=0.75, h = 10, k =1, Y = 130, m=1, t=0.4))
#
# adiskurvene <- Makrofigur(modellnavn='islmo')
# adiskurvene$numerisk(endvar=c('FastISC'), lukketpar=vislmopar, openpar=vopenpar, endrvar=iv, kat='init')
# adiskurvene$numerisk(endvar=c('FastISC'), lukketpar=evislmopar, openpar=evopenpar, endrvar=iv, kat='endringP')
#
# islmodftekst <- data.frame(kurve=c('IS-BoP','BoP'), farge=c('red', 'red'), x = c(2,5.25), y = c(200, 230), xlim=100, ylim=2)
# adiskurvene$grafisknum(samlikv=list(x=c(5), y=c(155)), dftekst=islmodftekst, manuell=1)
# adiskurvene$ggtyper[[1]]
#
# adiskurvene$grafiskstyle(labs=list(title='Mundell-Fleming modellen - fast kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                          skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(5)),
#                          skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(155)),
#                          figurnr=1)
#
# # Graf 1
# adiskurvene$ggtyper[[2]]  + coord_flip() + geom_line(data=data.frame(x=5, y=110:230), aes(x,y), color ='black', size=0.5)
#
#
# eislmodftekst <- data.frame(kurve=c("IS-BoP'","BoP"), farge=c('red', 'red'), x = c(2,5.25), y = c(220, 230), xlim=100, ylim=2)
# adiskurvene$grafisknumappend(samlikv=list(x=c(5), y=c(173)), dftekst=eislmodftekst, manuell=1, tilstand = 'endringP')
#
# adiskurvene$ggtyper[[3]] + coord_flip()
#
# adiskurvene$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                          skaleringx=list(label=c(TeX('$i_{0}}$'),TeX('$i_{1}}$')), breaks=c(5, 3)),
#                          skaleringy=list(label=c(TeX('$Y_{0}}$'),TeX('$Y_{1}}$')), breaks=c(155,167)),
#                          figurnr=3)
#
# adiskurvene$ggtyper[[4]] + coord_flip() + geom_line(data=data.frame(x=5, y=110:230), aes(x,y), color ='black', size=0.5)
#
#
#
# islmo$grafisknumappend(samlikv=list(x=c(5.75), y=c(158)), dftekst=eislmodftekst, manuell=1, tilstand='endringG',
#                        figurnr = 1)



# islmo$grafiskstyle(labs=list(title='Mundell-Fleming modellen - flytende kurs',x='rentenivå (i)', y='produksjon, inntekt (Y)'),
#                       skaleringx=list(label=c(TeX('$i_{0}}$')), breaks=c(4.4)),
#                       skaleringy=list(label=c(TeX('$Y_{0}}$')), breaks=c(144)))
# #
# eislmodftekst <- data.frame(kurve=c("IS-BoP'","LM"),
#                            farge=c('red', 'red'),
#                            x = c(2,7),
#                            y = c(220, 170),
#                            xlim=100,
#                            ylim=0)
#
#
# islmo$ggtyper[[3]] + coord_flip()
#
#islmo$grafisknumappend(samlikv=list(x=c(5.7), y=c(157)), manuell=1)

#islmo$ggtyper[[1]]


#
#
#
# ## Keynes
# ## Samtidig likevekt
# Yv = list(Y=1:600)
# keypar <- list(c_1 = 0.6, oC = 25, oG= 50, b = 30, i = 0.03, oI = 50, T = 10)
# ekeypar <- list(c_1 = 0.6, oC = 25, oG= 60, b = 30, i = 0.03, oI = 50, T = 10)
# eekeypar <- list(c_1 = 0.6, oC = 25, oG= 100, b = 30, i = 0.03, oI = 50, T = 10)
#
# keynes <- Makrofigur('keynes')
# #names(keynes$modellatr$keynes)
# keynes$numerisk(endvar=c('ID', 'IDPGD', 'IDPGDPCD' ,'YS'), lukketpar=keypar, endrvar=Yv, kat='init')
# keynes$numerisk(endvar=c('ID', 'IDPGD', 'IDPGDPCD' ,'YS'), lukketpar=ekeypar, endrvar=Yv, kat='endringG')
# keynes$numerisk(endvar=c('ID', 'IDPGD', 'IDPGDPCD' ,'YS'), lukketpar=eekeypar, endrvar=Yv, kat='endring2G')
#
# names(keynes$dfmodellres)
# keynes$grafisknum(samlikv=list(x=c(250), y=c(250)), manuell=1)
# keynes$grafisknumappend(samlikv=list(x=c(450), y=c(350)), manuell=1)
# keynes$grafisknumappend(samlikv=list(x=c(550), y=c(350)), manuell=1)
# keynes$grafiskstyle()
# keynes$ggtyper
# #grid.arrange(keynes$ggtyper)
# #grid.arrange(keynes$ggtyper[[1]], keynes$ggtyper[[2]] , keynes$ggtyper[[3]], keynes$ggtyper[[4]])

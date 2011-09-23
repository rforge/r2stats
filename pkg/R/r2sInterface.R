#--------------------------------------------------------------------------------------------------
#
#                      R2STATS: A Graphical User Interface for GLM and GLMM in R
#                       Yvonnick Noel, University of Brittany, Rennes 2, France
#                                            2006-2010
#
#--------------------------------------------------------------------------------------------------
#                                      INTERFACE PROTOTYPE
#--------------------------------------------------------------------------------------------------

# A first dataset
data(sleepstudy)

# Required libraries
options(guiToolkit="RGtk2",width=160)
require(gWidgetsRGtk2)
require(proto)
require(MASS)
require(lme4)
require(RGtk2Extras)
# require(ordinal)

r2stats = proto(

  # Create the GUI
  create = function(.) {

    # Main Window
    .$mainWindow = gwindow("R2STATS",visible=FALSE)
    add(.$mainWindow,bigGroup <- ggroup(horizontal=FALSE),expand=TRUE)

    # Menus
    tmp = list()
    tmp$Session$"Préférences"$handler = .$editOptions
    tmp$Session$sep$separator = TRUE
    tmp$Session$Quitter$handler = function(h,...) dispose(.$mainWindow)
    tmp$Session$Quitter$icon = "quit"
    tmp$Outils$"Calculateur de probabilités"$handler = .$probCalc
    tmp$Outils$"Mise à jour"$handler = .$updateR2stats
    .$menu = gmenu(tmp,cont=.$mainWindow)

    # Tableau d'onglets
    add(bigGroup,.$mainNotebook <- gnotebook(closebuttons=TRUE,dontCloseThese=1:5),expand=TRUE)

    #-------------------------------------------------- File Tab ---------------------------------------------
    add(.$mainNotebook,   dataGroup <- ggroup(horizontal=FALSE),label="Fichiers",override.closebutton=TRUE,expand=TRUE)
    add(dataGroup,dataNb    <- gnotebook(),expand=TRUE)

    # Load a file from a local or remote URL
    add(dataNb,        urlGroup     <- ggroup(horizontal=FALSE),label=" Adresse ",expand=TRUE)
    add(urlGroup,      urlFrame     <- gframe("Charger un fichier texte local ou distant",horizontal=FALSE),expand=TRUE)
    add(urlFrame,      tmp          <- ggroup())
    add(tmp,          .$dataUrl     <- gedit("http://yvonnick.noel.free.fr/data/",handler=.$dataLoad),expand=TRUE)
    addhandlerchanged(.$dataUrl,.$updateDFLists)
    add(tmp,                           gbutton(" Disque  ",handler=.$fileLoad))
    add(urlFrame,     .$hasHeader   <- gcheckbox("Entêtes de colonnes sur la première ligne",checked=TRUE))
    add(urlFrame,     .$hasRowNames <- gcheckbox("Noms d'observations dans la première colonne",checked=FALSE))
    addSpring(urlFrame)
    add(urlFrame,      tmp          <- ggroup())
    addSpring(tmp)
    add(tmp,           .$loadFromFileButton <- gbutton("  Charger  ",handler=.$dataLoad))
    addHandlerClicked(.$loadFromFileButton, handler=.$updateDFLists)

    # Load from a library
    add(dataNb,       libGroup      <- ggroup(horizontal=FALSE),label=" Librairie ",expand=TRUE)
    add(libGroup,     listlibFrame  <- gframe("Charger un fichier de données depuis une librairie R",horizontal=FALSE),expand=TRUE)
    add(listlibFrame, g1            <- ggroup(),expand=TRUE)
    add(g1,           tmp           <- ggroup(horizontal=FALSE))
    add(tmp,                          glabel("  Librairies disponibles  "))
    add(tmp,         .$libList      <- gtable(.$getLibList()),expand=TRUE)
    addhandlerclicked(.$libList,      .$updateLibDataList)
    add(g1,           tmp           <- ggroup(horizontal=FALSE),expand=TRUE)
    add(tmp,                           glabel("Fichiers de données"))
    add(tmp,         .$libDataList  <- gtable(.$getLibDataList(),handler=.$loadDataLib), expand=TRUE)
    addHandlerDoubleclick(.$libDataList, handler=.$updateDFLists)
    add(listlibFrame, tmp           <- ggroup())
    addSpring(tmp)
    add(tmp,                           gbutton(" Détails ",handler=.$getDataDescription))
    add(tmp,          .$loadFromLibButton  <- gbutton(" Charger ",handler=.$loadDataLib))
    addHandlerClicked(.$loadFromLibButton, handler=.$updateDFLists)

    svalue(dataNb) = 1

    #---------------------------------------------- Data grids Tab --------------------------------------
    add(.$mainNotebook,  gridTab    <- ggroup(),label="Données",override.closebutton=TRUE,expand=TRUE)
    add(gridTab,         gridGroup  <- ggroup(horizontal=FALSE), expand=TRUE)

    # Recode utility
    # add(gridGroup,  transfrm        <- gexpandgroup("Recodage et transformation"))
    transfrm        <- gexpandgroup("Recodage et transformation",cont=gridGroup)
    # add(transfrm,   tmp             <- ggroup(horizontal=FALSE))
    tmp             <- ggroup(horizontal=FALSE,cont=transfrm)
    add(tmp,        glabel("Avec :"))
    add(tmp,       .$currentFactor  <- gdroplist("Aucune",handler=.$printCat))
    add(transfrm,   tmp             <- ggroup(horizontal=FALSE),expand=TRUE)
    add(tmp,        glabel("Appliquer (ex. \"cat1,cat2=cat12;cat3,cat4=cat34\" ou log(.)) :"))
    add(tmp,       .$toCompute      <- gedit("",width=40))
    add(transfrm,   tmp             <- ggroup(horizontal=FALSE))
    add(tmp,        glabel("Stoquer dans :"))
    add(tmp,       .$newVarName     <- gdroplist("Aucune",editable=TRUE))
    add(transfrm,   tmp             <- ggroup(horizontal=FALSE))
    addSpring(tmp)
    add(tmp,                           gbutton("Exécuter",handler=.$varTransform))

    # Data grids
    add(gridGroup, .$gridNotebook   <- gnotebook(closebuttons=TRUE), expand=TRUE)
    addhandlerchanged(.$gridNotebook,.$updateTransf, action="ontabchange")
    addhandlerchanged(.$gridNotebook,.$changeDataset,action="ontabchange")

    # Open a default data grid
    dfList = get_all_tables()
    .$currentDataName = dfList[1]
    add(.$gridNotebook, tmp <- ggroup(),label = .$currentDataName,expand=TRUE)
    add(tmp,  gdfedit(eval(parse(text=.$currentDataName),envir=.GlobalEnv),name=.$currentDataName), expand=TRUE)

    # Data selector
    add(gridGroup,tmp             <- ggroup())
    add(tmp,          .$openGrid  <- gdroplist(dfList,handler=.$changeDataset,action="openswitchgrid"))
    addHandlerChanged(.$openGrid,   .$updateGrid)
    add(tmp,             myicon1  <- gimage("refresh",dir="stock",handler=.$updateDFLists))
    tooltip(myicon1) = "Cliquez pour mettre à jour la liste des tableaux disponibles en mémoire."

    addSpring(tmp)
    add(tmp,                           gbutton(" Enregistrer ",handler=.$saveGrid))

    #-------------------------------------------------- Model Tab --------------------------------------------
    add(.$mainNotebook,bigFrame     <- ggroup(),label="Modèles",override.closebutton=TRUE,expand=TRUE)
    add(bigFrame,     .$modelPanedGroup   <- gpanedgroup(),expand=TRUE)
    add(.$modelPanedGroup,  leftGroup    <- ggroup(horizontal=FALSE),expand=TRUE)
    add(leftGroup,     tabFrame     <- gframe("Tableau des données",horizontal=FALSE),expand=TRUE)
    add(tabFrame,      tmp          <- ggroup())
    add(tmp,          .$currentData <- gdroplist(dfList,handler=.$changeDataset,action="changemodeldata"),expand=TRUE)
    addhandlerchanged(.$currentData, .$updateVarList)
    addhandlerchanged(.$currentData, .$clearModelFields)
    addhandlerchanged(.$currentData, .$updateWeightList)
    addhandlerchanged(.$currentData, .$updateConstrFactor)
    add(tmp,             myicon2 <-   gimage("refresh",dir="stock",handler=.$updateDFLists))
    tooltip(myicon2) = "Cliquez pour mettre à jour la liste des tableaux disponibles en mémoire."

    add(tabFrame,     .$varList     <- gtable(.$getVarList(.$getDataName()),multiple=TRUE),expand=TRUE)
    
    addHandlerDoubleclick(.$varList, handler = .$updateFIVField, action = "add")
    addhandlerclicked(    .$varList, handler = .$updateVarSummary)
    
    add(leftGroup,     varFrame <- gframe("Résumé de variable"),expand=TRUE)
    add(varFrame,     .$varSummary  <- gtable(cbind(Attribut="Aucun",Valeur="Aucune")),expand=TRUE)

    add(.$modelPanedGroup,  rightFrame   <- ggroup(horizontal=FALSE),expand=TRUE)
    add(rightFrame,    modelFrame   <- gframe("Définition de modèle",horizontal=FALSE),expand=TRUE)

    .$distribList = gdroplist(c("Normale","Binomiale","Poisson","Gamma","Gaussienne inverse","Multinomiale","Multinomiale ordonnée"),handler=.$updateLink)
    .$linkLists = list(Normale      = c("Identique","Log","Inverse"),
                       Binomiale    = c("Logit","Probit","Cauchit","Log","Cloglog"),
                       Poisson      = c("Log","Racine","Identique"),
                       Gamma        = c("Inverse","Log","Identique"),
                       "Gaussienne inverse" = c("Inverse","Log","Identique","1/mu2"),
                       Multinomiale = c("Logit", "Probit", "Cloglog", "Loglog","Cauchit", "Aranda-Ordaz", "Log-gamma"),
            "Multinomiale ordonnée" = c("Logit", "Probit", "Cloglog", "Loglog","Cauchit", "Aranda-Ordaz", "Log-gamma"))
    .$currentLinkList = gdroplist(.$linkLists[[svalue(.$distribList)]])
    .$modelName = gdroplist(c("Nouveau"),editable=TRUE,handler=.$retrieveModel)
    .$dvList  = gtext(height=64,width=120)
    .$fivList = gtext(height=64,width=120)

    add(modelFrame,tmp <- ggroup())
    add(tmp,glabel("Nom du modèle"))
    add(tmp,.$modelName)
    addSpring(tmp)
    add(tmp,gbutton("Nouveau",handler=.$clearModelFields))

    add(modelFrame,frm <- gframe("Variables dépendantes",horizontal=FALSE),expand=TRUE)
    add(frm,.$dvList,expand=TRUE)
    add(frm,tmp <- ggroup())
    add(tmp,gbutton("Ajouter",handler=.$updateDVField))
    addSpring(tmp)
    add(tmp,gbutton("Effacer",handler=.$clearDVField))

    add(modelFrame,frm <- gframe("Variables indépendantes",horizontal=FALSE),expand=TRUE)
    add(frm,.$fivList,expand=TRUE)
    add(frm,tmp <- ggroup())
    add(tmp,gbutton("Ajouter",  handler=.$updateFIVField,action="add"))
    addSpring(tmp)
    add(tmp,gbutton(" + ",      handler=.$updateFIVField,action="+"))
    add(tmp,gbutton(" : ",      handler=.$updateFIVField,action=":"))
    add(tmp,gbutton(" * ",      handler=.$updateFIVField,action="*"))
    add(tmp,gbutton(" - ",      handler=.$updateFIVField,action="-"))
    add(tmp,gbutton(" () ",     handler=.$updateFIVField,action="()"))
    add(tmp,gbutton("Fixée",    handler=.$updateFIVField,action="offset"))
    add(tmp,gbutton(" +1 ",     handler=.$updateFIVField,action="1"))
    add(tmp,gbutton(" +0 ",     handler=.$updateFIVField,action="-1"))
    addSpring(tmp)
    add(tmp,gbutton(" | ",      handler=.$updateFIVField,action="|"))
    add(tmp,gbutton(" (1|.) ",  handler=.$updateFIVField,action="(1|)"))
    add(tmp,gbutton(" (.| ) ",  handler=.$updateFIVField,action="(.|)"))
    add(tmp,gbutton(" / ",      handler=.$updateFIVField,action="/"))
    addSpring(tmp)
    add(tmp,gbutton("Effacer",handler=.$clearFIVField))

    addSpring(modelFrame)
    add(modelFrame,tmp <- ggroup(),expand=TRUE)
    addSpring(tmp)
    add(tmp,layout <- glayout(homogeneous=FALSE),expand=TRUE)
    layout[1,1] = glabel(" Loi de distribution ")
    layout[1,2] = glabel("   Fonction de lien   ")
    layout[1,3] = glabel("Variable de pondération")
    layout[1,4] = glabel(" Facteur de contrainte ")
    layout[2,1] =  .$distribList
    layout[2,2] =  .$currentLinkList
    layout[2,3] <- .$weightList <- gdroplist(c("Aucune",.$getNumVarList(.$getCurrentDataName())))
    layout[2,4] <- .$structList <- gdroplist(c("Aucun","Constant",.$getCatVarList(.$getCurrentDataName())))
    layout[3,1] = glabel("Sélection obs.")
    layout[3,2:4] <- .$subsetVar <- gedit("")
    addSpring(tmp)

    add(rightFrame,tmp <- ggroup())
    addSpring(tmp)
    add(tmp,gbutton("Estimer",handler=.$run))

    #------------------------------------------------ Result Tab ------------------------------------------
    add(.$mainNotebook,resBigFrame <- ggroup(horizontal=FALSE),override.closebutton=TRUE,label="Résultats")
    add(resBigFrame,.$results <- gtext(wrap=FALSE),expand=TRUE)
    add(resBigFrame,tmp <- ggroup())
    add(tmp,gbutton("Effacer",handler=.$clearResults))

    #------------------------------------------------- Plot Tab -----------------------------------------
    add(.$mainNotebook,  graphBigFrame   <- ggroup(),override.closebutton=TRUE,label="Graphiques")
    add(graphBigFrame,  .$graphPanedGroup <- gpanedgroup(),expand=TRUE)
    add(.$graphPanedGroup, graphLeftGroup  <- ggroup(horizontal=FALSE),expand=TRUE)
    add(graphLeftGroup,                     glabel("Type de graphique :"))
    add(graphLeftGroup, .$plotType       <- gdroplist(c("Graphique de régression","Distribution de la réponse","Valeurs prévues et observées","Graphique quantile-quantile","Distribution des résidus","Prévisions et résidus"),handler=.$plotCurrentModel,action="plot"))
    add(graphLeftGroup,  graphNb         <- gnotebook(), expand=TRUE)
    add(graphNb,         graphModelGroup <- ggroup(horizontal=FALSE),label=" Modèle ",expand=TRUE)
    add(graphModelGroup,.$graphModelList <- gtable(.$getModelList()),expand=TRUE)
    addhandlerclicked(  .$graphModelList,  .$plotCurrentModel,action="plot")
    add(graphNb,         graphParamGroup <- ggroup(horizontal=FALSE),label=" Options ",expand=TRUE)
    add(graphParamGroup,layout <- glayout())
    
    layout[1,1] = glabel("Légende")
    layout[1,2] <- .$legendLoc <- gdroplist(c("Aucune","A droite","A gauche","En haut","En bas"),handler=.$plotCurrentModel,action="plot")
    svalue(.$legendLoc,index=TRUE) = 2
    layout[1,3] <- .$legendCols <- gdroplist(paste(1:10,"col."),handler=.$plotCurrentModel,action="plot")
    layout[2,1] = glabel("Limites X")
    layout[2,2:3] <- .$graphLimitsX <- gedit("",handler=.$plotCurrentModel,action="plot")
    layout[3,1] = glabel("Limites Y")
    layout[3,2:3] <- .$graphLimitsY <- gedit("",handler=.$plotCurrentModel,action="plot")
    layout[4,1] <- "Sélection"
    layout[4,2:3] <- .$groupList <- gdroplist(c("Tous groupes"),handler=.$plotCurrentModel,action="plot")
    layout[5,1] <- glabel("Ajouter")
    layout[5,2:3] <- .$addData <- gcheckbox("Données",checked=TRUE,handler=.$plotCurrentModel,action="plot")
    layout[6,2:3] <- .$addModel <- gcheckbox("Modèle",checked=TRUE,handler=.$plotCurrentModel,action="plot")
    layout[7,2:3] <- .$addRandCurves <- gcheckbox("Courbes aléatoires",checked=TRUE,handler=.$plotCurrentModel,action="plot")
    layout[8,2:3] <- .$addRefLine <- gcheckbox("Droite de référence",handler=.$plotCurrentModel,action="plot")
    layout[9,2:3] <- .$addGrid <- gcheckbox("Grille",handler=.$plotCurrentModel,action="plot")
    layout[10,2:3] <- .$addNoise <- gcheckbox("Bruitage",handler=.$plotCurrentModel,action="plot")
    
    add(.$graphPanedGroup,graphRightGroup <- ggroup(horizontal=FALSE),expand=TRUE)
    add(graphRightGroup,.$plotArea <- ggraphics())
    svalue(graphNb) = 1

    addhandlerclicked(.$plotArea,.$getXY)
    add(graphRightGroup, tmp <- ggroup())
    addSpring(tmp)
    add(tmp,.$obsId <- glabel(""))
    addSpring(tmp)
    add(tmp,gbutton("  Enregistrer  ", handler=.$savePlot,action="save"))
    add(tmp,gbutton("     Copier    ", handler=.$copyPlot,action="save"))
    add(tmp,gbutton("   Réafficher   ",handler=.$plotCurrentModel,action="plot"))

    #--------------------------------------------------- Model comparison tab -------------------------------------
    add(.$mainNotebook,compBigFrame <- ggroup(horizontal=FALSE),override.closebutton=TRUE,label="Comparaisons")
    add(compBigFrame,modelGroup <- gframe("Modèles testés"),expand=TRUE,horizontal=FALSE)
    add(modelGroup,.$modelList <- gtable(.$getModelList(),multiple=TRUE),expand=TRUE)
    add(compBigFrame,tmp <- ggroup())
    add(tmp,gbutton("Supprimer",handler=.$deleteModels))
    addSpring(tmp)
    add(tmp,gbutton("Tout sélectionner",handler=.$selectAllModels))
    add(tmp,gbutton("Comparer",handler=.$compareModels))

    # Status bar
    add(bigGroup,.$status <- gstatusbar("Prêt."))

    # Global variables
    .$currentPlot.XY = NULL
  },
  ### Make the R2STATS main window visible
  show = function(.) {

    if(is.null(.$mainWindow)) {
      gmessage("Erreur : l'interface n'est pas créée.")
      return()
    }
    
    # Seems necessary to have the graphics tab appear first for it to be the default plot
    # or the default cairoDevice would pop up
    svalue(.$mainNotebook) = 5

    # Popup main window
    visible(.$mainWindow) = TRUE
    .$setPlotParams()
    
    # Model tab appears first
    svalue(.$mainNotebook) = 3
    svalue(.$modelPanedGroup) = .25
    svalue(.$graphPanedGroup) = .35

  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS FILE MANAGEMENT METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Data file selector
  fileChoose = function(.,type) {
  
    # Under W32
    if(Sys.info()["sysname"] == "Windows") invisible(file.choose())
    # Under Linux
    else   invisible(gfile(text="Sélecteur de fichier",type=type,filter=list("Tous"=list(patterns = c("*")))))
  },
  ### Data file selection (from local disk)
  fileLoad = function(.,h,...) {

    filename = .$fileChoose("open")

    if(is.na(filename)) return()
    if(filename == "")    return()

    # The working directory is implicitly set
    setwd(dirname(filename))

    # This change triggers dataLoad() and updateDFLists() handlers
    svalue(.$dataUrl) = filename
  },
  ### Data file selection (from direct path or URL input)
  dataLoad = function(.,h,...) {
    
    filename = .$trim(svalue(.$dataUrl))

    if(is.na(filename)) return()
    if(filename == "")  return()
    
    .$setStatus("Statut : téléchargement du fichier en cours. Veuillez patienter....")

    # Extract file extension
    name.ext = .$getFileExtension(filename)
    tabname =  .$getBaseName(filename)
    r.names = NULL
    if(svalue(.$hasRowNames)) r.names = 1

    if(tolower(name.ext) == "csv") res = try(assign(tabname, read.csv2(filename,header=svalue(.$hasHeader),row.names=r.names),envir = .GlobalEnv))
    else                           res = try(assign(tabname,read.table(filename,header=svalue(.$hasHeader),row.names=r.names),envir = .GlobalEnv))
    
    if(inherits(res,"try-error")) {
      gmessage("Erreur de chargement : le serveur est\n peut-étre indisponible ou le\n nom du fichier incorrect.\n")
     .$setStatus("Statut : Prêt.")
      return()
    }

    # Close the corresponding grid upon reloading
    displayed.grids = names(.$gridNotebook) 
    if(tabname %in% displayed.grids) {
      pos = which(displayed.grids == tabname)
      svalue(.$gridNotebook) = pos
      dispose(.$gridNotebook)
    }

    # Update list of data frames (this implicitly updates the variable list and then open the data grid)
   .$currentDataName = tabname
    
    # Switch to data grid tab
    svalue(.$mainNotebook) = 2

   .$setStatus("Statut : Prêt.")

  },
  ### Get the list of installed libraries
  getLibList = function(.) {
    .packages(all.available = TRUE)
  },
  ### Get the list of datasets available in the selected library
  getLibDataList = function(.) {
  
    dl = data(package=svalue(.$libList))$results[,3:4]
    if(is.matrix(dl)) ndset = nrow(dl)
    if(is.vector(dl)) ndset = 1        # data() doesn't return a matrix when there is only one dataset!

    if(ndset==0)      { dl = data.frame(Tableau="Aucun",Description="Aucune",stringsAsFactors=FALSE) }
    else if(ndset==1) { dl = data.frame(Tableau=dl[1],  Description=dl[2],   stringsAsFactors=FALSE) }
    else              { colnames(dl) <- c("Tableau","Description") }
    dl
  },
  ### Update the list of available datasets upon library selection
  updateLibDataList = function(.,h,...) {
    .$libDataList[,] = .$getLibDataList()
  },
  ### Load a dataset from a selected library
  loadDataLib = function(.,h,...) {
  
    tabname = svalue(.$libDataList)
    data(list=tabname,package=svalue(.$libList))
    cl = class(eval(parse(text=tabname),envir=.GlobalEnv))

    if(cl != "data.frame")	{
      gmessage(paste("Ce tableau est de type",cl,"\net ne peut étre édité dans l\'interface.\nIl a néamoins été chargé en mémoire."))
      return()
    }
	
    # Set to current data
    .$currentDataName = tabname
        
    # Switch to the datagrid tab
    svalue(.$mainNotebook) = 2
  },
  ### Get the vector of level names in a given categorical variable in a data table
  getFacLevelList = function(.,dataname,varname) {

     level.names = "Aucune"
     datatab = eval(parse(text=dataname),envir=.GlobalEnv)
     if(is.factor(datatab[,varname]))    level.names = levels(datatab[,varname])
     if(is.character(datatab[,varname])) level.names = sort(unique(datatab[,varname]))
     
     cbind("Modalités"=level.names)
  },
  ### Display the help file about this dataset
  getDataDescription = function(.,h,...) {
  
    if(Sys.info()["sysname"] == "Windows")
      eval(parse(text=paste("help('",svalue(.$libDataList),"',package='",svalue(.$libList),"')",sep="")),envir=.GlobalEnv)

    else
      eval(parse(text=paste("ghelp('",svalue(.$libDataList),"',package='",svalue(.$libList),"',container=TRUE)",sep="")),env=.GlobalEnv)

  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS DATA MANAGEMENT METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Get the selected data frame
  getDataName = function(.) {
    return(svalue(.$currentData))
  },
  ### Get the active dataset
  getCurrentDataName = function(.) {
    return(.$currentDataName)
  },
  ### Update the table list in the grid tab
  updateDFLists = function(.,h,...) {

    available.tables = get_all_tables()

    if(is.null(available.tables)) {
      available.tables = "Aucun tableau"
      nt = 0
      return()
    }
    
    nt = length(available.tables)

    .$openGrid[] = available.tables
    svalue(.$openGrid) = .$currentDataName

    .$currentData[] = available.tables
    svalue(.$currentData) = .$currentDataName
    
    .$setStatus(paste("Statut :",nt,"tableau(x) dans l'espace de travail."))
  },
  ### Update the data grids upon grid selector change
  updateGrid = function(.,h,...) {

    dataName = .$currentDataName

    # Warning: a droplist may be temporarily set to NULL when changed
    if(is.null(dataName)) return()
    
    # No available table in working memory
    if(dataName == "Aucun tableau") return()
    
    # Which tables are already displayed?
    displayed.grids = names(.$gridNotebook)
    
    # None: Open the table
    if(is.null(displayed.grids)) {
      .$showData(dataName)
      return()
    }
    
    # Which one is visible?
    which.visible = displayed.grids[svalue(.$gridNotebook)]

    # Already selected: Useless to trigger all other handlers
    if(which.visible == dataName) return()

    # Put the data grid to the foreground if already opened
    if(dataName %in% displayed.grids) {
      svalue(.$gridNotebook) = which(displayed.grids == dataName)
      return()
    }
    
    # ... or open it
    .$showData(dataName)
  },
  ### Variable transformations
  varTransform = function(.,h,...) {

    # Current data tab
    displayed.grids = names(.$gridNotebook)
    if(length(displayed.grids)==0) return()
    
    # Current data  table and variables
    dataname = displayed.grids[svalue(.$gridNotebook)]
    varList = .$getVarList(dataname)
    varTypes = varList[,"Type"]
    names(varTypes) = varList[,"Variables"]

    transExp = .$trim(svalue(.$toCompute))
    if(transExp == "") return()

    sourceVar = svalue(.$currentFactor)
    destVar   = svalue(.$newVarName)
    sourceAccess = paste(dataname,"[,'",sourceVar,"']",sep="")
    destAccess = paste(dataname,"[,'",destVar,"']",sep="")
    source.isFactor = varTypes[sourceVar] == "F"
    
    # RECODING
    if(source.isFactor) {
    
      eval(parse(text=paste(destAccess,"= as.character(",sourceAccess,")")),envir=.GlobalEnv)

      # Parse and execute commands
      commands = unlist(strsplit(transExp,";"))
      for(com in commands) {
        com = .$removeSpaces(com)
        arguments = unlist(strsplit(com,"="))
        if(length(arguments) != 2) {
          gmessage("Problème dans la définition des commandes.")
          return()
        }
	      sourceCat = unlist(strsplit(arguments[1],","))
	      newCat = arguments[2]
	      eval(parse(text=paste(dataname,"[",destAccess," %in% c('",paste(sourceCat,collapse="','"),"'),'",destVar,"'] ='",newCat,"'",sep="")),envir=.GlobalEnv)
      }
	
	  # Recode to a factor or to a numeric (indicator) variable
	  if(any(is.na(as.numeric(eval(parse(text=destAccess),envir=.GlobalEnv)))))
	    eval(parse(text=paste(destAccess,"=factor(",destAccess,")",sep="")),envir=.GlobalEnv)
	  else
	    eval(parse(text=paste(destAccess,"=as.numeric(",destAccess,")",sep="")),envir=.GlobalEnv)
    }
    
    # NUMERIC TRANSFORM
    else {
    
      # The dot is interpreted as the source variable itself
      transExp = .$removeSpaces(transExp)
      transExp = gsub("[.]",sourceVar,transExp)

      eval(parse(text=paste(destAccess,"<-with(",dataname,",",transExp,")")),envir=.GlobalEnv)
    }
    
    # Update various variable lists
    if(svalue(.$currentData)==dataname) {
      .$updateVarList(h,...)
      .$updateWeightList(h,...)
      .$updateConstrFactor(h,...)
    }

    # Redisplay data (this also call updateTransf() via the implicit tab change
   .$showData(dataname)

    # Update the droplists
    svalue(.$currentFactor) = destVar
    svalue(.$newVarName)    = destVar
  },
  ### Update the transform fields when dataset is changed
  updateTransf = function(.,h,...) {

    displayed.grids = names(.$gridNotebook)
    if(length(displayed.grids) == 0) { 
     .$currentFactor[,]   = ""
	    svalue(.$toCompute) = ""
	   .$newVarName[,]      = ""
	    return()
    }
    
    # Get the current active data tab
    if(is.null(h$action))             dataname = displayed.grids[svalue(.$gridNotebook)]
    else if(h$action=="ontabchange")  dataname = displayed.grids[h$pageno]

    if(!length(dataname)) return()

    .$currentFactor[,] <- .$newVarName[,] <- .$getVarList(dataname)[,1]
    
    # Just to trigger printCat()
    if(length(.$currentFactor[,])>1) {
      svalue(.$currentFactor,index=TRUE) = 2
      svalue(.$newVarName,index=TRUE) = 2
    }
  },
  ### Print factor categories in the recode/transform field
  printCat = function(.,h,...) {
  
    # Get the opened data tables
    displayed.grids = names(.$gridNotebook)

    # Empty transformation fields if none
    if(length(displayed.grids) == 0) { 
     .$currentFactor[,]   = ""
	    svalue(.$toCompute) = ""
	   .$newVarName[,]      = ""
	    return()
    }

    # Get the current active data tab
    dataname = displayed.grids[svalue(.$gridNotebook)]
    if(!length(dataname)) return()

    # Current source variable
    sourceVar = svalue(.$currentFactor)
    if(is.null(sourceVar)) return()
    
    # Display category names in transform field
    if(is.factor(eval(parse(text=paste(dataname,sourceVar,sep="$")),envir=.GlobalEnv))) {
      factorLevels = .$getFacLevelList(dataname,sourceVar)
      svalue(.$toCompute) = paste(factorLevels,collapse=",")
    }
    
    else svalue(.$toCompute) = ""
  },
  ### Save data grid under a new name
  saveGridAs = function(.,h,...) {

    filename = .$fileChoose("save")

    if(is.na(filename)) return()
    if(filename=="")    return()
    
    # Extract file extension
    name.ext = tolower(.$fileExtension(filename))
    if(name.ext != "csv") {
      gmessage("L\'enregistrement ne peut se faire \nqu\'au format CSV")
      return()
    }
    
    tabname =  .$baseName(filename)

    res = try(write.csv2(tabname,file=filename,row.names=FALSE))
    if(inherits(res,"try-error")) {
      gmessage("Erreur lors de l'écriture du fichier.")
      return()
    }
    
    # The working directory is implicitly set
    setwd(dirname(filename))
  },
  ### Save grid under its current name
  saveGrid = function(.,h,...) {

    displayed.grids = names(.$gridNotebook)
    dataname = displayed.grids[svalue(.$gridNotebook)]
    filename = paste(dataname,".csv",sep="")
    
    res = try(write.csv2(eval(parse(text=dataname),envir=.GlobalEnv),file=filename,row.names=FALSE))
    if(inherits(res,"try-error")) {
      gmessage("Erreur lors de l'écriture du fichier.")
      return()
    }

    .$setStatus(paste("Statut : fichier '",filename,"' correctement sauvegardé."))
  },
  ### Update data view in the datagrid tab
  showData = function(.,dataname) {

    if(!length(dataname) || (dataname == "Aucun tableau")) return()
    displayed.grids = names(.$gridNotebook)

    # Position of the new tab
    pos = length(displayed.grids) + 1
    
    # Data table already opened: Close it before updating
    if(dataname %in% displayed.grids) {
      pos = which(displayed.grids==dataname)
      svalue(.$gridNotebook) = pos
      dispose(.$gridNotebook)
    }
    
   .$setStatus("Statut : affichage des données. Veuillez patienter....")
    
    # Add a new page to gridNotebook
    add(.$gridNotebook,tmp <- ggroup(),label = dataname,index=pos,expand=TRUE)
    addHandlerUnrealize(tmp,.$closeData)
    
    # A workaround, suggested by Tom Taverner, as DfEdit does not deal with ordered factors (to be corrected soon)
    eval(parse(text=paste("for(var in which(sapply(",dataname,",is.ordered))) class(",dataname,"[,var])='factor'",sep="")),envir=.GlobalEnv)
    
    # Open grid
    add(tmp,  gdfedit(eval(parse(text=dataname),envir=.GlobalEnv),name=dataname), expand=TRUE)

    # Make it the active data
   .$currentDataName = dataname
    
    # Set to 2nd element of currentFactor just to trigger a change and call printCat
    svalue(.$currentFactor,index=TRUE) = 2
    svalue(.$newVarName,index=TRUE) = 2

    .$setStatus("Statut : Prêt.")

  },
  ### Called when a data grid is closed
  closeData = function(.,h,...) {

    displayed.grids = names(.$gridNotebook)
    if(is.null(displayed.grids)) return()

    dataname = displayed.grids[svalue(.$gridNotebook)]
    .$currentDataName = dataname
    
    svalue(.$openGrid) = dataname
    svalue(.$currentData) = dataname
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS MODEL DEFINITION METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Dataset is changed from somewhere: Reset all dataframes droplists
  changeDataset = function(.,h,...) {
  
    # Authorized actions only
    if(is.null(h$action)) return()
    
    # From data grid selector
    if(h$action == "openswitchgrid")  {
    
      newName = svalue(.$openGrid)

      # Warning: A droplist may be temporarily set to NULL when changed
      if(is.null(newName)) return()
    
      # Change is already done (avoid infinite loops)
      if(newName == .$currentDataName) return()
      
     .$currentDataName = newName
      svalue(.$currentData) = newName
    }
    
    # From data frame selector in the model tab
    else if(h$action == "changemodeldata") {
      newName = svalue(.$currentData)

      # Warning: A droplist may be temporarily set to NULL when changed
      if(is.null(newName)) return()
    
      # Change is already done (avoid infinite loops)
      if(newName == .$currentDataName) return()
      
     .$currentDataName = newName
     
      # This will trigger updateGrid too
      svalue(.$openGrid) = newName
    }
    
    else if(h$action == "ontabchange") {
    
      newName = names(.$gridNotebook)[h$pageno]

      # Change is already done (avoid infinite loops)
      if(newName == .$currentDataName) return()
      
     .$currentDataName = newName
     
      # This will trigger the model panel reset too
      svalue(.$currentData) = newName
    }
    
  },
  ### Get the variable names and types list in a given data frame
  getVarList = function(.,dataname,type=TRUE) {

    if(!length(dataname) || (dataname == "") || (dataname == "Aucun tableau")) {
      return(data.frame(Variables="Aucune variable",Type="-"))
    }
    
    dataFrame = eval(parse(text=dataname),envir=.GlobalEnv)
    vList = colnames(dataFrame)
    varType = with(dataFrame,sapply(vList,function(x) is.numeric(eval(parse(text=x)))))

    if(type) return(cbind(Variables=vList,Type=ifelse(varType,"N","F")))
    else     return(cbind(Variables=vList))
  },
  ### Get the vector of numeric variable names in the current dataframe
  getNumVarList = function(.,dataname) {

    if(!length(dataname)) return()
    if(dataname == "")    return()
    if(dataname == "Aucun tableau") return(NULL)

    dataFrame = eval(parse(text=dataname),envir=.GlobalEnv)
    vList = colnames(dataFrame)
    varType = with(dataFrame,sapply(vList,function(x) is.numeric(eval(parse(text=x)))))
    return(vList[varType])
  },
  ### Get the vector of categorical variable names in the current dataframe
  getCatVarList = function(.,dataname) {

    if(!length(dataname)) return()
    if(dataname == "")    return()
    if(dataname == "Aucun tableau") return(NULL)

    dataFrame = eval(parse(text=dataname),envir=.GlobalEnv)
    vList = colnames(dataFrame)
    varType = with(dataFrame,sapply(vList,function(x) is.numeric(eval(parse(text=x)))))
    return(vList[!varType])
  },
  ### Update the variable list
  updateVarList = function(.,h,...) {

    if(.$currentDataName == "Aucun tableau") return()

    # Update main variable list
    variables = .$getVarList(.$currentDataName)
    if(!length(variables)) return()
    .$varList[,] = variables
  },
  ### Update the weighting variable list
  updateWeightList = function(.,h,...) {

    if(.$currentDataName == "Aucun tableau") {
      svalue(.$weightList) = "Aucune"
      return()
    }
  
    variables = .$getNumVarList(.$currentDataName)
    weightVar = svalue(.$weightList)
    .$weightList[] = c("Aucune",variables)
    if(!weightVar %in% .$weightList[]) svalue(.$weightList) = "Aucune"
    else svalue(.$weightList) = weightVar  
  },
  ### Update the constraint factor list
  updateConstrFactor = function(.,h,...) {

    if(.$currentDataName == "Aucun tableau") {
      svalue(.$structList) = "Aucun"
      return()
    }
  
    variables = .$getCatVarList(.$currentDataName)
    structVar = svalue(.$structList)
    .$structList[] = c("Aucun","Constant",variables)
    if(!structVar %in% .$structList[]) svalue(.$structList) = "Aucun"
    else svalue(.$structList) = structVar  
  },
  ### Update the variable summary each time a variable name is clicked
  updateVarSummary = function(.,h,...) {

    currentVar = svalue(.$varList,drop=FALSE)
    varType    = currentVar[1,2]
    currentVar = currentVar[1,1]

    # No variable name selected
    if(!length(currentVar) || is.na(currentVar)) {
     .$varSummary[,] = data.frame(Attribut="Aucun",Valeur="Aucune",stringsAsFactors=FALSE)
      return()
    }
    
    varContent = eval(parse(text=paste(.$getDataName(),"[,'",currentVar,"']",sep="")),envir=.GlobalEnv)

    # A factor is selected
    if(varType == 'F') {
      vs = table(varContent)
      cn = c(names(vs),"Manquantes","Total")
      vs = c(as.vector(vs),sum(is.na(varContent)),length(varContent))
     .$varSummary[,] = cbind(Attribut=cn,Valeur=vs)
    }
    
    # A numeric variable
    else if(varType == 'N') {
      Q = quantile(varContent,probs=c(0,.25,.5,.75,1),na.rm=TRUE)
     .$varSummary[,] = cbind(Attribut=c("Min.","1er Quart.","Médiane","Moyenne","3ème Quart.","Max.","Ecart-type","Obs.","Manquantes"),
                             Valeur=round(c(Q[1],Q[2],Q[3],mean(varContent,na.rm=TRUE),Q[4],Q[5],sd(varContent,na.rm=TRUE),length(varContent),sum(is.na(varContent))),4))
    }
  },

  ### Update link function list upon distribution selection
  updateLink = function(.,h,...) {
   .$currentLinkList[] = linkLists[[svalue(.$distribList)]]
    svalue(.$currentLinkList) = .$currentLinkList[1]
  },
  ### Get the defined model name
  getModelName = function(.) {
    .$trim(svalue(.$modelName))
  },
  ### Get all model names
  getModelNames = function(.) {
  
    if(!length(.$models)) return("")
    sapply(.$models, function(m) m$getName())
  },
  ### Clear current model name field
  clearModelName = function(.,h,...) {
    svalue(.$modelName) = ""
  },
  ### Refill the model fields with a given model specs
  retrieveModel = function(.,h,...) {
  
    modname = .$getModelName()
    svalue(.$dvList)  = .$models[[modname]]$dvField
    svalue(.$fivList) = .$models[[modname]]$ivField
    svalue(distribList,index=TRUE) = .$models[[modname]]$family
    svalue(currentLinkList,index=TRUE) = .$models[[modname]]$link
    svalue(weightList) = ifelse(.$models[[modname]]$weights=="NULL","Aucune",.$models[[modname]]$weights)
    svalue(subsetVar) = ifelse(.$models[[modname]]$subset=="NULL", "",.$models[[modname]]$subset)
    svalue(structList) = .$models[[modname]]$constrFactor
  },
  ### Update the list of model names when a new one is fitted
  updateModelNameList = function(.,h,...) {
    .$modelName[,] = .$getModelNames()
  },
  ### Add dependent variable
  updateDVField = function(.,h,...) {
    dv = .$trim(svalue(.$dvList))
    model = paste(svalue(.$varList),collapse=",")
    if(nchar(dv)) svalue(.$dvList) = paste(dv,", ",model,sep="")
    else          svalue(.$dvList) = model
  },
  ### Clear dependent variable field
  clearDVField = function(.,h,...) {
    svalue(.$dvList) = ""
  },
  ### Edit model field
  updateFIVField = function(.,h,...) {

    op = h$action
    fiv = .$trim(svalue(.$fivList))
    vl = svalue(.$varList)
    
    if(op == "-1") {
      svalue(.$fivList) = paste(fiv,"+0",sep="")
      return()
    }
    
    else if(op == "1") {
      if(nchar(fiv)) svalue(.$fivList) = paste(fiv,"+1",sep="")
      else           svalue(.$fivList) = "1"
      return()
    }
    
    else if(op == "()") {
      pat = svalue(.$fivList,drop=TRUE)
      repl = paste("(",pat,")",sep="")

      if(nchar(pat)) svalue(.$fivList) = sub(pat,repl,fiv,fixed=TRUE)
      else           svalue(.$fivList) = paste(fiv,"()",sep="")
      return()
    }
    
    else if(op == "offset") {

      # Is there selected text ?
      pat = svalue(.$fivList,drop=TRUE)
      if(nchar(pat)) {
        repl = paste("offset(",pat,")",sep="")
        svalue(.$fivList) = sub(pat,repl,fiv,fixed=TRUE)
        return()
      }

      # Any variable selected in the list ?
      pat = vl[1]
      if(length(pat)) {
        if(nchar(fiv)) svalue(.$fivList) = paste(fiv,"+offset(",pat,")",sep="")
        else           svalue(.$fivList) = paste(fiv,"offset(",pat,")",sep="")
        return()
      }

      svalue(.$fivList) = paste(fiv,"+offset()",sep="")
      return()
    }
    
    else if(op == "|") {
      svalue(.$fivList) = paste(fiv,"|",sep="")
      return()
    }
    
    else if(op == "(1|)") {
	    if(length(vl))  svalue(.$fivList) = paste(fiv,"+(1|",vl[1],")",sep="")
	    else            svalue(.$fivList) = paste(fiv,"+(1|.)",sep="")
      return()
    }
    
    else if(op == "(.|)") {
	    if(length(vl))  svalue(.$fivList) = paste(fiv,"+(",vl[1],"|.)",sep="")
	    else            svalue(.$fivList) = paste(fiv,"+(.|.)",sep="")
      return()
    }
    
    else if(op == "add") {
    
      # Only relevant when a variable is selected
      if(!nchar(vl[1])) return()
      
      # Is there selected text ?
      pat = svalue(.$fivList,drop=TRUE)
      if(nchar(pat)) {
      
        # Only replace the first occurrence of '.'
        if(.$trim(pat) == '.') svalue(.$fivList) = sub(pat,vl[1],fiv,fixed=TRUE)

        # Replace all occurrences of a variable name
        else                   svalue(.$fivList) = gsub(pat,vl[1],fiv,fixed=TRUE)
        
        return()
      }
      else {
	      svalue(.$fivList) = paste(fiv,vl[1],sep="")
        return()
      }
    }
    
    else if(op == "/") {
	    paste(fiv,"/",sep="")
      return()
    }
    
    else {

      # no variable selected: just print the operator
      if(length(vl)==0) { svalue(.$fivList) = paste(fiv,op,sep="")    ; return() }

      # one variable selected: append it with the operator
      if(length(vl)==1) { 
        if(nchar(fiv)) svalue(.$fivList) = paste(fiv,op,vl,sep="")
        else           svalue(.$fivList) = vl
        return() 
      }

      # At least two variables selected
      model = paste(vl,collapse=op)
      if(nchar(fiv)) svalue(.$fivList) = paste(fiv,"+",model,sep="")
      else           svalue(.$fivList) = model
    }
  },
  ### Extract random variable name from a (1|.) term
  extractRandomVar = function(.,h,...) {
    fterms = unlist(strsplit(.$removeSpaces(.$ivField)),"+",fixed=TRUE)
    which.rand = grep("|",fterms,fixed=TRUE)
    if(length(which.rand)>1) {
      gmessage("Un seul facteur aléatoire n'est possible.")
      return("error")
    }
    
    # grep("^\\(1\\|.*)$",fterms,value=T) # Look exactly for the pattern (1|.)
    randvar = .$removeParentheses(fterms[which.rand])
    randvar = unlist(strsplit(randvar,"|",fixed=T))
    if(randvar[1] != "1") {
      gmessage("Seuls les modèles à incercept aléatoire sont possibles.")
      return("error")
    }
    
    randvar[2]
  },
  ### Clear model field
  clearFIVField = function(.,h,...) {
    svalue(.$fivList) = ""
  },
  ### Clear the case subsetting field
  clearSelectField = function(.,h,...) {
    svalue(.$subsetVar) = ""
  },
  ### Clear all definition fields
  clearModelFields = function(.,h,...) {
    .$clearModelName(h,...)
    .$clearDVField(h,...)
    .$clearFIVField(h,...)
    .$clearSelectField(h,...)
  },
  ### Get the active R2STATS model
  getCurrentModel = function(.,h,...) {
    if(.$currentModelName == "Aucun") return("Aucun")
    .$models[[.$currentModelName]]
  },
  ### Get a named R2STATS model
  getModelByName = function(.,modelname) {
    .$models[[modelname]]
  },
  ### Get variables
  getDVField = function(.) {
    .$trim(svalue(.$dvList))
  },
  getDV = function(.) {
    unlist(strsplit(.$strip.cbind(.$trim(.$getDVField())),","))
  },
  getIVField = function(.) {
    ivf = .$trim(svalue(.$fivList))
    if(ivf=="") ivf = "1"
    ivf
  },
  getIV = function(.) {
    all.vars(as.formula(paste("~",.$getIVField(),sep="")))
  },
  getModelVars = function(.) {
    all.vars(as.formula(.$getFormulaAsString()))
  },
  ### Get model formula
  getFormulaAsString = function(.) {
    dvs = .$getDV()
    dv  = .$getDVField()
    iv  = .$getIVField()
    
    if(length(dvs) == 0)    return("")
    else if(length(dvs)==1) return(paste(dv,"~",iv))
    else                    return(paste("cbind(",dv,")~",iv))
  },
  ### Get link index as specified in the interface
  getLink = function(.) {
    svalue(.$currentLinkList,index=TRUE)
  },
  ### Get distribution family index as specified in interface
  getFamily = function(.) {
    svalue(.$distribList,index=TRUE)
  },
  ### Get subset command as text
  getSubset = function(.) {
    # subset=NULL is the default in glm()
    subset = "NULL"
    if(nchar(svalue(.$subsetVar))) subset = svalue(.$subsetVar)
    subset
  },
  ### Get the name of weighting variable
  getWeights = function(.) {
    weights = svalue(.$weightList)
    if(weights == "Aucune") return("NULL")
    weights
  },
  ### Get constraint factor
  getConstrFactor = function(.) {
    svalue(.$structList)
  },
  ### Model identification and estimation
  run = function(.,h,...) {

    modname = .$getModelName()
    tableau = .$getDataName()

    ##------------------------------- Some checks about model specs
    
    # No model name provided
    if(modname %in% c("","Nouveau")) {
      gmessage("Vous devez spécifier un nom de modèle.")
      return()
    }

    # Don't give the model the dataframe name
    if(modname == tableau) {
      gmessage("Vous ne pouvez donner au modèle\nle nom du tableau de données.")
      return()
    }
	
    # This model name already exists
    if(modname %in% .$getModelNames()) {
      if(!gconfirm("Un modèle du même nom existe déjà.\nVoulez-vous l\'écraser ?")) {
        return()
      }
    }
    
    # Check dependent variable field
    vd = .$getDVField()
    if(!nchar(vd)) {
      gmessage("Vous devez spécifier au moins une VD.")
      return()
    }

    # Check independent variable field
    vif = .$getIVField()
    if(!nchar(vif)) {
      vif = "1"
    }
    
    # Do variables exist?
    listvar = .$getVarList(tableau,type=FALSE)   
    if(!all(.$getModelVars() %in% listvar)) {
      gmessage("Erreur : l\'une des variables invoquées n\'existe pas.")
      return()
    }
    
    # Distribution misspecification
    vds   = .$getDV()
    ndv   =  length(vds)
    distr = .$getFamily()

    current.data = eval(parse(text=tableau),envir=.GlobalEnv)
    if( (length(ndv)==1) && is.factor(current.data[,vds]) && ( !(distr %in% c(2,6,7))) ) {
      gmessage("Cette distribution ne convient pas\npour une variable catégorisée.")
      return()
    }

    # Model definition
    vifs    = .$getIV()
    link    = .$getLink()
    
    subset  = .$getSubset()
    if(.$trim(subset) == "") subset = "NULL"
    
    weights = .$getWeights()
    if(weights == "Aucune") weights = "NULL"
    
    constrFactor = .$getConstrFactor()

    # Random effect model
    if(length(grep("|",vif,fixed=T))) {

      # Multinomial mixed model
      if(distr %in% 6:7) {
        randvar = .$extractRandomVar()
        if(randvar == "error") return()
        
        model = r2sCLMM$new(name=modname,class="clmm",func="clmm",dvField=vd,dv=vds,ivField=vif,iv=vifs,random=randvar,
                            data=tableau,family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)      
      }
      
      else {
        model = r2sGLMM$new(name=modname,class="mer",func="glmer",dvField=vd,dv=vds,ivField=vif,iv=vifs,data=tableau,
                             family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)
      }
	  }
     	   
    # Fixed effects model
    else {

      # Multivariate model
      if(ndv>1) {

        # Multivariate gaussian
        if(distr==1) {
          model = r2sMANOVA$new(name=modname,class="mlm",func="manova",dvField=vd,dv=vds,ivField=vif,iv=vifs,data=tableau,
                               family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)
	      }
	      
	      # Binomial with success/failure counts in two columns
	      else if(distr==2) {
	        if(ndv>2) {
	          gmessage("Pour un modèle binomial, les comptages doivent\nêtre présentés en deux colonnes.")
	          return()
	        }
          model = r2sGLM$new(name=modname,class="glm",func="glm",dvField=vd,dv=vds,ivField=vif,iv=vifs,data=tableau,
                             family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)    
	      }
      }
      
      # Multinomial with multicolumn counts
      else if(distr %in% 6:7) {
        model = r2sCLM$new(name=modname,class="clm",func="clm",dvField=vd,dv=vds,ivField=vif,iv=vifs,data=tableau,
                            family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)
      }
        
      # Standard univariate GLM
      else {
        model = r2sGLM$new(name=modname,class="glm",func="glm",dvField=vd,dv=vds,ivField=vif,iv=vifs,data=tableau,
                           family=distr,link=link,weights=weights,constrFactor=constrFactor,subset=subset)
      }
    }
    
    # Model estimation
    res = model$estimate()
    if(inherits(res,"try-error")) {
      r2stats$setStatus("Statut : Prêt.")
      return()
    }
    
    # Save model in R2STATS model list
   .$models[[model$name]] = model
   .$updateModelNameList(h)
   
    # Make it the active model
   .$currentModelName = modname
    
    # Print results
    model$Summary()

    # Add to model lists (in graph and compare tabs)
   .$updateModelList(h)
       
    # Update list of available plots (if necessary)
    r2stats$setStatus("Statut : construction des graphiques....")
    possiblePlots = model$getAvailablePlots()
    lastPlotType = svalue(.$plotType)
    if(any(.$plotType[,] != possiblePlots)) {
     .$plotType[,] = possiblePlots
      if(lastPlotType %in% possiblePlots) svalue(.$plotType) = lastPlotType
      else                                svalue(.$plotType,index=TRUE) = 1
    }

    # Highlight in plot list (this change automatically triggers the plotCurrentModel() handler)
    whichToPlot = which(.$graphModelList[,1] == modname)
    svalue(.$graphModelList,index=TRUE) = whichToPlot

    # Switch to result tab
    r2stats$setStatus("Statut : Prêt.")
    svalue(.$mainNotebook) = 4
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS RESULT PRINTING METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  clearResults = function(.,h,...) {
    svalue(.$results)=""
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS MODEL PLOTTING METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Generates group colors (borrowed from fBasics and RColorBrewer)
  getColors = function(.,n,name=c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3"),alpha=255) {

    Accent = rgb(c(127, 190, 253, 255, 56, 240, 191, 102),
                 c(201, 174, 192, 255, 108, 2, 91, 102), 
                 c(127, 212, 134, 153, 176, 127, 23, 102),maxColorValue = 255)
    Dark2 = rgb(c(27, 217, 117, 231, 102, 230, 166, 102), 
                c(158, 95, 112, 41, 166, 171, 118, 102),
                c(119, 2, 179, 138, 30, 2, 29, 102), maxColorValue = 255)
    Paired = rgb(c(166, 31, 178, 51, 251, 227, 253, 255, 202, 106, 255, 177),
                 c(206, 120, 223, 160, 154, 26, 191, 127, 178, 61, 255, 89),
                 c(227, 180, 138, 44, 153, 28, 111,   0, 214, 154, 153, 40), maxColorValue = 255)
    Pastel1 = rgb(c(179, 251, 204, 222, 254, 255, 229, 253, 242), 
                  c(205, 180, 235, 203, 217, 255, 216, 218, 242), 
                  c(227, 174, 197, 228, 166, 204, 189, 236, 242), maxColorValue = 255)
    Pastel2 = rgb(c(179, 253, 203, 244, 230, 255, 241, 204), 
                  c(226, 205, 213, 202, 245, 242, 226, 204),
                  c(205, 172, 232, 228, 201, 174, 204, 204), maxColorValue = 255)
    Set1 =    rgb(c(55, 228, 77, 152, 255, 255, 166, 247, 153), 
                  c(126, 26, 175, 78, 127, 255, 86, 129, 153),
                  c(184, 28, 74, 163, 0, 51, 40, 191, 153), maxColorValue = 255)
    Set2 =    rgb(c(102, 252, 141, 231, 166, 255, 229, 179),
                  c(194, 141, 160, 138, 216, 217, 196, 179),
                  c(165, 98, 203, 195, 84, 47, 148, 179), maxColorValue = 255)
    Set3 =    rgb(c(141, 255, 190, 251, 128, 253, 179, 252, 217, 188, 204, 255),
                  c(211, 255, 186, 128, 177, 180, 222, 205, 217, 128, 235, 237),
                  c(199, 179, 218, 114, 211, 98, 105, 229, 217, 189, 197, 111), maxColorValue = 255)
    name = match.arg(name)
    orig = eval(parse(text = name))
    
    if(n<10) return(orig[1:n])

    rgb = t(col2rgb(orig))
    temp = matrix(NA, ncol = 3, nrow = n)
    x = seq(0,1, ,length(orig))
    xg = seq(0,1,,n)
    for (k in 1:3) {
        hold = spline(x, rgb[, k], n = n)$y
        hold[hold < 0] = 0
        hold[hold > 255] = 255
        temp[,k] = round(hold)
    }
    
    rgb(temp[,1],temp[,2],temp[,3],alpha=alpha,maxColorValue = 255)
  },
  ### Set graphical parameters for a given model, depending upon the number of groups or classes
  setPlotParams = function(.,n=9) {

    fullColors   = .$getColors(n,"Set1")
    pastelColors = .$getColors(n,"Pastel1")

    trellis.par.set(    
      plot.symbol       = list(col = fullColors,fill = pastelColors,cex = rep(.8,n),font = rep(1,n),pch = rep(21,n),alpha = rep(1,n)),
      superpose.symbol  = list(col = fullColors,fill = pastelColors,cex = rep(.8,n),font = rep(1,n),pch = rep(21,n),alpha = rep(1,n)),
      plot.line         = list(alpha = rep(1,n),col = fullColors,lty = rep(1,n),lwd = rep(1,n)),
      superpose.line    = list(alpha = rep(1,n),col = fullColors,lty = rep(1,n),lwd = rep(1,n)),
      plot.polygon      = list(border = fullColors,col = fullColors),
      superpose.polygon = list(alpha = rep(1,n),col = fullColors,border="black",lty = rep(1,n),lwd = rep(1,n)),
      add.line          = list(alpha=rep(1,n),col=fullColors,lty=rep(1,n),lwd=rep(2,n)),
      add.text          = list(alpha=rep(1,n),cex=rep(1,n),col=fullColors,font=rep(1,n),lineheight=rep(1.2,n))
    )
  },
  ### Plot current model
  plotCurrentModel = function(.,h,...) {

    # No plot if no model exists
    if(!length(.$models)) return()

    # Current model is defined by the state of graphModelList
    selectedModel = svalue(.$graphModelList)
    
    # No plot if no model selected
    if(!length(selectedModel)) return()

    # Make it the active model
   .$currentModelName = svalue(.$graphModelList)
   
    # Set colors
    currentModel = .$models[[.$currentModelName]]
    if(length(currentModel$designFactors)) .$setPlotParams(nlevels(currentModel$groupLabels))
    
    # The model constructs its plot...
    currentModel$Plot(h)
    
    # ... but R2STATS print it on the device
    print(.$currentPlot)
    
    # Reset observation locator
    svalue(.$obsId) = ""
  },
  ### A blank plot when none is sensible
  emptyPlot = function(.) {
    xyplot(1:5~1:5,type="n",bty="n",scales=list(draw=F),xlab="",ylab="",
           panel = function(x,y,...) {
             panel.text(3,3,"Pas de graphique disponible.")
           })
    # Ajouter texte : "Pas de graphique disponible.")
  },
  ### Get desired plot type
  getPlotType = function(.) {
    svalue(.$plotType)
  },
  getPlotTypeAsIndex = function(.) {
    svalue(.$plotType,index=TRUE)
  },
  ### Get the selected group for plotting
  getSelectedGroup = function(.) {
    svalue(.$groupList)
  },
  ### Get the desired legend placement
  getLegendLocation = function(.) {
    c("none","right","left","top","bottom")[svalue(.$legendLoc,index=T)]  
  },
  ### Get the desired legend number of columns
  getLegendCols = function(.) {
    c(1:10)[svalue(.$legendCols,index=T)]  
  },
  ### Get limits on the x-axis
  getXLim = function(.) {
    
    xlim = NULL
    graph.xlim = .$trim(svalue(.$graphLimitsX))

    if(graph.xlim != "") {
      if(length(grep(",",graph.xlim))) {
        gmessage("Veuillez utiliser le point comme séparateur décimal\n et l\'espace comme séparateur de valeurs.")
      }
      else {
        xlim = as.numeric(unlist(strsplit(graph.xlim,split=" ")))
        xlim = xlim[!is.na(xlim)]
      }
    }
    xlim
  },
  ### Get limits on the x-axis
  getYLim = function(.) {
    
    ylim = NULL
    graph.ylim = .$trim(svalue(.$graphLimitsY))

    if(graph.ylim != "") {
      if(length(grep(",",graph.ylim))) {
        gmessage("Veuillez utiliser le point comme séparateur décimal\n et l\'espace comme séparateur de valeurs.")
      }
      else {
        ylim = as.numeric(unlist(strsplit(graph.ylim,split=" ")))
        ylim = ylim[!is.na(ylim)]
      }
    }
    ylim
  },
  ### Get the flag value for adding the Y=X line
  getAddLine01 = function(.) {
    svalue(.$addLine01)
  },
  ### Display observation id. on a click
  getXY = function(.,h,...) {
  
    # Only relevant for XY scatterplots
    if(is.null(.$currentPlot$panel.args[[1]]$y)) return()

    # Which obs. is closest to mouse pointer
    currentPlot.XY = data.frame(x=.$currentPlot$panel.args[[1]]$x,y=.$currentPlot$panel.args[[1]]$y)
    
    # Argh.. this returns (Inf,Inf) with lattice plots
    target = c(h$x,h$y)

    D2 = (currentPlot.XY$x - h$x)**2 + (currentPlot.XY$y - h$y)**2
    closest = which(D2 == min(D2))[1]

    # Display the observation number/name that is closest to the mouse pointer
    # svalue(.$obsId) = paste("Obs.",names(.$currentPlot$panel.args[[1]]$y)[closest])
  },
  ### Save plot in a PNG file
  savePlot = function(.,h,...) {
  
    fn = .$fileChoose("save")
    if(is.na(fn) || (fn == "")) return()
    
    fn = unlist(strsplit(fn,"\\."))[1]
    fn = paste(fn,".png",sep="")
    png(filename=fn)
   .$plotCurrentModel(h,...)
    dev.off()
  },
  ### Copy plot in clipboard
  copyPlot = function(.,h,...) {
    if(Sys.info()["sysname"] != "Windows") {
      gmessage("Fonction disponible sous Windows uniquement.")
    	return()
    }
    
    win.metafile()
   .$plotCurrentModel(h,...)
    dev.off()
    gmessage("Le graphique est copié en mémoire-tampon.\nVous pouvez le coller dans un autre programme.")
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                           R2STATS MODEL COMPARISON METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Return the full list of all R2STATS models and their main attributes
  getModelList = function(.) {

    if(!length(.$models)) return(data.frame(Nom="Aucun modèle",Formule="Aucune formule",Contrainte="Aucun",Distribution="Non spécifiée",Lien="Non spécifié",stringsAsFactors=FALSE))

    mnames = sapply(.$models, function(model) model$getName())
    formul = sapply(.$models, function(model) model$getFormula())
    constr = sapply(.$models, function(model) model$getConstrFactor())
    distr  = sapply(.$models, function(model) model$getFamilyAsIndex())
    distr  = names(.$linkLists)[distr]
    link   = sapply(.$models, function(model) .$linkLists[[model$getFamilyAsIndex()]][model$getLinkAsIndex()])

    cbind(Nom=mnames,Formule=formul,Contrainte=constr,Distribution=distr,Lien=link)
  },
  ### Update model list in tab 5
  updateModelList = function(.,h,...) {

    ml = .$getModelList()
   .$modelList[,]      = ml
   .$graphModelList[,] = ml
  },
  ### Delete models in model list
  deleteModels = function(.,h,...) {
   
    # Vector of selected model names
    modelsToDelete = svalue(.$modelList)
    if(!length(modelsToDelete) || (modelsToDelete == "Aucun modèle")) {
      .$currentModelName = "Aucun"
      return()
    }
    
    # Remove from model list in the Compare tab
   .$models[modelsToDelete] = NULL
   
    # Remove from model list in the Model tab
    .$updateModelNameList(h,...)
    
    # Refresh model list in tab 5
   .$updateModelList(h,...)
  },
  ### Select all entries in R2STATS model list (tab 5)
  selectAllModels = function(.,h,...) {
    if(.$modelList[1,1]=="Aucun modèle") return()
    nmodels = nrow(as.data.frame(.$modelList[,]))
    svalue(.$modelList,index=TRUE) = 1:nmodels
  },
  ### Compare selected models
  compareModels = function(.,h,...) {

    # Names and indicies of selected models
    comp = svalue(.$modelList)
    idx  = svalue(.$modelList,index=TRUE)

    # Reorder by complexity
    Dfs = sapply(.$models[comp], function(m) m$df())
    k = order(Dfs)
    if(length(k)>1) {
      comp = comp[k]
      idx = idx[k]
    }

    # Model characteristics (as vectors or matrices)
    classes  = unique(lapply(.$models[comp], function(m) m$getClass()))
    dvs      = lapply(.$models[comp], function(m) m$getDV())
    distribs = sapply(.$models[comp], function(m) m$getFamily())
    sizes    = lapply(.$models[comp], function(m) length(m$getY()))
    
    # Do model have the same dependent variable?
    same.depvar = length(unique(dvs)) == 1
    if(!same.depvar) {
      gmessage("Les modèles ne portent pas sur la même variable dépendante.\nLa comparaison n\'a pas de sens.")
      return()
    }

    # Do models bear upon the same number of observations?
    same.size = length(unique(sizes)) == 1
    if(!same.size) {
      gmessage("Les modèles ne portent pas sur le même\n sous-ensemble d'observations.")
      return()
    }
    
    # If all models are from the same family, deviance is analyzed (AIC-BIC otherwise)
    same.distrib = length(unique(distribs)) == 1

    # Model statistics
    AICs     = sapply(.$models[comp], function(m) m$Aic())
    BICs     = sapply(.$models[comp], function(m) m$Bic())
    Expl     = sapply(.$models[comp], function(m) m$devExplained())
    logLiks  = sapply(.$models[comp], function(m) m$LogLik())
    Dfs      = sapply(.$models[comp], function(m) m$df())

    # Print titles
    add(.$results,"Comparaison et résumé de modèles",font.attr=c(style="normal",weights="bold",size="large",col="blue"))
    add(.$results,"")
    add(.$results,"Table d\'analyse de la variance/déviance",font.attr=c(style="normal",weights="bold",col="black"))
    add(.$results,"")
    
    if( (length(classes)==1) && (classes == "glm")) {
    
      # If all models are gaussian or Gamma, then F tests are used (chi-square otherwise)
      all.gaussian = all(distribs == "gaussian")
      all.gamma    = all(distribs == "Gamma")

      # Several models selected: Compare them
     	if(length(comp)>1) {
     	
        if(same.distrib)  {

          # Use 'F' in case of gaussian or Gamma models, chi-square otherwise
          cmd = paste(".$tabdev = anova(",paste(".$models$",comp,"$Rmodel",collapse=",",sep=""),ifelse(all.gaussian || all.gamma, ",test='F')", ",test='Chisq')"))

          # Execute command
          eval(parse(text=cmd))
          
          # Adapt column headers
          if(all.gaussian)
            attr(.$tabdev,"names")=c("Ddl rés.","Dév. res.","Ddl diff.","Rap.Vr.","F","Pr(>F)")
          else 
            attr(.$tabdev,"names")=c("Ddl rés.","Dév. res.","Ddl diff." ,"Rap.Vr.","Pr(>Chi2)")

          # Model names appear as row names
          attr(.$tabdev,"row.names") = comp
          
          # Remind me of model formulae
          attr(.$tabdev,"heading") = c(capture.output(.$modelList[idx,2:5]),"")

          # Add AIC and BIC when several models are compared (anova also works for a single one)
          if(length(comp)>1)  {
            # TODO: cet ajout de colonnes modifie l'affichage des décimales (!?)
            .$tabdev[["AIC"]]      = AICs
            .$tabdev[["BIC"]]      = BICs
            .$tabdev[["Expl.(%)"]] = Expl
          }
        }

        # Just AIC and BIC in case of different distributions or non-nested models
        else  {
        
          .$tabdev = data.frame(Distribution=.$modelList[idx,4],Lien=.$modelList[idx,5],AIC=AICs,BIC=BICs)

          # Model names appear as row names
          attr(.$tabdev,"row.names") = comp
          
          # Remind me of model formulae
          attr(.$tabdev,"heading") = c(paste(.$modelList[idx,1],.$modelList[idx,2],sep=" : ",collapse="\n"),"")
        }
      }
      
      # Only one model selected: Print summary
     	if(length(k)==1) {
     	
     	  # Gaussian models: Standard anova table
     	  if(distribs == "gaussian") {
       	    cmd = paste(".$tabdev = summary(aov(.$models$",comp,"$Rmodel))[[1]]",sep="")
      	    eval(parse(text=cmd))
            attr(.$tabdev,"names")=c("Ddl.","SC","CM","F","Pr(>F)")
     	  }
     	  
     	  # Non-gaussian models: Compare to the null
     	  else {
       	    cmd = paste(".$tabdev = anova(.$models$",comp,"$Rmodel,test='Chisq')",sep="")
      	    eval(parse(text=cmd))
      	  
      	    # For some reasons, columns are not in the same order with only one model
            .$tabdev = .$tabdev[2:1,c(3,4,1,2,5)]
            attr(.$tabdev,"names")=c("Ddl rés.","Dév. res.","Ddl diff." ,"Rap.Vr.","Pr(>Chi2)")
     	  }

        # Remind me of model formulae
        attr(.$tabdev,"heading") = c(paste(.$modelList[idx,1],.$modelList[idx,2],sep=" : ",collapse="\n"),"")
     	}
	  }
	  
	  # Compare GLMMs
	  else if( (length(classes)==1) && (classes == "mer") ) {
	  
	    # anova() for a single model not implemented
     	if(length(comp)==1) {
     	  gmessage("Il faut sélectionner au moins deux modèles.")
     	  return()
      }
      
      chisq = 2 * pmax(0, c(NA, diff(logLiks)))
      dfChisq = c(NA, diff(Dfs))
      pchi2 = pchisq(chisq, dfChisq, lower = FALSE)
     .$tabdev = data.frame(logV=logLiks,"Ddl."=Dfs,Chi2=chisq,"Ddl diff."=dfChisq,Prob.=pchi2,AIC=AICs,BIC=BICs)

      # Model names appear as row names
      attr(.$tabdev,"row.names") = comp
      
      # Remind me of model formulae
      attr(.$tabdev,"heading") = c(paste(.$modelList[idx,1],.$modelList[idx,2],sep=" : ",collapse="\n"),"")
      
      # Just to get figures nicely printed
      class(.$tabdev) = c("anova",class(.$tabdev))
    }

    # Compare different models from different classes: use AIC and BIC
	  else {
	  
     .$tabdev = data.frame(Distribution=.$modelList[idx,4],Lien=.$modelList[idx,5],AIC=AICs,BIC=BICs,stringsAsFactors=FALSE)

      # Model names appear as row names
      attr(.$tabdev,"row.names") = comp
      
      # Remind me of model formulae
      heading = c(paste(.$modelList[idx,1],.$modelList[idx,2],sep=" : "))

      # print.data.frame() does not print headings
      add(.$results,heading,font.attr=c(family="monospace",size="medium"))
      add(.$results,"")
	  }
	  
    # Output
    add(.$results,capture.output(.$tabdev),font.attr=c(family="monospace",size="medium"))
    add(.$results,"")
    
    # Automatically switch to result page
    svalue(.$mainNotebook) = 4
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS GENERIC METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  setVersion = function(.,version) {
   .$version = paste(version)
   .$mainWindowTitle = paste("R2STATS",Sys.info()["sysname"],version,sep="-")
    svalue(.$mainWindow) = .$mainWindowTitle
  },
  getVersion = function(.) {
    return(.$version)
  },
  setStatus = function(.,text) {
    svalue(.$status)
    svalue(.$status) = text
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS TOOLS
  #
  #------------------------------------------------------------------------------------------------------------------------
  editOptions = function(.,h,...) {
  
  },
  probCalc = function(.,h,...) {
  
  },
  updateR2stats = function(.,h,...) {
  
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS STRING MANAGEMENT METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Remove all spaces from a string
  removeSpaces = function(.,x) {
    gsub("[ ]","",x)
  },
  ### Trim leading and trailing spaces from a string
  trim = function(.,x) {

    # Replace all multiple spaces with a single space
    x <- gsub("[ ]+", " ", x)
    # Remove all trailing spaces
    x <- gsub("[ ]+$", "", x)
    # Remove all leading spaces
    x <- gsub("^[ ]+", "", x)
    x
  },
  ### Get file base name
  getBaseName = function(.,x) {
    sub("(?x) # allow embedded comments
         (.+) # match and remember at least one arbitrary character
         [.] # match a dot
         [^.]+ # match at least one non-dot character
         $", # end of string anchor
         "\\1",basename(x), perl=TRUE)
  },
  ### Get file extension
  getFileExtension = function(.,x) {
    sub(".*\\.", "", x)
  },
  ### Remove cbind() from a string
  strip.cbind = function(.,l) {
    l = sapply(l,function(x) sub("cbind","",x))
    .$removeParentheses(l)
  },
  ### Remove parentheses from an expression
  removeParentheses = function(.,l) {
    l = sapply(l,function(x) sub("\\(","",x))
    sapply(l,function(x) sub("\\)","",x))  
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 R2STATS SLOTS
  #
  #------------------------------------------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                                    CONTENT
  #------------------------------------------------------------------------------------------------------------------------
  #----- General slots
  version                    = "??",
  mainWindow                 = NULL,
  mainWindowTitle            = "R2STATS",
  menu                       = NULL,
  mainNotebook               = NULL,
  status                     = "Statut : Prêt",
  #----- Data slots
  dataUrl                    = NULL,
  hasHeader                  = NULL,
  hasRowNames                = NULL,
  libList                    = NULL,
  libDataList                = NULL,
  loadDataBut                = NULL,
  #----- Grid slots
  currentFactor              = NULL,
  toCompute                  = NULL,
  newVarName                 = NULL,
  gridNotebook               = NULL,
  openGrid                   = NULL,
  #----- Model slots
  models                     = list(),
  panedGroup                 = NULL,
  currentData                = NULL,
  currentDataName            = "Aucun tableau",
  varList                    = NULL,
  varSummary                 = NULL,
  modelName                  = NULL,                  # Name of the model currently defined in the model tab
  currentModelName           = "Aucun",               # Name of the model currently active (from the model or graph tabs)
  dvList                     = NULL,
  fivList                    = NULL,
  distribList                = NULL,
  linkLists                  = NULL,
  currentLinkList            = NULL,
  weightList                 = NULL,
  structList                 = NULL,
  subsetVar                  = "NULL",
  #----- Result slots
  results                    = NULL,
  #----- Graphics slots
  currentPlot                = NULL,                  # Current trellis/lattice plot
  plotType                   = NULL,
  graphModelList             = NULL,
  legendLoc                  = NULL,
  legendCols                 = NULL,
  groupList                  = NULL,
  graphLimitsX               = NULL,
  graphLimitsY               = NULL,
  addData                    = NULL,
  addModel                   = NULL,
  addRefLine                 = NULL,
  addGrid                    = NULL,
  addNoise                   = NULL,
  addRandCurves              = NULL,
  obsId                      = NULL,
  #----- Model comparison slots
  modelList                  = NULL,
  tabdev                     = NULL
  #----- Probability calculator slots
  #----- Option setting
 )
 
# Main
R2STATS = function() {

  # Create R2STATS main window
  r2stats$create()
  
  # Some settings
  r2stats$setVersion(0.65)

  # Show R2STATS interface
  r2stats$show()
}


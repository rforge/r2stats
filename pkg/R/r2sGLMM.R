#--------------------------------------------------------------------------------------------------
#
#                      R2STATS: A Graphical User Interface for GLM and GLMM in R
#                       Yvonnick Noel, University of Brittany, Rennes 2, France
#                                            2006-2010
#
#--------------------------------------------------------------------------------------------------
#                                        GLM PROTOTYPE
#--------------------------------------------------------------------------------------------------

r2sGLMM = proto(

  ###------------------------------------- Model constructor --------------------------------------------

  new = function(.,name,class,func,dvField,dv,ivField,iv,data,family,link,weights,constrFactor,subset) {
    .$proto(name=name,class=class,func=func,dvField=dvField,dv=dv,ivField=ivField,iv=iv,data=data,
            family=family,link=link,weights=weights,constrFactor=constrFactor,subset=subset)
  },

  ###---------------------------------- Model estimation method -----------------------------------------
  estimate = function(.) {
  
   .$estimated = FALSE

    family = .$getFamily()
    link   = .$getLink()
    
    r2stats$setStatus("Statut : estimation des paramètres en cours....")

    # Is the dependent variable a matrix?
    dv = .$dvField
    if(length(.$dv) > 1) dv = paste("cbind(",.$dvField,")",sep="")
    
    # Estimate
    res = try(eval(parse(text=paste(".$Rmodel <-",.$func,"(",dv,"~",.$ivField,",family=",family,"(link=",link,"),data=",.$data,",subset=",.$subset,",weights=",.$weights,")",sep=""))))
    if(inherits(res,"try-error")) return(res)
        
    # Store initial factor names
    indicList = .$getIndicList()
    modelVars = .$getModelVars()
    
    # This allows to get design factors in model definition order
   .$designFactors  = modelVars[ modelVars %in% union(.$getFactorList(),indicList) ]
   
    # Exclude random factors from the IV list
   .$iv = .$getFIV()
  
    # Get explicit labels for factor or indic categories (e.g. Sex0/Sex1 instead of 0/1, -1/1 etc.)
    if(length(.$designFactors)) {
    
      # 'subset' seems to be the only way to get a data.frame when selection retains a single factor
      modelFactors = subset(.$getModelData(),select=.$designFactors)
      for(i in 1:ncol(modelFactors)) modelFactors[,i] = paste(.$designFactors[i],modelFactors[,i],sep="")
     .$groupLabels = factor(do.call("paste",modelFactors))
     
      # Set colors for plots
     .$groupFullColors   = r2stats$getColors(nlevels(.$groupLabels),"Set1")
      names(.$groupFullColors) = levels(.$groupLabels)
     .$groupPastelColors = r2stats$getColors(nlevels(.$groupLabels),"Pastel1")
      names(.$groupPastelColors) = levels(.$groupLabels)
    }
    
    else {
     .$groupFullColors   = r2stats$getColors(1,"Set1")
     .$groupPastelColors = r2stats$getColors(1,"Pastel1")
    }

	  # If a contrast factor is defined, refit with this constraint
	  if(.$constrFactor != "Aucun") {
      res = try(eval(parse(text=paste(".$Rmodel <-update(.$Rmodel,.~",.$getConstrainedFormula(),")"))))
      if(inherits(res,"try-error")) return(res)
	  }

   .$estimated = TRUE
  },
  ###------------------------------------------------ Model summary and print method ------------------------------------------------
  Summary = function(.) {
  
    if(! .$estimated) {
      r2stats$setStatus("Statut : Prêt.")
      return()
    }
    
    model.data   = .$getModelData()  
    familyIndex  = .$getFamilyAsIndex()
    family       =  names(r2stats$linkLists)[familyIndex]
    linkIndex    = .$getLinkAsIndex()
    link         =  r2stats$linkLists[[familyIndex]][linkIndex]
    residuals    = .$Residuals()

    # Subset command
    nobs = length(residuals)
    subset = rep(TRUE,nobs)
    if(.$subset != "NULL") subset = .$getSubset()

    r2stats$setStatus("Statut : statistiques de qualité d\'ajustement....")
    s = summary(.$Rmodel)

    if(nrow(s@coefs)) {
      s@coefs = round(s@coefs,3)
      colnames(s@coefs)[1:3]=c("Estimations","Erreurs-type","T de Student")
      if(.$hasIntercept()) rownames(s@coefs)[1]="Constante"
    }

    res = data.frame(Statistiques=c("Logvraisemblance","Nombre de paramètres","AIC","BIC","Déviance"), 
                      Valeurs=round(c(.$LogLik(),.$df(),.$Aic(),.$Bic(),.$Deviance()),3),row.names=1)
    
    # Group structure (if any) for additional tests
    if(length(.$designFactors))                        groups = .$groupLabels
    if( !(.$constrFactor %in% c("Aucun","Constant")) ) groups = model.data[,.$constrFactor]
    
    r2stats$setStatus("Statut : test des conditions d\'application....")

    # Test normality and homogeneity of variances if family is gaussian and model not constant
    normtest = NULL
    vartest = NULL
    
    # Gaussian family
    if(familyIndex == 1) {

      # Shapiro-Wilk test for normality
      normtest = .$normalityTest(.$Residuals())
    
      # Levene test for homogeneity of variances
      if(exists("groups") && (.$constrFactor !="Constant")) .$varTest(.$Residuals(),groups)
    }

    r2stats$setStatus("Statut : sortie des résultats numériques....")
    
    # Display model name and specifications
    add(r2stats$results,paste("Modèle",.$name),font.attr=c(style="normal",weights="bold",size="large",col="blue"))
    modelProps = cbind(c("Tableau","Formule","Contrainte","Lien","Distribution"),
                       c(.$data,.$getFormula(),.$getConstrFactor(),link,family))
    add(r2stats$results,capture.output(prmatrix(modelProps,rowlab=rep("",5),collab=rep("",2),quote=F)),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display fit statistics
    add(r2stats$results,"Qualité d\'ajustement", font.attr=c(style="normal",col="black",weights="bold"))
    add(r2stats$results,"")
    add(r2stats$results,capture.output(res),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display test of normality
    if(!is.null(normtest)) { 
      add(r2stats$results,"Test de normalité",font.attr=c(style="normal",col="black",weights="bold"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(normtest),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    
    # Display test of homogeneity of variance
    if(!is.null(vartest)) {
      add(r2stats$results,"Test d\'homogénéité des variances", font.attr=c(style="normal",col="black",weights="bold"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(vartest),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    
    # Display fitted values averaged by group, and group frequencies
    if(length(.$designFactors)) {
      add(r2stats$results,"Valeurs prévues (par groupes)",font.attr=c(style="normal",col="black",weights="bold"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(.$getPrediction(),model.data[,.$designFactors],mean)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,"Effectifs (par groupes)",font.attr=c(style="normal",sizes="large",weights="bold",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(.$getPriorWeights(),model.data[,.$designFactors],sum)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }

    # Display estimates
    add(r2stats$results,"Effets fixes",font.attr=c(style="normal",col="black",weights="bold"))
    add(r2stats$results,"")
    if(nrow(s@coefs) > 0) {
      add(r2stats$results,capture.output(s@coefs),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    add(r2stats$results,"Effets aléatoires",font.attr=c(style="normal",col="black",weights="bold"))
    add(r2stats$results,"")
    if(nrow(s@REmat) > 0) {
      colnames(s@REmat)[1:4] = c("Groupes","Noms","Variance","Ecart-type")
      add(r2stats$results,capture.output(noquote(s@REmat)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }  

  },
  ###------------------------------------------------ Model plot methods ------------------------------------------------
  Plot = function(.,h) {
  
    # Model specs
    distr    = .$getFamily()
    liens    = .$getLink()
    varNames = .$getModelVars()
    is.vect  = .$getNumVarList()
    randFact = .$poolRandomFactors()

    # Get R2STATS plotting options
    plotType    = r2stats$getPlotTypeAsIndex()
    selGroup    = r2stats$getSelectedGroup()
    subsetVar   = r2stats$getSubset()
    legend.loc  = r2stats$getLegendLocation()
    legend.cols = r2stats$getLegendCols()
    xlim        = r2stats$getXLim()
    ylim        = r2stats$getYLim()
    addData     = svalue(r2stats$addData)
    addModel    = svalue(r2stats$addModel)
    addGrid     = svalue(r2stats$addGrid)
    addRefLine  = svalue(r2stats$addRefLine)
    addNoise    = svalue(r2stats$addNoise)
    addRandCurves = svalue(r2stats$addRandCurves)

    # Get data
    current.data = .$getModelData()
    y <- yaxis <- .$getY()

    # Fitted values
    fit  = .$getPrediction()
    ffit = .$getFixedPrediction()

    # TODO: Data subset as defined on the graph tab

    # Set colors
    if(length(.$designFactors)) r2stats$setPlotParams(nlevels(.$groupLabels))
    else                        r2stats$setPlotParams(1)
    
    # Set the graphic device (except when we want to copy to clipboard or save to a file)
    if(h$action != "save") visible(r2stats$plotArea) = TRUE
    
    # 1 - Quantile-quantile plot
    if(plotType==4) {
    
      if(distr=="gaussian") {
      
        if(is.null(.$groupLabels)) {
        
          r2stats$currentPlot = qqmath(~.$Residuals(),main = paste("Graphique quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Résidus observés",
                                        panel = function(x, ...) {
                                           panel.qqmathline(x, ...)
                                           panel.qqmath(x, ...)
                                      })
        }
        
        else {

          r2stats$currentPlot = qqmath(~.$Residuals(),groups=.$groupLabels,
                                        key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                        panel = "panel.superpose",main = paste("Graphique quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Résidus observés",
                                        panel.groups = function(x, ...) {
                                           panel.qqmathline(x, ...)
                                           panel.qqmath(x, ...)
                                      })
        }
      }
      
      else if(distr=="Gamma") {
      
        shape = gamma.shape(.$Rmodel)$alpha
        fit = unique(fit)

        if(is.null(.$groupLabels)) {
        
          r2stats$currentPlot = qqmath(~y,distribution = function(x) qgamma(x,shape=shape,rate=shape/fit),
                                        main = paste("Quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Quantiles observés",
                                        panel = function(x, ...) {
                                           panel.qqmathline(x, ...)
                                           panel.qqmath(x, ...)
                                      })
        }
        
        else {
        
          rate = shape/fit
          r2stats$currentPlot = qqmath(~y,groups=.$groupLabels,
                                        key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                        panel = "panel.superpose",main = paste("Quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Quantiles observés",
                                        distribution = function(x) qgamma(x,shape,rate),
                                        prepanel = prepanel.qqmathline,
                                        panel.groups = function(x,group.number,...) {
                                           panel.qqmathline(x,rate=rate[group.number],...)
                                           panel.qqmath(x,...)
                                      })
        }
      }
      
      # No QQ plot for other distributions
      else r2stats$currentPlot = r2stats$emptyPlot()
    }

    # 2 - Regression plot
    else if(plotType==1) {
    
      # No numeric IV
      if(length(is.vect)==0) {
      
        xaxis = .$getFixedLinearTerm()
        
        # No factors neither: A constant model
        if(is.null(.$groupLabels)) {

          xlabel = "Constante estimée"
          r2stats$currentPlot = xyplot(yaxis~xaxis,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       panel = function(x,y,...) {
                                         if(addData)  panel.xyplot(x,y,type="p",cex=.8)
                                         if(addModel) panel.xyplot(x,fit,type="p",pch="+",cex=1.3)
                                       })          
        }
        
        # Purely categorical model
        else {

          xlabel = "Valeurs prévues"
          obsColors = r2stats$getColors(nlevels(randFact),"Set1")
          r2stats$currentPlot = xyplot(y~reorder(.$groupLabels,ffit),groups=randFact,xlab="",ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       auto.key=list(space=legend.loc,col=obsColors,points=F),
                                       panel = function(x,y,groups,subscripts,...) { 
                                         if(addGrid)       panel.grid(h=-1,v=-1)
                                         if(addData)       panel.superpose(x,y,groups,subscripts,type="p",jitter.x=addNoise,jitter.y=addNoise,col=obsColors)
                                         if(addRandCurves) panel.superpose(x,fit[subscripts],groups,subscripts,type="a",col=obsColors)
                                         if(addModel)      panel.superpose(x,ffit[subscripts],groups,subscripts,type="a",col.lines=.$groupFullColors)
                                       })
        }
      }
      
      # A single numeric variable: Take it as the x-axis
      else if(length(is.vect)==1) {
      
        xaxis = current.data[,is.vect]
        xlabel = is.vect

        # No groups
        if(is.null(.$groupLabels)) {

          r2stats$currentPlot = xyplot(y~xaxis,groups=randFact,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       auto.key=FALSE,
                                       panel = function(x,y,groups,subscripts,...) { 
                                         if(addGrid)       panel.grid(h=-1,v=-1)
                                         if(addRefLine)    panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)       panel.superpose(x,y,groups,subscripts,type="p",jitter.x=addNoise,jitter.y=addNoise)
                                         if(addRandCurves) panel.superpose(x,fit[subscripts],groups,subscripts,type="a")
                                         if(addModel)      panel.superpose(x,ffit[subscripts],groups,subscripts,type="a",col="black")
                                       })
        }
        
        # With a group structure
        else {
        
          gfcol = tapply(.$groupFullColors[.$groupLabels],randFact,unique)
          gpcol = tapply(.$groupPastelColors[.$groupLabels],randFact,unique)

          r2stats$currentPlot = xyplot(y~xaxis,groups=randFact,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                       panel = function(x,y,groups,subscripts) { 
                                         if(addGrid)       panel.grid(h=-1,v=-1)
                                         if(addRefLine)    panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)       panel.superpose(x,y,groups,subscripts,type="p",jitter.x=addNoise,jitter.y=addNoise,col=gfcol,fill=gpcol)
                                         if(addRandCurves) panel.superpose(x,fit,groups,subscripts,type="a",col=gpcol)
                                         if(addModel)      panel.superpose(x,ffit,groups,subscripts,type="a",col=gfcol)
                                       })
        }
      }
      
      # Several numeric predictors: Combine them
      else {
      
        # Prepare plot area
        xaxis = .$getNumLinearTerm()
        xlabel = "Combinaison des VI"

        # No groups
        if(is.null(.$groupLabels)) {

          r2stats$currentPlot = xyplot(y~xaxis,groups=randFact,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       auto.key=FALSE,
                                       panel = function(x,y,groups,subscripts,...) { 
                                         if(addGrid)       panel.grid(h=-1,v=-1)
                                         if(addRefLine)    panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)       panel.superpose(x,y,groups,subscripts,type="p",jitter.x=addNoise,jitter.y=addNoise)
                                         if(addRandCurves) panel.superpose(x,fit[subscripts],groups,subscripts,type="a")
                                         if(addModel)      panel.superpose(x,ffit[subscripts],groups,subscripts,type="a",col="black")
                                       })
        }
        
        # With a group structure
        else {
        
          gfcol = tapply(.$groupFullColors[.$groupLabels],randFact,unique)
          gpcol = tapply(.$groupPastelColors[.$groupLabels],randFact,unique)

          r2stats$currentPlot = xyplot(y~xaxis,groups=randFact,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                       panel = function(x,y,groups,subscripts) { 
                                         if(addGrid)       panel.grid(h=-1,v=-1)
                                         if(addRefLine)    panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)       panel.superpose(x,y,groups,subscripts,type="p",jitter.x=addNoise,jitter.y=addNoise,col=gfcol,fill=gpcol)
                                         if(addRandCurves) panel.superpose(x,fit,groups,subscripts,type="a",col=gpcol)
                                         if(addModel)      panel.superpose(x,ffit,groups,subscripts,type="a",col=gfcol)
                                       })
        }
      }
    }

    # 3 - Prediction plot
    else if(plotType==3) {
    
      # No groups
      if(is.null(.$groupLabels)) {
        r2stats$currentPlot = xyplot(y~fit,xlab="Valeurs prévues",ylab=.$dv[1],main=paste("Prédiction",.$name,sep=" - "),
                                     panel = function(x,y,...) { 
                                       if(addGrid)    panel.grid(h=-1,v=-1)
                                       panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                       panel.xyplot(x,y,type="p",...)
                                     })
      }
          
      # With a group structure
      else {
        r2stats$currentPlot = xyplot(y~fit,groups=.$groupLabels,xlab="Valeurs prévues",ylab=.$dv[1],main=paste("Prédiction",.$name,sep=" - "),
                                     key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                     panel = function(x,y,groups,subscripts,...) { 
                                       if(addGrid)    panel.grid(h=-1,v=-1)
                                       panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                       panel.superpose(x,y,groups,subscripts,type="p",...)
                                     })
      }
    }

    # 4 - TODO: Response distribution plot
    else if(plotType==2) {
      r2stats$currentPlot = r2stats$emptyPlot()
    }
    
    # 5 - Residuals distribution
    else if(plotType==5) {
      # Only relevant for gaussian family
      if(distr=="gaussian") {
      
        resids = .$Residuals()
        h = hist(resids,plot=FALSE,freq=FALSE)
        r2stats$currentPlot = histogram(~resids,type="density",main="Histogramme",xlab="Résidus",ylab="Densité",
                                        panel = function(x,...) {
                                          panel.histogram(x,breaks=h$breaks,col=NULL,border=.$groupFullColors[1])
                                          panel.mathdensity(dnorm,args = list(mean=mean(x),sd=.$getDispersion()),col="black")
                                       })
      }
      else r2stats$currentPlot = r2stats$emptyPlot()
    }
    
    # 6 - Prediction and residuals
    else if(plotType==6) {
    
      # No groups
      if(is.null(.$groupLabels)) {
        r2stats$currentPlot = xyplot(.$Residuals(type="standard")~fit,xlab="Valeurs prévues",ylab="Résidus standard",main=paste("Prédiction et résidus",.$name,sep=" - "),
                                     panel = function(x,y,...) { 
                                       if(addGrid)    panel.grid(h=-1,v=-1)
                                       panel.abline(a=1.96,b=0,col="lightgrey",lty=2)
                                       panel.abline(a=-1.96,b=0,col="lightgrey",lty=2)
                                       panel.xyplot(x,y,type="p",...)
                                     })
      }
          
      # With a group structure
      else {
        r2stats$currentPlot = xyplot(.$Residuals(type="standard")~fit,groups=.$groupLabels,xlab="Valeurs prévues",ylab="Résidus standard",
                                     main=paste("Prédiction et résidus",.$name,sep=" - "),
                                     key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                     panel = function(x,y,groups,subscripts,...) { 
                                       if(addGrid)    panel.grid(h=-1,v=-1)
                                       panel.abline(a=1.96,b=0,col="lightgrey",lty=2)
                                       panel.abline(a=-1.96,b=0,col="lightgrey",lty=2)
                                       panel.superpose(x,y,groups,subscripts,type="p",...)
                                     })
      }
    }  

    # Is there a legend?
    if(legend.loc == "none") r2stats$currentPlot = update(r2stats$currentPlot,legend=NULL)
    
    # Are there restriction or dilation on the axis limits?
    if(!is.null(xlim)) r2stats$currentPlot = update(r2stats$currentPlot,xlim=xlim)
    if(!is.null(ylim)) r2stats$currentPlot = update(r2stats$currentPlot,ylim=ylim)
    
    # Want noise?
    if(addNoise) r2stats$currentPlot = update(r2stats$currentPlot,jitter.x=TRUE,jitter.y=TRUE)    
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 ACCESSOR METHODS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Get model name
  getName = function(.) {
    return(.$name)
  },
  ### Get model class ("glm","mer",...)
  getClass = function(.) {
    return(.$class)
  },
  ### Get dataframe name
  getDataName = function(.) {
    return(.$data)
  },
  ### Get dependent variable name
  getDV = function(.) {
    return(.$dv)
  },
  ### Get dependent variable
  getY = function(.) {
    return(.$Rmodel@y)
  },
  ### Get independent variable names
  getIV = function(.) {
    return(.$iv)
  },
  ### Get *fixed* independent variable names
  getFIV = function(.) {
    return(.$iv[ !(.$iv %in% .$getRandomFactorList())])
  },
  getModelVars = function(.) {
    all.vars(terms(.$Rmodel))
  },
  ### Get the names of factor predictors
  getFactorList = function(.) {
    varTypes = attr(terms(.$Rmodel),"dataClasses")
    fn = names(varTypes)[varTypes == "factor"]    
    fn[ !(fn %in% .$dv) ]
  },
  # Get the names of the random factors
  getRandomFactorList = function(.) {
    names(.$Rmodel@flist)
  },
  ### Get the names of numeric predictors
  getNumVarList = function(.) {
    setdiff(.$iv,.$designFactors)
  },
  ### Get the names of indicator (numeric binary) variables
  getIndicList = function(.) {
    if(!length(.$iv)) return(NULL)
    .$iv[sapply(.$getModelData()[,.$iv], function(var) length(unique(var))==2)]
  },
  ### Get model formula
  getFormula = function(.) {
    paste(.$dvField,.$ivField,sep="~")
  },
  ### Get constrained formula
  getConstrainedFormula = function(.) {
  
    Formula = .$ivField
    factor.list = union(.$getFactorList(),.$getIndicList())
    constrF = .$getConstrFactor()
    
    if(constrF == "Constant") return("1")
    
    # This replaces all factor occurences by the constraint factor
    for(v in factor.list) Formula = gsub(v,constrF,Formula)
    Formula
  },
  ### Get distribution family in R
  getFamily = function(.) {
    names(.$RLinkList)[.$family]
  },
  getFamilyAsIndex = function(.) {
    return(.$family)
  },
  ### Get link function in R
  getLink = function(.) {
    family = .$getFamily()
    .$RLinkList[[family]][.$link]
  },
  getLinkAsIndex = function(.) {
    return(.$link)
  },
  ### Get the list of available plots for this model
  getAvailablePlots = function(.) {
    return(.$graphTypes)
  },
  ### Get the name of the constraint factor
  getConstrFactor = function(.) {
    return(.$constrFactor)
  },
  ### Get the data subtable used in model estimation
  getModelData = function(.) {

    formula = paste(c(.$dv,.$iv),collapse="+")
    
    # If a (non constant) constraint factor is defined, it must be added to the model frame
    if( !(.$constrFactor %in% c("Aucun","Constant"))) formula = paste(c(formula,.$getConstrFactor()),collapse="+")

    # With model frames, subsetting one variable as a data.frame is possible
    eval(parse(text=paste("model.frame(~",formula,",data=",.$data,",subset=",.$subset,")",sep="")),envir=.GlobalEnv)
  },
  ### Get the full data table
  getFullData = function(.) {
    eval(parse(text=.$Rmodel@call[[3]]),env=.GlobalEnv)
  },
  ### Get individual fitted values
  getPrediction = function(.) {
    fitted(.$Rmodel)
  },
  ### Get fixed (group) predicted values
  getFixedPrediction = function(.) {
    .$invLink(.$getFixedLinearTerm())
  },
  ### Get the eta linear term (including random effects)
  getLinearTerm = function(.) {
    .$Rmodel@eta
  },
  ### Get the eta linear term (including random effects)
  getFixedLinearTerm = function(.) {
    .$Rmodel@X %*% fixef(.$Rmodel)
  },
  ### Get the linear term on numeric variables only
  getNumLinearTerm = function(.) {
  
    # Factors are binary recoded in design matrix
    whichNum = apply(.$Rmodel@X,2,function(var) length(unique(var))>2)
    
    # Numeric prediction
    numCoefs = cbind(coef(summary(.$Rmodel))[whichNum,1])
    cbind(.$Rmodel@X)[,whichNum] %*% numCoefs
  },
  ### Get the prior weights
  getPriorWeights = function(.) {

    if(length(.$Rmodel@pWt)) return(.$Rmodel@pWt)
    else                     return(rep(1,length(.$getY())))
  },
  ### Boolean: Has the model an intercept
  hasIntercept = function(.) {
    attr(terms(.$Rmodel),"intercept")
  },
  ### Evaluate the subset command
  getSubset = function(.) {
    eval(parse(text=paste("with(",.$data,",",.$subset,")",sep="")),envir=.GlobalEnv)
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                             VARIABLE EDITION TOOLS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Get response function (inverse of link function
  invLink = function(.,x) {
  
    link = .$getLink()
    
         if(link == "identity") return(x)
    else if(link == "log")      return(exp(x))
    else if(link == "inverse")  return(1/x)
    else if(link == "logit")    return(plogis(x))
    else if(link == "probit")   return(pnorm(x))
    else if(link == "cauchit")  return(0.5 + atan(x)/pi)
    else if(link == "cloglog")  return(1-exp(-exp(x)))
    else if(link == "sqrt")     return(x**2)
    else if(link == "1/mu^2")   return(sqrt(1/x))
  },
  ### Make a single factor from all random variables
  poolRandomFactors = function(.) {
     factor(do.call("paste",.$Rmodel@flist))
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                 EDITION TOOLS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Remove cbind() from a string
  strip.cbind = function(l) {
    l = sapply(l,function(x) sub("cbind","",x))
    l = sapply(l,function(x) sub("\\(","",x))
    sapply(l,function(x) sub("\\)","",x))
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                              ADDTIONAL TESTS AND STATISTICS
  #
  #------------------------------------------------------------------------------------------------------------------------
  ### Homoskedasticity test
  varTest = function(.,y,group) {
  
    nl = nlevels(group)
    if(nl < 2) return()
    
    # Two groups
    if(nl == 2) {
      vt = var.test(y~group)
      table = c(vt$statistic,vt$parameter,p=vt$p.value)
      names(table)=c("F","d.d.l. num.","d.d.l. dénom.","p")
    }
    
    # More than two groups
    else {
      meds = tapply(y, group, median, na.rm = TRUE)
      resp = abs(y - meds[group])
      table = anova(lm(resp ~ group))[, c(1, 4, 5)]
      colnames(table) = c("d.d.l.","F de Levene","Pr(>F)")
      rownames(table) = c("Groupes","Erreur")
    }
    
    table
  },
  ### Test of normality
  normalityTest = function(.,y) {
  
    # Shapiro.test accepts 5000 obs. max.
    if(length(y) > 5000) normtest = shapiro.test(sample(y,5000))
    else                 normtest = shapiro.test(y)

    normtest = data.frame(W=round(normtest$statistic,3),Prob=round(normtest$p.value,3))
    row.names(normtest)="Shapiro-Wilk"
    
    normtest
  },
  ### AIC
  Aic = function(.) { 
    AIC(.$LogLik())
  },
  ### BIC function
  Bic = function(.) {
    BIC(.$LogLik())  
  },  
  ### Loglikelihood
  LogLik = function(.,REML=FALSE) { 
    logLik(.$Rmodel,REML=REML)
  },
  ### Degrees of freedom
  df = function(.) { 
    attr(logLik(.$Rmodel),"df")
  },
  ### Deviance
  Deviance = function(.) { 
    deviance(.$Rmodel)
  },
  ### Percentage of deviance explained
  devExplained = function(.) { 
  
    # Null deviance
    #eval(parse(text="M0 <- glm(",.$dvField,"~1,family=",family,"(link=",link,"),data=",.$data,",subset=",.$subset,",weights=",.$weights,")",sep=""))
    #nullDeviance = deviance()

    #1 - (deviance(.$Rmodel)/nullDeviance)
  },
  ### Get model residuals
  Residuals = function(.,type="raw") {
  
    if(type=="raw")           residuals(.$Rmodel)
    else if(type=="standard") residuals(.$Rmodel)/.$getDispersion()
  },
  getRandomVar = function(.) {
    summary(.$Rmodel)@sigma  
  },
  getDispersion = function(.) {
    attr(VarCorr(.$Rmodel),"sc")
  },
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                    SLOTS
  #
  #------------------------------------------------------------------------------------------------------------------------
  # SLOT                    INITIAL VALUE                                               DESCRIPTION
  #------------------------------------------------------------------------------------------------------------------------
  Rmodel        = NULL,                                                              # R model object
  name          = "",                                                                # Model name
  class         = "mer",                                                             # Model type: GLM, GLMM, MANOVA, VGLM...
  func          = "glmer",                                                           # Estimation function
  data          = NULL,                                                              # Data set name
  dvField       = "",                                                                # Content of the DV field as a string
  dv            = NULL,                                                              # Vector of dependent variables names
  ivField       = "",                                                                # Content of the IV field as a string
  iv            = NULL,                                                              # Vector of independent variables names (including rand. vars)
  family        = 1,                                                                 # Index of the family function (see list below)
  link          = 1,                                                                 # Index of the link function (see list below)
  RLinkList     = list(gaussian = c("identity","log","inverse"),                     # List of vectors of link function by family
                      binomial = c("logit","probit","cauchit","log","cloglog"),
                      poisson  = c("log","sqrt","identity"),
                      Gamma    = c("inverse","log","identity"),
                      inverse.gaussian = c("inverse","log","identity","1/mu^2"),
                      multinomial = c("logistic","probit","cloglog","loglog",
                                       "cauchit","Aranda-Ordaz", "log-gamma"),
              "ordered multinomial"= c("logistic","probit","cloglog","loglog",
                                       "cauchit","Aranda-Ordaz", "log-gamma")),      # List of link functions
  weights       = "NULL",                                                            # Name of the weighting variable ("NULL" if none)
  constrFactor  = NULL,                                                              # Name of the constraint factor
  designFactors = NULL,                                                              # Names of the original factors (before constraints)
  subset        = "NULL",                                                            # Character vector containing the subsetting command ("NULL" if none)
  estimated     = FALSE,                                                             # Flag: Model is estimated (TRUE) or not (FALSE)
  groupFullColors   = 1,                                                             # Colors to be used in plots
  groupPastelColors = 1,                                                             # Colors to be used in plots
  groupLabels   = NULL,                                                              # Labels for the groups
  graphTypes    = c("Graphique de régression","Distribution de la réponse",          # Vector of possible plots for this type of model
                   "Valeurs prévues et observées","Graphique quantile-quantile",
                   "Distribution des résidus","Prévisions et résidus")
)

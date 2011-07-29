#--------------------------------------------------------------------------------------------------
#
#                      R2STATS: A Graphical User Interface for GLM and GLMM in R
#                       Yvonnick Noel, University of Brittany, Rennes 2, France
#                                            2006-2010
#
#--------------------------------------------------------------------------------------------------
#                                        GLM PROTOTYPE
#--------------------------------------------------------------------------------------------------

r2sGLM = proto(

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
    s = summary.glm(.$Rmodel)

    if(nrow(s$coefficients)) {
      s$coefficients = round(s$coefficients,3)
      colnames(s$coefficients)[1:3]=c("Estimations","Erreurs-type","T de Student")
      if(.$hasIntercept()) rownames(s$coefficients)[1]="Constante"
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
      if(exists("groups") && (.$constrFactor !="Constant")) vartest = .$varTest(.$Residuals(),groups)
    }

    r2stats$setStatus("Statut : sortie des résultats numériques....")
    
    # Display model name and specifications
    add(r2stats$results,paste("Modèle",.$name),font.attr=c(style="normal",weights="bold",size="large",col="blue"))
    modelProps = cbind(c("Tableau","Formule","Contrainte","Lien","Distribution"),
                       c(.$data,.$getFormula(),.$getConstrFactor(),link,family))
    add(r2stats$results,capture.output(prmatrix(modelProps,rowlab=rep("",5),collab=rep("",2),quote=F)),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display fit statistics
    add(r2stats$results,"Qualité d\'ajustement", font.attr=c(style="normal",weights="bold",col="black"))
    add(r2stats$results,"")
    add(r2stats$results,capture.output(res),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display test of normality
    if(!is.null(normtest)) { 
      add(r2stats$results,"Test de normalité",font.attr=c(style="normal",weights="bold",col="black"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(normtest),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    
    # Display test of homogeneity of variance
    if(!is.null(vartest)) {
      add(r2stats$results,"Test d\'homogénéité des variances", font.attr=c(style="normal",weights="bold",col="black"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(vartest),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    
    # Display fitted values averaged by group, and group frequencies
    if(length(.$designFactors)) {
      add(r2stats$results,"Valeurs prévues (par groupes)",font.attr=c(style="normal",weights="bold",col="black"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(.$getPrediction(),model.data[,.$designFactors],mean)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,"Effectifs (par groupes)",font.attr=c(style="normal",weights="bold",col="black"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(.$getPriorWeights(),model.data[,.$designFactors],sum)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }

    # Display coefficients
    add(r2stats$results,"Coefficients",font.attr=c(style="normal",weights="bold",col="black"))
    add(r2stats$results,"")
    if(nrow(s$coefficients) > 0) {
      add(r2stats$results,capture.output(s$coefficients),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
    add(r2stats$results,paste("Dispersion :",round(s$dispersion,4)),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

  },
  ###------------------------------------------------ Model plot methods ------------------------------------------------
  Plot = function(.,h) {
  
    # Model specs
    distr    = .$getFamily()
    liens    = .$getLink()
    varNames = .$getModelVars()
    is.vect  = .$getNumVarList()

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

    # Get data
    current.data = .$getModelData()
    y <- yaxis <- .$getY()

    # Fitted values
    fit = .$getPrediction()

    # TODO: Data subset as defined on the graph tab

    # Set lattice colors options
    if(length(.$designFactors)) r2stats$setPlotParams(nlevels(.$groupLabels))
    else                        r2stats$setPlotParams(1)    

    # Set the graphic device (except when we want to copy to clipboard or save to a file)
    if(h$action != "save") visible(r2stats$plotArea) = TRUE
    
    # 1 - Quantile-quantile plot
    if(plotType==4) {
    
      if(distr=="gaussian") {
      
        if(is.null(.$groupLabels)) {
        
          r2stats$currentPlot = qqmath(~.$Residuals(type="standard"),aspect = "fill",main = paste("Graphique quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Résidus standards observés",
                                        panel = function(x, ...) {
                                           panel.qqmathline(x, ...)
                                           panel.qqmath(x, ...)
                                      })
        }
        
        else {

          r2stats$currentPlot = qqmath(~.$Residuals(type="standard"),groups=.$groupLabels,
                                        key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                        panel = "panel.superpose",main = paste("Graphique quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Résidus standards observés",
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
                                        xlab = "Quantiles théoriques",ylab="Résidus standards observés",
                                        panel = function(x, ...) {
                                           panel.qqmathline(x, ...)
                                           panel.qqmath(x, ...)
                                      })
        }
        
        else {
        
          rate = shape/fit
          r2stats$currentPlot = qqmath(~y,groups=.$groupLabels,
                                        panel = "panel.superpose",main = paste("Quantile-quantile",.$name,sep=" - "),
                                        xlab = "Quantiles théoriques",ylab="Quantiles observés",
                                        distribution = function(x) qgamma(x,shape,rate),
                                        panel.groups = function(x,group.number,...) {
                                           panel.qqmathline(x,shape=shape,rate=rate[group.number],...)
                                           panel.qqmath(x,...)
                                      })
        }
      }
      
      # No QQ plot for other distributions
      else r2stats$emptyPlot()
    }

    # 2 - Regression plot
    else if(plotType==1) {
    
      # No numeric IV
      if(length(is.vect)==0) {
      
        xaxis = .$getLinearTerm()      

        # No factors neither: A constant model
        if(is.null(.$groupLabels)) {

          xlabel = "Constante estimée"
          r2stats$currentPlot = xyplot(yaxis~xaxis,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       panel = function(x,y,...) {
                                         if(addData) panel.xyplot(x,y,type="p",cex=.8)
                                         if(addModel) panel.xyplot(x,fit,type="p",pch="+",cex=1.3)
                                       })          
        }
        
        # Purely categorical model
        else {

          xlabel = "Valeurs prévues (par groupes)"
          r2stats$currentPlot = xyplot(yaxis~xaxis,groups=.$groupLabels,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                       panel = function(x,y,groups,subscripts,...) {
                                         if(addGrid)    panel.grid(h=-1,v=-1)
                                         if(addRefLine) panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)    panel.superpose(x,y,groups,subscripts,type="p",cex=.8)
                                         if(addModel)   panel.superpose(x,fit,groups,subscripts,type="p",pch="+",cex=1.3)
                                       })
        }
      }
      
      # A single numeric variable: Take it as the x-axis
      else if(length(is.vect)==1) {
        xaxis = current.data[,is.vect]
        xlabel = is.vect

        # No groups: A simple linear model
        if(is.null(.$groupLabels)) {

          r2stats$currentPlot = xyplot(y~xaxis,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       panel = function(x,y,...) { 
                                         if(addGrid)    panel.grid(h=-1,v=-1)
                                         if(addRefLine) panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)    panel.xyplot(x,y,type="p",jitter.x=addNoise,jitter.y=addNoise)
                                         if(addModel)   panel.xyplot(x,fit,type="a",col="black")
                                       })
        }
        
        # With a group structure
        else {

          r2stats$currentPlot = xyplot(y~xaxis,groups=.$groupLabels,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                       panel = function(x,y,groups,subscripts,group.number,...) { 
                                         if(addGrid)    panel.grid(h=-1,v=-1)
                                         if(addRefLine) panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)    panel.superpose(x,y,groups,subscripts,type="p",col=.$groupFullColors[group.number],
                                                              jitter.x=addNoise,jitter.y=addNoise)
                                         if(addModel)   panel.superpose(x,fit,groups,subscripts,type="a",col=.$groupFullColors[group.number])
                                       })
        }
      }
      
      # Several numeric predictors: Combine them
      else {
      
        xaxis = as.matrix(current.data[,is.vect]) %*% coef(.$Rmodel)[is.vect]
        xlabel = "Combinaison des VI"

        # No groups: A simple linear model
        if(is.null(.$groupLabels)) {

          r2stats$currentPlot = xyplot(y~xaxis,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       panel = function(x,y,...) { 
                                         if(addGrid)    panel.grid(h=-1,v=-1)
                                         if(addRefLine) panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)    panel.xyplot(x,y,type="p",jitter.x=addNoise,jitter.y=addNoise)
                                         if(addModel)   panel.xyplot(x,fit,type="a",col="black")
                                       })
        }
        
        # With a group structure
        else {

          r2stats$currentPlot = xyplot(y~xaxis,groups=.$groupLabels,xlab=xlabel,ylab=.$dv[1],main=paste("Régression",.$name,sep=" - "),
                                       key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                       panel = function(x,y,groups,subscripts,group.number,...) { 
                                         if(addGrid)    panel.grid(h=-1,v=-1)
                                         if(addRefLine) panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                         if(addData)    panel.superpose(x,y,groups,subscripts,type="p",col=.$groupFullColors[group.number],
                                                              jitter.x=addNoise,jitter.y=addNoise)
                                         if(addModel)   panel.superpose(x,fit,groups,subscripts,type="a",col=.$groupFullColors[group.number])
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
                                       panel.xyplot(fit,y,type="p",...)
                                     })
      }
          
      # With a group structure
      else {
        r2stats$currentPlot = xyplot(y~fit,groups=.$groupLabels,xlab="Valeurs prévues",ylab=.$dv[1],main=paste("Prédiction",.$name,sep=" - "),
                                     key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                     panel = function(x,y,groups,subscripts,...) { 
                                       if(addGrid)    panel.grid(h=-1,v=-1)
                                       panel.abline(a=0,b=1,col="lightgrey",lty=2)
                                       panel.superpose(fit,y,groups,subscripts,type="p",...)
                                     })
      }
    }

    # 4 - Response distribution plot
    else if(plotType==2) {
    
      # No groups
      if(is.null(.$groupLabels)) {
      
        r2stats$currentPlot = densityplot(~y,xlab=.$dv[1],ylab="Densité",main=paste("Densité de la réponse",.$name,sep=" - "),
                                     panel = function(x,...) {
                                       if(distr == "gaussian")   panel.mathdensity(dnorm,args = list(mean=mean(x),sd=sqrt(.$getDispersion())))
                                       else if(distr == "Gamma") {
                                         shape = gamma.shape(.$Rmodel)$alpha
                                         panel.mathdensity(dgamma,args = list(shape=shape,rate=shape/unique(fit)),col="black")
                                       }
                                       panel.rug(x,...)
                                     })
      }
          
      # With a group structure
      else {
        fit = tapply(fit,.$groupLabels,mean)
        r2stats$currentPlot = densityplot(~y,groups=.$groupLabels,xlab=.$dv[1],ylab="Densité",main=paste("Densité de la réponse",.$name,sep=" - "),lwd=2,
                                     panel = "panel.superpose",col=.$groupFullColors,
                                     key = list(space=legend.loc,text=list(levels(.$groupLabels)),col=.$groupFullColors,columns=legend.cols),
                                     panel.groups = function(x,group.number,...) {
                                       if(distr == "gaussian")   panel.mathdensity(dnorm,args = list(mean=fit[group.number],sd=sqrt(.$getDispersion())),...)
                                       else if(distr == "Gamma") {
                                         shape = gamma.shape(.$Rmodel)$alpha
                                         panel.mathdensity(dgamma,args = list(shape=shape,rate=shape/fit[group.number]),...)
                                       }
                                       panel.rug(x,...)
                                     })
      }
    
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
                                          panel.mathdensity(dnorm,args = list(mean=mean(x),sd=sqrt(.$getDispersion())),col="black")
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

    # No legend?
    if(legend.loc == "none") r2stats$currentPlot = update(r2stats$currentPlot,legend=NULL)
    
    # Are there restriction or dilation on the axis limits?
    if(!is.null(xlim)) r2stats$currentPlot = update(r2stats$currentPlot,xlim=xlim)
    if(!is.null(ylim)) r2stats$currentPlot = update(r2stats$currentPlot,ylim=ylim)
    
    # Want noise?
    if(addNoise) r2stats$currentPlot = update(r2stats$currentPlot,jitter.x=TRUE,jitter.y=TRUE)
    
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
    return(.$Rmodel$y)
  },
  ### Get independent variable names
  getIV = function(.) {
    return(.$iv)
  },
  getModelVars = function(.) {
    all.vars(terms(.$Rmodel))
  },
  ### Get the names of factor/indicator predictors
  getFactorList = function(.) {
    names(.$Rmodel$xlevels)
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
    factor.list = .$getFactorList()
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
    .$Rmodel$data
  },
  ### Get fitted values
  getPrediction = function(.) {
    .$Rmodel$fitted.values
  },
  ### Get the eta term
  getLinearTerm = function(.) {
    .$Rmodel$linear.predictors
  },
  ### Get the prior weights
  getPriorWeights = function(.) {
    .$Rmodel$prior.weights
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
      table = c(vt$statistic,round(vt$parameter),p=vt$p.value)
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
  ### # Generic BIC function for GLMs
  Bic = function(.) {
  
    L = logLik(.$Rmodel)[1]
    npar = attr(logLik(.$Rmodel),"df")
    family = .$getFamily()
    
    if(family == "binomial")     N = sum(.$Rmodel$prior.weights)
    else if(family == "poisson") N = sum(.$Rmodel$y)
    else N = length(.$Rmodel$y)
    
    -2*L+log(N)*npar
  },  
  ### Loglikelihood
  LogLik = function(.) { 
    logLik(.$Rmodel)
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
    nullDeviance = deviance(update(.$Rmodel,.~1))
    1 - (deviance(.$Rmodel)/nullDeviance)
  },
  ### AIC
  Aic = function(.) { 
    AIC(.$Rmodel)
  },
  ### Get model residuals
  Residuals = function(.,type="raw") {
  
    if(type=="raw")           residuals(.$Rmodel)
    else if(type=="standard") rstandard(.$Rmodel)
    else if(type=="student")  rstudent(.$Rmodel)
  },
  ### Get dispersion parameter
  getDispersion = function(.) {
    summary(.$Rmodel)$dispersion
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
  class         = "glm",                                                             # Model type: GLM, GLMM, MANOVA, VGLM...
  func          = "glm",                                                             # Estimation function
  data          = NULL,                                                              # Data set name
  dvField       = "",                                                                # Content of the DV field as a string
  dv            = NULL,                                                              # Vector of dependent variables names
  ivField       = "",                                                                # Content of the IV field as a string
  iv            = NULL,                                                              # Vector of independent variables names
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
  groupPastelColors = 1,                                                             # Pastel colors to be used in plots
  groupLabels   = NULL,
  graphTypes    = c("Graphique de régression","Distribution de la réponse",          # Vector of possible plots for this type of model
                   "Valeurs prévues et observées","Graphique quantile-quantile",
                   "Distribution des résidus","Prévisions et résidus")
)

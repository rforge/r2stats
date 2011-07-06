#--------------------------------------------------------------------------------------------------
#
#                      R2STATS: A Graphical User Interface for GLM and GLMM in R
#                       Yvonnick Noel, University of Brittany, Rennes 2, France
#                                            2006-2010
#
#--------------------------------------------------------------------------------------------------
#                                          CLM PROTOTYPE
#--------------------------------------------------------------------------------------------------

r2sCLM = proto(

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
    
    # Estimate
    if(family == "multinomial") cmd = paste(".$Rmodel <-",.$func,"(",dv,"~1,nominal=",.$ivField,",link='",link,"',data=",.$data,",subset=",.$subset,",weights=",.$weights,")",sep="")
    else if(family == "ordered multinomial") cmd = paste(".$Rmodel <-",.$func,"(",dv,"~",.$ivField,",link='",link,"',data=",.$data,",subset=",.$subset,",weights=",.$weights,")",sep="")
    
    res = try(eval(parse(text=cmd)))
    if(inherits(res,"try-error")) return(res)
        
    # Store initial factor names
    indicList = .$getIndicList()
    modelVars = .$getModelVars()
    
    # This allows to get design factors in model definition order
   .$designFactors  = modelVars[ modelVars %in% union(.$getFactorList(),indicList) ]
   
    # Get independent variables
   .$iv = .$getIV()
  
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

    # Subset command
    nobs = .$Rmodel$nobs
    subset = rep(TRUE,nobs)
    if(.$subset != "NULL") subset = .$getSubset()

    r2stats$setStatus("Statut : statistiques de qualité d\'ajustement....")
    s = summary(.$Rmodel)

    if(nrow(s$coef)) {
      s$coef = round(s$coef,3)
      colnames(s$coef)[1:3]=c("Estimations","Erreurs-type","Z")
    }

    res = data.frame(Statistiques=c("Logvraisemblance","Nombre de paramètres","AIC","BIC","Déviance"), 
                      Valeurs=round(c(.$LogLik(),.$df(),.$Aic(),.$Bic(),.$Deviance()),3),row.names=1)
    
    # Group structure (if any) for additional tests
    if(length(.$designFactors))                        groups = .$groupLabels
    if( !(.$constrFactor %in% c("Aucun","Constant")) ) groups = model.data[,.$constrFactor]
    
    r2stats$setStatus("Statut : sortie des résultats numériques....")
    
    # Display model name and specifications
    add(r2stats$results,paste("Modèle",.$name),font.attr=c(style="normal",weights="bold",sizes="large",col="blue"))
    modelProps = cbind(c("Tableau","Formule","Contrainte","Lien","Distribution"),
                       c(.$data,.$getFormula(),.$getConstrFactor(),link,family))
    add(r2stats$results,capture.output(prmatrix(modelProps,rowlab=rep("",5),collab=rep("",2),quote=F)),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display fit statistics
    add(r2stats$results,"Qualité d\'ajustement", font.attr=c(style="normal",weights="bold",sizes="medium"))
    add(r2stats$results,"")
    add(r2stats$results,capture.output(res),font.attr=c(family="monospace",size="medium"))
    add(r2stats$results,"")

    # Display fitted values averaged by group, and group frequencies
    if(length(.$designFactors)) {
      add(r2stats$results,"Valeurs prévues (par groupes)",font.attr=c(style="normal",weights="bold",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(s$fit,model.data[,.$designFactors],mean)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,"Effectifs (par groupes)",font.attr=c(style="normal",weights="bold",size="medium"))
      add(r2stats$results,"")
      add(r2stats$results,capture.output(tapply(.$getPriorWeights(),model.data[,.$designFactors],sum)),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }

    # Display estimates
    add(r2stats$results,"Coefficients",font.attr=c(style="normal",weights="bold",size="medium"))
    add(r2stats$results,"")
    if(nrow(s$coef) > 0) {
      add(r2stats$results,capture.output(s$coef),font.attr=c(family="monospace",size="medium"))
      add(r2stats$results,"")
    }
  },
  ###------------------------------------------------ Model plot methods ------------------------------------------------
  Plot = function(.,h) {
  
    ## TODO: Désactivé pour l'instant
    return()
    
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
    fit  = .$getPrediction()

    # TODO: Data subset as defined on the graph tab

    # Set colors
    if(length(.$designFactors)) r2stats$setPlotParams(nlevels(.$groupLabels))
    else                        r2stats$setPlotParams(1)
    
    # Set the graphic device (except when we want to copy to clipboard or save to a file)
    if(h$action != "save") visible(r2stats$plotArea) = TRUE
    
    # 1 - Regression plot
    if(plotType==1) {
    
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
    else if(plotType==2) {
    
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
    return(.$Rmodel$y)
  },
  ### Get independent variable names
  getIV = function(.) {
    return(.$iv)
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
  ### Get the full data table (not in model object, so we fetch it from GlobalEnv
  getFullData = function(.) {
    eval(parse(text=.$Rmodel@misc$dataname),env=.GlobalEnv)
  },
  ### Get individual fitted values
  getPrediction = function(.) {
    fitted(.$Rmodel)
  },
  ### Get the eta linear term (including random effects)
  getLinearTerm = function(.) {
    .$Rmodel@predictors
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

    if(length(weights(.$Rmodel))) return(weights(.$Rmodel))
    else                          return(rep(1,nrow(.$getY())))
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
  #------------------------------------------------------------------------------------------------------------------------
  #
  #                                                    SLOTS
  #
  #------------------------------------------------------------------------------------------------------------------------
  # SLOT                    INITIAL VALUE                                               DESCRIPTION
  #------------------------------------------------------------------------------------------------------------------------
  Rmodel        = NULL,                                                              # R model object
  name          = "",                                                                # Model name
  class         = "mnm",                                                             # Model type: GLM, GLMM, MANOVA, VGLM...
  func          = "vglm",                                                            # Estimation function
  data          = NULL,                                                              # Data set name
  dvField       = "",                                                                # Content of the DV field as a string
  dv            = NULL,                                                              # Vector of dependent variables names
  ivField       = "",                                                                # Content of the IV field as a string
  iv            = NULL,                                                              # Vector of independent variables names (including rand. vars)
  family        = 6,                                                                 # Index of the family function (see list below)
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
  graphTypes    = c("Graphique de régression","Valeurs prévues et observées")        # Vector of possible plots for this type of model
)

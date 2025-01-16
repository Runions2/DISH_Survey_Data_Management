# MSM_call_by_group ####

# This function runs MSM_call for different groups
MSM_call_by_group <- function(data_set, bygroup, id, intake, model, output, 
                              FFQvar=NULL, P_cons=NULL, start_group=NULL, 
                              stop_group=NULL, verbose="yes", seed=123456789,
                              precision=0.000001, dslabel=NULL, append=FALSE)
{
  # determine characteristics of the group column:
  bygroup_list <- lapply(data_set, unique)[ bygroup]
  bygroup_count <- sapply(bygroup_list, length)
  if (is.null(start_group))
  {
    start_group = 1
  }
  if (is.null(stop_group))
  {
    stop_group = bygroup_count
  }
  print(paste(start_group, stop_group, bygroup_list))
  
  for (ix in start_group:stop_group)
  {
    print(paste(bygroup, ix))
    bygroup_set <- subset(data_set, get(bygroup) == bygroup_list[[1]][ix])
    ioutput <- paste(output, bygroup, bygroup_list[[1]][ix], sep = "_")
    bygrouplabel <- paste(dslabel, bygroup, bygroup_list[[1]][ix], sep = " ")
    MSM_call(bygroup_set,  id, intake, model,
             ioutput, FFQvar, P_cons, verbose, seed, precision, byGroup=output, dslabel=bygrouplabel, append=append)
  }
}

MSM_call_group <- function(data_set, bygroup, groupno, id, intake, model, 
                           output, FFQvar=NULL, P_cons=NULL, verbose="yes",
                           seed=123456789, precision=0.000001,
                           dslabel=NULL, append=FALSE)
{
  # determine characteristics of the group column:
  bygroup_set <- subset(data_set, get(bygroup) == groupno)
  bygrouplabel <- paste(dslabel, bygroup, groupno, sep = " ")
  
  MSM_call(bygroup_set, id, intake, model,  output, FFQvar, P_cons, verbose, seed, precision, byGroup=bygroup, groupNo=groupno, dslabel=bygrouplabel, append=append)
}

# *****************************************************************************************************
# MSM_call ####
# This function runs the MSM.UNISTATS and MSM functions
MSM_call <- function(data_set,  id, intake, model, 
                     output=NULL, FFQvar=NULL, P_cons=NULL, verbose="yes",
                     seed=123456789, precision=0.000001, byGroup=NULL, groupNo=NULL,
                     dslabel=NULL, append=FALSE, output.suffix=".txt", output.sep= "\t")
{
  dataset_name <- deparse(substitute(data_set))
  
  if (is.null(output))
  {
    myresult <- paste(dataset_name, "msm",sep = "_")
  }
  else
  {
    myresult <- output
  }
  
  if (! is.null(byGroup))
  {
    myresult <- paste(myresult, byGroup,  groupNo, sep="_")
  }
  
  if (is.null(dslabel))
  {
    dslabel <- myresult
  }
  
  print(paste('== ===> start calculations for', myresult, sep=" "))
  print(paste('== =====> begin', date(), sep=" "))
  
  if (!is.null(id)  )
  { 
    assign(myresult, MSM(data_set, id, intake, model, FFQvar=FFQvar, 
                         P_cons=P_cons, outputfile=output, 
                         verbose=verbose, seed=seed, precision=precision, 
                         label=dslabel), pos=1 )
  # set group var
    if (!is.null(byGroup))
    {
      mr <- get(myresult)
      mr[, byGroup] <- groupNo
      assign(myresult, mr)
    }
  }
  
  # store result data set in local file
  if (!is.null(output))
  {
    resultname <- output
    if (! is.null(byGroup))
    {
      if ( append)
      {
        resultname <- paste(resultname, byGroup, sep="_")
      }
      else
      {
        resultname <- paste(resultname, byGroup,  groupNo, sep="_")
      }
    }
    resultfile <- paste(resultname,output.suffix, sep = "")
  }
  
  if (!is.null(myresult))
  {
    myvars <- c(paste(intake, "_c_m", sep=""), paste(intake, "_c_usual", sep=""), 
                paste(intake, "_all_m", sep=""), paste(intake, "_all_usual", sep="") )
    if (is.null(byGroup))
    {
      uni <- MSM.UNISTATS(get(myresult), vars=myvars, 
                          stats=c('length', 'mean' , 'sd', 'skewness'), 
                          quantiles=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), 
                          output=output, output.append=TRUE, 
                          output.suffix=output.suffix, 
                          output.sep=output.sep, doround=1)
    }
    else
    {
      uni <- MSM.UNISTATS(get(myresult), 
                          vars=myvars, 
                          stats=c('length', 'mean' , 'sd', 'skewness'), 
                          quantiles=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), 
                          output=resultname, output.append=append, 
                          output.suffix=output.suffix, 
                          output.sep=output.sep, 
                          group.name=byGroup, group.val=groupNo, 
                          doround=1)
      grfile <- paste(myresult, byGroup,  groupNo, sep="_")
    }
    
    print(paste("Univariate Statistics for", dslabel))
    print(uni, digits=2)
  }
  
  print(paste('== =====> end', date(), sep=" "))
}


# *****************************************************************************************************
# MSM ####
# This function runs the MSM_prep and MSM modules 1-3 functions
MSM  <- function(data_set, id, intake, model=1, FFQvar=NULL, P_cons=NULL, 
                 outputfile=NULL, verbose="yes", seed=123456789, precision=0.000001,
                 outBC="no",nboot=NULL, version_ck="new",label=NULL)
{
  print("INFO == Running MSM analysis using version 19Nov2009 ==")
  print("INFO Program parameters =======")
  print(paste("INFO == ID variable", id, sep = ": "))
  print(paste("INFO == Intake variable", intake,  sep = ": "))
  
  if (model == 1)
  {
    print(paste("INFO == Regression model:", intake, " = (intercept)", sep = " "))
  }
  else
  {
    print(paste("INFO == Regression model:", intake, " = ", model, sep = " "))
  }
  print("INFO Consumption frequency information")
  print(paste("INFO ==  consumption frequency variable", FFQvar,  sep = ": "))
  print(paste("INFO ==  consumption probability",  P_cons, sep = ": "))
  print("INFO Additional information")
  print(paste("INFO ==  output name ", label,  sep = ": "))
  
  print("=== STEP 0:  Data preparation ===")
  
  data_prep <- MSM_Prep(data_set, id, intake, model, FFQvar, P_cons, verbose)
  data_set <- data_prep$data
  Kmax <- data_prep$km
  cknew <- data_prep$cknew
  modelV <- data_prep$model
  modelO <- data_prep$modelO
  if (verbose == "debug")
  {
    print(paste(" Model parameters: ", modelO$model , sep=" "))
    print(paste('KM', data_prep$km ,  'NKP', data_prep$nkp, 'CKn',  data_prep$cknew))
  }
  
  print("=== STEP 1: Estimation of the habitual consumption amount on consumption days ===")
  m1Result <- MSM_M1(data_set, id, intake, modelO, FFQvar,  P_cons, verbose, seed, precision,Kmax, version_ck, cknew= data_prep$cknew)
  signum <- m1Result$params$bc$signum
  G_hab <- m1Result$data
  print("==> STEP 1 done ")
  
  print("=== STEP 2 : Estimation of the habitual consumption probability")
  C_hab <- MSM_M2(data_set, id, intake, "C", modelO, FFQvar, P_cons, verbose, seed, precision, Kmax, nkp=data_prep$nkp)
  print("==> STEP 2 done ")
  
  print("=== STEP 3 :  Estimation of the habitual consumption amounts for all days")
  output <- MSM_M3(data_set, id, intake, "C", G_hab, C_hab, modelO, m1Result$params, FFQvar, P_cons, verbose, seed, precision)
  print("==> STEP 3 done ")
  
  return(output)
}


# *****************************************************************************************************
## MSM_Prep ####
MSM_Prep <- function(data_set, id, response, model = 1, FFQvar = NULL, P_cons = NULL, verbose = "no") {
  if (verbose == "debug") {
    print(paste("DEBUG ", id, response, model, FFQvar, P_cons, verbose, sep = ":"))
  }
  signum <- NULL
  # parse model text for words = variables for the model and assign them to variables with modelx
  
  # [R]: trimm leading spaces that will interfere with strsplit function
  model <- sub("^\\s+", "", model, perl = TRUE) # Got the error: '\s' ist eine unbekannte Escape-Sequenz in der Zeichenkette beginnend mit "'^\s"
  # model <- trimws(model, "l") #AW
  # [R]: split on spaces between words and create vector of words
  modelV <- strsplit(model, "[ ,+]+")[[1]]
  # if we have no model = default 1 -> only an intercept model then set model Variables to NULL
  if (modelV[1] == 1) {
    modelV <- NULL
  }
  modelO <- NULL
  modelO$model <- model
  modelO$modelV <- modelV
  
  data_set$C <- ifelse(((data_set[, response] == 0) | is.na(data_set[, response])), 0, 1)
  data_set$K <- 1
  
  # HACK: to handle missing values in data set (in response: R sets them as NA
  # data_set$C <- ifelse( is.na(data_set$C)  , 0, data_set$C)
  
  # dataset aggregated by id using sum with columns id, K and C
  # !! new table has column name "id" not value in variable id
  kc_aggregate <- aggregate(data_set[, c("K", "C")], list(id = data_set[, id]), sum, na.rm = FALSE)
  # preserve original id name
  names(kc_aggregate)[1] <- id
  
  # min and max of summed recalls
  Kmin <- min(kc_aggregate[, "K"])
  Kmax <- max(kc_aggregate[, "K"])
  
  Cmax <- max(kc_aggregate[, "C"], na.rm = T)
  
  if (Cmax < 2) {
    signum <- 2
    stop("EE ERROR  --> no subjects with more than 1 positive intake in 24h-recalls, within person variance can not be estimated!")
  }
  
  P_consd <- 0
  t_assigned_consumer <- NULL
  if ((is.null(FFQvar)) || (length(FFQvar) == 0)) {
    if ((is.null(P_cons)) || (length(P_cons) == 0)) {
      P_consd <- 0.5
    } else {
      propTable <- as.data.frame(prop.table(table(kc_aggregate[, "C"], dnn = "SC")))
      if (propTable$SC[1] == 0) {
        P_consd <- max(0, (1 * P_cons - 1 + propTable$Freq[1]) / propTable$Freq[1])
      }
    }
    if (verbose == "debug") {
      print(paste("DEBUG P_consd ", P_consd))
    }
    if (P_consd != 0) {
      t_assigned_consumer <- MSM.sample(subset(kc_aggregate, C == 0)[, id], P_consd)
      # construct a table for merging if we have a return value: can be missing  P and sample suze are small
      if (length(t_assigned_consumer) > 0) {
        t_assigned_consumer <- as.data.frame(t_assigned_consumer)
        names(t_assigned_consumer) <- id
        t_assigned_consumer$P_consd <- P_consd
        # and merge with original dataset , adds a flag to ds named P_consd
        data_set <- merge(data_set, t_assigned_consumer, all = T)
      } else { # reset P_consd since there was no assignment
        P_consd <- 0
      }
    }
  }
  
  consumerFx <- function(x, ffqvar = NULL, P_consd = NULL) {
    if (!is.null(ffqvar)) {
      x[, "consumer"] <- ifelse(((x[, ffqvar] > 0) | ((x[, ffqvar] == 0) & (sum(x["C"]) > 0))), 1, 0)
    } else {
      if ((!is.null(P_consd)) && (P_consd != 0)) {
        x[, "consumer"] <- ifelse(((sum(x["C"]) > 0) | ((!is.na(x["P_consd"])) && (x["P_consd"] != 0))), 1, 0)
      } else {
        x[, "consumer"] <- ifelse(sum(x["C"]) > 0, 1, 0)
      }
    }
    return(x)
  }
  
  consumerFx1 <- function(x, ffqvar = NULL) {
    x[, "consumer"] <- ifelse(((x[, ffqvar] > 0) | ((x[, ffqvar] == 0) & (sum(x["C"]) > 0))), 1, 0)
    return(x)
  }
  
  consumerFx2 <- function(x, P_consd = NULL) {
    if ((!is.null(P_consd)) && (P_consd != 0)) {
      x[, "consumer"] <- ifelse(((sum(x["C"]) > 0) | ((!is.na(x["P_consd"])) && (x["P_consd"] != 0))), 1, 0)
    } else {
      x[, "consumer"] <- ifelse(sum(x["C"]) > 0, 1, 0)
    }
    return(x)
  }
  
  if ((!is.null(FFQvar)) && (FFQvar %in% names(data_set))) {
    data_set <- do.call("rbind", lapply(split(data_set, data_set[, id]), consumerFx1, FFQvar))
  } else {
    data_set <- do.call("rbind", lapply(split(data_set, data_set[, id]), consumerFx2, P_consd))
  }
  
  nn <- nrow(subset(data_set, C == 1))
  
  cs <- subset(data_set, C == 1)
  c1_aggregate <- aggregate(cs[, "C"], list(id = cs[, id]), sum, na.rm = FALSE)
  # set names
  names(c1_aggregate) <- c(id, "C")
  c1_aggregate$C2 <- c1_aggregate$C * c1_aggregate$C
  kk <- nrow(c1_aggregate)
  
  nn2 <- sum(c1_aggregate$C2)
  
  nmean <- mean(c1_aggregate$C)
  
  cknew <- (1 - (nn2 - 2 * nmean * nn + nmean * nmean * kk) / (kk - 1) / nn / nmean)
  
  nn <- nrow(subset(data_set, consumer > 0))
  
  cs <- subset(data_set, consumer > 0)
  c1_aggregate <- aggregate(cs$consumer, list(id = cs[, id]), sum, na.rm = FALSE)
  names(c1_aggregate) <- c(id, "consumer")
  
  c1_aggregate$C2 <- c1_aggregate$consumer^2
  kk <- nrow(c1_aggregate)
  
  nn2 <- sum(c1_aggregate$C2)
  
  nkp <- (nn^2 - nn2) / (kk - 1) / nn
  
  return(list(data = data_set, model = modelV, km = Kmax, modelO = modelO, cknew = cknew, nkp = nkp))
}


MSM.sample <- function(N, p) {
  s <- NULL
  # special behavior if length(N) = 1 -> then sample samples from 1:N
  if (length(N) == 1) {
    # adapt to SAS PROC surveyselect m=SRS -> samples N if p > 0
    if (p > 0) {
      s <- N
    }
  } else {
    n <- length(N) * p
    if (n != 0) {
      s <- sample(N, n)
    }
    # else { s <- NULL}
  }
  return(s)
}


MSM_M1 <- function(data_set, id, intake, modelO, FFQvar=NULL,  P_cons=NULL,verbose="yes", seed=123456789, precision=0.000001, Kmax,  version_ck="new", cknew=NULL)
{
  # # TODO: check for existence of variables id, var, and those from model, in dataset
  
  #
  #/*-----------------------------------------------------------------------------------------*/
  #/*--------------- Modul 1: Schaetzung der habituellen Aufnahmemengen an Konsumtagen --------*/
  #/*---------------------- Group39-Daten der 2. Potsdamer Kalibrierungsstudie ---------------*/
  #/*-----------------------------------------------------------------------------------------*/
  # 	options nonotes nomprint nomtrace nosymbolgen
  
  # NOW done before M1 method call in a seperate Prep function : UH 19Aug09
  # ########## Preparations ##################
  
  modelV <- modelO$modelV
  model <- modelO$model
  if (verbose == "debug")
  {
    print(paste("M1: Model parameters: response:", intake, " model:", model ))
  }
  
  # ############### END preparations ###################
  # print(paste("M1:cknew", version_ckcknew))
  ########### A: calculate residuals and transform ###########
  # 	/*A. Bestimmung von Residuen und Residuentransformation*/
  
  # prepare model formula for reg
  
  lmFormula <- paste(intake, '~', model, sep=' ')
  if (verbose != "no")
  {
    print(paste( "Linear regression (lmFormula)", lmFormula, sep=' '))
  }
  # create subset for consumer data only
  gkonsum <- subset(data_set, C == 1)
  
  myLM <- lm(lmFormula, data=gkonsum, subset=C==1, x = TRUE, y = TRUE)
  
  #	 store coefficients for later use: as  named (?) vector: access example myLMCoeffs("SEX") or myLMCoeffs[1]
  # can be uses as such in calculations
  # return regression values with result, need in M3
  M1RegCoeffs <- coefficients(myLM)
  # or as data frame
  
  # get predicted values :
  # SOLUTION: use merge
  regRes <- as.data.frame( fitted(myLM))
  names(regRes) <- c("P")
  regRes$R <- residuals(myLM)
  gkonsum <- merge(gkonsum, regRes, by="row.names", all=T)
  
  if (verbose != "no")
  {
    # output model stats
    print("Linear Regression: Summary")
    print(summary(myLM))
  }
  Rmin <-  min(gkonsum[, "R"], na.rm=T )
  gkonsum$Rm <- gkonsum$R-Rmin+0.01
  print("==> start BoxCox ")
  bc_result <- MSM_BC(gkonsum, "Rm",  precision)
  gkonsum <- bc_result$data
  bc_params <- bc_result$pars
  print("==> end BoxCox ")
  if (verbose  == "debug")
  {
    print("DEBUG bc_params")
    print(bc_params)
  }
  
  ########### B: create individual means ###########
  
  G_hab <- aggregate(gkonsum[, c(intake, "P", modelV ) ],  list(id=gkonsum[,id]), mean, na.rm=T)
  G_hab$BCR <- tapply(gkonsum$BC_Rm, list(id=gkonsum[,id]), mean, na.rm=TRUE)
  G_hab$VBCR <- tapply(gkonsum$BC_Rm, list(id=gkonsum[,id]), var, na.rm=TRUE, use="pairwise.complete.obs")
  G_hab$days <- tapply(gkonsum$C, list(id=gkonsum[,id]), sum, na.rm=TRUE)
  # rename id column name to preserve original value
  names(G_hab)[1] <- id
  
  ########### C: calculate means and variances from individual means ###########
  
  pars <- NULL
  BCRM <- mean(G_hab$BCR, na.rm=TRUE)
  VBCRM <- mean(G_hab$VBCR, na.rm=TRUE) #VBCRM is average intraindividual variance
  daysM <- mean(G_hab$days)
  BCRV  <- var(G_hab$BCR, na.rm=TRUE) # variance of the individual means
  daysV <- var(G_hab$days)
  pars$BCRM <- BCRM
  pars$VBCRM <- VBCRM
  pars$daysM <- daysM
  pars$BCRV  <- BCRV
  pars$daysV <- daysV
  
  sa2 <- BCRV-VBCRM/daysM
  VBCRMa <- VBCRM
  
  ck <- 1-daysV/3+(daysM-1.5)*daysV/4 #/*correction factor for unbalancendness*/
  if (sa2 < 0 )
  {
    print("WW WARNING  ===> negative variance at the estimation of usual intake on consumption days!")
    sa2 <- BCRV/daysM^8
    VBCRMa <- (BCRV-sa2)*daysM
  }
  kappa <- sqrt(sa2/BCRV)
  
  VarRatio <- VBCRMa /sa2
  
  pars$sa2 <- sa2
  pars$VBCRMa <- VBCRMa
  pars$cknew <- cknew
  pars$ckold <- ck
  pars$kappa <- kappa
  pars$Rmin <- Rmin
  pars$VarRatio <- VarRatio
  # we return pars at the end of function
  
  if (version_ck == 'new') {ck <- pars$cknew } else {ck <- pars$ckold}
  pars$ck <- ck
  
  print(paste("II   ck (", version_ck, ")", ck))
  print(paste("II   within person variance :", VBCRMa))
  print(paste("II   between person variance:", sa2))
  print(paste("II   ratio within person variance / between person variance:", VarRatio))
  
  ########### D: Backtransformation and Description ###########
  
  G_hab$BCRk = kappa*(G_hab$BCR - BCRM) + BCRM
  
  if (bc_params$ii == 0)
  {
    G_hab["G_hab"] <- G_hab$P + exp(G_hab$BCRk + VBCRMa/2) + Rmin - 0.01 - bc_params$Wnew
    G_hab["G_habc"] <- G_hab$P + exp(G_hab$BCRk + ck*VBCRMa/2) + Rmin - 0.01 - bc_params$Wnew
  }
  else
  {
    G_hab$sum <- 0
    G_hab$sum1<- 0
    
    for (k in 0:trunc(bc_params$ii/2) )
    {
      G_hab$sum1 <- G_hab$sum1 + choose(bc_params$ii,2*k)* (G_hab$BCRk+bc_params$ii)^(bc_params$ii-2*k)*VBCRMa^k*factorial(2*k)/2^k/factorial(k)
      G_hab$sum <- G_hab$sum + choose(bc_params$ii,2*k)* (G_hab$BCRk+bc_params$ii)^(bc_params$ii-2*k)*(ck*VBCRMa)^k*factorial(2*k)/2^k/factorial(k)
    }
    G_hab["G_hab"] <- G_hab$P + bc_params$ii^(-bc_params$ii)*G_hab$sum1 + Rmin - 0.01 - bc_params$Wnew
    G_hab["G_habc"] <- G_hab$P + bc_params$ii^(-bc_params$ii)*G_hab$sum + Rmin - 0.01 - bc_params$Wnew
  }
  
  ########### E: Table for variances ###########
  
  ########### F: Return ###########
  return(list(data=G_hab,params=list(m1=pars,bc=bc_params,m1lm=M1RegCoeffs)))
}

MSM_BC <- function(data_set, vars, prec, outBoxCox="no", wend=20)
{
  # library e1071 needed for calculation of skewness and kurtosis
  # NEEDS to be installed sytem wide !!!!!!!!
  require(e1071)
  #######DC 	require(moments)
  #	%global signum ii Wnew
  # local assignments, the vars get exported upon return
  signum <- NULL
  ii <- NULL
  Wnew <- NULL
  
  dsMean <- mean(data_set[,vars], na.rm=T) # FALSE underscore variable names are problematic in R, use ds prefix instead
  
  tBoxCox <- NULL
  for (lambda in 0:10)
  {
    for (tw in 0:wend)
    {
      gHelp <- NULL
      tHelp <- data_set
      if (lambda == 0 )
      {
        if (tw == 0)
        {
          tHelp$tBoxCox <- log(tHelp[,vars])
        }
        else
        {
          if (tw<=10)
          {
            tHelp$tBoxCox <- log(tHelp[,vars]+dsMean/(tw))
          }
          else
          {
            tHelp$tBoxCox <- log(tHelp[,vars]+dsMean*(tw-9))
          }
        }
      }
      else
      {
        if (tw == 0)
        {
          tHelp$tBoxCox <- ((tHelp[,vars])^(1/lambda)-1)/(1/lambda)
        }
        else
        {
          if (tw<=10)
          {
            tHelp$tBoxCox <- ((tHelp[,vars]+dsMean/(tw))^(1/lambda)-1)/(1/lambda)
          }
          else
          {
            tHelp$tBoxCox <- ((tHelp[,vars]+dsMean*(tw-9))^(1/lambda)-1)/(1/lambda)
          }
        }
      }
      if (lambda == 0 )
      {
        gHelp$Lambda <- 0
        if (tw == 0)
        {
          gHelp$W <- 0
        }
        else
        {
          if (tw <= 10)
          {
            gHelp$W <- dsMean/tw
          }
          else
          {
            gHelp$W <- dsMean*(tw-9)
          }
        }
      }
      else
      {
        gHelp$Lambda <- 1/lambda
        if (tw == 0)
        {
          gHelp$W <- 0
        }
        else
        {
          if (tw <= 10)
          {
            gHelp$W <-  dsMean/tw
          }
          else
          {
            gHelp$W <- dsMean*(tw-9)
          }
        }
      }
      # skewness kurtosis from package "e1017"
      gHelp$skewness <- skewness(tHelp[,"tBoxCox"], na.rm=T)
      #####DC    gHelp$kurtosis <- kurtosis(tHelp[ , "tBoxCox"], na.rm=T)
      #tHelp$probn <- probn........??
      # test of normality from package "stats"
      # !!! this test has a limitation to max 5000 datapoints
      # need to sample or use other testtHelp[, "tBoxCox"]
      if (length(tHelp[, "tBoxCox"]) > 2000 )
      {
        gHelp$probn <- ks.test(tHelp[, "tBoxCox"], pnorm, mean(tHelp[, "tBoxCox"], na.rm=TRUE), sd(tHelp[, "tBoxCox"], na.rm=TRUE)  )$p.value
      }
      else
      {
        gHelp$probn <- shapiro.test(tHelp[, "tBoxCox"])$p.value
      }
      tBoxCox <- rbind(tBoxCox, data.frame(gHelp))
    }
  }
  
  tBoxCox <- tBoxCox[ do.call(order, tBoxCox), ]
  #R: iterate through each row of tBoxCox and find max values
  # R: iteration through dataset
  for (ix in 1:length(tBoxCox[,1]))
  {  		# crude way to get number of rows in table
    currentrow <- tBoxCox[ix,]
    if (ix == 1 )
    {
      sig <- sign( currentrow$skewness)
      l1 <- currentrow$Lambda
      w1 <- currentrow$W
      s1 <- currentrow$skewness
      p1 <- currentrow$probn
      maxP <- p1+abs(s1)*(currentrow$probn-p1)/(currentrow$skewness-s1)
      maxL <- NA
      maxW1 <- NA
      maxW2 <- NA
      maxS1 <- NA
      maxS2 <- NA
    }
    else
    {
      if ((l1 == currentrow$Lambda) && (s1*currentrow$skewness < 0 ))
      {
        sig <- 2
        if ( is.na(maxP)  || ( (p1 + abs(s1)*(currentrow$probn-p1)/(currentrow$skewness-s1) ) > maxP ) )
        {
          maxP <- p1+abs(s1)*(currentrow$probn-p1)/(currentrow$skewness-s1)
          maxL <- currentrow$Lambda
          maxW1 <- w1
          maxW2 <- currentrow$W
          maxS1 <- s1
          maxS2 <- currentrow$skewness
        }
      }
      l1 <- currentrow$Lambda
      w1 <- currentrow$W
      s1 <- currentrow$skewness
      p1 <- currentrow$probn
    }
    if (ix == length(tBoxCox[,1]))
    {
      signum <- sig
      maxL <-  maxL
      maxW1 <- maxW1
      maxW2 <- maxW2
      maxS1 <- maxS1
      maxS2 <- maxS2
    }
  } # end iteration through dataset
  
  
  if ( signum == 0 || signum == 1)
  {
    print(paste("WW     NOTE: only positive values for skewness of variable" , vars, "during Box-Cox-transformation!"))
    print(paste("WW     NOTE: back transformation carried out with minimal skewness"))
    
    tBoxCox <- tBoxCox[order(tBoxCox["skewness"],tBoxCox["W"]),]
    
    if ( tBoxCox[1, "Lambda"] == 0)
    {
      ii <-  tBoxCox[1, "Lambda"]
    }
    else
    {
      ii <- round(1/ tBoxCox[1, "Lambda"])
    }
    Wnew <- tBoxCox[1, "W"]
    skew <- tBoxCox[1, "skewness"]
    maxL <- tBoxCox[1, "Lambda"]
    
    print(paste("==  ===>  Lambda=", ii, "W=", Wnew, "Skewness=", skew))
    
    if ( ( is.na(maxL) )  || (maxL == 0) )
    {
      if ( is.na(Wnew)  )
      {
        data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars])
      }
      else
      {
        data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars]+Wnew)
      }
    }
    else
    {
      data_set[, paste("BC_", vars, sep="")] <- (exp(maxL*log(data_set[,vars]+Wnew))-1)/maxL
    }
  }
  else
  {
    if (signum == -1)
    {
      print(paste("WW     NOTE:  only negative values for skewness of variable" , vars , "during Box-Cox-transformation!"))
      print(paste("WW     NOTE:  continue calculation with parameters where skewness is closest to zero"))
      
      tBoxCox <- tBoxCox[order(tBoxCox["skewness"],tBoxCox["W"], decreasing=T),]
      
      if ( tBoxCox[1, "Lambda"] == 0)
      {
        ii <-  tBoxCox[1, "Lambda"]
      }
      else
      {
        ii <- round(1/ tBoxCox[1, "Lambda"])
      }
      
      Wnew <- tBoxCox[1, "W"]
      skew <- tBoxCox[1, "skewness"]
      maxL <- tBoxCox[1, "Lambda"]
      
      print(paste("    ===>  Lambda=", ii, "W=", Wnew, "Skewness=", skew))
      
      if ( ( is.na(maxL) )  || (maxL == 0) )
      {
        if ( is.na(Wnew)  )
        {
          data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars])
        }
        else
        {
          data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars]+Wnew)
        }
      }
      else
      {
        data_set[, paste("BC_", vars, sep="")] <- (exp(maxL*log(data_set[,vars]+Wnew))-1)/maxL
      }
    }
    else
    {
      ghelp <- NULL
      expr <- 1
      ti <- 0
      
      while (expr > 0)
      {
        uv <- NULL
        ti <- ti + 1
        #R: set Wnew
        Wnew <- maxW1 + maxS1*(maxW2-maxW1)/(maxS1-maxS2)
        tHelp <-  data_set
        
        if ( (is.na(maxL) )   || (maxL == 0) )
        {
          if ( is.na(Wnew)  )
          {
            tHelp$BoxCox=log(tHelp[,vars])
          }
          else
          {
            tHelp$BoxCox=log(tHelp[,vars]+Wnew)
          }
        }
        else
        {
          tHelp$BoxCox = (exp(maxL*log(tHelp[,vars]+Wnew))-1)/maxL
        }
        
        myskewness <- skewness(tHelp[, "BoxCox"], na.rm=T)   #######DC , added before "BoxCox"
        #####DC     mykurtosis <- kurtosis(tHelp[, "BoxCox"], na.rm=T)
        # test of normality from package "stats"
        #tHelp$probn <- shapiro.test(tHelp[,"BoxCox"])$p.value #gHelp <- NULL
        # !!! this test has a limitation to max 5000 datapoints
        # need to sample or use other testtHelp[, "tBoxCox"]
        
        if (length(tHelp[, "BoxCox"]) > 2000 )
        {
          myprobn <- ks.test(tHelp[, "BoxCox"], pnorm,  mean(tHelp[, "BoxCox"], na.rm=TRUE), sd(tHelp[, "BoxCox"], na.rm=TRUE)  )$p.value
        }
        else
        {
          myprobn <- shapiro.test(tHelp[, "BoxCox"])$p.value
        }
        if (myskewness < 0 )
        {
          maxS1 <- myskewness
          maxW1 <- Wnew
        }
        else
        {
          maxS2 <- myskewness
          maxW2 <- Wnew
        }
        expr <- sign(abs(myskewness)-prec)
        skew <- abs(myskewness)
        if ( ( is.na(maxL) )  || (maxL == 0) )
        {
          ii <- 0
        }
        else
        {
          ii <- round(1/maxL)
        }
        uv$expr <- expr
        uv$ti <- ti
        uv$skewness <- myskewness
        #####DC   uv$kurtosis <- mykurtosis
        uv$probn <- myprobn
        uv$maxL <- maxL
        uv$ii <- ii
        uv$Wnew <- Wnew
        ghelp <- rbind(ghelp, data.frame(uv))
      } # while
      
      print(paste("     ===> Iterations:", ti,  "Lambda=", ii, "W=", Wnew, "Skewness=", skew))
      
      if ( ( is.na(maxL) )  || (maxL == 0) )
      {
        if ( is.na(Wnew)  )
        {
          data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars])
        }
        else
        {
          data_set[, paste("BC_", vars, sep="")] <- log(data_set[,vars]+Wnew)
        }
      }
      else
      {
        data_set[, paste("BC_", vars, sep="")] <- (exp(maxL*log(data_set[,vars]+Wnew))-1)/maxL
      }
    }
  }
  
  #R: return value
  #return(data_set)
  BC_parameters <- (list(signum=signum,maxL=maxL,maxW1=maxW1,maxW2=maxW2,maxS1=maxS1,Wnew=Wnew,ii=ii))
  return(list(data=data_set,pars=BC_parameters)) #, model=modelV, km=Kmax))
}


# *****************************************************************************************************
MSM_M2 <- function(data_set, id, intake, consum, modelO, FFQvar=NULL, P_cons=NULL, verbose="yes", seed=123456789, precision=0.000001, Kmax, nkp=NULL)
{
  # 	/*------------------------------------------------------------------------------------------*/
  #/*--------------- Module 2 Schaetzung der habituellen Konsumptionswahrscheinlichkeit -----*/
  #/*---------------------- Group39-Daten der 2. Potsdamer Kalibrierungsstudie ---------------*/
  #/*-----------------------------------------------------------------------------------------*/

  # ########## Preparations ##################
  
  modelV <- modelO$modelV
  model <- modelO$model
  if (verbose == "debug")
  {
    print(paste("M2: Model parameters: response:", intake, " model:", model ))
  }
  
  ########### A: Calculate residuals and residual transformation ###########
  # __H1 = original dataset with consumer / consumption indicators = dataC from M1 = data_set here
  
  # Hack fro handlign all consumer
  allconsumer <- 0
  minC <- min(data_set$C, na.rm=T)
  C_hab <- subset(data_set,  consumer > 0)
  
  #TODO: skip this message if we use log transformed vars !!!!!!!!
  if (minC == 0)
  {
    if (verbose != "no")
    {
      print(paste("logistic regression model for probability of consumption of ", intake))
      print("==           --> some non-consumers!")
    }
    formula <- paste(consum, "~", model, sep=" ")
    myGLM <- glm(formula, family=binomial(logit), data=C_hab)
    myGLMCoeffs <- coefficients(myGLM)
    if (verbose != "no")
    {
      # output model stats
      print(paste("Logistic regression model: ", formula))
      print("Linear Regression: Summary")
      print(summary(myGLM))
    }
    # get predicted values: Problem with missings
    # #C_hab$P <- fitted(myGLM)
    regRes <- as.data.frame( fitted(myGLM))
    names(regRes) <- c("P")
    C_hab <- merge(C_hab, regRes, by="row.names", all=T)
  }
  else
  {
    if (verbose != "no")
    {
      print("           --> only consumers!")
    }
    
    allconsumer <- 1
    C_hab$P <- 1
    
  }
  # stop ods output
  
  # add residual data
  C_hab$R <- C_hab$C - C_hab$P
  # add transformed R: fR
  C_hab$fR <- log((C_hab$R+1)/(1-C_hab$R))
  
  ########### B: Create individual means ###########
  
  # VfR: intra-individual variance for each person
  KLEIN <- aggregate(C_hab[,c("fR", consum, "P", modelV )],  list(id=C_hab[,id]), mean, na.rm=T)
  KLEIN$VfR <- tapply(C_hab$fR,  list(id=C_hab[,id]), var, na.rm=TRUE)
  names(KLEIN)[1] <- id
  
  ########### C: Estimate mean and variances from KLEIN of individual means ###########
  
  fRM <- mean(KLEIN[,c("fR")], na.rm=T)
  VfRM <- mean(KLEIN[,c("VfR")], na.rm=T)
  fRV <- var(KLEIN[,c("fR")], na.rm=T)
  
  rho <- NULL
  kappa <-NULL
  
  if (! is.null(nkp) )
  {
    Kmax <- nkp
  }
  
  if (verbose != "no" )
  {
    print(paste("M2: C: Estimate mean and variances of individual means: fRM: ", fRM,
                " VfRM: ",VfRM, "fRV: ", fRV , " Kmax: ", Kmax))
  }
  if ( minC == 0 )
  {
    # rho is the Intraclass correction
    rho <- 1-VfRM/(fRV+VfRM/Kmax)  # TODO better result passing or combine all in one
    if (fRV > VfRM/Kmax)
    {
      kappa <- sqrt((fRV - VfRM/Kmax)/fRV)
    }
    else
    {
      print(paste("KAPPA rho", rho))
      kappa <- rho*sqrt(1/(1+ rho^2))
    }
  }
  
  negVar <- ifelse( fRV > VfRM/Kmax, 1, 0)
  
  if ( (negVar == 0) && ( minC == 0 ) )
  {
    print("WARNING:   --> negative variance at the estimation of probability of consumption!")
  }
  # needed for dignostic only
  pars <- NULL
  pars$fRM <- fRM
  pars$VfRM <- VfRM
  pars$fRV <- fRV
  pars$rho <- rho
  pars$kappa <- kappa
  # only as diagnostic output
  
  ########### D: Backtransformation and description ###########
  
  if (verbose == "debug")
  {
    print("DEBUG: D: Backtransformation and description")
  }

  C_hab <- KLEIN
  # hack to avoid problems wit allconsumers
  if (allconsumer)
  {
    C_hab$C_hab <- 1
  }
  else
  {
    C_hab$fRk <- kappa*(C_hab$fR-fRM) + fRM
    C_hab$thab <- C_hab$P+(exp(C_hab$fRk)-1)/(exp(C_hab$fRk)+1)-(exp(fRM*sqrt(1-rho))-1)/(exp(fRM*sqrt(1-rho))+1)
    C_hab$C_hab <- ifelse(ifelse(C_hab$thab > 1, 1, C_hab$thab) > 0,ifelse(C_hab$thab > 1, 1, C_hab$thab), 0)
  }
  # prepare model formula for reg
  lmFormula <- paste("C_hab", "~", model, sep=" ")
  # regression
  myLM <- lm(lmFormula, data=C_hab )
  # get predicted values
  regRes <- as.data.frame( fitted(myLM))
  names(regRes) <- c("PC_hab")
  C_hab <- merge(C_hab, regRes, by="row.names", all=T)
  C_hab$Row.names <- NULL
  
  # prepare model formula for reg
  lmFormula <- paste(consum, "~", model, sep=" ")
  myLM <- lm(lmFormula, data=C_hab )
  
  regRes <- as.data.frame( fitted(myLM))
  names(regRes) <- c("PC")
  C_hab <- merge(C_hab, regRes, by="row.names", all=T)
  
  C_hab$thab <- NULL # reomve column from table
  C_hab$Row.names <- NULL
  
  ########### D: return ###########
  
  return(C_hab)
}


MSM_M3 <- function(data_set, id,  intake, consum="C", G_hab, C_hab, modelO, params, FFQvar=NULL, P_cons=NULL, verbose="no", seed=123456789, precision=0.000001)
{
  
  ########## Preparations ##################
  modelV <- modelO$modelV
  model <- modelO$model
  print(paste("M3",modelV, model, params))
  BCParameter <- params$bc
  M1Parameter <- params$m1
  M1RegCoeffs <- params$m1lm
  
  if (is.null(modelV))
  {
    tTMP <- aggregate(data_set[,c("consumer")],  list(id=data_set[,id]), mean, na.rm=T)
    # HACK  to set a proper column name
    names(tTMP)[2] <- "consumer"
  }
  else
  {
    tTMP <- aggregate(data_set[,c( "consumer",  modelV)],  list(id=data_set[,id]), mean, na.rm=T)
  }
  
  # preserve original id name
  names(tTMP)[1] <- id
  tGhab <- G_hab[, c(id, intake, "G_habc", "P")]
  tChab <- C_hab[, c(id, consum, "C_hab")]
  tTMP <- merge(tTMP, tGhab, by=id, all=TRUE)
  tTMP <- merge(tTMP, tChab, by=id, all=TRUE)
  
  # helper function to calculate P
  regModelPFx <- function(x, regCoeffs)
  {
    regCoeffs["(Intercept)"]+sum(x*regCoeffs[names(x)], na.rm=T)
  }
  
  set.seed(seed) # define the seed -> reproduces same result starting from here
  tTMP["BCRk"] <- NA # initiate helper var for backtransformation
  
  c0Fx <- function(x,response,c1 )
  {
    if (is.na(x[response]))
    {
      return(x)
    }
    if (x[response] == 0 )
    {
      x[c1] <- 0
      x["BCRk"] <- M1Parameter$BCRM+sqrt(M1Parameter$sa2)*rnorm(1)
      if (BCParameter$ii == 0)
      {
        x["G_habc"] <- x$P + exp(x$BCRk + M1Parameter$ck*M1Parameter$VBCRMa/2) + M1Parameter$Rmin - 0.01 - BCParameter$Wnew
      }
      else
      {
        x$sum<- 0
        for (k in 0:trunc(BCParameter$ii/2) )
        {
          x$sum <- x$sum + choose(BCParameter$ii,2*k)*
            (x$BCRk+BCParameter$ii)^(BCParameter$ii-2*k)*
            (M1Parameter$ck*M1Parameter$VBCRMa)^k*factorial(2*k)/2^k/factorial(k)
        }
        x["G_habc"] <- x$P + BCParameter$ii^(-BCParameter$ii)*x$sum +
          M1Parameter$Rmin - 0.01 - BCParameter$Wnew
      }
      x["G_habc"] <- ifelse(x["G_habc"] > 0, x["G_habc"], 0)
      x$sum <- NULL
    }
    return(x)
  }
  
  print(modelV)
  print(M1RegCoeffs)
  
  tTMP["P"] <- apply(tTMP[modelV], 1, regModelPFx, M1RegCoeffs)
  
  print("check each row for C = response")
  # check each row for "C" = response:  new procedure with implicit backtransform function
  tTMP <- do.call("rbind", lapply(split(tTMP, tTMP[, id]), c0Fx, "C", intake ))
  
  ######## B: Product of consumption amount and consumption probability ##################
  
  print("B: Product of consumption amount and consumption probability")
  
  tTMP$G_usual <- ifelse(tTMP[, "consumer"] == 1 , tTMP$C_hab*tTMP$G_habc, 0)
  tTMP$total_gr <- ifelse(tTMP[, "consumer"] == 1 , tTMP$C*tTMP[, intake], 0)
  
  total_hab <- tTMP
  
  regFormula <- paste("G_usual", '~', model, sep=' ')
  
  myLM <- lm(regFormula, data=total_hab)
  
  regRes <- as.data.frame( fitted(myLM))
  names(regRes) <- c("PG_usual")
  total_hab <- merge( total_hab, regRes, by="row.names", all=T)
  total_hab$Row.names <- NULL
  
  regFormula <- paste("total_gr", '~', model, sep=' ')
  
  myLM <- lm(regFormula, data=total_hab)
  # get predicted values
  regRes <- as.data.frame( fitted(myLM))
  names(regRes) <- c("Ptotal")
  total_hab <- merge( total_hab, regRes, by="row.names", all=T)
  total_hab$Row.names <- NULL
  
  if (verbose == "debug")
  {
    # output model stats
    print(paste("M3 B: regression model: ", regFormula))
    print("Linear Model: Summary")
    print(summary(myLM))
  }
  
  # BAD HACK to get proper output ds below
  kc_aggregate <- aggregate(data_set[,c("K", "C")],  list(id=data_set[, id]), sum, na.rm=FALSE)
  names(kc_aggregate)[1] <- id
  Kmin <- min(kc_aggregate[,"K"])
  Kmax <- max(kc_aggregate[,"K"])
  
  if (Kmin < Kmax)
  {
    w <- vector("numeric",length=Kmax)
    z <-  data.frame(0, t(w))
    # transpose fx
    transpFx <- function(x,c1)
    {
      cols <- c(names(x)[1],paste(c(c1), 1:Kmax, sep=""))
      # payload data in 2.row
      y <- t(x[2])
      nc <- ncol(y)+1
      z[1,1:nc] <- c(x[1,1], y)
      names(z) <- cols
      return(z)
    }
  }
  else
  {
    transpFx <- function(x,c1)
    {
      cols <- c(names(x)[1],paste(c(c1), 1:Kmax, sep=""))
      # payload data in 2.row
      y <- t(x[2])
      z <- data.frame(x[1,1], y)
      names(z) <- cols
      return(z)
    }
  }
  
  consumT <- do.call( "rbind", lapply(split(data_set[, c(id, intake)], data_set[, id]), transpFx, "day"))
  
  responseT <- do.call( "rbind", lapply(split(data_set[, c(id, consum)], data_set[, id]), transpFx, "C"))
  
  transposeT <- merge(consumT, responseT, by=id, all=TRUE)
  
  output <-  merge(transposeT, total_hab[ c(id, "consumer")], by=id, all=TRUE)
  output <-  merge(output, G_hab[c(id, intake)], by=id, all=TRUE)
  output <-  merge(output, total_hab[c(id, "total_gr")], by=id, all=TRUE)
  output <-  merge(output, C_hab[c(id, "C_hab")], by=id, all=TRUE)
  output <-  merge(output, G_hab[c(id,   "G_habc")], by=id, all=TRUE)
  output <-  merge(output, total_hab[c(id, "G_usual")], by=id, all=TRUE)
  
  # rename columns: custom function
  col.rename <- function(ds,c1)
  {
    for (ix in 1:nrow(c1))
    {
      for (i in 1:ncol(ds))
      {
        if ( (c1[ix,1]) == names(ds)[i])
        {
          names(ds)[i] <- c1[ix,2]
        }
      }
    }
    return(ds)
  }
  
  # ugly hack for  renaming columns
  output <- col.rename(output, cbind(
    c(intake,"G_habc", "total_gr", "G_usual", "C_hab" ),
    c(	paste(intake,"_c_m",sep=""),paste(intake,"_c_usual",sep=""),
       paste(intake, "_all_m", sep=""), paste(intake,"_all_usual",sep=""),
       paste("P_", intake,sep="")
    )
  ))
  
  ########## Cleanup ##################
  return(output)
}


# *****************************************************************************************************
# MSM.UNISTATS ####
MSM.UNISTATS <- function(data_set, vars, stats = c("length", "mean", "sd"),
                         quantiles = FALSE, output = NULL, output.path = "",
                         output.suffix = ".txt", output.append = "FALSE",
                         output.sep = "\t", group.name = NULL, group.val = NULL,
                         doround = NULL) {
  # needed for calculation of skewness and kurtosis
  require(e1071)
  if (typeof(quantiles) == "logical") {
    if (quantiles == TRUE) {
      quantiles <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
    } else {
      quantiles <- c(0)
    }
  }
  # prepapre col header for quantiles
  quants <- if (length(quantiles) > 1) {
    paste(100 * quantiles, "%", sep = "")
  }
  
  mystats <- c(stats, quants)
  if (!is.null(group.name)) {
    mystats <- c(group.name, mystats)
  }
  
  mytable <- data.frame(row.names = vars)
  mytable[, mystats] <- 0
  
  # get number of rows
  if ("length" %in% colnames(mytable)) {
    colnames(mytable)[match("length", colnames(mytable))] <- "N"
    mytable[, "N"] <- length(data_set[, vars[1]])
    stats <- stats[!(stats == "length")] # remove "@length" value form list
  }
  
  if (!is.null(group.name)) {
    mytable[, group.name] <- group.val
  }
  
  mytable[, quants] <- t(apply(as.matrix(data_set[, vars]), 2, quantile, probs = quantiles, na.rm = TRUE, names = FALSE, type = 2))
  
  # foreach stat element get value: ATTN! will fail if stat element is not a valid function
  for (f in stats)
  {
    mytable[, f] <- apply(data_set[, vars], 2, f, na.rm = TRUE)
  }
  
  # if a rounding parameter is specifeid, then round values to the 'doround'th digit
  exclude_from_round <- c("skewness")  ######DC , "kurtosis" removed
  
  if (!is.null(doround) && doround >= 0) {
    for (f in exclude_from_round)
    {
      stats <- stats[!(stats == f)] # remove "@length" value form list
    }
    for (f in stats)
    {
      mytable[, f] <- round(mytable[, f], doround)
    }
    mytable[, quants] <- round(mytable[, quants], doround)
  }
  return(mytable)
}

# temporary directory for storing file output
TMPDIR <- "Temp/"

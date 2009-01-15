### Module for ols/gaussian linear models:
### ols: normal linear regression, driver ols()
### ols.functional: same as ols, but uses glm( and slower); driver ols.functional()
### Contains following functions:
### covdel(xmat, tol=tol) eliminates colinearities of xmat for tolerance=tol
### eliminate.colinear(xw,nmx, delta.tol, tol) call covdel recursively
### inverse.trans(yin,yout,y,whotransform) inverse to trans (make.mortality.data)
### ols() calls covdel,eliminate.colinear and inverse.trans
### ols.functional() calls covdel, glm.colinear and inverse.trans
### glm.colinear(xw, yw, nmx, delta.tol, tol) uses glm (built in function)
### and checks for colinearities with covdel 
###

## ************************************************************************
##
## FUNCTION NAME: inverse.trans
##
## INPUT:     yin, yout, whoinsampy, mortality matrices (1 col) for csid 
##            whotransform integer that defines transformation apply to mortality 
##
## DESCRIPTION: inputs whoinsampy and whoutsampy (1col), matrices for linear models, 
##              are functions of mortality = dth/population,depend on whotransform;   
##              outputs are always predicted yhatin, yhatout as log(mortality)  
##              Calculate the transformation to the input functions to get predictions
##           
## IMPORTED functions: none (refer to function trans in make.mortality.data) 
##
## OUTPUT:    yhatin, yhatout,and y (1cols) matrices for csid as log(mortality)
##            predicted for years of insample and outsample
##           
##  WRITTEN BY: Elena Villalon
##              evillalon@latte.harvard.edu
##              CBRSS, Harvard University
## 
## Last modified: 05/21/2003
## 
## ************************************************************************
## ************************************************************************

    inverse.trans <- function(yin,yout,y, whotransform=NULL, ebase) {
      ebase <- get("env.base", env=parent.frame())
      env.base <- ebase
      ewho <- get("env.who",env=ebase) 
      if(length(whotransform) <= 0)
        whotransform <- get("whotransform", env =ewho)
    if (whotransform == 1){
### retp <- log(dthvec)  
        yhatin  <- yin
        yhatout <- yout
    }else if (whotransform == 2){
### retp <- sqrt(dthvec)      
        yhatin  <- log(yin^2)
        yhatout <- log(yout^2)
        y <- log(y^2)
    }else if (whotransform == 3){
###  retp <- log(dthvec + 1)
        yhatin  <- log(exp(yin) - 1)
        yhatout <- log(exp(yout) - 1)
        y <- log(exp(y) - 1)
    }else{
### retp <- dthvec
        yhatin   <- log(yin)
        yhatout  <- log(yout)
        y <- log(y)}
    return(list(yhatin=yhatin, yhatout=yhatout,y=y))}

## ************************************************************************
## ************************************************************************
##
## FUNCTION NAME: y2logmortality
##
## INPUT:     y = cross-sectional time series of the dependent variable.
##                It is a transformed version of the mortality time series,
##                where the transformed is defined by the parameter whotransform.
##
##            whotransform = scalar (1,2,3), the global parameter which defines the
##                           transformation which has been applied to mortality to
##                           obtain y (see its documentation)
##
## DESCRIPTION: the dependent variable y is usually a transformed version of mortality
##              (usually the log, but sometimes the square root). Results are always
##              saved as log mortality, so it is necessary to convert the dependent
##              variable to log mortality before we save it
##           
## IMPORTED functions: none (refer to function trans in make.mortality.data) 
##
## OUTPUT:    the cross-sectional time series of log(mortality) which corresponds to y
##           
##  WRITTEN BY: Elena Villalon
##              evillalon@latte.harvard.edu
##              CBRSS, Harvard University
## 
## Last modified: 05/21/2003
## 
## ************************************************************************
## ************************************************************************
    y2logmortality <- function(y, trans) {
    if (trans == 1){
### y is already log mortality      
        logm  <- y;
    }else if (trans == 2){
### y is square root of mortality
        logm <- log(y^2);
    }else if (trans == 3){
###  y is  log(mortality + 1)
        logm  <- log(exp(y) - 1);
    }else{
### y is mortality
        logm  <- log(y)}

    return(logm)}


svd.inv <-  function(x,svdtol=1e-10){
  s <- svd(x);
  w <- s$d;
  U <- s$u;
  V <- s$v;
  w.inv <- 0*w;
  w.inv[w > svdtol] <- 1/w[w > svdtol];
  if(length(w.inv) <= 1)
    w.inv <- as.matrix(w.inv)
  W.inv <- diag(w.inv);
  return(V %*% W.inv %*% t(U)); 
}


## ************************************************************************
## ************************************************************************
##
## FUNCTION NAME: ols
##
## INPUT:     whoinsampx, whoutsampx matrix of covariates for given csid (cntry+age group)
##            whoinsampy, whoutsampy (1 col) matrices of mortality observations 
##            delta.tol tolerance decrement to supress covariates linear dependencies
##            tol, tolerance starting value(0.999) for colinearities.   
##
## DESCRIPTION: obtain the estimates for dth's of insample and outsample years
##              for each csid and,if necessary, eliminating colinearities.  
##              Results to be compared to ols.functional 
##
## IMPORTED functions:
##                      covdel
##                      eliminate.colinear
##
## OUTPUT:    list with matrix elements for each csid
##            with predicted dth yhatin,for years of insample
##            and predicted dth yhatout for years of outsample
##            The fitting coefficients coeff and standard deviations. 
##           
##
##  WRITTEN BY: Federico Girosi
##              fgirosi@latte.harvard.edu
##              CBRSS, Harvard University
##             (modified by EV to include colinearities checks,if necessary?)
## 
## Last modified: 05/12/2003
## 
## ************************************************************************
## ************************************************************************
                    

ols <- function(ebase){
  ebase <- get("env.base", env=parent.frame())
  env.base <- ebase
  ewho <- get("env.who", env=ebase)
 
  whoinsampy <- get("whoinsampy", env=ewho)
  whoutsampy <- get("whoutsampy", env=ewho)
  whoinsampx <- get("whoinsampx", env=ewho)
  whoutsampx <- get("whoutsampx", env=ewho)
  who.ols.sigma.param <-  try(get("who.ols.sigma.param", env=ewho), silent=T)
  if(class(who.ols.sigma.param) == "try-error")
    who.ols.sigma.param <- get("ols.sigma.param", env=ebase)
  
  whoyrest <- try(get("whoyrest", env=ewho), silent=T)
  if(class(whoyrest) == "try-error")
    whoyrest <- get("yrest", env=ebase)
  age.vec <- get("age.vec", env=ewho)
  
  who.cntry.digits <- get("who.cntry.digits", env=ewho)
  who.age.digits <- get("who.age.digits", env=ewho)
  who.year.digits  <- get("who.year.digits", env=ewho)
  who.digit.first  <- get("who.digit.first", env=ewho)
  whomodel <- get("whomodel", env=ewho)
  svdtol <- get("svdtol", env=ewho)
  verbose <- get("verbose", env=ewho)
  digit.year.begin <- who.digit.first + who.cntry.digits + who.age.digits + 1;  
####
  cntry.names.lst <- get("cntry.names.lst", env=ewho)
  if(toupper(trim.blanks(whomodel)) == "OLS")
    messout("Running OLS model...", verbose);
  clist <- vector(mode="list",length=length(whoinsampy));
  names(clist) <- names(whoinsampy)
  coeff <- clist; 
  std   <- clist;  
  sigma <- make.sigma(who.ols.sigma.param);
### I need the following list for the computations of Gibbs' sample
   XX  <- clist;
   Xy  <- clist;
   rlm <- clist;

  for (i in 1:length(whoinsampy)){
  
    y <- whoinsampy[[i]];
    x <- whoinsampx[[i]];    
    yw <- whoinsampy[[i]]/sigma[[i]];
    xw <- t(scale(t(whoinsampx[[i]]),center=FALSE,scale=sigma[[i]]));
    yxw <- na.omit(cbind(yw, xw));

    yw <- yxw[,1]
    xw <- yxw[,-1]  
    rlm[[i]] <- try(glm(yw ~ xw - 1, family=gaussian,na.action=na.omit, offset=rep(0, length(yw))), silent=T)
    if( all(rlm[[i]]!="try-error"))
      matcoeff <- sapply(rlm[[i]]$coefficients, summary)
     
### also anova(rlm[[i]])  
###  matcoeff[,1] are the beta's
###  matcoeff[,2] are the std error of the betas
    n.obs <- length(yw);
    txw <- t(xw);
    invxw  <- svd.inv(txw%*% xw, svdtol);
    
    beta <- try(invxw %*% txw %*% yw, silent=T);
    if(class(beta) == "try-error"){
      csid <- names(whoinsampy)[i]
      mess <- paste("Data not consistent. Posibly too many NA's for...", csid); 
      stop(mess)
              
      return(lst) 
    }
    
    yin  <- x %*% beta; 
    std[[i]] <- sqrt(crossprod(na.omit(yin - y))/(n.obs-1));
    coeff[[i]] <- beta;
    rownames(coeff[[i]]) <- colnames(whoinsampx[[i]]);

### matrices to calculate likelihood for the Gibbs sampler
    XX[[i]] <- txw %*% xw
    Xy[[i]] <- txw %*% yw
  }

  
   assign("XX", XX, env=get("env.who", env=env.base));
   assign("Xy",Xy, env=get("env.who", env=env.base));

### now we make the forecasts of the dependent variable
   yhatin <- make.forecast(coeff,whoinsampx);
   yhatout <- make.forecast(coeff,whoutsampx);  
 
### the dependent variable may not be log-mortality, so we transform to log mortality
### before saving the results


  model <- model.string()
  
  
  lst <- list(yrest=whoyrest,model=model,age.vec=age.vec, cntry.lst=cntry.names.lst,
              coeff=coeff,yhatin=yhatin,yhatout=yhatout,std=std,
              insampy =whoinsampy, outsampy=whoutsampy)
  assign("lst.output", lst, env=ewho)
  return(invisible(lst));       
}


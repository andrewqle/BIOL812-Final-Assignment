library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
dat <- read.csv(file.choose())
dat<- na.omit(dat)


dat <- dat %>%
  mutate(day = as.Date(CollectDate, format = "%Y-%m-%d"))
      
dat1 <- dat %>%
  mutate(day = as.Date(day, format="%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(sum(COVID=="Positive"))
colnames(dat1)<- c("date", "cases")
                           

dat2<- subset(dat1, format(as.Date(date),"%Y")==2021)
plot(dat2)
library(lubridate)
dat2$day<-date(dat2$date)
dat2$day<-as.numeric(dat2$date)



######gammmmmm


## Model Checking function
tsDiagGamm <- function(x, timevar, observed, f = 0.3, type = "normalized") {
  resi <- resid(x$lme, type = type)
  fits <- fitted(x$lme)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  qqnorm(resi)
  qqline(resi)
  acf(resi, main = "ACF of Residuals")
}

## Functions for derivatives of GAM models ##
Deriv <- function(mod, n = 200, eps = 1e-7, newdata) {
  if(isTRUE(all.equal(class(mod), "list")))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
  newD <- newD + eps
  X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  # number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    ## Xi <- Xp * 0 ##matrix(0, nrow = Xp.r, ncol = Xp.c)
    ## J <- bs.dims[i]
    ## Xi[,(i-1) * J + 1:J + 1] <- Xp[,(i-1) * J + 1:J +1]
    ## df <- Xi %*% coef(mod)
    ## df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    ## lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  return(lD)
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term))
    term <- term.labs
  Term <- match(term, term.labs)
  ##term <- term[match(term, term.labs)]
  if(any(miss <- is.na(Term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  ## if(is.na(term))
  ##     stop("'term' not a valid model term.")
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- length(object$gamModel$y) - sum(object$gamModel$edf)
  tVal <- qt(1 - (alpha/2), residual.df)
  ## tVal <- qt(1 - (alpha/2), object$gamModel$df.residual)
  for(i in seq_along(term)) {
    upr <- object[[term[i]]]$deriv + tVal * object[[term[i]]]$se.deriv
    lwr <- object[[term[i]]]$deriv - tVal * object[[term[i]]]$se.deriv
    res[[term[i]]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term, eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term))
    term <- term.labs
  Term <- match(term, term.labs)
  if(any(miss <- is.na(Term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(is.na(Term)))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  ## tVal <- qt(1 - (alpha/2), x$gamModel$df.residual)
  residual.df <- length(x$gamModel$y) - sum(x$gamModel$edf)
  tVal <- qt(1 - (alpha/2), residual.df)
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")[Term]
    names(xlab) <- xlab
  }
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  CI <- confint(x, term = term, alpha = alpha)
  for(i in seq_along(term)) {
    ## for(i in seq_len(l)) {
    upr <- CI[[term[i]]]$upper
    lwr <- CI[[term[i]]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,term[i]], x[[term[i]]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[term[i]], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,term[i]], rev(x$eval[,term[i]])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,term[i]], upr, lty = "dashed")
      lines(x$eval[,term[i]], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,term[i]], x[[term[i]]]$deriv, lwd = 1)
      S <- signifD(x[[term[i]]]$deriv, x[[term[i]]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,term[i]], S$incr, lwd = lwd, col = "blue")
      lines(x$eval[,term[i]], S$decr, lwd = lwd, col = "red")
    } else {
      lines(x$eval[,term[i]], x[[term[i]]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}

## For GAMM models from mgcv:::gamm

## Model Checking function
gam <- function(x, timevar, observed, f = 0.3, type = "normalized") {
  resi <- resid(x$lme, type = type)
  fits <- fitted(x$lme)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  qqnorm(resi)
  qqline(resi)
  acf(resi, main = "ACF of Residuals")
}
#######dont use rn
## Model Checking function
gam <- function(dat2, day, observed, f = 0.3, type = "normalized") {
  resi <- resid(dat$year, type = type)
  fits <- fitted(dat$year)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  qqnorm(resi)
  qqline(resi)
  acf(resi, main = "ACF of Residuals")
}
#############################







#######
library(mgcv)
library(Hmisc)

gamm_covid <- gamm(cases ~ s(day, k=7), data = dat2)

summary(gamm_covid$gam) # Ignore the p-value; look at the R-sq term
gam.check(gamm_covid$gam)


gamm_covid <- gamm(cases ~ s(day, k=2), data = dat2)

summary(gamm_covid$gam) # Ignore the p-value; look at the R-sq term
gam.check(gamm_covid$gam)

gamm_covid <- gamm(cases ~ s(day, k=15), data = dat2)

summary(gamm_covid$gam) # Ignore the p-value; look at the R-sq term
gam.check(gamm_covid$gam)

gamm_covid <- gamm(cases ~ s(day, bs="cr"), data = dat2)

summary(gamm_covid$gam) # Ignore the p-value; look at the R-sq term
gam.check(gamm_covid$gam)


summary(gamm_covid)
###case counts per day

library(mgcv)
library(Hmisc)




## Then, look at other diagnostics
#with(s.climatedata, tsDiagGamm(gamm_chla, timevar = Depth, observed = Chl.a))
#Look at the periods of significant change
gam.d <- Deriv(gamm_covid$gam, n = 200)
plot(gam.d, sizer = T, alpha = 0.01)

pdat <- with(dat2, data.frame(day = seq(min(day), max(day), length = 200))) # Set up a dataframe for the predicted values to be added to in the next line
p2 <- predict(gamm_covid$gam, newdata = pdat) # Add the fitted GAM values to the new dataset


CI <- confint(gam.d, alpha = 0.01)
S <- signifD(p2, gam.d$cases$deriv, CI$cases$upper, CI$cases$lower,
             eval = 0)
## PLOT

plot(cases ~ day, data = dat2, pch = 19, col = "grey50", type = "p", ylab = "Number of Cases", xlab = "Day") # plots the observed values
lines(p2 ~ day, data = pdat, lwd = 1) # plots the fitted GAM values

S <- signifD(p2, gam.d$day$deriv, CI$day$upper, CI$day$lower, eval = 0) # Calculate the periods of significance
lines(S$incr ~ day, data = pdat, lwd = 4, col = "blue") # Shows periods of significant increases
lines(S$decr ~ day, data = pdat, lwd = 4, col = "red") # Shows periods of significant decreases

abline(h=mean(dat2$cases[dat2$day > 10]), col = "grey60", lwd = 1.5, lty = 2) # shows the mean of the observed values from the pre-impact period (make sure the "10" is replaced by the depth of the start of the pre-impact period)


##############

mod2 <-  gam(all ~ s(cases)+s(day)+  te(cases,day, 
                 bs = c("cc","tp"), k=c(20,30)) +  s(nmonth) + s(nday) +   data=dat2,method = "REML",family = quasipoisson)

summary(mod2)
## 






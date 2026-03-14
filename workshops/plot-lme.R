library('nlme')
library('Matrix')

plot.lme <- function(mod, vcov.id){

  resid.norm <- resid(mod, type="normalized")
  fit        <- fitted(mod)

  rand.effs  <- random.effects(mod)
  rand.names <- colnames(rand.effs)
  n.rand     <- dim(rand.effs)[2]

  n.rows <- 2 + ceiling(n.rand/2)
  par(mfrow=c(n.rows,2), mar=c(2,2,2,2))

  # Residuals vs fitted
  plot(fit, resid.norm, main='Residuals vs fitted')
  lines(lowess(fit,resid.norm))

  # Normal Q-Q
  qqnorm(resid.norm, main="Residuals") 
  qqline(resid.norm)
  
  # Scale-Location
  plot(fit, sqrt(abs(resid.norm)), main='Scale-Location')
  lines(lowess(fit,sqrt(abs(resid.norm))))

  ## Normal Q-Q of the random effects
  for (i in 1:n.rand){
    RE <- rand.effs[,i]
    qqnorm(RE, main=rand.names[i]) 
    qqline(RE)
  }

  # Visualisation of the marginal covariance structure
  V <- getVarCov(mod, type='marginal', individual=vcov.id)[[vcov.id]]
  graphics::image(as.matrix(V)[nrow(V):1, ], 
                  main=paste0('Marginal covariance structure (ID=', vcov.id, ')'),
                  col = gray.colors(128),
                  axes=FALSE)
}
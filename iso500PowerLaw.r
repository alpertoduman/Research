### iso 500 operating revenue power law
library(poweRlaw)

m_misoOR = conpl$new(isoOR)


m_misoOR$getXmin()

m_misoOR$getPars()

(estisoOR = estimate_pars(m_misoOR))

#(estisoOR = estimate_xmin(m_misoOR))

# m_misoOR$setXmin(estisoOR)

## Plot the data (from xmin)
plot(m_misoOR, main="Power Law Distribution of ISO 500", 
     ylab="", xlab=" ", options(scipen=999))
## Add in the fitted distribution
lines(m_misoOR, col=2, lwd=4)
legend("topright", c("alpha= 2.13"), text.col="red")

library("Synth")

rm(list = ls())
setwd("C:/Users/diego/Google Drive/Disasters")
load("datasets/regions_1960_2001.rda")

#reducings digits in gdp data
base2$gdpcap <- base2$gdpcap/1000000


dataprep.out <-
              dataprep(foo = base2,
                       predictors = c("sec.agricultureper" , "sec.fishingper" , "sec.miningper" ,
                                      "sec.industryper" , "sec.energyper" , "sec.constructionper","sec.retailper", "sec.transportper","sec.othersper") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1984 ,
                       special.predictors = list(
                         list("gdpcap" , 1960:1984 , "mean")),
                       dependent = "gdpcap",
                       unit.variable = "id",
                       unit.names.variable = "regionname",
                       time.variable = "year",
                       treatment.identifier = 5,
                       controls.identifier = c(1:4,6:13),
                       time.optimize.ssr = 1960:1984,
                       time.plot = 1960:2001
                       )
 
	

synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "BFGS")
	
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
 
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
	
synth.tables$tab.w

 path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "real per-capita GDP (millions 1996 CLP)",
           Xlab = "year",
           Ylim = c(1,2.5),
           Legend = c(" Valparaiso region","Synthetic Valparaiso region"),
           Legend.position = "bottomright"
           )
abline(v=1985, col = "red", lty = 2)


gaps.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "gap in real per-capita GDP (millions 1996 CLP)",
           Xlab = "year",
           #Ylim = c(-1.5,1.5),
           Main = NA
           )


store <- matrix(NA,length(1960:2001),13)
colnames(store) <- unique(base2$regionname)

# run placebo test
for(iter in 1:13) {
 dataprep.out <-
              dataprep(foo = base2,
                       predictors = c("sec.agricultureper" , "sec.fishingper" , "sec.miningper" ,
                                      "sec.industryper" , "sec.energyper" , "sec.constructionper","sec.retailper", "sec.transportper","sec.othersper") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1960:1984 ,
                       special.predictors = list(
                         list("gdpcap" , 1960:1984 , "mean")),
                      dependent = "gdpcap",
                       unit.variable = "id",
                       unit.names.variable = "regionname",
                       time.variable = "year",
                       treatment.identifier = iter,
                       controls.identifier = c(1:13)[-iter],
                       time.optimize.ssr = 1960:1984,
                       time.plot = 1960:2001
                       )

# run synth
synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "BFGS"
                   )

# store gaps
store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}
store
# now do figure
data <- store
rownames(data) <- 1960:2001
length(data)

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1960:2001
gap.end.pre  <- which(rownames(data)=="1984")

#  MSPE Pre-Treatment
mse        <-             apply(data[ gap.start:gap.end.pre,]^2,2,mean)
valparaiso.mse <- as.numeric(mse[5])
# Exclude states with 5 times higher MSPE than basque
data <- data[,mse<5*valparaiso.mse]

# Plot
plot(years,data[gap.start:gap.end,7],
     #ylim=c(-200,200),
     xlab="year",
     xlim=c(1960,2001),ylab="gap in real per-capita GDP (millions 1996 CLP)",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Valparaiso Line
lines(years,data[gap.start:gap.end,7],lwd=2,col="black")

# Add grid
abline(v=1985,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("topright",legend=c("Valparaiso","control regions"),
lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)

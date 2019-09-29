setwd("D://")
getwd()

library(readxl)
df<-read_excel("mds_data.xlsx")
View(df)
actor<-df[-c(1,12:22)]
View(actor)
dist_response<-dist(actor)
dist_response
# Transpose data
t_actor<-t(actor)
t_actor
t_dist_response<-dist(t_actor)
t_dist_response
mds_scale<-cmdscale(t_dist_response,k=2)
mds_scale
plot(mds_scale, col="blue", cex=.8, pch=19, xlim = c(-20,15), ylim = c(-15,15))
text(mds_scale[,1], mds_scale[,2], row.names(mds_scale))
abline(h=0)
abline(v=0)

# to take mean of all the actors (of all variables)
# Install a package
install.packages("summarytools")
library(summarytools)
summarytools::descr(actor) # data used is before transposing and where only actors are present
plot(mds_scale, col="blue", cex=.8, pch=19, xlim = c(-20,15), ylim = c(-15,15), main = "Perceptual mapping of Bollywood actors", 
ylab = "low preferred,            high preferred", xlab = "Khan Brothers,                     New Generation" )

# Slicing data gender wise
percep_mapp <- df[-c(1,12:21)]
percep_mapp
mds_data_boys <- percep_mapp[percep_mapp$Gender>1, ]
mds_data_girls <- percep_mapp[percep_mapp$Gender<2, ]
mds_data_boys1 <- mds_data_boys[-c(1,11)]
mds_data_girls1 <- mds_data_girls[-c(1,11)]

head(mds_data_boys)
head(mds_data_girls)
dim(mds_data_boys)
dim(mds_data_girls)

# Perceptual mapping
# It is called multi dimensional scaling
par(mfrow=c(1,2))

t_actor_boys<-t(mds_data_boys1)
t_actor_boys
t_dist_response_boys<-dist(t_actor_boys)
t_dist_response_boys
mds_scale_boys<-cmdscale(t_dist_response_boys,k=2)
mds_scale_boys
plot(mds_scale_boys, col="blue", cex=.8, pch=19, xlim = c(-20,15), ylim = c(-15,15))
text(mds_scale_boys[,1], mds_scale_boys[,2], row.names(mds_scale_boys))
abline(h=0)
abline(v=0)

t_actor_girls<-t(mds_data_girls1)
t_actor_girls
t_dist_response_girls<-dist(t_actor_girls)
t_dist_response_girls
mds_scale_girls<-cmdscale(t_dist_response_girls,k=2)
mds_scale_girls
plot(mds_scale_girls, col="blue", cex=.8, pch=19, xlim = c(-20,15), ylim = c(-15,15))
text(mds_scale_girls[,1], mds_scale_girls[,2], row.names(mds_scale_girls))
abline(h=0)
abline(v=0)


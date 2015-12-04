risk2=rep(0,nrow(data))
for(i in 1:nrow(risk)){
  id=which(data$user_id==risk$user_id[i])
  risk2[id]=diag(as.matrix(risk[,-c(1:4)]))[i]
}
summary(risk2)
data=cbind(data,risk2)

library(ape)

R=50
N=length(which(table(data$user_id)>R))
N

data_new=data[which(table(data$user_id)>R),]
data_new=data_new[which(data_new$baseline_color!="black"),]
dim(data_new)
table(data_new$baseline_color)

summary(data_new$risk2[which(data_new$baseline_color=="red")])
summary(data_new$risk2[which(data_new$baseline_color=="blue")])

ls()
rm(baseline_color,data,i,id,inds,risk,risk2)
ls()
dists=as.matrix(dist(cbind(data_new$longitude, data_new$latitude)))
dists.inv=1/dists
diag(dists.inv)=0
dists.inv[is.infinite(dists.inv)]=0
Moran.I(data_new$risk2,dists.inv)
Moran.I(data_new$risk2[which(data_new$baseline_color=="red")], 
        dists.inv[which(data_new$baseline_color=="red"),
        which(data_new$baseline_color=="red")])
Moran.I(data_new$risk2[which(data_new$baseline_color=="blue")], 
        dists.inv[which(data_new$baseline_color=="blue"),
                  which(data_new$baseline_color=="blue")])





library(spdep)
coords=cbind(data_new$longitude, data_new$latitude)
nb=knn2nb(knearneigh(coords, k = 10,longlat=T))
list=nb2listw(nb)
joincount.test(data_new$baseline_color,list)



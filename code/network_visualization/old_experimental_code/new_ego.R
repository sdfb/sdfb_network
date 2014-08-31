## Note that this is code from some stat undergrad. 
## TODO: Figure out the exact source of this code (it's in the emails somewhere...)

library(network)
library(sna)

# Load data and names into R
data.matrix<-read.csv("new.lambda.matrix.csv",header=T)
data.matrix<-data.matrix[,(-1)]

names<-read.csv("new.idnamecount.csv")
colnames(data.matrix)<-names[,3]

############ Pick the whole row for the target person ############
row.Milton<-as.matrix(data.matrix[2914,])
row.Hobbes<-as.matrix(data.matrix[5185,])
row.Bacon<-as.matrix(data.matrix[1096,])
row.Winstanley<-as.matrix(data.matrix[1495,])
row.Cavendish<-as.matrix(data.matrix[3500,])
row.I<-as.matrix(data.matrix[487,])
row.Shakespeare<-as.matrix(data.matrix[6173,])
row.Harrington<-as.matrix(data.matrix[2054,])

######## Record everyone's weight with the target person ###########

weights<-function(matrix){
record=c()
for (i in 1:6289){
	record[i]=matrix[i][1]}
return (record)}

weights.Milton=weights(row.Milton)
weights.Hobbes=weights(row.Hobbes)
weights.Bacon=weights(row.Bacon)
weights.Winstanley=weights(row.Winstanley)
weights.Cavendish=weights(row.Cavendish)
weights.I=weights(row.I)
weights.Shakespeare=weights(row.Shakespeare)
weights.Harrington=weights(row.Harrington)

####### Pick those that have weights greater than certain value with the target person ######################

Mod.strong<-function(weight.list){
j=1
new.list=c()
for (i in 1:6289){
	if (weight.list[i]>=0.001){
		new.list[j]=i
		j=j+1
	}
}
return(new.list)
}

strong<-function(weight.list){
j=1
new.list=c()
for (i in 1:6289){
	if (weight.list[i]>=0.01){
		new.list[j]=i
		j=j+1
	}
}
return(new.list)
}

ms.Milton=Mod.strong(weights.Milton)
ms.Hobbes=Mod.strong(weights.Hobbes)
ms.Bacon=Mod.strong(weights.Bacon)
ms.Winstanley=Mod.strong(weights.Winstanley)
ms.Cavendish=Mod.strong(weights.Cavendish)
ms.Shakespeare=Mod.strong(weights.Shakespeare)
ms.Harrington=Mod.strong(weights.Harrington)

st.Milton=strong(weights.Milton)
st.Hobbes=strong(weights.Hobbes)
st.Bacon=strong(weights.Bacon)
st.I=strong(weights.I)
st.Harrington=strong(weights.Harrington)

#################### Add the target person himself ####################
ms.Milton.add<-rep(0,153)
for (i in 1:152){
	ms.Milton.add[i]=ms.Milton[i]
}
ms.Milton.add[153]=2914

st.Milton.add<-rep(0,21)
for (i in 1:20){
	st.Milton.add[i]=st.Milton[i]
}
st.Milton.add[21]=2914

#######################################################################
ms.Hobbes.add<-rep(0,95)
for (i in 1:94){
	ms.Hobbes.add[i]=ms.Hobbes[i]
}
ms.Hobbes.add[95]=5185

st.Hobbes.add<-rep(0,12)
for (i in 1:11){
	st.Hobbes.add[i]=st.Hobbes[i]
}
st.Hobbes.add[12]=5185

#####################################################################
ms.Bacon.add<-rep(0,203)
for (i in 1:202){
	ms.Bacon.add[i]=ms.Bacon[i]
}
ms.Bacon.add[203]=1096

st.Bacon.add<-rep(0,22)
for (i in 1:21){
	st.Bacon.add[i]=st.Bacon[i]
}
st.Bacon.add[22]=1096

######################################################################
ms.Winstanley.add<-rep(0,33)
for (i in 1:32){
	ms.Winstanley.add[i]=ms.Winstanley[i]
}
ms.Winstanley.add[33]=1495

#######################################################################
ms.Cavendish.add<-rep(0,36)
for (i in 1:35){
	ms.Cavendish.add[i]=ms.Cavendish[i]
}
ms.Cavendish.add[36]=3500

########################################################################
st.I.add<-rep(0,64)
for (i in 1:63){
	st.I.add[i]=st.I[i]
}
st.I.add[64]=487

########################################################################
ms.Shakespeare.add<-rep(0,79)
for (i in 1:78){
	ms.Shakespeare.add[i]=ms.Shakespeare[i]
}
ms.Shakespeare.add[79]=6173

########################################################################
ms.Harrington.add<-rep(0,64)
for (i in 1:63){
	ms.Harrington.add[i]=ms.Harrington[i]
}
ms.Harrington.add[64]=2054

st.Harrington.add<-rep(0,6)
for (i in 1:5){
	st.Harrington.add[i]=st.Harrington[i]
}
st.Harrington.add[6]=2054

######################## Pick the sub-matrix ###########################
subMilton.ms<-as.matrix(data.matrix[ms.Milton.add,ms.Milton.add])
subMilton.st<-as.matrix(data.matrix[st.Milton.add,st.Milton.add])

subHobbes.ms<-as.matrix(data.matrix[ms.Hobbes.add,ms.Hobbes.add])
subHobbes.st<-as.matrix(data.matrix[st.Hobbes.add,st.Hobbes.add])

subBacon.ms<-as.matrix(data.matrix[ms.Bacon.add,ms.Bacon.add])
subBacon.st<-as.matrix(data.matrix[st.Bacon.add,st.Bacon.add])

subWinstanley.ms<-as.matrix(data.matrix[ms.Winstanley.add,ms.Winstanley.add])

subCavendish.ms<-as.matrix(data.matrix[ms.Cavendish.add,ms.Cavendish.add])

subI.st<-as.matrix(data.matrix[st.I.add,st.I.add])

subShakespeare.ms<-as.matrix(data.matrix[ms.Shakespeare.add,ms.Shakespeare.add])

subHarrington.ms<-as.matrix(data.matrix[ms.Harrington.add,ms.Harrington.add])
subHarrington.st<-as.matrix(data.matrix[st.Harrington.add,st.Harrington.add])

################## Change everything to network #####################
ms.net.Milton<-network(subMilton.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")
st.net.Milton<-network(subMilton.st,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Hobbes<-network(subHobbes.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")
st.net.Hobbes<-network(subHobbes.st,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Bacon<-network(subBacon.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")
st.net.Bacon<-network(subBacon.st,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Winstanley<-network(subWinstanley.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Cavendish<-network(subCavendish.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

st.net.I<-network(subI.st,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Shakespeare<-network(subShakespeare.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

ms.net.Harrington<-network(subHarrington.ms,type="adjacency",directed=F,ignore.eval=F,names.eval="value")
st.net.Harrington<-network(subHarrington.st,type="adjacency",directed=F,ignore.eval=F,names.eval="value")

############# Change the target person himself to bigger dot ############
big.Milton<-rep(1,153)
big.Milton[153]=3

big.Milton2<-rep(1,21)
big.Milton2[21]=3

big.Hobbes<-rep(1,95)
big.Hobbes[95]=3

big.Hobbes2<-rep(1,12)
big.Hobbes2[12]=3

big.Bacon2<-rep(1,22)
big.Bacon2[22]=3

big.Winstanley<-rep(1,33)
big.Winstanley[33]=3

big.Cavendish<-rep(1,36)
big.Cavendish[36]=3

big.I2<-rep(1,64)
big.I2[64]=3

big.Shakespeare<-rep(1,79)
big.Shakespeare[79]=3

big.Harrington<-rep(1,64)
big.Harrington[64]=3

big.Harrington2<-rep(1,6)
big.Harrington2[6]=3
################# Plot weighted network with statnet ##################
plot(ms.net.Milton,vertex.cex=big.Milton,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Milton%e%'value')/log(10))+6,main="John Milton's network with 152 people \n (only moderate and strong ties connected to Milton)")
legend("bottomleft",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(st.net.Milton,vertex.cex=big.Milton2,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=T,edge.col=ceiling(-log(st.net.Milton%e%'value')/log(10))+6,main="John Milton's network with 20 people \n (only strong ties connected to Milton)")
legend("bottomleft",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(ms.net.Hobbes,vertex.cex=big.Hobbes,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Hobbes%e%'value')/log(10))+6,main="Thomas Hobbes's network with 94 people \n (only moderate and strong ties connected to Hobbes)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(st.net.Hobbes,vertex.cex=big.Hobbes2,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=T,edge.col=ceiling(-log(st.net.Hobbes%e%'value')/log(10))+6,main="Thomas Hobbes's network with 11 people \n (only strong ties connected to Hobbes)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(st.net.Bacon,vertex.cex=big.Bacon2,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=T,edge.col=ceiling(-log(st.net.Bacon%e%'value')/log(10))+6,main="Francis Bacon's network with 21 people \n (only strong ties connected to Bacon)")
legend("bottomleft",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(ms.net.Winstanley,vertex.cex=big.Winstanley,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Winstanley%e%'value')/log(10))+6,main="Gerrard Winstanley's network with 32 people \n (only moderate and strong ties connected to Winstanley)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(ms.net.Cavendish,vertex.cex=big.Cavendish,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Cavendish%e%'value')/log(10))+6,main="Margaret Cavendish's network with 35 people \n (only moderate and strong ties connected to Cavendish)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(st.net.I,vertex.cex=big.I2,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(st.net.I%e%'value')/log(10))+6,main="Charles I's network with 63 people \n (only strong ties connected to Charles I)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(ms.net.Shakespeare,vertex.cex=big.Shakespeare,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Shakespeare%e%'value')/log(10))+6,main="William Shakespeare's network with 78 people \n (only moderate and strong ties connected to Shakespeare)")
legend("topright",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(ms.net.Harrington,vertex.cex=big.Harrington,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=F,edge.col=ceiling(-log(ms.net.Harrington%e%'value')/log(10))+6,main="James Harrington's network with 63 people \n (only moderate and strong ties connected to Harrington)")
legend("bottomleft",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))

plot(st.net.Harrington,vertex.cex=big.Harrington2,label.cex=0.5,displaylabels=T,edge.lty=3,edge.lwd=1,usecurve=T,edge.col=ceiling(-log(st.net.Harrington%e%'value')/log(10))+6,main="James Harrington's network with 5 people \n (only strong ties connected to Harrington)")
legend("bottomleft",c("strong","moderate","weak"),lty=c(1,1,1),col=c(8,9,10))








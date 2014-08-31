require('glmnet')

################## transforma adjacency matrix B to Poisson transformative matrix A. 
# B is the graph adjacency matrix
# A is the transformation matrix for Multivariate Poisson 
# transforma B to A
# type is concise or full; concise only put a column if there is an edge; full put all combinations
adj2A <- function(B,type="full"){
	if(nrow(B)!=ncol(B)){
		print("not a symmetric matrix")
		return;
	}
	A=diag(1,nrow=nrow(B),ncol=ncol(B))
	for(i in 1:(nrow(B)-1)){
		for( j in (i+1):ncol(B)){
			
			if(type=="full"){
				tmp=rep(0,nrow(B))
				tmp[c(i,j)]=1
				A=cbind(A,tmp)	
				
			}else if(B[i,j]==1){
				tmp=rep(0,nrow(B))
				tmp[c(i,j)]=1
				A=cbind(A,tmp)				
			}
			
		}
		
		
	}
	#colnames(A)=c()
	return(A)
	
}




# generate a lattcie adjacency matrix B
adj <- function(type="lattice", rnodes=3,cnodes=3){
	
	
	if(type=="lattice"){
		B=diag(1,nrow=rnodes*cnodes,ncol=cnodes*rnodes)
		for(i in 1:(rnodes*cnodes)){
			#print(i);
			if( i%% rnodes !=0 ){
				B[i,i+1]=1
			}
			if((i-1)%%rnodes!=0){
				B[i,i-1]=1
			}
			if((i-rnodes)>0){
				B[i,i-rnodes]=1
			}			
			if((i+rnodes)<=rnodes*cnodes){
				B[i,i+rnodes]=1
			}			
		}
		return(B)
	}	
}

############### sample iid multivariate poissons with lambda vector
# each row is a poison variate, each column is a sample
# n is the number of samples, lambda.v is the vector for lambda
##################################################################################
rmpois<-function(n,lambda.v){
	tmp=c();
	# for(i in lambda.v){
		# tmp=rbind(tmp,rpois(n,i))
	# }
	require(multicore)
	tmp =  do.call(rbind, mclapply(lambda.v,function(i) { rpois(n,i)}))
	return(tmp)
}


##### Generate a poison network ############################################
#type = random, hub, cluster, band, scale-free, lattice
#lambda for edges
#labbda.c for backgrounds
##### return values mydata
### B is the graph adjacency matrix
### A is the poison construction matrix
### X is the data n by p matrix
### sigma is the convariance matrix
#############################################################################


mp.generator<-function(lambda,lambda.c,n,p,type="lattice", myGraph=NULL,speed.pois="fast"){
	#set.seed(123)
	require("huge")
	B=c()
	if(type == "lattice")
	{
		B = adj(rnodes=p,cnodes=p)
		
	}else if(type == "pre-defined" | !is.null(myGraph)){
		B = myGraph
	}else {
		B= as.matrix(huge.generator(n=n, d=p, graph=type)$theta)
	}
	
	
	if(speed.pois == "full"){
		A = adj2A(B,type="full")
		sigma=lambda*B+lambda.c*abs(1-B)
		ltri.sigma=sigma[lower.tri(sigma)]
		Y.lambda = c(rep(lambda,nrow(sigma)), ltri.sigma)
		Y = rmpois(n,Y.lambda)
		X =A%*%Y
		mydata=c()
		mydata$X=X
		mydata$B=B
		mydata$A=A
		mydata$sigma=sigma
		return(mydata)	
	}
	if(speed.pois == "fast"){
		A = adj2A(B,type=type)
		sigma=lambda*B
		ltri.sigma=sigma[lower.tri(sigma)]
		nonzero.sigma = ltri.sigma[which(ltri.sigma !=0 )]
		Y.lambda = c(rep(lambda,nrow(sigma)), nonzero.sigma)
		
		Y = rmpois(n,Y.lambda)
		X =A%*%Y
		# add the labmda.c to all the nodes. 
		X = X + rmpois(ncol(X), rep(lambda.c,nrow(X)))
		mydata=c()
		mydata$X=X
		mydata$B=B
		mydata$A=A
		mydata$sigma=sigma
		return(mydata)	
		
	}
		
		
}


#### plot a adjancey
myplot <-function(s, mylayout=NULL, top=0,main=NULL, true_graph=NULL){
	s=as.matrix(s)
	if(is.null(mylayout)){
		print("mylayout is null")
		#tmp=graph.adjacency(adj("lattice",sqrt(nrow(s)),sqrt(nrow(s))), mode ="undirected")
		tmp = graph.adjacency(s,mode="undirected")
		V(tmp)$label=1:nrow(s)
		#mylayout<-layout.kamada.kawai(tmp) 	
		mylayout = layout.fruchterman.reingold(tmp)

	}

	if(top==0){
			diag(s)=0
			s[abs(s)>0]=1
			tmp=graph.adjacency(s, mode ="undirected")
			V(tmp)$label=1:nrow(s)
			tmp$layout<-mylayout
			if(!is.null(true_graph)){

				colVec=c()
				for(i in 1:length(tmp[[3]])){
					if(true_graph[tmp[[3]][i]+1,tmp[[4]][i]+1]){
						colVec=c(colVec,"red")
					}else{
						colVec=c(colVec,"grey")
					}
				}
				
				E(tmp)$color= colVec

			}
			
			
			
		plot(tmp,main=main,edge.width = 5)	
		return(mylayout);
	}
	
	diag(s)=0
	lbound = max(max(min(abs(s)),0),sort(abs(s),decreasing=T)[2*top+1])
	s=abs(s)
	s[s<lbound]=0
	s[s>=lbound]=1
	tmp=graph.adjacency(s, mode ="undirected")
	V(tmp)$label=1:nrow(s)
	tmp$layout<-mylayout
	#print(s)
	
	plot(tmp,main=main,edge.width = 5)
	
	
	return(mylayout)
	
}


##### generate ROC curv from regularization path
myroc <-function(true_graph_list, est_graph, add=T, method="ROC",col="black"){
	
	output=matrix(0,nrow=2,ncol=length(est_graph))
	if(method=="ROC"){
		for(i in 1:length(est_graph)){
			true_graph=c()
			if(is.list(true_graph_list) && length(true_graph_list)==length(est_graph)){	
				true_graph = true_graph_list[[i]]	
			}else{
				true_graph = true_graph_list
			}	

			true_graph = true_graph[upper.tri(true_graph)]
			tmp_graph = as.matrix(est_graph[[i]])
			tmp_graph = tmp_graph[upper.tri(tmp_graph)]
			
			TP=sum(tmp_graph&true_graph)
			TN=sum((!tmp_graph)&(!true_graph))
			
			FP=sum(tmp_graph&(!true_graph))
			FN=sum((!tmp_graph)&true_graph)
			
			TPR = TP/(TP+FN)
			FPR = FP/(FP+TN)		
			
			#print(c(TP,TN,FP,FN))
			output[1,i]=TPR
			output[2,i]=FPR
		}
		
		if(add==T){
			points(output[2,],output[1,],col=col)
			lines(output[2,],output[1,],col=col)
		}else{
			plot(output[2,],output[1,],col=col,xlim=c(0,1),ylim=c(0,1))
			lines(output[2,],output[1,],col=col)
		}
	return(output)
	}	
}



###### compare the TP, FP

myperf<-function(B,B_hat,method="F1",add=FALSE,col="black")
{
	if(method=="F1"){
	B=B[upper.tri(B)]
	B_hat=B_hat[upper.tri(B_hat)]
	
	#TP
	TP=sum(B&B_hat)
	#TN
	TN=sum(!B&(!B_hat))
	#FP
	FP=sum(!B&B_hat)
	#FN
	FN=sum(B&(!B_hat))
	
	Precision = TP/(TP+FP)
	Recall = TP/(TP+FN)
	
	F1=2*(Precision*Recall)/(Precision+Recall)	
	return(F1)	
	}
	if(method=="ROC"){
		B_hat=B_hat[upper.tri(B_hat)]
		B=B[upper.tri(B)]
		require(ROCR)
		rocr.perf= performance(prediction(B_hat,B),"tpr","fpr")
		plot(rocr.perf,add=add,col=col)
		return(rocr.perf)
	}
	
	
}



############ select the optimum threshold lambda for a poisson network
##### glmnet using star as selection.
# X is the input matrix.  
########################################################################

myglmnet.select <- function(X,method="star",link="log",N=100,beta=0.05, lambda.path=NULL, delta = 0.01,parallel=F,warmStart=T,nCpus=4){
	
	if(is.null(lambda.path) ){
		maxlambda = myglmnet.max(X,link=link, delta = delta)
		lambda.path = exp(seq(log(maxlambda),log(0.05*maxlambda), length = 100))
	}
	
	if(method=="star" & link=="log" & warmStart == T){
		b = min(c(10*sqrt(ncol(X)), 0.8*ncol(X))) 
		ghat=list()
		ghat.path=list()
		ghat.path$path=vector("list",length(lambda.path))
		v=c()

		for(i in 1:N){
				cat(paste("LLGM: Conducting sampling ... in progress: ", floor(100*(i/N)), "%", collapse=""),"\r")
				flush.console()
				index = sample(1:ncol(X),b,replace=F)
				#tmp=glmpois(X[,index],lambda.path[j],parallel=parallel,warmStart=warmStart,nCpus=nCpus)
				ghat.path$raw= glmpois(X[,index],lambda=lambda.path,parallel=parallel,nCpus=nCpus)
				
				for(j in 1:length(lambda.path)){
					tmp=ghat.path$raw[,,j]
					tmp[abs(tmp)<1e-06]=0
					tmp[abs(tmp)>1e-06]=1
					diag(tmp)=0
					if(is.null(ghat.path$path[[j]])){
						ghat.path$path[[j]]=tmp;
					}else{
						ghat.path$path[[j]]=ghat.path$path[[j]]+tmp	
					}
						
				}	
		}
		
		for(i in 1:length(lambda.path)){
			D=ghat.path$path[[i]]
			D=D/N
			D=2*D*(1-D)
			v=c(v,mean(D[upper.tri(D)]))	
		}
					
		
		v=cummax(v)
		ghat$v=v
		ghat$lambda.path = lambda.path
		ghat$opt.lambda = lambda.path[which(v==max(v[v<beta]))]	
			
		return(ghat)
	}
	
	
	if(method=="star" & link=="log" & warmStart == F){
		b = min(c(10*sqrt(ncol(X)), 0.8*ncol(X))) 
		ghat=list()
		v=c()
		
		
		for( j in 1:length(lambda.path)){
			cat(paste("Conducting sampling ... in progress: ", floor(100*(j/length(lambda.path))), "%", collapse=""),"\r")
			flush.console()
			D=matrix(0,nrow=nrow(X),ncol=nrow(X))
			for(i in 1:N){
				index = sample(1:ncol(X),b,replace=F)
				tmp=glmpois(X[,index],lambda.path[j],parallel=F)
				tmp[abs(tmp)<1e-06]=0
				tmp[abs(tmp)>1e-06]=1
				D=D+tmp
			}
			D=D/N
			D=2*D*(1-D)
			v=c(v,mean(D[upper.tri(D)]))			
		}
		v=cummax(v)
		ghat$v=v
		ghat$lambda.path = lambda.path
		ghat$opt.lambda = lambda.path[which(v==max(v[v<beta]))]	
			
		return(ghat)
	}
		
}


myglmnet.max <-function(X, link ="log",delta=0.1){
	minlambda =0;
	maxlambda = 1000;
	### binary search the interval
	while(1){
		mid = (minlambda+maxlambda)/2
		tmp=glmpois(X,mid)
		tmp[abs(tmp)<1e-06]=0
		tmp[abs(tmp)>1e-06]=1
		if(sum(tmp)>0){
			minlambda = mid+delta
		}else{
			maxlambda = mid-delta
		}			
		
		if(abs(maxlambda-minlambda)<delta){
			return(mid);
		}
	}
	
	
}


myglmnet.select2 <- function(X,method="star",link="log",N=100,beta=0.05, lambda.path=NULL, delta = 0.01,parallel=F,warmStart=T,nCpus=4){
	
	if(is.null(lambda.path) ){
		maxlambda = myglmnet.max(X,link=link, delta = delta)
		lambda.path = exp(seq(log(maxlambda),log(0.05*maxlambda), length = 100))
	}
	
	if(method=="star" & link=="log" & warmStart == T){
		b = min(c(10*sqrt(ncol(X)), 0.8*ncol(X))) 
		ghat=list()
		ghat.path=list()
		ghat.path$path=vector("list",length(lambda.path))
		v=c()


		mclapply(1:N, function(r){
			
				cat(paste("LLGM: Conducting sampling ... in progress: ", floor(100*(i/N)), "%", collapse=""),"\r")
				flush.console()
				index = sample(1:ncol(X),b,replace=F)
				#tmp=glmpois(X[,index],lambda.path[j],parallel=parallel,warmStart=warmStart,nCpus=nCpus)
				ghat.path$raw= glmpois(X[,index],lambda=lambda.path,parallel=F,nCpus=nCpus)
				
				for(j in 1:length(lambda.path)){
					tmp=ghat.path$raw[,,j]
					tmp[abs(tmp)<1e-06]=0
					tmp[abs(tmp)>1e-06]=1
					diag(tmp)=0
					if(is.null(ghat.path$path[[j]])){
						ghat.path$path[[j]]=tmp;
					}else{
						ghat.path$path[[j]]=ghat.path$path[[j]]+tmp	
					}
						
				}	
			
			
		})
		# for(i in 1:N){
				# cat(paste("LLGM: Conducting sampling ... in progress: ", floor(100*(i/N)), "%", collapse=""),"\r")
				# flush.console()
				# index = sample(1:ncol(X),b,replace=F)
				# #tmp=glmpois(X[,index],lambda.path[j],parallel=parallel,warmStart=warmStart,nCpus=nCpus)
				# ghat.path$raw= glmpois(X[,index],lambda=lambda.path,parallel=parallel,nCpus=nCpus)
				
				# for(j in 1:length(lambda.path)){
					# tmp=ghat.path$raw[,,j]
					# tmp[abs(tmp)<1e-06]=0
					# tmp[abs(tmp)>1e-06]=1
					# diag(tmp)=0
					# if(is.null(ghat.path$path[[j]])){
						# ghat.path$path[[j]]=tmp;
					# }else{
						# ghat.path$path[[j]]=ghat.path$path[[j]]+tmp	
					# }
						
				# }	
		# }
		
		for(i in 1:length(lambda.path)){
			D=ghat.path$path[[i]]
			D=D/N
			D=2*D*(1-D)
			v=c(v,mean(D[upper.tri(D)]))	
		}
					
		
		v=cummax(v)
		ghat$v=v
		ghat$lambda.path = lambda.path
		ghat$opt.lambda = lambda.path[which(v==max(v[v<beta]))]	
			
		return(ghat)
	}
	
	
	if(method=="star" & link=="log" & warmStart == F){
		b = min(c(10*sqrt(ncol(X)), 0.8*ncol(X))) 
		ghat=list()
		v=c()
		
		
		for( j in 1:length(lambda.path)){
			cat(paste("Conducting sampling ... in progress: ", floor(100*(j/length(lambda.path))), "%", collapse=""),"\r")
			flush.console()
			D=matrix(0,nrow=nrow(X),ncol=nrow(X))
			for(i in 1:N){
				index = sample(1:ncol(X),b,replace=F)
				tmp=glmpois(X[,index],lambda.path[j],parallel=F)
				tmp[abs(tmp)<1e-06]=0
				tmp[abs(tmp)>1e-06]=1
				D=D+tmp
			}
			D=D/N
			D=2*D*(1-D)
			v=c(v,mean(D[upper.tri(D)]))			
		}
		v=cummax(v)
		ghat$v=v
		ghat$lambda.path = lambda.path
		ghat$opt.lambda = lambda.path[which(v==max(v[v<beta]))]	
			
		return(ghat)
	}
		
}



glmpois.path <-function(X,lambda.path=NULL, delta= 0.1, parallel = F, warmStart=T,link="log",nLambda=100,nCpus=4){
	
	
	ghat.path=c()
	ghat.path$raw=list()
	ghat.path$path=list()	
	
	
	if(is.null(lambda.path) ){
		maxlambda = myglmnet.max(X,link=link, delta = delta)
		lambda.path = exp(seq(log(maxlambda),log(0.01*maxlambda), length = nLambda))
	}

	
	if(warmStart==F){
		for(i in 1:length(lambda.path)){
		tmp=glmpois(X,lambda.path[i],parallel=parallel)
		ghat.path$raw[[i]]=tmp
		
		tmp[abs(tmp)<1e-06]=0
		tmp[abs(tmp)>1e-06]=1
		diag(tmp)=0
		ghat.path$path[[i]]=tmp
		
		}
	}
	if(warmStart==T){
		
		ghat.path$raw = glmpois(X,lambda=lambda.path,parallel=parallel,nCpus=nCpus)
		for(i in 1:length(lambda.path)){
			tmp=ghat.path$raw[,,i]
			tmp[abs(tmp)<1e-06]=0
			tmp[abs(tmp)>1e-06]=1
			diag(tmp)=0
			ghat.path$path[[i]]=tmp	
		}
	}
			
	return(ghat.path);	
	
}




# # ### poisson based mb neigbhorhood selection with X p by n and fixed lambda for all. 

# glmpois <- function(X,lambda,parallel=F, nCpus = 4){
	
	# if(length(lambda)>1){
	
	# ghat = array(0,dim=c(nrow(X),nrow(X),length(lambda)))
	
	# if(parallel){
		# library(snowfall)
		# sfInit(parallel=T,cpus=nCpus)
		
		# sfExport("X",local=T)
		# sfExport("ghat",local=T)
		# sfExport("lambda",local=T)
		# sfLibrary(glmnet)
		# wrapper <- function(i){
			# fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			# fit$beta=as.matrix(fit$beta)
			# if(i==1){
				# ghat[i,2:nrow(X),]=fit$beta
			# }else if(i==nrow(X)){
				# ghat[i,1:(nrow(X)-1),]=fit$beta

			# }else{
				# ghat[i,1:(i-1),]=fit$beta[1:(i-1),]
				# ghat[i,(i+1):nrow(X),]=fit$beta[i:nrow(fit$beta),]	
			# }
			# return(ghat[i,,])
		# }
		# sfExport("wrapper")		
		# ghat2=sfLapply(1:nrow(X),wrapper)	
		# sfStop()
		# for(i in 1:nrow(X)){
			# ghat[i,,]=ghat2[[i]]
		# }
		
		# return(ghat)

	# }
	# if(parallel==F)
	# {
		# wrapper <- function(i){
			# fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			# fit$beta=as.matrix(fit$beta)
			# if(i==1){
				# ghat[i,2:nrow(X),]=fit$beta
			# }else if(i==nrow(X)){
				# ghat[i,1:(nrow(X)-1),]=fit$beta

			# }else{
				# ghat[i,1:(i-1),]=fit$beta[1:(i-1),]
				# ghat[i,(i+1):nrow(X),]=fit$beta[i:nrow(fit$beta),]	
			# }
			# return(ghat[i,,])
		# }
		# ghat2=lapply(1:nrow(X),wrapper)	
		# for(i in 1:nrow(X)){
			# ghat[i,,]=ghat2[[i]]
		# }
		# return(ghat)

	# }
	
	
	
	
	
	# }
	
	
	
	# if(length(lambda ==1)){
		
		# ghat=matrix(0,nrow=nrow(X),ncol=nrow(X))
		# if(parallel){
		# library(snowfall)
		# sfInit(cpus=nCpus)
		
		# sfExport("X",local=T)
		# sfExport("ghat",local=T)
		# sfLibrary(glmnet)
		# #modify ghat
		# wrapper <- function(i){
			# fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			# fit$beta=as.numeric(fit$beta)
			# if(i==1){
				# ghat[i,2:nrow(X)]=fit$beta
			# }else if(i==nrow(X)){
				# ghat[i,1:(nrow(X)-1)]=fit$beta

			# }else{
				# ghat[i,1:(i-1)]=fit$beta[1:(i-1)]
				# ghat[i,(i+1):nrow(X)]=c(fit$beta[i:length(fit$beta)])	
			# }
			# return(ghat[i,])
		# }
		# sfExport("wrapper")
		# ghat=sfSapply(1:nrow(X),wrapper)	
		# sfStop()
		# return(ghat)
	# }
	
	
	# for(i in 1:nrow(X)){
	# fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
	# fit$beta=as.numeric(fit$beta)
	# if(i==1){
		# ghat[i,2:nrow(X)]=fit$beta
	# }else if(i==nrow(X)){
		# ghat[i,1:(nrow(X)-1)]=fit$beta

	# }else{
		# ghat[i,1:(i-1)]=fit$beta[1:(i-1)]
		# ghat[i,(i+1):nrow(X)]=c(fit$beta[i:length(fit$beta)])	
		# }
		
	# }
	# return(ghat)
	# }
	

# }

### poisson based mb neigbhorhood selection with X p by n and fixed lambda for all. 

glmpois <- function(X,lambda,parallel=F, nCpus = 4){
	
	if(length(lambda)>1){
	
	ghat = array(0,dim=c(nrow(X),nrow(X),length(lambda)))
	
	if(parallel){
#		library(snowfall)
#		sfInit(parallel=T,cpus=nCpus)
#		
#		sfExport("X",local=T)
#		sfExport("ghat",local=T)
#		sfExport("lambda",local=T)
#		sfLibrary(glmnet)
		wrapper <- function(i){
			fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			fit$beta=as.matrix(fit$beta)
			if(i==1){
				ghat[i,2:nrow(X),]=fit$beta
			}else if(i==nrow(X)){
				ghat[i,1:(nrow(X)-1),]=fit$beta

			}else{
				ghat[i,1:(i-1),]=fit$beta[1:(i-1),]
				ghat[i,(i+1):nrow(X),]=fit$beta[i:nrow(fit$beta),]	
			}
			return(ghat[i,,])
		}
#		sfExport("wrapper")		
#		ghat2=sfLapply(1:nrow(X),wrapper)	
#		sfStop()
		library(multicore)
		ghat2=mclapply(1:nrow(X),wrapper)	
		
		
		for(i in 1:nrow(X)){
			ghat[i,,]=ghat2[[i]]
		}
		
		return(ghat)

	}
	if(parallel==F)
	{
		wrapper <- function(i){
			fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			fit$beta=as.matrix(fit$beta)
			if(i==1){
				ghat[i,2:nrow(X),]=fit$beta
			}else if(i==nrow(X)){
				ghat[i,1:(nrow(X)-1),]=fit$beta

			}else{
				ghat[i,1:(i-1),]=fit$beta[1:(i-1),]
				ghat[i,(i+1):nrow(X),]=fit$beta[i:nrow(fit$beta),]	
			}
			return(ghat[i,,])
		}
		ghat2=lapply(1:nrow(X),wrapper)	
		for(i in 1:nrow(X)){
			ghat[i,,]=ghat2[[i]]
		}
		return(ghat)

	}
	
	
	
	
	
	}
	
	
	
	if(length(lambda ==1)){
		
		ghat=matrix(0,nrow=nrow(X),ncol=nrow(X))
		if(parallel){
		library(snowfall)
		sfInit(cpus=nCpus)
		
		sfExport("X",local=T)
		sfExport("ghat",local=T)
		sfLibrary(glmnet)
		#modify ghat
		wrapper <- function(i){
			fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
			fit$beta=as.numeric(fit$beta)
			if(i==1){
				ghat[i,2:nrow(X)]=fit$beta
			}else if(i==nrow(X)){
				ghat[i,1:(nrow(X)-1)]=fit$beta

			}else{
				ghat[i,1:(i-1)]=fit$beta[1:(i-1)]
				ghat[i,(i+1):nrow(X)]=c(fit$beta[i:length(fit$beta)])	
			}
			return(ghat[i,])
		}
		sfExport("wrapper")
		ghat=sfSapply(1:nrow(X),wrapper)	
		sfStop()
		return(ghat)
	}
	
	
	for(i in 1:nrow(X)){
	fit=glmnet(t(X[-i,]),X[i,],family="poisson",lambda= lambda)
	fit$beta=as.numeric(fit$beta)
	if(i==1){
		ghat[i,2:nrow(X)]=fit$beta
	}else if(i==nrow(X)){
		ghat[i,1:(nrow(X)-1)]=fit$beta

	}else{
		ghat[i,1:(i-1)]=fit$beta[1:(i-1)]
		ghat[i,(i+1):nrow(X)]=c(fit$beta[i:length(fit$beta)])	
		}
		
	}
	return(ghat)
	}
	

}





my.huge.mb<-function (x, lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, 
    scr = NULL, scr.num = NULL, idx.mat = NULL, sym = "or", verbose = TRUE,link=NULL) 
{
    gcinfo(FALSE)
    n = nrow(x)
    d = ncol(x)
    fit = list()
    fit$cov.input = isSymmetric(x)
    if (fit$cov.input) {
        if (verbose) 
            cat("The input is identified as the covriance matrix.\n")
        S = cov2cor(x)
    }
    if (!fit$cov.input) {
        x = scale(x)
        S = cor(x)
    }
    rm(x)
    gc()
    if (is.null(idx.mat)) {
        if (is.null(scr)) 
            scr = FALSE
        if (scr) {
            if (is.null(scr.num)) {
                if (n < d) 
                  scr.num = n - 1
                if (n >= d) {
                  if (verbose) 
                    cat("lossy screening is skipped without specifying scr.num.\n")
                  scr = FALSE
                }
            }
        }
        fit$scr = scr
    }
    if (!is.null(idx.mat)) {
        scr = TRUE
        fit$scr = scr
        scr.num = nrow(idx.mat)
    }
    if (!is.null(lambda)) 
        nlambda = length(lambda)
    if (is.null(lambda)) {
        if (is.null(nlambda)) 
            nlambda = 10
        if (is.null(lambda.min.ratio)) 
            lambda.min.ratio = 0.1
        lambda.max = max(max(S - diag(d)), -min(S - diag(d)))
        lambda.min = lambda.min.ratio * lambda.max
        lambda = exp(seq(log(lambda.max), log(lambda.min), length = nlambda))
        rm(lambda.max, lambda.min, lambda.min.ratio)
        gc()
    }
    maxdf = min(d, n)
        
    
    if(link=="Poisson"){
    	
    	
    	
    	
    	
    }
    
    
    
    if (scr) {
        if (verbose) {
            cat("Conducting Meinshausen & Buhlmann graph estimation (mb) with lossy screening....")
            flush.console()
        }
        if (is.null(idx.mat)) 
            idx.mat = apply(-abs(S), 2, order)[2:(scr.num + 1), 
                ] - 1
        fit$idx.mat = idx.mat
        out = .C("SPMBscr", S = as.double(S), idx_scr = as.integer(idx.mat), 
            lambda = as.double(lambda), nnlambda = as.integer(nlambda), 
            dd = as.integer(d), nnscr = as.integer(scr.num), 
            x = as.double(rep(0, d * maxdf * nlambda)), col_cnz = as.integer(rep(0, 
                d + 1)), row_idx = as.integer(rep(0, d * maxdf * 
                nlambda)), PACKAGE = "huge")
        for (i in 1:d) {
            if (out$col_cnz[i + 1] > out$col_cnz[i]) {
                idx.tmp = (out$col_cnz[i] + 1):out$col_cnz[i + 
                  1]
                ord = order(out$row_idx[idx.tmp])
                out$row_idx[idx.tmp] = out$row_idx[ord + out$col_cnz[i]]
                out$x[idx.tmp] = out$x[ord + out$col_cnz[i]]
            }
        }
    }
    if (!scr) {
        if (verbose) {
            cat("Conducting Meinshausen & Buhlmann graph estimation (mb)....")
            flush.console()
        }
        fit$idx_mat = NULL
        out = .C("SPMBgraph", S = as.double(S), lambda = as.double(lambda), 
            nnlambda = as.integer(nlambda), dd = as.integer(d), 
            x = as.double(rep(0, d * maxdf * nlambda)), col_cnz = as.integer(rep(0, 
                d + 1)), row_idx = as.integer(rep(0, d * maxdf * 
                nlambda)), PACKAGE = "huge")
        for (i in 1:d) {
            if (out$col_cnz[i + 1] > out$col_cnz[i]) {
                idx.tmp = (out$col_cnz[i] + 1):out$col_cnz[i + 
                  1]
                ord = order(out$row_idx[idx.tmp])
                out$row_idx[idx.tmp] = out$row_idx[ord + out$col_cnz[i]]
                out$x[idx.tmp] = out$x[ord + out$col_cnz[i]]
            }
        }
    }
    
     
    
    G = new("dgCMatrix", Dim = as.integer(c(d * nlambda, d)), 
        x = as.vector(out$x[1:out$col_cnz[d + 1]]), p = as.integer(out$col_cnz), 
        i = as.integer(out$row_idx[1:out$col_cnz[d + 1]]))
    fit$beta = list()
    fit$path = list()
    fit$df = matrix(0, d, nlambda)
    fit$rss = matrix(0, d, nlambda)
    fit$sparsity = rep(0, nlambda)
    for (i in 1:nlambda) {
        fit$beta[[i]] = G[((i - 1) * d + 1):(i * d), ]
        fit$path[[i]] = abs(fit$beta[[i]])
        fit$df[, i] = apply(sign(fit$path[[i]]), 2, sum)
        if (sym == "or") 
            fit$path[[i]] = sign(fit$path[[i]] + t(fit$path[[i]]))
        if (sym == "and") 
            fit$path[[i]] = sign(fit$path[[i]] * t(fit$path[[i]]))
        fit$sparsity[i] = sum(fit$path[[i]])/d/(d - 1)
    }
    rm(G, idx.tmp, ord, out)
    fit$lambda = lambda
    if (verbose) {
        cat("done\n")
        flush.console()
    }
    rm(verbose, nlambda)
    gc()
    return(fit)
}




figroc<-function(fname ="default.pdf",...){
	# lambda = 1
	# lambda.c = 0.01

	# n=200  ## number of observations
	# p=50 # number nodes

	# generate the graph into mydata
	# the data is in mydata$X
	# the graph structure is in mydata$B
	mydata = mp.generator(lambda,lambda.c,n,p,type=type)
	#myplot(mydata$B)
	
	#mydata$layout = myplot(mydata$B)

	pdf(file = fname)

	ghat=huge(t(mydata$X),method="glasso",nlambda=100)
	ghat.roc=myroc(mydata$B,ghat$path,add=F, col = "dark gray")

	## glass (log (1+ data))

	ghat2 = huge(t(log(1+mydata$X)),method="glasso",nlambda=100)
	ghat2.roc=myroc(mydata$B,ghat2$path,add=T, col="gray" )

	### LLGM
	#ghat$lambda=seq(2*max(ghat$lambda),min(ghat$lambda)/100,length=100)
	ghat3 = glmpois.path(mydata$X,warmStart=T,parallel=F,nCpus=8,nLambda=100)
	ghat3.roc=myroc(mydata$B,ghat3$path,add=T, col="black" )

	mtext(paste("lambda ", format(mean(c(mydata$X)),digits=2),sep=""))
	
	dev.off()
	
	return(rbind(ghat.roc,ghat2.roc,ghat3.roc))
}


figroc_sum<-function(epic = 10, add=F, ...){

roc.sum=lapply(1:epic,figroc)
tmp=0

for(i in 1:length(roc.sum)){
	
	tmp=roc.sum[[i]]+tmp
	
}
tmp = tmp / length(roc.sum)

	par(lwd = 2)
	if(!add){
		plot(tmp[2,],tmp[1,],col="white",xlim=c(0,0.5),ylim=c(0,1),cex=0.5,xlab="False positive rate", ylab="True positive rate")
		legend("bottomright",legend=c("GLASSO","Log-GLASSO","LLGM"), col=c("dark gray","light gray","black"),lty=c(1,2,4),lwd=2)

	}
	lines(tmp[2,],tmp[1,],col="dark gray",lty=1,pch=15)
	
	#points(tmp[4,],tmp[3,],col="dark gray",pch=15,cex=0.5)
	lines(tmp[4,],tmp[3,],col="light gray",lty=2,pch=16)

	#points(tmp[6,],tmp[5,],col="black",pch=16,cex=0.5)
	lines(tmp[6,],tmp[5,],col="black",lty=4,pch=17)
	return(tmp);
}	








































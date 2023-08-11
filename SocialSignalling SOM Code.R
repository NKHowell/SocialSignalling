##########CODE FOR SOCIAL SIGNALLING CORRELATES OF MAMMAL COLOURATION
########CREATED: 15/02/22
######LAST EDITED: 11/08/23

#Set the working directory
setwd("C:/Users/natas/Documents/PhD/Data Analysis/")

#Read in the data
x<-read.csv("ssdata.csv")
#There should be 4,411 spp.

#Trim out any of the necessary variables with missing data
##ONLY use lines relevant for variables being tested in the model
if(sum(is.na(x$Head.Pattern))>0){x<-x[-which(is.na(x$Head.Pattern)),]}
if(sum(is.na(x$Num.Head.Cols))>0){x<-x[-which(is.na(x$Num.Head.Cols)),]}
if(sum(is.na(x$Pri.Head.Col))>0){x<-x[-which(is.na(x$Pri.Head.Col)),]}
if(sum(is.na(x$Sec.Head.Col))>0){x<-x[-which(is.na(x$Sec.Head.Col)),]}
if(sum(is.na(x$Chest.Bib))>0){x<-x[-which(is.na(x$Chest.Bib)),]}
if(sum(is.na(x$Countershaded))>0){x<-x[-which(is.na(x$Countershaded)),]}
if(sum(is.na(x$Sec.Body.Col))>0){x<-x[-which(is.na(x$Sec.Body.Col)),]}
if(sum(is.na(x$Rump.Patch))>0){x<-x[-which(is.na(x$Rump.Patch)),]}
if(sum(is.na(x$Pri.Rump.Col))>0){x<-x[-which(is.na(x$Pri.Rump.Col)),]}
if(sum(is.na(x$Tail.Pattern))>0){x<-x[-which(is.na(x$Tail.Pattern)),]}
if(sum(is.na(x$Num.Tail.Cols))>0){x<-x[-which(is.na(x$Num.Tail.Cols)),]}
if(sum(is.na(x$Pri.Tail.Col))>0){x<-x[-which(is.na(x$Pri.Tail.Col)),]}
if(sum(is.na(x$Sec.Tail.Col))>0){x<-x[-which(is.na(x$Sec.Tail.Col)),]}
if(sum(is.na(x$Head.Dichromatism))>0){x<-x[-which(is.na(x$Head.Dichromatism)),]}
if(sum(is.na(x$Body.Dichromatism))>0){x<-x[-which(is.na(x$Body.Dichromatism)),]}
if(sum(is.na(x$Activity.Cycle))>0){x<-x[-which(is.na(x$Activity.Cycle)),]}
if(sum(is.na(x$Social.Group.Size))>0){x<-x[-which(is.na(x$Social.Group.Size)),]}
if(sum(is.na(x$Dimorphism.Magnitude))>0){x<-x[-which(is.na(x$Dimorphism.Magnitude)),]}
if(sum(is.na(x$Mating.System))>0){x<-x[-which(is.na(x$Mating.System)),]}
if(sum(is.na(x$Species.Overlap))>0){x<-x[-which(is.na(x$Species.Overlap)),]}

#Check sample size is big enough in focal orders
sum(x$Order=="Afrosoricida")
sum(x$Order=="Carnivora")
sum(x$Order=="Cetartiodactyla")
sum(x$Order=="Dasyuromorphia")
sum(x$Order=="Didelphimorphia")
sum(x$Order=="Diprotodontia")
sum(x$Order=="Eulipotyphla")
sum(x$Order=="Lagomorpha")
sum(x$Order=="Primates")
sum(x$Order=="Rodentia")

#Cut the data down to just the focal taxonomic group(s) - changeable line
x<-x[which(x$Order=="Rodentia"),]

#Re-code the categorical variables
N<-dim(x)[1]
x$HP<-rep(0,N)
x$HP[which(x$Head.Pattern==1)]<-1
x$HP[which(x$Head.Pattern==2)]<-1
x$HP[which(x$Head.Pattern==3)]<-1

x$NHC<-rep(0,N)
x$NHC[which(x$Num.Head.Cols>1)]<-1

x$PHcon1<-rep(0,N)
x$PHcon1[which(x$Pri.Head.Col=="A1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="B1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="C1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="D1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="E1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="F1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="G1")]<-1
x$PHcon5<-rep(0,N)
x$PHcon5[which(x$Pri.Head.Col=="B5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="C5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="D5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="E5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="F5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="G5")]<-1
x$SHcon1<-rep(0,N)
x$SHcon1[which(x$Sec.Head.Col=="A1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="B1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="C1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="D1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="E1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="F1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="G1")]<-1
x$SHcon5<-rep(0,N)
x$SHcon5[which(x$Sec.Head.Col=="B5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="C5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="D5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="E5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="F5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="G5")]<-1
x$HCongen<-rep(0,N)
x$HCongen[which(x$PHcon1==1|x$SHcon5==1)]<-1
x$HCongen[which(x$PHcon5==1|x$SHcon1==1)]<-1

x$CB<-rep(0,N)
x$CB[which(x$Chest.Bib==1)]<-1

x$CS<-rep(0,N)
x$CS[which(x$Countershaded==3)]<-1
x$CSA<-rep(0,N)
x$CSA[which(x$Sec.Body.Col=="A1")]<-1
x$CST<-rep(0,N)
x$CST[which(x$CS==1|x$CSA==1)]<-1

x$RP<-rep(0,N)
x$RP[which(x$Rump.Patch==1)]<-1
x$RP[which(x$Rump.Patch==2)]<-1
x$RP[which(x$Rump.Patch==3)]<-1
x$RC<-rep(0,N)
x$RC[which(x$Pri.Rump.Col=="A1")]<-1
x$Rump<-rep(0,N)
x$Rump[which(x$RP==1|x$RC==1)]<-1

x$TP<-rep(0,N)
x$TP[which(x$Tail.Pattern==1)]<-1
x$TP[which(x$Tail.Pattern==2)]<-1
x$TP[which(x$Tail.Pattern==3)]<-1
x$TP[which(x$Tail.Pattern==4)]<-1
x$TP[which(x$Tail.Pattern==5)]<-1
x$TP[which(x$Tail.Pattern==6)]<-1

x$NTC<-rep(0,N)
x$NTC[which(x$Num.Tail.Cols>1)]<-1

x$PTcon1<-rep(0,N)
x$PTcon1[which(x$Pri.Tail.Col=="A1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="B1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="C1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="D1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="E1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="F1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="G1")]<-1
x$PTcon5<-rep(0,N)
x$PTcon5[which(x$Pri.Tail.Col=="B5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="C5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="D5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="E5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="F5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="G5")]<-1
x$STcon1<-rep(0,N)
x$STcon1[which(x$Sec.Tail.Col=="A1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="B1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="C1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="D1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="E1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="F1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="G1")]<-1
x$STcon5<-rep(0,N)
x$STcon5[which(x$Sec.Tail.Col=="B5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="C5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="D5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="E5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="F5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="G5")]<-1
x$TCongen<-rep(0,N)
x$TCongen[which(x$PTcon1==1|x$STcon5==1)]<-1
x$TCongen[which(x$PTcon5==1|x$STcon1==1)]<-1

x$HD<-rep(0,N)
x$HD[which(x$Head.Dichromatism==1)]<-1
x$BD<-rep(0,N)
x$BD[which(x$Body.Dichromatism==1)]<-1
x$Dich<-rep(0,N)
x$Dich[which(x$HD==1|x$BD==1)]<-1

x$AC<-rep(0,N)
x$AC[which(x$Activity.Cycle==3)]<-1
x$AC[which(x$Activity.Cycle==32)]<-1
x$AC[which(x$Activity.Cycle==13)]<-1

x$NAC<-rep(0,N)
x$NAC[which(x$Activity.Cycle==1)]<-1
x$NAC[which(x$Activity.Cycle==12)]<-1
x$NAC[which(x$Activity.Cycle==13)]<-1

x$SGS<-rep(0,N)
x$SGS[which(x$Social.Group.Size==3)]<-1
x$SGS[which(x$Social.Group.Size==4)]<-1
x$SGS[which(x$Social.Group.Size==5)]<-1

x$NSGS<-rep(0,N)
x$NSGS[which(x$Social.Group.Size==1)]<-1
x$NSGS[which(x$Social.Group.Size==2)]<-1

x$Dimo<-rep(0,N)
x$Dimo[which(x$Dimorphism.Magnitude>=10)]<-1

x$NDimo<-rep(0,N)
x$NDimo[which(x$Dimorphism.Magnitude<=9)]<-1

x$MS<-rep(0,N)
x$MS[which(x$Mating.System==2)]<-1
x$MS[which(x$Mating.System==4)]<-1
x$MS[which(x$Mating.System==5)]<-1
x$MS[which(x$Mating.System==6)]<-1
x$MS[which(x$Mating.System==8)]<-1
x$MS[which(x$Mating.System==9)]<-1
x$MS[which(x$Mating.System==10)]<-1

x$NMS<-rep(0,N)
x$NMS[which(x$Mating.System==1)]<-1
x$NMS[which(x$Mating.System==3)]<-1
x$NMS[which(x$Mating.System==5)]<-1
x$NMS[which(x$Mating.System==7)]<-1
x$NMS[which(x$Mating.System==8)]<-1
x$NMS[which(x$Mating.System==9)]<-1
x$NMS[which(x$Mating.System==10)]<-1

x$SO<-rep(0,N)
x$SO[which(x$Species.Overlap==1)]<-1

x$NSO<-rep(0,N)
x$NSO[which(x$Species.Overlap==0)]<-1

#Install and open the necessary packages
install.packages("MCMCglmm")
install.packages("phangorn")
install.packages("beepr")
library(MCMCglmm)
library(phangorn)
library(beepr)

#Read in 1,000 random tree topologies, cut down to 100, then down to 1
trees<-read.nexus("trees1k.nex")
t100<-trees[1:100]
tree<-t100[[1]]
beep(2)

#Create csv of tip labels so you can see taxonomy of tree
tree$tip.label
treeforcsv<-data.frame(tree$tip.label)
write.csv(treeforcsv, file="tree tips trimmed.csv")

#Make sure the taxonomy of the dataset matches that of the tree
x$NewName<-paste(gsub(" ","_",x$Binomial),toupper(x$Family),toupper(x$Order),sep="_")

#Make sure that there isn't a species in the dataset that's missing from the tree
bad<-rep(0,dim(x)[1])
for(i in 1:dim(x)[1]){
  if(sum(x$NewName[i]==tree$tip.label)==0){bad[i]=1}
}
if(sum(bad)>0){x<-x[-which(bad==1),]}

#Trim out species from the tree that are not in the dataset
t100<-lapply(t100,drop.tip,tip=setdiff(tree$tip.label,x$NewName))

#Force the tree to be ultrametric
i=1
tree<-t100[[i]] 
tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
animalA<-inverseA(tree)$Ainv

#Calculate the priors
##The '9' in 'mu=rep(0,9)' MUST be calculated according to the variables appearing in each model
###Calculation as follows: 1 + (number of continuous variables) + (k-1 for each categorical variable), where 'k' is the number of categories in the variable
####Variables that appear after 'V=gelman.prior(~' can be added/removed depending on what model is being run
prior.Test<-list(B=list(mu=rep(0,6),V=gelman.prior(~AC+SGS+Dimo+MS+SO, data = x,  scale=1+pi^2/3)),R=list(V=1,fix=1),G=list(G1=list(V=1E-10,nu=-1)))

#Conduct a dummy run to set up the structure of the model and get the starting point
Final.disp<-MCMCglmm(TP~AC+SGS+Dimo+MS+SO, 
                     random=~NewName, 
                     ginverse=list(NewName=animalA), 
                     prior = prior.Test, 
                     verbose=TRUE,
                     family="categorical", 
                     data = x,
                     nitt=11000,
                     thin=10,
                     burnin=1000,
                     pl=TRUE,
                     pr=TRUE,
                     slice=TRUE)

#Set up the starting point from the dummy run
nsamp.l<-nrow(Final.disp$VCV)
start1.l=list(R=Final.disp$VCV[nsamp.l,"units"], G=list(G1=Final.disp$VCV[nsamp.l,"NewName"]))

#Save the dummy run as a .Rdata file
save(Final.disp,file="RodTP-dummyrun.Rdata")

#Run the true model over 100 tree topologies
for(i in 1:100){
  tree<-t100[[i]]  
  tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
  
  animalA<-inverseA(tree)$Ainv
  
  mod<-MCMCglmm(TP~AC+SGS+Dimo+MS+SO,  
                random=~NewName, 
                ginverse=list(NewName=animalA), 
                prior = prior.Test, 
                verbose=TRUE,
                family="categorical", 
                start= start1.l,
                data = x,
                nitt=11000,  
                thin=1000, 
                burnin=1000, 
                pl=TRUE, 
                pr=TRUE, 
                slice=TRUE)
  print(i)
  
  Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
  Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
  Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 
  
  nsamp.l<-nrow(mod$VCV)
  start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"NewName"]))
  
  save(Final.disp,file="RodTP.Rdata")
  
}
beep(5)

#See the model summary
summary(Final.disp)

#See plots of the model distribution
par("mar")
par(mar=c(1,1,1,1))
plot(Final.disp)

#Make sure to save the model as a .Rdata file
save(Final.disp,file="RodTP.Rdata")

#Reset the working directory to check model VIFs (if you've moved Rdata file to different location)
setwd("C:/Users/natas/Documents/PhD/Data Analysis/R Outputs/RData Files/Social Signalling")

#Load in the .Rdata file for the relevant model
load("RodTP.Rdata")

#Create the VIF check function
vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
  nF <- fit$Fixed$nfl
  v <- cov(as.matrix(fit$X[,1:nF]))
  nam <- colnames(fit$Sol[,1:nF])
  v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
  nam <- nam[-intercept.columns]
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#Run the VIF check
vif.MCMCglmm(Final.disp)

#View summary of model when typing up results
summary(Final.disp)
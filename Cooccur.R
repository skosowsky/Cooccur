det<-vector(length=253)
for(i in 1:253){
if(cooc.p[i,4]<.025){
det[i]="Positive"}
	else if(cooc.p[i,4]>.975){
	det[i]="Negative"}
else if(cooc.p[i,4]>.025&cooc.p[i,4]<.975){
det[i]="Random"}}
cooc.L<-as.data.frame(cbind(cooc.p,det))



colmns<-c(0,count(as.numeric(substr(colnames(Abund),5,5)))[,2])
KARA.list<-list()
KARA.cooc<-list()
sumcols<-vector(length=8)
for(i in 2:8){
    sumcols[1]=1
    sumcols[i]=sum(colmns[1:i])
    KARA.mat=matrix(nrow=443,ncol=colmns[i])
    KARA.list[[i-1]]<-as.data.frame(cbind(Abund[,(sumcols[i-1]):sumcols[i]]))
    KARA.cooc[[i-1]]<-cooccur(KARA.list[[i-1]],type="spp_site",thresh=T,spp_names=T)
    }

qplot(as.factor(cooc.L[,2]),as.factor(cooc.L[,1]),col=as.factor(cooc.L$det),xlab="sp2",ylab="sp1",
main="Species Co-occurrence Matrix")+scale_colour_manual(values=c("red","green","black"))+geom_point(aes(size=1))


c.KARA<-list()
for(i in 1:7){
c.KARA[[i]]<-as.data.frame(cbind(print(KARA.cooc[[i]])[,1],print(KARA.cooc[[i]])[,2],print(KARA.cooc[[i]])[,8],print(KARA.cooc[[i]])[,9]))
colnames(c.KARA[[i]])<-c("sp1","sp2","p lt","p gt")
}


K.KARA<-list()
for(j in 1:7){
det.KARA<-vector(length=dim(c.KARA[[j]])[1])
    for(i in 1:dim(c.KARA[[j]])[1]){
        if(c.KARA[[j]][i,4]<.025){
            det.KARA[i]="Positive"}
        else if(c.KARA[[j]][i,4]>.975){
            det.KARA[i]="Negative"}
        else if(c.KARA[[j]][i,4]>.025&c.KARA[[j]][i,4]<.975){
            det.KARA[i]="Random"}}
K.KARA[[j]]<-as.data.frame(cbind(c.KARA[[j]],det.KARA))}

for(j in 1:7){
K.KARA[[j]]<-as.data.frame(cbind(K.KARA[[j]],rep(j,dim(K.KARA[[j]])[1])))
colnames(K.KARA[[j]])<-c("sp1","sp2","p lt","p gt","Cooccurrence","Week")}

pdat<-matrix(nrow=437,ncol=6)
rowcount<-vector(length=8)
for(i in 1:7){
rowcount[1]=0
rowcount[i+1]<-rowcount+dim(K.KARA[[i]])[1]
pdat[(rowcount[i]+1):rowcount[i+1],]<-K.KARA[[i]]}





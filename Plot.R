Data <- Get_Data(file="Data/CorrosionData.csv")
RealData = Data[["Original"]]
t.data = Data[["Location"]]
New_Data <- Data_Preprocess(RealData,t.data)
Model_Data <- Data_Modeling(New_Data,seed,k,RealData)
model <- Model_Data[["Model"]]
print(model)
print(summary(model))
print(parameters(model))
results <- Model_Data[["Label_data"]]



# plot all data 
plot(-1,1,xlab="Depth (cm)",ylab="Chloride concentration (kg/m3)"
     ,xlim=c(0,2.3),ylim=c(0,60),cex.axis=1.8,cex.lab=2.2,font.main=1,xaxt='n',yaxt='n')
axis(side=1,at=c(0,1,2,3,4,5,6)/2.54,labels = c(0,1,2,3,4,5,6),cex.axis=1.5)  
axis(side=2,at=c(0,5,10,15,20,25,30,35)/0.594,labels = c(0,5,10,15,20,25,30,35),cex.axis=1.5)  
for(i in 1:148)
{
  points(t.data[i,],RealData[i,9:14],cex=0.7)
}
lines(t.data[69,],RealData[69,9:14],type='l',col=1,lwd=5,lty=2)
lines(t.data[76,],RealData[76,9:14],type='l',col=2,lwd=5,lty=1)
lines(t.data[77,],RealData[77,9:14],type='l',col=3,lwd=5,lty=5)
lines(t.data[143,],RealData[143,9:14],type='l',col=4,lwd=5,lty=4)
lines(t.data[139,],RealData[139,9:14],type='l',col=5,lwd=5,lty=3)
legend("topright",legend=c("Bridge1-1", "Bridge1-2","Bridge1-3","Bridge2-1","Bridge2-2"),lty=c(2,1,5,4,3),col=c(1,2,3,4,5),lwd=5,cex=1.5,bty='n')

#Plot the modeling curves
tt=seq(0,10,0.1)
for(k in 1:7){
  plot(-1,1,xlab="Depth (cm)",ylab="Chloride concentration (kg/m3)"
       ,xlim=c(0,3),ylim=c(0,60),cex.axis=1.8,cex.lab=2.2,font.main=1,xaxt='n',yaxt='n')
  axis(side=1,at=c(0,1,2,3,4,5,6,7)/2.54,labels = c(0,1,2,3,4,5,6,7),cex.axis=1.5)  
  axis(side=2,at=c(0,5,10,15,20,25,30,35)/0.594,labels = c(0,5,10,15,20,25,30,35),cex.axis=1.5)  
  for(i in 1:148)
  {
    if(results[i,7]==k)
      points(t.data[i,],results[i,1:6])
  }
  
  lines(tt,exp(parameters(m1)[1,k]+tt*parameters(m1)[2,k]+tt^2*parameters(m1)[3,k]+tt^3*parameters(m1)[4,k]),type='l',col="red",lwd=3)
  lines(tt,exp(parameters(m1)[1,k]+tt*parameters(m1)[2,k]+tt^2*parameters(m1)[3,k]+tt^3*parameters(m1)[4,k]+2*parameters(m1)[5,k]),type='l',col="red",lty=2,lwd=3)
  lines(tt,exp(parameters(m1)[1,k]+tt*parameters(m1)[2,k]+tt^2*parameters(m1)[3,k]+tt^3*parameters(m1)[4,k]-2*parameters(m1)[5,k]),type='l',col="red",lty=2,lwd=3)
  legend("topright",legend=c("Estimated trajectory","Variability bounds"),lty=c(1,2),col="red",lwd=3,cex=2,bty='n')
}
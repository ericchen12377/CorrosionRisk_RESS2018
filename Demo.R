Get_Data <- function(file){
  #Original corrosion data
  RealData=read.csv(file=file)
  #measure location data
  ta=c(NA,0.11,0.38,0.75,1.50,NA)
  tb=c(NA,0.11,0.38,0.75,1.25,2.00)
  tc=c(0.04,0.19,0.45,0.70,0.95,1.33)
  td=c(0.04,0.19,0.45,0.83,1.33,2.08)
  te=c(0.06,0.22,0.49,0.74,0.99,1.37)
  tf=c(0.08,0.26,0.53,0.78,1.03,1.41)
  tg=c(NA,0.125,0.375,0.625,0.875,NA)
  
  t.data=matrix(NA,nrow=148,ncol=6)
  for(i in 1:148)
  {
    if(RealData[i,15]==1) {
      t.data[i,]=ta}
    else if (RealData[i,15]==2) {
      t.data[i,]=tb}
    else if (RealData[i,15]==3) {
      t.data[i,]=tc}
    else if (RealData[i,15]==4) {
      t.data[i,]=td}
    else if (RealData[i,15]==5) {
      t.data[i,]=te}
    else if (RealData[i,15]==6) {
      t.data[i,]=tf}
    else {
      t.data[i,]=tg}
  }
  return(list("Original" = RealData,
              "Location" = t.data))
}

Data_Preprocess <- function(RealData,t.data){
  # reformate data by removing NA and convert into appropriate format
  count <- 0
  for(i in 1:148)
  {
    for(j in 1:6)
    {
      if(is.na(RealData[i,8+j])) {count=count}
      else {count=count+1}
    }
  }
  t <- rep(0, count)
  y <- rep(0, count)
  id <- rep(0, count)
  data1 <- data.frame(t=t,y=y,id=id,label=rep(0,count))
  index <- 0
  for(i in 1:148)
  {
    for(j in 1:6)
    {
      if(is.na(RealData[i,8+j])) {index=index}
      else {
        index=index+1
        data1[index,1]=t.data[i,j]
        data1[index,2]=RealData[i,8+j]
        data1[index,3]=i
      }
    }
  }
  data.add=data.frame(t=c(rep(10,148),rep(20,148)),y=rep(-30,296),id=rep(seq(1,148,1),2),label=rep(0,296))
  data1$y=log(data1$y)
  
  data.new=rbind(data1,data.add)
  return(list("New_all" = data.new,
              "New_org" = data1))
}


Data_Modeling <- function(New_Data,seed=10,k=7,RealData){
  data.new = New_Data[["New_all"]]
  data1 = New_Data[["New_org"]]
  require(flexmix)
  #seed is random seed for reproduction
  #k is the number of clusters
  set.seed(seed)
  m1 <- flexmix(y ~ t+ I(t^2)+ I(t^3) | id, data = data.new,k=k)
  
  #Get label
  data.new$label=clusters(m1)
  data1$label=data.new$label[1:754]
  ### put label back to data
  label.vec=rep(0,148)
  for(i in 1:754)
  {
    label.vec[data1[i,3]]=data1[i,4]
  }
  results=matrix(NA,nrow=148,ncol=7)
  results[,1:6]=as.matrix(RealData[,9:14])
  results[,7]=label.vec
  return(list("Model" = m1,
              "Label_data" = results))
}
##############################
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







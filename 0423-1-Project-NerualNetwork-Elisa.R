rm(list=ls())
# save(q9_s,file='ml/q9_s.rda') do this assignment at first
load('ml/q9_s.rda')

# step 1 check and delete NAs ---- 
# There are still NAs in q9_s
q9_s.omit = na.omit(q9_s)

# step 2 manually divide the K-fold ----

all.rows = nrow(q9_s.omit)
kfold.sample = sample(1:all.rows,all.rows)
k = 10 # main areas(First) where might modify parameters

# divide dataFrame to k pieces
l_q9_s = list()
for(i in 1:k)
{
  interval = all.rows %/% k
  left = 1 + (i - 1) * interval
  right = left + interval - 1
  l_q9_s[[i]] = q9_s.omit[kfold.sample[left:right],]
}

# write k piece of data to files, for need of package h2o
for(i in 1:k)
{
  write.csv(l_q9_s[[i]],file = paste0('ml/proj_test',as.character(i),'.csv'),row.names = F)
  k_1.fold = l_q9_s[-i]
  temp = k_1.fold[[1]]
  for(j in 2:(k-1))
  {
    temp = rbind(temp,k_1.fold[[j]])
  }
  write.csv(temp,file = paste0('ml/proj_train',as.character(i),'.csv'),row.names = F)
}


# step3 train ----

allnames = names(q9_s.omit)

# some cols which are not needed in the final model
drop_names.useless = c('drop','Dispute_prior')

# some cols whose effect is duplicate
drop_names.dup = c('Tags')

# set response variables names
DV = 'relief'

# set predictor variables names
IV = c('servicemenber','olderAm','Sub.product') # main areas(Second) where might modify parameters
# This can be set manually or by subtracting from allnames

avg.baseAcc = 0
avg.realAcc = 0
avg.TP = 0

# h2o is a pakage for deep learning(neural network)
if(!require(h2o)){install.packages('h2o')}
library(h2o)
h2o.init()

for(i in 1:k)
{
  train.Frame = h2o.importFile(paste0('ml/proj_train',as.character(i),'.csv'))
  test.Frame = h2o.importFile(paste0('ml/proj_test',as.character(i),'.csv'))
  
  # names of columns whose type needs to be factor, but their values may like numeric
  factor.numeric.names = c('ZIP.code',
                           'relief',
                           'servicemenber',
                           'olderAm',
                           'Fips',
                           'MedicalDebtClusters',
                           'Year')
  
  for(j in 1:length(factor.numeric.names))
  {
    train.Frame[[factor.numeric.names[j]]] = h2o.asfactor(train.Frame[[factor.numeric.names[j]]])
    test.Frame[[factor.numeric.names[j]]] = h2o.asfactor(test.Frame[[factor.numeric.names[j]]])
  }
  
  # core function about deep learning train
  # main areas(Third) where might modify parameters
  dl = h2o.deeplearning(x = IV,y = DV,training_frame = train.Frame, epochs = 1000, standardize = F)
  
  # test in test data
  cm = table(as.vector(test.Frame$relief),as.vector(predict(dl,newdata = test.Frame)[,1]))
  
  baseAcc = sum(as.vector(test.Frame$relief) == 0) / sum(cm)
  realAcc = (cm[1,1] + cm[2,2])/sum(cm)
  TP = cm[2,2] / (cm[1,2] + cm[2,2])
  
  avg.baseAcc = avg.baseAcc + baseAcc  # baseline
  avg.realAcc = avg.realAcc + realAcc # acc
  avg.TP = avg.TP + TP # TP
  
  print(paste0(as.character(i),'th'))
  print(paste0('baseAcc = ',as.character(baseAcc)))  
  print(paste0('realAcc = ',as.character(realAcc)))  
  print(paste0('TP = ',as.character(TP)))  
}

print(avg.baseAcc / k) # k-fold baseline acc
print(avg.realAcc / k) # k-fold model acc
print(avg.TP / k) # k-fold model TP


# above is about k-fold
# now it's direct to train all train data and test
# use another package to use neural network model
load('ml/0413-project/q9_s_test.rda')
names(q9_s_test)

library(neuralnet)

# IV = c('servicemenber','olderAm','Sub.product')
q9_s.omit
q9_s_test
nn.train.data = model.matrix(~relief+servicemenber+olderAm+Sub.product-1,data = q9_s.omit)
colnames(nn.train.data)
colnames(nn.train.data)=gsub('-','',colnames(nn.train.data))
colnames(nn.train.data)=gsub(' ','',colnames(nn.train.data))
colnames(nn.train.data)=gsub('\\.','',colnames(nn.train.data))

nn.test.data = model.matrix(~relief+servicemenber+olderAm+Sub.product-1,data = q9_s_test)
colnames(nn.test.data)
colnames(nn.test.data)=gsub('-','',colnames(nn.test.data))
colnames(nn.test.data)=gsub(' ','',colnames(nn.test.data))
colnames(nn.test.data)=gsub('\\.','',colnames(nn.test.data))


nn.model = neuralnet(relief~.,
                      data = nn.train.data,
                      hidden = c(2),
                      linear.output = F,
                      stepmax = 1e+06,
                      rep = 2)

nn.pred = predict(nn.model, newdata = nn.test.data)

nn.prec = vector('double')
nn.recall = vector('double')

nn.threshold = seq(0.01,0.9,0.01)

best.threshold = 1
best.f1 = 0
for(i in 1:length(nn.threshold))
{
  print(i)
  cm = table(as.vector(nn.test.data[,1]),
           ifelse(nn.pred>=nn.threshold[i],1,0))
  if(dim(cm)[2] == 2)
  {
    nn.prec = c(nn.prec,cm[2,2] / (cm[2,2] + cm[1,2])) 
    nn.recall = c(nn.recall,cm[2,2] / (cm[2,2] + cm[2,1])) 
  }
  else{
    nn.prec = c(nn.prec,0)
    nn.recall = c(nn.recall,0)
  }
  temp.f1 = 2 * nn.prec[i] * nn.recall[i] / (nn.prec[i] + nn.recall[i])
  print(temp.f1)
  if(!is.nan(temp.f1))
  {
    if(temp.f1 > best.f1)
    {
      best.f1 = temp.f1
      best.threshold = nn.threshold[i]
    }
  }
}
plot(nn.recall,nn.prec)

cm.best = table(as.vector(nn.test.data[,1]),
                ifelse(nn.pred >= best.threshold,1,0))

cm.best.df = as.data.frame(cm.best)

library(ggplot2)
ggplot(data = cm.best.df,
       mapping = aes(x = Var2,
                     y = Var1)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Predicted",y = "Actural") +
  scale_x_discrete(labels=c("negative","postive")) +
  scale_y_discrete(labels=c("negative","postive")) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  geom_text(x = 1.8, y = 1.982, aes(label = 'True Positive')) +
  geom_text(x = 1.8, y = 0.982, aes(label = 'False Positive')) +
  geom_text(x = 0.8, y = 1.982, aes(label = 'False Negative')) +
  geom_text(x = 0.8, y = 0.982, aes(label = 'True Negative')) +
  scale_fill_gradient(low = "#BDF28A",
                      high = "#FFC9C7",
                      trans = "log")

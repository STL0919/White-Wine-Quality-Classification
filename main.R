library(corrplot)
library(car)
library(ggplot2)
library(caret)
library(MASS)     
library(boot)
library(pROC)
library(dplyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(tidyr)
library(scales)
library(reshape2)
library(randomForest)
library(MLmetrics)
library(tibble)
library(purrr)
library(ipred)
library(ada)
library(e1071)  


data<-read.csv(file="winequality-white.csv", header=T, sep=";")
dim(data)
names(data)
summary(data)
table(data[,12])



#1.Check for missing values
#colSums(is.na(data))
##The results show no missing values
org_par<-par(no.readonly=TRUE)



#2.reset quality labels (0 and 1)
n<- length(data[,1])
n

#2.1 Select the partition threshold
table(data$quality)
summary(data$quality)
#mean of data:5.878
#median of data:6
#2.1.1 Number of positive and negative samples when the threshold is set to 5
label_5 <- ifelse(data$quality >= 5, 1, 0)
cat("Threshold = 5:\n")
print(table(label_5))

#2.1.2 Number of positive and negative samples when the threshold is set to 6
label_6 <- ifelse(data$quality >= 6, 1, 0)
cat("Threshold = 6:\n")
table(label_6)

#2.1.3 Number of positive and negative samples when the threshold is set to 7
label_7 <- ifelse(data$quality >= 7, 1, 0)
cat("Threshold = 7:\n")
table(label_7)
##First, considering that high-quality red wine is relatively scarce in reality (i.e., the quantity is small), and considering data balance,
##the threshold is selected as 6. Quality ≥ 6 is 1, and quality ≤ 5 is 0.

#2.2 Assign values of 0 and 1 to the quality variable
quality.code<- 0
for(i in 1:n){
if(data[i,12] >=6){quality.code[i]<-1}
else{quality.code[i]<-0}
}

wine<-cbind(data, quality.code)
wine$quality <- NULL
dim(wine)

head(wine)

table(wine$quality.code)



#3.EDA
#3.1 summary
by(wine, wine$quality.code, summary)

#3.1 Plot a boxplot of each variable
par(mfrow=c(4,3), mar= c(4, 4, 2.5, 1), oma=c(2, 2, 4, 1), mgp=c(2.5,1, 0))
box_col<-"#ADD8E6"
border_col<-"black"
for (var in names(wine)) {
  boxplot(wine[[var]]~wine$quality.code, main= paste("Boxplot of", var),xlab="quality.code", ylab= var,
          col= box_col, border=border_col)
}
mtext("Boxplots of Wine Dataset Variables", outer=TRUE, cex=1.5, font=2)
par(org_par)

#3.2 Plot a KDE of each variable
par(mfrow=c(4,3), mar= c(4, 4, 2.5, 1), oma=c(2, 2, 4, 1), mgp=c(2.5,1, 0))
kde_col0 <- "#ADD8E6"   # for quality.code = 0
kde_col1 <- "#F08080"   # for quality.code = 1

for (var in names(wine)) {
  x0 <- wine[[var]][wine$quality.code == 0]
  x1 <- wine[[var]][wine$quality.code == 1]
  
  #Compute the respective density estimates
  d0 <- density(x0)
  d1 <- density(x1) 
  x_lim <- range(d0$x, d1$x)
  y_lim <- range(d0$y, d1$y)
  
  #plot the first density curve (quality.code = 0)
  plot(d0,xlim= x_lim, ylim=y_lim, main=paste("KDE of", var), xlab =var, ylab="Density",
       col=kde_col0,lwd=2)
  #Add the second density curve (quality.code = 1)
  lines(d1, col= kde_col1, lwd=2, lty=2)
  
  legend("topright", legend = c("quality.code=0", "quality.code=1"), col= c(kde_col0, kde_col1),
         lty= c(1, 2),lwd= 2,bty="n", cex=0.8, y.intersp=0.5, inse= c(0.001, 0.001))
}
mtext("KDE Plots of Wine Variables by Quality Code", side=3, line=1, outer=TRUE, cex=1.5, font= 2)
par(org_par)


#3.3 plot the Q-Q plots and Shapiro-Wilk
vars_nor <- c("fixed.acidity", "density", "pH",
          "volatile.acidity", "citric.acid", "sulphates")
par(mfrow=c(2, 3), mar=c(4, 4, 2, 1), oma=c(0, 0, 4, 0) )

for (var in vars_nor) {
  x0 <- sort(wine[wine$quality.code==0, var])
  x1 <- sort(wine[wine$quality.code==1, var])
  q0 <- qnorm(ppoints(x0));  q1 <- qnorm(ppoints(x1))
  
  plot(q0, x0,main = paste("Q-Q Plot of", var),col  = "#89CFF0",pch  = 1,
       xlab = "Theoretical Quantiles",ylab = "Sample Quantiles")
  slope0<-diff(quantile(x0, c(0.25,0.75)))/diff(qnorm(c(0.25,0.75)))
  intercept0 <- quantile(x0,0.25) - slope0*qnorm(0.25)
  abline(intercept0, slope0, col ="#89CFF0", lwd = 2)
  
  points(q1, x1,col = "#F08080",pch = 2)
  slope1<-diff(quantile(x1, c(0.25,0.75))) /diff(qnorm(c(0.25,0.75)))
  intercept1<- quantile(x1,0.25) - slope1*qnorm(0.25)
  abline(intercept1, slope1, col="#F08080", lwd=2, lty = 2)
  
  legend("topleft",legend=c("Low Quality","High Quality"),
         col= c("#89CFF0","#F08080"), pch= c(1,2), bty= "n")
  
  # Shapiro-Wilk
  cat("\n===== ", var, " =====\n")
  cat("Low Quality:\n");  print(shapiro.test(x0))
  cat("High Quality:\n"); print(shapiro.test(x1))
}

mtext("Q-Q Plots of Selected Wine Variables by Quality",side=3,outer= TRUE,cex= 1.5,font= 2) 
par(org_par)



if (dev.cur() > 1) dev.off()
plot.new()
#3.4 plot a heatmap (observe the correlation between variables)
wine_var<-wine[,-which(names(wine) == "quality.code")]
cor_mat<-cor(wine)
corrplot(cor_mat, method="color", addCoef.col="black", col=colorRampPalette(c("#2166AC", "white", "#B2182B"))(100),
         tl.col="black", tl.cex=1.2, tl.srt=45, mar=c(0,0,0,0))

mtext("Correlation Heatmap of Wine Dataset", side=3, line=0, cex=1.5, font= 2)

#3.4 calculate the VIF for each explanatory variables.
vif_values<-diag(solve(cor_mat))
vif_table<-data.frame(Variable=names(vif_values),VIF = round(vif_values, 2))
vif_table<-vif_table[order(vif_table$VIF, decreasing = TRUE), ]
vif_table



attach(wine)
#4. Divide the dataset (training set and test set)
#4.1.Set the split ratio
ratio<-0.8
n_train<-floor(ratio * n)
names(wine)
old_seed<-if (exists(".Random.seed")) .Random.seed else NULL

set.seed(1234)
train_ind<-sample(n, size=n_train, replace=FALSE)

if (!is.null(old_seed)) {
  .Random.seed <- old_seed
} else {
  rm(.Random.seed, envir = .GlobalEnv)
}

train_set <- wine[train_ind, ]
test_set  <- wine[-train_ind, ]
train_set$quality.code <- factor(train_set$quality.code, levels = c(0,1))
test_set$quality.code  <- factor(test_set$quality.code,  levels = c(0,1))

counts <- table(train_set$quality.code)
print(counts)
props <- prop.table(counts)
print(props)

#4.2.Full variable logistic regression model

full_model <- glm(quality.code ~ ., data = train_set, family = binomial)
#Predictions on the testset
test_pred <- predict(full_model, newdata = test_set, type = "response")
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

#Confusion Matrix and ROC
conf_matrix <- table(Predicted = test_pred_class, Actual = test_set$quality.code)
print("Full model confusion matrix:")
print(conf_matrix)

roc_obj_log<-roc(test_set$quality.code, test_pred, quiet = TRUE)
auc_value_log<-auc(roc_obj_log)

roc_df_log <- data.frame(fpr=1 -roc_obj_log$specificities,   
                         tpr= roc_obj_log$sensitivities)
ggplot(roc_df_log, aes(x = fpr, y = tpr)) +
  geom_line(color="#5CB356", size =0.9) +
  geom_abline(intercept=0, slope = 1,
              linetype ="dashed", color="gray60") +
  labs(title= "ROC Curve for Full Logistic Model",
    subtitle=paste0("AUC = ", round(auc_value_log, 3)),
    x="FPR", y="TPR") +
  theme_minimal(base_size= 14) +
  theme(plot.title=element_text(face ="bold", size=18, hjust=0.5),
    plot.subtitle=element_text(face="bold", size=14,
                                 hjust=0.5,
                                 margin =margin(b= 15)),
    axis.title= element_text(size = 14),axis.text=element_text(size = 12),
    panel.grid.major=element_line(color = "gray90"),panel.grid.minor = element_blank())



#LOOCV
wine$quality.code <- as.factor(wine$quality.code)
levels(wine$quality.code) <- c("low","high")

loocv_ctrl <- trainControl(method="LOOCV", classProbs= TRUE, 
                           summaryFunction = twoClassSummary,savePredictions="final")
loocv_res <- train(quality.code ~ ., data=wine,method= "glm",
                   family=binomial,trControl=loocv_ctrl,metric= "ROC")
loocv_preds <-loocv_res$pred
loocv_preds
cm_loocv <- confusionMatrix(data=loocv_preds$pred, reference=loocv_preds$obs,positive="high")
print(cm_loocv)
loocv_avg_error_rate<-mean(loocv_preds$pred != loocv_preds$obs)
cat("LOOCV average error rate:", round(loocv_avg_error_rate, 4), "\n")
cat("LOOCV Average Accuracy =", round(1-loocv_avg_error_rate, 4), "\n")
print(loocv_res)

#10_fold cross validation
cv_ctrl<-trainControl(method ="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary,savePredictions  = "all" )
cv_res<-train(quality.code ~ .,data=wine,method="glm",family= binomial,trControl=cv_ctrl,metric="ROC")

cv_preds<- cv_res$pred
cv_avg_error_rate <- mean(cv_preds$pred != cv_preds$obs)
cm_10cv <- confusionMatrix(data=cv_preds$pred, reference=cv_preds$obs,positive="high")
print(cm_10cv)
cat("10-fold CV average error rate:", round(cv_avg_error_rate, 4), "\n")
cat("10-fold CV Average Accuracy =", round(1 - cv_avg_error_rate, 4), "\n")

print(cv_res)

#.632+Bootstrap

#.632+Bootstrap error calculation function
wine$quality.code <- factor(wine$quality.code)
boot_fn <- function(data, indices) {
  train   <- data[indices, ]
  oob_idx <- setdiff(seq_len(nrow(data)), unique(indices))
  if (length(oob_idx)==0) return(NA)
  test    <- data[oob_idx, ]
  
  fit  <- glm(quality.code ~ ., data=train, family=binomial)
  p    <- predict(fit, newdata=test, type="response")
  pred <- factor(ifelse(p>0.5,
                        levels(train$quality.code)[2],
                        levels(train$quality.code)[1]),
                 levels=levels(train$quality.code))
  mean(pred != test$quality.code)
}


B<-1000
boot_out<-boot(data=wine,
                 statistic=boot_fn,
                 R=B)
e_boot<-mean(boot_out$t, na.rm=TRUE)
fit0<-glm(quality.code ~ ., data=wine, family=binomial)
p0<-predict(fit0, newdata=wine, type="response")
pred0<- factor(ifelse(p0>0.5,levels(wine$quality.code)[2],levels(wine$quality.code)[1]),
                  levels=levels(wine$quality.code))
err<-mean(pred0!= wine$quality.code)
tab<- table(wine$quality.code)
maj_err<-1-max(tab)/sum(tab)  
e_noinfo<-maj_err
gamma<-(e_boot-err)/(e_noinfo-err)
gamma<-pmin(pmax(gamma,0), 1)  


w<-0.632/(1-0.368*gamma)
e_632p<-(1-w)*err + w *e_boot

cm_boots<-confusionMatrix(data=pred0,reference= wine$quality.code,positive=levels(wine$quality.code)[2])
print(cm_boots)

cat(sprintf(".632+ Bootstrap error：%.4f\n", e_632p))


#4.3 stepwise regression
train_set$quality.code <- factor(train_set$quality.code, levels = c(0, 1))
test_set$quality.code <- factor(test_set$quality.code, levels = c(0, 1))

step_model <- stepAIC(full_model,direction = "both",trace = FALSE)

selected_vars <- names(coef(step_model))[-1]
cat("Variables selected by stepwise regression:\n")
print(selected_vars)
cat("Original number of variables:", ncol(train_set) - 1, "\n")
cat("Number of selected variables:", length(selected_vars), "\n")



#Comparing the performance of the full model and the stepwise model

#the performance of the full model
full_pred <- predict(full_model, newdata = test_set, type = "response")
full_pred_class <- ifelse(full_pred > 0.5, 1, 0)
full_acc <- mean(full_pred_class == as.numeric(test_set$quality.code) - 1)
full_roc <- roc(test_set$quality.code, full_pred)
full_auc <- auc(full_roc)
full_aic<-AIC(full_model)

## 4.2 the performance of the stepwise model 
step_pred <- predict(step_model, newdata = test_set, type = "response")
step_pred_class <- ifelse(step_pred > 0.5, 1, 0)
step_acc <- mean(step_pred_class == as.numeric(test_set$quality.code) - 1)
step_roc <- roc(test_set$quality.code, step_pred)
step_auc <- auc(step_roc)
step_aic<-AIC(step_model)

results <- data.frame(
  Model=c("Full Model", "Stepwise Model"),
  Accuracy= c(full_acc, step_acc),
  AUC=c(full_auc, step_auc),
  Variables=c(ncol(train_set) - 1, length(selected_vars)),
  AIC=c(full_aic, step_aic),
  stringsAsFactors = FALSE
)
print(results)

#Visualize the results
##ROC curve
roc_df <- rbind(
  data.frame(Model = "Full Model", 
             FPR = 1 - full_roc$specificities, 
             TPR = full_roc$sensitivities),
  data.frame(Model = "Stepwise Model", 
             FPR = 1 - step_roc$specificities, 
             TPR = step_roc$sensitivities)
)

ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size =0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves Comparison",
       subtitle = paste("Full Model AUC:", round(full_auc,4)," | Stepwise Model AUC:",round(step_auc, 4)),
       x = "FPR ",y = "TPR") +
  theme_minimal() +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold",size = 16,hjust = 0.5),
        plot.subtitle = element_text(size = 12,hjust = 0.5),
        axis.title = element_text(size = 12))



#5.Dicision Tree
#5.1 Decision Tree via Gini index
dt_model<-rpart(formula=quality.code~., data=train_set, method="class", parms=list(split= "gini"),
  control=rpart.control(maxdepth=5, minsplit= 20, minbucket=10, cp= 0.001,maxsurrogate= 0))

#Select the optimal cp and prune according to the cross-validation results
best_cp <- dt_model$cptable[which.min(dt_model$cptable[,"xerror"]), "CP"]
dt_pruned <- prune(dt_model, cp = best_cp)

cp_tab <-as.data.frame(dt_model$cptable)
cp_tab$TreeSize<-cp_tab$nsplit+1

#Compute accuracy on training and test sets for each CP
train_acc<-sapply(cp_tab$CP, function(cp_val){
  pruned<- prune(dt_model, cp=cp_val)
  mean(predict(pruned, train_set, type="class")==train_set$quality.code)
})
test_acc<-sapply(cp_tab$CP, function(cp_val){
  pruned<-prune(dt_model, cp=cp_val)
  mean(predict(pruned, test_set, type="class")== test_set$quality.code)
})

#10-fold cv to select the parameter conbination
ctrl<-trainControl(method= "cv", number=10)
grid<-data.frame(cp=cp_tab$CP)

cv_fit <- train(quality.code ~ ., data=train_set, method="rpart", trControl=ctrl, 
                tuneGrid=grid,parms=list(split="gini"), 
                control=rpart.control(maxdepth=19, minspli=5,minbucket=3, cp=0.001,maxsurrogate= 0))
cv_results<-cv_fit$results
cv_acc<-cv_results$Accuracy[match(cp_tab$CP,cv_results$cp)]

df<-data.frame(TreeSize=cp_tab$TreeSize,Training=train_acc,
               CrossValidation=cv_acc,Test=test_acc)

df_long<-reshape2::melt(df, id.vars="TreeSize",
  variable.name="Dataset",value.name="Accuracy")

ggplot(df_long, aes(x=TreeSize, y=Accuracy, color=Dataset)) +
  geom_line(size=1) +geom_point(size=3) +
  scale_x_continuous(breaks=df$TreeSize) +scale_y_continuous(labels=percent_format(accuracy= 1)) +
  scale_color_manual(values=c(Training="#1f78b4", CrossValidation="#33a02c", Test="#ff7f00"))+
  labs(title= "Accuracy Comparison Across Tree Sizes",
      subtitle=sprintf("Optimal Pruning CP = %.5f (MaxDepth = %d, MinSplit = %d)",
      cp_tab$CP[which.max(cv_acc)], dt_model$control$maxdepth, dt_model$control$minsplit),
      x="Number of Leaf Nodes (Tree Size)", y="Accuracy", color =NULL)+
  theme_minimal(base_family ="Arial") +
  theme(
    plot.title= element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle=element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.title=element_text(size = 12),
    axis.text=element_text(size = 10),
    legend.position=c(0.8, 0.2),
    legend.background=element_rect(fill = alpha("white", 0.6), color = NA),
    panel.grid.major=element_line(color = "gray90"),
    panel.grid.minor=element_blank())

#Visualize the tree structure before and after pruning
par(mar=c(1, 1, 2, 1))
rpart.plot(dt_model,main= "Original Tree",type=2,extra=104,fallen.leaves=TRUE,uniform=TRUE, 
           cex=NULL,tweak=1.3,split.cex=1.3,under.cex=1,xcompact=FALSE,ycompact=FALSE)
rpart.plot(dt_pruned, main="Tree after pruning", type=2, extra=104,
           fallen.leaves=TRUE, uniform=TRUE, cex=NULL,tweak=1.1,
           split.cex=1.3, under.cex=1, xcompact= FALSE, ycompact=FALSE)

#Predict on the test set
pred_class<-predict(dt_pruned,test_set, type ="class")
pred_prob<-predict(dt_pruned, test_set,type="prob")[,2] 

dt_roc <- roc(response=test_set$quality.code,predictor=pred_prob,
              levels=c("0","1"),direction = "<")
dt_auc<-auc(dt_roc)
roc_df<-data.frame(
  Model = "Decision Tree",
  FPR=1-dt_roc$specificities,
  TPR=dt_roc$sensitivities
)

#ROC curve
ggplot(roc_df, aes(x=FPR, y=TPR, color=Model)) +
  geom_line(size =0.7) +
  geom_abline(intercept=0, slope =1, linetype="dashed", color="gray") +
  labs(title="ROC Curve for Decision Tree", subtitle=paste("AUC =", round(dt_auc, 4)),
       x="False Positive Rate (FPR)",y="True Positive Rate (TPR)")+
  theme_minimal() +
  scale_color_manual(values = "#377EB8")+
  theme(legend.position="none",plot.title=element_text(face= "bold", size=16, hjust=0.5),
    plot.subtitle=element_text(size=12, hjust =0.5),axis.title=element_text(size=12))

cm_dt<-table(Predicted = pred_class, Actual = test_set$quality.code)
print(cm_dt)
tp<-cm_dt[2,2]
tn<-cm_dt[1,1]
fp<-cm_dt[2,1]
fn<-cm_dt[1,2]
metrics<-c(
  Accuracy=(tp+tn)/sum(cm_dt),
  Precision=tp/(tp+fp),
  Recall=tp/ (tp+ fn),
  Specificity= tn/(tn +fp),
  FPR=fp/(fp+tn),
  F1= 2*tp/(2*tp+fp+fn))
print(round(metrics, 4))

#Scatter plot
top2<-names(sort(dt_model$variable.importance, decreasing=TRUE))[1:2]
feat1<-top2[1]
feat2<-top2[2]

splits<-dt_model$splits
cut1<- splits[rownames(splits)==feat1, "index"][1]
cut2<-splits[rownames(splits)==feat2, "index"][1]

xrng<-range(test_set[[feat1]],na.rm=TRUE)
yrng<-range(test_set[[feat2]], na.rm=TRUE)

ggplot(test_set, aes_string(x=feat1, y=feat2, color="factor(quality.code)")) +
  geom_point(alpha=0.6, size=2) +
  geom_vline(xintercept=cut1, color= "darkgreen", size = 1) +
  geom_segment(x=xrng[1],xend =cut1,y=cut2,yend=cut2,color="darkgreen",size= 1)+
  annotate("text",x=mean(c(cut1, xrng[2])),y=yrng[1],label ="R1",
           vjust=-1, size=6, fontface="bold")+
  annotate("text",x=mean(c(xrng[1], cut1)),y=yrng[1],label ="R2",
           vjust=-1, size=6, fontface="bold") +
  annotate("text",x=mean(c(xrng[1], cut1)),y=yrng[2],label ="R3",
           vjust=1, size=6, fontface="bold")+
  scale_color_manual(values = c("0" = "#1f78b4", "1" = "#e31a1c")) +
  labs(title="Decision Regions",
    subtitle=sprintf("%s < %.3f    %s < %.3f", feat1, cut1, feat2, cut2),
    x=feat1, y=feat2, color="Quality")+
  theme_minimal()+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=11, color="gray40", hjust=0.5))





#5.2 Decision Tree via Generalized Gini index
#Define the loss matrix
loss_mat<- matrix(
  c(0, 2,    
    1, 0),     
  nrow=2, byrow = TRUE,
  dimnames=list(pred= c("0","1"),
                true=c("0","1"))
)


#5.2 Decision Tree via Generalized Gini index
#Define the loss matrix
loss_mat<- matrix(
  c(0, 1.7,    
    1, 0),     
  nrow=2, byrow = TRUE,
  dimnames=list(pred= c("0","1"),
                true=c("0","1"))
)

# 2. Build the original decision tree via Generalized Gini index
dt_gen <-rpart(quality.code ~ ., data=train_set,method="class",
               parms=list(split="gini",loss=loss_mat),
               control= rpart.control(maxdepth=5, minsplit=20, minbucket=10, cp=0.001, maxsurrogate= 0))

best_cp_gen<-dt_gen$cptable[ which.min(dt_gen$cptable[,"xerror"]), "CP" ]
dt_pruned_gen<-prune(dt_gen, cp = best_cp_gen)
pred_class<-predict(dt_pruned_gen, test_set, type="class")
pred_prob<-predict(dt_pruned_gen,test_set, type="prob")[, "1"]

cm_dt_gen<-table(Pred=pred_class, True=test_set$quality.code)
print(cm_dt_gen)

tp_dt_gen <-cm_dt_gen["1","1"]
tn_dt_gen<-cm_dt_gen["0","0"]
fp_dt_gen<-cm_dt_gen["1","0"]
fn_dt_gen<-cm_dt_gen["0","1"]
metrics_dt_gen <- c(
  Accuracy=(tp_dt_gen+tn_dt_gen)/sum(cm_dt_gen),
  Precision=tp_dt_gen/(tp_dt_gen+fp_dt_gen),
  Recall=tp_dt_gen/(tp_dt_gen+fn_dt_gen),
  Specificity=tn_dt_gen/(tn_dt_gen+fp_dt_gen),
  NPV = tn_dt_gen / (tn_dt_gen + fn_dt_gen),
  F1=2*tp_dt_gen/(2*tp_dt_gen+fp_dt_gen+fn_dt_gen))
print(round(metrics_dt_gen, 4))

#Scatter plot
top2_gen<-names(sort(dt_gen$variable.importance, decreasing=TRUE))[1:2]
print(sort(dt_gen$variable.importance, decreasing=TRUE))
feat1_gen<-top2_gen[1]
feat2_gen<-top2_gen[2]

splits_gen<-dt_gen$splits
cut1_gen<- splits_gen[rownames(splits_gen)==feat1_gen, "index"][1]
cut2_gen<-splits_gen[rownames(splits_gen)==feat2_gen, "index"][1]

xrng_gen<-range(test_set[[feat1_gen]],na.rm=TRUE)
yrng_gen<-range(test_set[[feat2_gen]], na.rm=TRUE)

ggplot(test_set, aes_string(x =feat1_gen, y=feat2_gen, color="quality.code"))+
  geom_point(alpha=0.6, size=2)+
  geom_vline(xintercept =cut1_gen, color="darkgreen", size=1)+
  geom_segment(x =xrng_gen[1],xend=cut1_gen,y=cut2_gen, yend=cut2_gen,
               color="darkgreen", size=1) +
  annotate("text",x=mean(c(cut1_gen, xrng_gen[2])), y= yrng_gen[1],
           label="R1", vjust =-1, size=6, fontface="bold")+
  annotate("text", x=mean(c(xrng_gen[1],cut1_gen)), y=yrng_gen[1],
           label="R2", vjust=-1, size=6, fontface="bold")+
  annotate("text", x=mean(c(xrng_gen[1],cut1_gen)), y=yrng_gen[2],
           label="R3", vjust=1, size=6, fontface ="bold") +
  labs(title="Decision Regions via Generalized Gini Index",
       subtitle = sprintf("%s < %.3f    %s < %.3f",feat1_gen, cut1_gen, feat2_gen, cut2_gen),
       x=feat1_gen, y=feat2_gen, color="Quality") +
  theme_minimal()+
  theme(plot.title= element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle=element_text(size=11, color="gray40", hjust=0.5)
  )


#6.Bagging
ctrl<-trainControl(method = "cv", number= 10)
par_bag<-rpart::rpart.control(maxdepth=5, minsplit=20,
                              minbucket=10, cp=0.001, maxsurrogate= 0)

bag_model<-train(quality.code ~ ., data=train_set, method="treebag", 
                 trControl=ctrl,metric="Accuracy", control=par_bag)
print(bag_model)
bag_pred <- predict(bag_model, test_set)
cm_bag <- confusionMatrix(bag_pred, test_set$quality.code, positive = "1")
print(cm_bag)


#1.Feature split frequency statistics
trees_bag <- purrr::map(bag_model$finalModel$mtrees, "btree")
split_vars_bag <- purrr::map(trees_bag, ~ .x$frame$var[.x$frame$var != "<leaf>"]) %>%
  unlist()
split_freq_bag <- as.data.frame(table(split_vars_bag)) %>% arrange(desc(Freq))

ggplot(split_freq_bag, aes(x=reorder(split_vars_bag, -Freq), y=Freq, fill=Freq)) +
  geom_col(width=0.7) +
  geom_text(aes(label=Freq), vjust=-0.5, size=3) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(title="Split Frequency of Features in Bagging",
    x="Feature", y="Split Count", fill="Count") +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(face="bold", hjust=0.5),
    axis.text.x=element_text(angle=45, hjust=1, face= "bold"),
    axis.title=element_text(face="bold"),
    panel.grid.major=element_line(color="gray90"),
    panel.grid.minor=element_blank()
  )


#2.Calculate the prediction correlation coefficient between trees
tree_preds_list <-lapply(trees_bag, function(tree) {
  predict(tree, newdata=test_set, type="prob")[, "1"]})
preds_mat<-do.call(cbind, tree_preds_list)
colnames(preds_mat)<-paste0("tree_", seq_len(ncol(preds_mat)))
corr_mat<-cor(preds_mat)
mean_corr<- mean(corr_mat[upper.tri(corr_mat)])
cat("Average correlation coefficient: ", round(mean_corr, 3), "\n")


#7.Random Forest
#Modeling
p<-ncol(train_set)- 1
mtry_val<-floor(sqrt(p))

rf_model<-randomForest(
  quality.code ~ .,data= train_set, ntree=200, mtry=mtry_val, importance=TRUE,
  maxnodes=32, nodesize=10)

#print(rf_model)
oob_error<-rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
cat(sprintf("OOB estimate of error rate: %.2f%%\n", 100*oob_error))

# Predict on the testset
rf_pred_class<-predict(rf_model, newdata=test_set, type="response")
accuracy_rf<-mean(rf_pred_class==test_set$quality.code)
cat(sprintf("Random Forest Accuracy on Test Set: %.2f%%\n", 100*accuracy_rf))

# Confudion matrix
conf_matrix_rf<-table(
  Predicted=rf_pred_class,
  Actual=test_set$quality.code)
cat("Random Forest Confusion Matrix:\n")
print(conf_matrix_rf)



# Hyperparameter Optimization via Grid Search
# parameter space
p       <- ncol(train_set) - 1
m_vals  <- seq(floor(sqrt(p)/2), ceiling(2*sqrt(p))) 
M_vals  <- seq(50, 500, by = 50)                     
grid    <- expand.grid(mtry = m_vals, ntree = M_vals)
grid$F1 <- NA 

folds <- createFolds(train_set$quality.code, k = 5, list = TRUE)

# 5-fold CV for each parameter combination
for (i in seq_len(nrow(grid))) {
  mtry_val<-grid$mtry[i]
  ntree_val<-grid$ntree[i]
  
  f1_scores<-numeric(length(folds))
  for (j in seq_along(folds)) {
    idx_val<-folds[[j]]
    cv_train<-train_set[-idx_val, ]
    cv_valid<-train_set[ idx_val, ]
    
    rf_cv<-randomForest(quality.code ~ ., data= cv_train, mtry=mtry_val, ntree=ntree_val)
    pred_cv <- predict(rf_cv, newdata=cv_valid, type="response")
    f1_scores[j]<- F1_Score(y_pred=pred_cv, y_true= cv_valid$quality.code,
                             positive=levels(train_set$quality.code)[2])
    }
  grid$F1[i]<-mean(f1_scores)
}

# Find optimal parameter
best_idx<- which.max(grid$F1)
best_m<-grid$mtry[best_idx]
best_M<-grid$ntree[best_idx]
best_F1<- grid$F1[best_idx]
cat(sprintf("Best params — mtry = %d, ntree = %d (mean CV-F1 = %.4f)\n",
            best_m, best_M, best_F1))

rf_final<-randomForest(
  quality.code ~ .,
  data=train_set,
  mtry=best_m,
  ntree= best_M,
  importance=TRUE,
  maxnodes=32, nodesize=10)
rf_pred<-predict(rf_final, newdata=test_set, type="response")
cat("Confusion matrix on test set:\n")
print(table(Predicted=rf_pred, Actual=test_set$quality.code))
acc_rf<-mean(rf_pred==test_set$quality.code)
cat(sprintf("Test set accuracy: %.2f%%\n", 100*acc_rf))

#Feature importance
imp<-importance(rf_final)
imp_df <-as.data.frame(imp) %>%
  rownames_to_column(var="Feature") %>%
  select(Feature, MeanDecreaseGini, MeanDecreaseAccuracy) %>%
  pivot_longer(-Feature,names_to="Metric",values_to="Importance") %>%
  group_by(Metric) %>%
  arrange(Importance) %>%
  mutate(Feature=factor(Feature, levels=unique(Feature))) %>%
  ungroup()

ggplot(imp_df, aes(x=Importance, y=Feature, fill=Metric))+
  geom_col(position=position_dodge(width=0.8), width=0.7) +
  facet_wrap(~ Metric, scales="free_x", ncol = 1) +
  scale_fill_manual(values=c("MeanDecreaseAccuracy"="#1b9e77",
                               "MeanDecreaseGini"="#d95f02"))+
  labs(
    title= "Variable Importance in Random Forest",
    subtitle="Comparison of Gini‐based vs. Permutation‐based measures",
    x="Importance",y=NULL, fill= NULL)+
  theme_minimal(base_size=14)+
  theme(
    legend.position="none",
    strip.text=element_text(face="bold", size= 12),
    axis.text.y=element_text(size=11),
    plot.title=element_text(face="bold", size=16, hjust =0.5),
    plot.subtitle=element_text(size=12, hjust=0.5),
    panel.grid.major.y=element_blank()
  )



#8.AdaBoost
ada_grid <- expand.grid(
  iter=c(50, 100, 150),
  maxdepth=5,     
  nu=c(0.05, 0.1, 0.2))

ada_model <- train( quality.code ~ ., data= train_set, method="ada", 
                    trControl=ctrl, tuneGrid=ada_grid, metric ="Accuracy")
print(ada_model)
ada_pred <- predict(ada_model, test_set)
cm_ada <- confusionMatrix(ada_pred, test_set$quality.code, positive = "1")
print(cm_ada)



#9.SVM

#9.1 Linear SVM in 2D
tune_lin<-tune(
  svm,
  quality.code ~ alcohol+volatile.acidity,
  data=train_set,
  kernel="linear",
  ranges=list(cost=c(0.01, 0.1, 1, 10, 100)),
  tunecontrol= tune.control(cross=5)
  )

best_lin<-tune_lin$best.model
cat("Best cost=", best_lin$cost, "\n")

#Train the final linear SVM on the training set with the optimal cost
svm_lin_model<-best_lin
svm_lin_model<-svm(
  quality.code ~ alcohol+volatile.acidity,
  data=train_set,
  kernel="linear",
  cost=best_lin$cost,
  probability=TRUE
)

#Predict in the test set
pred_class_lin <- predict(svm_lin_model, newdata=test_set)

pred_prob_lin<-attr(
  predict(svm_lin_model, newdata=test_set, probability=TRUE),
  "probabilities"
)[, "1"]

#Confusion matrix of linear model
cm_lin<-table(Predicted=pred_class_lin, Actual=test_set$quality.code)
print(cm_lin)
tp<-cm_lin["1","1"]
tn<-cm_lin["0","0"]
fp<-cm_lin["1","0"]
fn<-cm_lin["0","1"]
metrics_lin<-c(
  Accuracy=(tp+tn)/sum(cm_lin),
  Precision=tp/(tp+fp),
  Recall=tp/(tp+fn),
  Specificity=tn/(tn+fp),
  F1=2*tp/(2*tp+fp+ fn)
)
print(round(metrics_lin, 4))

#Plot a line graph of classification accuracy under different regularization strengths
#Extract CV performance from tune() results
cv_perf<-tune_lin$performances
cv_df<-data.frame(
  cost=cv_perf$cost,
  accuracy=1-cv_perf$error
)

ggplot(cv_df, aes(x=cost, y=accuracy)) +
  geom_line(size=1.1, color="steelblue") +
  geom_point(size=3, color="steelblue", fill="white", shape=21) +
  scale_x_log10(breaks=cv_df$cost,
                labels=as.character(cv_df$cost))+
  labs(
    x="Regularization Parameter",
    y="Cross Validation Accuracy",
    title="The Cross Validation Accuracy by Regularization Strength"
  )+
  theme_minimal()+
 theme(
    plot.title=element_text(hjust=0.5, face="bold"),
    panel.grid.major=element_line(color="grey85"))




#Plot decision boundary
al_seq <- seq(min(train_set$alcohol), max(train_set$alcohol), length = 200)
va_seq <- seq(min(train_set$volatile.acidity), max(train_set$volatile.acidity), length = 200)
grid_lin <- expand.grid(alcohol = al_seq, volatile.acidity = va_seq)
grid_lin$pred <- predict(svm_lin_model, newdata = grid_lin)
grid_lin$pred_class <- as.numeric(predict(svm_lin_model, newdata = grid_lin))

ggplot()+
  geom_point(data = test_set, 
             aes(x = alcohol, y = volatile.acidity, color = factor(quality.code)),
             alpha = 0.6, size = 2) +
  geom_contour(data = grid_lin, 
               aes(x = alcohol, y = volatile.acidity, z =pred_class),
               breaks = 1.5, linetype = "solid", color = "black") +
  scale_color_manual(
    values=c("0"="#4C72B0", "1"="#C44E52"),
    labels=c("Low quality", "High quality")
  )+
  labs(
    title="Linear SVM Decision Boundary in Two Dimension",
    x="Alcohol",
    y="Volatile Acidity",
    color="Quality Code"
  )+
  theme_minimal(base_family="Arial", base_size=14) +
  theme(
    plot.title=element_text(face="bold",size=16, hjust=0.5),
    axis.title=element_text(size=12),
    axis.text=element_text(size=11),
    legend.position=c(0.85, 0.15),
    legend.background= element_rect(fill=alpha("white", 0.7), color=NA),
    panel.grid.major=element_line(color="gray90"),
    panel.grid.minor=element_blank()
  )



#9.2 Linear SVM in High Dimension
tune_lin_hd<-tune(
  svm,
  quality.code ~ .,
  data=train_set,
  kernel="linear",
  ranges=list(cost=c(0.01, 0.1, 1, 10, 100)),
  tunecontrol= tune.control(cross=10)
)

best_lin_hd<-tune_lin_hd$best.model
cat("Best cost=", best_lin_hd$cost, "\n")

#Train the final linear SVM on the training set with the optimal cost
svm_lin_model_hd<-best_lin_hd
svm_lin_model_hd<-svm(
  quality.code ~ .,
  data=train_set,
  kernel="linear",
  cost=best_lin_hd$cost,
  probability=TRUE
)

#Predict in the test set
pred_class_lin_hd <- predict(svm_lin_model_hd, newdata=test_set)

pred_prob_lin_hd<-attr(
  predict(svm_lin_model_hd, newdata=test_set, probability=TRUE),
  "probabilities"
)[, "1"]

#Confusion matrix of linear model
cm_lin_hd<-table(Predicted=pred_class_lin_hd, Actual=test_set$quality.code)
print(cm_lin_hd)
tp<-cm_lin_hd["1","1"]
tn<-cm_lin_hd["0","0"]
fp<-cm_lin_hd["1","0"]
fn<-cm_lin_hd["0","1"]
metrics_lin_hd<-c(
  Accuracy=(tp+tn)/sum(cm_lin_hd),
  Precision=tp/(tp+fp),
  Recall=tp/(tp+fn),
  Specificity=tn/(tn+fp),
  F1=2*tp/(2*tp+fp+ fn)
)
print(round(metrics_lin_hd, 4))

#Plot a line graph of classification accuracy under different regularization strengths
#Extract CV performance from tune() results
cv_perf_hd<-tune_lin_hd$performances
cv_df_hd<-data.frame(
  cost=cv_perf_hd$cost,
  accuracy=1-cv_perf_hd$error
)

ggplot(cv_df_hd, aes(x=cost, y=accuracy)) +
  geom_line(size=1.1, color="steelblue") +
  geom_point(size=3, color="steelblue", fill="white", shape=21) +
  scale_x_log10(breaks=cv_df_hd$cost,
                labels=as.character(cv_df_hd$cost))+
  labs(
    x="Regularization Parameter",
    y="Cross Validation Accuracy",
    title="The Cross Validation Accuracy by Regularization Strength (Full covariables)"
  )+
  theme_minimal()+
  theme(
    plot.title=element_text(hjust=0.5, face="bold"),
    panel.grid.major=element_line(color="grey85"))


# 9.3 Nonlinear SVM in two dimension
costs<-c(0.1, 1, 10, 100)
degrees<-c(3,4,5)
coef0s<-c(2,3,4)

tune_poly<-tune(
  svm,
  quality.code ~ alcohol+volatile.acidity,
  data=train_set,
  kernel="polynomial",
  ranges=list(
    cost=costs,
    degree=degrees,
    coef0=coef0s),
  tunecontrol=tune.control(cross=10),
  tolerance=1e-3, 
  cachesize=500
)


print(tune_poly)
best_poly<-tune_poly$best.model
cat("Best parameters:\n",
    " cost   =", best_poly$cost,"\n",
    " degree =", best_poly$degree,"\n",
    " coef0  =", best_poly$coef0, "\n"
)


svm_poly_final <- svm(
  quality.code~alcohol+volatile.acidity,
  data= train_set,
  kernel="polynomial",
  cost=best_poly$cost,
  degree=best_poly$degree,
  coef0=best_poly$coef0,
  probability=TRUE,
  tolerance=1e-3,
  cachesize=200
)

pred_poly<-predict(svm_poly_final, newdata=test_set)
acc_poly<-mean(pred_poly==test_set$quality.code)

cm_poly<-table(Predicted=pred_poly,
                 Actual=test_set$quality.code)
print(cm_poly)
cat("Test-set Accuracy (poly):", round(acc_poly, 4), "\n")


al_seq<-seq(min(train_set$alcohol), max(train_set$alcohol), length=200)
va_seq<-seq(min(train_set$volatile.acidity), max(train_set$volatile.acidity), length=200)
grid_poly<-expand.grid(
  alcohol=al_seq,
  volatile.acidity=va_seq
)

grid_poly$pred_class<-as.numeric(
  predict(svm_poly_final, newdata=grid_poly)
)


ggplot()+
  geom_point(data=test_set, 
             aes(x= alcohol, y =volatile.acidity, color=factor(quality.code)),
             alpha=0.6, size=2) +
  geom_contour(data=grid_poly, 
               aes(x=alcohol, y=volatile.acidity, z=pred_class),
               breaks=1.5, linetype="solid", color="black") +
  scale_color_manual(
    values=c("0"="#4C72B0", "1"="#C44E52"),
    labels=c("Low quality", "High quality")
  )+
  labs(
    title="Nonlinear SVM Decision Boundary with Poly Kernel",
    x="Alcohol",
    y="Volatile Acidity",
    color="Quality Code"
  )+
  theme_minimal(base_family="Arial", base_size=14) +
  theme(
    plot.title=element_text(face="bold",size=16, hjust=0.5),
    axis.title=element_text(size=12),
    axis.text=element_text(size=11),
    legend.position=c(0.85, 0.15),
    legend.background= element_rect(fill=alpha("white", 0.7), color=NA),
    panel.grid.major=element_line(color="gray90"),
    panel.grid.minor=element_blank()
  )


# 9.4 Nonlinear SVM in full feature space
costs<-c(0.1, 1, 10, 100)
degrees<-c(3,4,5)
coef0s<-c(0,1)

tune_poly_full<-tune(
  svm,
  quality.code ~ .,
  data=train_set,
  kernel="polynomial",
  ranges=list(
    cost=costs,
    degree=degrees,
    coef0=coef0s),
  tunecontrol=tune.control(cross=10),
  tolerance=1e-3, 
  cachesize=500
)


print(tune_poly)
best_poly_full<-tune_poly_full$best.model
cat("Best parameters:\n",
    " cost   =", best_poly_full$cost,"\n",
    " degree =", best_poly_full$degree,"\n",
    " coef0  =", best_poly_full$coef0, "\n"
)


svm_poly_final_full <- svm(
  quality.code ~ .,
  data= train_set,
  kernel="polynomial",
  cost=best_poly_full$cost,
  degree=best_poly_full$degree,
  coef0=best_poly_full$coef0,
  probability=TRUE,
  tolerance=1e-3,
  cachesize=200
)

pred_poly_full<-predict(svm_poly_final_full, newdata=test_set)
acc_poly_full<-mean(pred_poly_full==test_set$quality.code)

cm_poly_full<-table(Predicted=pred_poly_full,
               Actual=test_set$quality.code)
print(cm_poly_full)
cat("Test-set Accuracy (poly):", round(acc_poly_full, 4), "\n")



# 9.5 Nonlinear SVM in two dimension with RBF kernel
costs<-c(0.1, 1, 10, 100)
gammas<-c(0.001, 0.01, 0.1, 1)

tune_rbf<-tune(
  svm,
  quality.code ~ alcohol+volatile.acidity,
  data=train_set,
  kernel="radial",
  ranges=list(cost=costs, gamma=gammas),
  tunecontrol=tune.control(cross=10),
  tolerance=1e-3,
  cachesize=500)

print(tune_rbf)
best_rbf<-tune_rbf$best.model
cat("Best parameters:\n",
    " cost  =", best_rbf$cost, "\n",
    " gamma =", best_rbf$gamma, "\n"
)

svm_rbf_final<-svm(
  quality.code ~ alcohol+volatile.acidity,
  data=train_set,
  kernel="radial",
  cost=best_rbf$cost,
  gamma=best_rbf$gamma,
  probability=TRUE,
  tolerance=1e-3,
  cachesize=200)

pred_rbf<-predict(svm_rbf_final, newdata=test_set)
acc_rbf<-mean(pred_rbf==test_set$quality.code)

cm_rbf <-table(Predicted=pred_rbf,
                Actual= test_set$quality.code)
print(cm_rbf)
cat("Test-set Accuracy (RBF):", round(acc_rbf, 4), "\n")


al_seq<-seq(min(train_set$alcohol), max(train_set$alcohol), length = 200)
va_seq<-seq(min(train_set$volatile.acidity), max(train_set$volatile.acidity), length = 200)
grid_rbf<-expand.grid(alcohol=al_seq, volatile.acidity=va_seq)

grid_rbf$pred_class<-as.numeric(predict(svm_rbf_final, newdata=grid_rbf))

ggplot()+
  geom_point(data=test_set,
             aes(x=alcohol, y=volatile.acidity, color=factor(quality.code)),
             alpha=0.6, size=2)+
  geom_contour(data=grid_rbf,
               aes(x=alcohol, y=volatile.acidity, z=pred_class),
               breaks=1.5, linetype="solid", color="black")+
  scale_color_manual(
    values=c("0" = "#4C72B0", "1" = "#C44E52"),
    labels=c("Low quality", "High quality")
    )+
  labs(
    title="Nonlinear SVM Decision Boundary with RBF Kernel",
    x="Alcohol", y="Volatile Acidity",
    color="Quality Code"
  )+
  theme_minimal(base_family= "Arial", base_size=14) +
  theme(
    plot.title=element_text(face="bold", size=16, hjust=0.5),
    axis.title=element_text(size=12),
    axis.text=element_text(size=11),
    legend.position=c(0.85, 0.15),
    legend.background=element_rect(fill=alpha("white", 0.7), color= NA),
    panel.grid.major=element_line(color="gray90"),
    panel.grid.minor=element_blank()
  )


# 9.6 Nonlinear SVM in full feature space with RBF kernel
costs<-c(0.1, 1, 10, 100)
gammas<-c(0.001, 0.01, 0.1, 1)

tune_rbf_full<-tune(
  svm,
  quality.code ~ .,
  data= train_set,
  kernel="radial",
  ranges=list(cost=costs, gamma=gammas),
  tunecontrol=tune.control(cross=10),
  tolerance=1e-3,
  cachesize=500
)

print(tune_rbf_full)
best_rbf_full <- tune_rbf_full$best.model
cat("Best parameters:\n",
    " cost  =", best_rbf_full$cost, "\n",
    " gamma =", best_rbf_full$gamma, "\n"
)

svm_rbf_final_full <- svm(
  quality.code ~ .,
  data=train_set,
  kernel="radial",
  cost=best_rbf_full$cost,
  gamma=best_rbf_full$gamma,
  probability=TRUE,
  tolerance=1e-3,
  cachesize= 200
)

pred_rbf_full<-predict(svm_rbf_final_full, newdata=test_set)
acc_rbf_full<-mean(pred_rbf_full==test_set$quality.code)

cm_rbf_full<-table(Predicted=pred_rbf_full, Actual=test_set$quality.code)
print(cm_rbf_full)
cat("Test-set Accuracy (RBF):", round(acc_rbf_full, 4), "\n")


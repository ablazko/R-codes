# Author: Andre Blazko
# Version: 2.0
# Created in: 2015-08-03
# Last updated in: 2016-03-28


# Goodness of Fit Metrics (for prediction models with target [0;1])
funcGFMmetrics = 
function (var.target="", var.prediction="", var.df="")
{
    # Mandatory library
    library(reshape2) # upload 'reshape2' package
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"
    
    # Basic sanity tests
    if (nrow(df)>=50)                  {san_test1="ok"} else {san_test1="nok"}
    if (length(unique(df$Target))==2 ) {san_test2="ok"} else {san_test2="nok"}
    
    if (san_test1=="ok" & san_test2=="ok")
    {
        # Dataframe preparation
        df = df[order(-df$Score), ]
        df$Percentile = round((floor(1:dim(df)[1] * 500 / (dim(df)[1]+1)) + 1) * (100/500), 0)
        df$Predicted = with(df, as.factor(ifelse(Score>=mean(Score), 1, 0)))
        
        # Precision, Recall, F1, Accuracy calculation
        confusionMatrix = c(table(df$Predicted, df$Target))
        TN = confusionMatrix[1]
        FP = confusionMatrix[2]
        FN = confusionMatrix[3]
        TP = confusionMatrix[4]
        
        # Accuracy and Error Rate calculation
        var.Accuracy = (TP + TN) / sum(confusionMatrix)
        var.ErrorRate = 1 - var.Accuracy
        
        # Kappa calculation (agreement level between prediction and true labels)
        pr_e = ((TN + FP) / sum(confusionMatrix)) * ((TN + FN) / sum(confusionMatrix)) +
               ((FN + TP) / sum(confusionMatrix)) * ((FP + TP) / sum(confusionMatrix))
        var.Kappa = (var.Accuracy - pr_e) / (1 - pr_e)
        
        # Precision, Recall and F1 calculation
        var.Precision = TP / (TP + FP) # equals 'predicted' hit rate   
        var.Recall = TP / (TP + FN)    # equals sensitivity = 'true' hit rate
        var.F1 = (2 * var.Precision * var.Recall) / (var.Precision + var.Recall) # harmonic mean
        
        # Logloss calculation (uncertainty of the probabilities of the model by comparing them to the true labels)
        pred1 = c(df$Score)
        if (max(pred1)>1) {pred1 = rescale(pred1, to=c(0.001, 0.999))}
        pred2 = 1 - pred1
        pred <- cbind(pred1,pred2)
        
        act1 <- c(pred1 / pred1)
        act2 <- c(pred1 - pred1)
        act <- cbind(act1,act2)
    
        eps = 1e-15;
        nr = nrow(pred)
        pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
        pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
        ll = sum(act*log(pred) + (1-act)*log(1-pred))
        var.LogLoss = ll * -1/(nrow(act))
        
        # Basic counts calculation for next metrics
        df = aggregate(. ~ Target + Percentile, data=df, function(x) length(x))
        colnames(df)[3]="Count"
        df = reshape2::dcast(df, Percentile ~ Target, value.var="Count")
        colnames(df)[2]="N_0"
        colnames(df)[3]="N_1"
        df$N_0 = ifelse(is.na(df$N_0), 0, df$N_0)
        df$N_1 = ifelse(is.na(df$N_1), 0, df$N_1)
    
        # KS calculation
        df$pctCol_1_acm = cumsum(df$N_1) / sum(df$N_1) # true-positive (Sensitivity)
        df$pctCol_0_acm = cumsum(df$N_0) / sum(df$N_0) # false-positive
        df$abs_dif = abs(df$pctCol_1_acm - df$pctCol_0_acm)      # temporary
        var.KS = max(df$abs_dif)
    
        # AUC and GINI calculation
        df$Xtime  = df$pctCol_0_acm # False Positive Rate
        df$Yvalue = df$pctCol_1_acm # Sensitivity
    
        df$LagTime  = c(rep(NA, 1), df$Xtime)[1:length(df$Xtime)]    # ~lag() function in R
        df$LagValue = c(rep(NA, 1), df$Yvalue)[1:length(df$Yvalue)]  # ~lag() function in R
    
        df$LagTime  = ifelse(is.na(df$LagTime), 0, df$LagTime)
        df$LagValue = ifelse(is.na(df$LagValue), 0, df$LagValue)
    
        df$LagTime  = ifelse(df$Xtime==0, 0, df$LagTime)
        df$LagValue = ifelse(df$Xtime==0, 0, df$LagValue)
    
        df$Trapezoid = (df$Xtime-df$LagTime)*(df$Yvalue+df$LagValue)/2
        df$SumTrapezoid = cumsum(df$Trapezoid)
    
        var.AUC  = max(df$SumTrapezoid)
        var.GINI = 2 * max(df$SumTrapezoid) - 1
        
        # Salve the performance metrics in a vector
        var.FitMetrics = c(var.AUC, var.KS, var.GINI, var.LogLoss, var.Kappa, var.F1, var.Accuracy, var.ErrorRate, var.Precision, var.Recall)
        var.FitMetrics = round(var.FitMetrics, 5)
        names(var.FitMetrics) = c("AUC", "KS", "GINI", "LogLoss", "Kappa", "F1", "Accuracy", "ErrorRate", "Precision", "Recall")
        return(var.FitMetrics)
    
        # Interpretation of the measures:
        #     
        # AUC        = total area under the ROC Curve
        # KS         = maximum distance between cumulative distributions frequencies of the target and no-target
        # GINI       = measure the inequality level among values of the frequency distribution between target and no-target
        # Logloss    = uncertainty of the probabilities of the model by comparing them to the true labels
        # Kappa      = agreement level between prediction and true labels
        # F1         = harmonic mean between Precision and Recall measures
        # Accuracy   = overall accuracy of the model considering target and no-target predictions
        # Error Rate = overall error of the model considering target and no-target predictions
        # Precision  = 'predicted' hit rate
        # Recall     = 'true' hit rate
        
    } else
    {
        cat("\nWARNING: data frame must have >= 50 rows AND target variable must be of length 2!\n")
    }
}
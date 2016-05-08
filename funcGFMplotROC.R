# Author: Andre Blazko
# Version: 2.0
# Created in: 2014-04-20
# Last updated in: 2016-03-15


# Goodness of Fit Metrics 'ROC plot' (for prediction models with target [0;1])
funcGFMplotROC = 
function (var.target="", var.prediction="", var.group="", var.df="", var.fsize)
{
    ## Parameters tips
    # 'var.fsize' = reference for the text font size used in ggplot2. Default value is set to '3'.
    # 'var.group' = identify the grouping variable. Default value is set to NULL.
    
    # Mandatory library
    library(reshape2) # upload 'reshape2' package
    library(ggplot2)  # upload 'ggplot2' package
    
    # Dataframe preparation
    if (!(exists('var.fsize'))) {var.fsize=3}
    if (var.group=="")
    {
        dfref = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
        var.levels=var.group

    } else
    {
        dfref = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction, var.group))
        colnames(dfref)[3]="Group"
        dfref$Group = as.factor(dfref$Group)
        var.levels = levels(dfref$Group)
    }
    colnames(dfref)[1]="Target"
    colnames(dfref)[2]="Score"
    
    # Basic sanity tests
    if (nrow(dfref)>=50)                  {san_test1="ok"} else {san_test1="nok"}
    if (length(unique(dfref$Target))==2 ) {san_test2="ok"} else {san_test2="nok"}
    
    if (san_test1=="ok" & san_test2=="ok")
    {
        if (length(var.levels)==1)
        {        
            # Dataframe preparation
            df = dfref[order(-dfref$Score), ]
            df$Percentile = round((floor(1:dim(df)[1] * 500 / (dim(df)[1]+1)) + 1) * (100/500), 0)
            
            # Basic counts calculation
            df = aggregate(. ~ Target + Percentile, data=df, function(x) length(x))
            colnames(df)[3]="Count"
            df = dcast(df, Percentile ~ Target, value.var="Count")
            colnames(df)[2]="N_0"
            colnames(df)[3]="N_1"
            df$N_0 = ifelse(is.na(df$N_0), 0, df$N_0)
            df$N_1 = ifelse(is.na(df$N_1), 0, df$N_1)
        
            # ROC axies calculation
            df$pctCol_1_acm = round(cumsum(df$N_1) / sum(df$N_1), 4) # true-positive (Sensitivity)
            df$pctCol_0_acm = round(cumsum(df$N_0) / sum(df$N_0), 4) # false-positive
        
            # AUC calculation
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
        
            var.AUC  = round(max(df$SumTrapezoid), 4)
        
            # ROC Curve generation
            q = ggplot(data=df, aes(x=pctCol_0_acm, y=pctCol_1_acm))
            q = q + theme_grey()
            q = q + theme(legend.position="top", title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
            q = q + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
            q = q + ggtitle(paste0("Receiver Operating Characteristic Curve (AUC = ", var.AUC, ")\n", var.levels[1]))
            q = q + geom_ribbon(aes(x=pctCol_0_acm, ymin=pctCol_0_acm, ymax=pctCol_1_acm), colour="black", alpha=0.2, size=0)
            q = q + geom_line(colour="blue")
            q = q + geom_line(aes(x=0, y=1), colour="black")
            q = q + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
            q = q + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
            q = q + xlab("\nFalse Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)\n")
            return(q)
            
        } else
        {
            int = 1
            for (k in var.levels)
            {
                # Dataframe preparation
                df = dfref[Group==k, ]
                df = droplevels(df)
                df = df[order(-df$Score), ]
                df$Percentile = round((floor(1:dim(df)[1] * 500 / (dim(df)[1]+1)) + 1) * (100/500), 0)
    
                # Basic counts calculation
                df = aggregate(. ~ Target + Percentile, data=df, function(x) length(x))
                colnames(df)[3]="Count"
                df = dcast(df, Percentile ~ Target, value.var="Count")
                colnames(df)[2]="N_0"
                colnames(df)[3]="N_1"
                df$N_0 = ifelse(is.na(df$N_0), 0, df$N_0)
                df$N_1 = ifelse(is.na(df$N_1), 0, df$N_1)
    
                # ROC axies calculation
                df$pctCol_1_acm = round(cumsum(df$N_1) / sum(df$N_1), 4) # true-positive (Sensitivity)
                df$pctCol_0_acm = round(cumsum(df$N_0) / sum(df$N_0), 4) # false-positive
    
                # AUC calculation
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
                var.AUC = round(max(df$SumTrapezoid), 4)
                df$Group = paste0(k, " [", var.AUC, "]")
    
                if (int==1) {df_end = df} else
                            {df_end = rbind(df_end, df)}
                
                int = int + 1
            }
            df$Group = as.factor(df$Group)
        
            # ROC Curve generation
            q = ggplot(data=df_end, aes(x=pctCol_0_acm, y=pctCol_1_acm, colour=Group))
            q = q + theme_grey()
            q = q + theme(legend.position="top", title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
            q = q + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
            q = q + ggtitle("Receiver Operating Characteristic Curve [AUC]")
            q = q + geom_abline(intercept=0, slope=1.0, colour="black", size=0.2, alpha=0.5)
            q = q + geom_line(size=1.5, alpha=0.8)
            q = q + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
            q = q + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
            q = q + xlab("\nFalse Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)\n")
            q = q + labs(colour="")
            return(q)
        }
    }
}
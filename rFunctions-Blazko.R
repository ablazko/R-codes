#---------------------------------------------------------------------------------------#
#                                       R-Functions                                     #
#---------------------------------------------------------------------------------------#
# R code written by         | Andre Blazko                                              #
# Start date                | 05/08/2014                                                #
#---------------------------------------------------------------------------------------#

# Colors references
# q = q + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
# q = q + scale_fill_brewer(palette="Spectral")
# RColorBrewer::display.brewer.all()




##### Functions


library("coin")
funcWilcoxTest =
    function(data, test.group1, test.group2, measure.name)
    {        
    tmp.data1 = data[data$Group %in% c(test.group1, test.group2) & data$Measure==measure.name, c("Group", "Value")]
    tmp.data1 = droplevels(tmp.data1)
    wilcox_test (Value ~ Group, data=tmp.data1, distribution="asymptotic")
    }


library("ggplot2")
library("doBy")
funcBoxplot =
    function(data, tot.groups, test.group1, test.group2, test.group3, test.group4, test.group5, measure.name, measure.label, measure.unit, jitter)
    {
    if (tot.groups==2)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2) & data$Measure==measure.name, c("Group", "Value")]}

    if (tot.groups==3)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3) & data$Measure==measure.name, c("Group", "Value")]}
    
    if (tot.groups==4)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3, test.group4) & data$Measure==measure.name, c("Group", "Value")]}
        
    if (tot.groups==5)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3, test.group4, test.group5) & data$Measure==measure.name, c("Group", "Value")]}

    tmp.data1 = droplevels(tmp.data1)

    q = qplot(data=tmp.data1, Group, Value, geom="boxplot", fill=Group, main=measure.label, alpha=0.8)
    if (jitter=="yes") { q = q + geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width=0.1), size=5, shape=1) }
#     q = q + xlab("") + ylab(expression(paste(" (em ", mu^2, ")")))
    q = q + xlab("") + ylab(measure.unit)
    q = q + theme(legend.position="none", title=element_text(size=15), text=element_text(colour="black", size=20))
    q = q + scale_fill_manual(values=c("#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
    q
    }


funcHistogramPlot =
    function(data, density, vline.ref, tot.groups, test.group1, test.group2, test.group3, test.group4, test.group5, measure.name, measure.label, measure.unit)
    {
    if (tot.groups==2)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2) & data$Measure==measure.name, c("Group", "Value")]}

    if (tot.groups==3)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3) & data$Measure==measure.name, c("Group", "Value")]}
    
    if (tot.groups==4)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3, test.group4) & data$Measure==measure.name, c("Group", "Value")]}
        
    if (tot.groups==5)
    {tmp.data1 = data[data$Group %in% c(test.group1, test.group2, test.group3, test.group4, test.group5) & data$Measure==measure.name, c("Group", "Value")]}

    tmp.data1 = droplevels(tmp.data1)    
    tmp.data2 = data.frame(summaryBy(Value ~ Group, data=tmp.data1, FUN=c(mean, median, min, max)))
    min.value = min(tmp.data2$Value.min) * 0.9
    max.value = max(tmp.data2$Value.max) * 1.1
    round.ref = (max.value-min.value)/10
    round.dig = if (round.ref<1) {if (round.ref<0.1) {if (round.ref<0.01) {3} else {2}} else {1}} else {0}
    tmp.data1$Value = round(tmp.data1$Value, round.dig)
    vline.ref.label = if (vline.ref=="mean") {"Médias"} else {"Medianas"}
    
    q = ggplot(data=tmp.data1, aes(x=Value, fill=Group))
    if (density=="TRUE"){
        q = q + geom_density(alpha=0.6)
#         q = q + xlab(expression(paste(" (em ", mu^2, ")"))) + ylab(paste("Densidade", vline.ref.label, sep=" & "))
        q = q + xlab("") + ylab(paste("Densidade", vline.ref.label, sep=" & "))
    } else {
        q = q + geom_histogram(binwidth=((max.value-min.value)/100), alpha=0.6, position="identity") # ou position="dodge"
#         q = q + xlab(expression(paste(" (em ", mu^2, ")"))) + ylab(paste("Quantidade", vline.ref.label, sep=" & "))
        q = q + xlab("") + ylab(paste("Quantidade", vline.ref,label, sep=" & "))
    }
    q = q + theme(legend.justification=c(1,1), legend.position=c(1,1), legend.background=element_rect(fill="gray90"))
    q = q + theme(title=element_text(size=15), text=element_text(colour="black", size=15))
    q = q + theme(axis.text.x=element_text(size=15, angle=45, vjust=0.5, hjust=0.5))
    q = q + scale_x_continuous(limits = c(min.value, max.value), breaks = seq(round(min.value, round.dig), round(max.value, round.dig), round(round.ref, round.dig)))
    q = q + ggtitle(measure.label)
#     q = q + ggtitle(paste(measure.label, measure.unit, sep=" "))
    
    if (vline.ref=="mean"){
        q = q + geom_vline(data=tmp.data2, aes(xintercept=Value.mean, colour=Group), linetype="dashed", size=1)
    }
    if (vline.ref=="median"){
        q = q + geom_vline(data=tmp.data2, aes(xintercept=Value.median, colour=Group), linetype="dashed", size=1)
    }
    q = q + scale_fill_manual(values=c("#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
    q = q + scale_colour_manual(values=c("#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
    q = q + labs(fill="")
    q
    }


funcRCopy =
    function(table)
    {
    clip = pipe("pbcopy", "w")
    write.table(table, clip, sep="\t", dec=".", row.names=FALSE, col.names=TRUE)
    close(clip)
    }

funcRPaste =
    function()
    {
    read.delim(pipe("pbpaste"), header=TRUE, sep="\t", dec=".")
    }


funcCleanText <- function(text, tolower.option)
{
    text = trimws(text, "both")
    text = gsub("[ \t]{2,}", " ", text)
    text = gsub(" {2,}", " ", text)
    text = gsub("^\\s+|\\s+$", "", text)
    text = gsub("[[:punct:]]", " ", text)
    text = gsub("[[:digit:]]", " ", text)
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", text)
    text = gsub("@\\w+", " ", text)
    text = gsub("http\\w+", " ", text)
    text = gsub("amp", " ", text)
    
    if (tolower.option=="yes") {text = tolower(text)}
    
    text = gsub("á", "a", text)
    text = gsub("é", "e", text)
    text = gsub("í", "i", text)
    text = gsub("ó", "o", text)
    text = gsub("ú", "u", text)
    text = gsub("ã", "a", text)
    text = gsub("õ", "o", text)
    text = gsub("â", "a", text)
    text = gsub("ê", "e", text)
    text = gsub("ô", "o", text)
    text = gsub("ü", "u", text)
    text = gsub("ü", "u", text)
    text = gsub("ç", "c", text)
    text = gsub("ñ", "n", text)
    text = gsub("à", "a", text)

    text = gsub("Á", "A", text)
    text = gsub("É", "E", text)
    text = gsub("Í", "I", text)
    text = gsub("Ó", "O", text)
    text = gsub("Ú", "U", text)
    text = gsub("Ã", "A", text)
    text = gsub("Õ", "O", text)
    text = gsub("Â", "A", text)
    text = gsub("Ê", "E", text)
    text = gsub("Ô", "O", text)
    text = gsub("Ü", "U", text)
    text = gsub("Ç", "C", text)
    text = gsub("Ñ", "N", text)
    text = gsub("À", "A", text)    
}
 

funcWriteCSV =
    function (data, outputFile, outputPath)
    {
    write.table(data, paste(outputPath, outputFile, sep="/"), sep=";", dec=",", row.names=F, col.names=T)
    }


# Function to recover the labels accordingly to their specific indexes
funcGetLabels = function (var.table.target="", var.table.source="", var.id="", var.desc="")
{
    # Create the vector containing the index vs labels
    var.a = as.matrix(subset(eval(parse(text=var.table.source)), select=c(var.id, var.desc)))
    var.a = trimws(var.a)
    var.a.labels = as.vector(var.a[,1])
    names(var.a.labels) = as.vector(var.a[,2])
    
    # Modify the dataframe/column with the specific labels for that indexes
    df = eval(parse(text=var.table.target))
    df[,var.desc] = sapply(df[,var.desc]
                           ,function(x=df[,var.desc])
                           {  ifelse(x=="[Total]", "[Total]", names(var.a.labels[var.a.labels==as.integer(x)]))  }
    )
    return (df)

}


# Goodness of Fit Metrics Table (for prediction models with target [0;1])
funcGFMtable = 
function (var.target="", var.prediction="", var.df="", var.ngroups)
{
    library(reshape2) # upload 'reshape2' package
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"    
    df = df[order(-df$Score), ]
    df$Percentile = round((floor(1:dim(df)[1] * var.ngroups / (dim(df)[1]+1)) + 1) * (100/var.ngroups), 0)

    # Cutoff calculation of each percentile
    df.cutoff = aggregate(Score ~ Target + Percentile, data=df, function(x) round(max(x), 4))
    colnames(df.cutoff)[3]="Cutoff_max"
    df.cutoff = subset(df.cutoff, Target==1, select=-Target)
    
    # Basic counts calculation
    df             = aggregate(. ~ Target + Percentile, data=df, function(x) length(x))
    colnames(df)[3]="Count"
    df             = dcast(df, Percentile ~ Target, value.var="Count")
    colnames(df)[2]="N_0"
    colnames(df)[3]="N_1"
    df$N_0         = ifelse(is.na(df$N_0), 0, df$N_0)
    df$N_1         = ifelse(is.na(df$N_1), 0, df$N_1)

    # Metrics calculation
    df$pctRow_1     = round(df$N_1 / (df$N_1 + df$N_0), 4)
    df$pctRow_1_acm = round(cumsum(df$N_1) / (cumsum(df$N_1) + cumsum(df$N_0)), 4)
    df$pctCol_1     = round(df$N_1 / sum(df$N_1), 4)
    df$pctCol_0     = round(df$N_0 / sum(df$N_0), 4)
    df$pctCol_1_acm = round(cumsum(df$N_1) / sum(df$N_1), 4)                        # true-positive (Sensitivity)
    df$pctCol_0_acm = round(cumsum(df$N_0) / sum(df$N_0), 4)                        # false-positive
    var.pctTot_1    = round(sum(df$N_1) / (sum(df$N_1) + sum(df$N_0)), 4)           # temporary
    df$Lift_acm     = round(df$pctRow_1_acm / var.pctTot_1, 4)
    df$Accuracy     = round((cumsum(df$N_1) + (sum(df$N_0) - cumsum(df$N_0))) / (sum(df$N_1) + sum(df$N_0)), 4)
    df$IFP_acm      = round(1 / (cumsum(df$N_1) / (cumsum(df$N_1) + cumsum(df$N_0))), 1)

    # Output dataframe
    df.GFMetrics = merge (x=df, y=df.cutoff, by="Percentile", suffices=c("",""), all.x=T)
    df.GFMetrics$Cutoff_max = ifelse(is.na(df.GFMetrics$Cutoff_max), 0, df.GFMetrics$Cutoff_max)
    return(df.GFMetrics)
}


# Percentile Analysis Plot (for prediction models with target [0;1])
funcPlotPercentileAnalysis = 
function (var.target="", var.prediction="", var.df="", var.group="", var.title="", var.fsize=5)
{
    library(ggplot2)  # upload 'ggplot2'  package
    var.ngroups = 20  # number of percentiles to be plotted
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction, var.group))
    colnames(df)[1]="target"
    colnames(df)[2]="score"
    colnames(df)[3]="group"
    df$target = as.integer(as.character(df$target))
    df$group = as.factor(df$group)
    
    # Percentile's calculation for each group to be compared
    var.levels = length(levels(df$group))
    for (k in 1:var.levels)
    {
        df.tmp1 = droplevels(subset(df, group==levels(df$group)[k]))
        df.tmp1 = df.tmp1[order(-df.tmp1$score), ]
        df.tmp1$percentile = round((floor(1:dim(df.tmp1)[1] * var.ngroups / (dim(df.tmp1)[1]+1)) + 1) * (100/var.ngroups), 0)
        df.tmp2 = as.data.frame(prop.table(table(df.tmp1$percentile, df.tmp1$target), 1))
        colnames(df.tmp2)[1]="Percentile"
        colnames(df.tmp2)[2]="Target"
        colnames(df.tmp2)[3]="pctRow"
        rownames(df.tmp2) = NULL
        df.tmp2$group = levels(df.tmp1$group)
        colnames(df.tmp2)[4]=var.group
        df.tmp2$pctRow = round(df.tmp2$pctRow*100, 1)
        df.tmp2 = subset(df.tmp2, Target==1)
    
        if (k==1) 
            {df.final = df.tmp2} else
            {df.final = rbind(df.final, df.tmp2)}
    }
    rownames(df.final) = NULL
    rm(k)
    
    # Bar plot generation
    var.hr.avg      = round((sum(as.numeric(df$target)) / dim(df)[1])*100, 1)
    df.final$hr_avg = var.hr.avg
    
    if (var.levels==1) {q = ggplot(data=df.final, aes(x=Percentile, y=pctRow), environment=environment())} else
                       {q = ggplot(data=df.final, aes(x=Percentile, y=pctRow, fill=df.final[,4]), environment=environment())}
    q = q + theme_grey()
    q = q + theme(legend.position="bottom", title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
    q = q + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
    q = q + ggtitle(paste(var.title))
    q = q + geom_bar(stat="identity", position="dodge")
    
    if (var.levels<=5) {q = q + scale_fill_manual(values=c("#58ACFA", "#767472", "#A4A4A4", "#01DFD7", "#FA8258"))}
    
    q = q + geom_hline(aes(yintercept=hr_avg), colour="red", linetype="dashed", size=0.7)
    q = q + annotate("text", x=var.ngroups, y=(var.hr.avg), label=paste("Avg. Hit Rate: ",var.hr.avg, "%", sep=""), hjust=1, vjust=-1.5, size=var.fsize, colour="red")
    q = q + xlab("Percentile Score") + ylab(paste("% Hit Rate (",var.target,"=1)", sep=""))
    q = q + labs(fill=var.group)
    return(q)
}


# Goodness of Fit Metrics (for prediction models with target [0;1])
funcGFMmetrics = 
function (var.target="", var.prediction="", var.df="")
{
    library(reshape2) # upload 'reshape2' package
    library(scales)   # upload 'scales' package
        
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"
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
}


# Goodness of Fit Metrics 'ROC plot' (for prediction models with target [0;1])
funcGFMplotROC = 
function (var.target="", var.prediction="", var.df="", var.fsize)
{
    library(reshape2) # upload 'reshape2' package
    library(ggplot2)  # upload 'ggplot2' package
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"    
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

    df$Trapezoid = (df$Xtime-df$LagTime)*(df$Yvalue+df$LagValue)/2;
    df$SumTrapezoid = cumsum(df$Trapezoid)

    var.AUC  = round(max(df$SumTrapezoid), 4)

   # ROC Curve generation
    plotROC = ggplot(data=df, aes(x=pctCol_0_acm, y=pctCol_1_acm))
    plotROC = plotROC + theme_grey()
    plotROC = plotROC + theme(legend.position="bottom", title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
    plotROC = plotROC + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
    plotROC = plotROC + ggtitle(paste("Receiver Operating Characteristic Curve (AUC =", var.AUC, ")"))
    plotROC = plotROC + geom_ribbon(aes(x=pctCol_0_acm, ymin=pctCol_0_acm, ymax=pctCol_1_acm), colour="black", alpha=0.2, size=0)
    plotROC = plotROC + geom_line(colour="blue")
    plotROC = plotROC + geom_line(aes(x=0, y=1), colour="black")
    plotROC = plotROC + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
    plotROC = plotROC + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
    plotROC = plotROC + xlab("False Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)")
    return(plotROC)
}


# Goodness of Fit Metrics 'KS plot' (for prediction models with target [0;1])
funcGFMplotKS = 
function (var.target="", var.prediction="", var.df="", var.fsize)
{
    library(reshape2) # upload 'reshape2' package
    library(ggplot2)  # upload 'ggplot2' package
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"    
    df = df[order(-df$Score), ]
    df$Percentile = round((floor(1:dim(df)[1] * 500 / (dim(df)[1]+1)) + 1) / 500, 4)
    
    # Basic counts calculation
    df = aggregate(. ~ Target + Percentile, data=df, function(x) length(x))
    colnames(df)[3]="Count"
    df = dcast(df, Percentile ~ Target, value.var="Count")
    colnames(df)[2]="N_0"
    colnames(df)[3]="N_1"
    df$N_0 = ifelse(is.na(df$N_0), 0, df$N_0)
    df$N_1 = ifelse(is.na(df$N_1), 0, df$N_1)

    # KS calculation
    df$pctCol_1_acm = round(cumsum(df$N_1) / sum(df$N_1), 4) # true-positive (Sensitivity)
    df$pctCol_0_acm = round(cumsum(df$N_0) / sum(df$N_0), 4) # false-positive
    df$abs_dif = abs(df$pctCol_1_acm - df$pctCol_0_acm)      # temporary
    var.KS = max(df$abs_dif)

    # KS positions in the graph
    var.positions    = as.vector(subset(df, Percentile==0.5, select=c(Percentile, pctCol_1_acm, pctCol_0_acm)))
    var.ypos.target1 = var.positions[[2]]
    var.ypos.target0 = var.positions[[3]]

    var.positions    = as.vector(tail(subset(df, abs_dif==var.KS, select=c(Percentile, pctCol_1_acm, pctCol_0_acm, abs_dif)), 1))
    var.xpos.ks      = var.positions[[1]]
    var.ypos1.ks     = var.positions[[2]]
    var.ypos2.ks     = var.positions[[3]]
   
    # KS graph generation
    plotKS = ggplot(data=df)
    plotKS = plotKS + theme_grey()
    plotKS = plotKS + theme(title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
    plotKS = plotKS + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
    plotKS = plotKS + ggtitle(paste("Kolmogorov Smirnov Curve (KS =", var.KS, ")"))
    plotKS = plotKS + geom_line(aes(x=Percentile, y=pctCol_0_acm), colour="blue")
    plotKS = plotKS + geom_line(aes(x=Percentile, y=pctCol_1_acm), colour="red")
    plotKS = plotKS + geom_point(aes(x=var.xpos.ks, y=var.ypos1.ks), colour="black", size=0.5) # KS line
    plotKS = plotKS + geom_point(aes(x=var.xpos.ks, y=var.ypos2.ks), colour="black", size=0.5) # KS line
    plotKS = plotKS + geom_segment(aes(x=var.xpos.ks, xend=var.xpos.ks, y=var.ypos1.ks, yend=var.ypos2.ks), colour="black", linetype="dashed", size=0.1) # KS line
    plotKS = plotKS + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
    plotKS = plotKS + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
    plotKS = plotKS + xlab("Observations Rate") + ylab("Hit Rate")
    plotKS = plotKS + annotate("text", x=0.5, y=var.ypos.target1, label="Target=1", colour="red", hjust=1, vjust=-1, size=var.fsize)
    plotKS = plotKS + annotate("text", x=0.5, y=var.ypos.target0, label="Target=0", colour="blue", hjust=-0.2, vjust=1, size=var.fsize)
    # plotKS = plotKS + annotate("text", x=var.xpos.ks, y=0.55, label=paste("KS=", var.KS, sep=""), hjust=-0.1, size=var.fsize)
    return(plotKS)
}


# Goodness of Fit Metrics 'Score Density plot' (for prediction models with target [0;1])
funcGFMplotDensity = 
function (var.target="", var.prediction="", var.df="", var.fsize)
{
    library(ggplot2)  # upload 'ggplot2' package
    
    # Dataframe preparation
    df = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(df)[1]="Target"
    colnames(df)[2]="Score"
    
   # Density Curve generation
    q = ggplot(data=df, aes(x=Score, fill=as.factor(Target), colour=Target))
    q = q + theme_grey()
    q = q + theme(title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
    q = q + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
    q = q + theme(legend.position='right')
    q = q + guides(fill=guide_legend(override.aes=list(colour=NULL))) # delete 'slash' inside the legend
    q = q + geom_density(alpha=0.6, adjust=2)
    q = q + scale_fill_manual(values=c("#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
    q = q + scale_colour_manual(values=c("#04B4AE", "#F8766D", "#2E9AFE", "#FACC2E", "#E32DFB"))
    q = q + xlab("Score") + ylab("")
    q = q + ggtitle(paste0("Score Density Distribution (", var.prediction, ")"))
    q = q + labs(fill="Target")
    q
    return(q)
}


# Create a 'score' variable in the interval [1;999] (for prediction models with target [0;1])
funcCreateScore =
function(var.target="", var.prediction="", var.df="", var.pctTarget1)
{
    # Mandatory parameters:
    #   - var.prediction
    #   - var.df
    # 
    # Optional parameters:
    #   - var.target
    #   - var.pctTarget1
    #
    # Note: If 'var.pctTarget1' is passed (known), then the parameter 'var.target' is ignored. Usually used for score code procedures.
    
    df.tmp = subset(eval(parse(text=var.df)))

    if (is.null(var.pctTarget1))
        {
            var.1 = sum(eval(parse(text=paste("df.tmp$", var.target, sep=""))))
            var.2 = dim(df.tmp)[1]
            var.pctTarget = var.1 / var.2
        } else
        {var.pctTarget = var.pctTarget1}

    # Score adjustment considering a 50/50 division class (output 'score' between [1;999])
    df.tmp$aux = eval(parse(text=paste("df.tmp$", var.prediction, sep="")))
    df.tmp$P1_score = (df.tmp$aux * 0.5) / var.pctTarget
    df.tmp$P0_score =  (1 - df.tmp$aux) * 0.5 / (1 - var.pctTarget)
    df.tmp$sum      = ((1 - df.tmp$aux) * 0.5 / (1 - var.pctTarget)) + (df.tmp$aux * 0.5 / var.pctTarget)
    df.tmp$P_score  = ifelse(df.tmp$sum>0, (df.tmp$P1_score / df.tmp$sum), df.tmp$P1_score)
    df.tmp$score    = round(df.tmp$P_score * 1000, 0)
    assign(paste(var.df), subset(df.tmp, select=-c(aux, P1_score, P0_score, sum, P_score)))
}


##### This function takes a number and returns a compressed string (e.g. 1624 => 1.6K or 2K, depending on round.by)
# by StackOverflow user 'BondedDust' : http://stackoverflow.com/a/28160474
compress.number = function(x, round.by=1) {
    
    div = findInterval(as.numeric(gsub("\\,", "", x)), c(1, 1e3, 1e6, 1e9, 1e12) )
    n = ifelse(x>1000
               ,paste(round(as.numeric(gsub("\\,", "", x))/10^(3*(div-1)), round.by), c("","K","M","B","T")[div], sep="")
               ,paste0(round(as.numeric(gsub("\\,", "", x)), 0))
            )
    return (n)
}
	
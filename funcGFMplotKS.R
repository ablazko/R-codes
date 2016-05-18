# Author: Andre Blazko
# Version: 2.0
# Created in: 2014-04-20
# Last updated in: 2016-03-15


# Goodness of Fit Metrics 'KS plot' (for prediction models with target [0;1])
funcGFMplotKS = 
function (var.target="", var.prediction="", var.df="", var.fsize)
{
    ## Parameters tips
    # 'var.fsize' = reference for the text font size used in ggplot2. Default value is set to '3'.
    
    # Mandatory library
    library(reshape2) # upload 'reshape2' package
    library(ggplot2)  # upload 'ggplot2' package
    
    # Dataframe preparation
    if (!(exists('var.fsize'))) {var.fsize=3}
    dfref = subset(eval(parse(text=var.df)), select=c(var.target, var.prediction))
    colnames(dfref)[1]="Target"
    colnames(dfref)[2]="Score"
    
    # Basic sanity tests
    if (nrow(dfref)>=50)                  {san_test1="ok"} else {san_test1="nok"}
    if (length(unique(dfref$Target))==2 ) {san_test2="ok"} else {san_test2="nok"}
    
    if (san_test1=="ok" & san_test2=="ok")
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
        
        # KS calculation
        df$pctCol_1_acm = cumsum(df$N_1) / sum(df$N_1) # true-positive (Sensitivity)
        df$pctCol_0_acm = cumsum(df$N_0) / sum(df$N_0) # false-positive
        df$abs_dif = abs(df$pctCol_1_acm - df$pctCol_0_acm)      # temporary
        var.KS = max(df$abs_dif)
    
        # KS positions in the graph
        var.positions    = as.vector(subset(df, Percentile==50, select=c(Percentile, pctCol_1_acm, pctCol_0_acm)))
        var.ypos.target1 = var.positions[[2]]
        var.ypos.target0 = var.positions[[3]]
    
        var.positions    = as.vector(tail(subset(df, abs_dif==var.KS, select=c(Percentile, pctCol_1_acm, pctCol_0_acm, abs_dif)), 1))
        var.xpos.ks      = var.positions[[1]]/100
        var.ypos1.ks     = var.positions[[2]]
        var.ypos2.ks     = var.positions[[3]]
        
        # KS Curve generation
        q = ggplot(data=df)
        q = q + theme_grey()
        q = q + theme(title=element_text(size=3*var.fsize), text=element_text(colour="gray40", size=4*var.fsize))
        q = q + theme(axis.text.x=element_text(size=3*var.fsize, angle=0, vjust=0.5, hjust=0.5))
        q = q + ggtitle(paste0("Kolmogorov Smirnov Curve (KS = ", sprintf("%.4f", var.KS), ")\n"))
        q = q + geom_line(aes(x=Percentile/100, y=pctCol_0_acm), colour="blue")
        q = q + geom_line(aes(x=Percentile/100, y=pctCol_1_acm), colour="red")
        q = q + geom_point(aes(x=var.xpos.ks, y=var.ypos1.ks), colour="black", size=0.5) # KS line
        q = q + geom_point(aes(x=var.xpos.ks, y=var.ypos2.ks), colour="black", size=0.5) # KS line
        q = q + geom_segment(aes(x=var.xpos.ks, xend=var.xpos.ks, y=var.ypos1.ks, yend=var.ypos2.ks), colour="black", linetype="dashed", size=0.1) # KS line
        q = q + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        q = q + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        q = q + xlab("\nObservations Rate") + ylab("Hit Rate\n")
        q = q + annotate("text", x=0.5, y=var.ypos.target1, label="Target=1", colour="red", hjust=1, vjust=-1, size=var.fsize)
        q = q + annotate("text", x=0.5, y=var.ypos.target0, label="Target=0", colour="blue", hjust=-0.2, vjust=1, size=var.fsize)
        q
        return(q)
    }
}
#---------------------------------------------------------------------------------------#
#                                       R-Functions                                     #
#---------------------------------------------------------------------------------------#
# R code written by         | Andre Blazko                                              #
# Start date                | 05/08/2014                                                #
# Last updated in           | 09/10/2016                                                #
#---------------------------------------------------------------------------------------#

# Colors references
# q = q + scale_fill_manual(values=c(
# "#999999", cinza
# "#E69F00", laranja
# "#D55E00", laranja escuro
# "#009E73", verde escuro
# "#F0E442", amarelo escuro
# "#FACC2E", amarelo claro
# "#56B4E9", azul claro
# "#0072B2", azul escuro
# "#2E9AFE", azul claro fosflorescente
# "#CC79A7", roxo claro
# "#04B4AE", verde claro
# "#F8766D", vermelho claro
# "#E32DFB", roxo claro fosflorescente
# ))
# q = q + scale_fill_brewer(palette="Spectral")
# RColorBrewer::display.brewer.all()


##### Libraries (backup)
# 
# library("sqldf")
# library("dplyr")
# library("WriteXLS")
# library("coin")
# library("ggplot2")
# library("doBy")
# library("foreign")
# library("reshape2")
# library("tm")
# library("wordcloud")
# library("caret")
# library("readxl")         --> Leitura e gravacao de planilhas excel .xls e .xlsx
# library("DiagrammeR")     --> Desenha diagramas e fluxogramas interligados
# 
# install.packages('shiny', type='source')
# install.packages('htmlwidgets', type='source')
# library("shiny")
# library("shinydashboard")
# library("shinyjs")
# library("Rcpp")
# library("DT")
# 
# 
# 
# A serem verificados / uteis:
# install.packages("dygraphs")       --> Desenha grafico de series temporais interativo (panning, zooming, shading, annotations)




##### Lista de chamada das funcoes dessa biblioteca
# 
# 
# 
# funcRCopy(table=data)
# funcRPaste()
# funcCleanText(text=)
# 
# funcWriteCSV(data=, outputFile="", outputPath="")
# 
# funcWilcoxTest(
#      data          = mydata.u2
#     ,test.group1   = "Controle"
#     ,test.group2   = "GCTMO"
#     ,measure.name  = "TMV"
# )
# 
# funcBoxplot(
#      data          = mydata.u2
#     ,tot.groups    = 3
#     ,test.group1   = "Controle"
#     ,test.group2   = "GCTMO"
#     ,test.group3   = "GCMO"
#     ,test.group4   = ""
#     ,test.group5   = ""
#     ,measure.name  = "TMNV"
#     ,measure.label = "TMNV"
#     ,measure.unit  = "(em u2)"
# )    
# 
# funcHistogramPlot(
#      data          = mydata.u2
#     ,density       = "TRUE"
#     ,vline.ref     = "mean"
#     ,tot.groups    = 3
#     ,test.group1   = "Controle"
#     ,test.group2   = "GCTMO"
#     ,test.group3   = "GCMO"
#     ,test.group4   = ""
#     ,test.group5   = ""
#     ,measure.name  = "TMV"
#     ,measure.label = "TMV"
#     ,measure.unit  = "(em u2)"
# )
# 
# 

##### Inicio do codigo das funcoes
# 
# 
# 
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


# Round-Robin Tournament tables creation
funcRoundRobingMatches = function (vTeams, nGamesInDuel, isPlayoffs="no")
{
    # Notes
    # vTeams = vector containing the team's names in the competition
    # nGamesInDuel = [1, if is one game per duel; 2, if is two games per duel]
    # isPlayoffs = ["yes", if the games result in elimination of competition; "no", otherwise]
    # 
    # IMPORTANT!
    # If (isPlayoffs=="yes"), it is MANDATORY that the 'vTeams' is submitted to this function considering
    # the order of the final classification (first -> last) in competition
    
    # Sanity checks
    if (is.vector(vTeams))
    { # teams' names are defined as a vector
        
        nTeams = length(vTeams)
        nUniqueNames = length(unique(vTeams))
        if (nTeams==nUniqueNames)
        { # teams' names are unique
            
            if (between(nGamesInDuel, lower=1, upper=2, incbounds=T))
            { # only ONE or TWO games (home and visitor) allowed
                
                if (isPlayoffs=="no")
                { # it is a classification round
                    
                    if (nTeams>2)
                    { # it has at least three teams in competition
                        
                        check_test = "ok"
                        
                    } else
                    { # no sense for a competition!
                        
                        cat("ERROR (nTeams): the number of teams must be greater than 2!\n")
                        check_test = "nok"
                    }
                
                } else
                { # it is playoff rounds
                    
                    if (nTeams %in% c(2,4,8,16))
                    { # only four possibilities: oitavas (16), quartas (8), semi (4) and final (2)!
                        
                        check_test = "ok"
                        
                    } else
                    {
                        cat("ERROR (nTeams): in playoff games the number of teams must be 2, 4, 8 or 16!\n")
                        check_test = "nok"
                    }
                }
                
            } else
            {
                cat("ERROR (nGamesInDuel): the number of duels must be 1 or 2!\n")
                check_test = "nok"
            }
            
        } else
        { # teams' names ARE NOT unique
            
            cat("ERROR (vTeams): duplicated team's names are not allowed!\n")
            check_test = "nok"
        }
        
    } else
    { # teams' names ARE NOT defined as a vector
        
        cat("ERROR (vTeams): the team names must be a vector object!\n")
        check_test = "nok"
    }
    
    if ((check_test=="ok"))
    {
        # Check if nTeam number is even
        if ((nTeams%%2)==1)
        { # make sure nTeams is even
        
            nTeams = nTeams + 1
            GhostTeamIndex <- nTeams
            
        } else
        {
            GhostTeamIndex = 0
        }
        
        # Prepare the matrix initialization position
        v = c(seq(1:nTeams))
        v1 = v[1:(nTeams/2)]
        v2 = v[(nTeams/2+1):nTeams]
        v2 = sort(v2, decreasing=T)
        v = c(v1,v2)
        m = matrix(v, ncol=(nTeams/2), byrow=T)
        
        if (isPlayoffs=="no")
        {
            # Execute the loop to set the duels and respective rounds
            for (R in 1:(nTeams-1))
            { # rounds
                
                for (C in 1:(nTeams/2))
                { # column index
                    
                    if (R==1)
                    { # first round is set, no need for rotation
                        
                        tmp = data.frame(
                             home = m[1,C]
                            ,visitor = m[2,C]
                            ,round = R
                            ,group = "Grupo 1"
                        )
                        if (exists("dftmp_duels")) {dftmp_duels = rbind(dftmp_duels,tmp)} else {dftmp_duels <- tmp}
                        
                    } else
                    { # proceed with the rotation of the cells and save the duel
                        
                        m[1,C] = ifelse((m[1,C]-1)==1, nTeams, (m[1,C]-1))
                        m[2,C] = ifelse((m[2,C]-1)==1, nTeams, (m[2,C]-1))
                        m[1,1] = 1
                        tmp = data.frame(
                             home = m[1,C]
                            ,visitor = m[2,C]
                            ,round = R
                            ,group = "Grupo 1"
                        )
                        if (exists("dftmp_duels")) {dftmp_duels = rbind(dftmp_duels,tmp)} else {dftmp_duels <- tmp}
                    }
                }
            }
            rm(R,C,tmp,m,v,v1,v2)
            dftmp_duels$phase = "1a. Fase"
            
            # Delete the duels regarding the Ghost Team
            if (GhostTeamIndex>0)
            {
                dftmp_duels = dftmp_duels[ (dftmp_duels$home!=GhostTeamIndex) & (dftmp_duels$visitor!=GhostTeamIndex) ,]
                nTeams = nTeams - 1
            }
        
        } else
        {
            # Execute the loop ONLY ONCE to set the duels
            for (C in 1:(nTeams/2))
            { # column index, and no need for rotation
                
                tmp = data.frame(
                     home = m[1,C]
                    ,visitor = m[2,C]
                    ,round = 1
                    ,group = paste("Grupo", C)
                )
                if (exists("dftmp_duels")) {dftmp_duels = rbind(dftmp_duels,tmp)} else {dftmp_duels <- tmp}
                
            }
            rm(C,m,v,v1,v2)
            dftmp_duels$phase = ifelse(nTeams==16, "Oitavas-final"
                                       ,ifelse(nTeams==8, "Quartas-final"
                                               ,ifelse(nTeams==4, "Semi-final", "Final")))
        }
        
        # Create second matches among the teams if applied
        if (nGamesInDuel==2)
        {
            # Invert the order of the games so the last game is played in the house of the best classified team
            tmp <- dftmp_duels
            dftmp_duels = tmp[,c("visitor","home","round","group")]
            colnames(dftmp_duels)[1] = "home"
            colnames(dftmp_duels)[2] = "visitor"
            dftmp_duels$phase = ifelse(isPlayoffs=="no","1a. Fase"
                               ,ifelse(nTeams==16, "Oitavas-final"
                                       ,ifelse(nTeams==8, "Quartas-final"
                                               ,ifelse(nTeams==4, "Semi-final", "Final"))))
            tmp$round = tmp$round + max(tmp$round)
            tmp$phase = ifelse(isPlayoffs=="no","2a. Fase"
                               ,ifelse(nTeams==16, "Oitavas-final"
                                       ,ifelse(nTeams==8, "Quartas-final"
                                               ,ifelse(nTeams==4, "Semi-final", "Final"))))
            dftmp_duels = rbind(dftmp_duels,tmp)
            rm(tmp)
        }
        
        if (isPlayoffs=="no")
        {
            # Sort by random the vector containing the team names
            v_random = sample(seq(1:nTeams), size=nTeams, replace=F)
            names(v_random) = vTeams
            v_random = sort(v_random)
                
        } else
        {
            # Keep original order in the inputed teams' name vector
            v_random = seq(1:nTeams)
            names(v_random) = vTeams
            
        }
        
        # Recover the team names
        colnames(dftmp_duels)[1] = "homeIndex"
        colnames(dftmp_duels)[2] = "visitorIndex"
        dftmp_duels$team_home = with(dftmp_duels, names(v_random[homeIndex]) )
        dftmp_duels$team_visitor = with(dftmp_duels, names(v_random[visitorIndex]) )
        dftmp_duels$round = with(dftmp_duels, ifelse(round<10, paste0("Rodada 0",round), paste("Rodada",round)) )
        dftmp_duels$game = 1:nrow(dftmp_duels)
        dftmp_duels = dftmp_duels[,c("game","phase","group","round","team_home","team_visitor")]
        return(dftmp_duels)
    }
}


# Gauge plot using ggplot2
funcGaugePlot = function(value, breaks=c(0,30,70,100), breaksLabel="", var_fsize=1, var_title="Gauge Plot", breaks_color=c("red","gold","forestgreen"))
{
    
    # Sanity check
    sanity_check = "NOK"
    
    if (is.numeric(value))
    {
        if ( is.numeric(breaks)  &  is.vector(breaks)  &  min(breaks)>=0  &  max(breaks)<=100 )
        {
            if ( between(value, min(breaks), max(breaks)) )
            {
                if (length(breaks)==length(breaks_color))
                {
                    sanity_check = "OK"
                    
                } else
                {
                    stop("The length of 'breaks' must be equal to the length of 'breaks_color'!")
                }
                
            } else
            {
                stop("'value' argument must be inside the range of 'breaks'!")
            }
            
        } else
        {
            stop("'breaks' argument must be a numeric vector between 0 and 100!")
        }
        
    } else
    {
        stop("'value' argument must be numeric!")
    }
    
    if (sanity_check=="OK")
    {
        # Function to get polar measures
        get_polar = function(a, b, r1=0.5, r2=1.0)
        {
            th.start = pi*(1-a/100)
            th.end   = pi*(1-b/100)
            th       = seq(th.start, th.end, length=100)
            x        = c(r1*cos(th), rev(r2*cos(th)))
            y        = c(r1*sin(th), rev(r2*sin(th)))
            return(data.frame(x,y))
        }
        
        # Plot design
        q = ggplot()
        for (k in 1:(length(breaks)-1) )
        {
            q = q + geom_polygon(data=get_polar(breaks[k], breaks[k+1]), aes(x,y), fill=breaks_color[k], alpha=0.7)
        }
        q = q + geom_polygon(data=get_polar(value-1, value+1,0.2), aes(x,y), fill="black")
        q = q + geom_text(data=as.data.frame(breaks)
                ,size=var_fsize*4, fontface="bold", vjust=0
                ,aes( x=1.1*cos(pi*(1-breaks/100)), y=1.1*sin(pi*(1-breaks/100)), label=paste0(breaks,breaksLabel) )
        )
        q = q + annotate("text", x=0, y=0, label=paste0(value,"%"), vjust=0, size=var_fsize*6, fontface="bold")
        q = q + ggtitle(paste0(var_title,"\n"))
        q = q + coord_fixed()
        q = q + theme_bw()
        q = q + theme(
             axis.text=element_blank()
            ,axis.title=element_blank()
            ,axis.ticks=element_blank()
            ,panel.grid=element_blank()
            ,panel.border=element_blank()
            ,plot.title = element_text(hjust=0.5)
            ,title=element_text(size=13*var_fsize)
        )
        return(q)
    }
}

# Gauge Plots
funcGaugePlot(value=10, breaks=c(0,25,50,75,100), breaksLabel="%", var_title="Gauge Plot 1", breaks_color=c("red","gold","blue","forestgreen"))





# Import Libs and set working dir

library(sqldf)
library(xtable)
library(skimr)
library(DataExplorer)
library(ggcorrplot)
library(ggplot2)
library(keras)
library(lime)
library(rsample)
library(recipes)
library(randomForest)
library(ROCR)
library(mlr)
library(effsize)
library(caret)
library(tidyverse)
library(pROC)

#setwd("/Users/rbonifacio/Documents/papers/msr2020/rep_data/")
setwd("/Users/Amaral/tools/MasterResearch/MSR2020/msr2020/rep_data")

#load and filter SZZ results
szz_phase1 <- read.csv(file = "szz_phaseI.csv")
szz_phase2 <- read.csv("raszz_phase2.csv")
co_change <- read.csv("co-change.csv",header = F)
allCommits <- read.csv("all_commits.csv",sep = " ")

allCommits <- allCommits[,c("Project","Commits")]
names(allCommits)[1]<-"Name"

projects <- unique(szz_phase2[3])
names(projects) <- 'Name'
row.names(projects)<-NULL
projects <- as.data.frame(projects[projects$Name!='cassandra',])
names(projects) <- 'Name'
projects <- as.data.frame(projects[projects$Name!='accumulo',])
names(projects) <- 'Name'
projects <- as.data.frame(projects[projects$Name!='spark',])
names(projects) <- 'Name'


levels(szz_phase1$name)[levels(szz_phase1$name)=="amq"] <- "activemq"
levels(szz_phase1$name)[levels(szz_phase1$name)=="ww"] <- "struts"
levels(szz_phase1$name)[levels(szz_phase1$name)=="scb"] <- "servicecomb-java-chassis"
levels(szz_phase1$name)[levels(szz_phase1$name)=="parquet"] <- "parquet-mr"
levels(szz_phase1$name)[levels(szz_phase1$name)=="mng"] <- "maven"
levels(szz_phase1$name)[levels(szz_phase1$name)=="lucene"] <- "lucene-solr"
levels(szz_phase1$name)[levels(szz_phase1$name)=="james"] <- "james-project"
levels(szz_phase1$name)[levels(szz_phase1$name)=="cb"] <- "cordova-android"
levels(szz_phase1$name)[levels(szz_phase1$name)=="lang"] <- "commons-lang"
names(szz_phase1)[4] <- "Name"

# load all merge scenarios 
merge_scenarios_all <- unique(read.csv(file = "combined_merge_commits.csv",stringsAsFactors=FALSE))

merge_scenarios <- unique(merge(merge_scenarios_all,projects))

#nxxl merge scenarios
#nxxl_merge_scenarios <- merge_scenarios[sqrt(merge_scenarios$rightFiles * merge_scenarios$leftFiles)<15.780,]
#merge_scenarios <- merge_scenarios[sqrt(merge_scenarios$rightFiles * merge_scenarios$leftFiles)<15.780,]

#xxl merge scenarios
#xxl_merge_scenarios <- merge_scenarios[sqrt(merge_scenarios$rightFiles * merge_scenarios$leftFiles)>=15.780,]
#merge_scenarios <- merge_scenarios[sqrt(merge_scenarios$rightFiles * merge_scenarios$leftFiles)>=15.780,]

conflicted_merge_scenarios <- merge_scenarios[merge_scenarios$conflict>0,]

#simple conflicting merge scenarios
simple_conflicted_merge_scenarios <- conflicted_merge_scenarios[conflicted_merge_scenarios$conflict<3,]
#conflicted_merge_scenarios <- conflicted_merge_scenarios[conflicted_merge_scenarios$conflict<3,]

#complex conflicting merge scenarios
complex_conflicted_merge_scenarios <- conflicted_merge_scenarios[conflicted_merge_scenarios$conflict>=3,]
#conflicted_merge_scenarios <- conflicted_merge_scenarios[conflicted_merge_scenarios$conflict>=3,]

allCommits <- merge(allCommits,projects)
szz_phase1 <- merge(szz_phase1,projects)

names(szz_phase2)[2]<-'bic'
names(szz_phase2)[3]<-'Name'

szz_phase2_merge <- szz_phase2[,2:3]
names(szz_phase2_merge)[1]<-'Merge'

szz_merge_scenarios <- unique(merge(szz_phase2_merge,conflicted_merge_scenarios))
szz_merge_scenarios['bug'] <- 1

final_ds <- merge(merge_scenarios,szz_merge_scenarios, all = T)
final_ds[is.na(final_ds)] <- 0

final_ds["bic"] <- ifelse(final_ds$bug > 0, T, F)

table(final_ds$bic)

szz_phase2 <- sqldf("select p2.* from szz_phase2 p2 where p2.name in (select f.name from final_ds f)")
unique(szz_phase2$Name)

final_ds["files"]<-final_ds$leftFiles+final_ds$rightFiles
final_ds["authors"]<-final_ds$authorsLeft+final_ds$authorsRight
final_ds["commits"]<-final_ds$nCommitsRight+final_ds$nCommitsLeft
final_ds["days"] <- as.numeric(as.Date(final_ds$a_mergeDate) - as.Date(final_ds$a_baseDate))
final_ds["mg"]<-sqrt(final_ds$leftFiles*final_ds$rightFiles)

#count bic and bfc per project

repsProject <- sqldf("select name as Project, count(*) as Bfc_Bic
              from szz_phase2
              group by Project")

BFC_BIC_PerProject <- sqldf("select name as Name, count(distinct bfc) as BFC, count(distinct bic) as BIC
              from szz_phase2
              group by Name")

mergesPerProject <- sqldf("select Name, count(*) as Merges
              from merge_scenarios
              group by Name")

conflictedPerProject <- sqldf("select Name, count(*) as Conflicted
              from merge_scenarios where (conflict > 0)
              group by Name")

# Simple case 
#Complex case
# conflictedPerProject <- sqldf("select Name, count(*) as Conflicted
#               from merge_scenarios where (conflict > 2)
#               group by Name")


bugsPerProject <- sqldf("select Name, count(*) as Induced_merges
              from szz_merge_scenarios where bug == 1
              group by Name")


#Co-change merge with projects

names(co_change)[1]<-"Name"
names(co_change)[2]<-"cochanges"
summaryProjects <- merge(mergesPerProject,co_change)
summaryProjects <- merge(summaryProjects,BFC_BIC_PerProject)
summaryProjects <- merge(conflictedPerProject,summaryProjects)
summaryProjects <- merge(bugsPerProject,summaryProjects,all = T)
summaryProjects$Induced_merges <-  ifelse(is.na(summaryProjects$Induced_merges), 0, summaryProjects$Induced_merges)

summaryProjects <- merge(allCommits,summaryProjects)

sum(co_change$cochanges)

summaryProjects<-merge(szz_phase1,summaryProjects)

summaryProjects["rate5"] <- summaryProjects$Conflicted/summaryProjects$Commits
summaryProjects["rate2"] <- summaryProjects$BFC/summaryProjects$fixes
summaryProjects["rate3"] <- summaryProjects$Conflicted/summaryProjects$Merges
summaryProjects["rate4"] <- 100.0*summaryProjects$Induced_merges/summaryProjects$Conflicted

##Boxplot ISSUES and Fixes
pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/boxIssuesFixes.pdf",width = 8,height = 5)
boxplot(log(summaryProjects$issues),log(summaryProjects$fixes),
        main = NULL,
        at = c(2,1),
        names = c("Issues", "Fixes"),
        las = 1,
        col = c("orange","brown"),
        border = "black",
        horizontal = TRUE,
        notch = TRUE,xlab='SZZ Phase I: Issues and Fixes (log scale)'
)
dev.off()

pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/boxPerProject.pdf",width = 8,height = 5)
boxplot(log(summaryProjects$Merges),log(summaryProjects$Conflicted),
        main = NULL,
        at = c(1,2),
        names = c("Merges","Conflicted"),
        las = 0,
        col = c("orange","brown"),
        border = "black",
        horizontal = T,
        notch = TRUE,
        xlab="Log-Scale of the number of conflicting merge scenarios and merge scenarios"
)
dev.off()

pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/boxErrors.pdf",width = 8,height = 4)
boxplot(log(bugsPerProject$Induced_merges),
        main = NULL,
        las = 0,
        border = "black",
        horizontal = T,
        xlab="BICs linked to conflicting merge scenarios"
)
dev.off()

pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/histRateBFC.pdf",width = 8,height = 5)
hist((summaryProjects$fixes/summaryProjects$issues),main = "",xlab = "Distribution of BFC over Issues per project",breaks =4,ylim = c(0,20))
dev.off()

pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/histRateConf.pdf",width = 8,height = 5)
hist((x = 100*summaryProjects$rate3),main = "", xlab = "Percentage of conflicting merge scenarios", breaks = 5, ylim = c(0,20))
dev.off()

summaryProjects

summaryProjects <- summaryProjects[order(-summaryProjects$rate4),]
print(xtable(summaryProjects[summaryProjects$rate4 > 10, c("Name","Commits","Merges","Conflicted","Induced_merges","rate4")]),include.rownames = F)

##RQ1 paried tests 

data1 = (summaryProjects$BIC-summaryProjects$Induced_merges)/(summaryProjects$Commits-summaryProjects$Conflicted)
data2 = summaryProjects$Induced_merges/summaryProjects$Conflicted

# #T.test
# shapiro.test(data1)
# shapiro.test(data2)
# tTest <- t.test(x=summaryProjects$BIC/summaryProjects$Commits,y=summaryProjects$Induced_merges/summaryProjects$Conflicted,paired = TRUE)
# print(tTest)

#WilcoxonTest
wilTest <- wilcox.test(x = data1, y = data2, paired = T)
print(wilTest)
#Cliff's Delta
clif <- cliff.delta(d = data1,f = data2,return.dm=TRUE)
print.effsize(clif)

#Data Preparation and Feature Engineering
final_ds <- unique(final_ds)
df <- final_ds[,27:30]
df["conflicts"]<-final_ds$conflict
df["complex"] <- ifelse((sqrt(final_ds$rightFiles*final_ds$leftFiles)) > 15.780, TRUE, FALSE)
df["bic"]<-final_ds$bic
df<-df[df$conflicts>0,]
skim(df)
df$bic <- as.factor(df$bic)
df_complex <- df[df$complex==T,]
df_simple <- df[df$complex==F,]
df$complex <- as.factor(df$complex)

#Split Data test and training
set.seed(100)
train_test_split <- initial_split(df, prop = 0.8)
train_test_split
## All curated conflicted merges
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)


corr <- cor(df[,1:5],method = "spearman")
#corrSimple <- cor(train_tbl[train_tbl$complex==F,1:5],method = "spearman")
#corrComplex <- cor(train_tbl[train_tbl$complex==T,1:5],method = "spearman")

pdf(file = "~/tools/MasterResearch/MSR2020/msr2020/images/corrPlot.pdf",width = 8,height = 4)
ggcorrplot(corr, type = "lower", outline.col = "black",
           lab=TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
dev.off()


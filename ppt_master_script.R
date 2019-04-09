#### PPT MASTER SCRIPT ####
library(RODBC)
library(data.table)


RMSGray <- "#555555"
RMSRed <- "#c41230"
RMSBlue <- "#0086a1"
RMSAqua <- "#6bbbae"
RMSDarkBlue <- "#004f71"
RMSDarkRed <- "#76232f"
RMSOrange <- "#d86018"
RMSGreen <- "#a8ad00"
RMSCyan <- "#2ddbff"
RMSLightGray <- "#95968a"
RMSBlack<- "#000000"
RMScolors <- data.table(RMSDarkRed, RMSDarkBlue, RMSGreen, RMSOrange, RMSAqua, RMSBlue, RMSRed, 
                        RMSCyan, RMSLightGray,RMSGray, RMSBlack)  


#########################################################################################################################################
#### HEADER INFORMATION ####
#########################################################################################################################################

cn <- odbcDriverConnect(connection= paste0("Driver={SQL Server};server=", "CA1MDNAHUDB50", ";trusted_connection=yes;"))


#explimtable <- sqlQuery(cn,paste0("
#                              SELECT * FROM NAWF18_Client_Hartford_Workspace..NAWF18_explimtable"))

#mastertable <- sqlQuery(cn,paste0("
#                              SELECT * FROM NAWF18_Client_Hartford_Workspace..NAWF18_mastertable"))

dom_state <- head(sqlQuery(cn,paste0("
select Admin1Code, sum(TIV_USD) * 100 / sum(sum(TIV_USD)) over() Per_TIV
from NAWF18_Client_Hartford_Workspace..NAWF18_mastertable 
WHERE PORTINFOID = 4 
and Zone1 like '%SmCom%'
group by Admin1Code
ORDER BY sum(TIV_USD) * 100 / sum(sum(TIV_USD)) over() DESC
                                  ")),1)

dom_LOB <- head(sqlQuery(cn,paste0("
select LOB, sum(TIV_USD) * 100 / sum(sum(TIV_USD)) over() Per_TIV
from NAWF18_Client_Hartford_Workspace..NAWF18_mastertable 
WHERE PORTINFOID = 4
and Zone1 like '%SmCom%'
group by LOB
ORDER BY sum(TIV_USD) * 100 / sum(sum(TIV_USD)) over() DESC
                                  ")),1)

num_locs <- nrow(subset(mastertable,PORTINFOID == 4 & grepl("SmCom",Zone1)))

tiv <-  aggregate(subset(mastertable,PORTINFOID == 4 & grepl("SmCom",Zone1))$TIV_USD, FUN = sum, by = list(subset(mastertable,PORTINFOID == 4 & grepl("SmCom",Zone1))$PORTINFOID))$x
explim <-  aggregate(subset(explimtable, LOCID %in% subset(mastertable,PORTINFOID == 4 & grepl("SmCom",Zone1))$LOCID)$LOC_ALLOC_GR, FUN = sum, by = list(subset(mastertable,PORTINFOID == 4 & grepl("SmCom",Zone1))$PORTINFOID))$x
#########################################################################################################################################
#### AAL INFORMATION ####
#########################################################################################################################################

locaal <- fread("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/2815_MR28_hartford_PID4_FRSMKwPLA50k_V3/AAL/LocationAAL.csv")
colnames(locaal)[2] <- "LOCID"
merge <- Reduce(function(x,y) merge(x,y,by="LOCID",all=TRUE) ,list(mastertable,explimtable,locaal))
merge <- merge[merge$PORTINFOID.x == 4 
               & grepl("SmCom",merge$Zone1)
               ,]
#lossexplim <- sum(merge[merge$LOCID %in% locaal$LOCID & merge$PORTINFOID.x == 4,]$LOC_ALLOC_GR)

stateaal <- aggregate(merge[,c("GU AAL","GR AAL")], 
                      FUN = sum, 
                      by = list("State" = merge$Admin1Code),
                      na.rm = T)

port_gu <- sum(stateaal$`GU AAL`)
port_gr <- sum(stateaal$`GR AAL`)


stateaal_5 <- head(stateaal[order(-stateaal$`GR AAL`),],5)
stateaal_5 <- melt(stateaal_5, id.vars = c("State"))
stateaal_5$Per <- 0
stateaal_5[stateaal_5$variable == "GU AAL",]$Per <- stateaal_5[stateaal_5$variable == "GU AAL",]$value/port_gu
stateaal_5[stateaal_5$variable == "GR AAL",]$Per <- stateaal_5[stateaal_5$variable == "GR AAL",]$value/port_gr

#levels(stateaal_5$variable) <- c("GU","GR")


merge$Admin2Name <- gsub(" COUNTY","",merge$Admin2Name)
merge$Admin2Name <- paste0(merge$Admin2Name,"\n",merge$Admin1Code)
countyaal <- aggregate(merge[,c("GR AAL","LOC_ALLOC_GR")], 
                      FUN = sum, 
                      by = list("County" = merge$Admin2Name),
                      na.rm = T)
countyaal_5 <- head(countyaal[order(-countyaal$`GR AAL`),],5)
countyaal_5 <- melt(countyaal_5, id.vars = c("County"))
countyaal_5$Per <- 0

explim <-  aggregate(subset(explimtable, LOCID %in% subset(mastertable,PORTINFOID == 4 
                                                           & grepl("SmCom",Zone1)
                                                           )$LOCID)$LOC_ALLOC_GR, FUN = sum, by = list(subset(mastertable,PORTINFOID == 4 
                                                                                                                                      & grepl("SmCom",Zone1)
                                                                                                              )$PORTINFOID))$x

countyaal_5[countyaal_5$variable == "GR AAL",]$Per <- countyaal_5[countyaal_5$variable == "GR AAL",]$value/port_gr
countyaal_5[countyaal_5$variable == "LOC_ALLOC_GR",]$Per <- countyaal_5[countyaal_5$variable == "LOC_ALLOC_GR",]$value/explim




#levels(stateaal_5$State) <- as.character((stateaal_5[order(-stateaal_5$value),]$State)

stateaal_5$State <- factor(stateaal_5$State, levels = unique(as.character((subset(stateaal_5[order(-stateaal_5$value),], variable == "GR AAL")$State))))
stateaal_5 <- stateaal_5 %>% arrange(State, variable)



# contrib_plot <- ggplot(stateaal_5,aes(State, value/1000000, fill = variable))+
#   geom_col(position = "dodge")+
#   geom_text(aes(State, value/1000000, label = paste0(sprintf("%.0f",stateaal_5$Per*100),"%")),
#             position = position_dodge(width = 1),
#             vjust = -0.5, 
#             size = 6,
#             )+
#   ylab("Loss ($M)")+
#   scale_y_continuous(label = dollar, limits = c(0,(max(stateaal_5$value)/1000000)+1))+
#   scale_x_discrete()+
#   scale_fill_manual(values = c(RMSBlue,RMSRed))+
#   theme_bw()+
#   theme(legend.title=element_blank(),
#         panel.grid.major.x = element_blank(),
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 0, hjust = 1),
#         legend.justification = c(0, 1), legend.position = c(0.7, 0.7))
# 
# png(paste0("PID3_FR_STATE_BAR.png"),width=7.75,height=7.53,res=200, units = "in")
# print(contrib_plot)
# dev.off()

levels(countyaal_5$variable) <- c("GR AAL","EXPLIM")
countyaal_5$variable<- factor(countyaal_5$variable, levels = c("EXPLIM","GR AAL"))
countyaal_5$County <- factor(countyaal_5$County, levels = unique(as.character((countyaal_5[order(-countyaal_5$Per),]$County))))
countyaal_5 <- countyaal_5 %>% arrange(County, Per)

contrib_plot <- ggplot(countyaal_5,aes(County, Per, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(County, Per, label = paste0(sprintf("%.0f",countyaal_5$Per*100),"%")),
            position = position_dodge(width = 1),
            vjust = -0.5, 
            size = 4,
  )+
  ylab("")+
  scale_y_continuous(breaks = NULL, limits = c(0,max(countyaal_5$Per+0.02)))+
  scale_x_discrete()+
  scale_fill_manual(values = c(RMSAqua,RMSRed))+
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.justification = c(0, 1), legend.position = c(0.884, 0.99), legend.text = element_text(size = 8))

png(paste0("PID4SmCom_FRSMK_COUNTY_BAR.png"),width=7.31,height=2.5,res=200, units = "in")
print(contrib_plot)
dev.off()



#########################################################################################################################################
#### EP CREATION ####
#########################################################################################################################################




library(data.table)
library(sqldf)
require(bit64)


HDA_dir <- "//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/"
topdir <- list.files("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults")
topdir <- topdir[grepl("MR28_hartford_PID",topdir)]
topdir <- topdir[grepl("EX",topdir) == F]
topdir <- topdir[grepl("V3",topdir) == T]

source("\\\\ca1ntap01/pat/Tools/Data_Metrics/PLT_Contribution/Contribution_toolbox_final1_v3.R")
for(t in topdir){
  print("working on ")
  print(t)
  cplt <- PLTfunction(paste0(HDA_dir,t),"/PLT/CrestaPLT.csv")
  cplt <- cplt[cplt$LOB != "All" & cplt$Cedant != 'All',]
  #cplt <- cplt[cplt$LOB == "All" & cplt$Cedant == 'All',]
  
  LOBf <- 2
  
  cplt <- cplt[substr(cplt$EXPOSUREID,9,9) == LOBf,]
  
  EPGUAEP <- EPfunction(cplt,"GU")
  EPGUAEP$FP <- "GU"
  EPGUAEP$TYPE <- "AEP"
  colnames(EPGUAEP)[3:4] <- c("LOSS","rpAEP")
  EPGRAEP <- EPfunction(cplt,"GR")
  EPGRAEP$FP <- "GR"
  EPGRAEP$TYPE <- "AEP"
  colnames(EPGRAEP)[3:4] <- c("LOSS","rpAEP")
  EPGUOEP <- EPfunction_OEP(cplt,"GU")
  EPGUOEP$FP <- "GU"
  EPGUOEP$TYPE <- "OEP"
  colnames(EPGUOEP)[3:4] <- c("LOSS","rpAEP")
  EPGROEP <- EPfunction_OEP(cplt,"GR")
  EPGROEP$FP <- "GR"
  EPGROEP$TYPE <- "OEP"
  colnames(EPGROEP)[3:4] <- c("LOSS","rpAEP")
  EP <- rbind(EPGUAEP,EPGRAEP,EPGUOEP,EPGROEP)
  
  EP$Def <- "temp"
  
  EP <- EP[,c("TYPE","FP","LOSS","rpAEP","Def")]
  colnames(EP) <- c("TYPE","FP","TOTLOSS_GU","rpAEP","DEF")
  #EP$TOTLOSS_GU <- EP$TOTLOSS_GU/1000
  #EP$TOTLOSS_GR <- EP$TOTLOSS_GR/1000
  
  #EP$rpAEP <- 1/EP$rpAEP
  #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Prep for Plotting ~~~~~~~~~~~~~~~~~~~~~~~~~ ####
  
  RMSGray <- "#555555"
  RMSRed <- "#c41230"
  RMSBlue <- "#0086a1"
  RMSAqua <- "#6bbbae"
  RMSDarkBlue <- "#004f71"
  RMSDarkRed <- "#76232f"
  RMSOrange <- "#d86018"
  RMSGreen <- "#a8ad00"
  RMSCyan <- "#2ddbff"
  RMSLightGray <- "#95968a"
  RMSBlack<- "#000000"
  RMScolors <- data.table(RMSDarkRed, RMSDarkBlue, RMSGreen, RMSOrange, RMSAqua, RMSBlue, RMSRed, 
                          RMSCyan, RMSLightGray,RMSGray, RMSBlack)  
  
  
  
  
  base_breaks <- function(n = 10){
    function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  
  reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
  }
  
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  
  check=FALSE
  
  if(nchar(round(EP$TOTLOSS_GU[EP$rpAEP==max(EP$rpAEP)]))>4){
    # for bil
    if(nchar(round(EP$TOTLOSS_GU[EP$rpAEP==max(EP$rpAEP)]))>=10 & check == FALSE){
      EP$TOTLOSS_GU <- EP$TOTLOSS_GU/1000000000.0
      #EP$TOTLOSS_GR <- EP$TOTLOSS_GR/1000000000.0
      check <- TRUE
      place <- "Billions"
    }
    # for mil
    if(nchar(round(EP$TOTLOSS_GU[EP$rpAEP==max(EP$rpAEP)]))>=7  & check == FALSE){
      EP$TOTLOSS_GU <- EP$TOTLOSS_GU/1000000.0
      #EP$TOTLOSS_GR <- EP$TOTLOSS_GR/1000000.0
      check <- TRUE
      place <- "Millions"
    }
    # for thousands
    if(nchar(round(EP$TOTLOSS_GU[EP$rpAEP==max(EP$rpAEP)]))>=4  & check == FALSE){
      EP$TOTLOSS_GU <- EP$TOTLOSS_GU/1000.0
      #EP$TOTLOSS_GR <- EP$TOTLOSS_GR/1000.0
      check <- TRUE
      place <- "Thousands"
    }
  }
  
  
  
  
  spline_interp <- data.table(EP$rpAEP,EP$TOTLOSS_GU,EP$FP,EP$TYPE,EP$DEF)
  spline_interp <- setorder(spline_interp,V2)
  
  
  #spline_interp <- melt(spline_interp, id = c("V1","V4","V5"))
  #colnames(spline_interp) <- c("V1","V4","V5","V3","V2")
  #spline_interp[spline_interp$V3 == "V3",]$V3 <- "GR"
  #spline_interp[spline_interp$V3 == "V2",]$V3 <- "GU"
  
  
  bmt <- seq(1,10,1)
  
  contrib_plot <- ggplot(data=subset(spline_interp,V5 == "temp"),aes(x=V1,y=V2))+
    geom_line(data=subset(spline_interp,V5 == "temp"),aes(x=V1,y=V2,color = interaction(V3,V4), linetype = interaction(V3,V4)),size=1)+
    scale_y_continuous(labels = dollar,expand = c(0,0), limits = c(0,max(spline_interp[spline_interp$V3 == "GU" & round(spline_interp$V1 == 5000),]$V2)))+
    coord_flip() +
    scale_color_manual(values = c(RMSRed,RMSBlue,RMSRed,RMSBlue), labels = c("GR AEP","GU AEP", "GR OEP", "GU OEP"))+
    scale_linetype_manual(values = c(1,1,2,2), labels = c("GR AEP","GU AEP", "GR OEP", "GU OEP"))+
    scale_x_continuous(labels = comma, expand = c(0,0), limits = c(5000,1), breaks = c(5,10,25,50,100,250,500,1000,2500,5000),
                       minor_breaks = c(bmt,bmt*10,bmt*100,bmt*1000),
                       trans=reverselog_trans(10))+
    xlab('Return Period (Years)')+
    ylab(paste0('Loss (',place,')'))+
    theme(panel.background=element_rect(fill="white", color="black"),axis.text=element_text(size=12,hjust=1),
          axis.title=element_text( size=12, color="grey", face = 'bold'), 
          panel.grid.major = element_line(color="grey85"), panel.grid.minor = element_line(color="grey85"),
          plot.title=element_text(size=14,hjust=0.5, face = 'bold', color="grey"),
          axis.line = element_line(colour = "grey", size = 0.5))+
    theme(legend.title=element_blank())+
    theme(legend.key=element_blank(),
          legend.justification = c(0, 1), legend.position = c(0.8, 0.9))+
    ggtitle("EP Results")
  contrib_plot
  
  
  
  png(paste0(t,"midmark_ep.png"),width=7.28,height=5.19,res=200, units = "in")
  print(contrib_plot)
  dev.off()
  
  
  
}


#########################################################################################################################################
#### AAL COMPARISON ####
#########################################################################################################################################


locaal <- fread("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/2814_MR28_hartford_PID3_FRSMKwPLA50k_V3/AAL/LocationAAL.csv")
colnames(locaal)[2] <- "LOCID"
merge <- Reduce(function(x,y) merge(x,y,by="LOCID",all=TRUE) ,list(mastertable,explimtable,locaal))
merge <- merge[merge$PORTINFOID.x == 3 
               #& grepl("SmCom",merge$Zone1)
               ,]

stateaal <- aggregate(merge[,c("GU AAL","GR AAL")], 
                      FUN = sum, 
                      by = list("State" = merge$Admin1Code),
                      na.rm = T)

port_gu <- sum(stateaal$`GU AAL`)
port_gr <- sum(stateaal$`GR AAL`)


locaal2 <- fread("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/2816_MR28_hartford_PID3_FRwPLA50k_V3/AAL/LocationAAL.csv")
colnames(locaal2)[2] <- "LOCID"
merge2 <- Reduce(function(x,y) merge(x,y,by="LOCID",all=TRUE) ,list(mastertable,explimtable,locaal2))
merge2 <- merge2[merge2$PORTINFOID.x == 3 
               #& grepl("SmCom",merge2$Zone1)
               ,]

stateaal2 <- aggregate(merge2[,c("GU AAL","GR AAL")], 
                      FUN = sum, 
                      by = list("State" = merge2$Admin1Code),
                      na.rm = T)

port_gu2 <- sum(stateaal2$`GU AAL`)
port_gr2 <- sum(stateaal2$`GR AAL`)

port_change_gu <- port_gu/port_gu2-1
port_change_gr <- port_gr/port_gr2-1


stateaal_5 <- head(stateaal[order(-stateaal$`GR AAL`),],5)
stateaal_5 <- melt(stateaal_5, id.vars = c("State"))
stateaal_52 <- head(stateaal2[order(-stateaal2$`GR AAL`),],5)
stateaal_52 <- melt(stateaal_52, id.vars = c("State"))

state_comb <- merge(stateaal_5,stateaal_52, by = c("State","variable"))
state_comb$Diff <- state_comb$value.x/state_comb$value.y-1


state_comb$State <- factor(state_comb$State, levels = unique(as.character((subset(state_comb[order(-state_comb$value.x),], variable == "GR AAL")$State))))
state_comb <- state_comb %>% arrange(State, variable)


contrib_plot <- ggplot(state_comb,aes(State, Diff, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(State, Diff, label = paste0("+",sprintf("%.0f",state_comb$Diff*100),"%")),
            position = position_dodge(width = 1),
            vjust = -0.5, 
            size = 4,
  )+
  ylab("")+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), limits = c(0,max(state_comb$Diff+0.1)))+
  scale_x_discrete()+
  scale_fill_manual(values = c(RMSBlue,RMSRed))+
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.justification = c(0, 1), legend.position = c(0.7, 0.9))

png(paste0("PID3_DIFF_STATE_BAR.png"),width=7.75,height=7.5,res=200, units = "in")
print(contrib_plot)
dev.off()



#levels(stateaal_5$variable) <- c("GU","GR")
# 
# 
# merge$Admin2Name <- gsub(" COUNTY","",merge$Admin2Name)
# merge$Admin2Name <- paste0(merge$Admin2Name,"\n",merge$Admin1Code)
# countyaal <- aggregate(merge[,c("GR AAL","LOC_ALLOC_GR")], 
#                        FUN = sum, 
#                        by = list("County" = merge$Admin2Name),
#                        na.rm = T)
# countyaal_5 <- head(countyaal[order(-countyaal$`GR AAL`),],5)
# countyaal_5 <- melt(countyaal_5, id.vars = c("County"))
# countyaal_5$Per <- 0
# countyaal_5[countyaal_5$variable == "GR AAL",]$Per <- countyaal_5[countyaal_5$variable == "GR AAL",]$value/port_gr
# countyaal_5[countyaal_5$variable == "LOC_ALLOC_GR",]$Per <- countyaal_5[countyaal_5$variable == "LOC_ALLOC_GR",]$value/lossexplim
# 
# 
# 
# 
# #levels(stateaal_5$State) <- as.character((stateaal_5[order(-stateaal_5$value),]$State)
# 
# stateaal_5$State <- factor(stateaal_5$State, levels = unique(as.character((subset(stateaal_5[order(-stateaal_5$value),], variable == "GR AAL")$State))))
# stateaal_5 <- stateaal_5 %>% arrange(State, variable)
# 
# 
# 
# contrib_plot <- ggplot(stateaal_5,aes(State, value/1000000, fill = variable))+
#   geom_col(position = "dodge")+
#   geom_text(aes(State, value/1000000, label = paste0(sprintf("%.0f",stateaal_5$Per*100),"%")),
#             position = position_dodge(width = 1),
#             vjust = -0.5,
#             size = 6,
#   )+
#   ylab("Loss ($M)")+
#   scale_y_continuous(label = dollar, limits = c(0,(max(stateaal_5$value)/1000000)+1))+
#   scale_x_discrete()+
#   scale_fill_manual(values = c(RMSBlue,RMSRed))+
#   theme_bw()+
#   theme(legend.title=element_blank(),
#         panel.grid.major.x = element_blank(),
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 0, hjust = 1),
#         legend.justification = c(0, 1), legend.position = c(0.7, 0.7))
# 
# png(paste0("PID4midmark_FR_STATE_BAR.png"),width=7.75,height=7.53,res=200, units = "in")
# print(contrib_plot)
# dev.off()
# 
# levels(countyaal_5$variable) <- c("GR AAL","EXPLIM")
# countyaal_5$variable<- factor(countyaal_5$variable, levels = c("EXPLIM","GR AAL"))
# countyaal_5$County <- factor(countyaal_5$County, levels = unique(as.character((countyaal_5[order(-countyaal_5$Per),]$County))))
# countyaal_5 <- countyaal_5 %>% arrange(County, Per)
# 
# contrib_plot <- ggplot(countyaal_5,aes(County, Per, fill = variable))+
#   geom_col(position = "dodge")+
#   geom_text(aes(County, Per, label = paste0(sprintf("%.0f",countyaal_5$Per*100),"%")),
#             position = position_dodge(width = 1),
#             vjust = -0.5, 
#             size = 4,
#   )+
#   ylab("")+
#   scale_y_continuous(breaks = NULL, limits = c(0,max(countyaal_5$Per+0.02)))+
#   scale_x_discrete()+
#   scale_fill_manual(values = c(RMSRed,RMSAqua))+
#   theme_bw()+
#   theme(legend.title=element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.text.x = element_text(angle = 0, hjust = 0.5),
#         legend.justification = c(0, 1), legend.position = c(0.884, 0.99), legend.text = element_text(size = 8))
# 
# png(paste0("PID4midmark_FR_COUNTY_BAR.png"),width=7.31,height=2.5,res=200, units = "in")
# print(contrib_plot)
# dev.off()
# 
# 




#########################################################################################################################################
#### AAL COMPARISON ####
#########################################################################################################################################


locaal <- fread("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/2814_MR28_hartford_PID3_FRSMKwPLA50k_V3/AAL/LocationAAL.csv")
colnames(locaal)[2] <- "LOCID"
merge <- Reduce(function(x,y) merge(x,y,by="LOCID",all=TRUE) ,list(mastertable,explimtable,locaal))
merge <- merge[merge$PORTINFOID.x == 3 
              # & grepl("SmCom",merge$Zone1)
               ,]


merge$Admin2Name <- gsub(" COUNTY","",merge$Admin2Name)
merge$Admin2Name <- paste0(merge$Admin2Name,"\n",merge$Admin1Code)
countyaal <- aggregate(merge[,c("GR AAL")], 
                       FUN = sum, 
                       by = list("County" = merge$Admin2Name),
                       na.rm = T)



#port_gu <- sum(stateaal$`GU AAL`)
#port_gr <- sum(stateaal$`GR AAL`)


locaal2 <- fread("//EUN-MD-HDA7DB02/f$/HDA_Results/DownloadResults/2816_MR28_hartford_PID3_FRwPLA50k_V3/AAL/LocationAAL.csv")
colnames(locaal2)[2] <- "LOCID"
merge2 <- Reduce(function(x,y) merge(x,y,by="LOCID",all=TRUE) ,list(mastertable,explimtable,locaal2))
merge2 <- merge2[merge2$PORTINFOID.x == 3 
                # & grepl("SmCom",merge2$Zone1)
                 ,]

merge2$Admin2Name <- gsub(" COUNTY","",merge2$Admin2Name)
merge2$Admin2Name <- paste0(merge2$Admin2Name,"\n",merge2$Admin1Code)
countyaal2 <- aggregate(merge2[,c("GR AAL")], 
                       FUN = sum, 
                       by = list("County" = merge2$Admin2Name),
                       na.rm = T)

colnames(countyaal2)[2] <- "GR AAL"
colnames(countyaal)[2] <- "GR AAL"

#port_gu2 <- sum(stateaal2$`GU AAL`)
#port_gr2 <- sum(stateaal2$`GR AAL`)

#port_change_gu <- port_gu/port_gu2-1
#port_change_gr <- port_gr/port_gr2-1


countyaal_5 <- head(countyaal[order(-countyaal$`GR AAL`),],5)
countyaal_5 <- melt(countyaal_5, id.vars = c("County"))
countyaal_52 <- head(countyaal2[order(-countyaal2$`GR AAL`),],5)
countyaal_52 <- melt(countyaal_52, id.vars = c("County"))

state_comb <- merge(countyaal_5,countyaal_52, by = c("County"))
state_comb$Diff <- state_comb$value.x/state_comb$value.y-1


state_comb$County <- factor(state_comb$County, levels = unique(as.character((subset(state_comb[order(-state_comb$value.x),], variable.x == "GR AAL")$County))))
state_comb <- state_comb %>% arrange(County, variable.x)

state_comb$variable.x <- "FRSMK"
state_comb <- state_comb[,c(1:3,6)]

countyaal_52$Diff <- NA
countyaal_52$variable <- "FR"
colnames(countyaal_52) <- colnames(state_comb)
state_comb <- rbind(state_comb,countyaal_52)

#state_comb[state_comb$variable.x == "FR",]$Diff <- 0
contrib_plot <- ggplot(state_comb,aes(County, value.x/1000000, fill = variable.x))+
  geom_col(position = "dodge")+
  geom_text(aes(
    label = ifelse(is.na(state_comb$Diff),"",
      paste0("+",sprintf("%.0f",state_comb$Diff*100),"%"))),
            position = position_dodge(width = 1),
            vjust = -0.5, 
            size = 4,
  )+
  ylab("")+
  scale_y_continuous(label = dollar, limits = c(0,max(state_comb$value.x/1000000+1.5)))+
  scale_x_discrete()+
  ylab("GR AAL ($M)")+
  scale_fill_manual(values = c(RMSBlue,RMSRed))+
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.justification = c(0, 1), legend.position = c(0.7, 0.9), legend.text = element_text(size = 8))
  # theme(legend.title=element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       text = element_text(size = 20),
  #       axis.text.x = element_text(angle = 0, hjust = 1),
  #       legend.justification = c(0, 1), legend.position = c(0.7, 0.9))

png(paste0("PID3_DIFF_COUNTY_BAR.png"),width=7.31,height=2.5,res=200, units = "in")
print(contrib_plot)
dev.off()




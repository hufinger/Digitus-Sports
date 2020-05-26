library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

nfl14 = read.csv('NFL-2014-Win-Predictions.csv')
nfl15 = read.csv('NFL-2015-Win-Predictions.csv')
nfl16 = read.csv('NFL-2016-Win-Predictions.csv')
nfl17 = read.csv('NFL-2017-Win-Predictions.csv')
nfl18 = read.csv('NFL-2018-Win-Predictions.csv')
nfl19 = read.csv('NFL-2019-Win-Predictions.csv')

hhf_theme = function(){
  #Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

ggplot(nfl14)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=1.3053, intercept = -2.6983, col = 'red') +scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2014") +
  hhf_theme()
summary(lm(ActualWins ~ Win.Total, data = nfl14))

ggplot(nfl15)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=.8469, intercept = 1.0526, col = 'red')+scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2015") +
  hhf_theme()
summary(lm(ActualWins ~ Win.Total, data = nfl15))

ggplot(nfl16)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=1.0603, intercept = -.7269, col = 'red')+scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2016") +
  hhf_theme()
summary(lm(ActualWins ~ Win.Total, data = nfl16))

ggplot(nfl17)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=.7783, intercept = 1.7010, col = 'red')+scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2017") +
  hhf_theme()
summary(lm(ActualWins ~ Win.Total, data = nfl17))

ggplot(nfl18)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=1.0023, intercept = -.2222, col = 'red')+scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2018") +
  hhf_theme()
summary(lm(ActualWins ~ Win.Total, data = nfl18))

ggplot(nfl19)+
  geom_point(aes(x = Win.Total, y = ActualWins))+
  geom_abline(slope=1.1659, intercept = -1.4861, col = 'red')+
  scale_y_continuous(name = "Actual Wins", limits = c(0,16))+
  scale_x_continuous(name="Vegas Predictions", limits = c(0,16))+
  labs(title = "Actual Wins vs. Vegas Predictions for NFL 2019") +
  hhf_theme() + geom_text()
summary(lm(ActualWins ~ Win.Total, data = nfl19))

#Finding Mean Absolute Error of each year

mae = function(data, year){
  data$MAE = mean(abs(data$ActualWins - data$Win.Total))
  data$Season = year
  return(data)
}


nfl19 = mae(nfl19, 2019)
nfl18 = mae(nfl18, 2018)
nfl17 = mae(nfl17, 2017)
nfl16 = mae(nfl16, 2016)
nfl15 = mae(nfl15, 2015)
nfl14 = mae(nfl14, 2014)

fulldata = rbind(nfl14, nfl15, nfl16, nfl17, nfl18, nfl19) %>% select(-X)
ggplot(fulldata)+geom_point(aes(x = Win.Total, y = ActualWins))+geom_abline(slope=1.0156, intercept = -.3068, col = 'red')+scale_y_continuous(name = "Actual Wins", limits = c(0,16))+scale_x_continuous(name="Vegas Predictions", limits=c(0,16))
summary(lm(ActualWins ~ Win.Total, data = fulldata))


split_list = split(fulldata, fulldata$Team)

ARI = split_list[[1]]
ATL = split_list[[2]]
BAL = split_list[[3]]
BUF = split_list[[4]]
CAR = split_list[[5]]
CHI = split_list[[6]]
CIN = split_list[[7]]
CLV = split_list[[8]]
DAL = split_list[[9]]
DEN = split_list[[10]]
DET = split_list[[11]]
GB = split_list[[12]]
HOU = split_list[[13]]
IND = split_list[[14]]
JAC = split_list[[15]]
KC = split_list[[16]]
MIA = split_list[[17]]
MIN = split_list[[18]]
NE = split_list[[19]]
NO = split_list[[20]]
NYG = split_list[[21]]
NYJ = split_list[[22]]
OAK = split_list[[23]]
PHI = split_list[[24]]
PIT = split_list[[25]]
SD = split_list[[26]]
SF = split_list[[27]]
SEA = split_list[[28]]
STL = split_list[[29]]
TB = split_list[[30]]
TEN = split_list[[31]]
WAS = split_list[[32]]
LAR = split_list[[33]]
LAC = split_list[[34]]

LAR = rbind(STL, LAR)
LAC = rbind(SD, LAC)

teamMAE = function(data){
  data$team_mae = mean(abs(data$ActualWins - data$Win.Total))
  data$lagWins = lag(data$ActualWins, 1)
  return(data)
}
ARI = teamMAE(ARI) %>% rename(season_mae = MAE)
ATL = teamMAE(ATL) %>% rename(season_mae = MAE)
BAL = teamMAE(BAL) %>% rename(season_mae = MAE)
BUF = teamMAE(BUF) %>% rename(season_mae = MAE)
CAR = teamMAE(CAR) %>% rename(season_mae = MAE)
CHI = teamMAE(CHI) %>% rename(season_mae = MAE)
CIN = teamMAE(CIN) %>% rename(season_mae = MAE)
CLV = teamMAE(CLV) %>% rename(season_mae = MAE)
DAL = teamMAE(DAL) %>% rename(season_mae = MAE)
DEN = teamMAE(DEN) %>% rename(season_mae = MAE)
DET = teamMAE(DET) %>% rename(season_mae = MAE)
GB = teamMAE(GB) %>% rename(season_mae = MAE)
HOU = teamMAE(HOU) %>% rename(season_mae = MAE)
IND = teamMAE(IND) %>% rename(season_mae = MAE)
JAC = teamMAE(JAC) %>% rename(season_mae = MAE)
KC = teamMAE(KC) %>% rename(season_mae = MAE)
MIA = teamMAE(MIA) %>% rename(season_mae = MAE)
MIN = teamMAE(MIN) %>% rename(season_mae = MAE)
NE = teamMAE(NE) %>% rename(season_mae = MAE)
NO = teamMAE(NO) %>% rename(season_mae = MAE)
NYG = teamMAE(NYG) %>% rename(season_mae = MAE)
NYJ = teamMAE(NYJ) %>% rename(season_mae = MAE)
OAK = teamMAE(OAK) %>% rename(season_mae = MAE)
PHI = teamMAE(PHI) %>% rename(season_mae = MAE)
PIT = teamMAE(PIT) %>% rename(season_mae = MAE)
SF = teamMAE(SF) %>% rename(season_mae = MAE)
SEA = teamMAE(SEA) %>% rename(season_mae = MAE)
TB = teamMAE(TB) %>% rename(season_mae = MAE)
TEN = teamMAE(TEN) %>% rename(season_mae = MAE)
WAS = teamMAE(WAS) %>% rename(season_mae = MAE)
LAR = teamMAE(LAR) %>% rename(season_mae = MAE)
LAC = teamMAE(LAC) %>% rename(season_mae = MAE)

rejoin = rbind(ARI,
               ATL,
               BAL,
               BUF,
               CAR,
               CHI,
               CIN,
               CLV,
               DAL,
               DEN,
               DET,
               GB,
               HOU,
               IND,
               JAC,
               KC,
               MIA,
               MIN,
               NE,
               NO,
               NYG,
               NYJ,
               OAK,
               PHI,
               PIT,
               SF,
               SEA,
               TB,
               TEN,
               WAS,
               LAR,
               LAC)

single = filter(rejoin, Season == 2019) %>% select(Team, team_mae) %>% arrange(team_mae)
final = rbind(single[1:5,], single[28:32,])



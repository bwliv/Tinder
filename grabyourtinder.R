#import necessary functions
library(gridExtra)
library(GGally)
library(tidyverse)
library(parcoords)
library(lubridate)
library(rjson)
library(lubridate)

#### STEP 1: Download Tinder data from https://www.help.tinder.com/hc/en-us/articles/115005626726-How-do-I-request-a-copy-of-my-personal-data-
#### STEP 2: Wait a few days until you receive data
#### STEP 3: Download data when available and copy data.JSON from zipped folder to current working directory
#### STEP 4: Run this code
#### STEP 5: View graphs of your Tinder stats in R, 

#### NOTE: Code in line 219 will remove dates with missing data (i.e. if there was a period where Tinder did not track your app opens / messages) - if this causes errors, simply remove this block

#read file from JSON
result = fromJSON(file="data.json")$Usage

#start dataframe with date column
tinder = data.frame(ymd(names(result$app_opens)))
colnames(tinder) = 'date'

#add opens
nums = vector()
names = vector()
for (i in result$app_opens) {
  nums = c(nums,i)
}
for (i in names(result$app_opens)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','opens')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#add messages received
nums = vector()
names = vector()
for (i in result$messages_received) {
  nums = c(nums,i)
}
for (i in names(result$messages_received)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','messages_received')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#add messages sent
nums = vector()
names = vector()

for (i in result$messages_sent) {
  nums = c(nums,i)
}
for (i in names(result$messages_sent)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','messages_sent')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#add likes
nums = vector()
names = vector()
for (i in result$swipes_likes) {
  nums = c(nums,i)
}
for (i in names(result$swipes_likes)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','likes')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#add passes
nums = vector()
names = vector()
for (i in result$swipes_passes) {
  nums = c(nums,i)
}
for (i in names(result$swipes_passes)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','passes')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#add matches
nums = vector()
names = vector()
for (i in result$matches) {
  nums = c(nums,i)
}
for (i in names(result$matches)) {
  names = c(names,i)
}
dnums = data.frame(nums)
dnames = data.frame(names)
output = cbind(dnames,dnums)
colnames(output) = c('date','matches')
output[,1] = ymd(output[,1])
tinder = merge(tinder,output,by='date')

#set color scheme
tinder_pink = '#FE3C72'
tinder_shade = '#FFE2E0'
tinder_theme = function() {
  theme(
    panel.background = element_rect(fill=tinder_shade),
    panel.grid.major = element_line(color='white'),
    panel.grid.minor = element_line(color='white'),
    panel.border = element_rect(linetype = 'twodash',color=tinder_pink,fill=NA),
    strip.text = element_text(color='white',face='bold'),
    strip.background = element_rect(fill=tinder_pink),
    plot.title = element_text(hjust = 0.5)
  )
}

#write to csv
mytinder = tinder
write.csv(mytinder,"mytinder.csv")

#add seperator line
sp = paste(rep('-',500),collapse='')

#Print message totals
print(sp)
print(paste0('Total messages sent: ',sum(mytinder$messages_sent)))
print(paste0('Total messages received: ',sum(mytinder$messages_received)))
print(sp)

#Create new messages data frame and combine messages in main data frame
messages = mytinder %>% select(date,messages_sent,messages_received) %>% mutate(message_differential = messages_received - messages_sent)
mytinder = mytinder %>% mutate(messages = messages_sent + messages_received) %>% select(-c(messages_sent,messages_received))

#Combine likes and passes
mytinder = mytinder %>% mutate(swipes=likes+passes)

#Calculate sums
print(sp)
print('All time totals:')
sapply(mytinder[-1],sum)
print(sp)

#Calculate maximums
print(sp)
print('All time daily maximums:')
sapply(mytinder[-1],max)
print(sp)

#Find days where maximums occured
print(sp)
print('Days where maximums occured:')
mytinder %>% filter(opens==max(mytinder$opens)|likes==max(mytinder$likes)|passes==max(mytinder$passes)|matches==max(mytinder$matches)|messages==max(mytinder$messages)|swipes==max(mytinder$swipes)) %>% mutate(day = wday(date,label = T))
print(sp)

#Calculate swipe right percentage
print(sp)
print('Swipe right percentage:')
100 * (sum(mytinder$likes) / (sum(mytinder$likes) + sum(mytinder$passes)))
print(sp)

#Calculate match-to-swipe-right ratio:
print(sp)
print('Match percentage:')
100 * sum(mytinder$matches) / sum(mytinder$likes)
print(sp)

#Calculate swipes per match:
print(sp)
print('Percent of swipes converted to matches:')
100 * sum(mytinder$swipes) / sum(mytinder$matches)
print(sp)

#Apply log transformation to form new "match rate" statistic which is higher the more you convert your swipes right to matches
mytinder = mytinder %>% mutate(swipe_right_rate = (likes / (likes+passes))) %>% mutate(match_rate = log( ifelse(matches==0,1,matches) / ifelse(likes==0,1,likes)))

#Put rates in own data frame
rates = mytinder %>% select(date,swipe_right_rate,match_rate) 

#Graph swipe rates over time
match_rate_plot = ggplot(rates) +
  geom_point(size=0.2,alpha=0.5,aes(date,match_rate)) +
  geom_smooth(aes(date,match_rate),color=tinder_pink,size=2,se=FALSE) +
  tinder_theme() +
  coord_cartesian(ylim = c(fivenum(rates$match_rate)[2],fivenum(rates$match_rate)[4])) +
  ggtitle('Match Rate Over Time') +
  ylab('')
swipe_rate_plot = ggplot(rates) +
  geom_point(aes(date,swipe_right_rate),size=0.2,alpha=0.5) +
  geom_smooth(aes(date,swipe_right_rate),color=tinder_pink,size=2,se=FALSE) +
  tinder_theme() +
  coord_cartesian(ylim = c(fivenum(rates$swipe_right_rate)[2],fivenum(rates$swipe_right_rate)[4])) +
  ggtitle('Swipe Right Rate Over Time') +
  ylab('') 
grid.arrange(match_rate_plot,swipe_rate_plot,nrow=2)

#Drop columns
mytinder = mytinder %>% select(-c(likes,passes,swipe_right_rate,match_rate))

#Drop rows with missing values for opens and messages - REMOVE IF CAUSES ISSUES
for (i in 1:10000){
  if (mytinder[1,'messages'] == 0 | mytinder[1,'opens'] == 0){
    mytinder = mytinder[-1,]
    messages = messages[-1,]
  } else {
    break
  }
}

#Calculate all-time daily averages:
print(sp)
print('Daily averages:')
sapply(mytinder[-c(1,7,8)],mean)
print(sp)

#Print scatterplot matrix of Tinder stats
ggduo(mytinder[2:5], types=list(continuous = wrap("smooth_loess", alpha = 0.4,size=0.2))) + tinder_theme()

#Print box plots of daily usage stats
tidytinder = mytinder %>% gather(key = 'var',value = 'value',-date)
ggplot(tidytinder,aes(y=value)) +
  coord_flip() +
  geom_boxplot() +
  facet_wrap(~var,scales = 'free',nrow=5) +
  tinder_theme() +
  xlab("") +
  ylab("") +
  ggtitle('Daily Tinder Stats') +
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) 

#Graph message differential
ggplot(messages) +
  geom_point(aes(date,message_differential),size=0.2,alpha=0.5) +
  geom_smooth(aes(date,message_differential),color=tinder_pink,size=2,se=FALSE) +
  tinder_theme() +
  ylab('Msg Received - Msg Sent In Day') +
  xlab('Date') +
  ggtitle('Message Differential Over Time') +
  coord_cartesian(ylim=c(messages$message_differential[2],messages$message_differential[4])) 

#Graph messages received vs. sent
tidy_messages = messages %>% select(-message_differential) %>% gather(key = 'key',value = 'value',-date)
ggplot(tidy_messages) +
  geom_smooth(aes(date,value,color=key),size=2,se=FALSE) +
  tinder_theme() +
  ylab('Msg Received & Msg Sent In Day') +
  xlab('Date') +
  ggtitle('Message Differential Over Time')

#Graph stats over time
ggplot(tidytinder,aes(x=date,y=value)) +
  geom_point(size=0.5,alpha=0.3) +
  geom_smooth(color=tinder_pink) +
  facet_wrap(~var,scales = 'free') +
  tinder_theme() + 
  ggtitle('Daily Tinder Stats Over Time')
mat = ggplot(mytinder) +
  geom_point(aes(x=date,y=matches),size=0.5,alpha=0.3) +
  geom_smooth(aes(x=date,y=matches),color=tinder_pink,se=FALSE,size=2) +
  tinder_theme() +
  coord_cartesian(ylim=c(fivenum(mytinder$matches)[2],1.5*fivenum(mytinder$matches)[4])) +
  ylab('Matches') +
  xlab('Date') + 
  ggtitle('Matches Over Time')
mes = ggplot(mytinder) +
  geom_point(aes(x=date,y=messages),size=0.5,alpha=0.3) +
  geom_smooth(aes(x=date,y=messages),color=tinder_pink,se=FALSE,size=2) +
  tinder_theme() +
  coord_cartesian(ylim=c(fivenum(mytinder$messages)[2],1.5*fivenum(mytinder$messages)[4])) +
  ylab('Messages') +
  xlab('Date') + 
  ggtitle('Messages Over Time')
grid.arrange(mat,mes,nrow=2)
opns = ggplot(mytinder) +
  geom_point(aes(x=date,y=opens),size=0.5,alpha=0.3) +
  geom_smooth(aes(x=date,y=opens),color=tinder_pink,se=FALSE,size=2) +
  tinder_theme() +
  coord_cartesian(ylim=c(fivenum(mytinder$opens)[2],1.5*fivenum(mytinder$opens)[4])) +
  ylab('App Opens') +
  xlab('Date') + 
  ggtitle('Tinder Opens Over Time')
swps = ggplot(mytinder) +
  geom_point(aes(x=date,y=swipes),size=0.5,alpha=0.3) +
  geom_smooth(aes(x=date,y=swipes),color=tinder_pink,se=FALSE,size=2) +
  tinder_theme() +
  coord_cartesian(ylim=c((fivenum(mytinder$swipes)[2]),1.5*fivenum(mytinder$swipes)[4])) +
  ylab('Swipes') +
  xlab('Date') + 
  ggtitle('Swipes Over Time')
grid.arrange(opns,swps,nrow=2)

#Organize by month
by_months = mytinder %>% group_by(month(date)) %>% summarize(messages=mean(messages),matches=mean(matches),opens=mean(opens),swipes=mean(swipes)) %>% rename(month = 'month(date)')
by_months$month = factor(c('Ja','Fe','Mr','Ap','My','Jn','Jl','Au','Sp','Oc','Nv','Dc'),levels=c('Ja','Fe','Mr','Ap','My','Jn','Jl','Au','Sp','Oc','Nv','Dc'))
by_month = by_months %>% gather(key='var',value='value',-month)
ggplot(by_month) +
  geom_col(aes(x=month,y=value),fill=tinder_pink,color='black') +
  tinder_theme() +
  facet_wrap(~var,scales='free') +
  ggtitle('Tinder Stats By Month') +
  xlab("") +
  ylab("")

rates_by_months =  rates %>% group_by(month(date)) %>% rename(month = 'month(date)') %>% summarize(swipe_right_rate=mean(swipe_right_rate,na.rm=T),match_rate=mean(match_rate))
rates_by_months$month = factor(c('Ja','Fe','Mr','Ap','My','Jn','Jl','Au','Sp','Oc','Nv','Dc'),levels=c('Ja','Fe','Mr','Ap','My','Jn','Jl','Au','Sp','Oc','Nv','Dc'))
rates_by_month = rates_by_months %>% gather(key='var',value='value',-month)
ggplot(rates_by_month) +
  geom_col(aes(x=month,y=value),fill=tinder_pink,color='black') +
  tinder_theme() +
  facet_wrap(~var,scales='free') +
  ggtitle('Tinder Connection Rates By Month') +
  xlab("") +
  ylab("")

#Organize by day
by_day = mytinder %>% group_by(wday(date,label=TRUE)) %>% summarize(messages=mean(messages),matches=mean(matches),opens=mean(opens),swipes=mean(swipes)) 
colnames(by_day)[1] = 'day'
mutate(by_day,day = substr(day,1,2))
by_days = by_day %>% gather(key='var',value='value',-day)
ggplot(by_days) +
  geom_col(aes(x=fct_relevel(day,'Sat'),y=value),fill=tinder_pink,color='black') +
  tinder_theme() +
  facet_wrap(~var,scales='free') +
  ggtitle('Tinder Stats By Day of Week') +
  xlab("") +
  ylab("")
rates_by_day = rates %>% group_by(wday(date,label=TRUE)) %>% summarize(swipe_right_rate=mean(swipe_right_rate,na.rm=T),match_rate=mean(match_rate,na.rm=T))
colnames(rates_by_day)[1] = 'day'
mutate(rates_by_day,day = substr(day,1,2))
rates_by_days = rates_by_day %>% gather(key='var',value='value',-day)
ggplot(rates_by_days) +
  geom_col(aes(x=fct_relevel(day,'Sat'),y=value),fill=tinder_pink,color='black') +
  tinder_theme() +
  facet_wrap(~var,scales='free') +
  ggtitle('Tinder Stats By Day of Week') +
  xlab("") +
  ylab("")

#Parallel coordinate plot
nodates = select(mytinder,-date)
parcoords(nodates,
          rownames = F,
          brushMode = "1D-axes",
          alpha = .4,
          reorderable = T,
          queue = T,
          color = tinder_pink)

print(sp)
print('Analysis complete, view graphs and output')
print('Personal statistics will be sandwiched by dashed lines')
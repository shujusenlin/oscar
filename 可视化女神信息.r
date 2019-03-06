library(readxl)
library(ggplot2)
library(ggimage)
library(ggthemr)
library(ggthemes)
library(Cairo)
library(plyr)

## 读取女神信息
setwd('D:/爬虫/奥斯卡影后')
data = read_excel('影后信息整理.xlsx')

data$age = as.numeric(data$year) - as.numeric(data$born)
data$height = as.numeric(substr(data$height,1,3))
data$is_winner = as.numeric(data$is_winner)
data$winner_height = data$is_winner*data$height
data$winner_age = data$is_winner*data$age
data$score = as.numeric(data$score)
data$count = as.numeric(data$count)

## 历练入围名单、女神信息汇总
reward = ddply(data,.(year),summarise,
               age=sum(age[!is.na(age)])/length(age[!is.na(age)]),
               height=sum(height[!is.na(height)])/length(height[!is.na(height)]),
               winner_age=sum(winner_age[!is.na(winner_age)])/
                 length(winner_age[!is.na(winner_age)&winner_age>0]),
               winner_height=sum(winner_height[!is.na(winner_height)])/
                 length(winner_height[!is.na(winner_height)&winner_height>0]))

actress = ddply(data,.(name,img,profile),summarise,
                total_nominate = length(year),
                total_award=sum(is_winner),
                max_age=max(age),
                min_age=min(age),
                score=round(mean(score)+mean(count)/10000,2),
                count=mean(count))

## 最受欢迎
actress$label = paste(actress$name,'欢迎度:',actress$score)
actress_show = subset(actress,count>120)
ggthemr('pale')
p<-ggplot(actress_show[order(actress_show$score,decreasing = T),][1:10,],
          aes(x=reorder(name,score),y=score,fill=rep(10:1,each=1)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=name,y=30,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('最受欢迎的奥斯卡女神TOP10') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,100)
print(p)

ggsave("最受欢迎top10.png", p, width = 10, height = 12) 


## 最受关注
actress$label = paste(actress$name,'投票人数:',actress$count)
actress_show = subset(actress,count>120)
ggthemr('pale')
p<-ggplot(actress_show[order(actress_show$count,decreasing = T),][1:10,],
          aes(x=reorder(name,count),y=count,fill=rep(10:1,each=1)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=name,y=5000,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('最受关注的奥斯卡女神TOP10') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,15000)
print(p)

ggsave("最受关注top10.png", p, width = 10, height = 12) 




## 年龄/身高趋势

k <- lm(age~year,data = reward)
reward$smooth_age <- predict(k,year = reward$year)
ggplot(reward,aes(x=year))+geom_line(aes(y=age),size=1.5)+
  theme_bw()+
  ggtitle('各年度入围者平均年龄')+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title  = element_text(size=25)
  )+geom_line(aes(y=smooth_age),col='darkblue',size=1.5)

k <- lm(height~poly(year,2),data = reward)
reward$smooth_height <- predict(k,year = reward$year)
ggplot(reward,aes(x=year))+geom_line(aes(y=height),size=1.5)+
  geom_line(aes(y=smooth_height),col='darkblue',size=1.5)+
  theme_bw()+
  ggtitle('各年度入围者平均身高(cm)')+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title  = element_text(size=25)
  )


## 最年长入围
data$age = as.numeric(data$year)-as.numeric(data$born)
data$label = paste(data$name,'年份:',data$year,'入围时年龄：',data$age)
data$is_winner = as.factor(data$is_winner)
data_show = subset(data,age>0&age<100)
ggthemr('dust')
p<-ggplot(data[order(data$age,decreasing = T),][1:10,],
          aes(x=reorder(label,age),y=age,fill=is_winner))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=label,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=label,y=40,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('入围年龄最年长TOP10') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,100)
print(p)

ggsave("最年长top10.png", p, width = 10, height = 12) 

## 最年少入围
data$age = as.numeric(data$year)-as.numeric(data$born)
data$label = paste(data$name,'年份:',data$year,'入围时年龄：',data$age)
data$is_winner = as.factor(data$is_winner)
data_show = subset(data,age>0&age<100)
ggthemr('dust')
p<-ggplot(data[order(data$age,decreasing = F),][1:10,],
          aes(x=reorder(label,-age),y=age,fill=is_winner))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=label,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=label,y=10,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('入围年龄最年轻TOP10') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,25)
print(p)

ggsave("最年轻top10.png", p, width = 10, height = 12) 


## 提名最多
actress$label = paste(actress$name,'提名次数:',actress$total_nominate,'获奖次数:',actress$total_award)
ggthemr('pale')
p<-ggplot(actress[order(actress$total_nominate,decreasing = T),][1:15,],
          aes(x=reorder(name,total_nominate),y=total_nominate,fill=rep(5:1,each=3)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.05)+
  geom_text(aes(x=name,y=5.5,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('提名次数最多TOP15') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,18)
print(p)

ggsave("提名次数最多top15.png", p, width = 12, height = 15) 

## 获奖最多
actress$label = paste(actress$name,'获奖次数:',actress$total_award,'提名次数:',actress$total_nominate)
ggthemr('pale')
p<-ggplot(actress[order(actress$total_award,decreasing = T),][1:15,],
          aes(x=reorder(name,total_award),y=total_award,fill=rep(5:1,each=3)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.05)+
  geom_text(aes(x=name,y=1.5,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('获奖次数最多TOP15') + theme_wsj()+                 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
  )+coord_flip()+ylim(0,4)
print(p)

ggsave("获奖次数最多top15.png", p, width = 12, height = 15) 


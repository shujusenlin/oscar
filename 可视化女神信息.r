library(readxl)
library(ggplot2)
library(ggimage)
library(ggthemr)
library(ggthemes)
library(Cairo)
library(plyr)

## ��ȡŮ����Ϣ
setwd('D:/����/��˹��Ӱ��')
data = read_excel('Ӱ����Ϣ����.xlsx')

data$age = as.numeric(data$year) - as.numeric(data$born)
data$height = as.numeric(substr(data$height,1,3))
data$is_winner = as.numeric(data$is_winner)
data$winner_height = data$is_winner*data$height
data$winner_age = data$is_winner*data$age
data$score = as.numeric(data$score)
data$count = as.numeric(data$count)

## ������Χ������Ů����Ϣ����
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

## ���ܻ�ӭ
actress$label = paste(actress$name,'��ӭ��:',actress$score)
actress_show = subset(actress,count>120)
ggthemr('pale')
p<-ggplot(actress_show[order(actress_show$score,decreasing = T),][1:10,],
          aes(x=reorder(name,score),y=score,fill=rep(10:1,each=1)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=name,y=30,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('���ܻ�ӭ�İ�˹��Ů��TOP10') + theme_wsj()+                 
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

ggsave("���ܻ�ӭtop10.png", p, width = 10, height = 12) 


## ���ܹ�ע
actress$label = paste(actress$name,'ͶƱ����:',actress$count)
actress_show = subset(actress,count>120)
ggthemr('pale')
p<-ggplot(actress_show[order(actress_show$count,decreasing = T),][1:10,],
          aes(x=reorder(name,count),y=count,fill=rep(10:1,each=1)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=name,y=5000,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('���ܹ�ע�İ�˹��Ů��TOP10') + theme_wsj()+                 
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

ggsave("���ܹ�עtop10.png", p, width = 10, height = 12) 




## ����/�������

k <- lm(age~year,data = reward)
reward$smooth_age <- predict(k,year = reward$year)
ggplot(reward,aes(x=year))+geom_line(aes(y=age),size=1.5)+
  theme_bw()+
  ggtitle('�������Χ��ƽ������')+
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
  ggtitle('�������Χ��ƽ�����(cm)')+
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(hjust=0.5,size=35,face='bold'),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title  = element_text(size=25)
  )


## ���곤��Χ
data$age = as.numeric(data$year)-as.numeric(data$born)
data$label = paste(data$name,'���:',data$year,'��Χʱ���䣺',data$age)
data$is_winner = as.factor(data$is_winner)
data_show = subset(data,age>0&age<100)
ggthemr('dust')
p<-ggplot(data[order(data$age,decreasing = T),][1:10,],
          aes(x=reorder(label,age),y=age,fill=is_winner))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=label,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=label,y=40,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('��Χ�������곤TOP10') + theme_wsj()+                 
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

ggsave("���곤top10.png", p, width = 10, height = 12) 

## ��������Χ
data$age = as.numeric(data$year)-as.numeric(data$born)
data$label = paste(data$name,'���:',data$year,'��Χʱ���䣺',data$age)
data$is_winner = as.factor(data$is_winner)
data_show = subset(data,age>0&age<100)
ggthemr('dust')
p<-ggplot(data[order(data$age,decreasing = F),][1:10,],
          aes(x=reorder(label,-age),y=age,fill=is_winner))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=label,y=0.1,image=img),size=0.07)+
  geom_text(aes(x=label,y=10,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('��Χ����������TOP10') + theme_wsj()+                 
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

ggsave("������top10.png", p, width = 10, height = 12) 


## �������
actress$label = paste(actress$name,'��������:',actress$total_nominate,'�񽱴���:',actress$total_award)
ggthemr('pale')
p<-ggplot(actress[order(actress$total_nominate,decreasing = T),][1:15,],
          aes(x=reorder(name,total_nominate),y=total_nominate,fill=rep(5:1,each=3)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.05)+
  geom_text(aes(x=name,y=5.5,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('�����������TOP15') + theme_wsj()+                 
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

ggsave("�����������top15.png", p, width = 12, height = 15) 

## �����
actress$label = paste(actress$name,'�񽱴���:',actress$total_award,'��������:',actress$total_nominate)
ggthemr('pale')
p<-ggplot(actress[order(actress$total_award,decreasing = T),][1:15,],
          aes(x=reorder(name,total_award),y=total_award,fill=rep(5:1,each=3)))+
  geom_bar(stat='identity',width = 0.7)+
  geom_image(aes(x=name,y=0.1,image=img),size=0.05)+
  geom_text(aes(x=name,y=1.5,label=label),size = 7,col='black',fontface='bold')+
  ggtitle('�񽱴������TOP15') + theme_wsj()+                 
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

ggsave("�񽱴������top15.png", p, width = 12, height = 15) 


library(tidyverse)
ggplot2::mpg
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data=mpg)+  geom_point(mapping = aes(x = hwy, y = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
ggplot(data = mpg)+geom_point(mapping = aes(x = displ, y = hwy, color=class))

transcripts<-read_csv()
ggplot(data=transcripts)+geom_boxplot(mapping=aes(y=Grade))+facet_grid(.~Year)
transcripts%>%
  filter(course%in% c("COR1005", "SSC1009"))%>%
  ggplot(aes(y=Grade))+
  geom_boxplot()+
  facet_grid(course~Year)


transcripts%>%
  filter(course%in% "HUM1011")%>%
  arrange(Year_num,desc(Grade))%>%
  select(Grade)

transcripts%>%
  ggplot(aes(y=Grade))+
  geom_boxplot()+
  facet_grid(.~Year)

transcripts%>%
  filter(course%in% c("SKI1004", "SKI1008"))%>%
  ggplot(aes(y=Grade))+
  geom_boxplot()+
  facet_grid(.~course)


transcripts%>%
  group_by(course)%>%
  summarise(mean = mean(Grade, na.rm = TRUE), count=n())%>%
  filter(count>10)%>%
  arrange(count)

transcripts%>%
  filter(course%in% c("SKI1004", "SKI1008"))%>%
  ggplot(aes(x=Grade, color=course))+ 
  geom_density()

x<-transcripts%>%
  filter(course=="SSC2051")%>%
  pull(Grade)

hist(x)

mu0<-0
t20<-0.1
s20<-2
nu0<-2

mean.y<-mean(x)
var.y<-var(x)
n<-length(x)


S<-10000
PHI<-matrix(nrow=S, ncol=2)
phi<-c(mean.y, 1/var.y)
PHI[1,]<-phi

for(s in 2:S){
  mun<-(mu0/t20+n*mean.y*phi[2])/(1/t20+n*phi[2])
  t2n<-1/(1/t20+n*phi[2])
  phi[1]<-rnorm(1,mun, sqrt(t2n))
  
  nun<-nu0+n
  s2n<-(nu0*s20+(n-1)*var.y+n*(mean.y-phi[1])^2)/nun
  phi[2]<-rgamma(1,nun/2, nun*s2n/2)
  PHI[s,]<-phi
}



as.data.frame(PHI)%>%
  ggplot(aes(V1, V2))+
  geom_point()

as.data.frame(PHI)%>%
  ggplot(aes(V1))+
  geom_histogram(binwidth = 0.01)


myfunction<-function(courseID){
  x<-transcripts%>%
    filter(course==courseID)%>%
    pull(Grade)
  return(x)
}
myfunction("COR1005")
courseNames<-transcripts%>%
  group_by(course)%>%
  summarise(mean = mean(Grade, na.rm = TRUE), count=n())%>%
  filter(count>50)%>%
  arrange(count)%>%
  pull(course)
g<-as.data.frame(t(courseNames))
pull(g, course)
S<-10000

mygibbs<-function(x){
  mu0<-7
  t20<-2
  s20<-2
  nu0<-2
  x<-
  mean.y<-mean(x)
  var.y<-var(x)
  n<-length(x)
  
  
  PHI<-matrix(nrow=S, ncol=2)
  phi<-c(mean.y, 1/var.y)
  PHI[1,]<-phi
  
  for(s in 2:S){
    mun<-(mu0/t20+n*mean.y*phi[2])/(1/t20+n*phi[2])
    t2n<-1/(1/t20+n*phi[2])
    phi[1]<-rnorm(1,mun, sqrt(t2n))

    nun<-nu0+n
    s2n<-(nu0*s20+(n-1)*var.y+n*(mean.y-phi[1])^2)/nun
    phi[2]<-rgamma(1,nun/2, nun*s2n/2)
    PHI[s,]<-phi
  }
  return(PHI[,1])
}
  
#output column for theta
mu<-matrix(nrow = S, ncol=length(courseNames))
for(i in 1:length(courseNames)){
  x<-myfunction(courseNames[i])
  gibbsoutput<-mygibbs(x)
  mu[,i]<-gibbsoutput
}



means<-colMeans(mu)
sort(means)
t_small<-transcripts%>%
  group_by(course)%>%
  summarise(mean = mean(Grade, na.rm = TRUE), count=n())%>%
  filter(count>50)

sample <- sample.int(n = nrow(transcripts),
                     size = floor(.80*nrow(transcripts)), # Selecting 70% of data
                     replace = F)

train <- transcripts[sample, ]
test  <- transcripts[-sample, ]


  
transcripts%>%
  filter(course==courseNames[1])
x

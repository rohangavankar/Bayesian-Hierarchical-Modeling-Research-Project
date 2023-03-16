library(tidyverse)
d_test_train <- transcripts %>%
  sample_n(n()) %>%
  group_by(course) %>%
  filter(Year_num >= 2014) %>%
  mutate(n_student_in_class = 1 : n())
d_test_train<-d_test_train%>%
  group_by(course) %>%
  filter(n()>10)%>%
  ungroup()

course_names<-d_test_train%>%
  group_by(course)%>%
  count()%>%
  select(course)
cutoff<-5
set_test<-filter(d_test_train, n_student_in_class<=cutoff)
set_train<-filter(d_test_train, n_student_in_class>cutoff)
set_train<-select(set_train, course, Grade)
set_test<-select(set_test, course, Grade)



fit_model_hier<-function(x)
{
  nu0<-1  
  s20<-3
  eta0<-1
  t20<-3
  mu0<-7 
  g20<-1
  course_names<-as.data.frame(t(unique(x[,1])))
  m<-ncol(course_names)
  n<-sv<-ybar<-rep(NA,m)
  for(j in 1:m){
    x_new<-filter(x, course==course_names[,j])%>%pull(Grade)
    
    ybar[j]<-mean(x_new)
    n[j]<-length(x_new)
    sv[j]<-var(x_new)
    
  }
    theta<-ybar
    sigma2<-mean(sv, na.rm=TRUE)
    mu<-mean(theta)
    tau2<-var(theta)
    
    ### setup MCMC
    set.seed(1)
    S<-20000
    THETA<-matrix( nrow=S,ncol=m) 
    SMT<-matrix( nrow=S,ncol=3)
    
    ###MCMC algorithm
    for(s in 1:S)
    {
        for(j in 1:m)
        {
        #sample new values of thetas
        vtheta<-1/(n[j]/sigma2+1/tau2)
        etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
        theta[j]<-rnorm(1,etheta, sqrt(vtheta))
        }
      #sample new values of sigma2
      nun<-nu0+sum(n)
      ss<-nu0*s20
      for(j in 1:m){
        x_new<-filter(x, course==course_names[,j])%>%pull(Grade)
        
        ss<-ss+sum((mean(x_new)-theta[j])^2)
      }
      sigma2<-1/rgamma(1,nun/2,ss/2)
      
      #sample new values of mu
      vmu<-1/(m/tau2+1/g20)
      emu<-vmu*(m*mean(theta)/tau2+mu0/g20)
      mu<-rnorm(1,emu, sqrt(vmu))
      
      #sample new values of tau2
      etam<-eta0+m
      ss<-eta0*t20+sum((theta-mu)^2)
      tau2<-1/rgamma(1,etam/2,ss/2)
      
      #store results
      THETA[s,]<-theta
    }
    
    return(THETA)
    }
    
fit_model_indep<-function(x){
  mu0<-7
  t20<-2
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
  return(PHI[,1])
}



fit_model_pool<-function(x){
  mu0<-7
  t20<-2
  s20<-2
  nu0<-2
  mean.y<-mean(x,na.rm = TRUE)
  var.y<-var(x, na.rm = TRUE)
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
  return(PHI[,1])
}




my_predict<-function(model, data){
  #model should be df with 2 columns:course, and theta
  MSE<- left_join(data, model, by="course")%>%
    mutate(square_error=(Grade-theta)^2)
  return(MSE)
}



c<-fit_model_pool(set_train[,2]%>%
                    pull(Grade))
e<-mean(c)
e<-data.frame(mean(c), 1:207)
c_new<-as.data.frame(course_names)
c_new<-cbind(c_new, e)
c_new<-c_new%>%
  select(course, mean.c.)
names(c_new)[names(c_new) == "mean.c."] <- "theta"
pool_error<-my_predict(c_new, set_test)



myfunction<-function(courseID){
  x<-set_train%>%
    filter(course==courseID)%>%
    pull(Grade)
  return(x)
}
course_names1<-t(course_names)
b<-matrix(nrow = S, ncol=length(course_names1))
for(i in 1:length(course_names1)){
  x<-myfunction(course_names1[,i])
  gibbsoutput<-fit_model_indep(x)
  b[,i]<-gibbsoutput
}
b_new<-t(as.data.frame(course_names1))
e<-as.data.frame(colMeans(b))
b_new<-cbind(e, b_new)
names(b_new)[names(b_new) == "colMeans(b)"] <- "theta"
indep_error<-my_predict(b_new, set_test)



a<-fit_model_hier(set_train)
a_new<-as.data.frame(colMeans(a))
course_names<-as.data.frame(unique(set_train[,1]))
a_new<-cbind(a_new, course_names)
names(a_new)[names(a_new) == "colnames(a)"] <- "course"
names(a_new)[names(a_new) == "colMeans(a)"] <- "theta"
hier_error<-my_predict(a_new, set_test)

summary(pool_error)
summary(indep_error)
summary(hier_error)

pool_error<-subset(pool_error, Grade>0)
hier_error<-subset(hier_error, Grade>0)
indep_error<-subset(indep_error, Grade>0)

a_sel<-as.data.frame(a[,1])

ggplot(data = a_sel)+geom_line(mapping = aes(x=1:20000, y=a[,1]))+
  xlim(2,20000)+ylim(6.95, 6.953)+xlab("Iteration")+ylab("Grade")+ggtitle("Plot of Gibbs Sampler Iterations")

hier_error%>%
  ggplot(aes(x=Grade, y=square_error))+
  geom_point()+ggtitle("Plot of Square Error and Grade per Course")+ylab("Square Error")
d_test_train
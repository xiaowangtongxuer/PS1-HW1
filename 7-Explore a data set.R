#7.1 �����ļ��ӱ�����������2000~2019������ƽ���¶�
setwd("D:/R-HW1")
tdata <- read.csv(file = "Temperature.csv", header = T)
colnames(tdata) 
year <- tdata$year.mon
t1  <- tdata$p1 #̽���ӱ�����������P1���¶ȵı仯���
#clean data
t1  <- t1*0.1 #�����������¶ȵĵ�λΪ0.1�棬����λ��Ϊ1��
t1[which(t1 > 40)]  <- NA
t1[which(t1 < -20)]  <- NA#clean possible data points with missing values or bad quality.
#7.2 ����p1��������¶ȵ�ʱ������ͼ
year <- as.POSIXlt(year)
plot_year <- function(n){#����һ����������plot������ݵ�����¶ȱ仯ͼ
  i1=1+(n-2000)*12
  i2=12+(n-2000)*12
  plot(mon[i1:i2],t1[i1:i2],type = 'o',xlab = n,ylab = 'T(��)')
}

plot_year(2000)

plot(year,t1,type = 'l',xlab = 'year',ylab = 'T(��)')

#7.3 �������ݵľ�ֵ����Χ��
check_year <- function(n){#����һ��������ݵĺ���
  i1=1+(n-2000)*12
  i2=12+(n-2000)*12
  m1=max(t1[i1:i2])
  m2=min(t1[i1:i2])
  m3=round(mean(t1[i1:i2]),2)
  m4=round(median(t1[i1:i2]),)
  m5 <- range(t1[i1:i2])
  print(paste0(n,'������¶�:',m1,'�棬����¶�:',m2,'��'))
  print(paste0('ƽ���¶�:',m3,'�棬��ֵ�¶�:',m4,'�棬�¶ȷ�Χ:',m5[1],'~',m5[2],'�档'))
  }
for(i in 2000:2019){#�ֱ���2000~2019������ݣ������
  check_year(i)
}

#���淢��
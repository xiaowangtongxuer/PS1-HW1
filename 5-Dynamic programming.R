#5.1
#��ٷ� ��123456789�ڲ��롰 �� ��+�� ��-�� ���ֲ������������ 3^8 �����
#ֱ�ӽ�3^8ת����3���Ƶ�����������������һ��6561*8�ľ�������
#�����ÿһ�оʹ�����һ�����㷽��
#Ȼ�����6561��ѭ�����þ���ÿһ�е�8������123456789��һ������0.1.2�ֱ�������ӷ���/-��/+��
#��str����¼�Դ�������ܱ���ʽ
#Ȼ����eval(parse(text=str))��������õ�sumֵ
#��ÿ��sumֵ��Ŀ��ֵ�Աȣ������ȣ��������Ӧ�ĵ�ʽ
Find_expression <- function(k)
{
  m_operator <- matrix(data=0,nrow = 6561,ncol = 8)#������������������ľ���
  for (i in 1:6561){#��1��6561��Ϊ3���Ƶ������浽�����У��������󴢴档
    #����8λ������λ����0��ֵ
    zh=i
    for(j in 8:1){
      m_operator[i,j]=zh%%3 #ʮ����ת��Ϊ3���Ƽ���
      zh =floor(zh/3)
      if(zh == 0 ){
        break
      }
    }
  }
  t=0
  for(i in 1:6561){#���������¼�õ���������뵽123456789��
    #0��1��2�ֱ����������ţ����źͼӺ�
    str <- '1'
    for(j in 1:8){#���ַ���str����¼123456789֮��Ĳ�������
      if (m_operator[i,j]==2){
        str=paste0(str,'+',j+1)#��paste0 ��������1��9���������γ�����Ҫ���ַ���str
      }
      if(m_operator[i,j] == 1){
        str=paste0(str,'-',j+1)
      }
      if(m_operator[i,j]==0){
        str=paste0(str,j+1)
      }
    }
    sum <- eval(parse(text=str)) #�øú���������str��ֵ����ֵ��sum
    #print(nchar(str))
    #print(str)
    #print(sum)
    if(sum==k){#ÿ����һ��sum����Ŀ��ֵ�Ƚϣ�����ȣ����������ʽ
      t=t+1
      print(paste('��',t,'����',str,'=',k))
    }
  }
  #����str����������ʽ��ֵ
}

#test
Find_expression(50)
Find_expression(100)

#5.2
Total_solutions <- vector(length=100)
Find_solution <- function(n){#���¶����˸�����������ֵ��Ϊ�����������ĸ���
  #���岿�ֺ�5.1��Find_expression������ͬ��ֻ����return�������޸�
  m_operator <- matrix(data=0,nrow = 6561,ncol = 8)
  for (i in 1:6561){
    zh=i
    for(j in 8:1){
      m_operator[i,j]=zh%%3
      zh =floor(zh/3)
      if(zh == 0 ){
        break
      }
    }
  }
  m_operator
  t=0
  for(i in 1:6561){
    str <- '1'
    for(j in 1:8){
      if (m_operator[i,j]==2){
        str=paste0(str,'+',j+1)
      }
      if(m_operator[i,j] == 1){
        str=paste0(str,'-',j+1)
      }
      if(m_operator[i,j]==0){
        str=paste0(str,j+1)
      }
    }
    sum <- eval(parse(text=str))
    #print(nchar(str))
    #print(str)
    #print(sum)
    if(sum==n){#��¼sum��Ŀ��ֵ��ȵĴ�������t����¼
      t=t+1
    }
  }
  if (i==6561){#ִ�����һ��ѭ��ʱ������tֵ
    return(t)
  }
}

#test ����1-100�ڸ����������������ĸ���
for (i in 1:100){
  Total_solutions[i] = Find_solution(i)
  #print(paste0(i,'->',Total_solutions[i]))
}


max=1
min=100
for(i in 1:100){#����Find_solution(1~100��������¼�������������������ֵ����Сֵ
  if(Total_solutions[i]>max){
    max=Total_solutions[i]
  }
  if(Total_solutions[i]<min){
    min=Total_solutions[i]
  }
}
ma=mi=0
ma_num <- vector()
mi_num <- vector()
for(i in 1:100){#��¼Total_solutions[1~100]�е���max��minֵ������Ȼ�����
  if(Total_solutions[i]==max){
    ma=ma+1
    ma_num[ma]=i
  }
  if(Total_solutions[i]==min){
    mi=mi+1
    print(mi)
    mi_num[mi]=i
  }
}

#plot
plot(seq(1,100,1),Total_solutions,type="o",
     xlab="number",ylab="solutions")

{print(paste0('max = ',max,' and the number is: ',ma_num[1],', ',ma_num[2]))
print(paste0('min = ',min,' and the number is: ',mi_num[1]))}

#2.1 ���ɾ���M1��M2
#��sample������������0-50ʱ֮����������ÿ�����ʮ����
c1 <- sample(0:50,50)
c2 <- sample(0:50,50)
M1 <- matrix(data=c1,nrow=5,ncol=10)#M1,M2��Ϊ����Ҫ��ľ���
M2 <- matrix(data=c2,nrow=10,ncol=5)

#2.2 ����Matrix_multip����������M1*M2
c3 <- vector(lenth=25)#���ڼ�¼��Ӧ������˺��ֵ
Matrix_multip <- function(ma,mb){
  ti=1
  for(k in 1:5 ) {#�˴�Ҳ�ɽ�5/5/10�޸�Ϊ��������Ӧ��������
    for (i in 1:5)
      {
      for (j in 1:10)
      {
        sum=sum+M1[i,j]*M2[j,k]#sum������¼ÿ��M1����Ԫ�غ�M2����Ԫ�صĳ˻�֮��
        #ÿ��forѭ����sum���¼ʮ���˻�֮��
      }
      c3[ti]=sum#c3 ������¼ÿ��sumֵ������ti����¼����
      sum=0#��sum���³�ʼ��
      ti=ti+1 #�������
    }
  }
  M3 <- matrix(data=c3[1:25],nrow=5,ncol=5)#��ΪM3Ϊ5x5�������ѡȡc3��ǰ25��ֵ����M3����
  return(M3)#M3��Ϊ����Ľ��
}
Matrix_multip(M1,M2)
M4 <- M1 %*% M2
M4
#���Ƚϣ�M3��M4��ȣ���ʵ�־�����ˡ�
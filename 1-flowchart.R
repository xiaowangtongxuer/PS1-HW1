Print_values <- function(a,b,c) {
  if(a>b){#�ȱȽ�a��b�Ĵ�С
    if(b>c){#a>bʱ���ٱȽ�b��c�Ĵ�С
      labc <- list(a=a,b=b,c=c)
      print(labc)
    }
    else{
      if(a>c){
        labc <- list(a=a,c=c,b=b)
        print(labc)
      }
      else {
        labc <- list(c=c,a=a,b=b)
        print(labc)
      }
    }
  }
  else {
    if (b>c){
      #����ͼ��a<bʱ����b>cʱ���ֻ��ӡ��c��a��b��
       labc <- list(c=c,a=a,b=b)
       print(labc)
      'if(c>a){
        labc <- list(b=b,c=c,a=a)
        print(labc)
      }
      else {
        labc <- list(b=b,a=a,c=c)
        print(labc)
      }
    }'#������b>c>a �� b>a>c �����
    else {
      labc <- list(c=c,b=b,a=a)
      print(labc)
    }
  }
}

#test ����ʵ������ͼ��Ҫ��
Print_values(3,2,1)
Print_values(4,2,3)
Print_values(4,2,5)
Print_values(1,2,3)
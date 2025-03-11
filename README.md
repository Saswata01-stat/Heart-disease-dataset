data=read.csv("heart.csv",header=T)
data
View(data)

hist(data$age,xlab="age",col="red",main="Age")
hist(data$trestbps,xlab="trestbps",col="blue",main="Resting Blood Sugar")
hist(data$chol,xlab="chol",col="green",main="Serum Cholesterol in mg/dl")
hist(data$thalach,xlab="thalach",col="yellow",main="Maximum Heart rate achieved")
hist(data$oldpeak,xlab="oldpeak",col="pink",main="ST depression induced by exercise relative to rest")

x=data$sex
male=sum(x>0)
male
female=sum(x<1)
female
total_people=male+female
total_people
x1=c(male,female)
labls_x=c("Male","Female")
pct_x=round((x1*100)/total_people)
labls_x=paste(labls_x,pct_x)
labls_x=paste(labls_x,"%",sep=" ")
pie(x1,labels=labls_x,col=rainbow(length(labls_x)),main="Sex Ratio")

y=data$cp
cp_type_I=sum(y<1);cp_type_I
cp_type_II=sum(y>0&y<2);cp_type_II
cp_type_III=sum(y>1&y<3);cp_type_III
cp_type_IV=sum(y>2);cp_type_IV
cp_total=cp_type_I+cp_type_II+cp_type_III+cp_type_IV;cp_total
y1=c(cp_type_I,cp_type_II,cp_type_III,cp_type_IV)
labls_y=c("type 1","type 2","type 3","type 4")
pct_y=round((y1*100)/cp_total)
labls_y=paste(labls_y,"-",pct_y)
labls_y=paste(labls_y,"%",sep="")
pie(y1,labels=labls_y,col=rainbow(length(labls_y)),main="Chest Pain Type")

z=data$fbs
fbs_normal=sum(z>0)
fbs_normal
fbs_high=sum(z<1)
fbs_high
total_fbs=fbs_normal+fbs_high
total_fbs
z1=c(fbs_normal,fbs_high)
labls_z=c("People with Normal fbs","People with High fbs")
pct_z=round((z1*100)/total_fbs)
labls_z=paste(labls_z,pct_z)
labls_z=paste(labls_z,"%",sep=" ")
pie(z1,labels=labls_z,col=rainbow(length(labls_z)),main="Fasting Blood Sugar")

restecg=data$restecg
restecg_I=sum(restecg<1);restecg_I
restecg_II=sum(restecg>0&restecg<2);restecg_II
restecg_III=sum(restecg>1);restecg_III
restecg_total=restecg_I+restecg_II+restecg_III;restecg_total
restecg1=c(restecg_I,restecg_II,restecg_III)
labls_restecg=c("restecg(value-0)","restecg(value-1)","restecg(value-2)")
pct_restecg=round((restecg1*100)/restecg_total)
labls_restecg=paste(labls_restecg,pct_restecg)
labls_restecg=paste(labls_restecg,"%",sep="")
pie(restecg1,labels=labls_restecg,col=rainbow(length(labls_restecg)),main="resting electrocardiographic results ")

exang=data$exang
exang_pos=sum(exang>0)
exang_pos
exang_neg=sum(exang<1)
exang_neg
total_exang=exang_pos+exang_neg
total_exang
exang1=c(exang_pos,exang_neg)
labls_exang=c("positive case","negative case")
pct_exang=round((exang1*100)/total_people)
labls_exang=paste(labls_exang,pct_exang)
labls_exang=paste(labls_exang,"%",sep=" ")
pie(exang1,labels=labls_exang,col=rainbow(length(labls_exang)),main="Exercise Indused Agina")

t=data$thal
n=sum(t<1)#normal
fd=sum(t>0&t<2)#fixed defect
rd=sum(t>1)#reversable defect
total=n+fd+rd
n_p=(n*100)/total;n_p
fd_p=(fd*100)/total;fd_p
rd_p=(rd*100)/total;rd_p
t_1=c(n_p,fd_p,rd_p)
labls=c("Normal","Fixed Defect","Reversible Defect")
t_p_1=round(t_1)
labls=paste(labls,t_p_1)
labls=paste(labls,"%",sep=" ")
pie(t_1,labels=labls,main="thalasemia defect")

s=data$slope
s_I=sum(s<1);s_I
s_II=sum(s>0&s<2);s_II
s_III=sum(s>1);s_III
s_total=s_I+s_II+s_III;s_total
s1=c(s_I,s_II,s_III)
labls_s=c("(slope-0)","(slope-1)","(slope-2)")
pct_s=round((s1*100)/s_total)
labls_s=paste(labls_s,pct_s)
labls_s=paste(labls_s,"%",sep="")
pie(s1,labels=labls_s,col=rainbow(length(labls_s)),main="Slope of the peak exercise ST segment")

ca=data$ca
ca_type_I=sum(ca<1);ca_type_I
ca_type_II=sum(ca>0&ca<2);ca_type_II
ca_type_III=sum(ca>1&ca<3);ca_type_III
ca_type_IV=sum(ca>2&ca<4);ca_type_IV
ca_type_V=sum(ca>3);ca_type_V
ca_total=ca_type_I+ca_type_II+ca_type_III+ca_type_IV+ca_type_V;ca_total
ca1=c(cp_type_I,cp_type_II,cp_type_III,cp_type_IV,ca_type_V)
labls_ca=c("0 vessels","1 vessels","2 vessels","3 vessels","4 vessels")
pct_ca=round((ca1*100)/ca_total)
labls_ca=paste(labls_ca,"-",pct_ca)
labls_ca=paste(labls_ca,"%",sep="")
pie(ca1,labels=labls_ca,col=rainbow(length(labls_ca)),main="Number of major vessels(0-4) coloured by fluroscopy")

d=data$sex
disease=sum(d>0)
disease
no_disease=sum(d<1)
no_disease
total_d=disease+no_disease
total_d
d1=c(disease,no_disease)
labls_d=c("Disease ","No Disease ")
pct_d=round((d1*100)/total_d)
labls_d=paste(labls_d,pct_d)
labls_d=paste(labls_d,"%",sep=" ")
pie(d1,labels=labls_d,col=rainbow(length(labls_d)),main="Patients with or without any disease(target)")


par(mfrow=c(2,2))
hist(data$age,xlab="age",col="red",main="Age")
pie(x1,labels=labls_x,col=rainbow(length(labls_x)),main="Sex Ratio")
pie(y1,labels=labls_y,col=rainbow(length(labls_y)),main="Chest Pain Type")
hist(data$trestbps,xlab="trestbps",col="blue",main="Resting Blood Sugar")

par(mfrow=c(2,2))
hist(data$chol,xlab="chol",col="green",main="Serum Cholesterol in mg/dl")
pie(z1,labels=labls_z,col=rainbow(length(labls_z)),main="Fasting Blood Sugar")
pie(restecg1,labels=labls_restecg,col=rainbow(length(labls_restecg)),main="resting electrocardiographic results ")
hist(data$thalach,xlab="thalach",col="yellow",main="Maximum Heart rate achieved")

par(mfrow=c(2,2))
pie(exang1,labels=labls_exang,col=rainbow(length(labls_exang)),main="Exercise Indused Agina")
hist(data$oldpeak,xlab="oldpeak",col="pink",main="ST depression induced by exercise relative to rest")
pie(s1,labels=labls_s,col=rainbow(length(labls_s)),main="Slope of the peak exercise ST segment")
pie(ca1,labels=labls_ca,col=rainbow(length(labls_ca)),main="Number of major vessels(0-4) coloured by fluroscopy")

par(mfrow=c(1,2))
pie(t_1,labels=labls,main="thalasemia defect")
pie(d1,labels=labls_d,col=rainbow(length(labls_d)),main="Patients with or without any disease(target)")



#chest pain table
#female
a_0=sum(x<1&y<1);a_0
b_0=sum(x<1&y>0&y<2);b_0
c_0=sum(x<1&y>1&y<3);c_0
d_0=sum(x<1&y>2);d_0
#male
a_1=sum(x>0&y<1);a_1
b_1=sum(x>0&y>0&y<2);b_1
c_1=sum(x>0&y>1&y<3);c_1
d_1=sum(x>0&y>2);d_1
#sum
sum_0=a_0+b_0+c_0+d_0;sum_0
sum_1=a_1+b_1+c_1+d_1;sum_1
sum_a_01=a_0+a_1;sum_a_01
sum_b_01=b_0+b_1;sum_b_01
sum_c_01=c_0+c_1;sum_c_01
sum_d_01=d_0+d_1;sum_d_01


#blood pressure table
#female
a_2=sum(x<1&data$trestbps<120);a_2
b_2=sum(x<1&data$trestbps>119&data$trestbps<130);b_2
c_2=sum(x<1&data$trestbps>129&data$trestbps<140);c_2
d_2=sum(x<1&data$trestbps>139&data$trestbps<181);d_2
e_2=sum(x<1&data$trestbps>180);e_2
#male
a_3=sum(x>0&data$trestbps<120);a_3
b_3=sum(x>0&data$trestbps>119&data$trestbps<130);b_3
c_3=sum(x>0&data$trestbps>129&data$trestbps<140);c_3
d_3=sum(x>0&data$trestbps>139&data$trestbps<181);d_3
e_3=sum(x>0&data$trestbps>180);e_3
#sum
sum_2=a_2+b_2+c_2+d_2+e_2;sum_2
sum_3=a_3+b_3+c_3+d_3+e_3;sum_3
sum_a_23=a_2+a_3;sum_a_23
sum_b_23=b_2+b_3;sum_b_23
sum_c_23=c_2+c_3;sum_c_23
sum_d_23=d_2+d_3;sum_d_23
sum_e_23=e_2+e_3;sum_e_23


#fbs
#female
a_4=sum(x<1&z>0);a_4
b_4=sum(x<1&z<1);b_4
#male
a_5=sum(x>0&z>0);a_5
b_5=sum(x>0&z<1);b_5
#sum
sum_4=a_4+b_4;sum_4
sum_5=a_5+b_5;sum_5
sum_a_45=a_4+a_5;sum_a_45
sum_b_45=b_4+b_5;sum_b_45


#Cholesterol
#female
a_6=sum(x<1&data$chol<200);a_6
b_6=sum(x<1&data$chol>199&data$chol<240);b_6
c_6=sum(x<1&data$chol>239);c_6
#male
a_7=sum(x>0&data$chol<200);a_7
b_7=sum(x>0&data$chol>199&data$chol<240);b_7
c_7=sum(x>0&data$chol>239);c_7
#sum
sum_6=a_6+b_6+c_6;sum_6
sum_7=a_7+b_7+c_7;sum_7
sum_a_67=a_6+a_7;sum_a_67
sum_b_67=b_6+b_7;sum_b_67
sum_c_67=c_6+c_7;sum_c_67


#restecg
#female
a_8=sum(x<1&restecg<1);a_8
b_8=sum(x<1&restecg>0&restecg<2);b_8
c_8=sum(x<1&restecg>1);c_8
#male
a_9=sum(x>0&restecg<1);a_9
b_9=sum(x>0&restecg>0&restecg<2);b_9
c_9=sum(x>0&restecg>1);c_9
#sum
sum_8=a_8+b_8+c_8;sum_8
sum_9=a_9+b_9+c_9;sum_9
sum_a_89=a_8+a_9;sum_a_89
sum_b_89=b_8+b_9;sum_b_89
sum_c_89=c_8+c_9;sum_c_89

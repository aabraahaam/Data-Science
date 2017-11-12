media <- c(0,0)
sig <- matrix(c(1,.8,.8,1),2)
df<-data.frame(mvrnorm(100,media,sig))
df1<-mvrnorm(100,media,sig)
str(df)
df[1,2]
df<-list(df,c(1,2))
df<-data.frame(df)
length(df)
df[201]
df[201]=1
df[101][2]=3
plot(df)
str(df)
df1<-rbind(df1,c(1,2))

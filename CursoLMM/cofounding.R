library(ggplot2)
rm(list=ls())


RnD = rnorm(250,40, 45)
profit = 10-3*RnD+rnorm(250,2000,480)
cname <- rep('A',250)

RnD2 = rnorm(250,155, 50)
profit2 = 160-1.4*RnD2+rnorm(250,3220,680)
cname2 <- rep('B',250)

RnD3 = rnorm(250,280, 65)
profit3 = 450-2*RnD3+rnorm(250,5200,480)
cname3 <- rep('C',250)

df = data.frame(RnD = double(), profit = double(),cname=character())

df= data.frame(RnD,profit,cname)
df2= data.frame(RnD2,profit2,cname2)
df3= data.frame(RnD3,profit3,cname3)

colnames(df2) <- colnames(df)
colnames(df3) <- colnames(df)

dfinal = rbind(df,df2,df3)

#df = rbind(df1,df2,df3)

dfinal |> ggplot(aes(RnD, profit))+geom_point()+stat_smooth(method=lm,col="steelblue",se=FALSE)+theme_minimal()+
  labs(x='PnD',y = 'Lucro')

dfinal |> ggplot(aes(RnD, profit))+
            geom_point(aes(colour = cname))+
            stat_smooth(method = lm, aes(group=cname,colour=cname),se=FALSE)+
            theme_minimal()+
            labs(x='PnD',y = 'Lucro',colour = 'Setor')



?stat_smooth
library(ggplot2)

# Simulate some data with a positive overall relationship, but negative within each group
set.seed(42)
n <- 200
data <- data.frame(
  x = rep(1:n, 4),
  y = rep(1:n, 4) + rnorm(n*4, 0, 5),
  group = factor(rep(1:4, each=n))
)

# Create a plot with stat_smooth for linear models, grouping by 'group'
ggplot(data, aes(x=x, y=y, color=group)) +
  geom_point() +
  stat_smooth(method="lm", aes(group=group), se=FALSE) +
  theme_minimal() +
  labs(title="Negative Linear Relationships Within Groups", 
       x="X Variable", y="Y Variable")


library(ggplot2)

# Simulate some data with a positive overall relationship but negative within each group
set.seed(42)
n <- 200
data <- data.frame(
  x = rep(1:n, 4),
  y = rep(1:n, 4) - rnorm(n*4, 0, 5),  # Ensure a negative slope within groups
  group = factor(rep(1:4, each=n))
)

# Create a plot with stat_smooth for linear models, grouping by 'group'
ggplot(data, aes(x=x, y=y, color=group)) +
  geom_point() +
  stat_smooth(method="lm", aes(group=group), se=FALSE) +
  theme_minimal() +
  labs(title="Negative Linear Relationships Within Groups", 
       x="X Variable", y="Y Variable")


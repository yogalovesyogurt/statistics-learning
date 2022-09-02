# Chapter 2 Lab: Introduction to R
# Basic Commands
x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
# 查看对象列表
ls()
# 去除不想要对象
rm(x,y)
ls()
# 消除所有对象
rm(list=ls())
# 查看帮助文件
?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
# 为了复现结果
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
# 标准差
sqrt(var(y))
sd(y)

# Graphics
x=rnorm(100)
y=rnorm(100)
plot(x,y)

# example:
# plot:散点图
#      xlab: 横坐标标题
#      main: 标题
#      col: 颜色
x=rnorm(100)
y=rnorm(100)
# pdf("D:\\code\\R\\plot\\Figure.pdf") # 用pdf保存到指定文件夹
# width,height:图片大小
# 这里注意\是转义字符(escape chacter,就是在该语言中有特殊含义的字符串)
# 如果用windows,需要改变路径为\\
# getwd() # current working directory
# setwd("D:\\R\\hw1") # set working directory
jpeg("D:\\code\\R\\plot\\Figure.jpeg",width=1200,height=600) 
# 截图并保存推荐一款windows好用的截图软件: paste
plot(x,y,xlab="this is the x-axis",
         ylab="this is the y-axis",
         main="Plot of X vs Y",
         col="green")
dev.off()

# 
x=seq(1,10)
x
x=1:10
x
# -pi,pi,元素个数 50
x=seq(-pi,pi,length=50)
y=x

# outer:外积函数
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
class(f) # 查看f的类型
dim(f)   # 查看f的shape

# contour:等高线图，用于表示三维数据
#         nlevels:number of contour levels desired iff levels is not supplied.
#         levels: numeric vector of levels at which to draw contour lines.
#         add: logical. If TRUE, add to a current plot.
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

# image: 热地图(相当于hearmap)
image(x,y,fa)

# persp: 三维图.theta,phi:观看图形的角度
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data
# 注:这里与python不同，index是从0开始的
A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)] # row:1,2   col:2,4 (integer)
A[1:3,2:4]       # row:1,2,3 col:2,3,4 (matrix)
A[1:2,]          # row:1,2   col:all
A[,1:2]          # row:all   col:1,2
A[1,]            # R中把单行和单列作为一个向量(integer)
A[-c(1,3),]      # -表示不包括指定的行和列
A[-c(1,3),-c(1,3,4)]
dim(A)

# Loading Data
# Auto.data来自课本官网数据集
# read.table(加载数据集) 这里需要改变为自己保存数据路径
#           header=T:第一行包含变量名(或header=True)
#           na.strings="?":用"?"表示缺失数据
Auto=read.table("D:\\code\\R\\R_book_data\\Auto.data") # data.frame
Auto=read.table("D:\\code\\R\\R_book_data\\Auto.data",header=T,na.strings="?")
# data.frame
fix(Auto)   # 电子窗口进行预览，在有新的R命令前需要对该窗口进行关闭
head(Auto)  # 显示data.frame的前行
names(Auto) # 查看数据的变量名
Auto$cylinders # 查看某一列的值
nrow(Auto) # 行数
ncol(Auto) # 列数
dim(Auto)  # shape
Auto[ Auto$cylinders == 8 , ]               # 找到 Auto$cylinders == 8 的行
Auto[ Auto$cylinders == 8 & Auto$mpg==18, ] # 找到 Auto$cylinders == 8 且 Auto$mpg==18 的行
Auto[ Auto$cylinders == 8 | Auto$mpg==18, ] # 找到 Auto$cylinders == 8 且 Auto$mpg==18 的行
Auto[ Auto$cylinders == 8 , 'mpg']          # Auto[ Auto$cylinders == 8,]$mpg


Auto=read.csv("D:\\code\\R\\R_book_data\\Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto) # 剔除有缺失数据的行

# Additional Graphical and Numerical Summaries
plot(cylinders, mpg) # 报错 cylinders和mpg均为变量名称
plot(Auto$cylinders, Auto$mpg)
attach(Auto) # 指定Auto数据集，直接通过变量名调用
plot(cylinders, mpg)
cylinders=as.factor(cylinders) # 定量变量转化为定性变量
# 如果x轴的变量是定性的，boxplot自动产生
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
# varwidth=TRUE to make boxplot widths proportional to the square root of the samples sizes. 
# horizontal=TRUE to reverse the axis orientation.
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
# hist:直方图
# breaks:a single number giving the number of cells for the histogram
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)

# pairs:散点图矩阵
# A response will be interpreted as another variable, but not treated specially, 
pairs(Auto)
# pairs(formula,data)
# 变量子集的散点图(numeric value)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower,mpg)
identify(horsepower,mpg,name) # 单击某点打印指定变量值
summary(Auto) # 对每个变量进行数值汇总
summary(mpg)


# 其它
# 推荐一款免费文本编辑器：sublime text
# R-studio的设置
# Tools->Global Options->Appearance(FontSize/Editor theme) 调整字体界面大小
# Tools->Global Options->Panel(调整4块panel的相对位置)
# Tools->Global Options->packages-->Primary CRAN repository-->change选择国内镜像源

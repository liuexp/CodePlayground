#coding=utf-8
from math import *

#u=open("u.dat");u=u.readlines();
#u=list(map(lambda x:'%.2f'% x,map(float,u)))
#i=open("i.dat");i=i.readlines();
#i=list(map(lambda x:'%.2f'% x,map(float,i)))

def sqr(x):
	return x*x

def handle(x):
	avg=0
	sigma=0
	for c in x:
		avg+=c
	n=len(x)
	avg/=n
	for c in x:
		sigma+=sqr(c-avg)
	print(avg)
	sigma=sqrt(sigma/(n-1))
	print(sigma)
#	print("\\begin{center}\n\\begin{tabular}{")
#	print("|>{\centering}m{1.0cm}")
#	for i in range(0,n):
#		print("|>{\centering}m{1.4cm}")
#	print("|}")
#	print("\\hline ")
#	print("编号 &")
#	print("&".join(str(i) for i in range(1,n+1)))
#	print("\\tabularnewline")
#	print("\\hline ")
#	print("t(s)&")
#	print("&".join(str(i) for i in x))
#	print("\\tabularnewline")
#	print("\\hline	\\end{tabular}	\\end{center}")
#	print("顶角$\\alpha$的平均值为$$\\bar{t}=\\frac{1}{",n,"}\\sum_{k=1}^{",n,"}t_k=",avg,"(s)$$")
#	print("顶角测量值的标准差为$$\sigma_t=\\sqrt{\\frac{\sum_{k=1}^{",n,"}(\\bar{t}-t_k)^2}{(",n,"-1)}}=",sigma,"(s)$$")
#	print("实际装置的示值误差为:$\\delta_{仪}=0(s)$\n\n顶角不确定度的 A 类分量为:$\\delta_A=",sigma,"(s)$,	B类分量为$\\delta_B=(s)$\n\n	于是下落时间的合成不确定度为$$u_t=\\sqrt{\\delta_A^2+\\delta_B^2}=(s)$$\n\n相对不确定度为$$u_r=\\%$$")
#	print("测量结果为$\\begin{cases}t=",avg,"\\pm",sigma,"(s)\\\\u_r=\\%\\end{cases}$")



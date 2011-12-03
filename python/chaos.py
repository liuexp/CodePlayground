out1=open('chaos1.dat','w+')
out2=open('chaos2.dat','w+')
out3=open('chaos3.dat','w+')
for kk in range(100,2000):
	k=kk/1000.0
	x=1.2
	for i in range(0,500):
		x=1.0-k*x*x
	for i in range(0,500):
		x=1.0-k*x*x
		out1.write(str(k)+'\n')
		out2.write(str(x)+'\n')
		out3.write(str(k)+'\t'+str(x)+'\n')
out1.close()
out2.close()
out3.close()


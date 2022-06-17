import os
N = ["0"+str(i) for i in range(1,10)]+[str(i) for i in range(10,41)]
z=0
for n in N:
  os.system("./MicroC casos/caso{}.mc < casos/caso{}.in > casos/salida{}.out".format(n,n,n))
  os.system("./MicroC -o casos/caso{}.mc < casos/caso{}.in > casos/salida{}.opt".format(n,n,n))
  nt = os.popen("diff casos/caso{}.out casos/salida{}.out".format(n,n)).read()
  no = os.popen("diff casos/caso{}.opt casos/salida{}.opt".format(n,n)).read()
  if(len(nt)> 0):
    z+=1
    print("Error en .out caso",n)
  if(len(no)> 0):
    z+=1
    print("Error en .opt caso",n)
if(z):
  print("Tienes ",z,"errores")
else:
  print("0w0")

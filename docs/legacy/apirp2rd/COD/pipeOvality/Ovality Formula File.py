from DataProvision.Ovality_01 import *

a1=0.015*c6   #a1=DNV-Pipe Except End(Out Of Roundness)
a2=10
a3=0.01*c6    #a3=DNV-Pipe End(Out Of Roundness)

if(c6<60.3):
   print("Included in diameter")  
if(c6<610):
   print("The value of a1 is",a1)
if(c6>1422):
    print("Agreed")
if(0.01*c6>10):
    print("10",a2)
else:
   print("a3",a3)

a4=0.02*c6    #a4=API-Pipe Except End(Out Of Roundness)
a5=15
a6=0.015*c6   #a6=API-Pipe End(Out Of Roundness)

if(c6<60.3):
   print("c6")  
if(c6<610):
   print("The value of a4 is",a4)
if(c6>1422):
    print("Agreed")
if(0.015*c6>15):
    print("15",a5)
else:
   print("a6",a6)

PipeExceptEnd = a1/c6
print("Ovality_DNV_PipeExceptEnd", PipeExceptEnd)
PipeEnd = a3/c6
print("Ovality_DNV_PipeEnd", PipeEnd)

APIPipeExceptEnd = a4/c6
print("Ovality_API_PipeExceptEnd", APIPipeExceptEnd)
APIPipeEnd = a6/c6
print("Ovality_API_PipeEnd", APIPipeEnd)

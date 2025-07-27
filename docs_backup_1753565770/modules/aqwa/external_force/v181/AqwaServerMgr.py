import socket
import sys
import struct
from math import *

# A simple set of classes for easy logging
class LogStdOut(object):
    def __init__(self,Log):
        self.Log=Log
        self.stdout = sys.stdout
        sys.stdout = self

    def __del__(self):
        sys.stdout = self.stdout
        self.Log.close()

    def write(self,data):
        self.stdout.write(data)
        self.stdout.flush()
        self.Log.write(data)
        self.Log.flush()

class LogStdErr(object):
    def __init__(self,Log):
        self.Log=Log
        self.stderr = sys.stderr
        sys.stderr = self

    def __del__(self):
        sys.stderr = self.stderr
        self.Log.close()

    def write(self,data):
        self.stderr.write(data)
        self.stderr.flush()
        self.Log.write(data)
        self.Log.flush()

class LogClass(object):
    def __init__(self,filename):
        self.Log=open(filename,"w")
        self.stderr=LogStdErr(self.Log)
        self.stdout=LogStdOut(self.Log)

    def write(self,data):
        self.stdout.write(data)

Log=LogClass("AqwaServerLogFile.txt")

# A class for socket message setup
class AqwaMsg:
    def __init__(self):
        self.ContentDescriptor=""
        Content=""

    def AddContent(self,DataType,Content):
        self.ContentDescriptor+=DataType
    

# The main class
class AqwaServerMgr:
    def __init__(self,port=0):
       
        self.IsOpen=False
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        Log.write('Socket created\n')
 
        #Bind socket to local host and port

        try:
            self.socket.bind(('',port))
        except socket.error as msg:
            Log.write('Bind failed. Error Code : ' + str(msg[0]) + ' Message ' + msg[1]+'\n')
            sys.exit()

        self.hostname = socket.gethostname()
        self.port = self.socket.getsockname()[1]

        fd=open("AQWA_SocketUserForceServerDetails.cfg","w")
        msg=("# Server waiting for a connection from an AqwaClient on\n%s:%s\n" % 
             (self.hostname,self.port))
        fd.write(msg)
        Log.write(str(msg))
        fd.close()
 
    def BaseReset(self):
        #Start listening on socket
        self.socket.listen(0)
        Log.write('Socket now listening\n')
  
        #now keep talking with the client
        
        #wait to accept a connection - blocking call
        self.Connection, addr = self.socket.accept()
        Log.write('Connected with ' + addr[0] + ':' + str(addr[1]) + "\n")
            
        self.IsOpen=True

        self.DataSizes={"i":4,"f":4,"d":8}


    def Receive(self,Descriptor):
        ExpectedSize=0
        # handle special case of strings
        if (Descriptor[0]=='s'):
            try:
                data = self.Connection.recv(4)
            except socket.error as E:
                self.Connection.shutdown(socket.SHUT_RDWR)
                self.Connection.close()
                self.IsOpen=False
                raise E
            S=struct.unpack("!i",data)
            ExpectedLength=S[0]
            try:
                SizeOfInt=4
                PaddedLength = ExpectedLength+(SizeOfInt-ExpectedLength%SizeOfInt)%SizeOfInt
                data = self.Connection.recv(PaddedLength)[0:ExpectedLength]
            except socket.error as E:
                self.Connection.shutdown(socket.SHUT_RDWR)
                self.Connection.close()
                self.IsOpen=False
                raise E
            if ((len(Descriptor)>1) & (Descriptor[1]=='u')):
                return data.decode('utf-8')
            return data

        # Now other cases

        for d in Descriptor:
            ExpectedSize+=self.DataSizes[d]

        #print "Total expected size in next message : %d" % ExpectedSize

        data=""

        try:
            data = self.Connection.recv(ExpectedSize)
        except socket.error as E:
            self.Connection.shutdown(socket.SHUT_RDWR)
            self.Connection.close()
            self.IsOpen=False
            raise E
     
        S=struct.unpack("!"+Descriptor,data)
        return S

    def Send(self,Objects):
        data=""
        try:
            for obj in Objects:
                try:
                    if (len(obj)>0):
                        L=len(obj)
                        if (type(obj[0])==int):
                            data+=struct.pack("!%di" % L, *obj)
                        elif (type(obj[0])==float):
                            data+=struct.pack("!%df" % L, *obj)
                except (TypeError):   
                    if (type(obj)==int):
                        data+=struct.pack("!i",obj)
                    elif (type(obj)==float):
                        data+=struct.pack("!f",obj)
                
        except (TypeError):
            if (type(Objects)==int):
                data+=struct.pack("!i",Objects)
            elif (type(Objects)==float):
                data+=struct.pack("!f",Objects)
        if len(data)>0:
            try:
                self.Connection.send(data)
            except socket.error:
                self.Connection.shutdown(socket.SHUT_RDWR)
                self.Connection.close()
                self.IsOpen=False
  
    def __del__(self):
        try:
            self.Connection.shutdown(socket.SHUT_RDWR)
            self.Connection.close()
            self.IsOpen=False
            self.socket.shutdown(socket.SHUT_RDWR)
            self.socket.close()
        except(socket.error):
            pass

# A simple class for dealing with vector rotations

class RotationMatrix:
    def __init__(self,RotAng=None):
        if (RotAng==None):
            self.m_XX=1      
            self.m_YX=0      
            self.m_ZX=0
            self.m_XY=0        
            self.m_YY=1      
            self.m_ZY=0
            self.m_XZ=0 
            self.m_YZ=0                 
            self.m_ZZ=1

        # from http://mathworld.wolfram.com/EulerAngles.html (pitch-roll-yaw convention)
        
        RX = RotAng[0]
        RY = RotAng[1]
        RZ = RotAng[2]

        cx=cos(RX)
        sx=sin(RX)
        cy=cos(RY)
        sy=sin(RY)
        cz=cos(RZ)
        sz=sin(RZ)

        self.m_XX=  cy*cz      
        self.m_XY=  cy*sz        
        self.m_XZ= -sy 
        self.m_YX=  sx*sy*cz - cx*sz   
        self.m_YY=  sx*sy*sz + cx*cz     
        self.m_YZ=  sx*cy                 
        self.m_ZX=  cx*sy*cz + sx*sz
        self.m_ZY=  cx*sy*sz - sx*cz
        self.m_ZZ=  cx*cy

    def Apply(self,X,Y,Z):
        rX= self.m_XX*X + self.m_YX*Y + self.m_ZX*Z
        rY= self.m_XY*X + self.m_YY*Y + self.m_ZY*Z
        rZ= self.m_XZ*X + self.m_YZ*Y + self.m_ZZ*Z
        return (rX,rY,rZ)

    def Inv(self):
        Inv=RotationMatrix()
        Inv.m_XX = self.m_XX      
        Inv.m_XY = self.m_YX      
        Inv.m_XZ = self.m_ZX
        Inv.m_YX = self.m_XY        
        Inv.m_YY = self.m_YY      
        Inv.m_YZ = self.m_ZY
        Inv.m_ZX = self.m_XZ 
        Inv.m_ZY = self.m_YZ                 
        Inv.m_ZZ = self.m_ZZ
        return Inv

def ForceTorque(FX,FY,FZ,DX,DY,DZ):
    Tx = DY*FZ - DZ*FY
    Ty = DZ*FX - DX*FZ
    Tz = DX*FY - DY*FX
    return (Tx,Ty,Tz)

# A simple class for storing Analysis related information
class Analysis:
    InputFileName = ""
    NOfStruct = 0
    I_Control = []
    R_Control = []
    COGs = []
    Pos=[]
    Vel=[]
    Time=0.0

    def GetNodeCurrentPosition(self,Struct,DefAxesX,DefAxesY,DefAxesZ):

        if ((self.Pos==[]) | (self.COGs == [])):
            sys.stderr.write("")
            raise Exception("Trying to use GetNodeCurrentPosition out of user defined function !")

        RotMat=RotationMatrix(self.Pos[Struct][3:]) # Pos[Struct][3:] are the rotation angles

        # Calculating offset from COGs in definition axes system
        COGOffsetX = DefAxesX - self.COGs[Struct][0]
        COGOffsetY = DefAxesY - self.COGs[Struct][1]
        COGOffsetZ = DefAxesZ - self.COGs[Struct][2]

        # Need to apply rotation on that vector
        RotatedX,RotatedY,RotatedZ = RotMat.Apply(COGOffsetX,COGOffsetY,COGOffsetZ)

        # Now we just need to add the current COG position to the rotated vector
        FinalX = RotatedX + self.Pos[Struct][0]
        FinalY = RotatedY + self.Pos[Struct][1]
        FinalZ = RotatedZ + self.Pos[Struct][2]
        return (FinalX,FinalY,FinalZ)

    def ApplyForceOnStructureAtPoint(self,Struct,FX,FY,FZ,AppPtX,AppPtY,AppPtZ):
        Force = BlankForce(self.NOfStruct)

        DX = AppPtX - self.Pos[Struct][0]
        DY = AppPtY - self.Pos[Struct][1]
        DZ = AppPtZ - self.Pos[Struct][2]
        
        Tx,Ty,Tz = ForceTorque(FX,FY,FZ,DX,DY,DZ)

        Force[Struct] = [FX,FY,FZ,Tx,Ty,Tz]

        return Force

# A subclass that deal with trading the analysis data with the Aqwa executable
class AqwaUserForceServer(AqwaServerMgr):
    def __init__(self):
        AqwaServerMgr.__init__(self)


    def Start(self):
        self.CurrentInterface = 0
        self.LatestInterfaceOnClient = self.Receive("i")[0]
        Log.write("Latest interface version on Client is v%d\n" % self.LatestInterfaceOnClient)

        # We are using interface v0. Let's test that it is lower than the latest available
        if (self.LatestInterfaceOnClient<self.CurrentInterface):
            Log.write("Unsupported interface\n")
            exit()

        # Let's tell AQWA that we want to use interface version 0
        self.Send(self.CurrentInterface)

        # Let's attach an Analysis
        self.Analysis = Analysis()

        # And lets get the input filename
        self.Analysis.InputFileName = self.Receive("su")

        Log.write(u"Input File Name is \""+self.Analysis.InputFileName+u".DAT\"\n")

        # Now let us get the number of structures
        self.Analysis.NOfStruct =  self.Receive("i")[0]
        # The I_Control
        self.Analysis.I_Control = self.Receive(100*"i")
        Log.write("I_Control = "+str(self.Analysis.I_Control)+"\n")
        # The R_Control
        self.Analysis.R_Control = self.Receive(100*"f")
        Log.write("R_Control = "+str(self.Analysis.R_Control)+"\n")
        # The Cog positions for each structure
        self.Analysis.COGs = []
        for ns in range(self.Analysis.NOfStruct):
            self.Analysis.COGs.append(self.Receive("fff"))
            Log.write("Position of CoG of Aqwa structure #%d in the definition axis : (%f,%f,%f)\n" % (ns+1,self.Analysis.COGs[-1][0],self.Analysis.COGs[-1][1],self.Analysis.COGs[-1][2]))


    def Run(self,UserForceFunction,ClosingFuntion=None):

        self.BaseReset()
        self.Start()

        while(True):
            Mode,Stage,Time,TimeStep = self.Receive("iiff")

            Pos=[]
            Vel=[]
            for ns in range(self.Analysis.NOfStruct):
                Pos.append(self.Receive("ffffff"))
            for ns in range(self.Analysis.NOfStruct):
                Vel.append(self.Receive("ffffff"))

            if (Mode==99):
                break
        
            self.Analysis.Pos=Pos
            self.Analysis.Vel=Vel
            self.Analysis.Time=Time

            Force,AddMass,ErrorFlag = UserForceFunction(self.Analysis,Mode,Stage,Time,TimeStep,Pos,Vel)

            # Need to flatten Force and AddedMass

            FlatForce = Force.Flat()
            FlatAddMass = AddMass.Flat()
        
            self.Send(FlatForce)
            self.Send(FlatAddMass)
            self.Send(ErrorFlag)

        if ClosingFuntion:
            ClosingFuntion(self)

# A set of class for manipulating Forces and AddMass

class BlankForceAddMass(list):
    Name="Force or AddMass"

    def MyBinOpPerStruct(self,other,output,s,op):
        raise TypeError("Class BlankForceAddMass should not be used directly.")

    def MyUnaryOpPerStruct(self,output,s,op):
        raise TypeError("Class BlankForceAddMass should not be used directly.")

    def MakeNew(self):
        if (self.Name=="AddedMass"):
            return BlankAddedMass(len(self))
        if (self.Name=="Force"):
            return BlankForce(len(self))
        raise TypeError("Class BlankForceAddMass should not be used directly.")        
        
    def MyBinOp(self,other,op):
        Output = self.MakeNew()
        if (type(other)!=type(self)):
            raise TypeError("Trying to sum a "+Name+" with something else.")
        if (len(self)!=len(other)):
            raise TypeError("Trying to sum two "+Name+"s with different numbers of structures.")

        for struct in range(len(self)):
            self.MyBinOpPerStruct(other,Output,struct,op)

        return Output

    def MyUnaryOp(self,op):
        Output = self.MakeNew()
        for struct in range(len(self)):
            self.MyUnaryOpPerStruct(Output,struct,op)

        return Output

    def __add__(self,other):
        return self.MyBinOp(other,lambda x,y : x+y)

    def __iadd__(self,other):
        return self + other

    def __sub__(self,other):
        return self.MyBinOp(other,lambda x,y : x-y)

    def __isub__(self,other):
        return self - other

    def __mul__(self,coef):
        if (type(coef)==int):
            coef=float(coef)
        if (type(coef)!=float):
            raise TypeError("Illegal attempt to multiply with unsupported type object.")
        return self.MyUnaryOp(lambda x: coef*x)

    def __imul__(self,coef):
        return self * coef
        
    def __rmul__(self,coef):
        return self.__mul__(coef)

    def __neg__(self):
        return (-1.0 * self)

class BlankAddedMass(BlankForceAddMass):
    def __init__(self,NS):
        self.Name = "AddedMass"
        for ns in range(NS):
            self.append([
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
                ]
                  )

    def Flat(self):
        return [float(AddMassOnDofDof) 
                for AddMassOfStruct in self 
                for AddMassOnDof in AddMassOfStruct 
                for AddMassOnDofDof in AddMassOnDof]

    # Overload the base class's method
    def MyBinOpPerStruct(self,other,Output,s,op):
        for dof in range(6):
            for dof2 in range(6):
                Output[s][dof][dof2] = op(self[s][dof][dof2],other[s][dof][dof2])

    def MyUnaryOpPerStruct(self,Output,s,op):
        for dof in range(6):
            for dof2 in range(6):
                Output[s][dof][dof2] = op(self[s][dof][dof2])

  

class BlankForce(BlankForceAddMass):
    def __init__(self,NS):
        self.Name = "Force"
        for ns in range(NS):
            self.append([0.0, 0.0, 0.0, 0.0, 0.0, 0.0])

    def Flat(self):
        return [float(ForceOnDof) 
                for ForceOfStruct in self 
                for ForceOnDof in ForceOfStruct]

    # Overload the base class's method
    def MyBinOpPerStruct(self,other,Output,s,op):
        for dof in range(6):
                Output[s][dof] = op(self[s][dof],other[s][dof])

    def MyUnaryOpPerStruct(self,Output,s,op):
        for dof in range(6):
                Output[s][dof] = op(self[s][dof])

if (__name__=="__main__"):

    # Simple example
    def UF(Analysis,Mode,Stage,Time,TimeStep,Pos,Vel):
        AddMass = BlankAddedMass(Analysis.NOfStruct)
        Force   = BlankForce(Analysis.NOfStruct)
        Error   = 0

        # User defined code here
        # Here additional inertial force for structure s : F[s][dof] = AddMass[s][dof2][dof] * Acc[s][dof2] 
        # (Einstein notation, and Python Row-Major order of array indices)
        for s in range(Analysis.NOfStruct):
            for dof in range(6):
                Force[s][dof] = 1000.0*Pos[s][dof]
                for dof2 in range(6):
                    AddMass[s][dof2][dof]=1e-3*(dof2*6+dof) # AddMass goes [0.000, 0.001, 0.002, ..., 0.036]
        
        # Now return the results

        return Force,AddMass,Error

    Server = AqwaUserForceServer()
    while True:
        Server.Run(UF)

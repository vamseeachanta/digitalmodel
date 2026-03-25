import math
import numpy
import os
import os.path
import ctypes
import tempfile
import shutil
import json

StringLength = 1024
encoding = f"cp{ctypes.windll.kernel32.GetACP()}"


def getBooleanTagValue(modelObject, name):
    value = modelObject.tags.get(name, None)
    if value is None or value == "False":
        return False
    elif value == "True":
        return True
    else:
        raise Exception("Unrecognised value for {}: {}".format(name, value))


def suppressRangeJumps(previous, this):
    if previous is None:
        return this
    # Make sure that Previous differs from "This" by no more than "360/2" by shifting "This" by an integer multiple of 360.
    modulusOver2 = 180
    difference = this - previous
    if abs(difference) > modulusOver2:
        return (
            previous - modulusOver2 + ((difference + modulusOver2) % (2 * modulusOver2))
        )
    else:
        return this


class ActuatorState(object):
    def __init__(self, x, xdot, xdotdot):
        self.x = x
        self.xdot = xdot
        self.xdotdot = xdotdot


class Actuator(object):
    def __init__(self, omega, gamma, dt):
        self.omega = omega
        self.gamma = gamma
        self.dt = dt  # assumes contant time step
        self.uprev = 0.0
        # assume zero inital state (i.e. can not start from a half run sim)
        self.prevState = ActuatorState(0.0, 0.0, 0.0)
        beta = math.sqrt(1.0 - gamma**2)
        self.g = (
            math.exp(-gamma * omega * dt) * math.sin(beta * omega * dt) / (beta * omega)
        )
        self.f = math.exp(-gamma * omega * dt) * (
            gamma * math.sin(beta * omega * dt) / beta + math.cos(beta * omega * dt)
        )

    def output(self, input):
        u = input
        f, g = self.f, self.g
        omegaSqr = self.omega**2
        gamma2Omega = 2.0 * self.gamma * self.omega
        xprev, xdotprev = self.prevState.x, self.prevState.xdot
        udot = (u - self.uprev) / self.dt
        state = ActuatorState(
            f * xprev
            + g * xdotprev
            + (2.0 * self.gamma * (f - 1.0) / self.omega + self.dt - g) * udot
            + (1.0 - f) * self.uprev,
            -g * omegaSqr * (xprev - self.uprev)
            + (f - gamma2Omega * g) * xdotprev
            + (1.0 - f) * udot,
            (gamma2Omega * g - f) * omegaSqr * (xprev - self.uprev)
            + ((4.0 * self.gamma**2 - 1.0) * omegaSqr * g - gamma2Omega * f)
            * xdotprev
            + omegaSqr * g * udot,
        )
        self.prevState = state
        self.uprev = u
        return state.x, state.xdot, state.xdotdot


class Controller(object):
    def __init__(self, info):
        turbine = info.ModelObject
        model = info.Model
        general = model.general
        modelDirectory = info.ModelDirectory
        self.kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)

        if turbine.DataNameValid("PitchControlMode"):  # if v11.0a or later
            if turbine.PitchControlMode == "Common":
                self.controlledBladeCount = 1
            else:
                self.controlledBladeCount = turbine.BladeCount
            if self.controlledBladeCount > 3:
                raise Exception(
                    turbine.Name
                    + " must not use pitch control on more than three blades."
                )
        else:
            raise Exception("Wrapper only supports OrcaFlex v11.0a and later.")

        self.DLLCanBeShared = getBooleanTagValue(turbine, "ControllerDLLCanBeShared")
        self.useActuator = getBooleanTagValue(turbine, "UseActuator")
        self.accelRefPosRrtTurbine = turbine.tags.get("AccelRefPosRrtTurbine", None)
        if self.accelRefPosRrtTurbine is not None:
            self.accelRefPosRrtTurbine = numpy.array(
                json.loads(self.accelRefPosRrtTurbine)
            )
        self.lastupdateTime = -numpy.inf
        self.firstCall = True
        self.simulationStartTime = model.simulationStartTime
        self.momentScaleFactor = turbine.UnitsConversionFactor("FF.LL")
        self.velocityScaleFactor = turbine.UnitsConversionFactor("LL.TT^-1")
        self.accelerationScaleFactor = turbine.UnitsConversionFactor("LL.TT^-2")

        self.yaw = 0.0
        self.yawDot = 0.0

        if general.DynamicsSolutionMethod == "Explicit time domain":
            self.dt = general.ActualOuterTimeStep
        elif (
            general.DynamicsSolutionMethod == "Implicit time domain"
            and not OrcFxAPI.HelperMethods.GetDataBoolean(
                general.handle, "ImplicitUseVariableTimeStep", 0
            )
        ):
            self.dt = general.ImplicitConstantTimeStep
        else:
            raise Exception("Turbine controllers require a constant time step.")

        if self.useActuator:
            omega = float(turbine.tags.ActuatorOmega)
            gamma = float(turbine.tags.ActuatorGamma)
            self.actuators = []
            for bladeIndex in range(self.controlledBladeCount):
                self.actuators.append(Actuator(omega, gamma, self.dt))

        DLLfileName = os.path.join(modelDirectory, turbine.tags.ControllerDLL)
        if not self.DLLCanBeShared:
            with tempfile.NamedTemporaryFile(suffix=".dll", delete=False) as tmp:
                self.DLLfileName = tmp.name
            shutil.copy2(DLLfileName, self.DLLfileName)
            DLLfileName = self.DLLfileName

        try:
            # load with LoadLibraryW so we can use FreeLibrary before we del the DLL
            LoadLibrary = self.kernel32.LoadLibraryW
            LoadLibrary.restype = ctypes.c_void_p
            LoadLibrary.argtypes = (ctypes.c_wchar_p,)
            self.libHandle = LoadLibrary(DLLfileName)
            if self.libHandle is None:
                raise ctypes.WinError(ctypes.get_last_error())

            self.dll = ctypes.CDLL("", handle=self.libHandle)
            self.DISCON = self.dll.DISCON
            self.DISCON.restype = None
            self.DISCON.argtypes = (
                OrcFxAPI.wrapped_ndpointer(
                    dtype=numpy.float32, ndim=1, flags="C_CONTIGUOUS"
                ),
                ctypes.POINTER(ctypes.c_int),
                ctypes.c_char_p,
                ctypes.c_char_p,
                ctypes.c_char_p,
            )

            self.avrSwap = numpy.zeros(84, numpy.float32)
            self.aviFail = ctypes.c_int()

            inputFileName = turbine.tags.get("InputFile", None)
            if inputFileName is None:
                self.accInfile = (ctypes.c_char * StringLength)()
            else:
                inputFileName = os.path.join(modelDirectory, turbine.tags.InputFile)
                self.accInfile = inputFileName.encode(encoding)

            """ Specify a file name that dll output can be written to. In this example, we combine the model name, the turbine object name and add
                5 extra characters. In the specific case of the ROSCO control dll, these extra characters will be removed and replace with "RO.dbg" """
            self.avcOutname = "{}_{}_     ".format(
                os.path.splitext(info.ModelFileName)[0], info.ModelObject.Name
            ).encode(encoding)
            self.avcMsg = (ctypes.c_char * StringLength)()
        except BaseException:
            self.unloadDLL()
            raise

    def __del__(self):
        if not self.firstCall:
            self.finalise()
        self.unloadDLL()

    def unloadDLL(self):
        if self.libHandle is not None:
            FreeLibrary = self.kernel32.FreeLibrary
            FreeLibrary.restype = ctypes.c_bool
            FreeLibrary.argtypes = (ctypes.c_void_p,)
            FreeLibrary(self.libHandle)
        if not self.DLLCanBeShared:
            os.remove(self.DLLfileName)

    def getRecord(self, index):
        # convert between 1-based FORTRAN indexing and 0-based numpy indexing
        return self.avrSwap[index - 1]

    def setRecord(self, index, value):
        # convert between 1-based FORTRAN indexing and 0-based numpy indexing
        self.avrSwap[index - 1] = value

    def callDLL(self):
        self.DISCON(
            self.avrSwap, self.aviFail, self.accInfile, self.avcOutname, self.avcMsg
        )

    def update(self, info):
        if info.SimulationTime <= self.lastupdateTime:
            return

        self.lastupdateTime = info.SimulationTime

        turbine = info.ModelObject
        instantaneousPeriod = OrcFxAPI.Period(OrcFxAPI.pnInstantaneousValue)

        # Prepare spec & index map for GetMultipleTimeHistories call used to extract
        # the required state not passed in info.InstantaneousCalculationData
        spec = []
        indexMap = {}

        def addToSpec(modelObject, varName, objectExtra=None):
            if not varName in indexMap: # handle multiple results for different blade indices
                indexMap[varName] = len(spec)
            spec.append(
                OrcFxAPI.TimeHistorySpecification(modelObject, varName, objectExtra)
            )

        def addToSpecForAllBlades(turbine, varName):
            for bladeIndex in range(self.controlledBladeCount):
                addToSpec(turbine, varName, OrcFxAPI.oeTurbine(bladeIndex + 1))

        oeEnv = OrcFxAPI.oeEnvironment(info.InstantaneousCalculationData.TurbinePosition)
        addToSpec(info.Model.environment, "Wind direction", oeEnv),
        addToSpec(turbine, "Azimuth")
        addToSpec(turbine, "Connection Lx moment")
        addToSpec(turbine, "Connection Ly moment")
        addToSpecForAllBlades(turbine, "Root connection Ex moment")
        addToSpecForAllBlades(turbine, "Root connection Ey moment")
        if turbine.PitchControlMode != "Common":
            addToSpecForAllBlades(turbine, "Blade pitch")
        thValues = OrcFxAPI.GetMultipleTimeHistories(spec, instantaneousPeriod)[0]

        # iStatus
        if self.firstCall:
            self.torque = turbine.TimeHistory(
                "Generator torque", instantaneousPeriod
            )[0]
            self.yawError = None
            self.nacelleYaw = None
            self.setRecord(50, len(self.accInfile))
            self.setRecord(51, len(self.avcOutname))
            self.setRecord(1, 0)
            self.firstCall = False
        else:
            self.setRecord(1, 1)

        # length of avcMsg character array
        self.setRecord(49, StringLength)

        # number of blades
        self.setRecord(61, info.InstantaneousCalculationData.BladeCount)

        # blade pitch
        if turbine.PitchControlMode == "Common":
            pitch = info.InstantaneousCalculationData.BladePitchAngle
            self.setRecord(28, 0)  # 0 for common control
            self.setRecord(4, pitch)
            self.setRecord(33, pitch)
            self.setRecord(34, pitch)
        else:
            self.setRecord(28, 1)  # 1 for individual control
            for bladeIndex in range(self.controlledBladeCount):
                recordIndex = [4, 33, 34][bladeIndex]
                self.setRecord(
                    recordIndex,
                    numpy.radians(
                        thValues[indexMap["Blade pitch"] + bladeIndex]
                    ),
                )

        # yaw error
        self.yawError = suppressRangeJumps(
            self.yawError, thValues[indexMap["Wind direction"]] - thValues[indexMap["Azimuth"]]
        )
        self.setRecord(24, numpy.radians(self.yawError))

        # nacelle yaw angle from North
        azimuthNorth = info.Model.general.NorthDirection
        if azimuthNorth is None or azimuthNorth == OrcFxAPI.OrcinaDefaultReal():
            # assume 180 deg, so 0 deg wind is from north
            azimuthNorth = 180.0

        self.nacelleYaw = suppressRangeJumps(
            self.nacelleYaw,
            thValues[indexMap["Azimuth"]] - (azimuthNorth - 180.0)
        )
        self.setRecord(37, numpy.radians(self.nacelleYaw))

        # horizontal hub wind speed
        self.setRecord(
            27,
            info.InstantaneousCalculationData.HorizontalHubWindSpeed
            / self.velocityScaleFactor,
        )

        # rotor azimuth angle
        self.setRecord(60, info.InstantaneousCalculationData.RotorAngle)

        # time
        self.setRecord(2, info.SimulationTime - self.simulationStartTime)

        # time step
        self.setRecord(3, self.dt)

        # generator speed
        self.setRecord(20, info.InstantaneousCalculationData.GeneratorAngVel)

        # rotor speed
        self.setRecord(21, info.InstantaneousCalculationData.MainShaftAngVel)

        # torque and power
        # DLL assumed to work in Nm
        dllTorque = -self.torque * 1000.000 / self.momentScaleFactor
        self.setRecord(23, dllTorque)
        self.setRecord(
            15, dllTorque * info.InstantaneousCalculationData.GeneratorAngVel
        )  # power not factored by efficiency

        # root in/out of plane bending moment
        for bladeIndex in range(self.controlledBladeCount):
            self.setRecord(
                69 + bladeIndex,
                -thValues[indexMap["Root connection Ex moment"] + bladeIndex] * 1000.000 / self.momentScaleFactor
            )  # DLL assumed to work in Nm
            self.setRecord(
                30 + bladeIndex,
                -thValues[indexMap["Root connection Ey moment"] + bladeIndex] * 1000.000 / self.momentScaleFactor
            )  # DLL assumed to work in Nm

        # "nodding" acceleration
        accelWrtTurbineRelGlobal = numpy.dot(
            info.InstantaneousCalculationData.TurbineOrientation,
            info.InstantaneousCalculationData.TurbineAcceleration,
        )
        angVelWrtTurbineRelGlobal, angAccelWrtTurbineRelGlobal = (
            info.InstantaneousCalculationData.TurbineAngularVelocity,
            info.InstantaneousCalculationData.TurbineAngularAcceleration,
        )
        if self.accelRefPosRrtTurbine is not None:
            # calculate the translational acceleration at the user nominated acceleration reference position.
            accelWrtTurbineRelGlobal += numpy.cross(
                angAccelWrtTurbineRelGlobal, self.accelRefPosRrtTurbine
            ) + numpy.cross(
                angVelWrtTurbineRelGlobal,
                numpy.cross(angVelWrtTurbineRelGlobal, self.accelRefPosRrtTurbine),
            )
        self.setRecord(
            53, accelWrtTurbineRelGlobal[2] / self.accelerationScaleFactor
        )  # translational
        self.setRecord(
            83, -angAccelWrtTurbineRelGlobal[1]
        )  # rotational, -ve convert to FAST coordinate system

        # Hub moments
        # assumes: DLL in Nm; ofx turbine Ly = -ve DLL Ly; ofx turbine Lx = DLL Lz; and DLL
        # load is rotor side to gen side (whereas ofx connection load is parent to child)
        self.setRecord(
            75, 
            thValues[indexMap["Connection Ly moment"]] * 1000.000 / self.momentScaleFactor
        ) 
        self.setRecord(
            76, 
            -thValues[indexMap["Connection Lx moment"]] * 1000.000 / self.momentScaleFactor
        )

        # call DISCON
        self.callDLL()

        if self.aviFail.value < 0:
            raise Exception(
                "Call to DISCON failed:\n{}".format(
                    self.avcMsg.value.decode(encoding)
                )
            )

        # read output from DISCON and assign state to be returned by external functions
        self.pitch, self.pitchDot, self.pitchDotDot = [], [], []
        for bladeIndex in range(self.controlledBladeCount):
            if turbine.PitchControlMode == "Common":
                pitchCommand = self.getRecord(45)
            else:
                pitchCommand = self.getRecord(42 + bladeIndex)
            if self.useActuator:
                actuatorOutput = self.actuators[bladeIndex].output(pitchCommand)
                self.pitch.append(actuatorOutput[0])
                self.pitchDot.append(actuatorOutput[1])
                self.pitchDotDot.append(actuatorOutput[2])
            else:
                self.pitch.append(pitchCommand)
                self.pitchDot.append(0.0)
                self.pitchDotDot.append(0.0)

        # DLL assumed to return value in Nm, first convert to OrcaFlex SI units (kN.m) and then to OrcaFlex model units
        self.torque = -self.getRecord(47) / 1000.0 * self.momentScaleFactor

        self.yawDot = self.getRecord(48)
        self.yaw += self.yawDot * self.dt

    def finalise(self):
        # iStatus
        self.setRecord(1, -1)

        # call DISCON
        self.callDLL()


class BladedController(object):
    def Initialise(self, info):
        key = info.ModelObject.handle.value
        controller = info.Workspace.get(key, None)
        if controller is None:
            controller = Controller(info)
            info.Workspace[key] = controller
        self.controller = controller

    def Finalise(self, info):
        key = info.ModelObject.handle.value
        if key in info.Workspace:
            del info.Workspace[key]

    def Calculate(self, info):
        self.controller.update(info)
        if info.DataName == "GeneratorTorqueController":
            info.Value = self.controller.torque
        elif info.DataName == "PitchController":
            if info.ModelObject.PitchControlMode == "Common":
                info.StructValue.Value = self.controller.pitch[0]
                info.StructValue.Velocity = self.controller.pitchDot[0]
                info.StructValue.Acceleration = self.controller.pitchDotDot[0]
            else:
                for bladeIndex in range(self.controller.controlledBladeCount):
                    info.StructValue[bladeIndex].Value = self.controller.pitch[bladeIndex]
                    info.StructValue[bladeIndex].Velocity = self.controller.pitchDot[bladeIndex]
                    info.StructValue[bladeIndex].Acceleration = self.controller.pitchDotDot[bladeIndex]
        else:
            raise Exception(
                "Turbine controller can only be used to control pitch or torque."
            )


class YawController(object):

    ''' An external function to implement yaw control by imposing Rz motion of a constraint (info.ModelObject).
        The yaw demand is calculated by the above BladedController external function. To link the two external
        functions, the constraint (info.ModelObject) must have an object tag named 'TurbineName', which should
        contain the name of the associated turbine using the BladedController. The controller examples package
        (https://github.com/Orcina-Ltd/turbine-controllers) contains a model in which yaw control, including the
        stiffness and damping of the yaw system, is demonstrated. '''

    def Initialise(self, info):
        turbineName = info.ModelObject.tags.get("TurbineName", None)
        if not turbineName:
            raise KeyError(
                f"To use the '{info.ModelObject.Name}' constraint to impose yaw controller demand, "
                "it must have an object tag named 'TurbineName', which should contain the associated turbine object name. "
                "No such tag was found."
            )

        if turbineName not in (obj.Name for obj in info.Model.objects):
            raise KeyError(
                f"The '{info.ModelObject.Name}' constraint is being used to impose yaw controller demand for turbine '{turbineName}'. "
                "However, this turbine cannot be found in the model. Please review the 'TurbineName' object tag for "
                f"'{info.ModelObject.Name}'."
            )

        self.key = info.Model[turbineName].handle.value
        self.controller = None

    def Calculate(self, info):

        if self.controller is None:
            self.controller = info.Workspace.get(self.key, None)

        if self.controller is None:
            raise RuntimeError("Turbine controller not found in workspace. Ensure controllers are active for the associated turbine object.")

        info.StructValue.AngularVelocity  = [0, 0, self.controller.yawDot]
        c, s = math.cos(self.controller.yaw), math.sin(self.controller.yaw)
        info.StructValue.Orientation = [[ c, s, 0], [-s, c, 0], [0 , 0, 1]]
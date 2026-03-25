import numpy
import math
import json
import os

"""
This module gives examples of Python external functions implementing control systems for use with the OrcaFlex Turbine object.
The contoller is based on the controller developed by NREL for the 5MW reference turbine  https://www.nrel.gov/docs/fy09osti/38060.pdf
The following optional tags, on the turbine object, can be used to modify the behaviour:

FloatingSystem : If given and set to True then the modifications proposed in https://www.nrel.gov/docs/fy10osti/47535.pdf are made to the controller.
                 If set to False, or not given, then no modification is made.

UseActuator :     If given and set to True, a 2nd order system is used to model the blade pitch actuators. The pitch command is input and the pitch
                  and its first two derivatives are calculated and returned to OrcaFlex.
                  If set to False, or not given, then no actuator is used.

ActuatorOmega :   Only used if UseActuator is True. The natural angular frequency of the 2nd order system used to model the actuator.

ActuatorGamma :   Only used if UseActuator is True. The damping ratio of the 2nd order system used to model the actuator.
"""


def getBooleanTagValue(modelObject, name):
    value = modelObject.tags.get(name, None)
    if value is None or value == "False":
        return False
    elif value == "True":
        return True
    else:
        raise Exception("Unrecognised value for {}: {}".format(name, value))


class Actuator(object):
    def state(self, x, xdot, xdotdot):
        return {
            "x": x,
            "xdot": xdot,
            "xdotdot": xdotdot,
        }

    def __init__(self, omega, gamma, dt):
        self.omega = omega
        self.gamma = gamma
        self.dt = dt  # assumes contant time step
        self.uprev = 0.0
        self.prevState = self.state(0.0, 0.0, 0.0)
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
        xprev, xdotprev = self.prevState["x"], self.prevState["xdot"]
        udot = (u - self.uprev) / self.dt
        stateNow = self.state(
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
        self.prevState = stateNow
        self.uprev = u
        return stateNow["x"], stateNow["xdot"], stateNow["xdotdot"]


class ControllerEngine(object):
    def __init__(self, info):
        model = info.Model
        general = model.general
        turbine = info.ModelObject

        def checkInitialPitch(initialPitch):
            if initialPitch != 0.0:
                raise Exception(turbine.Name + " initial blade pitch must be zero.")

        if turbine.DataNameValid("PitchControlMode"):  # if v11.0a or later
            checkInitialPitch(turbine.InitialPitch[0])
            if turbine.PitchControlMode != "Common":
                raise Exception(turbine.Name + " must use common pitch control mode.")
        else:
            checkInitialPitch(turbine.InitialPitch)

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

        self.useActuator = getBooleanTagValue(turbine, "UseActuator")
        self.FloatingSystem = getBooleanTagValue(turbine, "FloatingSystem")
        self.simulationStartTime = model.simulationStartTime
        self.momentScaleFactor = turbine.UnitsConversionFactor("FF.LL")
        self.velocityScaleFactor = turbine.UnitsConversionFactor("LL.TT^-1")

        if self.useActuator:
            omega = float(turbine.tags.ActuatorOmega)
            gamma = float(turbine.tags.ActuatorGamma)
            self.actuator = Actuator(omega, gamma, self.dt)

        self.cornerFreq = 1.570796
        self.targetSpeed = 122.9096  # rad/s

        self.minPitch, self.maxPitch = 0.0, numpy.pi / 2.0  # rad
        self.maxPitchRate = 0.1396263  # rad/s
        self.thetak = 0.1099965  # rad
        if not self.FloatingSystem:
            self.kp = 0.01882681
            self.ki = 0.008068634
        else:
            self.kp = 0.006275604
            self.ki = 0.0008965149

        self.ctInSpeed = (
            70.16224  # Transitional generator speed between regions 1 and 1 1/2, rad/s.
        )
        self.maxTorqueRate = 1.5e4  # N.m/s
        self.maxTorque = 47402.91  # N-m
        self.rgn2K = 2.332287  # torque constant in Region 2, N-m/(rad/s)^2
        self.rgn2Speed = (
            91.21091  # transition speed between regions 1 1/2 and 2, rad/s.
        )
        # min pitch at which torque is computed in region 3 regardless of gen
        # speed, rad (chosen to be 1.0deg above min pitch)
        self.rgn3minPitch = 0.01745329
        self.genTargetSpeed = (
            121.6805  # rated generator speed rad/s (chosen to be 99% of targetSpeed)
        )
        self.ratedPower = 5296610.0  # Watts (chosen to be 5MW divided by the electrical generator efficiency of 94.4%)
        self.slipPercent = 10.0  # rated generator slip percentage in Region 2 1/2
        self.sySpeed = self.genTargetSpeed / (1.0 + 0.01 * self.slipPercent)
        self.slope15 = (
            self.rgn2K
            * self.rgn2Speed
            * self.rgn2Speed
            / (self.rgn2Speed - self.ctInSpeed)
        )
        if not self.FloatingSystem:
            self.slope25 = (self.ratedPower / self.genTargetSpeed) / (
                self.genTargetSpeed - self.sySpeed
            )
        else:
            self.slope25 = (self.ratedPower / self.targetSpeed) / (
                self.genTargetSpeed - self.sySpeed
            )

        if self.rgn2K == 0.0:
            self.TrGnSp = self.sySpeed
        else:
            self.TrGnSp = (
                self.slope25
                - math.sqrt(
                    self.slope25 * (self.slope25 - 4.0 * self.rgn2K * self.sySpeed)
                )
            ) / (2.0 * self.rgn2K)

        if info.StateData is None:
            self.lastUpdateTime = -numpy.inf
            self.filteredSpeed = 0.0
            self.pitchCom = 0.0
            self.torqueCom = 0.0
            self.errorIntegral = 0.0
        else:
            stateData = json.loads(info.StateData)
            self.lastUpdateTime = stateData[0]
            self.filteredSpeed = stateData[1]
            self.pitchCom = stateData[2]
            self.torqueCom = stateData[3]
            self.errorIntegral = stateData[4]
            self.pitch = stateData[5]
            self.pitchDot = stateData[6]
            self.pitchDotDot = stateData[7]
            self.torque = stateData[8]
            if self.useActuator:
                self.actuator.uprev = stateData[9]
                self.actuator.prevState = stateData[10]

    def storeState(self, info):
        toDump = [
            self.lastUpdateTime,
            self.filteredSpeed,
            self.pitchCom,
            self.torqueCom,
            self.errorIntegral,
            self.pitch,
            self.pitchDot,
            self.pitchDotDot,
            self.torque,
        ]
        if self.useActuator:
            toDump += [self.actuator.uprev, self.actuator.prevState]
        info.StateData = json.dumps(toDump)

    def update(self, info):
        if info.NewTimeStep and info.SimulationTime > self.lastUpdateTime:
            self.lastUpdateTime = info.SimulationTime

            lastPitchCom, lastTorqueCom = self.pitchCom, self.torqueCom
            gainCorrection = 1.0 / (1.0 + lastPitchCom / self.thetak)

            pitchNow = info.InstantaneousCalculationData.BladePitchAngle  # rad
            speedNow = info.InstantaneousCalculationData.GeneratorAngVel  # rad/s

            # Apply the filter:
            alpha = math.exp(-self.dt * self.cornerFreq)
            self.filteredSpeed = (1.0 - alpha) * speedNow + alpha * self.filteredSpeed
            error = self.filteredSpeed - self.targetSpeed  # rad
            self.errorIntegral += error * self.dt

            ### Pitch control ###

            # saturate the integral term using the pitch angle limits
            self.errorIntegral = max(
                self.errorIntegral, self.minPitch / (gainCorrection * self.ki)
            )
            self.errorIntegral = min(
                self.errorIntegral, self.maxPitch / (gainCorrection * self.ki)
            )

            self.pitchCom = gainCorrection * (
                self.kp * error + self.ki * self.errorIntegral
            )
            self.pitchCom = max(
                min(self.pitchCom, self.maxPitch), self.minPitch
            )  # limit

            # saturate pitch rate and command
            pitchRate = (self.pitchCom - pitchNow) / self.dt
            pitchRate = max(min(pitchRate, self.maxPitchRate), -self.maxPitchRate)
            self.pitchCom = pitchNow + pitchRate * self.dt
            self.pitchCom = max(
                min(self.pitchCom, self.maxPitch), self.minPitch
            )  # limit (again)

            # set return values
            if self.useActuator:
                self.pitch, self.pitchDot, self.pitchDotDot = self.actuator.output(
                    self.pitchCom
                )
            else:
                self.pitch = self.pitchCom
                self.pitchDot = 0.0
                self.pitchDotDot = 0.0

            ### Torque control ###

            if (self.filteredSpeed >= self.genTargetSpeed) or (
                lastPitchCom >= self.rgn3minPitch
            ):
                if not self.FloatingSystem:
                    self.torqueCom = (
                        self.ratedPower / self.filteredSpeed
                    )  # region 3: power is constant
                else:
                    self.torqueCom = (
                        self.ratedPower / self.targetSpeed
                    )  # region 3: torque is constant for the floating system
            elif self.filteredSpeed <= self.ctInSpeed:
                self.torqueCom = 0.0  # region 1: torque is zero
            elif self.filteredSpeed < self.rgn2Speed:
                # region 1 1/2: linear ramp in torque from zero to optimal
                self.torqueCom = self.slope15 * (self.filteredSpeed - self.ctInSpeed)
            elif self.filteredSpeed < self.TrGnSp:
                # region 2: optimal torque is proportional to the sqr of the speed
                self.torqueCom = self.rgn2K * self.filteredSpeed * self.filteredSpeed
            else:
                # region 2 1/2: simple induction generator transition region
                self.torqueCom = self.slope25 * (self.filteredSpeed - self.sySpeed)

            self.torqueCom = min(self.torqueCom, self.maxTorque)  # limit

            # saturate torque rate and command
            torqueRate = (self.torqueCom - lastTorqueCom) / self.dt
            torqueRate = max(min(torqueRate, self.maxTorqueRate), -self.maxTorqueRate)
            self.torqueCom = lastTorqueCom + torqueRate * self.dt

            # set return value, dll returns value in Nm, first convert to OrcaFlex SI
            # units (kN.m) and then to OrcaFlex model units
            self.torque = -self.torqueCom / 1000.0 * self.momentScaleFactor


class PythonController(object):
    def Initialise(self, info):
        key = info.ModelObject.handle.value
        controller = info.Workspace.get(key, None)
        if controller is None:
            controller = ControllerEngine(info)
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
            info.StructValue.Value = self.controller.pitch
            info.StructValue.Velocity = self.controller.pitchDot
            info.StructValue.Acceleration = self.controller.pitchDotDot
        else:
            raise Exception(
                "Turbine controller can only be used to control pitch or torque."
            )

    def StoreState(self, info):
        self.controller.storeState(info)

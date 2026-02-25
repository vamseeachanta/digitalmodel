import numpy
import matplotlib.pyplot as pyplot
import OrcFxAPI

def f(x, a, omega, gamma):
    # damped harmonic oscillator
    return numpy.exp(-gamma*x) * a * numpy.cos(omega*x)

# 1000 equally spaced values between 0 and 100
X = numpy.linspace(0.0, 100.0, num=1000)
# evaluate f on these values
values = [f(x, 10.0, 0.4, 0.02) for x in X]

# plot the damped oscillator
pyplot.plot(values)
pyplot.show()

# calculate and output rainflow half cycles
halfCycleRanges = OrcFxAPI.RainflowHalfCycles(values)
for halfCycleRange in halfCycleRanges:
    print(halfCycleRange)

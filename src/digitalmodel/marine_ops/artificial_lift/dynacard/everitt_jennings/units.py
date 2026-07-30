# ABOUTME: Unit conversion factors for the Everitt-Jennings finite-difference solver.
# ABOUTME: The solver works internally in SI; these bridge to/from oilfield units.
"""Unit conversion constants.

The finite-difference rod-string solver operates entirely in SI (m, kg, s, N).
Callers supply oilfield units (in, lb, ft, psi), so every boundary crossing goes
through these factors. Each pair is defined once and inverted, so a typo in one
direction cannot silently disagree with the other.
"""

from math import pi

# meter & foot
M2FT = 1000.0 / (12.0 * 25.4)
FT2M = 1.0 / M2FT

# meter & inch
IN2M = 25.4 / 1000.0
M2IN = 1.0 / IN2M

# foot & inch
FT2IN = 12.0
IN2FT = 1.0 / FT2IN

# square feet & square inches
FTSQD2INSQD = 144.0
INSQD2FTSQD = 1.0 / FTSQD2INSQD

# day & second
DAY2SEC = 86400.0
SEC2DAY = 1.0 / DAY2SEC

# rpm & radians per second
RPM2RADPS = 2.0 * pi / 60.0
RADPS2RPM = 1.0 / RPM2RADPS

# rpm & rps
RPM2RPS = 1.0 / 60.0
RPS2RPM = 1.0 / RPM2RPS

# feet per hour & meters per second
FTPH2MPS = 0.000084667
MPS2FTPH = 1.0 / FTPH2MPS

# newton & pound force
N2KLB = 0.000224809
KLB2N = 1.0 / N2KLB
N2LB = N2KLB * 1000.0
LB2N = 1.0 / N2LB

# newton-meter & pound-foot
NM2LBFT = 0.737562121
LBFT2NM = 1.0 / NM2LBFT

# newton-meter & kilo-inch-pound
NM2KIP = 0.00885
KIP2NM = 1.0 / NM2KIP

# gallon & cubic meter
GAL2M3 = 0.003785412
M32GAL = 1.0 / GAL2M3

# gallon per minute & cubic meter per second
GALPM2M3PS = GAL2M3 / 60.0
M3PS2GALPM = 1.0 / GALPM2M3PS

# pound per gallon & kilogram per cubic meter
LBPGAL2KGPM3 = 119.826427317
KGPM32LBPGAL = 1.0 / LBPGAL2KGPM3

# pound per cubic foot & kilogram per cubic meter
LBPFT32KGPM3 = 16.0185
KGPM32LBPFT3 = 1.0 / LBPFT32KGPM3

# radians & degrees
RAD2DEG = 180.0 / pi
DEG2RAD = 1.0 / RAD2DEG

# psi & pascal
PSI2PA = 6894.7572931783
PA2PSI = 1.0 / PSI2PA

# pound & kilogram
LB2KG = 0.45359237
KG2LB = 1.0 / LB2KG

# mscf & scf
MSCF2SCF = 1000.0
SCF2MSCF = 1.0 / MSCF2SCF

# bbl & scf
BBL2SCF = 5.615
SCF2BBL = 1.0 / BBL2SCF

# gauge & absolute pressure
GAUGE2ABS = 14.7
ABS2GAUGE = -GAUGE2ABS

# fahrenheit & rankine
FHT2RANKINE = 459.67
RANKINE2FHT = -FHT2RANKINE

# centipoise & lb per ft-s
CP2LBPERFTSEC = 0.000671968994813
LBPERFTSEC2CP = 1.0 / CP2LBPERFTSEC


def convert(value: float, factor: float) -> float:
    """Apply a conversion factor.

    Kept as a named function so call sites read as a unit crossing rather than
    a bare multiplication — the boundary is where sign and scale errors hide.
    """
    return value * factor


def rpm_to_omega(rpm: float) -> float:
    """Convert rotational speed in RPM to angular velocity in rad/s."""
    return RPM2RADPS * rpm


def omega_to_rpm(omega: float) -> float:
    """Convert angular velocity in rad/s to rotational speed in RPM."""
    return omega * RADPS2RPM

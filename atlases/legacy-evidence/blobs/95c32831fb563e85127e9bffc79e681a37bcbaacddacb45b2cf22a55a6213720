# ABOUTME: DNV-RP-C203 S-N curve parameters, the single value source for
# ABOUTME: fatigue.sn_library and fatigue.sn_curves (#2165). Verified against 2011.
"""DNV-RP-C203 S-N curve parameters shared by the fatigue package.

Values are verified against DNV-RP-C203 (October 2011) Table 2-1 (in air),
Table 2-2 (seawater with cathodic protection) and Table 2-3 (seawater, free
corrosion), cross-checked against DNV's public PDF of that edition (#2165).
The records built from these values keep the edition label in
:data:`.c203_editions.DNV_RP_C203_IMPLEMENTED_EDITION`; whether the later
edition tabulates the same values is not established here.

Curve form (2011 section 2.4):

* in air: bilinear, knee at 1e7 cycles, m2 = 5 beyond it;
* seawater with CP: bilinear, knee at 1e6 cycles, m2 = 5 beyond it; the same
  second segment (log a2) as in air, so the fatigue limit at 1e7 cycles is the
  same as in air;
* free corrosion: single slope, m = 3.0 for every class and every cycle
  count; no knee and no fatigue limit.

Not covered here (follow-ups to #2165): the T curve (its thickness exponent
depends on the SCF), the 32 mm reference thickness for tubular joints, k = 0.10
for tubular butt welds made from one side, and the low-cycle cap at the B1
curve below 1e5 cycles.
"""

from __future__ import annotations

from typing import Final, Mapping, NamedTuple

#: Knee (slope change) in cycles.
N_KNEE_AIR: Final[float] = 1e7
N_KNEE_SEAWATER_CP: Final[float] = 1e6

#: Cycle count at which the tabulated fatigue limit is stated.
N_FATIGUE_LIMIT: Final[float] = 1e7

#: Slope of the free-corrosion curves (all classes).
M_FREE_CORROSION: Final[float] = 3.0

#: Reference thickness for welded connections other than tubular joints (mm).
T_REF_WELDED_MM: Final[float] = 25.0


class BilinearClass(NamedTuple):
    """One class of Table 2-1 / Table 2-2."""

    m1: float
    log_a1_air: float
    log_a1_cp: float
    m2: float
    log_a2: float
    fatigue_limit_mpa: float  # at 1e7 cycles, as tabulated
    k: float  # thickness exponent


#: Tables 2-1 and 2-2: m1, log a1 (air), log a1 (CP), m2, log a2, fatigue
#: limit at 1e7 cycles (MPa), thickness exponent k.
BILINEAR: Final[Mapping[str, BilinearClass]] = {
    "B1": BilinearClass(4.0, 15.117, 14.917, 5.0, 17.146, 106.97, 0.0),
    "B2": BilinearClass(4.0, 14.885, 14.685, 5.0, 16.856, 93.59, 0.0),
    "C": BilinearClass(3.0, 12.592, 12.192, 5.0, 16.320, 73.10, 0.15),
    "C1": BilinearClass(3.0, 12.449, 12.049, 5.0, 16.081, 65.50, 0.15),
    "C2": BilinearClass(3.0, 12.301, 11.901, 5.0, 15.835, 58.48, 0.15),
    "D": BilinearClass(3.0, 12.164, 11.764, 5.0, 15.606, 52.63, 0.20),
    "E": BilinearClass(3.0, 12.010, 11.610, 5.0, 15.350, 46.78, 0.20),
    "F": BilinearClass(3.0, 11.855, 11.455, 5.0, 15.091, 41.52, 0.25),
    "F1": BilinearClass(3.0, 11.699, 11.299, 5.0, 14.832, 36.84, 0.25),
    "F3": BilinearClass(3.0, 11.546, 11.146, 5.0, 14.576, 32.75, 0.25),
    "G": BilinearClass(3.0, 11.398, 10.998, 5.0, 14.330, 29.24, 0.25),
    "W1": BilinearClass(3.0, 11.261, 10.861, 5.0, 14.101, 26.32, 0.25),
    "W2": BilinearClass(3.0, 11.107, 10.707, 5.0, 13.845, 23.39, 0.25),
    "W3": BilinearClass(3.0, 10.970, 10.570, 5.0, 13.617, 21.05, 0.25),
}


class FreeCorrosionClass(NamedTuple):
    """One class of Table 2-3 (m = 3.0 throughout)."""

    log_a: float
    k: float  # thickness exponent


#: Table 2-3: log a (m = 3.0), thickness exponent k.
FREE_CORROSION: Final[Mapping[str, FreeCorrosionClass]] = {
    "B1": FreeCorrosionClass(12.436, 0.0),
    "B2": FreeCorrosionClass(12.262, 0.0),
    "C": FreeCorrosionClass(12.115, 0.15),
    "C1": FreeCorrosionClass(11.972, 0.15),
    "C2": FreeCorrosionClass(11.824, 0.15),
    "D": FreeCorrosionClass(11.687, 0.20),
    "E": FreeCorrosionClass(11.533, 0.20),
    "F": FreeCorrosionClass(11.378, 0.25),
    "F1": FreeCorrosionClass(11.222, 0.25),
    "F3": FreeCorrosionClass(11.068, 0.25),
    "G": FreeCorrosionClass(10.921, 0.25),
    "W1": FreeCorrosionClass(10.784, 0.25),
    "W2": FreeCorrosionClass(10.630, 0.25),
    "W3": FreeCorrosionClass(10.493, 0.25),
}

#: Curve classes in table order.
CLASSES: Final[tuple[str, ...]] = tuple(BILINEAR)

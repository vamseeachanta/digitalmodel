# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent P/A arithmetic and binary64 limits for nominal plate demands.

The synthetic geometry uses the hand-calculation basis in test_mechanics.py:
P=100 kN=100000 N, t=20 mm, pin=40 mm, hole=50 mm, W=150 mm,
e=75 mm. Its areas are 800, 2000 and 2000 mm². The unloaded case
uses P=0 N, so every stress is exactly zero for these positive areas.

Reference for the range probes: Python's sys.float_info documentation
(max_exp, min_exp, mant_dig) and the binary64 round-to-nearest-even model.
https://docs.python.org/3/library/sys.html#sys.float_info
The largest finite value is below 2**1024, the smallest positive subnormal
is 2**-1074, and the halfway value 2**-1075 rounds to zero. Powers of two
and adjacent floats below are synthetic numerical probes, not plate designs.
"""

import math

import pytest

from digitalmodel.lifting_lug.mechanics import plate_demands_mpa


@pytest.fixture
def plate_inputs():
    """Return fresh inputs from the independent hand calculation above."""
    return {
        "load_kn": 100.0,
        "thickness_mm": 20.0,
        "pin_diameter_mm": 40.0,
        "hole_diameter_mm": 50.0,
        "width_mm": 150.0,
        "centre_to_loaded_edge_mm": 75.0,
    }


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf],
                         ids=["nan", "positive-infinity", "negative-infinity"])
@pytest.mark.parametrize("argument", [
    "load_kn",
    "thickness_mm",
    "pin_diameter_mm",
    "hole_diameter_mm",
    "width_mm",
    "centre_to_loaded_edge_mm",
])
def test_all_nonfinite_arguments_are_refused(plate_inputs, argument, value):
    plate_inputs[argument] = value
    message = ("load_kn must be finite and nonnegative" if argument == "load_kn"
               else "plate dimensions must be finite and positive")
    with pytest.raises(ValueError, match=f"^{message}$"):
        plate_demands_mpa(**plate_inputs)


def test_equal_pin_and_hole_hand_demands(plate_inputs):
    """Pin=hole=50 mm: Ab=50*20=1000 mm², An=(150-50)*20=2000 mm².

    As=2*(75-50/2)*20=2000 mm². With P=100000 N, P/A independently
    gives bearing=100 MPa, net tension=50 MPa and tear-out=50 MPa.
    """
    plate_inputs["pin_diameter_mm"] = 50.0
    result = plate_demands_mpa(**plate_inputs)
    assert result.pin_bearing == 100.0
    assert result.net_tension == 50.0
    assert result.shear_tearout == 50.0


@pytest.mark.parametrize("argument,value,message", [
    ("width_mm", 50.0, "width must exceed hole diameter"),
    ("width_mm", math.nextafter(50.0, -math.inf),
     "width must exceed hole diameter"),
    ("centre_to_loaded_edge_mm", 25.0,
     "loaded edge must leave a positive clear ligament"),
    ("centre_to_loaded_edge_mm", math.nextafter(25.0, -math.inf),
     "loaded edge must leave a positive clear ligament"),
], ids=["zero-net", "negative-net", "zero-edge", "negative-edge"])
def test_degenerate_ligaments_are_refused(plate_inputs, argument, value, message):
    """W=hole=50 or e=hole/2=25 gives zero ligament; one float below is negative."""
    plate_inputs[argument] = value
    with pytest.raises(ValueError, match=f"^{message}$"):
        plate_demands_mpa(**plate_inputs)


def test_positive_clear_ligament_hand_demands(plate_inputs):
    """At e=nextafter(25,+inf), the clear ligament is 2**(4-52)=2**-48 mm.

    As=2*2**-48*20=40*2**-48 mm², hence shear=100000/As=2500*2**48
    MPa. The unchanged bearing and net areas give 125 and 50 MPa.
    """
    plate_inputs["centre_to_loaded_edge_mm"] = math.nextafter(25.0, math.inf)
    result = plate_demands_mpa(**plate_inputs)
    assert result.pin_bearing == 125.0
    assert result.net_tension == 50.0
    assert result.shear_tearout == 2500.0 * 2.0**48


def test_zero_load_zeroes_all_demands(plate_inputs):
    plate_inputs["load_kn"] = 0.0
    result = plate_demands_mpa(**plate_inputs)
    assert result.pin_bearing == 0.0
    assert result.net_tension == 0.0
    assert result.shear_tearout == 0.0


def test_unrepresentable_derived_force_is_refused(plate_inputs):
    """Finite P=2**1023 kN gives 1000*2**1023 N > 2**1024: overflow.

    Multiplication by the SI factor 1000 cannot underflow a positive finite
    load; the exactly zero load is covered separately.
    """
    plate_inputs["load_kn"] = 2.0**1023
    assert math.isfinite(plate_inputs["load_kn"])
    with pytest.raises(ValueError, match=(
        "^derived force and areas must be finite and representable$"
    )):
        plate_demands_mpa(**plate_inputs)


@pytest.mark.parametrize("area", ["bearing", "net", "shear"])
@pytest.mark.parametrize("range_limit", ["overflow", "underflow"])
def test_unrepresentable_derived_area_is_refused(plate_inputs, area, range_limit):
    """Isolate each area with finite inputs and zero load, which cannot mask it.

    Overflow uses t=2**512 mm and hole=2**512 mm. The target area is
    2**1024 mm² in each case; every other area remains finite and positive:
    * bearing: pin=hole, W=nextafter(hole,+inf), e=nextafter(hole/2,+inf).
      Ab=2**(512+512); An=2**(460+512); As=2**(1+459+512).
    * net: pin=1, W=2**513, same e. Ab=2**512; An=2**(512+512);
      As=2**(1+459+512).
    * shear: pin=1, W=nextafter(hole,+inf), e=hole. Ab=2**512;
      An=2**(460+512); As=2**(1+511+512).

    Underflow uses t=2**-1023 mm, pin=hole=1 mm, W=2 mm, e=1 mm,
    altering only the target geometry below. Non-target areas stay 2**-1023:
    * bearing: pin=2**-52, so Ab=2**(-52-1023)=2**-1075.
    * net: W=nextafter(1,+inf), so An=2**(-52-1023)=2**-1075.
    * shear: e=nextafter(1/2,+inf), so As=2**(1-53-1023)=2**-1075.
    Each target rounds to zero, although every input and ligament is positive.
    """
    plate_inputs["load_kn"] = 0.0
    if range_limit == "overflow":
        scale = 2.0**512
        plate_inputs.update(
            thickness_mm=scale,
            pin_diameter_mm=scale if area == "bearing" else 1.0,
            hole_diameter_mm=scale,
            width_mm=2.0**513 if area == "net" else math.nextafter(scale, math.inf),
            centre_to_loaded_edge_mm=(
                scale if area == "shear" else math.nextafter(scale / 2.0, math.inf)
            ),
        )
    else:
        plate_inputs.update(
            thickness_mm=2.0**-1023,
            pin_diameter_mm=2.0**-52 if area == "bearing" else 1.0,
            hole_diameter_mm=1.0,
            width_mm=math.nextafter(1.0, math.inf) if area == "net" else 2.0,
            centre_to_loaded_edge_mm=(
                math.nextafter(0.5, math.inf) if area == "shear" else 1.0
            ),
        )
    assert all(math.isfinite(value) for value in plate_inputs.values())
    with pytest.raises(ValueError, match=(
        "^derived force and areas must be finite and representable$"
    )):
        plate_demands_mpa(**plate_inputs)

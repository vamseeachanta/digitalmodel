"""API 650 Annex E public-source screen: independent arithmetic and limits."""
import math
from dataclasses import asdict

import pytest

from digitalmodel.hydrodynamics import sloshing as s


def inputs(**changes):
    values = dict(
        diameter_m=20.0, fill_height_m=10.0, liquid_density_kg_m3=1000.0,
        shell_weight_n=1_000_000.0, roof_weight_n=200_000.0, bottom_weight_n=190_000.0,
        shell_cg_m=5.5, roof_cg_m=11.0, roof_reaction_n=200_000.0,
        shell_thickness_m=0.012, sds_g=0.6, sd1_g=0.3, tl_s=8.0,
        importance_factor=1.25, rwi=3.5, rwc=2.0, k=1.5,
        minimum_ai_g=0.007, vertical_acceleration_g=0.0,
        seismic_use_group="II",
        available_freeboard_m=1.0, purchaser_freeboard=False,
    )
    values |= changes
    if "annular_resistance_n_per_m" not in changes:
        # 40 kN/m, or the E.6.2.1.1-1a cap 201.1*H*D*Ge where that is lower.
        try:
            ge = values["liquid_density_kg_m3"] / 1000 * (1 - .4 * values["vertical_acceleration_g"])
            cap = 201.1 * values["fill_height_m"] * values["diameter_m"] * ge
            values["annular_resistance_n_per_m"] = min(40_000.0, cap) if math.isfinite(cap) else 40_000.0
        except TypeError:
            values["annular_resistance_n_per_m"] = 40_000.0
    return values


def test_worked_example():
    r = s.api650_annex_e(**inputs())
    # Independent arithmetic, SI (API 2007 E.6.1; API 2016 E.7.2):
    # Wp=1000*9.80665*pi*20^2/4*10=30,808,499.596 N.
    # u=.866*2=1.732; tanh(u)=.939291838; Wi/Wp=.542316304.
    # z=3.67/2=1.835; Wc/Wp=.46*tanh(z)=.437143939.
    # Xi=.375*10=3.75; Xc=10*(1-tanh(z/2)/z)=6.050613 m.
    # Xis=3.75*[1+1.333*(1.732/tanh(1.732)-1)]=7.968657 m.
    # Xcs=Xc+10*.937/(z*sinh(z))=7.723285 m.
    # Tc=1.8*.578*sqrt(20/tanh(1.84))=4.771686 s.
    # Ai=.6*1.25/3.5=.214285714; Ac=1.5*.3*1.25/(2*Tc)=.058941430.
    # Af=1.5*.3*1.25*4/Tc^2=.098818621 (no Rwc).
    assert r.product_weight_n == pytest.approx(30_808_499.596, abs=.001)
    assert r.impulsive_weight_n == pytest.approx(16_707_951.624, abs=.001)
    assert r.convective_weight_n == pytest.approx(13_467_748.880, abs=.001)
    assert r.xi_m == 3.75
    assert r.xc_m == pytest.approx(6.050613, abs=1e-6)
    assert r.xis_m == pytest.approx(7.968657, abs=1e-6)
    assert r.xcs_m == pytest.approx(7.723285, abs=1e-6)
    assert r.convective_period_s == pytest.approx(4.771686, abs=1e-6)
    assert r.ai_g == pytest.approx(.214285714, abs=1e-9)
    assert r.ac_g == pytest.approx(.058941430, abs=1e-9)
    assert r.af_g == pytest.approx(.098818621, abs=1e-9)
    # Vi=.214285714*(1.2e6+1.9e5+16,707,951.624)=3,878,132.491 N (Wf included).
    # Vc=.058941430*13,467,748.880=793,808.376 N; V=hypot(Vi,Vc).
    assert r.impulsive_shear_n == pytest.approx(3_878_132.491, abs=.001)
    assert r.convective_shear_n == pytest.approx(793_808.376, abs=.001)
    assert r.base_shear_n == pytest.approx(3_958_540.559, abs=.001)
    # Dry moment = 1e6*5.5+2e5*11=7.7e6 N m.
    # Mrw=hypot(Ai*(Wi*3.75+7.7e6), Ac*Wc*6.050613).
    # Ms uses Xis=7.968657 and Xcs=7.723285.
    assert r.ringwall_moment_nm == pytest.approx(15_822_636.497, abs=.001)
    assert r.slab_moment_nm == pytest.approx(30_796_401.386, abs=.001)
    # delta=.42*20*.098818621; freeboard=.7*delta.
    assert r.wave_height_m == pytest.approx(.830076418, abs=1e-9)
    assert r.required_freeboard_m == pytest.approx(.581053493, abs=1e-9)
    assert r.freeboard_ok is True
    # wa=40,000 N/m is below the cap 201.1*10*20*1.0=40,220 N/m.
    # J=15,822,636.497/[400*(1.2e6/(pi*20)+4e4)]=.669332198.
    assert r.anchorage_ratio_j == pytest.approx(.669332198, abs=1e-9)
    assert r.anchorage_status == "no_calculated_uplift"
    # Ni=8480*Ai*20*10*.5*tanh(1.732)=170,682.745 N/m.
    # Nc=1850*Ac*400/cosh(1.84)=13,513.325 N/m.
    # dynamic=hypot(Ni,Nc)/.012=14,268,070.785 Pa.
    assert r.impulsive_hoop_force_n_per_m == pytest.approx(170_682.745, abs=.001)
    assert r.convective_hoop_force_n_per_m == pytest.approx(13_513.325, abs=.001)
    assert r.hydrodynamic_hoop_stress_pa == pytest.approx(14_268_070.785, abs=.001)
    assert r.combined_hoop_stress_pa == pytest.approx(95_990_154.118, abs=.001)
    assert r.citations and all(c["type"] == "Citation" for c in r.citations)
    assert asdict(r)["ai_g"] == r.ai_g


@pytest.mark.parametrize("diameter,height", [(30, 10), (10, 20), (1.333*10, 10)])
def test_geometry_branches(diameter, height):
    r = s.api650_annex_e(**inputs(diameter_m=diameter, fill_height_m=height))
    ratio = diameter/height
    expected_wi = (math.tanh(.866*ratio)/(.866*ratio) if ratio >= 1.333
                   else 1-.218*ratio)
    assert r.impulsive_weight_n/r.product_weight_n == pytest.approx(expected_wi)
    expected_xi = .375*height if ratio >= 1.3333 else (.5-.094*ratio)*height
    assert r.xi_m == pytest.approx(expected_xi)
    assert r.xcs_m > r.xc_m
    assert r.xis_m > r.xi_m


def test_force_and_wave_spectra_are_independent():
    a = s.api650_annex_e(**inputs())
    b = s.api650_annex_e(**inputs(rwc=4))
    assert b.ac_g == a.ac_g/2
    assert b.af_g == a.af_g
    long = s.api650_annex_e(**inputs(tl_s=2))
    assert long.ac_g == pytest.approx(1.5*.3*2/a.convective_period_s**2*1.25/2)
    assert long.af_g == a.af_g  # SUG II uses 4 s, not force TL.
    iii = s.api650_annex_e(**inputs(seismic_use_group="III", importance_factor=1.5, tl_s=2))
    assert iii.af_g == pytest.approx(1.5*.3*2/a.convective_period_s**2)
    assert iii.required_freeboard_m == iii.wave_height_m


@pytest.mark.parametrize("group,sds,purchaser,required", [
    ("I", .6, False, False), ("I", .6, True, True),
    ("II", .329, False, False), ("II", .33, False, True),
    ("III", .1, False, True),
])
def test_freeboard_rules(group, sds, purchaser, required):
    r = s.api650_annex_e(**inputs(seismic_use_group=group, sds_g=sds,
                                 purchaser_freeboard=purchaser, available_freeboard_m=0))
    assert (r.required_freeboard_m > 0) is required
    assert r.freeboard_ok is (False if required else None)
    assert r.advisory_freeboard_m > 0


@pytest.mark.parametrize("target,status", [(0.7,"no_calculated_uplift"),
    (.785,"no_calculated_uplift"), (1.0,"uplift_requires_further_checks"),
    (1.54,"uplift_requires_further_checks"), (1.55,"redesign_or_mechanical_anchorage_required")])
def test_j_criteria(target, status):
    r = s.api650_annex_e(**inputs())
    # J=M/[D^2*(Ws/(pi*D)+roof_reaction/(pi*D)+wa)] at Av=0.
    wa = r.ringwall_moment_nm/(20**2*target)-1_200_000/(math.pi*20)
    assert wa > 0
    trial = s.api650_annex_e(**inputs(annular_resistance_n_per_m=wa))
    assert trial.anchorage_ratio_j == pytest.approx(target)
    assert trial.anchorage_status == status


def test_hoop_depth_and_vertical_component():
    top = s.api650_annex_e(**inputs(evaluation_depth_m=0))
    assert top.impulsive_hoop_force_n_per_m == 0
    assert top.hydrostatic_hoop_stress_pa == 0
    assert top.convective_hoop_force_n_per_m > 0
    base = s.api650_annex_e(**inputs())
    vertical = s.api650_annex_e(**inputs(vertical_acceleration_g=.2))
    assert vertical.hydrodynamic_hoop_stress_pa == base.hydrodynamic_hoop_stress_pa
    assert vertical.combined_hoop_stress_pa > base.combined_hoop_stress_pa
    assert vertical.anchorage_ratio_j > base.anchorage_ratio_j
    tall = s.api650_annex_e(**inputs(diameter_m=10, fill_height_m=20))
    assert tall.impulsive_hoop_force_n_per_m == pytest.approx(2600*tall.ai_g*10**2)


def test_units_and_scaling():
    a = s.api650_annex_e(**inputs())
    b = s.api650_annex_e(**inputs(liquid_density_kg_m3=2000))
    assert b.product_weight_n == a.product_weight_n*2  # kg/m3 -> N, gravity once
    assert b.hydrodynamic_hoop_stress_pa == a.hydrodynamic_hoop_stress_pa*2
    thin = s.api650_annex_e(**inputs(shell_thickness_m=.006))
    assert thin.combined_hoop_stress_pa == a.combined_hoop_stress_pa*2
    assert a.hydrostatic_hoop_stress_pa == pytest.approx(1000*9.80665*10*20/(2*.012))


@pytest.mark.parametrize("key,value", [
    ("diameter_m",0), ("fill_height_m",-1), ("liquid_density_kg_m3",0),
    ("shell_thickness_m",0), ("rwi",0), ("rwc",-1), ("k",0),
    ("tl_s",0), ("sds_g",-1), ("sd1_g",float("nan")),
    ("diameter_m",float("inf")), ("minimum_ai_g",-.1),
    ("vertical_acceleration_g",1), ("roof_reaction_n",300_000),
    ("evaluation_depth_m",11), ("annular_resistance_n_per_m",-1),
    ("annular_resistance_n_per_m",40_221), ("bottom_weight_n",-1),
    ("seismic_use_group","IV"), ("purchaser_freeboard","false"),
])
def test_invalid_inputs(key,value):
    with pytest.raises(ValueError):
        s.api650_annex_e(**inputs(**{key:value}))


def test_citation_absence_fails_closed(monkeypatch):
    monkeypatch.setattr(s, "_ANNEX_E_CITATIONS", ())
    with pytest.raises(ValueError, match="citation"):
        s.api650_annex_e(**inputs())


def test_spectrum_short_period_cap_and_floor():
    r = s.api650_annex_e(**inputs(diameter_m=2, fill_height_m=1))
    assert r.convective_period_s < 4
    assert r.af_g == pytest.approx(1.5*.3*1.25/r.convective_period_s)
    capped = s.api650_annex_e(**inputs(sd1_g=100))
    assert capped.ac_g == capped.ai_g
    floor = s.api650_annex_e(**inputs(sds_g=.01, minimum_ai_g=.1))
    assert floor.ai_g == .1


def test_roof_reaction_is_separate_from_roof_inertia():
    shell_supported = s.api650_annex_e(**inputs())
    independent_roof = s.api650_annex_e(**inputs(roof_reaction_n=0))
    assert independent_roof.base_shear_n == shell_supported.base_shear_n
    assert independent_roof.ringwall_moment_nm == shell_supported.ringwall_moment_nm
    assert independent_roof.anchorage_ratio_j > shell_supported.anchorage_ratio_j


def test_tall_tank_hoop_upper_branch_and_slab_height():
    r = s.api650_annex_e(**inputs(diameter_m=10, fill_height_m=20, evaluation_depth_m=2))
    q = 2/(.75*10)
    assert r.impulsive_hoop_force_n_per_m == pytest.approx(5220*r.ai_g*100*(q-.5*q*q))
    shallow = s.api650_annex_e(**inputs(diameter_m=100, fill_height_m=1))
    assert shallow.xcs_m > 1  # effective slab height includes bottom-pressure moment


@pytest.mark.parametrize("key", [k for k,v in inputs().items() if isinstance(v,float)])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_numeric_inputs(key,bad):
    with pytest.raises(ValueError):
        s.api650_annex_e(**inputs(**{key:bad}))


def test_numerical_extremes_and_no_resistance_fail_closed():
    for changes in ({"diameter_m":1e200}, {"diameter_m":1e-300},
                    {"shell_weight_n":0,"roof_reaction_n":0,"annular_resistance_n_per_m":0}):
        with pytest.raises(ValueError):
            s.api650_annex_e(**inputs(**changes))


def test_public_example_and_package_export():
    import runpy
    from pathlib import Path
    from digitalmodel.hydrodynamics import api650_annex_e, AnnexEResult
    example = runpy.run_path(str(Path(__file__).parents[2] /
                                 "examples/hydrodynamics/api650_annex_e.py"))
    result = example["worked_example"]()
    assert isinstance(result, AnnexEResult)
    assert api650_annex_e is s.api650_annex_e
    assert result == s.api650_annex_e(**inputs())


def test_mandatory_zero_wave_is_an_evaluated_zero():
    r = s.api650_annex_e(**inputs(seismic_use_group="III", sd1_g=0))
    assert r.freeboard_required is True
    assert r.required_freeboard_m == 0
    assert r.freeboard_ok is True
    assert r.wave_height_m == 0

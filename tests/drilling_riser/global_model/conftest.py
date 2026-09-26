"""Synthetic drilling-riser global model spec (no project data)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.spec import (
    Contents,
    FlexJoint,
    GlobalEnvironment,
    LineSection,
    RiserGlobalModelSpec,
    TensionRing,
    Tensioners,
    tube_section,
)

E = 207e9


def _tube(name, length, od_in, wall_in, *, mass, vol, drag_in, seg=5.0, cd=1.0, ca=1.0,
          bore=True):
    props = tube_section(od_m=od_in * 0.0254, wall_m=wall_in * 0.0254, youngs_modulus_pa=E)
    return LineSection(
        name=name, length_m=length, segment_length_m=seg, mass_per_m_kg=mass,
        displaced_volume_per_m_m3=vol, bore_id_m=props["id_m"] if bore else 0.0,
        ei_nm2=props["ei_nm2"], ea_n=props["ea_n"], drag_diameter_m=drag_in * 0.0254,
        cd_normal=cd, ca_normal=ca, stress_od_m=od_in * 0.0254, stress_id_m=props["id_m"],
    )


def _rigid(name, length, *, mass, vol, drag_m, seg=1.0):
    return LineSection(
        name=name, length_m=length, segment_length_m=seg, mass_per_m_kg=mass,
        displaced_volume_per_m_m3=vol, bore_id_m=0.0, ei_nm2=1.0e12, ea_n=1.0e12,
        drag_diameter_m=drag_m, cd_normal=1.0, ca_normal=1.0,
    )


def synthetic_spec() -> RiserGlobalModelSpec:
    """A 300 m water-depth riser with the same topology as a deepwater drilling riser."""
    ufj_z = 20.0
    ib = [
        _rigid("UFJ lower body", 2.0, mass=2000.0, vol=0.0, drag_m=1.1),
        _tube("Inner barrel", 12.0, 46.0, 1.0, mass=520.0, vol=math.pi / 4 * (46 * 0.0254) ** 2,
              drag_in=46.0, seg=1.0),
    ]
    ring_z = ufj_z - sum(s.length_m for s in ib)  # 6.0 m above MSL
    riser = [
        _tube("Outer barrel", 18.0, 48.0, 1.0, mass=1500.0, vol=1.3, drag_in=48.0, seg=1.0),
        _tube("Slick joint", 80.0, 21.0, 1.0, mass=840.0, vol=0.50, drag_in=33.0),
        _tube("Buoyant joint", 150.0, 21.0, 0.875, mass=950.0, vol=1.26, drag_in=54.0),
        _tube("Thin slick joint", 50.0, 21.0, 0.875, mass=800.0, vol=0.50, drag_in=33.0),
        _rigid("LFJ upper body", 1.5, mass=2500.0, vol=0.0, drag_m=1.2, seg=0.5),
    ]
    lfj_z = ring_z - sum(s.length_m for s in riser)
    stack = [
        _rigid("LMRP", 7.0, mass=15000.0, vol=2.1, drag_m=5.7),
        _rigid("BOP", 9.0, mass=30000.0, vol=4.0, drag_m=5.7),
    ]
    datum_z = lfj_z - sum(s.length_m for s in stack)
    return RiserGlobalModelSpec(
        name="synthetic-drilling-riser",
        description="synthetic test riser",
        environment=GlobalEnvironment(water_depth_m=-datum_z, water_density_kg_m3=1025.0),
        contents=Contents(density_kg_m3=1500.0, pressure_ref_z_m=ufj_z + 1.0),
        upper_flex_joint=FlexJoint(pivot_z_m=ufj_z, rotational_stiffness_nm_per_rad=1.0e6),
        lower_flex_joint=FlexJoint(pivot_z_m=lfj_z, rotational_stiffness_nm_per_rad=2.5e6),
        tension_ring=TensionRing(mass_kg=30000.0, volume_m3=3.8,
                                 moments_of_inertia_kgm2=(3.0e4, 3.0e4, 6.0e4),
                                 z_static_m=ring_z + 0.2),
        tensioners=Tensioners(count=6, sheave_radius_m=3.75, sheave_z_m=ring_z + 14.0,
                              ring_attach_radius_m=1.8, total_vertical_tension_n=3.0e6),
        inner_barrel=ib, riser=riser, stack=stack, wellhead_datum_z_m=datum_z,
    )


@pytest.fixture
def spec() -> RiserGlobalModelSpec:
    return synthetic_spec()

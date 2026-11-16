"""Neutral illustrative example regression tests against existing methods.

A legacy engineering workbook cites DnV-RP-C201,Section 7.5, eqn7.38,
eqn7.42, eqn7.43, eqn7.44, eqn7.24, eqn7.32 and eqn7.27 and 7.28.
Its edition is unstated. These tests verify formulas, not design qualification.
The existing combined-section method uses one consistent centroid reference;
comparison against an inconsistent source inertia is a private follow-up.
"""

import math

import pytest

from digitalmodel.structural.structural_analysis.models import MaterialProperties, StressState
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry,
    StiffenedPanelGeometry,
    StiffenedPanelBucklingAnalyzer,
)


def example():
    """Invented illustrative example, mm/MPa; no value is taken from a source case."""
    material = MaterialProperties(285, 440, 190000, 0.28, 7800, 'illustrative example')
    panel = StiffenedPanelGeometry(2800, 16, StiffenerGeometry(410, 13, 145, 18, 850), 3100)
    return StiffenedPanelBucklingAnalyzer(material), panel


def test_existing_section_against_component_integrals():
    analyzer, panel = example()
    areas = (850*16, 410*13, 145*18)
    centroids = (8, 16+410/2, 16+410+18/2)
    area = sum(areas)
    centroid = sum(a*y for a,y in zip(areas, centroids))/area
    inertia = (850*16**3+13*410**3+145*18**3)/12
    inertia += sum(a*(y-centroid)**2 for a,y in zip(areas, centroids))
    result = analyzer.effective_section(panel)
    assert result['area_total'] == area
    assert result['neutral_axis_outer'] == pytest.approx(centroid)
    assert result['neutral_axis'] == pytest.approx(centroid-8)
    assert result['I'] == pytest.approx(inertia)
    assert result['r'] == pytest.approx(math.sqrt(inertia/area))


def test_existing_plate_elastic_references():
    analyzer, panel = example()
    base = 190000*(16/850)**2
    assert analyzer.plate_elastic_reference(panel) == pytest.approx({'fEpx': 3.62*base, 'fEpy': 0.9*base, 'fEpt': 5*base})


def test_existing_equivalent_stress_and_euler_load():
    analyzer, panel = example()
    assert StressState(sigma_x=60, sigma_y=25, tau_xy=8).von_mises() == pytest.approx(math.sqrt(60**2+25**2-60*25+3*8**2))
    assert StressState(sigma_x=60).von_mises() == 60
    section = analyzer.effective_section(panel)
    assert analyzer.column_analyzer.euler_buckling_load(section['I'], 2800) == pytest.approx(math.pi**2*190000*section['I']/2800**2)


def test_existing_torsion_against_independent_stress_interaction():
    analyzer, panel = example()
    E, fy, nu = 190000, 285, 0.28
    sx, sy, tau = 60, 25, 8
    elastic = (3.62*E*(16/850)**2, 0.9*E*(16/850)**2, 5*E*(16/850)**2)
    sj = math.sqrt(sx*sx+sy*sy-sx*sy+3*tau*tau)
    c = 2-850/2800
    lam_e = fy/sj*sum((stress/ref)**c for stress,ref in zip((sx,sy,tau),elastic))**(1/c)
    fep = fy/math.sqrt(1+lam_e**2)
    C = 410/850*(16/13)**3*math.sqrt(max(0,1-sj/fep))
    beta = (3*C+0.2)/(C+0.2)
    Aw, Af = 410*13, 145*18
    Iz = 18*145**3/12
    fet = (Aw+(18/13)**2*Af)/(Aw+3*Af)*beta*E/(2*(1+nu))*(13/410)**2
    fet += math.pi**2*E*Iz/((Aw/3+Af)*3100**2)
    lam = math.sqrt(fy/fet)
    mu = 0.35*(lam-0.6)
    phi = (1+mu+lam**2)/2
    ft = fy if lam<=0.6 else fy/(phi+math.sqrt(phi**2-lam**2))
    result = analyzer.torsional_buckling(panel,sx,sy,tau,gamma_m=1.2)
    assert result['beta'] == pytest.approx(beta)
    assert result['fET'] == pytest.approx(fet)
    assert result['lambda_T'] == pytest.approx(lam)
    assert result['fT'] == pytest.approx(ft)
    assert result['utilization'] == pytest.approx(sx*1.2/ft)


def test_existing_section_type_validation():
    with pytest.raises(ValueError, match='section_type'):
        StiffenerGeometry(410, 13, 145, 18, 850, 'unsupported')

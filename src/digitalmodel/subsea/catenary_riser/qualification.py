# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Route contracts for static screening; no native-model qualification is implied."""


def route_conventions() -> list[dict[str, str]]:
    """Return independent records; legacy export axes are deliberately unqualified.

    Analytical basis: Catenary Curve (2019), equations (1), (9), (10), (14).
    There is no time or phase variable in a static catenary calculation.
    """
    common = {
        "units": "m, N, N/m, degrees",
        "axes": "x toward hang-off; z upward; static, phase inapplicable",
        "origin": "horizontal-tangent touchdown vertex",
        "force_sign": "positive tension magnitude; positive weight downward",
        "angle": "top from vertical; touchdown/distribution from horizontal",
        "status": "TESTED",
        "evidence": "test_route_qualification.py::test_modern_vertex_and_distributions",
    }
    routes = []
    for name in ("simple_H", "simple_top_tension", "simple_span"):
        routes.append(dict(common, route=name))
    for name in ("legacy_force", "legacy_angle"):
        routes.append(dict(
            common, route=name, angle="q from vertical; no distribution returned",
            evidence="test_route_qualification.py::test_legacy_vertex_overlap",
        ))
    routes.append(dict(
        common, route="lazy_wave_analyzer", status="QUARANTINED",
        axes="no geometry returned", origin="no geometry returned",
        evidence="test_lazy_wave_quarantine.py::test_analyzer_refuses_rather_than_fabricating",
    ))
    routes.append(dict(
        common, route="legacy_exports", status="NOT_EVALUATED",
        origin="export-specific; no shared global datum qualified",
        axes="export-specific; native axes and phase require separate evidence",
        evidence="docs/qualification.json; missing type data and native evidence",
    ))
    routes.append(dict(
        common, route="installation_sections",
        evidence="test_installation_planning.py::test_piecewise_hand_case",
    ))
    return routes

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
        "axes": "x toward hang-off; z upward",
        "phase": "NOT_APPLICABLE; static calculation",
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
            common, route=name, angle=(
                "not returned; q is retained input only when F, w and d are supplied"
                if name == "legacy_force"
                else "q input from vertical; no distribution returned"
            ),
            evidence="test_route_qualification.py::test_legacy_vertex_overlap",
        ))
    routes.append(dict(
        common, route="lazy_wave_analyzer", status="QUARANTINED",
        axes="no geometry returned", origin="no geometry returned",
        evidence="test_lazy_wave_quarantine.py::test_analyzer_refuses_rather_than_fabricating",
    ))
    # Native exports cannot inherit a static analytical frame or SI force units.
    routes.append({
        "route": "legacy_exports",
        "status": "NOT_EVALUATED",
        "units": "NOT_EVALUATED; target model unit system required",
        "origin": "NOT_EVALUATED; export-specific connection datums",
        "axes": "NOT_EVALUATED; connection frames require native evidence",
        "phase": "NOT_EVALUATED; embedded RAO conventions require provenance",
        "angle": "NOT_EVALUATED; native end orientations require separate evidence",
        "force_sign": "NOT_EVALUATED; native result conventions require separate evidence",
        "evidence": "docs/qualification.json; missing type data and native evidence",
    })
    routes.append(dict(
        common, route="legacy_simple_plot", units="m",
        origin="X=0 at hang-off; Y=0 at mean sea level",
        axes="X toward touchdown; Y upward",
        angle="not returned", force_sign="not returned",
        evidence=(
            "test_route_qualification.py::test_legacy_simple_plot_matches_modern_vertex_frame; "
            "requires legacy_angle result with BendRadius; legacy_force output raises KeyError"
        ),
    ))
    routes.append(dict(
        common, route="installation_sections",
        evidence="test_installation_planning.py::test_piecewise_hand_case",
    ))
    return routes

"""Reasoned classifications used by the generated HTML freshness census.

This census is scoped to ``docs/api/**`` and it is **no longer the only
census** (#1961).

Two holes were found in it by manual sweep rather than by any check. One
leaking page sat inside ``docs/api/**`` but inside ``PAGE_EXCLUSIONS``, which
means a producer exists but is not deterministically reproducible -- so the
drift gate never regenerates it and no PR that hand-edits it fails. A second,
near-duplicate page sat outside ``docs/api/**`` entirely and was therefore
outside this census altogether.

``scripts/legal/check_protected_identifiers.py`` covers the complete tracked
tree returned by ``git ls-files``, with every path classified and an
unanticipated path scanned rather than skipped, so a leak cannot survive by
living somewhere this file does not look. A named subset of generated artifacts
is additionally digested by ``scripts/legal/public_surface_snapshot.py``, so a
hand-edit to one of them moves the public-surface snapshot even when nothing
regenerates it; that subset is declared in
``scripts/legal/protected-surface-v1.json`` under ``generated_census`` and is
deliberately not limited to ``docs/api/**``.

Nothing here changes what this census does. It records what it does *not* do,
so the next reader does not mistake its scope for coverage.
"""

DISCOVERY_FALSE_POSITIVES = {
    "scripts/benchmark/compare_hydro_properties.py": (
        "writes a user-selected benchmark report, not a committed docs/api page"
    ),
    "scripts/benchmark/qtf_postprocessing.py": (
        "writes OrcaWave benchmark plots under docs/modules, not docs/api"
    ),
    "scripts/benchmark/run_spar_benchmark.py": (
        "orchestrates a benchmark report under docs/domains, not docs/api"
    ),
    "scripts/benchmark/run_validation_benchmark.py": (
        "orchestrates benchmark reports outside docs/api"
    ),
    "scripts/benchmark/solver_metadata.py": (
        "provides report metadata but does not write docs/api HTML"
    ),
    "scripts/benchmark/validate_owd_vs_spec.py": (
        "writes validation reports outside docs/api; docs/api HTML is input only"
    ),
    "scripts/brand_guard.py": (
        "reads docs/api HTML to enforce brand tokens; it does not write pages"
    ),
    "scripts/benchmark_model_library.py": (
        "writes model-library reports under outputs and docs/domains, not docs/api"
    ),
    "scripts/capabilities/build_capabilities_inventory.py": (
        "writes JSON and Markdown; it only discovers existing docs/api HTML"
    ),
    "scripts/check_generated_html.py": (
        "executes registered generators; it does not emit a docs/api HTML page"
    ),
    "scripts/consolidate_docs.py": (
        "moves legacy docs content into docs/domains; it does not generate docs/api HTML"
    ),
    "scripts/debug_html_report.py": (
        "debugs an existing docs/reports HTML file; it does not target docs/api"
    ),
    "scripts/generated_html_ownership.py": (
        "contains this ownership census; it does not write HTML"
    ),
    "scripts/ingest_orcina_help.py": (
        "ingests Orcina help as Markdown under docs/domains, not docs/api HTML"
    ),
    "scripts/inventory/orcaflex_legacy_inventory.py": (
        "writes a Markdown inventory and only classifies HTML-producing scripts"
    ),
    "scripts/python/digitalmodel/legacy/generate_integration_charts.py": (
        "writes legacy charts under docs/charts, not docs/api"
    ),
    "scripts/python/digitalmodel/analysis/poisson.py": (
        "mentions documentation and an API example but does not write HTML"
    ),
    "scripts/semantic_validate.py": (
        "writes caller-selected validation reports, not committed docs/api pages"
    ),
    "docs/api/cfd/report_build/house.py": (
        "shared HTML rendering helpers; it does not write a page"
    ),
}

# Active generators which cannot run from repository-owned deterministic inputs.
EXCLUDED_GENERATORS = {
    "scripts/cfd/generate-kcs-verification.py": (
        "requires a caller-supplied solved OpenFOAM case"
    ),
    "docs/api/cfd/report_build/build_dam_break.py": (
        "requires a caller-supplied solved OpenFOAM case"
    ),
    "docs/api/cfd/report_build/build_decay_report.py": (
        "requires a caller-supplied decay-results snapshot"
    ),
    "docs/api/cfd/report_build/build_floating_body.py": (
        "requires caller-supplied solved case and results directories"
    ),
    "docs/api/cfd/report_build/build_kleefsman.py": (
        "requires caller-supplied solved case and experimental-data paths"
    ),
    "docs/api/cfd/report_build/build_wave_excited_body.py": (
        "requires caller-supplied solved case and results directories"
    ),
    "docs/api/cfd/report_build/build_wave_excited_body_rao.py": (
        "requires a caller-supplied RAO sweep-results snapshot"
    ),
    "docs/api/cfd/report_build/build_wave_tank.py": (
        "requires caller-supplied solved case and results directories"
    ),
    "docs/api/cfd/report_build/upgrade_reports.py": (
        "requires four caller-supplied solved runs and is intentionally non-idempotent"
    ),
}

# Generated-looking pages which cannot yet be reproduced deterministically.
PAGE_EXCLUSIONS = {
    "docs/api/cfd/cylinder-re100-verification.html": (
        "upgrade_reports.py requires a caller-supplied solved cylinder run"
    ),
    "docs/api/cfd/dam-break-verification.html": (
        "build_dam_break.py requires a caller-supplied solved OpenFOAM case"
    ),
    "docs/api/cfd/flat-plate-blasius-verification.html": (
        "upgrade_reports.py requires a caller-supplied solved flat-plate run"
    ),
    "docs/api/cfd/floating-body-decay-verification.html": (
        "build_floating_body.py requires caller-supplied solved case and results"
    ),
    "docs/api/cfd/kleefsman-impact-verification.html": (
        "build_kleefsman.py requires caller-supplied solved case and experiment data"
    ),
    "docs/api/cfd/naca0012-airfoil-verification.html": (
        "upgrade_reports.py requires a caller-supplied solved airfoil run"
    ),
    "docs/api/cfd/turbulent-flat-plate-verification.html": (
        "upgrade_reports.py requires a caller-supplied solved turbulent-plate run"
    ),
    "docs/api/cfd/wave-excited-body-decay-verification.html": (
        "build_decay_report.py requires a caller-supplied decay-results snapshot"
    ),
    "docs/api/cfd/wave-excited-body-rao-verification.html": (
        "build_wave_excited_body_rao.py requires a caller-supplied sweep snapshot"
    ),
    "docs/api/cfd/wave-excited-body-verification.html": (
        "build_wave_excited_body.py requires caller-supplied solved case and results"
    ),
    "docs/api/cfd/wave-tank-verification.html": (
        "build_wave_tank.py requires caller-supplied solved case and results"
    ),
    "docs/api/hydro/ocimf-coefficient-explorer.html": (
        "producer needs a caller-supplied workbook and emits random Plotly div ids"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_amplitude.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_combined.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_heatmap.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_phase.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_report.html": (
        "no active producer targets this committed copy; output embeds a timestamp"
    ),
    "docs/api/hydro/rao-comparison/index.html": (
        "producer targets another docs path and embeds datetime.now()"
    ),
    "docs/api/hydro/passing-ship-benchmark.html": (
        "producer targets another docs path and embeds datetime.now()"
    ),
    "docs/api/orcaflex/riser-mesh-sensitivity.html": (
        "licensed OrcFxAPI benchmark output embeds a timestamp"
    ),
    "docs/api/orcaflex/riser-validation-report.html": (
        "licensed OrcFxAPI benchmark output embeds a timestamp"
    ),
}

# Every remaining HTML page is classified explicitly. This page census, rather
# than source-text heuristics alone, prevents new docs/api output from silently
# bypassing ownership.
MANUAL_PAGES = {
    "docs/api/cfd/kcs-calm-water-resistance-verification.html": (
        "emitted from a solved case by scripts/cfd/generate-kcs-verification.py; "
        "the solve output is not in the repo, so CI cannot regenerate it"
    ),
    "docs/api/buckling/ship-panel-buckling.html": (
        "hand-maintained interactive report; no scripts producer found"
    ),
    "docs/api/buckling/ship-plate-buckling.html": (
        "hand-maintained interactive report; no scripts producer found"
    ),
    "docs/api/capabilities/index.html": (
        "hand-maintained capabilities portal"
    ),
    "docs/api/cfd/index.html": "hand-maintained CFD verification index",
    "docs/api/cfd/tank-sloshing-verification.html": (
        "committed verification report; no scripts producer found"
    ),
    "docs/api/ffs/build-wave-2026-07.html": (
        "hand-maintained build-wave status page"
    ),
    "docs/api/ffs/ffs-field-dashboard.html": (
        "hand-maintained field dashboard; no scripts producer found"
    ),
    "docs/api/ffs/ffs-showcase.html": (
        "hand-maintained showcase; no scripts producer found"
    ),
    "docs/api/hydro/rudder-maneuvering-explorer.html": (
        "hand-maintained explorer; no scripts producer found"
    ),
    "docs/api/hydro/short-horizon-motion-forecast.html": (
        "hand-maintained explorer; no scripts producer found"
    ),
    "docs/api/subsea/offshore-cross-section-report.html": (
        "hand-maintained report; no scripts producer found"
    ),
}

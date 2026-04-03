"""Count test functions per test file using AST."""
import ast
import sys
import os

# All test files to analyze
test_files = [
    # tests/orcawave/
    "tests/orcawave/test_wave_spectrum.py",
    "tests/orcawave/test_vessel_database.py",
    "tests/orcawave/test_spec_validation.py",
    "tests/orcawave/test_report_builder.py",
    "tests/orcawave/test_rao_processing.py",
    "tests/orcawave/test_panel_mesh.py",
    "tests/orcawave/test_motion_statistics.py",
    "tests/orcawave/test_hydro_coefficients.py",
    "tests/orcawave/test_drift_forces.py",
    "tests/orcawave/test_config_models.py",
    # tests/hydrodynamics/diffraction/
    "tests/hydrodynamics/diffraction/test_multi_solver_comparator.py",
    "tests/hydrodynamics/diffraction/test_module_boundary.py",
    "tests/hydrodynamics/diffraction/test_mesh_pipeline.py",
    "tests/hydrodynamics/diffraction/test_input_schemas.py",
    "tests/hydrodynamics/diffraction/test_gmsh_mesh_builder.py",
    "tests/hydrodynamics/diffraction/test_diffraction_units.py",
    "tests/hydrodynamics/diffraction/test_cli_integration.py",
    "tests/hydrodynamics/diffraction/test_benchmark_runner.py",
    "tests/hydrodynamics/diffraction/test_benchmark_plotter.py",
    "tests/hydrodynamics/diffraction/test_aqwa_runner.py",
    "tests/hydrodynamics/diffraction/test_aqwa_result_extractor.py",
    "tests/hydrodynamics/diffraction/test_aqwa_parser.py",
    "tests/hydrodynamics/diffraction/test_aqwa_batch_runner.py",
    "tests/hydrodynamics/diffraction/test_aqwa_backend_damping.py",
    "tests/hydrodynamics/diffraction/test_aqwa_backend.py",
    "tests/hydrodynamics/diffraction/test_aqwa_ah1_parser.py",
    "tests/hydrodynamics/diffraction/test_wrk5091_mesh_cli_exports.py",
    "tests/hydrodynamics/diffraction/test_wamit_reference_loader.py",
    "tests/hydrodynamics/diffraction/test_unit_box_benchmark.py",
    "tests/hydrodynamics/diffraction/test_spec_converter.py",
    "tests/hydrodynamics/diffraction/test_solver_smoke_unit.py",
    "tests/hydrodynamics/diffraction/test_solver_fixtures.py",
    "tests/hydrodynamics/diffraction/test_reverse_parsers.py",
    "tests/hydrodynamics/diffraction/test_result_extractor.py",
    "tests/hydrodynamics/diffraction/test_report_generator.py",
    "tests/hydrodynamics/diffraction/test_rao_plotter.py",
    "tests/hydrodynamics/diffraction/test_polars_exporter_additional.py",
    "tests/hydrodynamics/diffraction/test_polars_exporter.py",
    "tests/hydrodynamics/diffraction/test_output_validator_resonance.py",
    "tests/hydrodynamics/diffraction/test_orcawave_runner.py",
    "tests/hydrodynamics/diffraction/test_orcawave_batch_runner.py",
    "tests/hydrodynamics/diffraction/test_orcawave_backend.py",
    # Other relevant test files
    "tests/solver/test_diffraction_pipeline_e2e.py",
    "tests/solver/smoke_test.py",
    "tests/solvers/orcawave/diffraction/test_setup.py",
    "tests/workflows/orcawave/test_integration.py",
    "tests/workflows/orcawave/test_end_to_end.py",
    "tests/workflows/orcawave/test_com_connection.py",
    "tests/specialized/cli/test_diffraction_cli.py",
]

base = "/mnt/local-analysis/workspace-hub/digitalmodel"
total = 0
results = []

for tf in test_files:
    fpath = os.path.join(base, tf)
    if not os.path.isfile(fpath):
        results.append(f"  {tf}: FILE NOT FOUND")
        continue
    try:
        with open(fpath) as f:
            tree = ast.parse(f.read(), filename=fpath)
        count = 0
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef) and node.name.startswith("test_"):
                count += 1
            elif isinstance(node, ast.AsyncFunctionDef) and node.name.startswith("test_"):
                count += 1
        total += count
        results.append(f"  {tf}: {count} test functions")
    except Exception as e:
        results.append(f"  {tf}: PARSE ERROR: {e}")

print("TEST FUNCTION COUNTS PER FILE:")
for r in results:
    print(r)
print(f"\nTOTAL TEST FUNCTIONS: {total}")

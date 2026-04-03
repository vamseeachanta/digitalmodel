"""Map source files to test files and identify gaps."""
import os

base = "/mnt/local-analysis/workspace-hub/digitalmodel"

# ---- Source files: orcawave module ----
orcawave_src = [
    "src/digitalmodel/orcawave/__init__.py",
    "src/digitalmodel/orcawave/motion_statistics.py",
    "src/digitalmodel/orcawave/hydro_coefficients.py",
    "src/digitalmodel/orcawave/drift_forces.py",
    "src/digitalmodel/orcawave/panel_mesh.py",
    "src/digitalmodel/orcawave/rao_processing.py",
    "src/digitalmodel/orcawave/wave_spectrum.py",
    "src/digitalmodel/orcawave/vessel_database.py",
    "src/digitalmodel/orcawave/reporting/__init__.py",
    "src/digitalmodel/orcawave/reporting/builder.py",
    "src/digitalmodel/orcawave/reporting/config.py",
    "src/digitalmodel/orcawave/reporting/sections/__init__.py",
    "src/digitalmodel/orcawave/reporting/sections/rao_plots.py",
    "src/digitalmodel/orcawave/reporting/sections/qtf_heatmap.py",
    "src/digitalmodel/orcawave/reporting/sections/qa_summary.py",
    "src/digitalmodel/orcawave/reporting/sections/panel_pressures.py",
    "src/digitalmodel/orcawave/reporting/sections/multi_body.py",
    "src/digitalmodel/orcawave/reporting/sections/model_summary.py",
    "src/digitalmodel/orcawave/reporting/sections/mean_drift.py",
    "src/digitalmodel/orcawave/reporting/sections/hydro_matrices.py",
]

# ---- Source files: diffraction module ----
diffraction_src = [
    "src/digitalmodel/hydrodynamics/diffraction/__init__.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_ah1_parser.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_batch_runner.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_result_extractor.py",
    "src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py",
    "src/digitalmodel/hydrodynamics/diffraction/batch_processor.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_correlation.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_dof_sections.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_dof_tables.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_helpers.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_input_comparison.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_input_files.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_input_reports.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_plotter.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_rao_helpers.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_rao_plots.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_rao_summary.py",
    "src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py",
    "src/digitalmodel/hydrodynamics/diffraction/cli.py",
    "src/digitalmodel/hydrodynamics/diffraction/comparison_framework.py",
    "src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py",
    "src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py",
    "src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py",
    "src/digitalmodel/hydrodynamics/diffraction/gmsh_mesh_builder.py",
    "src/digitalmodel/hydrodynamics/diffraction/input_schemas.py",
    "src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py",
    "src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py",
    "src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py",
    "src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py",
    "src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py",
    "src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py",
    "src/digitalmodel/hydrodynamics/diffraction/orcawave_test_utilities.py",
    "src/digitalmodel/hydrodynamics/diffraction/output_schemas.py",
    "src/digitalmodel/hydrodynamics/diffraction/output_validator.py",
    "src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py",
    "src/digitalmodel/hydrodynamics/diffraction/rao_plotter.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_builders.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_builders_header.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_builders_hydrostatics.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_builders_responses.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_computations.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_data_models.py",
    "src/digitalmodel/hydrodynamics/diffraction/report_generator.py",
    "src/digitalmodel/hydrodynamics/diffraction/result_extractor.py",
    "src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py",
    "src/digitalmodel/hydrodynamics/diffraction/spec_converter.py",
    # solver subpackage
    "src/digitalmodel/hydrodynamics/diffraction/solver/__init__.py",
    "src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py",
    "src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py",
    "src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py",
]

# ---- All test files (from search) ----
orcawave_tests = {
    "test_wave_spectrum": "tests/orcawave/test_wave_spectrum.py",
    "test_vessel_database": "tests/orcawave/test_vessel_database.py",
    "test_spec_validation": "tests/orcawave/test_spec_validation.py",
    "test_report_builder": "tests/orcawave/test_report_builder.py",
    "test_rao_processing": "tests/orcawave/test_rao_processing.py",
    "test_panel_mesh": "tests/orcawave/test_panel_mesh.py",
    "test_motion_statistics": "tests/orcawave/test_motion_statistics.py",
    "test_hydro_coefficients": "tests/orcawave/test_hydro_coefficients.py",
    "test_drift_forces": "tests/orcawave/test_drift_forces.py",
    "test_config_models": "tests/orcawave/test_config_models.py",
}

diffraction_tests = {
    "test_multi_solver_comparator": "tests/hydrodynamics/diffraction/test_multi_solver_comparator.py",
    "test_module_boundary": "tests/hydrodynamics/diffraction/test_module_boundary.py",
    "test_mesh_pipeline": "tests/hydrodynamics/diffraction/test_mesh_pipeline.py",
    "test_input_schemas": "tests/hydrodynamics/diffraction/test_input_schemas.py",
    "test_gmsh_mesh_builder": "tests/hydrodynamics/diffraction/test_gmsh_mesh_builder.py",
    "test_diffraction_units": "tests/hydrodynamics/diffraction/test_diffraction_units.py",
    "test_cli_integration": "tests/hydrodynamics/diffraction/test_cli_integration.py",
    "test_benchmark_runner": "tests/hydrodynamics/diffraction/test_benchmark_runner.py",
    "test_benchmark_plotter": "tests/hydrodynamics/diffraction/test_benchmark_plotter.py",
    "test_aqwa_runner": "tests/hydrodynamics/diffraction/test_aqwa_runner.py",
    "test_aqwa_result_extractor": "tests/hydrodynamics/diffraction/test_aqwa_result_extractor.py",
    "test_aqwa_parser": "tests/hydrodynamics/diffraction/test_aqwa_parser.py",
    "test_aqwa_batch_runner": "tests/hydrodynamics/diffraction/test_aqwa_batch_runner.py",
    "test_aqwa_backend_damping": "tests/hydrodynamics/diffraction/test_aqwa_backend_damping.py",
    "test_aqwa_backend": "tests/hydrodynamics/diffraction/test_aqwa_backend.py",
    "test_aqwa_ah1_parser": "tests/hydrodynamics/diffraction/test_aqwa_ah1_parser.py",
    "test_wrk5091_mesh_cli_exports": "tests/hydrodynamics/diffraction/test_wrk5091_mesh_cli_exports.py",
    "test_wamit_reference_loader": "tests/hydrodynamics/diffraction/test_wamit_reference_loader.py",
    "test_unit_box_benchmark": "tests/hydrodynamics/diffraction/test_unit_box_benchmark.py",
    "test_spec_converter": "tests/hydrodynamics/diffraction/test_spec_converter.py",
    "test_solver_smoke_unit": "tests/hydrodynamics/diffraction/test_solver_smoke_unit.py",
    "test_solver_fixtures": "tests/hydrodynamics/diffraction/test_solver_fixtures.py",
    "test_reverse_parsers": "tests/hydrodynamics/diffraction/test_reverse_parsers.py",
    "test_result_extractor": "tests/hydrodynamics/diffraction/test_result_extractor.py",
    "test_report_generator": "tests/hydrodynamics/diffraction/test_report_generator.py",
    "test_rao_plotter": "tests/hydrodynamics/diffraction/test_rao_plotter.py",
    "test_polars_exporter_additional": "tests/hydrodynamics/diffraction/test_polars_exporter_additional.py",
    "test_polars_exporter": "tests/hydrodynamics/diffraction/test_polars_exporter.py",
    "test_output_validator_resonance": "tests/hydrodynamics/diffraction/test_output_validator_resonance.py",
    "test_orcawave_runner": "tests/hydrodynamics/diffraction/test_orcawave_runner.py",
    "test_orcawave_batch_runner": "tests/hydrodynamics/diffraction/test_orcawave_batch_runner.py",
    "test_orcawave_backend": "tests/hydrodynamics/diffraction/test_orcawave_backend.py",
}

# Map source -> test file name pattern
def source_stem(path):
    return os.path.basename(path).replace(".py", "")

# Build mapping for orcawave
print("=" * 80)
print("ORCAWAVE SOURCE FILE COVERAGE MAPPING")
print("=" * 80)
covered_ow = []
uncovered_ow = []
for sf in orcawave_src:
    stem = source_stem(sf)
    if stem == "__init__":
        # __init__ files don't need dedicated tests
        continue
    # Check for matching test
    test_key = f"test_{stem}"
    if test_key in orcawave_tests:
        covered_ow.append((sf, orcawave_tests[test_key]))
        print(f"  [COVERED]   {sf}")
        print(f"              -> {orcawave_tests[test_key]}")
    else:
        uncovered_ow.append(sf)
        print(f"  [NO TEST]   {sf}")

print()
print("=" * 80)
print("DIFFRACTION SOURCE FILE COVERAGE MAPPING")
print("=" * 80)
covered_diff = []
uncovered_diff = []
for sf in diffraction_src:
    stem = source_stem(sf)
    if stem == "__init__":
        continue
    test_key = f"test_{stem}"
    if test_key in diffraction_tests:
        covered_diff.append((sf, diffraction_tests[test_key]))
        print(f"  [COVERED]   {sf}")
        print(f"              -> {diffraction_tests[test_key]}")
    else:
        uncovered_diff.append(sf)
        print(f"  [NO TEST]   {sf}")

print()
print("=" * 80)
print("SUMMARY")
print("=" * 80)
total_src = len([s for s in orcawave_src + diffraction_src if source_stem(s) != "__init__"])
total_covered = len(covered_ow) + len(covered_diff)
total_uncovered = len(uncovered_ow) + len(uncovered_diff)
print(f"  Total non-__init__ source files: {total_src}")
print(f"  Source files WITH matching test:  {total_covered}")
print(f"  Source files WITHOUT test:        {total_uncovered}")
print(f"  Coverage ratio (by file):         {total_covered}/{total_src} = {100*total_covered/total_src:.1f}%")
print()
print("UNCOVERED SOURCE FILES (orcawave):")
for s in uncovered_ow:
    print(f"  - {s}")
print()
print("UNCOVERED SOURCE FILES (diffraction):")
for s in uncovered_diff:
    print(f"  - {s}")

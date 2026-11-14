"""License-free diffraction provenance integration regressions (#1019)."""

import hashlib

import pytest

from digitalmodel.hydrodynamics.diffraction import report_generator as rg
from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
)
from digitalmodel.reporting import Provenance, ProvenanceError
from tests.hydrodynamics.diffraction.test_report_generator_golden import (
    _full_basic,
)


def test_generator_appends_provenance_after_appendices(tmp_path):
    data, kwargs = _full_basic()
    prov = Provenance(
        code_edition="test edition",
        units_system="SI",
        validated_path="screening",
    ).add("file", "results.json")
    out = rg.generate_diffraction_report(
        data, tmp_path / "report.html", provenance=prov, **kwargs
    )
    doc = out.read_text()
    assert doc.index("Appendices") < doc.index('id="provenance"')
    assert "test edition" in doc and "results.json" in doc and "screening" in doc


def test_generator_rejects_incomplete_provenance_before_writing(tmp_path):
    data, kwargs = _full_basic()
    out = tmp_path / "report.html"
    with pytest.raises(ProvenanceError):
        rg.generate_diffraction_report(
            data,
            out,
            provenance=Provenance().add("file", "results.json"),
            **kwargs,
        )
    assert not out.exists()


def test_data_bearing_sections_declare_reads():
    metadata_only = {"toc", "phase_guide", "appendices"}
    for section in rg._DIFFRACTION_BACKBONE.sections:
        if section.key not in metadata_only:
            assert section.reads, section.key


def test_benchmark_declares_real_result_files(
    tmp_path, three_solver_results, monkeypatch
):
    seen = {}
    original = rg.generate_diffraction_report

    def capture(*args, **kwargs):
        seen["provenance"] = kwargs.get("provenance")
        return original(*args, **kwargs)

    monkeypatch.setattr(rg, "generate_diffraction_report", capture)
    result = BenchmarkRunner(
        BenchmarkConfig(output_dir=tmp_path, dry_run=True)
    ).run_from_results(three_solver_results)
    assert result.success, result.error_message
    prov = seen["provenance"]
    assert prov is not None and prov.is_complete()
    assert prov.validated_path == "screening"
    for path in (result.report_json_path, result.hydro_data_yaml_path):
        source = next(s for s in prov.sources if s.identifier == path.name)
        assert (
            source.digest == "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
        )
    assert 'id="provenance"' in result.report_html_path.read_text()


def test_owr_wrapper_threads_source_without_loading_solver(tmp_path, monkeypatch):
    source = tmp_path / "run.owr"
    source.write_bytes(b"fixture, never loaded by a solver")
    data, _ = _full_basic()
    monkeypatch.setattr(rg, "extract_report_data_from_owr", lambda path: data)
    out = rg.generate_report_from_owr(source)
    doc = out.read_text()
    assert 'id="provenance"' in doc and source.name in doc
    assert hashlib.sha256(source.read_bytes()).hexdigest() in doc


def test_structured_report_rejects_incomplete_provenance(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.report_catalog import (
        render_structured_report,
    )

    data, _ = _full_basic()
    out = tmp_path / "structured.html"
    with pytest.raises(ProvenanceError):
        render_structured_report(
            "barge",
            data,
            provenance=Provenance().add("file", "results.json"),
            output_path=out,
        )
    assert not out.exists()


def test_postprocess_threads_the_input_bundle(tmp_path, mock_diffraction_results):
    import json

    from digitalmodel.hydrodynamics.diffraction.postprocess import (
        postprocess_diffraction_run,
    )

    bundle = tmp_path / "bundle"
    bundle.mkdir()
    source = bundle / "diffraction_results.json"
    source.write_text(json.dumps(mock_diffraction_results.to_dict()))
    result = postprocess_diffraction_run(bundle, tmp_path / "out", make_plots=False)
    assert result.report is not None
    doc = result.report.read_text()
    assert 'id="provenance"' in doc and source.name in doc
    assert hashlib.sha256(source.read_bytes()).hexdigest() in doc


def test_structured_report_checks_declared_result_source(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.report_catalog import (
        render_structured_report,
    )
    from digitalmodel.reporting import ContractError

    data, _ = _full_basic()
    data.source_file = "actual-results.json"
    prov = Provenance(code_edition="test", validated_path="screening").add(
        "file", "different-results.json"
    )
    with pytest.raises(ContractError, match="actual-results.json"):
        render_structured_report("barge", data, provenance=prov)


def test_source_stamp_does_not_fabricate_digest_or_validation(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.report_provenance import (
        result_provenance,
    )

    prov = result_provenance([tmp_path / "unavailable.owr"], units_system="native")
    assert prov.is_complete() and prov.validated_path == "screening"
    assert prov.units_system == "native" and prov.sources[0].digest is None
    assert prov.sources[0].identifier == "unavailable.owr"

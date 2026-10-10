"""R02 transfer citations: synthetic frontmatter fixtures, no standard text."""
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.naval_architecture.friction_scaling import (
    transfer_model_to_ship, transfer_ship_to_model,
)

CIT_ROOT = Path(__file__).resolve().parent.parent / "citations" / "fixtures"


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_transfer_resolves_procedure_sidecar(transfer, coefficient):
    result = transfer(**coefficient, re_model=1e7, re_ship=1e9,
                      form_factor_k=0.2, fn_model=0.25, fn_ship=0.25,
                      repo_root=CIT_ROOT, cite="strict")
    assert {c.code_id for c in result.citations} == {"EN400", "ITTC-7.5-02-03-01.4"}
    assert result.unresolved_citations == ()
    procedure = next(c for c in result.citations if c.publisher == "ITTC")
    assert procedure.revision == "2021 Revision 05"
    assert "2.3" in procedure.section and "2.4.1" in procedure.section


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_transfer_missing_procedure_page_fails_closed(tmp_path, transfer, coefficient):
    import shutil
    shutil.copytree(CIT_ROOT / "knowledge", tmp_path / "knowledge")
    target = tmp_path / "knowledge/wikis/marine-engineering/wiki/standards/ittc-1978-performance-prediction.md"
    assert target.is_file(), "procedure fixture must exist before the missing-page probe"
    target.unlink()
    with pytest.raises(CitationResolutionError) as exc:
        transfer(**coefficient, re_model=1e7, re_ship=1e9,
                 form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, repo_root=tmp_path, cite="strict")
    assert exc.value.code_id == "ITTC-7.5-02-03-01.4"
    assert exc.value.reason == "page_missing"


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_transfer_unconfigured_resolver_fails_closed(monkeypatch, transfer, coefficient):
    from digitalmodel.citations import resolver
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
    def unconfigured(*args, **kwargs):
        raise CitationResolutionError(code_id="<resolver>", wiki_path="<base>",
                                      reason="resolver_unconfigured:test")
    monkeypatch.setattr(resolver, "resolve_wiki_path", unconfigured)
    with pytest.raises(CitationResolutionError, match="resolver_unconfigured"):
        transfer(**coefficient, re_model=1e7, re_ship=1e9,
                 form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, cite="strict")


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_transfer_opt_out_preserves_unresolved_record(transfer, coefficient):
    result = transfer(**coefficient, re_model=1e7, re_ship=1e9,
                      form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, cite=False)
    assert result.citations == [] and not result.cited
    assert result.unresolved_citations[0].source_id == "ITTC-7.5-02-03-01.4"
    assert any("citation" in item for item in result.omitted_corrections)


def test_prohaska_public_imports_and_source_limits():
    import ast
    from digitalmodel.naval_architecture import friction_scaling, prohaska
    assert friction_scaling.prohaska_form_factor is prohaska.prohaska_form_factor
    assert friction_scaling.ProhaskaFit is prohaska.ProhaskaFit
    for module in (friction_scaling, prohaska):
        source = Path(module.__file__).read_text()
        assert len(source.splitlines()) <= 400
        for node in ast.walk(ast.parse(source)):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                assert node.end_lineno - node.lineno + 1 <= 50, node.name


def test_wildcard_import_preserves_public_friction_api():
    from digitalmodel.naval_architecture import friction_scaling
    exports = {}
    exec("from digitalmodel.naval_architecture.friction_scaling import *", exports)
    for name in ("Fluid", "reynolds_number_si", "froude_number",
                 "ITTC_TRANSFER_PROCEDURE", "ITTC_TRANSFER_UNRESOLVED",
                 "AllowanceTerm", "UnresolvedCitation", "TransferResult",
                 "ittc57_cf", "ittc57_cf_cited", "ProhaskaFit", "prohaska_form_factor",
                 "transfer_model_to_ship", "transfer_ship_to_model"):
        assert exports.get(name) is getattr(friction_scaling, name), name


@pytest.mark.parametrize("allowance", ["ca_model", "ca_ship", "delta_cf"])
def test_invalid_allowance_precedes_citation_resolution(tmp_path, allowance):
    with pytest.raises(ValueError, match=allowance):
        transfer_model_to_ship(ct_model=0.004, re_model=1e7, re_ship=1e9,
                               form_factor_k=0.2, fn_model=0.25, fn_ship=0.25,
                               repo_root=tmp_path, **{allowance: float("nan")})


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_transfer_procedure_revision_mismatch_fails_closed(tmp_path, transfer, coefficient):
    import shutil
    shutil.copytree(CIT_ROOT / "knowledge", tmp_path / "knowledge")
    target = tmp_path / "knowledge/wikis/marine-engineering/wiki/standards/ittc-1978-performance-prediction.md"
    original = target.read_text()
    assert "revision: 2021 Revision 05" in original
    target.write_text(original.replace("revision: 2021 Revision 05", "revision: invalid"))
    with pytest.raises(CitationResolutionError) as exc:
        transfer(**coefficient, re_model=1e7, re_ship=1e9,
                 form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, repo_root=tmp_path, cite="strict")
    assert exc.value.code_id == "ITTC-7.5-02-03-01.4"
    assert exc.value.reason.startswith("frontmatter_mismatch:revision:")


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_default_transfer_without_procedure_page_matches_uncited(tmp_path, transfer, coefficient):
    import shutil
    source = CIT_ROOT / "knowledge/wikis/marine-engineering/wiki/standards/en400.md"
    target = tmp_path / "knowledge/wikis/marine-engineering/wiki/standards/en400.md"
    target.parent.mkdir(parents=True)
    shutil.copyfile(source, target)
    kw = dict(**coefficient, re_model=1e7, re_ship=1e9,
              form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, repo_root=tmp_path)
    result = transfer(**kw)
    plain = transfer(**kw, cite=False)
    assert result.ct_model == plain.ct_model and result.ct_ship == plain.ct_ship
    assert [c.code_id for c in result.citations] == ["EN400"]
    assert result.cited
    assert result.unresolved_citations[0].source_id == "ITTC-7.5-02-03-01.4"


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_default_standalone_transfer_warns_once_and_returns(monkeypatch, transfer, coefficient):
    import warnings
    from digitalmodel.citations import resolver
    from digitalmodel.naval_architecture import resistance

    def unconfigured(*args, **kwargs):
        raise CitationResolutionError(code_id="<resolver>", wiki_path="<base>",
                                      reason="resolver_unconfigured:test")

    monkeypatch.setattr(resolver, "resolve_wiki_path", unconfigured)
    monkeypatch.setattr(resistance, "_EN400_STANDALONE_WARNED", False)
    kw = dict(**coefficient, re_model=1e7, re_ship=1e9,
              form_factor_k=0.2, fn_model=0.25, fn_ship=0.25)
    with pytest.warns(RuntimeWarning, match="standalone mode: EN400 citation unavailable") as emitted:
        result = transfer(**kw)
    assert len(emitted) == 1
    with warnings.catch_warnings(record=True) as repeated:
        warnings.simplefilter("always")
        again = transfer(**kw)
    assert repeated == []
    assert result.ct_model == again.ct_model == transfer(**kw, cite=False).ct_model
    assert result.ct_ship == again.ct_ship == transfer(**kw, cite=False).ct_ship
    assert result.citations == [] and result.cited
    assert result.unresolved_citations[0].source_id == "ITTC-7.5-02-03-01.4"


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
@pytest.mark.parametrize("cite", ["Strict", "false", "off"])
def test_transfer_rejects_invalid_citation_mode(transfer, coefficient, cite):
    with pytest.raises(ValueError, match="cite"):
        transfer(**coefficient, re_model=1e7, re_ship=1e9,
                 form_factor_k=0.2, fn_model=0.25, fn_ship=0.25,
                 repo_root=CIT_ROOT, cite=cite)


@pytest.mark.parametrize("cite", [1, 0, None])
def test_legacy_nonstring_citation_flags_remain_compatible(cite):
    result = transfer_model_to_ship(ct_model=0.004, re_model=1e7, re_ship=1e9,
                                   form_factor_k=0.2, fn_model=0.25, fn_ship=0.25,
                                   repo_root=CIT_ROOT, cite=cite)
    assert result.cited == bool(cite)
    assert len(result.citations) == int(bool(cite))
    assert result.unresolved_citations


@pytest.mark.parametrize("transfer,coefficient", [
    (transfer_model_to_ship, {"ct_model": 0.004}),
    (transfer_ship_to_model, {"ct_ship": 0.0022}),
])
def test_strict_and_default_transfer_have_identical_coefficients(transfer, coefficient):
    kw = dict(**coefficient, re_model=1e7, re_ship=1e9,
              form_factor_k=0.2, fn_model=0.25, fn_ship=0.25, repo_root=CIT_ROOT)
    default, strict, plain = transfer(**kw), transfer(**kw, cite="strict"), transfer(**kw, cite=False)
    for field in ("ct_model", "ct_ship", "cr", "cf_model", "cf_ship"):
        assert getattr(default, field) == getattr(strict, field) == getattr(plain, field)
    assert default.unresolved_citations and not strict.unresolved_citations

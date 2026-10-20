"""Exercise the shipped atlas query through the engine's config merge."""
from pathlib import Path

from digitalmodel.engine import engine


def test_engine_merges_only_supported_query_policy(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    example = (Path(__file__).resolve().parents[2]
               / "examples/workflows/code-check-atlas-query/input.yml")
    cfg = engine(inputfile=str(example))
    query = cfg["parametric_query"]
    assert query["policy"] == {"on_out_of_range": "escalate"}
    result = query["result"]
    assert result["in_range"] is True
    assert result["response"] == "utilisation"
    assert result["value"] > 0
    assert result["screening_status"] in {"pass", "fail"}

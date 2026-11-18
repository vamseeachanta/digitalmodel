"""Registry paths and the ordered ten-section containment/HIPPS contract."""

import re
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]


def registered_packs(node):
    if isinstance(node, dict):
        for key, value in node.items():
            if key == "agent_pack":
                yield value
            else:
                yield from registered_packs(value)
    elif isinstance(node, list):
        for value in node:
            yield from registered_packs(value)


def test_every_registered_pack_exists():
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    paths = list(registered_packs(registry))
    assert paths
    assert not [path for path in paths if not (ROOT / path).is_file()]


def test_new_pack_registration_and_section_order():
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    entry = next(row for row in registry["modules"] if row["module"] == "subsea")
    implementation = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    for module in ("capping_stack", "hipps"):
        path = f"src/digitalmodel/subsea/{module}/docs/agent-pack.md"
        assert entry["submodules"][module] == {"agent_pack": path, "qa_family": "EC"}
        text = (ROOT / path).read_text()
        assert re.findall(r"(?m)^## (\d+)\. ", text) == [str(i) for i in range(1, 11)]
        assert implementation["agent_packs"][f"subsea/{module}"]["agent_pack"] == path

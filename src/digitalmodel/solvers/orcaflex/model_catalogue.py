# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""YAML-only catalogue; run this file directly with --output page.html."""

import argparse
from html import escape
import importlib.util
from pathlib import Path
import re
from types import ModuleType
from typing import Iterable, cast

import yaml

ROOTS = ("library/model_library", "templates", "library/templates")
STRUCTURAL_FOLDERS = (
    "base",
    "cases",
    "variations",
    "includes",
    "inputs",
    "modular",
    "monolithic",
)


def load_renderer() -> ModuleType:
    """Load the sibling renderer without executing the solver initializer."""
    path = Path(__file__).with_name("model_catalogue_render.py")
    spec = importlib.util.spec_from_file_location("model_catalogue_render", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


renderer = load_renderer()
MISSING: str = renderer.MISSING
SAFE_LOADER = renderer.SAFE_LOADER
mapping = renderer.mapping
render_card = renderer.render_card


def classify_provenance(name: str, readme: str, header: str) -> str:
    """Use affirmative provenance text, never a system-type 'generic' value."""
    text = (readme + "\n" + header).lower()
    pattern = r"project[- ]derived|sanitis|sanitiz|private (?:data )?repository"
    if re.search(pattern, text):
        return "project-derived sanitised"
    repositories = re.findall(r"github\.com/[^/\s)]+/([\w-]+)", text)
    if any(repo != "digitalmodel" for repo in repositories):
        return "project-derived sanitised"
    if re.search(r"provenance:\s*generic|origin class:\s*generic", text):
        return "generic"
    if name.startswith("mj") and "reconstructed from published literature" in text:
        return "generic (literature-derived)"
    if (
        re.match(r"[a-e]\d", name)
        and "orcina example lettering" in text
        or re.search(r"source:.*orcaflex examples|provenance:\s*vendor example", text)
    ):
        return "vendor example"
    return MISSING


def claim_levels(registry: str, inventory: str) -> dict[str, str]:
    """Keep pending inventory entries separate from attested registry levels."""
    pending = mapping(yaml.load(inventory, Loader=SAFE_LOADER)).get("inventory", [])
    claims = {row["name"]: "no claim (pending inventory)" for row in pending}
    for row in mapping(yaml.load(registry, Loader=SAFE_LOADER)).get("models", []):
        level = row.get("highest_validated_level")
        if level in ("L1", "L2", "L3"):
            claims[row["name"]] = f"{level} (registry)"
    return claims


def render_catalogue(
    entries: Iterable[tuple[str, str, str]],
    claims: dict | None = None,
    usage: dict | None = None,
    omissions: Iterable[str] = (),
    extracted_models: set[str] | None = None,
) -> str:
    """Consume one source at a time, then sort rendered cards deterministically."""
    cards = []
    omitted = list(omissions)
    for path, text, provenance in entries:
        if (
            provenance.startswith("project-derived")
            or "project-derived" in Path(path).parts
        ):
            omitted.append(path + ": excluded: project-derived")
            continue
        name = model_name(path)
        owner = model_path(path)
        claim = (claims or {}).get(name, "no claim")
        if claim != "no claim":
            claim += "; model-level claim"
        refs = (usage or {}).get(owner, [])
        card = render_card(
            path,
            text,
            provenance,
            claim,
            refs,
            extracted=owner in (extracted_models or set()),
        )
        cards.append((path, card))
    body = "".join(card for _, card in sorted(cards))
    listing = "".join("<li>" + escape(item) + "</li>" for item in sorted(omitted))
    return cast(str, renderer.page_template(body, listing))


def model_name(path: str) -> str:
    """Identify the owning model for registry matching and reference searches."""
    return Path(model_path(path)).name


def model_path(path: str) -> str:
    """Resolve the model folder above all structural directories."""
    parts = Path(path).parts
    if "model_library" in parts:
        stop = parts.index("model_library") + 2
        return Path(*parts[:stop]).as_posix()
    stop = next(
        (i for i, part in enumerate(parts[:-1]) if part in STRUCTURAL_FOLDERS),
        len(parts) - 1,
    )
    return Path(*parts[:stop]).as_posix()


def read_text(path: Path) -> str:
    """Expose invalid encoding to card rendering as an explicit read failure."""
    return path.read_text(encoding="utf-8-sig", errors="replace")


def header(path: Path) -> str:
    """Read only the initial comment block for provenance classification."""
    lines = []
    with path.open(encoding="utf-8-sig", errors="replace") as stream:
        for line in stream:
            if line.strip() and not line.startswith(("#", "%", "---")):
                break
            lines.append(line)
    return "".join(lines)


def source_groups(domain: Path) -> list[tuple[Path, list[Path]]]:
    """Walk the three catalogue roots in a stable order."""
    groups: dict[Path, list[Path]] = {}
    for folder in ROOTS:
        root = domain / folder
        for path in sorted(root.rglob("*")):
            if path.is_file() and path.suffix.lower() in (".yml", ".yaml"):
                relative = path.relative_to(root)
                group = root / relative.parts[0] if len(relative.parts) > 1 else root
                deep = folder == "templates" and len(relative.parts) > 2
                if deep and relative.parts[0] != "include_structure":
                    group /= relative.parts[1]
                groups.setdefault(group, []).append(path)
        for child in sorted(root.iterdir()):
            populated = any(child == g or child in g.parents for g in groups)
            if child.is_dir() and not populated:
                groups.setdefault(child, [])
    return sorted(groups.items())


def catalogue_entries(root: Path, groups: list, omissions: list) -> Iterable:
    """Check group provenance before emitting any member's metadata."""
    for group, paths in groups:
        readmes = [group / "README.md", group.parent / "README.md"]
        readme = "\n".join(read_text(p) for p in readmes if p.is_file())
        headers = [header(p) for p in paths]
        provenance = classify_provenance(group.name, readme, "\n".join(headers))
        if provenance.startswith("project-derived") or not paths:
            reason = "excluded: project-derived" if paths else "no YAML files"
            omissions.append(group.relative_to(root).as_posix() + ": " + reason)
            continue
        for path, source_header in zip(paths, headers):
            relative = path.relative_to(root).as_posix()
            if path.name in ("catalog.yaml", "audit_results.yaml"):
                omissions.append(relative + ": catalogue/audit metadata, not a model")
                continue
            origin = classify_provenance(model_name(relative), readme, source_header)
            yield relative, read_text(path), origin


def find_usage(root: Path, groups: list) -> dict[str, list[str]]:
    """Report references naming the model's parent and folder as path components."""
    owners = sorted(
        {
            model_path(p.relative_to(root).as_posix())
            for _, paths in groups
            for p in paths
        }
    )
    patterns = {
        owner: re.compile(
            r"(?<![\w.-])"
            + re.escape("/".join(owner.split("/")[-2:]))
            + r"(?![\w.-])"
        )
        for owner in owners
    }
    usage: dict[str, list[str]] = {owner: [] for owner in owners}
    candidates = [root / "docs/registry/workflows.yaml"]
    for folder in ("tests", "examples/workflows"):
        candidates += sorted((root / folder).rglob("*"))
    for path in candidates:
        if path.is_file() and path.suffix in (".py", ".yaml", ".yml", ".md", ".json"):
            text = read_text(path).replace("\\", "/")
            for owner, pattern in patterns.items():
                if pattern.search(text):
                    usage[owner].append(path.relative_to(root).as_posix())
    return usage


def extracted_models(root: Path, groups: list) -> set[str]:
    """Propagate extraction evidence from reference outputs and specifications."""
    extracted = set()
    for _, paths in groups:
        for path in paths:
            relative = path.relative_to(root).as_posix()
            if "monolithic" in Path(relative).parts:
                extracted.add(model_path(relative))
            elif path.name in ("spec.yml", "model-spec.yml"):
                data, _ = renderer.parse_model(read_text(path))
                descriptions = [data.get("description")] + [
                    mapping(data.get(key)).get("description")
                    for key in ("metadata", "model")
                ]
                if any(
                    isinstance(v, str) and v.startswith("Extracted from")
                    for v in descriptions
                ):
                    extracted.add(model_path(relative))
    return extracted


def build(root: Path) -> str:
    """Build from repository metadata and file text, without resolving includes."""
    domain = root / "docs/domains/orcaflex"
    claims = claim_levels(
        read_text(domain / "MODEL_CLAIM_REGISTRY.yaml"),
        read_text(domain / "MODEL_CLAIM_INVENTORY.yaml"),
    )
    groups = source_groups(domain)
    usage = find_usage(root, groups)
    omissions: list[str] = []

    extracted = extracted_models(root, groups)
    page = render_catalogue(
        catalogue_entries(root, groups, omissions),
        claims,
        usage,
        extracted_models=extracted,
    )
    listing = "".join("<li>" + escape(item) + "</li>" for item in sorted(omissions))
    return page.replace("</ul></html>", listing + "</ul></html>")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    page = build(Path(__file__).resolve().parents[4])
    args.output.write_text(page, encoding="utf-8", newline="\n")
    print(f"Catalogue: {len(page.encode('utf-8'))} bytes")

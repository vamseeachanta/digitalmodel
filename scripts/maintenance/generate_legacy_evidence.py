"""Regenerate the frozen reset evidence: python scripts/maintenance/generate_legacy_evidence.py BARE_MIRROR."""

import argparse
import hashlib
import json
import re
import sys
from functools import cache
from pathlib import Path, PurePosixPath

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "src"))
from digitalmodel.legacy_evidence import (  # noqa: E402
    MANIFEST, RESET_ROOT, evidence_path, git_output, publication_digest,
)


def additional_claims(read_reset, archive_git):
    # These are the individual files read in pinned mode by the three report evaluators.
    report_inputs = {
        "issue-001-hull-roll-screening": [
            "scripts/capabilities/build_generic_hull_screening.py",
            "src/digitalmodel/hydrodynamics/utube_tank.py",
            "docs/api/cfd/data/generic_hull_roll_screening.json",
        ],
        "issue-001-sloshing-frequency": [
            "docs/api/structural/sloshing-cfd-benchmark.json",
            "docs/api/cfd/sloshing-convergence.json",
        ],
        "issue-002-cp-anode-editions": [
            f"tests/fixtures/cathodic_protection/workflow_inputs/{name}.yml"
            for name in ("pipeline", "multi_component_riser", "published_jacket_84m",
                         "reference_jacket_60m_members", "monopile", "manifold")
        ],
    }
    for folder, paths in report_inputs.items():
        record = json.loads(read_reset(f"examples/report-evals/{folder}/results.json"))
        for path in paths:
            yield record["code"]["commit"], path, "exists"

    replay = json.loads(read_reset("docs/audits/analytical-riser-regression.json"))
    inputs = [row["path"] for row in replay["sources"].values()]
    last_change = archive_git("log", "-1", "--format=%H", "refs/heads/main", "--", *inputs)
    if last_change.decode().strip() != replay["source_revision"]:
        raise ValueError("Archived last modification differs for analytical riser inputs")
    for path in inputs:
        yield replay["source_revision"], path, "main-ancestry"

    register = json.loads(read_reset("docs/domains/risers/global-model-evidence-register.json"))
    revision = register["snapshot_revision"]
    for row in register["sources"].values():
        yield revision, row["path"], "exists"
    # Establish completeness from the archived tree independently of the register.
    inventory = archive_git("ls-tree", "-r", "--name-only", revision, "--",
                            "tests/drilling_riser/global_model/").decode().splitlines()
    for path in inventory:
        if PurePosixPath(path).match("test_*.py"):
            yield revision, path, "exists"

    campaign = json.loads(read_reset(
        "docs/domains/orcaflex/library/campaigns/generic-drilling-riser/manifest.campaign.json"))
    yield campaign["ref"], "docs/domains/orcaflex/library/templates/drilling_riser/spec.yml", "exists"

    mesh = json.loads(read_reset("tests/fixtures/test_vectors/naval_architecture/mesh_legacy_pickles.json"))
    for name in ("mesh_hydrostatics", "hull_fixtures"):
        yield mesh["source_revision"], f"src/digitalmodel/naval_architecture/{name}.py", "exists"

    review = json.loads(read_reset("docs/domains/cathodic_protection/reviews/edition-paths-review.json"))
    for row in review:
        for location in (row, *row.get("evidence", [])):
            yield row["commit"], location["file"], "exists"
        for reference in row["test"]:
            yield row["commit"], reference.partition("::")[0], "exists"

    pack = read_reset("src/digitalmodel/solvers/orcawave/docs/agent-pack.md").decode("utf-8")
    code_pin = re.search(r"^\| Code revision \| `([0-9a-f]{40})`", pack, re.MULTILINE)[1]
    for reference in re.findall(r"\[([^\]\n]+):L\d+-L\d+\]", pack):
        path, explicit, revision = reference.partition("@")
        yield revision if explicit else code_pin, path, "exists"

    pack = read_reset("src/digitalmodel/materials/docs/agent-pack.md").decode()
    revision = re.search(r"\| Code revision \| `([0-9a-f]{40})`", pack)[1]
    # The database independently requires the grades source revision on main.
    yield revision, "src/digitalmodel/materials/grades.py", "main-ancestry"
    for path in sorted(set(re.findall(r"\[([^\]\s]+):L\d+-L\d+\]", pack))):
        yield revision, path, "exists"

    capture = json.loads(read_reset(
        "tests/marine_ops/marine_engineering/visualization/fixtures/"
        "ocimf_explorer_pre_refactor_trace_signature.json"
    ))
    yield capture["source_commit_sha"], capture["build_script_path"], "exists"

    database = json.loads(read_reset("data/rao_database.manifest.json"))
    for entry in database["entries"]:
        for source in entry["sources"]:
            path = source["path"]
            revision = source["revision"]
            last_change = archive_git("log", "-1", "--format=%H", "refs/heads/main", "--", path)
            if last_change.decode().strip() != revision:
                raise ValueError(f"Archived last modification differs for {path}")
            workbook = archive_git("cat-file", "blob", f"{revision}:{path}")
            if hashlib.sha256(workbook).hexdigest() != source["sha256"]:
                raise ValueError(f"Archived RAO fixture digest differs for {revision}:{path}")
            yield revision, path, "main-ancestry"


def tree_files(git, revision, paths):
    raw = git("ls-tree", "-rz", revision, "--", *paths)
    files = {}
    for row in raw.split(b"\0"):
        if row:
            metadata, path = row.split(b"\t", 1)
            mode, kind, blob = metadata.decode().split()
            if kind == "blob" and mode in ("100644", "100755"):
                files[path.decode()] = (mode, blob)
    return files


def generate(mirror):
    def archive(*args):
        return git_output(["--git-dir=" + str(mirror)], *args)

    def local(*args):
        return git_output(["-C", str(ROOT)], *args)

    def read_reset(path):
        return local("cat-file", "blob", f"{RESET_ROOT}:{path}")

    old_main = archive("rev-parse", "refs/heads/main").decode().strip()
    if archive("rev-parse", old_main + "^{tree}") != local("rev-parse", RESET_ROOT + "^{tree}"):
        raise ValueError("Archived main tree does not match the reset root")

    @cache
    def digest(blob):
        return hashlib.sha256(archive("cat-file", "blob", blob)).hexdigest()

    def first_publication(row, expected):
        # Preserve the original search ordering, even across merged branches.
        candidates = archive("log", "--reverse", "--format=%H", old_main, "--",
                             row["manifest"], *row["sources"]).decode().splitlines()
        for candidate in candidates:
            files = tree_files(archive, candidate, expected)
            if files.keys() == expected.keys() and all(
                digest(files[path][1]) == sha for path, sha in expected.items()
            ):
                return candidate
        raise ValueError(f"No matching archived publication for {row['manifest']}")

    rows = json.loads(read_reset("atlases/historical-provenance.json"))["atlases"]
    claims = set(additional_claims(read_reset, archive))
    publications = {}
    for row in rows:
        expected = row["sources"].copy()
        if "source_revision" in row:
            revision = row["source_revision"]
        else:
            if row["source_publication"] != "first-matching-artifact-publication":
                raise ValueError("Unknown publication selector")
            expected.update(row["artifacts"])
            revision = first_publication(row, expected)
            publications[row["manifest"]] = {
                "revision": revision, "record_sha256": publication_digest(row),
            }
        files = tree_files(archive, revision, expected)
        for path, sha in expected.items():
            if digest(files[path][1]) != sha:
                raise ValueError(f"Atlas source digest mismatch at {revision}:{path}")
            claims.add((revision, path, "main-ancestry"))

    @cache
    def resolve_commit(ref):
        return archive("rev-parse", "--verify", ref + "^{commit}").decode().strip()

    by_revision = {}
    aliases = {}
    for ref, path, rule in sorted(claims):
        full = resolve_commit(ref)
        if ref != full:
            aliases[ref] = full
        paths = by_revision.setdefault(full, {})
        # If two consumers share a revision/path, satisfy the stronger proof.
        if paths.get(path) != "main-ancestry":
            paths[path] = rule

    entries = {}
    archived_refs = {}
    blobs = {}
    for full, paths in by_revision.items():
        if "main-ancestry" in paths.values():
            archive("merge-base", "--is-ancestor", full, old_main)
        if "exists" in paths.values():
            refs = archive("for-each-ref", "--sort=refname", "--format=%(refname)",
                           "--contains", full).decode().splitlines()
            archived_refs[full] = refs
        archived = tree_files(archive, full, paths)
        root = tree_files(local, RESET_ROOT, paths)
        for path, rule in paths.items():
            mode, blob = archived[path]
            sha = digest(blob)
            same = path in root and root[path][1] == blob
            entries.setdefault(full, {})[path] = {
                "mode": mode, "blob": blob, "sha256": sha, "rule": rule,
                "storage": "same-as-root" if same else "blob",
            }
            if not same:
                blobs[sha] = blob

    manifest = {"archived_main": old_main, "reset_root": RESET_ROOT, "aliases": aliases,
                "archived_refs": archived_refs, "publications": publications, "files": entries}
    raw = (json.dumps(manifest, indent=2, sort_keys=True) + "\n").encode()
    destination = evidence_path(ROOT, MANIFEST)
    store = MANIFEST.parent / "blobs"
    evidence_path(ROOT, store).mkdir(parents=True, exist_ok=True)
    for sha, blob in blobs.items():
        evidence_path(ROOT, store / sha).write_bytes(archive("cat-file", "blob", blob))
    destination.write_bytes(raw)
    print(f"{sum(map(len, entries.values()))} files, {len(blobs)} stored blobs; "
          f"manifest sha256 {hashlib.sha256(raw).hexdigest()}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mirror", type=Path)
    generate(parser.parse_args().mirror.resolve())

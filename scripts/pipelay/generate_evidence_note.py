# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate the evidence note and retain its synthetic bundle in scratch.

Run with PYTHONPATH=src and a fresh --work-dir outside the repository. Numerical
inputs are synthetic selections or inherited template assumptions, never design
allowables from a standard. Formula coefficients follow screening-basis.md E5-E6.
"""

import argparse
import json
from contextlib import chdir
from datetime import date
from pathlib import Path

import yaml

from digitalmodel.pipelay.evidence import verify_evidence_manifest
from digitalmodel.pipelay.workflow import router

ROOT = Path(__file__).resolve().parents[2]
NOTE = ROOT / "src/digitalmodel/pipelay/docs/evidence-manifest.md"
CATENARY = (
    "https://phys.libretexts.org/Bookshelves/Classical_Mechanics/"
    "Classical_Mechanics_(Tatum)/18%3A_The_Catenary"
)
TEXTBOOK = (
    "https://www.sciencedirect.com/book/9780123868886/"
    "subsea-pipeline-design-analysis-and-installation"
)


def fenced(value):
    """Render strict JSON without changing any recorded field."""
    return "```json\n" + json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n```"


def build_note(work_dir: Path) -> str:
    """Issue a real bundle, verify it and retain a one-byte tamper demonstration."""
    work_dir = work_dir.resolve()
    # Fail rather than overwrite an earlier example's evidence.
    work_dir.mkdir(parents=True, exist_ok=False)
    template = ROOT / "examples/workflows/reel-lay-pipelay/input.yml"
    settings = yaml.safe_load(template.read_text(encoding="utf-8"))["pipelay"]
    settings.pop("case")
    settings.pop("operability")
    # Synthetic set, also independently worked in test_independent_anchors.py.
    diameter, radius, efficiency, span = 0.2, 10.0, 0.8, 8.0
    settings["pipe"]["outer_diameter"] = diameter
    settings["config"].update(
        reel_core_radius_m=radius, straightener_efficiency=efficiency,
        residual_out_of_straightness_span_m=span,
    )
    settings["outputs"] = {"directory": "issued", "summary_json": "screening.json"}
    cfg = {"basename": "pipelay", "pipelay": settings}
    # Sorted strict JSON is the evidence input; YAML source bytes are not claimed.
    received = json.loads(json.dumps(cfg, sort_keys=True, allow_nan=False))
    with chdir(work_dir):
        result = router(cfg)
        summary_path = Path(result["pipelay"]["summary_json"])
        manifest_path = summary_path.with_name(summary_path.name + ".manifest.json")
        manifest = json.loads(manifest_path.read_bytes())
        summary = json.loads(summary_path.read_bytes())
        intact = verify_evidence_manifest(manifest_path)
        input_path = manifest_path.parent / manifest["input"]["file"]
        original = input_path.read_bytes()
        input_path.write_bytes(b"[" + original[1:])
        refused = verify_evidence_manifest(manifest_path)
        # Retain the refused bytes separately and restore the intact issued bundle.
        input_path.with_name(input_path.name + ".tampered").write_bytes(input_path.read_bytes())
        input_path.write_bytes(original)
        if intact != {"ok": True, "failures": []} or refused != {
            "ok": False, "failures": [{"artefact": "input", "reason": "digest_mismatch"}],
        }:
            raise RuntimeError("Evidence example did not meet its byte-integrity contract")

    # Independent arithmetic for presentation; E5-E6 derive the factors below.
    reeling = diameter / (2 * radius)
    curvature = (1 - efficiency) / radius
    residual = diameter * curvature / 2
    sagitta = curvature * span**2 / 8
    revision = 1  # First issue of this generated note, not an engineering value.
    issued = date(2026, 10, 22)
    source_modified = date(2022, 8, 8)  # Opened catenary text's last-modified date.
    textbook_year, force_section, geometry_section = 2014, "18.2", "18.3"
    return f"""# Pipelay screening evidence manifest

## Summary

An issued screening summary now has a received-config snapshot and a digest
manifest beside it. Readback verification detects changed or missing bytes and
refuses unsupported manifests. This is evidence consistency, not physical
qualification, design approval or an authenticated signature.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| {revision} | {issued.isoformat()} | Added the issued evidence contract and a reproducible synthetic example to make byte changes detectable. |

Table {revision}. Initial issue of this note.

## Inputs and retained artefacts

`router(cfg)` saves the entire config as received, before defaults and returned
results mutate it: `json.dumps(cfg, sort_keys=True, allow_nan=False)`, encoded as
UTF-8. This is a JSON snapshot of the received mapping; it is not the original
YAML file, its comments, or the bytes before engine preprocessing. An unserialisable
or nonfinite config is refused before a summary is written.

For a summary called `screening.json`, the siblings are
`screening.json.input.json` and `screening.json.manifest.json`. The latter records
the schema version, hash algorithm, distribution metadata version as
`tool_revision`, and the filenames and digests of the input and output.
Unavailable distribution metadata is `NOT_EVALUATED`; a source commit is not
inferred from a package version. The manifest hashes the saved input and summary
by reading their bytes from disk. Existing summary keys and criteria are unchanged.

All geometric values below are synthetic. Ancillary pipe properties and strain
criteria are inherited template assumptions, assumed, based on previous
experience; they are not standard allowables or qualified material data. All
inputs, including the output locator, are shown:

{fenced(received)}

## Method and worked answer

The local derivation is [screening-basis.md](screening-basis.md), equations E5-E6:
outer-fibre strain is half the diameter times curvature;
residual curvature is the unrecovered fraction of reel curvature. Integrating a
constant small-slope curvature over a symmetric span gives the sagitta expression.
For this synthetic set the arithmetic is:

```text
reeling strain = D / (2 R) = {reeling:.6f}
residual curvature = (1 - eta) / R = {curvature:.8f} /m
residual radius = 1 / curvature = {1 / curvature:.1f} m
residual strain = D curvature / 2 = {residual:.6f}
sagitta = curvature L^2 / 8 = {sagitta:.5f} m
```

The answer below is read from the issued summary, including the screen result
against the supplied illustrative criterion:

{fenced(summary)}

The issued manifest is read back without modifying its fields:

{fenced(manifest)}

Intact verification result:

{fenced(intact)}

Changing the initial opening brace of the saved input to an opening square bracket
changes exactly one byte. The verifier compares bytes, without trusting or parsing
the modified input. Its refusal is:

{fenced(refused)}

## Verification contract and limits

Call `verify_evidence_manifest(Path(...))`. `ok` is true only when `failures` is
empty. Failures name `input`, `output` or `manifest` with a reason:
`digest_mismatch`, `missing_file`, `unreadable_file`, `invalid_json`,
`invalid_schema`, `unsupported_schema_version`, `unsupported_hash_algorithm` or
`unknown_keys`. Duplicate keys are invalid JSON for this contract. Artefact names
must match the expected siblings. Missing fields, malformed digests and unknown
nested keys fail closed. Both changed artefacts can be reported together.

Retain the whole bundle and use a fresh output destination for each issue. The
existing destination overwrite behaviour is unchanged. Issuance is not a
transactional archive: an interrupted write can leave an incomplete bundle,
which verification refuses. A test-only hash in a test file is not an issued
manifest. The unsigned manifest cannot detect coordinated replacement of the
manifest and its artefacts. No signer identity, timestamp, original source
revision or protected chain of custody is established.

The manifest does not establish physical validation of the flexible-line
approximation, straightener efficiency, cyclic plasticity or ovalisation. It
does not establish dynamic installation response or suitability of a strain
criterion. No native solver is executed. The catenary arithmetic checks are
consistency checks of a static, inextensible, uniformly weighted flexible line.

## Sources and NOT_EVALUATED

- [Bai and Bai, Subsea Pipeline Design, Analysis and Installation]({TEXTBOOK})
  ({textbook_year}), chapters 5 and 6: inherited equation-form background in
  `workflow.py`. Publisher access returned HTTP 403; chapter text and page
  confirmation are NOT_EVALUATED. No inaccessible allowable is used.
- [Classical Mechanics, The Catenary]({CATENARY}), sections {force_section}
  and {geometry_section} (public text, last modified {source_modified.year}): force
  resolution, catenary parameter, arc length and tension identities were opened.
  These support independent arithmetic closure tests, not physical validation.
- NOT_EVALUATED: rights-cleared public measured or experimentally qualified
  J-lay and reel-lay benchmark datasets and their versioned physical tolerances.
  None was supplied or established by the issue or opened references. No licensed,
  private or inaccessible dataset is substituted for independent evidence.

## Reproduction

Generated by `scripts/pipelay/generate_evidence_note.py`. Run with `PYTHONPATH=src`,
`--work-dir` set to a fresh scratch directory outside the repository and `--output`
set to this note's path. The script retains the intact bundle and tampered copy
in scratch. Its regression test regenerates this entire note, including every
computed quantity, both artefact digests and the refusal. Only the displayed
distribution metadata version is normalized when comparing environments;
no digest or result is normalized. Bibliographic identifiers and revision
metadata are rendered by the same script.
"""


def main():
    """Write the generated note; all non-deliverable output stays in scratch."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=NOTE)
    args = parser.parse_args()
    if args.work_dir.resolve().is_relative_to(ROOT):
        parser.error("work-dir must be outside the repository")
    args.output.write_text(build_note(args.work_dir), encoding="utf-8")


if __name__ == "__main__":
    main()

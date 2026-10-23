"""Synthetic signed-admission controls; these do not qualify a native model."""

import hashlib
import json

from digitalmodel.solvers.orcaflex.qa_admission import AdmissionContext, AdmissionRequest


def control_fingerprint(master, case):
    """Construct the expected digest independently from the runner.

    Hand construction, using the repository fingerprint convention: sort the
    fixture files by relative name; concatenate ``name:SHA256(file)`` and a
    newline for each; append the case JSON with sorted keys and compact
    separators; SHA256 the resulting UTF-8 bytes. No solver output participates.
    """
    root = master.parent
    entries = sorted(
        (path.relative_to(root).as_posix(), hashlib.sha256(path.read_bytes()).hexdigest())
        for path in root.rglob("*") if path.is_file()
    )
    material = "".join(f"{name}:{digest}\n" for name, digest in entries)
    material += json.dumps(case, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(material.encode()).hexdigest()


def make_qa(cases, root, fingerprints=None, version="test-version"):
    """Approve exact synthetic records with a separate in-memory trust store.

    Expected decisions come from agent-pack.md section 5, B1–B9 (AQG r0
    section 12). Payload membership is a test double for signature verification.
    No signature algorithm or engineering acceptance limit is inferred here.
    """
    root.mkdir(parents=True, exist_ok=True)
    image = root / "image.bin"
    image.write_bytes(b"synthetic image")
    image_digest = hashlib.sha256(image.read_bytes()).hexdigest()
    approved = set()
    requests = {}
    for case in cases:
        fingerprint = (fingerprints or {}).get(
            case["case_id"], hashlib.sha256(case["case_id"].encode()).hexdigest()
        )
        record = {
            "case_id": case["case_id"],
            "input_sha256": fingerprint,
            "program": "OrcaFlex",
            "program_version": version,
            "criteria_revision": "test-revision",
            "producer": "synthetic-producer",
            "configuration": "synthetic-configuration",
            "sample": "synthetic-sample",
            "gates": {gate: {"status": "PASS"} for gate in
                      ("G0", "G1", "G2", "G3", "G4", "G5")},
            "images": {"static": image_digest},
            "deviations": [],
            "representatives": [{
                "configuration": "synthetic-configuration",
                "sample": "synthetic-sample",
                "program_version": version,
                "criteria_revision": "test-revision",
                "status": "PASS",
                "evidence_ref": "synthetic-reference",
                "evidence_sha256": image_digest,
            }],
            "signature": {"reviewer": "synthetic-reviewer", "approval": "test-only"},
        }
        payload = {key: value for key, value in record.items() if key != "signature"}
        approved.add(json.dumps(payload, sort_keys=True, separators=(",", ":"),
                                allow_nan=False).encode())
        requests[case["case_id"]] = AdmissionRequest(
            record=record, input_sha256=fingerprint,
            configuration="synthetic-configuration", sample="synthetic-sample",
            images={"static": image},
        )
    context = AdmissionContext(
        criteria_revision="test-revision", required_images=("static",),
        verify_signature=lambda payload, signature: (
            payload in approved and signature.get("approval") == "test-only"
            and signature.get("reviewer") == "synthetic-reviewer"
        ),
        requests=requests,
    )
    return context

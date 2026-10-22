"""Reproduce the documented LF/CRLF SHA-256 example in a supplied scratch folder."""
import argparse
import hashlib
import json
from pathlib import Path

from digitalmodel.parametric.text_basis import normalized_text_bytes


def build_example(work_dir: Path) -> dict[str, str]:
    """Hash literal byte streams before and after the shared text normalization."""
    work_dir.mkdir(parents=True, exist_ok=True)
    lf = work_dir / "example-lf.txt"
    crlf = work_dir / "example-crlf.txt"
    lf.write_bytes(b"alpha\nbeta\n")
    crlf.write_bytes(b"alpha\r\nbeta\r\n")
    normalized = normalized_text_bytes(lf)
    if normalized_text_bytes(crlf) != normalized:
        raise ValueError("LF and CRLF canonical text differs")
    return {
        "lf_sha256_before": hashlib.sha256(lf.read_bytes()).hexdigest(),
        "crlf_sha256_before": hashlib.sha256(crlf.read_bytes()).hexdigest(),
        "sha256_after": hashlib.sha256(normalized).hexdigest(),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(build_example(args.work_dir), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

"""Extract JSON string values from exact Git blobs for register checking."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess


def digest(data):
    return hashlib.sha256(data).hexdigest()


def string_values(value):
    if isinstance(value, str):
        return [value]
    if isinstance(value, dict):
        value = list(value.values())
    if isinstance(value, list):
        return [text for item in value for text in string_values(item)]
    return []


def extract(repo, revision, paths):
    inputs, values = [], []
    for path in paths:
        data = subprocess.check_output(
            ["git", "-C", str(repo), "show", f"{revision}:{path}"]
        )
        inputs.append({"path": path, "sha256": digest(data), "bytes": len(data)})
        values.extend(string_values(json.loads(data)))
    prose = "\n\n".join(values)
    encoded = prose.encode("utf-8")
    return {"inputs": inputs, "prose": prose, "extracted_bytes": len(encoded),
            "extracted_prose_sha256": digest(encoded), "revision": revision}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path.cwd())
    parser.add_argument("--revision", required=True, help="Git tree/commit; empty for index")
    parser.add_argument("paths", nargs="+")
    args = parser.parse_args()
    print(json.dumps(extract(args.repo, args.revision, args.paths), ensure_ascii=True))


if __name__ == "__main__":
    main()

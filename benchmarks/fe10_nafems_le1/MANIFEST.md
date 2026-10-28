# FE10 package manifest

The machine-readable [manifest](manifest.json) records committed SHA-256 digests, received input digests and publication transformations.
The imported preflight record is historical; [RESULT.md](RESULT.md) and [record.json](record.json) contain the measured outcomes.

Omitted source excerpts: `sources/feenox-nafems-le1-excerpt.txt` and `sources/featool-ex_planestress5-excerpt.txt`; reuse rights were not asserted.
No file exceeded 1 MB. A citation-only source record, public citations, generator and original input decks are retained.
The upstream implementation excerpt is omitted; its revision, blob and numerical definition remain in the source record.

## Manifest refresh

After an intentional retained-artifact edit, run this manifest refresh command
from the package directory, then rerun the benchmark tests:

```sh
python - <<'PY'
import hashlib
import json
from pathlib import Path

path = Path("manifest.json")
manifest = json.loads(path.read_text(encoding="utf-8"))
for entry in manifest["files"]:
    payload = Path(entry["path"]).read_bytes()
    entry["bytes"] = len(payload)
    entry["sha256"] = hashlib.sha256(payload).hexdigest()
path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8", newline="\n")
PY
```

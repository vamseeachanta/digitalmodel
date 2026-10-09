"""Selected native input readback; not numerical or engineering parity."""
import hashlib

import yaml


def selected_equal(observed, expected):
    if isinstance(expected, dict):
        return isinstance(observed, dict) and all(
            key in observed and selected_equal(observed[key], value)
            for key, value in expected.items())
    if isinstance(expected, list):
        return isinstance(observed, list) and len(observed) == len(expected) and all(
            selected_equal(a, b) for a, b in zip(observed, expected))
    if isinstance(observed, bool) != isinstance(expected, bool):
        return False
    return observed == expected


def verify_export(model, phase_dir, expected):
    path = phase_dir / "loaded.yml"
    if path.exists():
        raise FileExistsError("native readback export must be fresh")
    model.SaveData(path)
    data = yaml.safe_load(path.read_text(encoding="utf-8-sig"))
    if not isinstance(data, dict):
        raise ValueError("native input export must be a mapping")
    for section, objects in expected.items():
        entries = data.get(section)
        if not isinstance(entries, list):
            raise ValueError(f"native input export section missing: {section}")
        by_name = {}
        for entry in entries:
            if not isinstance(entry, dict) or "Name" not in entry:
                raise ValueError("native input export object has no identity")
            name = entry["Name"]
            if name in by_name:
                raise ValueError("native input export contains duplicate identities")
            by_name[name] = entry
        for name, properties in objects.items():
            if name not in by_name:
                raise ValueError("required object missing from native input export")
            for prop, value in properties.items():
                if prop not in by_name[name] or not selected_equal(by_name[name][prop], value):
                    raise ValueError(f"native input export differs: {section}.{prop}")
    return hashlib.sha256(path.read_bytes()).hexdigest()

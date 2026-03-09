# assetutilities API Contracts — digitalmodel

## Policy

- **stable**: No breaking changes without an assetutilities major version bump.
- **provisional**: Best-effort stability; consumers should guard with `try/except ImportError`.

## Contracted Symbols

| Module path | Symbol | Stability | Notes |
|---|---|---|---|
| `assetutilities.common.data` | `SaveData` | stable | Core persistence class; must remain callable |
| `assetutilities.common.data` | `ReadData` | stable | Core read class; must remain callable |
| `assetutilities.common.data` | `AttributeDict` | stable | Dict subclass with attribute access; `d.key` and `d["key"]` both required |
| `assetutilities.common.data` | `Transform` | stable | Data transformation class; must remain callable |
| `assetutilities.common.yml_utilities` | `WorkingWithYAML` | stable | YAML read/write facade; must remain callable |
| `assetutilities.common.yml_utilities` | `WorkingWithYAML.read_yaml` or `.load_yaml` | stable | At least one read method required |
| `assetutilities.common.yml_utilities` | `WorkingWithYAML.save_yaml` or `.dump_yaml` or `.write_yaml` | stable | At least one write method required |
| `assetutilities.common.yml_utilities` | `ymlInput` | provisional | Legacy module-level alias; skip if removed |
| `assetutilities.units.quantity` | `TrackedQuantity` | provisional | Units tracking; skip if module unavailable |
| `assetutilities.units.computation` | `unit_checked` | provisional | Decorator for unit enforcement; skip if unavailable |
| `assetutilities.units.exceptions` | `UnitMismatchError` | provisional | Must be an `Exception` subclass if present |
| `assetutilities.common.file_management` | `FileManagement` | stable | File utility class; must remain callable |
| `assetutilities.common.file_management` | `FileManagement.check_if_file_exists` or `.is_file_valid` | stable | At least one existence-check method required |
| `assetutilities.common.update_deep` | `update_deep_dictionary` | stable | Deep dict merge; must preserve existing keys |
| `assetutilities.common.utilities` | `is_file_valid_func` | stable | File validation helper; must remain callable |
| `assetutilities.common.utilities` | `is_dir_valid_func` | stable | Directory validation helper; must remain callable |

## Running Contracts

```bash
cd digitalmodel
PYTHONPATH="src:../assetutilities/src" uv run python -m pytest tests/contracts/ -v --tb=short -m contracts
```

## Violation Reporting

When a contract test fails, the failure output is prefixed:

```
[CONTRACT VIOLATION] symbol=<test_name> au_version=<version>
```

This identifies the broken symbol and the assetutilities version that introduced the regression.
File a bug against assetutilities and add an entry to `specs/wrk/` before upgrading.

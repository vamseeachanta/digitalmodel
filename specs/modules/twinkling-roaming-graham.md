# Plan: Add Input File Browsing to Benchmark Reports

## Context

Benchmark HTML reports currently show results tables and Plotly charts but don't let users inspect the actual input files (spec.yml, modular YAML, etc.). Users need to browse these files inline to review model definitions, validate parameters, and cross-check against results.

## Approach: Collapsible `<details>` Blocks with Embedded Content

Use native HTML5 `<details>/<summary>` elements to embed file contents as collapsible code blocks in the per-model sections. This keeps the report self-contained (no server needed) and lazy-renders content (browser only processes open blocks).

### What to Embed per Model

| File | Priority | Typical Size | Notes |
|------|----------|-------------|-------|
| `spec.yml` | HIGH | 8-260 KB | Primary input definition |
| `master.yml` | MEDIUM | ~1 KB | Shows include structure |
| Include YAMLs | MEDIUM | 1-4 KB each | Builder outputs (e.g. 20_generic_objects.yml) |
| `benchmark.json` | LOW | ~8 KB | Already shown as tables |

### Size Management

- Files > 500 lines: truncate with "... truncated (N total lines)" notice
- Large spec.yml files (~260KB from extractor): show first 200 lines
- Small files (master.yml, parameters.yml): embed fully
- Total overhead per model: ~50-100 KB HTML (acceptable for single-page report)

## Changes

### File: `scripts/benchmark_model_library.py`

#### 1. Add CSS for collapsible file viewer (in `_CSS` constant)

```css
.file-browser { margin: 1em 0; }
.file-browser details { border: 1px solid #ddd; border-radius: 4px; margin: 0.3em 0; }
.file-browser summary { padding: 0.5em 0.8em; background: #f0f0f0; cursor: pointer;
  font-size: 0.85em; font-weight: 600; color: #2c3e50; }
.file-browser summary:hover { background: #e0e0e0; }
.file-browser pre { margin: 0; padding: 0.8em; background: #fafafa;
  font-size: 0.78em; font-family: 'SF Mono','Cascadia Code','Consolas',monospace;
  overflow-x: auto; max-height: 500px; overflow-y: auto; border-top: 1px solid #eee; }
.file-browser .file-meta { color: #888; font-size: 0.75em; padding: 0.3em 0.8em;
  border-top: 1px solid #eee; background: #f8f8f8; }
```

#### 2. New helper function: `_embed_file()`

```python
def _embed_file(label: str, file_path: Path, max_lines: int = 200) -> str:
    """Generate a collapsible <details> block with file content."""
    if not file_path.exists():
        return ""
    text = file_path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()
    total = len(lines)
    truncated = total > max_lines
    display = "\n".join(lines[:max_lines])
    if truncated:
        display += f"\n\n... truncated ({total} total lines)"
    size_kb = file_path.stat().st_size / 1024
    return (
        f'<details><summary>{label} ({size_kb:.1f} KB)</summary>'
        f'<pre>{html.escape(display)}</pre>'
        f'<div class="file-meta">{file_path.name} &mdash; {total} lines</div>'
        f'</details>'
    )
```

#### 3. New helper function: `_build_file_browser()`

```python
def _build_file_browser(model_name: str, dat_path: Path) -> str:
    """Build the file browser section for a model."""
    sanitized = _sanitize_name(model_name)
    lib_dir = LIBRARY_ROOT / sanitized
    parts = ['<div class="file-browser"><h3>Input Files</h3>']

    # spec.yml
    spec = lib_dir / "spec.yml"
    parts.append(_embed_file("spec.yml", spec, max_lines=200))

    # master.yml
    master = lib_dir / "modular" / "master.yml"
    parts.append(_embed_file("master.yml", master, max_lines=50))

    # Include files
    includes_dir = lib_dir / "modular" / "includes"
    if includes_dir.exists():
        for f in sorted(includes_dir.glob("*.yml")):
            parts.append(_embed_file(f"includes/{f.name}", f, max_lines=300))

    parts.append('</div>')
    content = "\n".join(parts)
    # Only return if we found at least one file
    return content if any('<details>' in p for p in parts) else ""
```

#### 4. Insert file browser in per-model section

In `generate_report()`, after the line results table and before the closing `</div>` of model-text, add:

```python
# File browser (after line results table)
fb = _build_file_browser(r.name, Path(r.dat_path))
if fb:
    bp.append(fb)
```

#### 5. Add `import html` at top

For `html.escape()` in the `_embed_file` function.

#### 6. Add `LIBRARY_ROOT` constant

```python
LIBRARY_ROOT = Path("docs/modules/orcaflex/library/model_library")
```

Note: This path is already defined (added in the 3-way comparison changes). Verify it exists, don't duplicate.

## Verification

1. Run `uv run python scripts/generate_all_specs.py --max-models 3` to ensure spec/modular files exist
2. Run `uv run python scripts/benchmark_model_library.py --html-only` to regenerate report from existing JSON
3. Open `benchmark_output/model_library_report.html` in browser
4. Verify each converged model section shows "Input Files" with collapsible spec.yml, master.yml, and include files
5. Verify files open/close on click, content is syntax-preserved, large files are truncated

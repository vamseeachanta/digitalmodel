# Wall Thickness Quickcheck

Runs the deckhand wall-thickness quickcheck from the committed cache and writes
the self-contained HTML report plus JSON/CSV result extracts.

Run:

```bash
uv run python -m digitalmodel examples/workflows/wall-thickness-quickcheck/input.yml
```

Expected outputs are written to `examples/workflows/wall-thickness-quickcheck/results/`.

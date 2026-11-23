# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Run the segment table directly without importing licensed interfaces."""

import argparse
import json
from pathlib import Path

import yaml

from digitalmodel.subsea.on_bottom_stability.e305_workflow import BASENAME, router


def main() -> None:
    """Read explicit inputs and print deterministic output digests."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input', type=Path, default=Path(__file__).with_name('input.yml'))
    args = parser.parse_args()
    cfg = yaml.safe_load(args.input.read_text())
    cfg['_config_dir_path'] = str(args.input.resolve().parent)
    result = router(cfg)[BASENAME]['result']
    print(json.dumps(result.get('output_sha256', {}), sort_keys=True))


if __name__ == '__main__':
    main()

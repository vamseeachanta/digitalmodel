# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare Markdown through the same trusted formatter without writing inputs.

Equal canonical output establishes formatting equivalence under the selected
formatter and configuration; it does not establish engineering correctness.
"""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path


FORMAT_MARKDOWN = """
const prettier = require(process.argv[1]);
(async () => {
    process.stdin.setEncoding('utf8');
    let input = '';
    for await (const chunk of process.stdin) input += chunk;
    process.stdout.write(await prettier.format(input, {parser: 'markdown'}));
})().catch(() => process.exit(2));
"""


def canonical_markdown(path: Path, formatter: str) -> str:
    """Return the formatter's Markdown output, refusing unreadable inputs."""
    result = subprocess.run(
        ["node", "-e", FORMAT_MARKDOWN, str(Path(formatter).resolve())],
        input=path.read_text(encoding="utf-8"),
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=True,
        timeout=30,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    )
    return result.stdout


def main() -> int:
    """Report equivalence, content differences or an unavailable comparison."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--formatter", required=True, help="Trusted Prettier module directory")
    parser.add_argument("before", type=Path)
    parser.add_argument("after", type=Path)
    args = parser.parse_args()
    try:
        before = canonical_markdown(args.before, args.formatter)
        after = canonical_markdown(args.after, args.formatter)
    except (OSError, UnicodeError, subprocess.SubprocessError):
        print("Markdown formatting comparison could not run.")
        return 2
    if before != after:
        print("Markdown formatting comparison: different canonical content.")
        return 1
    print("Markdown formatting comparison: equivalent canonical content.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

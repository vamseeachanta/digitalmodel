"""List script-process candidates without signaling or modifying processes."""

import argparse
import os
from pathlib import Path


def find_script_processes(script: Path, cwd: Path, proc_root: Path = Path("/proc")) -> list[int]:
    """Return candidates with an exact script argument and the requested cwd.

    Linux man-pages 6.19, proc_pid_cmdline(5), DESCRIPTION: argv strings are
    NUL-separated. proc_pid_cwd(5), DESCRIPTION: cwd is a symbolic link.
    These mutable observations are diagnostic evidence, not process identity.
    Inaccessible or vanished entries are omitted, so an empty list does not
    establish that no script is running.
    """
    script = script.resolve(strict=True)
    cwd = cwd.resolve(strict=True)
    candidates = []
    for entry in proc_root.iterdir():
        if not entry.name.isascii() or not entry.name.isdecimal():
            continue
        try:
            observed_cwd = (entry / "cwd").resolve(strict=True)
            if observed_cwd != cwd:
                continue
            argv = (entry / "cmdline").read_bytes().split(b"\0")
        except (OSError, RuntimeError):
            # proc_pid_cwd(5): access is permission-controlled; processes exit.
            continue
        for argument in filter(None, argv):
            try:
                path = Path(os.fsdecode(argument))
                if not path.is_absolute():
                    path = observed_cwd / path
                if path.resolve() == script:
                    candidates.append(int(entry.name))
                    break
            except (OSError, RuntimeError):
                # One unresolvable argument must not hide a later match.
                continue
    return sorted(candidates)


def main() -> None:
    """Print candidate PIDs; leave any action to a separate identity check."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--script", type=Path, required=True)
    parser.add_argument("--cwd", type=Path, required=True)
    args = parser.parse_args()
    try:
        candidates = find_script_processes(args.script, args.cwd)
    except (OSError, RuntimeError) as error:
        parser.error(str(error))
    for pid in candidates:
        print(pid)


if __name__ == "__main__":
    main()

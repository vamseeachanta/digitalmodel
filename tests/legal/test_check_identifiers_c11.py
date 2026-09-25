"""Owner decision C11: every identifier category in the current tree fails CI.

The categories are operator/client, field/project, vessel, person or Windows
user, private path, and machine hostname. Each is exercised here with a
synthetic value assembled at runtime from fragments, so this file carries
nothing the gate would reject and needs no exclusion.

Names (operator, client, field, vessel) are matched through the salted-hash
list and the optional private list; a synthetic token stands in for them.
Everything else is a public pattern class: a Windows user profile, an
organisation's OneDrive folder, solver-export ``User``/``Machine`` header
lines, a Windows machine hostname, a mapped drive and a UNC share.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES = REPO / ".legal-deny-list.yaml"
TOKEN = "zzsynthetictestvessel"
BS = "\\"

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(), reason="identifier gate not installed"
)

_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")


@pytest.fixture()
def gate(tmp_path):
    """An isolated copy of the checker, its rules, and a home directory that
    holds no private list unless a test writes one."""
    root = tmp_path / "repo"
    (root / "scripts" / "legal").mkdir(parents=True)
    shutil.copy(CHECKER, root / "scripts" / "legal" / "check_identifiers.py")
    rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
    salt = str(rules.get("salt", ""))
    rules["hashed_names"] = list(rules.get("hashed_names") or []) + [
        hashlib.sha256(f"{salt}:{TOKEN}".encode()).hexdigest()
    ]
    (root / ".legal-deny-list.yaml").write_text(yaml.safe_dump(rules), encoding="utf-8")
    home = tmp_path / "home"
    home.mkdir()

    def run(*args, env=None):
        e = {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}
        e.pop("DIGITALMODEL_DENY_LIST", None)
        e["HOME"] = str(home)
        e["USERPROFILE"] = str(home)
        if env:
            e.update(env)
        return subprocess.run(
            [
                sys.executable,
                str(root / "scripts" / "legal" / "check_identifiers.py"),
                *args,
            ],
            cwd=root,
            capture_output=True,
            text=True,
            env=e,
            encoding="utf-8",
            errors="replace",
        )

    run.root = root
    run.home = home
    return run


def _file(gate, text: str, name: str = "sample.md") -> str:
    p = gate.root / name
    p.write_text(text, encoding="utf-8")
    return str(p)


def _user_path(sep: str = BS) -> str:
    return sep.join(["C:", "Users", "jdoe" + "123", "Documents", "model.yml"])


class TestEachC11CategoryIsDetected:
    """One positive case per category, named by the rule that must fire."""

    @pytest.mark.parametrize(
        "line,rule",
        [
            # operator / client and vessel names: the salted-hash list
            (f"Prepared for {TOKEN} under the charter.", "denied-name"),
            # field / project: a job code
            ("see B" + "1234 for the scope", "job-code"),
            # person / Windows user
            (f"fe_folder: {_user_path()}", "windows-user-path"),
            (f"fe_folder: {_user_path('/')}", "windows-user-path"),
            (f"path: {_user_path(BS * 2)}", "windows-user-path"),
            ("# " + "User: " + "jdoe" + "123", "solver-export-user"),
            # operator / client through a corporate OneDrive folder
            (
                "OneDrive" + " - " + "Example Operator Corporation" + BS + "Temp",
                "onedrive-org",
            ),
            # private path
            ("K:" + BS + "projects" + BS + "run" + BS + "a.dat", "mapped-drive-path"),
            (BS * 2 + "fileserver" + BS + "share" + BS + "a.dat", "unc-share"),
            (BS * 4 + "fileserver" + BS * 2 + "share" + BS * 2 + "a.dat", "unc-share"),
            # machine hostname
            ("# " + "Machine: " + "WORKSTATION" + "7", "solver-export-machine"),
            (
                "closeout on " + "abcd-hou-" + "rds" + "07" + " failed",
                "windows-hostname",
            ),
            ("run it on " + "ABCD-" + "ANSYS" + "09", "windows-hostname"),
        ],
    )
    def test_the_rule_fires(self, gate, line, rule):
        out = gate(_file(gate, line + "\n"))
        assert out.returncode == 1, out.stdout
        assert f"[{rule}]" in out.stdout, out.stdout

    def test_a_vessel_name_on_the_private_list_is_detected(self, gate):
        private = gate.home / "private.txt"
        private.write_text("zzprivatevessel\n", encoding="utf-8")
        out = gate(
            _file(gate, "moored alongside zzprivatevessel\n"),
            env={"DIGITALMODEL_DENY_LIST": str(private)},
        )
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout


class TestPlaceholdersPass:
    """The replacements the cleanup writes must not themselves be findings."""

    @pytest.mark.parametrize(
        "line",
        [
            "# " + "User: (removed)",
            "# " + "Machine: (removed)",
            '<span class="line"># ' + "User: (removed)</span>",
            "fe_folder: <private-data>" + BS + "Temp" + BS + "model.yml",
            "log_folder: <private-data>/results/",
            "C:" + BS + "Users" + BS + "Public" + BS + "Documents",
            "C:" + BS + "Users" + BS + "<user>" + BS + "AppData",
            "Store it under OneDrive" + " - " + "<org>" + BS + "Temp",
            "the licensed host ace-win-1 ran the case",
            "The mooring line has twelve anchors on a 2000 m spread.",
        ],
    )
    def test_no_finding(self, gate, line):
        out = gate(_file(gate, line + "\n"))
        assert out.returncode == 0, out.stdout


class TestUncFalsePositives:
    """A LaTeX command, an escaped relative path or a registry key is not a
    UNC share. The LaTeX lines are the two that tripped the old rule."""

    @pytest.mark.parametrize(
        "line",
        [
            '      latex: "'
            + BS * 2
            + "log N = "
            + BS * 2
            + "log "
            + BS * 2
            + "bar{a} - m "
            + BS * 2
            + "log "
            + BS * 2
            + "Delta"
            + BS * 2
            + 'sigma"',
            '    latex: "'
            + BS * 2
            + "log(N) = 15.19 - 3.0 "
            + BS * 2
            + "times "
            + BS * 2
            + "log("
            + BS * 2
            + "Delta"
            + BS * 2
            + 'sigma)"',
            "plt.savefig('results" + BS * 2 + "ASMEB31" + BS * 2 + "' + name)",
            '(vl-registry-write "HKEY_CURRENT_USER'
            + BS * 2
            + "SOFTWARE"
            + BS * 2
            + 'App" "App" "T")',
            "u = "
            + BS * 2
            + "frac{u_*}{"
            + BS * 2
            + "kappa} "
            + BS * 2
            + "ln"
            + BS * 2
            + "!"
            + BS * 2
            + "left( z "
            + BS * 2
            + "right)",
            'pattern: "^' + BS * 2 + "d+" + BS * 2 + 'd+$"',
        ],
    )
    def test_not_a_unc_share(self, gate, line):
        out = gate(_file(gate, line + "\n"))
        assert "[unc-share]" not in out.stdout, out.stdout


class TestPrivatePatterns:
    """A private list can hold regular expressions as well as names, so a
    pattern that would itself disclose something never has to be public."""

    def test_a_private_regex_is_applied(self, gate):
        private = gate.home / "private.txt"
        private.write_text("re:zzcode-[0-9]{3}\n", encoding="utf-8")
        out = gate(
            _file(gate, "job zzcode-481 closed\n"),
            env={"DIGITALMODEL_DENY_LIST": str(private)},
        )
        assert out.returncode == 1, out.stdout
        assert "private-pattern" in out.stdout
        # The pattern itself is not echoed into a public CI log.
        assert "zzcode-[0-9]" not in out.stdout

    def test_an_invalid_private_regex_is_an_error(self, gate):
        private = gate.home / "private.txt"
        private.write_text("re:(unclosed\n", encoding="utf-8")
        out = gate(_file(gate, "clean\n"), env={"DIGITALMODEL_DENY_LIST": str(private)})
        assert out.returncode not in (0, 1), out.stdout

    def test_the_default_private_location_is_read_when_present(self, gate):
        cfg = gate.home / ".config" / "digitalmodel"
        cfg.mkdir(parents=True)
        (cfg / "identifier-deny-list.txt").write_text(
            "zzdefaultlisted\n", encoding="utf-8"
        )
        out = gate(_file(gate, "the zzdefaultlisted field\n"))
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_no_default_private_list_is_not_an_error(self, gate):
        out = gate(_file(gate, "the zzdefaultlisted field\n"))
        assert out.returncode == 0, out.stdout


class TestExampleSentinel:
    """Documentation of a pattern has to show the pattern. A line marked as
    an example is exempt from the structural rules, and only from those."""

    SENTINEL = "identifier-gate: " + "example"

    def test_a_marked_example_line_passes_structural_rules(self, gate):
        line = "K:" + BS + "projects" + BS + "run" + BS + "a.dat"
        out = gate(_file(gate, f"{line}  # {self.SENTINEL}\n"))
        assert out.returncode == 0, out.stdout

    def test_a_marked_line_is_still_checked_for_names(self, gate):
        out = gate(_file(gate, f"{TOKEN}  # {self.SENTINEL}\n"))
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_the_marker_covers_only_its_own_line(self, gate):
        line = "K:" + BS + "projects" + BS + "run" + BS + "a.dat"
        out = gate(_file(gate, f"# {self.SENTINEL}\n{line}\n"))
        assert out.returncode == 1, out.stdout


class TestWholeTreeCeiling:
    """CI scans the whole tree. Files the gate cannot read are counted against
    an explicit ceiling rather than silently passed or blocking forever."""

    def test_uninspectable_within_the_ceiling_passes(self, gate):
        data = b"%PDF-1.7\n\x00\x01\x02 binary \x00\xff"
        path = gate.root / "a.pdf"
        path.write_bytes(data)
        out = gate(str(path), "--max-uninspectable", "1")
        assert out.returncode == 0, out.stdout
        assert "1 uninspectable" in out.stdout

    def test_uninspectable_above_the_ceiling_fails(self, gate):
        data = b"%PDF-1.7\n\x00\x01\x02 binary \x00\xff"
        path = gate.root / "a.pdf"
        path.write_bytes(data)
        out = gate(str(path), "--max-uninspectable", "0")
        assert out.returncode == 2, out.stdout

    def test_a_finding_fails_whatever_the_ceiling(self, gate):
        out = gate(_file(gate, "see B" + "1234\n"), "--max-uninspectable", "99")
        assert out.returncode == 1, out.stdout


class TestLargeStagedCommits:
    """A commit touching ~1,500 files overflowed the Windows command line
    (WinError 206) because every staged path went into one git invocation."""

    def test_index_lookups_are_batched(self, monkeypatch):
        import importlib.util

        spec = importlib.util.spec_from_file_location("ci_batch", CHECKER)
        mod = importlib.util.module_from_spec(spec)
        sys.modules["ci_batch"] = mod
        spec.loader.exec_module(mod)
        paths = [
            f"docs/domain_{i:05d}/a_long_file_name_for_batching.md" for i in range(3000)
        ]
        calls = []
        oid = "0" * 40

        def fake_git(args, stdin=None):
            calls.append(args)
            if args[:2] == ["ls-files", "-s"]:
                listed = args[args.index("--") + 1 :]
                return "".join(f"160000 {oid} 0\t{p}\0" for p in listed).encode()
            raise AssertionError(args)

        monkeypatch.setattr(mod, "_git", fake_git)
        got = mod.index_blobs(paths)
        assert set(got) == set(paths)
        assert len(calls) > 1
        for args in calls:
            assert sum(len(a) + 1 for a in args) < 30000


class TestSymlinks:
    """A tracked symlink is committed as its target text. On Linux CI a link
    to a directory read as 'file does not exist' and counted as uninspectable,
    so the same tree gave different counts on Windows and Linux."""

    def test_a_symlink_to_a_directory_is_read_as_its_target(self, gate):
        target = gate.root / "somewhere"
        target.mkdir()
        link = gate.root / "linked"
        try:
            os.symlink("somewhere", link, target_is_directory=True)
        except (OSError, NotImplementedError):
            pytest.skip("symlinks cannot be created here")
        out = gate(str(link))
        assert out.returncode == 0, out.stdout
        assert "0 uninspectable" in out.stdout

    def test_a_symlink_target_is_checked_for_identifiers(self, gate):
        target = gate.root / ("K:" + BS + "projects" + BS + "run")
        link = gate.root / "linked2"
        try:
            os.symlink(str(target), link)
        except (OSError, NotImplementedError):
            pytest.skip("symlinks cannot be created here")
        out = gate(str(link))
        assert out.returncode == 1, out.stdout
        assert "mapped-drive-path" in out.stdout


class TestCiRunsTheGate:
    def test_the_quality_workflow_scans_the_whole_tree(self):
        wf = (REPO / ".github" / "workflows" / "quality-gates.yml").read_text(
            encoding="utf-8"
        )
        assert "scripts/legal/check_identifiers.py --all" in wf
        assert "--max-uninspectable" in wf

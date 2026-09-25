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
        # The gate refuses --update-baseline under CI; a test that runs in CI
        # sets these itself when it means to.
        e.pop("CI", None)
        e.pop("GITHUB_ACTIONS", None)
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
            # a one-character host is still a host
            (BS * 2 + "h" + BS + "share" + BS + "a.dat", "unc-share"),
            (BS * 4 + "h" + BS * 2 + "share" + BS * 2 + "a.dat", "unc-share"),
            ("copy to " + BS * 2 + "h" + BS + "share", "unc-share"),
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


class TestDigitLeadingNames:
    """A name that starts with a digit (a hull number, a numbered vessel) was
    never tokenised whole: the word pattern had to start with a letter, so
    only the letters after the digit were hashed or looked up."""

    def test_a_private_name_with_a_leading_digit_is_caught(self, gate):
        private = gate.home / "private.txt"
        private.write_text("7zzhull\n", encoding="utf-8")
        out = gate(
            _file(gate, "towed by 7zzhull at dawn\n"),
            env={"DIGITALMODEL_DENY_LIST": str(private)},
        )
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_a_hashed_name_with_a_leading_digit_is_caught(self, gate):
        rules = yaml.safe_load(
            (gate.root / ".legal-deny-list.yaml").read_text(encoding="utf-8")
        )
        salt = str(rules.get("salt", ""))
        rules["hashed_names"].append(
            hashlib.sha256(f"{salt}:7zzhull".encode()).hexdigest()
        )
        (gate.root / ".legal-deny-list.yaml").write_text(
            yaml.safe_dump(rules), encoding="utf-8"
        )
        out = gate(_file(gate, "towed by 7zzhull at dawn\n"))
        assert out.returncode == 1, out.stdout

    def test_a_digit_bearing_name_after_a_digit_is_caught(self, gate):
        # Review r2 finding 2: with a digit-first word pattern, "9zzatlas7"
        # yielded only itself and "zzatlas" -- the letter-first token
        # "zzatlas7" that the earlier pattern produced was lost.
        private = gate.home / "private.txt"
        private.write_text("zzatlas7\n", encoding="utf-8")
        out = gate(
            _file(gate, "berth 9zzatlas7 at dawn\n"),
            env={"DIGITALMODEL_DENY_LIST": str(private)},
        )
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_a_hashed_digit_bearing_name_after_a_digit_is_caught(self, gate):
        rules = yaml.safe_load(
            (gate.root / ".legal-deny-list.yaml").read_text(encoding="utf-8")
        )
        salt = str(rules.get("salt", ""))
        rules["hashed_names"].append(
            hashlib.sha256(f"{salt}:zzatlas7".encode()).hexdigest()
        )
        (gate.root / ".legal-deny-list.yaml").write_text(
            yaml.safe_dump(rules), encoding="utf-8"
        )
        for text in ("berth 9zzatlas7 at dawn\n", "berth x-9zzatlas7-b at dawn\n"):
            out = gate(_file(gate, text))
            assert out.returncode == 1, (text, out.stdout)

    def test_a_plain_number_is_not_a_name(self, gate):
        out = gate(_file(gate, "the 2000 m spread and 12345 cycles\n"))
        assert out.returncode == 0, out.stdout


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
            # one-letter LaTeX commands and escapes next to a one-letter host
            'latex: "' + BS * 2 + "a" + BS * 2 + "b = " + BS * 2 + 'x"',
            "s = '" + BS * 2 + "n" + BS * 2 + "t'",
            'latex: "' + BS * 2 + "Delta" + BS * 2 + "sigma_" + BS * 2 + 'a"',
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


PDF = b"%PDF-1.7\n\x00\x01\x02 binary \x00\xff"


class TestUninspectableBaseline:
    """CI scans the whole tree. Files the gate cannot read are accepted only
    when a committed manifest lists their path AND content digest. A count
    ceiling accepted a replaced binary, or one deleted and another added, as
    long as the number did not rise (review finding 5)."""

    def _baseline(self, gate):
        return gate.root / "baseline.txt"

    def _accept(self, gate, *paths):
        out = gate(
            *map(str, paths),
            "--baseline",
            str(self._baseline(gate)),
            "--update-baseline",
        )
        assert out.returncode == 0, out.stdout + out.stderr
        return out

    def test_without_a_baseline_an_uninspectable_file_fails(self, gate):
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        out = gate(str(path))
        assert out.returncode == 2, out.stdout

    def test_update_baseline_records_path_and_sha256(self, gate):
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        self._accept(gate, path)
        text = self._baseline(gate).read_text(encoding="utf-8")
        assert hashlib.sha256(PDF).hexdigest() in text
        assert "a.pdf" in text

    def test_a_listed_unchanged_file_passes(self, gate):
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        self._accept(gate, path)
        out = gate(str(path), "--baseline", str(self._baseline(gate)))
        assert out.returncode == 0, out.stdout
        assert "1 uninspectable" in out.stdout

    def test_a_changed_listed_file_fails(self, gate):
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        self._accept(gate, path)
        path.write_bytes(PDF + b"confidential")
        out = gate(str(path), "--baseline", str(self._baseline(gate)))
        assert out.returncode == 2, out.stdout
        assert "changed" in out.stdout

    def test_an_unlisted_file_fails_even_when_the_count_is_unchanged(self, gate):
        a = gate.root / "a.pdf"
        a.write_bytes(PDF)
        self._accept(gate, a)
        a.unlink()
        b = gate.root / "b.pdf"
        b.write_bytes(PDF)
        out = gate(str(b), "--baseline", str(self._baseline(gate)))
        assert out.returncode == 2, out.stdout
        assert "not in the baseline" in out.stdout

    def test_a_finding_fails_whatever_the_baseline(self, gate):
        a = gate.root / "a.pdf"
        a.write_bytes(PDF)
        self._accept(gate, a)
        out = gate(
            _file(gate, "see B" + "1234\n"),
            str(a),
            "--baseline",
            str(self._baseline(gate)),
        )
        assert out.returncode == 1, out.stdout

    def test_a_named_baseline_that_is_missing_is_an_error(self, gate):
        a = gate.root / "a.pdf"
        a.write_bytes(PDF)
        out = gate(str(a), "--baseline", str(gate.root / "absent.txt"))
        assert out.returncode not in (0, 1), out.stdout

    def test_update_baseline_refuses_the_staged_mode(self, gate):
        out = gate("--update-baseline", "--baseline", str(self._baseline(gate)))
        assert out.returncode not in (0, 1), out.stdout
        assert not self._baseline(gate).exists()

    @pytest.mark.parametrize("var", ["CI", "GITHUB_ACTIONS"])
    def test_update_baseline_refuses_to_run_in_ci(self, gate, var):
        # Review r2 finding 3: in CI the update would accept any new
        # uninspectable file unread. Regeneration is a reviewed local step.
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        out = gate(
            str(path),
            "--baseline",
            str(self._baseline(gate)),
            "--update-baseline",
            env={var: "true"},
        )
        assert out.returncode == 3, out.stdout + out.stderr
        assert "CI" in out.stderr
        assert not self._baseline(gate).exists()

    def test_a_normal_check_still_runs_in_ci(self, gate):
        path = gate.root / "a.pdf"
        path.write_bytes(PDF)
        self._accept(gate, path)
        out = gate(
            str(path),
            "--baseline",
            str(self._baseline(gate)),
            env={"CI": "true", "GITHUB_ACTIONS": "true"},
        )
        assert out.returncode == 0, out.stdout + out.stderr

    def test_the_committed_baseline_has_digests_for_every_entry(self):
        manifest = REPO / ".legal-uninspectable-baseline.txt"
        assert manifest.exists()
        rows = [
            ln
            for ln in manifest.read_text(encoding="utf-8").splitlines()
            if ln and not ln.startswith("#")
        ]
        assert rows
        for ln in rows:
            digest, sep, path = ln.partition("  ")
            assert sep and len(digest) == 64 and path, ln


class TestPrivateListIsCoveredWithoutIt:
    """Review finding 3: CI has no private list, so every literal name on it
    must be caught by a public class or a salted hash. The private list is
    read from where the operator keeps it (DIGITALMODEL_DENY_LIST, else the
    default location); without it the test skips, so this file names none."""

    @staticmethod
    def _private_list() -> Path | None:
        named = os.environ.get("DIGITALMODEL_DENY_LIST")
        if named:
            return Path(named)
        default = Path.home() / ".config" / "digitalmodel" / "identifier-deny-list.txt"
        return default if default.exists() else None

    def test_every_literal_entry_is_caught_with_the_private_list_absent(self, gate):
        private = self._private_list()
        if private is None or not private.exists():
            pytest.skip("no private deny list on this machine")
        names = [
            ln.strip()
            for ln in private.read_text(encoding="utf-8-sig").splitlines()
            if ln.strip() and not ln.strip().startswith(("#", "re:"))
        ]
        assert names, "the private list holds no literal entries"
        escaped = []
        for i, name in enumerate(names, start=1):
            for n, text in enumerate(
                (f"{name}\n", f"moored alongside {name} today\n"), start=1
            ):
                out = gate(_file(gate, text, name=f"entry{i}_{n}.md"))
                caught = out.returncode == 1 and "denied-name" in out.stdout
                if not caught:
                    escaped.append(f"entry #{i} form {n}")
        # Report positions only: a failure message reaches a CI log.
        assert not escaped, "private-list entries CI would miss: " + ", ".join(escaped)


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
        assert "--baseline .legal-uninspectable-baseline.txt" in wf
        assert "--max-uninspectable" not in wf

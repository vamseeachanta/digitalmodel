# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep committed OrcaFlex defaults compatible without changing model values."""

import hashlib
import json
import os
import re
import subprocess
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MODEL_SECTION = re.compile(
    r"^(?:LineTypes|Lines|Vessels|VesselTypes|6DBuoys|"
    r"3DBuoys|Winches|Shapes|Supports|MorisonElementTypes|Groups|VariableData|"
    r"BaseFile|includefile):|^  (?:UnitsSystem|WaterDepth|WaveTrains):", re.MULTILINE,
)


def git_output(*arguments):
    """Read this checkout without inherited Git repository bindings."""
    environment = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    return subprocess.check_output(
        ["git", *arguments], cwd=ROOT, env=environment,
    )


def model_files():
    """Enumerate tracked model text, including modular section fragments.

    Native section keys identify executable input rather than lowercase specs,
    basis records, parameters, manifests or source catalogs. OrcaWave exports
    are excluded by their program header.
    """
    names = git_output("ls-files", "-z", "*.yml", "*.yaml", "*.dat").decode().split("\0")
    for name in filter(None, names):
        path = ROOT / name
        if path.is_symlink():
            continue
        raw = path.read_bytes()
        encoding = "utf-16" if raw.startswith((b"\xff\xfe", b"\xfe\xff")) else "utf-8-sig"
        try:
            text = raw.decode(encoding)
        except UnicodeError:
            try:
                text = raw.decode("cp1252")
            except UnicodeError:
                continue
        if re.search(r"^(?:basename|metadata|basis|cases):", text, re.MULTILINE):
            continue
        native_header = re.search(r"^# Type: Model\s*$", text, re.MULTILINE)
        native_include = "/includes/" in name and (
            "orcaflex" in name.lower() or "slay-campaign" in name
        )
        wave_header = re.search(r"^#\s*Program:\s*OrcaWave\b", text, re.MULTILINE)
        if not wave_header and (
            native_header or native_include or MODEL_SECTION.search(text)
        ):
            yield path, text


def null_values(text):
    """Locate explicit plain null values, excluding comments and quoted text."""
    node = yaml.compose(text)
    pending = [node]
    while pending:
        item = pending.pop()
        if isinstance(item, yaml.MappingNode):
            pending.extend(value for _, value in item.value)
        elif isinstance(item, yaml.SequenceNode):
            pending.extend(item.value)
        elif (isinstance(item, yaml.ScalarNode)
              and item.tag == "tag:yaml.org,2002:null"
              and item.value.lower() == "null"):
            yield item


def test_committed_models_have_no_null_value_tokens():
    """A default shall use a tilde in every tracked executable model fragment."""
    scanned = []
    findings = []
    for path, text in model_files():
        scanned.append(path)
        if re.search(r"(?i)(?<!\w)null(?!\w)", text):
            findings.extend(
                f"{path.relative_to(ROOT)}:{node.start_mark.line + 1}"
                for node in null_values(text)
            )
    assert scanned, "No executable model text was discovered"
    assert not findings, "\n".join(findings)


@pytest.mark.parametrize("text,count", [
    ("General:\n  Default: null\n", 1),
    ("Lines:\n  - Defaults: [null, ~, NULL]\n", 2),
    ("General:\n  Text: 'null'\n  Other: nullify\n  # null\n", 0),
])
def test_null_detection_recognizes_only_values(text, count):
    """Quoted strings and comments shall retain their existing meaning."""
    assert len(list(null_values(text))) == count


# Canonical parsed-value fingerprints include the explicit JONSWAP selectors.
# Numeric settings remain covered when the checkout has no parent Git objects.
BASE_MODEL_VALUES = {
    'docs/domains/orcaflex/installation/manifold/region_a/monolithic/__03c_add_dz_10k_production_manifold.yml':
        '225aec585043290ebd225a4632d4dfb8241f43c1f405a8079e23f4a5c84901c9',
    'docs/domains/orcaflex/installation/manifold/region_a/monolithic/__03c_add_sz_10k_production_manifold.yml':
        '7685d0bd7910432e3ac110d9c17de5541f1d48ce754b8eeae8c0e1de2f0d839c',
    'docs/domains/orcaflex/installation/mudmat/monolithic/__02c_add_dz_manifold.yml':
        '94a4b64dbcf070613f2c5427f4a32f47e3f1ed867fa327570578a493f2edc51c',
    'docs/domains/orcaflex/installation/mudmat/monolithic/__02c_add_sz_manifold.yml':
        'e0b60d5e37e4d6f3af21ec16bf1abb853d9c7505c6326fb5d6f15b2fa0c882e9',
    'docs/domains/orcaflex/installation/mudmat/monolithic/manifold_deep.yml':
        'dc2867735c8ff139eeb4f70a36f6340b5258e9e50b7efaaa6d829a123d2195e7',
    'docs/domains/orcaflex/installation/mudmat/monolithic/manifold_splash.yml':
        'a0d837c5a5fc136ef7eb6a25ee6a78fd7403d020d195eb9422f7b3d7205c7019',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step1.yml':
        '4433a44cdffcdc76dffba4cd97c97a686e3437f41fc8f25a4a0c25a113b5cb13',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step2.yml':
        '9b7bdc128fb7d456b86673677e0237b8bc23b4b1ef7d611763efd651453be858',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step3.yml':
        '4bd5b4a7ceaec73f2ea0698c35e2947722328029ebae60fe5e3231ccc058e38d',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step4.yml':
        '7fd15af7b1bca4035a5fe27fcab97fbd0d4984b9413912a4cc6ffffd36137e53',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step5.yml':
        '0356028ced45bc21c2cf15f99fbc0fcf77c9a236101e3e75244f58c5ef3d01dd',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/Step6.yml':
        '574aae9725afd4f5e7bbed98c4381137c401d23cabc01a01b9568f475dc721c5',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/StoryBoards.yml':
        'dbbdb761ff4110be79d13e891878e89262a82140ddc1237082a127043779be09',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__03b_app_uth_body_deep.yml':
        'c9b1775eb506ea5032fd85667dbb16d14d453db035a9fe8a68aafc8861f2dc95',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__05_add_uthassembly_rigging.yml':
        '86bec8a682f4988ab3278a4a77c4352b17a75cc5d26420f2a8a4be2dfe7220e4',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__05b_app_buoyancy2_deep.yml':
        '76be834df84b173ee1fe90acf6b63b63edcaa97e3ef1c6187391fb3f9d757d67',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__06b_app_buoyancy2_deep.yml':
        'cf20821a92a514cba8eff7750bf72d55bf9a7831a78e255751ea01c28b7f5fc4',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__07b_app_buoyancy2_deep.yml':
        '5b6bbc20d9868bde474875a11132a82c29f27deb278c8d3df4ad0d2eb5db871b',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__08_add_2_buoyancy.yml':
        '8290733e618333c3c0cdd8061fafb032c768776571ede5371fbd459b446e479d',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__09a_app_buoyancy270kg_deep.yml':
        '418a22a4a6ddbc8562badc8cdd1782100f74ebc8e3f64860521b2820eb7e1a54',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__10a_app_buoyancy250kg_deep.yml':
        '0eb6215ab034078954ab2cf60c94e9041ab9a960e5ec1cb15592c329d971c92f',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__11a_app_buoyancy350kg_deep.yml':
        '442a523c585d009ab147ccbabe86815d1fd11fb80d40f7657ceabae1bf0edd80',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__12a_app_buoyancy400kg_deep.yml':
        'c8b6c290a1ea0c1c9e24546d21bd4fd7668d414e974dae2b569dd9be85d3ee2b',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__ref_05_umba_uth1_deep_superseded.yml':
        '7680e6ec7736768898c7461e63f8c3c883a8f1ba6257dbc670d126757f7458a0',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/__ref_05_umba_uth1_splz.yml':
        'ff7ad1514b0332c8bfcbc8767049f7f2c58dd8f81752ee95c8689a7db34d9daa',
    'docs/domains/orcaflex/installation/pipeline/route_a/monolithic/delete_test_deep.yml':
        '7a60c48328098158962d3e71abe8755fc3ea6014c89c6af99ad50a5387b7f4b1',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/scr/modular/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/scr/modular/includes/03_environment.yml':
        'b7015949ad33ee193e7425968841fc9530253feef4bbede4e82c2c4ce60e0f0a',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/scr/modular/includes/05_riser_line_types.yml':
        '8bc87b7014ad08f758c05fe589d8b4fc558e4dabb8050ecfc82ff781be498934',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/scr/modular/includes/07_riser_lines.yml':
        '34283a9928bf8ed545da24f5f082fab1e24dac86a1e67c42ed127cc988873b5e',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/slwr/modular/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/slwr/modular/includes/03_environment.yml':
        'b7015949ad33ee193e7425968841fc9530253feef4bbede4e82c2c4ce60e0f0a',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/slwr/modular/includes/05_riser_line_types.yml':
        '2f02775371a8b28f77c079550c7bb920240aeac4362686ffb352c241db852096',
    'docs/domains/orcaflex/library/campaigns/generic-scr-slwr/slwr/modular/includes/07_riser_lines.yml':
        '4c6d648f97ecf79dfcdc039c570bf89c0a763782d7d6c3029c69b89ce7170264',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-a/includes/01_general.yml':
        'c978c65da7cb5feb872ae8b668b26b7c348bfe00261d5d522b5fa16677e33c5c',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-a/includes/03_environment.yml':
        '8449e2269b48ce41e83c2e793e067536625c5ff9409ded25e5b6ebff5c8db104',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-a/includes/20_generic_objects.yml':
        '6204b244023c5e9cf515e1e494200ddb99271cf422af02d918fee5c480468d96',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-b/includes/01_general.yml':
        'c978c65da7cb5feb872ae8b668b26b7c348bfe00261d5d522b5fa16677e33c5c',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-b/includes/03_environment.yml':
        '6516b13b89d00ff25040a85b8406c915df150c6062d75de5d9e918c7864068ea',
    'docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-b/includes/20_generic_objects.yml':
        'b6d66d32b175bb3bff63a57d05eaf74344b1c36158607efc5476a5fe8990c6f4',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/03_environment.yml':
        'fb34b102bc139f5486a733861843a14af4493507bd4b9e924391d58985762e6a',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/11_winches.yml':
        'd8d5b5a3ee55973d63346bd5b7ac13f56e6850d372bdad838e0f58269b69323a',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t0/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/03_environment.yml':
        'fb34b102bc139f5486a733861843a14af4493507bd4b9e924391d58985762e6a',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/11_winches.yml':
        '7ca1e7baabf6537ed1502b70e2ae4cf6298abd67748a0c88df765a119885ce66',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t1/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/03_environment.yml':
        'fb34b102bc139f5486a733861843a14af4493507bd4b9e924391d58985762e6a',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/11_winches.yml':
        '56b6f21da9113605f28e3339b6082868d8a81b89bb926a0d3d3d7990c502424e',
    'docs/domains/pipeline-installation/slay-campaign/models/d0-t2/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/03_environment.yml':
        'a2f66b740f9664d53f428efc8ecac0e76403a0059e775506d0adf36cdf15e87c',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/11_winches.yml':
        '56b6f21da9113605f28e3339b6082868d8a81b89bb926a0d3d3d7990c502424e',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t0/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/03_environment.yml':
        'a2f66b740f9664d53f428efc8ecac0e76403a0059e775506d0adf36cdf15e87c',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/11_winches.yml':
        '3b1dcb929c44915f3619db04e2fa1ca3d5cddf677e63a6aeb6b19b2aa33d6248',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t1/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/01_general.yml':
        'e53dcd44267107014b8b8851ae133178ffab9392ce27bb542b2f33f758573dc0',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/03_environment.yml':
        'a2f66b740f9664d53f428efc8ecac0e76403a0059e775506d0adf36cdf15e87c',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/05_line_types.yml':
        'a570c24601aeee708f5c5c3ca3ac9bfcab387d05d6a539bc91965fc0f2834025',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/07_lines.yml':
        '01e54a0b1a418ec6492a1cac8dbc6733f5c5b24ff7c762ac3913f37dd3caa364',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/11_winches.yml':
        'fdde80f54a0d9e21709af7887ed471cd9b089260bb0b3a46ea6b8e12b96ec52c',
    'docs/domains/pipeline-installation/slay-campaign/models/d1-t2/includes/14_morison.yml':
        'a38d46191070bf8393dea2dd359b92bd5248ec161e5c42a30c183c58d2bb42b5',
    'tests/solvers/fea/test_data/mooring/C05 Single point mooring.yml':
        'b0e67395c5cd39d96da189a9d6d41cfb58a290faccebb3b79cdbfd10a7d9d629',
    'tests/solvers/fea/test_data/mooring/C06 Discretised CALM buoy.yml':
        '663ee20b4d720aa4b1dd9292263f3bf3b3bc1ecf507e0baed460fa214db44784',
    'tests/solvers/fea/test_data/pipelay/WD_30m.yml':
        'f2d8370ac36c85294d1b405eec3dd991f852c1b08d73cf775d63754a38ee42fb',
}


def test_model_changes_preserve_parsed_base_values():
    """Compare every normalized model with its generated BASE value fingerprint."""
    for relative, expected in BASE_MODEL_VALUES.items():
        values = yaml.safe_load((ROOT / relative).read_bytes())
        canonical = json.dumps(
            values, sort_keys=True, separators=(",", ":"), ensure_ascii=True,
        ).encode()
        assert hashlib.sha256(canonical).hexdigest() == expected, relative


def test_value_preservation_needs_no_parent_git_objects(monkeypatch):
    """Shallow hosted checkouts shall retain the model value comparison."""
    def deny_git(*arguments):
        raise AssertionError("Parent Git objects shall not be required")

    monkeypatch.setitem(globals(), "git_output", deny_git)
    test_model_changes_preserve_parsed_base_values()


@pytest.mark.parametrize("section", ["General", "Links", "Constraints"])
def test_model_discovery_includes_partial_native_sections(tmp_path, monkeypatch, section):
    """Native include fragments shall be scanned without a complete model header."""
    relative = "docs/domains/orcaflex/templates/modular/includes/fragment.yml"
    path = tmp_path / relative
    path.parent.mkdir(parents=True)
    text = f"{section}:\n  Default: null\n"
    path.write_text(text)
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    monkeypatch.setitem(globals(), "git_output", lambda *args: relative.encode())
    assert list(model_files()) == [(path, text)]


@pytest.mark.parametrize("encoding", ["utf-8-sig", "cp1252", "utf-16"])
def test_model_discovery_retains_native_text_encodings(tmp_path, monkeypatch, encoding):
    """Legacy text and byte-order-mark exports shall retain model coverage."""
    relative = "docs/domains/orcaflex/templates/model.yml"
    path = tmp_path / relative
    path.parent.mkdir(parents=True)
    text = "General:\n  UnitsSystem: SI\n  # Angle °\n  Default: null\n"
    path.write_bytes(text.encode(encoding))
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    monkeypatch.setitem(globals(), "git_output", lambda *args: relative.encode())
    assert list(model_files()) == [(path, text)]


@pytest.mark.parametrize("header,expected", [
    ("# RAOs imported from OrcaWave", True),
    ("# Program: OrcaWave", False),
])
def test_model_discovery_uses_program_header(tmp_path, monkeypatch, header, expected):
    """An OrcaWave reference shall not exclude an OrcaFlex vessel-type fragment."""
    relative = "docs/domains/orcaflex/templates/modular/includes/vessel_types.yml"
    path = tmp_path / relative
    path.parent.mkdir(parents=True)
    text = header + "\nVesselTypes: []\n"
    path.write_text(text)
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    monkeypatch.setitem(globals(), "git_output", lambda *args: relative.encode())
    assert bool(list(model_files())) == expected


def environment_mappings(text):
    """Read native Environment blocks without parsing unrelated model objects."""
    for start in re.finditer(r"^Environment:", text, re.MULTILINE):
        end = re.search(r"^[^\s#]", text[start.end():], re.MULTILINE)
        stop = start.end() + end.start() if end else len(text)
        pending = [yaml.compose(text[start.start():stop])]
        while pending:
            node = pending.pop()
            if isinstance(node, yaml.MappingNode):
                yield node
                pending.extend(value for _, value in node.value)
            elif isinstance(node, yaml.SequenceNode):
                pending.extend(node.value)


def wave_order_findings(text):
    """Find selector inversions within each native wave-train mapping.

    The builder defines deterministic and spectral wave parameters and states
    that WaveGamma depends on WaveJONSWAPParameters. Native exports place
    WaveType before the other wave properties of each train. JONSWAP gamma
    requires an explicit parameter selector and must be set before Tp.
    """
    findings = []
    for node in environment_mappings(text):
        keys = [key.value for key, _ in node.value]
        if "WaveType" not in keys:
            continue
        for key in keys[:keys.index("WaveType")]:
            if key.startswith("Wave") and key not in {
                "WaveDirection", "WaveOrigin", "WaveTimeOrigin",
            }:
                findings.append(("WaveType", key))
        if "WaveJONSWAPParameters" in keys and "WaveGamma" in keys:
            if keys.index("WaveGamma") < keys.index("WaveJONSWAPParameters"):
                findings.append(("WaveJONSWAPParameters", "WaveGamma"))
        if "WaveGamma" in keys:
            if "WaveTp" in keys and keys.index("WaveTp") < keys.index("WaveGamma"):
                findings.append(("WaveGamma", "WaveTp"))
            wave_type = next(value.value for key, value in node.value
                             if key.value == "WaveType")
            if wave_type == "JONSWAP" and "WaveJONSWAPParameters" not in keys:
                findings.append(("WaveJONSWAPParameters", "WaveGamma"))
    return findings


def test_committed_models_have_selector_first_wave_keys():
    """Every tracked executable model and include shall apply wave modes first."""
    scanned = []
    findings = []
    for path, text in model_files():
        scanned.append(path)
        findings.extend(
            f"{path.relative_to(ROOT)}: {dependent} precedes {selector}"
            for selector, dependent in wave_order_findings(text)
        )
    assert scanned, "No executable model text was discovered"
    assert not findings, "\n".join(findings)


@pytest.mark.parametrize("train,expected", [
    ("WaveTp: 12.9\n    WaveType: JONSWAP", [("WaveType", "WaveTp")]),
    ("WaveHs: 11.2\n    WaveType: JONSWAP", [("WaveType", "WaveHs")]),
    ("WaveGamma: 3.3\n    WaveType: JONSWAP",
     [("WaveType", "WaveGamma"), ("WaveJONSWAPParameters", "WaveGamma")]),
    ("WaveType: JONSWAP\n    WaveTp: 12.9", []),
    ("WaveType: JONSWAP\n    WaveGamma: 3.3\n    WaveJONSWAPParameters: Fully specified",
     [("WaveJONSWAPParameters", "WaveGamma")]),
    ("WaveType: JONSWAP\n    WaveJONSWAPParameters: Partially specified\n"
     "    WaveTp: 12.9\n    WaveGamma: 3.3", [("WaveGamma", "WaveTp")]),
    ("WaveType: JONSWAP\n    WaveGamma: 3.3",
     [("WaveJONSWAPParameters", "WaveGamma")]),
    ("WaveType: JONSWAP\n    WaveJONSWAPParameters: Partially specified\n"
     "    WaveGamma: 3.3\n    WaveTp: 12.9", []),
    ("WaveType: JONSWAP\n    WaveJONSWAPParameters: Partially specified\n"
     "    WaveGamma: 3.3\n    WaveTz: 12.9", []),
])
def test_wave_order_detection(train, expected):
    """Check each train independently of preceding trains and other sections."""
    text = (
        "Environment:\n  WaveTrains:\n  - WaveType: Airy\n    WavePeriod: 8\n"
        f"  - {train}\nLines:\n  WaveTp: 1\n"
    )
    assert wave_order_findings(text) == expected

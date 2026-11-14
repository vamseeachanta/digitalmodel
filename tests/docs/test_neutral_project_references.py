# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Keep the selected documentation free of private project locators."""

from hashlib import sha256
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
DOCUMENTS = (
    "docs/cad-inventory/README.md",
    "docs/cad-inventory/project-domain-map.md",
    "docs/capability-map/capability-report-2026-02-14.md",
    "docs/domains/agent-os/enhanced-create-specs-user-guide.md",
    "docs/plans/2026-05-05-issue-463-ship-dimensions-manual-curation.md",
    "docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md",
    "docs/plans/2026-05-05-issue-475-jumper-lift-pytest-suite.md",
    "docs/plans/2026-05-05-issue-480-plet-plem-jumper-segments.md",
    "docs/plans/2026-05-05-issue-481-plet-plem-workbook-conversion.md",
)
# Casefolded token digests keep removed identifiers out of fixtures and failures.
TOKENS = (
    (8, "ba5d4d12bdf35290c49f02de919e2eb4713f15e3110c5b3cb57fc4f6b6a350f7"),  # T01
    (9, "37bceb36aa0cc95c1ee73ad258f7b4fdbdc10467d3c546ac966c43d816b77170"),  # T02
    (15, "9ffb23929177c8c260a272abfaa05353bf9cc43ecb46be8684eb66ee430b9f5b"),  # T03
    (13, "405f6ba9b7d99e2256a76bac06765ed9b85ee3058b590f743d6f51318da7e2e7"),  # T04
    (11, "df380f23ef7900dbc03ee440d3ef18b8d09317a125ec27556fc839709bca5078"),  # T05
    (18, "cdee991d77d6d397d5e0c142132cd553d7d80d7649e0a97530f39c6da719da16"),  # T06
    (33, "5fc13e70a9abd5752be160e7997f9da8bd33964a5b0402e7ad4e40fc8685f8e8"),  # T07
    (17, "a49634d52b1b848079a79f77798c7d7bc39391be7037d44ea2dadf0e51a89947"),  # T08
    (9, "b25c931deb8b02345d2dd3c35c6df27bb5c79a3713de702caca74db1dd34571b"),  # T09
    (16, "cdf270122bf8e01e2715875adaefa5dedba49164866f2c8f1d5302d94921158e"),  # T10
    (11, "02d1ef527cf6ba873113f2ebe6f4c5d6c2ec8a3e280b3938b76cdf485133ee57"),  # T11
    (14, "bfd3f79a629296147efdb6a2d7e72eb92887c3fba243746d3ad50b76b2cf0f4b"),  # T12
    (11, "b745d434ab5df47fcfff983ae5b24a06bae9704ebd3f525c46bfb95d101e3db7"),  # T13
    (10, "52f06678828e12268c9a740acc222b968845fa1f8cefa6e7ea88e50d85b921bb"),  # T14
    (8, "6167fb2bc1a8d6b30506a66f86fe60bfd1fa7e95d7427c537b26f592d3f24b7e"),  # T15
    (11, "b7e3265ff8d92bd7b8350ae5cc0a4832f8b456507a2aee9fa858ed794a7ae1c3"),  # T16
    (15, "247e2e0e36fd684bc7d49c39aa2a82babd50c011bf92dd4a86b496a4a8010b4a"),  # T17
    (17, "8d35fa7c1a921e7bdb464e9b46fe99310bf085e52cd33ae24911e225c089c195"),  # T18
    (10, "a07f687661c7636b063eafce1dfd0af1261e6add55c041010c36f8dd845e9733"),  # T19
    (13, "2940033c286aeca157934b4bb3d5ade62c9fb53136ba534467d932d43f2fb132"),  # T20
    (13, "50bdac01f485a3ce2921dda3265c3103f071a69d14dc9524113f086a9bfa5915"),  # T21
    (14, "afe29d952a07adf20fcd54cfa89dfb100c8227b5944ad1320f3bb01b0c5ace8d"),  # T22
    (24, "e27e770f2b7dae8aabc4a926024800c63b7f2783f6cde26cde50b7ab6eb0b1cf"),  # T23
    (5, "f0b2481d6b24d018036a9e3d03c97b1d9e4b466b017dde25cb9c6a00fadaac11"),  # T24
    (17, "c866bbdd751ba22cd6428c1137f759b3ae9d231a53fa8a93af6b96a65c278818"),  # T25
    (10, "2c7e34291dba41872286fd3e95575fd44eb9d8d776b81a89b82e0e681e289514"),  # T26
    (19, "1b7ebc4f3208cd7af3c12d1a545194a5701b45771d4574d0a8c0e6889e85f176"),  # T27
    (15, "f238b74ac66dcc00885966633aa5c382d5b7464f03da57ff6d287af2571d5df7"),  # T28
    (29, "0432a4f95d2efe9aa0000547eebb23f6163b3f529641f0e751b93bc540a4a8d1"),  # T29
    (11, "b7284833e9732daaf51041d7d4da2aafdba78d0bc2895c42398b8832e98a031d"),  # T30
    (5, "524b9d05b7d4a7f9a59dd514a2b5a9f4d72dc2beaf1bc26ad0e03be5060e410d"),  # T31
    (21, "e817c4315a05659bfca9b60bb8c3da8be1c461bf3aeee512682ee4dd6428abcd"),  # T32
    (15, "c73a2e50fc79fc845e8be251b0db0b3f596f3b4572939f154d5b154ec706409a"),  # T33
    (3, "52ae6769b92a47fe01fd4c6fccabf08f46f6feadc1f9c3f051b6ead630a5dacc"),  # T34
    (11, "4d77df3807e5b883c8a651a448eade11d4361aa701bfb0e69fd60f8042a6dcfa"),  # T35
    (18, "5146d3688c71ac27f95ddd3a6e71d38f37310137045d72d01af1800e3799680c"),  # T36
    (13, "6a36d55e2672611e4328a36211af3abff500552abe377c92b8de6b4c49d90ee1"),  # T37
    (5, "0a59181f2ee047a658f45aff7083b37b5fb83291b5b98c8aa1c4662df57e2fe4"),  # T38
    (11, "28d763b32e8f2616f0c7fa61e3a3f692757ae401bbd5cea04cbe8fa98cf91374"),  # T39
    (8, "2dbdc0f5ec0bb4bc0e1bd20d5a8f4cf167cdb896665c8a14eca657de12dc4f22"),  # T40
    (11, "0a36d305eba2b63e6a6989e3d2f447dccb23a0e26b27197911b0217845e322cc"),  # T41
    (11, "2aada7be2cf937f7825b7604d25cf0a197db2458cc78436e4b154cf9e84574bb"),  # T42
    (5, "d0fe6b72d499a036263f1fa31e68a36394af7e3cb03f3252f71e65502d652b88"),  # T43
    (7, "8fc0a5d0931f13f177d7df9056b060e1d08926561cbeeba4cb53a6e74edbfa1a"),  # T44
    (9, "ac56656ee0f5d330809a143ff69d841805871b8952dc8b97a0740ee94b31d9eb"),  # T45
)

def assert_neutral(path, tokens=TOKENS):
    assert path.is_file(), f"Missing regular document: {path.name}"
    offenders = []
    for line_number, line in enumerate(path.read_text(encoding="utf-8").casefold().splitlines(), 1):
        for number, (length, expected) in enumerate(tokens, 1):
            for start in range(len(line) - length + 1):
                digest = sha256(line[start:start + length].encode()).hexdigest()
                if digest == expected:
                    offenders.append((line_number, f"T{number:02}"))
    assert not offenders, f"{path.name}: line/token {offenders}"

@pytest.mark.parametrize("relative", DOCUMENTS)
def test_documents_use_neutral_references(relative):
    assert_neutral(ROOT / relative)

@pytest.mark.parametrize("prefix", ("", "# Title\n", "| Role |\n|---|\n", "\n" * 1000))
@pytest.mark.parametrize("transform", ("lower", "upper", "title"))
def test_scan_rejects_synthetic_tokens_anywhere(tmp_path, prefix, transform):
    token = "private_fixture"
    tokens = ((len(token), sha256(token.encode()).hexdigest()),)
    page = tmp_path / "example.md"
    page.write_text(prefix + getattr(token, transform)(), encoding="utf-8")
    with pytest.raises(AssertionError, match="line/token") as error:
        assert_neutral(page, tokens)
    assert token not in str(error.value).casefold()

def test_scan_accepts_neutral_roles(tmp_path):
    page = tmp_path / "example.md"
    page.write_text("source archive; riser equipment library; approved jumper workbook", encoding="utf-8")
    assert_neutral(page)

@pytest.mark.parametrize("directory", (False, True))
def test_scan_rejects_missing_regular_files(tmp_path, directory):
    page = tmp_path / "missing.md"
    if directory:
        page.mkdir()
    with pytest.raises(AssertionError, match="Missing regular document"):
        assert_neutral(page)

"""Download helper for the API raw-data archive (network replaced by a stub)."""

import sys
import types

from digitalmodel.workflows.workflow_automation.APIRawData import (
    download_file_from_url,
)


def test_download_replaces_existing_file(tmp_path, monkeypatch, capsys):
    downloads = []

    def download(url, out):
        # The previous copy is removed before the new download starts.
        downloads.append((url, out, (tmp_path / "APIRawData.zip").exists()))
        (tmp_path / "APIRawData.zip").write_bytes(b"new")

    monkeypatch.setitem(sys.modules, "wget", types.SimpleNamespace(download=download))
    (tmp_path / "APIRawData.zip").write_bytes(b"old")
    url = "https://example.invalid/files/APIRawData.zip"

    download_file_from_url({"url": url, "download_to": str(tmp_path)})

    target = str(tmp_path) + "/APIRawData.zip"
    assert downloads == [(url, target, False)]
    assert (tmp_path / "APIRawData.zip").read_bytes() == b"new"
    out = capsys.readouterr().out
    assert f"Dowloading file: {target} .... COMPLETE" in out
    assert "Time Taken to download: " in out

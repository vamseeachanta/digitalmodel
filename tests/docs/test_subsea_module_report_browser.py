# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Headless checks for offline charts, comment round trips and narrow screens.

Run with unittest and the optional playwright package. SUBSEA_TEST_BROWSER may
select an installed Chromium executable; no solver or network access is used.
"""

import json
import os
from pathlib import Path
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/subsea/subsea-module-report.html"


class SubseaReportBrowserTests(unittest.TestCase):
    """Exercise the delivered HTML rather than a separately built interface."""

    def setUp(self):
        try:
            from playwright.sync_api import sync_playwright
        except ImportError:
            self.skipTest("Optional playwright package is required for browser checks")
        self.runtime = sync_playwright().start()
        self.addCleanup(self.runtime.stop)
        self.browser = self.runtime.chromium.launch(
            executable_path=os.environ.get("SUBSEA_TEST_BROWSER"), headless=True,
            args=["--no-sandbox"],
        )
        self.addCleanup(self.browser.close)
        self.context = self.browser.new_context(accept_downloads=True)
        self.context.set_offline(True)
        self.page = self.context.new_page()
        self.errors = []
        self.page.on("pageerror", lambda error: self.errors.append(str(error)))
        self.page.goto(REPORT.as_uri())

    def test_offline_charts_and_mobile_layout(self):
        self.assertEqual(self.page.locator("#length-chart polyline").count(), 1)
        before = self.page.locator("#length-readout").inner_text()
        self.page.locator("#length-slider").fill("60")
        self.assertNotEqual(before, self.page.locator("#length-readout").inner_text())
        self.assertIn("limit 1500.0 m", self.page.locator("#length-readout").inner_text())
        self.assertIn("NOT_EVALUATED", self.page.locator("#tension-readout").inner_text())
        self.page.select_option("#changes-since", "00")
        self.assertIn("No engineering changes", self.page.locator("#change-list").inner_text())
        artifacts = os.environ.get("SUBSEA_TEST_ARTIFACTS")
        if artifacts:
            self.page.screenshot(path=str(Path(artifacts) / "subsea-desktop.png"), full_page=True)
        self.page.set_viewport_size({"width": 390, "height": 844})
        self.assertTrue(self.page.evaluate("document.body.scrollWidth <= innerWidth"))
        if artifacts:
            self.page.screenshot(path=str(Path(artifacts) / "subsea-mobile.png"), full_page=True)
        self.assertEqual(self.errors, [])

    def test_comment_download_import_and_direct_save(self):
        self.page.fill("#comment-text", "Confirm the hang-off datum <review>.")
        self.page.click("#add-comment")
        self.assertIn("<review>", self.page.locator("#comment-list").inner_text())
        self.assertEqual(self.page.locator("#comment-list review").count(), 0)
        with tempfile.TemporaryDirectory() as directory:
            with self.page.expect_download() as event:
                self.page.click("#download-comments")
            target = Path(directory) / event.value.suggested_filename
            event.value.save_as(target)
            values = json.loads(target.read_text())
            self.assertEqual(values[0]["text"], "Confirm the hang-off datum <review>.")
            self.page.reload()
            self.page.set_input_files("#import-comments", str(target))
            self.page.wait_for_function("document.querySelectorAll('#comment-list li').length === 1")
        # Model the browser directory API, including existing-file readback.
        self.page.evaluate("""() => {
          window.savedComments = null;
          window.showDirectoryPicker = async () => ({getFileHandle: async () => ({
            getFile: async () => new Blob(['[{"section":"Design Data","text":"Existing note."}]']),
            createWritable: async () => ({write: async value => {window.savedComments = value;}, close: async () => {}})
          })});
        }""")
        self.page.click("#save-comments")
        self.page.wait_for_function("window.savedComments !== null")
        saved = json.loads(self.page.evaluate("window.savedComments"))
        self.assertEqual(len(saved), 2)
        self.assertEqual(saved[0]["text"], "Existing note.")
        self.assertEqual(self.errors, [])


if __name__ == "__main__":
    unittest.main()

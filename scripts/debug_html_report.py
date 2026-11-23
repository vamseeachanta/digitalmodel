"""Debug HTML report to identify why data is missing."""

import asyncio
from playwright.async_api import async_playwright
import json
import sys

async def debug_report(html_path):
    """Debug the HTML report and capture console errors."""

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()

        # Capture console messages
        console_messages = []
        errors = []

        page.on("console", lambda msg: console_messages.append({
            "type": msg.type,
            "text": msg.text,
            "location": msg.location
        }))

        page.on("pageerror", lambda exc: errors.append(str(exc)))

        # Navigate to the page
        try:
            await page.goto(f"file:///{html_path}", wait_until="networkidle")

            # Wait a bit for JavaScript to execute
            await page.wait_for_timeout(3000)

            # Check if plots are rendered
            plot_divs = await page.query_selector_all('[id^="plot"]')

            print(f"\n{'='*60}")
            print("HTML REPORT DIAGNOSIS")
            print(f"{'='*60}\n")

            print(f"Number of plot divs found: {len(plot_divs)}")

            # Check each plot
            for i in range(1, 6):
                plot_id = f"plot{i}"
                plot_elem = await page.query_selector(f"#{plot_id}")
                if plot_elem:
                    # Check if it has Plotly content
                    inner_html = await plot_elem.inner_html()
                    has_plotly_content = "plotly" in inner_html.lower() or "svg" in inner_html.lower()
                    print(f"\nPlot {i} ({plot_id}):")
                    print(f"  - Element exists: Yes")
                    print(f"  - Has Plotly content: {has_plotly_content}")
                    print(f"  - Inner HTML length: {len(inner_html)} chars")
                    if not has_plotly_content and len(inner_html) < 100:
                        print(f"  - Content preview: {inner_html[:200]}")
                else:
                    print(f"\nPlot {i} ({plot_id}): Element NOT FOUND")

            # Check for JavaScript errors
            print(f"\n{'='*60}")
            print("CONSOLE MESSAGES")
            print(f"{'='*60}\n")

            if console_messages:
                for msg in console_messages:
                    print(f"[{msg['type'].upper()}] {msg['text']}")
            else:
                print("No console messages captured")

            # Check for page errors
            print(f"\n{'='*60}")
            print("PAGE ERRORS")
            print(f"{'='*60}\n")

            if errors:
                for err in errors:
                    print(f"ERROR: {err}")
            else:
                print("No page errors captured")

            # Check if Plotly is loaded
            plotly_loaded = await page.evaluate("typeof Plotly !== 'undefined'")
            print(f"\n{'='*60}")
            print("PLOTLY STATUS")
            print(f"{'='*60}\n")
            print(f"Plotly library loaded: {plotly_loaded}")

            if plotly_loaded:
                plotly_version = await page.evaluate("Plotly.version")
                print(f"Plotly version: {plotly_version}")

            # Check data variables
            print(f"\n{'='*60}")
            print("DATA VARIABLES")
            print(f"{'='*60}\n")

            for i in range(1, 6):
                var_name = f"plot{i}Data"
                exists = await page.evaluate(f"typeof {var_name} !== 'undefined'")
                print(f"{var_name} exists: {exists}")

                if exists:
                    has_data = await page.evaluate(f"{var_name} && {var_name}.data && {var_name}.data.length > 0")
                    print(f"  - Has data: {has_data}")
                    if has_data:
                        data_length = await page.evaluate(f"{var_name}.data.length")
                        print(f"  - Number of traces: {data_length}")

        except Exception as e:
            print(f"\nERROR loading page: {e}")
            import traceback
            traceback.print_exc()

        finally:
            await browser.close()

if __name__ == "__main__":
    html_path = "D:/workspace-hub/digitalmodel/docs/reports/rao_qa/vessel_heave_rao_qa_report.html"
    asyncio.run(debug_report(html_path))

from __future__ import annotations

import sys
import types

from digitalmodel.orcaflex.reporting import ReportConfig, generate_orcaflex_report
from digitalmodel.orcaflex.reporting import sections


class _RangeGraph:
    def __init__(self):
        self.X = [0.0, 50.0, 100.0]
        self.Min = [10.0, 12.0, 11.0]
        self.Max = [20.0, 22.0, 21.0]
        self.Mean = [15.0, 17.0, 16.0]


class _Line:
    def __init__(self, name: str = "Line1"):
        self.name = name

    def StaticResult(self, *_args):
        return 100.0

    def TimeHistory(self, *_args):
        return [100.0, 110.0, 90.0]

    def RangeGraph(self, *_args):
        return _RangeGraph()


class _Vessel:
    def __init__(self, name: str = "Vessel1"):
        self.name = name

    def StaticResult(self, var: str):
        if var == "Z":
            return -8.0
        return 0.0

    def TimeHistory(self, *_args):
        return [0.0, 0.1, -0.1]


class _Buoy:
    pass


class _Buoy3D(_Buoy):
    pass


class _Buoy6D(_Buoy):
    pass


class _General:
    OrcaFlexVersion = "11.6"
    UnitsSystem = "SI"
    NumberOfStages = 2


class _Environment:
    WaterDepth = 100.0
    Density = 1.025
    WaveType = "JONSWAP"
    HsAtOrigin = 4.0
    WaveDirection = 180.0


class _Model:
    def __init__(self):
        self.simulationFileName = "fake.sim"
        self.general = _General()
        self.environment = _Environment()
        self.objects = [_Line(), _Vessel(), _Buoy3D(), _Buoy6D()]
        self.codeChecks = [types.SimpleNamespace(Name="CC1", Utilisation=0.8)]
        self.ModalAnalysisResults = [types.SimpleNamespace(Description="Mode 1", NaturalFrequency=0.2)]

    def LoadSimulation(self, _path: str):
        return None

    def SampleTimes(self):
        return [0.0, 1.0, 2.0]


def _install_fake_orcfxapi(monkeypatch):
    fake = types.SimpleNamespace(
        Line=_Line,
        Vessel=_Vessel,
        Buoy=_Buoy,
        Model=_Model,
        oeEndA=1,
        oeEndB=2,
    )
    monkeypatch.setitem(sys.modules, "OrcFxAPI", fake)


def test_sections_render_with_fake_model(monkeypatch):
    _install_fake_orcfxapi(monkeypatch)
    model = _Model()
    cfg = ReportConfig()

    rendered = [
        sections.build_model_summary(model, cfg.model_summary),
        sections.build_static_config(model, cfg.static_config),
        sections.build_time_series(model, cfg.time_series),
        sections.build_range_graphs(model, cfg.range_graphs),
        sections.build_code_check(model, cfg.code_check),
        sections.build_mooring_loads(model, cfg.mooring_loads),
        sections.build_modal_analysis(model, cfg.modal_analysis),
        sections.build_qa_summary(model, cfg.qa_summary),
    ]

    assert all(isinstance(item, str) and item for item in rendered)


def test_report_builder_integration(tmp_path, monkeypatch):
    _install_fake_orcfxapi(monkeypatch)

    sim_path = tmp_path / "A01_catenary_riser.sim"
    sim_path.write_text("placeholder", encoding="utf-8")
    output = tmp_path / "report.html"

    cfg = ReportConfig()
    cfg.code_check.enabled = True
    cfg.mooring_loads.enabled = True
    cfg.modal_analysis.enabled = True
    cfg.qa_summary.enabled = False

    generate_orcaflex_report(sim_path=sim_path, output_html=output, config=cfg)

    html = output.read_text(encoding="utf-8")
    assert "OrcaFlex Analysis Report" in html
    assert "A01_catenary_riser.sim" in html
    assert "https://cdn.plot.ly/plotly-2.27.0.min.js" in html
    assert "bootstrap" not in html.lower()

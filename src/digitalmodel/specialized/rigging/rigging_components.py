import io
import math

import pkgutil

from assetutilities.common.data import ReadFromExcel
from digitalmodel.solvers.fea_model.LineType_components import LineType
from digitalmodel.solvers.fea_model.line_components import Line

read_excel = ReadFromExcel()


def _as_excel_io(data):
    """Wrap packaged workbook bytes in a ``BytesIO`` for the excel reader.

    ``pkgutil.get_data`` returns raw bytes; pandas warns (and, under the
    test suite's ``filterwarnings = error``, raises) when bytes are passed
    straight to ``read_excel``. Wrapping in a file-like object is the
    documented way to read an in-memory workbook.
    """
    if isinstance(data, (bytes, bytearray)):
        return io.BytesIO(data)
    return data


# Subcategory labels that identify a (polyester round / twin-path) sling.
SLING_SUBCATEGORIES = ["twinpathslings", "polyester_endless_round"]
# Subcategory labels that identify a Crosby shackle grade (= loaded workbooks).
SHACKLE_SUBCATEGORIES = ["G2100", "G2110", "G2130"]


def _to_number(value):
    """Coerce vendor-sheet cells to plain Python ints/floats when numeric.

    Excel ints arrive as ``numpy.int64`` and floats as ``numpy.float64``;
    converting them keeps the emitted summary JSON/YAML deterministic and
    free of numpy-specific tags. Non-numeric / NaN cells pass through.
    """
    if value is None:
        return None
    try:
        if isinstance(value, bool):
            return value
        number = float(value)
    except (TypeError, ValueError):
        return value
    if math.isnan(number):
        return None
    if number.is_integer():
        return int(number)
    return number


class Slings:
    def __init__(self):

        self.twinpathslings_datafile = pkgutil.get_data(
            "digitalmodel", "data/slings/" + "MIS-ProductSheets_twinPathSlings.xlsx"
        )
        self.sling_data = self.get_vendor_data()

    def get_vendor_data(self, cfg=None):
        cfg_data = {"io": _as_excel_io(self.twinpathslings_datafile)}
        excel_data = read_excel.from_xlsx(cfg_data)
        twinpathslings_data = excel_data["Sheet1"]
        return {"twinpathslings": twinpathslings_data}

    def get_sling(self, cfg=None):
        sling = {}
        sling_model = {}
        if cfg["subcategory"] in SLING_SUBCATEGORIES:
            sling = self.get_twinpathslings(cfg)
            sling_model = self.get_twinpathslings_model(sling)

        return sling, sling_model

    def get_twinpathslings(self, cfg=None):
        sling_data = self.sling_data["twinpathslings"]
        sling = sling_data.loc[sling_data["part_number"] == cfg["part_number"]].to_dict(
            "records"
        )[0]
        return sling

    def get_twinpathslings_model(self, sling=None):
        # Build a clean, deterministic model dict from the REAL vendor
        # columns of the twinpathslings Sheet1. There is no ``diameter``
        # column for these polyester round slings, so we surface the rated
        # capacities, width range, unit weight, and design factor that the
        # catalog actually provides. (Previously this read a non-existent
        # ``diameter`` key and then recursed into itself.)
        sling = sling or {}

        def _get(key):
            return _to_number(sling.get(key))

        sling_model = {
            "part_number": _to_number(sling.get("part_number")),
            "wll_vertical_lb": _get("vertical_lb"),
            "wll_choker_lb": _get("choker_lb"),
            "wll_vertical_basket_90_deg_lb": _get("vertical_basket_90_deg_lb"),
            "wll_basket_60_deg_lb": _get("basket_hitnges_60_deg_lb"),
            "wll_basket_45_deg_lb": _get("basket_hitnges_45_deg_lb"),
            "weight_lb_per_ft": _get("w_lb_per_ft"),
            "min_width_in": _get("min_width_in"),
            "max_width_in": _get("max_width_in"),
            "design_factor": _get("design_factor"),
        }

        return sling_model

    def GetLineTypeProperties(self, Asset):
        cfg = Asset
        line_type = LineType(cfg)
        fea_data = line_type.get_orcaflex_properties()

        return fea_data

    def GetLineProperties(self, Asset):
        cfg = Asset
        line = Line(cfg)
        fea_data = line.get_orcaflex_properties()

        return fea_data


class Shackles:
    def __init__(self):

        self.G2100_datafile = pkgutil.get_data(
            "digitalmodel", "data/crosby/" + "204_subsea_shackles_g2100.xlsx"
        )
        self.G2110_datafile = pkgutil.get_data(
            "digitalmodel", "data/crosby/" + "204_subsea_shackles_g2110.xlsx"
        )
        self.G2130_datafile = pkgutil.get_data(
            "digitalmodel", "data/crosby/" + "26_shackles_bolt_type_anchor_g2130.xlsx"
        )
        self.shackle_data = self.get_vendor_data()

    def get_vendor_data(self, cfg=None):
        cfg_G2100_data = {"io": _as_excel_io(self.G2100_datafile)}
        excel_data = read_excel.from_xlsx(cfg_G2100_data)
        G2100_data = excel_data["Sheet1"]

        cfg_G2110_data = {"io": _as_excel_io(self.G2110_datafile)}
        excel_data = read_excel.from_xlsx(cfg_G2110_data)
        G2110_data = excel_data["Sheet1"]

        cfg_G2130_data = {"io": _as_excel_io(self.G2130_datafile)}
        excel_data = read_excel.from_xlsx(cfg_G2130_data)
        G2130_data = excel_data["Sheet1"]

        shackle_data = {"G2100": G2100_data, "G2110": G2110_data, "G2130": G2130_data}

        return shackle_data

    def get_shackle(self, cfg=None):
        shackle = {}
        # Shackles are keyed by Crosby grade (G2100/G2110/G2130), which are
        # exactly the keys of ``self.shackle_data``. Dispatch to the existing
        # ``get_shackle_model`` lookup. (Previously this tested for *sling*
        # subcategories and called a non-existent ``get_twinpathslings``.)
        if cfg["subcategory"] in SHACKLE_SUBCATEGORIES:
            shackle = self.get_shackle_model(cfg)

        return shackle

    def get_shackle_model(self, cfg=None):
        shackle_data = self.shackle_data[cfg["subcategory"]]
        shackle = shackle_data.loc[
            shackle_data["part_number"] == cfg["part_number"]
        ].to_dict("records")[0]
        # Coerce numpy scalars to plain Python so downstream summaries stay
        # deterministic and serialise cleanly to JSON/YAML.
        shackle = {key: _to_number(value) for key, value in shackle.items()}
        return shackle

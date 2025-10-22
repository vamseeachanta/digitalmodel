# Standard library imports
import glob
import logging
import math
import os
import re
from dataclasses import dataclass, field
from typing import Dict, Iterable, List, Optional, Tuple

# Third party imports
import pandas as pd  # noqa : F401
from assetutilities.common.data import ReadData, SaveData
from assetutilities.common.file_management import FileManagement
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.yml_utilities import WorkingWithYAML

# Reader imports
from digitalmodel.modules.aqwa.aqwa_utilities import AqwaUtilities

try:
    import numpy as np
except ImportError:  # pragma: no cover - numpy is required but guard for optional envs
    np = None  # type: ignore[assignment]

wwy = WorkingWithYAML()


fm = FileManagement()
au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()
white_space = " "


FLOAT_PATTERN = re.compile(
    r"[-+]?\d*\.?\d+(?:[Ee][-+]?\d+)?"
)


@dataclass
class DATFrequencyGrid:
    frequencies: List[float] = field(default_factory=list)
    periods: List[float] = field(default_factory=list)

    def as_array(self) -> "np.ndarray":  # pragma: no cover - thin wrapper
        if np is None:
            raise ImportError("NumPy is required to produce array output")
        return np.asarray(self.frequencies, dtype=float)


@dataclass
class DATDragMatrix:
    matrix: "np.ndarray"
    row_labels: List[str]
    column_labels: List[str]


@dataclass
class DATDeckOneMetadata:
    cog_element: Optional[int]
    cog_coordinates: Optional[Tuple[float, float, float]]
    raw_lines: List[str]


@dataclass
class DATInspectionResult:
    dat_path: str
    decks: Dict[int, List[str]]
    options: Dict[str, bool]
    frequency_grid: DATFrequencyGrid
    drag_matrix: Optional[DATDragMatrix]
    deck_one: DATDeckOneMetadata
    mass: float = 0.0
    inertia: Dict[str, float] = field(default_factory=dict)
    restoring: Dict[str, float] = field(default_factory=dict)


class AqwaDATFiles:
    def __init__(self):
        pass

    def router(self, cfg):
        self.process_all_data_categories(cfg)

    def process_all_data_categories(self, cfg):
        for dc_cfg in cfg["inputs"]:
            data_category = str(dc_cfg["data"]["category"])
            data_category = data_category.zfill(2)
            dc_data = self.get_dc_data(cfg, dc_cfg, data_category)
            self.write_dc_data(dc_data, data_category, dc_cfg, cfg)

    def get_dc_data(self, cfg, dc_cfg, data_category):

        dc_header = self.get_dc_header(data_category)

        dc_body_func = getattr(self, f"get_dc_{data_category}_body")
        dc_body = dc_body_func(cfg, dc_cfg)

        dc_footer = self.get_dc_footer(data_category)
        dc_data = dc_header + dc_body + dc_footer

        return dc_data

    def write_dc_data(self, dc_data, data_category, dc_cfg, cfg):
        output_directory = cfg["Analysis"]["file_management_output_directory"]
        file_prefix = dc_cfg.get("file_prefix", None)
        if file_prefix is None:
            file_prefix = ""
        file_name = file_prefix + data_category + "." + dc_cfg["file_extension"]
        io = os.path.join(output_directory, file_name)
        save_data_cfg = {"io": io, "data": dc_data}
        save_data.save_ascii_file_from_array(dc_data, io)

    def get_dc_header(self, data_category):
        file_name = f"{data_category}_header.deck"

        library_name = "digitalmodel"
        library_file_cfg = {
            "filename": f"base_configs/modules/aqwa/template_decks/{file_name}",
            "library_name": library_name,
        }

        file_name = wwy.get_library_filename(library_file_cfg)
        rd_cfg = {"io": file_name}
        file_array = rd.from_ascii_file_get_lines_as_string_arrays(rd_cfg)

        return file_array

    def get_dc_footer(self, data_category):
        file_name = f"{data_category}_footer.deck"

        library_name = "digitalmodel"
        library_file_cfg = {
            "filename": f"base_configs/modules/aqwa/template_decks/{file_name}",
            "library_name": library_name,
        }

        file_name = wwy.get_library_filename(library_file_cfg)
        rd_cfg = {"io": file_name}
        file_array = rd.from_ascii_file_get_lines_as_string_arrays(rd_cfg)

        return file_array

    def get_dc_01_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        body = []
        for body_item_idx in range(0, len(raw_data)):
            body_item = raw_data[body_item_idx]
            structure = body_item["structure"]
            node = body_item["node"]
            X = body_item["X"]
            X = f"{X:.3f}"
            Y = body_item["Y"]
            Y = f"{Y:.3f}"
            Z = body_item["Z"]
            Z = f"{Z:.3f}"

            body_item_str = f"{white_space:>1s}{white_space:>3s}{structure:>2d}{node:>5d}{white_space:>4s}{white_space:>5s}{X:>10s}{Y:>10s}{Z:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_03_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        body = []
        for body_item_idx in range(0, len(raw_data)):
            body_item = raw_data[body_item_idx]

            structure = body_item["structure"]
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'STRC':>4s}{white_space:>5s}{structure:>5d}"
            body.append(body_item_str)

            node = body_item["node"]
            mass = body_item["mass"]
            mass = f"{mass:.1f}"

            # For units of mass, the unit is in kg?
            # mass = f"{mass:.3e}"

            # For units of mass, the unit is in mT?
            body_item_str = f"{white_space:>1s}{white_space:>3s}{1:>2d}{white_space:>4s}{white_space:>5s}{node:>5d}{mass:>10s}"
            body.append(body_item_str)

            # For units of mass, the unit is in kg?
            # body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{white_space:>4s}{white_space:>5s}{node:>5d}{white_space:>4s}{white_space:>5s}{mass:>10s}"
            # body.append(body_item_str)

        return body

    def get_dc_04_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        element_type = dc_cfg["data"]["element_type"]
        body = []
        for body_item_idx in range(0, len(raw_data)):
            body_item = raw_data[body_item_idx]

            structure = body_item["structure"]
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'STRC':>4s}{white_space:>5s}{structure:>5d}"
            body.append(body_item_str)

            node = body_item["node"]
            Ixx = body_item.get("Ixx", 0)
            Ixx = f"{Ixx:.3e}"
            Ixy = body_item.get("Ixy", 0)
            Ixy = f"{Ixy:.3e}"
            Ixz = body_item.get("Ixz", 0)
            Ixz = f"{Ixz:.3e}"
            Iyy = body_item.get("Iyy", 0)
            Iyy = f"{Iyy:.3e}"
            Iyz = body_item.get("Iyz", 0)
            Iyz = f"{Iyz:.3e}"
            Izz = body_item.get("Izz", 0)
            Izz = f"{Izz:.3e}"

            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{white_space:>5s}{node:>5d}{Ixx:>10s}{Ixy:>10s}{Ixz:>10s}{Iyy:>10s}{Iyz:>10s}{Izz:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_06_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        element_type = dc_cfg["data"]["element_type"]
        body = []
        period_array = raw_data["period"]
        period_array.sort(reverse=True)
        for body_item_idx in range(0, len(period_array)):
            period = raw_data["period"][body_item_idx]
            frequency = 1 / period
            frequency = f"{frequency:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{body_item_idx+1:>5d}{body_item_idx+1:>5d}{frequency:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_07_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        element_type = dc_cfg["data"]["element_type"]
        body = []
        for body_item_idx in range(0, 6):
            if body_item_idx in [0, 1, 2, 5]:
                data_array = [0] * 6
            elif body_item_idx in [3]:
                data_array = [0] * 3 + [raw_data["added_damping"]["rxx"]] + [0] * 2
            elif body_item_idx in [4]:
                data_array = [0] * 4 + [raw_data["added_damping"]["ryy"]] + [0]

            data_array_str = [f"{data:.3e}" for data in data_array]
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{white_space:>5s}{body_item_idx+1:>5d}{data_array_str[0]:>10s}{data_array_str[1]:>10s}{data_array_str[2]:>10s}{data_array_str[3]:>10s}{data_array_str[4]:>10s}{data_array_str[5]:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_10_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        body = []

        directions = []
        for item in raw_data:
            element_type = item["element_type"]
            csv_file = item["csv"]
            is_file_valid, file_name = is_file_valid_func(csv_file)
            if not is_file_valid:
                logging.error(f"File {file_name} is not valid")
                raise ValueError(f"File {file_name} is not valid")

            df_item = pd.read_csv(file_name)
            directions += list(df_item["Direction"])

        directions = list(set(directions))
        direction_identifier_df = pd.DataFrame(columns=["Direction"], data=directions)
        direction_identifier_df.sort_values(by=["Direction"], inplace=True)
        direction_identifier_df["direction_identifier"] = [
            str(item) for item in list(range(1, len(directions) + 1))
        ]

        self.get_direction_data(body, direction_identifier_df)

        for item in raw_data:
            element_type = item["element_type"]
            csv_file = item["csv"]
            is_file_valid, file_name = is_file_valid_func(csv_file)
            if not is_file_valid:
                logging.error(f"File {file_name} is not valid")
                raise ValueError(f"File {file_name} is not valid")

            df_item = pd.read_csv(file_name)
            res_df = pd.merge(
                df_item, direction_identifier_df, how="inner", on=["Direction"]
            )
            scaling = item["scaling"]

            if element_type == "CFC":
                body = self.get_CFC_data(body, res_df, scaling)

            elif element_type == "WFC":
                body = self.get_WFC_data(body, res_df, scaling)

        return body

    def get_direction_data(self, body, direction_identifier_df):

        direction_index = 0
        for idx in range(0, len(direction_identifier_df)):
            direction_index = direction_index + 1
            direction = f"{direction_identifier_df.iloc[idx]['Direction']:.1f}"
            direction_identifier = direction_identifier_df.iloc[idx][
                "direction_identifier"
            ]
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'DIRN':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{direction:>10s}"
            body.append(body_item_str)

    def get_WFC_data(self, body, df_item, scaling):
        for idx in range(0, len(df_item)):

            direction_identifier = df_item.iloc[idx]["direction_identifier"]

            Cx = f"{df_item.iloc[idx]['Cx'] * scaling['Cx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cx:>10s}"
            body.append(body_item_str)

            Cy = f"{df_item.iloc[idx]['Cy'] * scaling['Cy']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cy:>10s}"
            body.append(body_item_str)

            Cz = f"{df_item.iloc[idx]['Cz'] * scaling['Cz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIFZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cz:>10s}"
            body.append(body_item_str)

            Cnx = f"{df_item.iloc[idx]['Cnx'] * scaling['Cnx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnx:>10s}"
            body.append(body_item_str)

            Cny = f"{df_item.iloc[idx]['Cny'] * scaling['Cny']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cny:>10s}"
            body.append(body_item_str)

            Cnz = f"{df_item.iloc[idx]['Cnz'] * scaling['Cnz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'WIRZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnz:>10s}"
            body.append(body_item_str)

        return body

    def get_CFC_data(self, body, df_item, scaling):
        for idx in range(0, len(df_item)):

            direction_identifier = df_item.iloc[idx]["direction_identifier"]

            Cx = f"{df_item.iloc[idx]['Cx'] * scaling['Cx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cx:>10s}"
            body.append(body_item_str)

            Cy = f"{df_item.iloc[idx]['Cy'] * scaling['Cy']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cy:>10s}"
            body.append(body_item_str)

            Cz = f"{df_item.iloc[idx]['Cz'] * scaling['Cz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CUFZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cz:>10s}"
            body.append(body_item_str)

            Cnx = f"{df_item.iloc[idx]['Cnx'] * scaling['Cnx']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURX':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnx:>10s}"
            body.append(body_item_str)

            Cny = f"{df_item.iloc[idx]['Cny'] * scaling['Cny']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURY':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cny:>10s}"
            body.append(body_item_str)

            Cnz = f"{df_item.iloc[idx]['Cnz'] * scaling['Cnz']:.3e}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{'CURZ':>4s}{direction_identifier:>5s}{direction_identifier:>5s}{Cnz:>10s}"
            body.append(body_item_str)

        return body

    def get_dc_11_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        body = []

        for item in raw_data:
            element_type = item["element_type"]

            if element_type == "CPRF":
                depth = item["depth"]
                if depth > 0:
                    logging.error("Depth must be negative")
                    raise ValueError("Depth must be negative")
                depth = f"{depth:.1f}"
                speed = item["speed"]
                speed = f"{speed:.2f}"
                direction = item["direction"]
                direction = f"{direction:.1f}"
                body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{depth:>10s}{speed:>10s}{direction:>10s}"

            elif element_type == "WIND":
                speed = item["speed"]
                speed = f"{speed:.2f}"
                direction = item["direction"]
                direction = f"{direction:.1f}"
                body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{element_type:>4s}{speed:>10s}{direction:>10s}"

            body.append(body_item_str)

        return body

    def get_dc_15_body(self, cfg, dc_cfg):
        if dc_cfg["function"] is None:
            body = self.get_dc_15_body_generic(cfg, dc_cfg)
        else:
            dc_15_body_func = getattr(self, f"get_dc_15_{dc_cfg['function']}")
            body = dc_15_body_func(cfg, dc_cfg)

            body = self.get_dc_15_wlng(cfg, dc_cfg)

        return body

    def get_dc_15_body_generic(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        analysis_root_folder = cfg["Analysis"]["analysis_root_folder"]
        directory = raw_data["directory"]
        filename_pattern = raw_data["filename_pattern"]
        file_path = os.path.join(analysis_root_folder, directory)
        filenames = glob.glob(file_path + "/" + filename_pattern)
        position_columns = [
            "POSITION OF COG_in X direction",
            "POSITION OF COG_in Y direction",
            "POSITION OF COG_in Z direction",
            "POSITION OF COG_about X axis",
            "POSITION OF COG_about Y axis",
            "POSITION OF COG_about Z axis",
        ]
        all_structure_pos = []
        for filename in filenames:
            df = pd.read_csv(filename)
            df.iloc[-1][position_columns]
            structure_pos = df.iloc[-1][position_columns].values.flatten().tolist()
            all_structure_pos.append(structure_pos)

        body = []
        for pos_idx in range(0, len(all_structure_pos)):
            structure_pos = all_structure_pos[pos_idx]
            structure_tag = f"POS{pos_idx+1}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{structure_tag:>4s}{white_space:>5s}{white_space:>5s}{structure_pos[0]:>10.4f}{structure_pos[1]:>10.4f}{structure_pos[2]:>10.4f}{structure_pos[3]:>10.4f}{structure_pos[4]:>10.4f}{structure_pos[5]:>10.4f}"
            body.append(body_item_str)

        for pos_idx in range(0, len(all_structure_pos)):
            structure_vel = [0] * 6
            structure_tag = f"VEL{pos_idx+1}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{structure_tag:>4s}{white_space:>5s}{white_space:>5s}{structure_vel[0]:>10.4f}{structure_vel[1]:>10.4f}{structure_vel[2]:>10.4f}{structure_vel[3]:>10.4f}{structure_vel[4]:>10.4f}{structure_vel[5]:>10.4f}"
            body.append(body_item_str)

        return body

    def get_dc_15_wlng(self, cfg, dc_cfg):
        custom_structures = ["d_4_cog", "d_7_cog"]
        raw_data = dc_cfg["data"]["raw"]
        analysis_root_folder = cfg["Analysis"]["analysis_root_folder"]
        directory = raw_data["directory"]
        filename_pattern = raw_data["filename_pattern"]
        file_path = os.path.join(analysis_root_folder, directory)
        filenames = glob.glob(file_path + "/" + filename_pattern)
        position_columns = [
            "POSITION OF COG_in X direction",
            "POSITION OF COG_in Y direction",
            "POSITION OF COG_in Z direction",
            "POSITION OF COG_about X axis",
            "POSITION OF COG_about Y axis",
            "POSITION OF COG_about Z axis",
        ]
        all_structure_pos = []
        for filename in filenames:
            df = pd.read_csv(filename)
            df.iloc[-1][position_columns]
            structure_pos = df.iloc[-1][position_columns].values.flatten().tolist()
            basename = os.path.basename(filename)
            if any(
                custom_structure in basename for custom_structure in custom_structures
            ):
                structure_pos = structure_pos[0:3] + [0] * 3
            all_structure_pos.append(structure_pos)

        body = []
        for pos_idx in range(0, len(all_structure_pos)):
            structure_pos = all_structure_pos[pos_idx]
            structure_tag = f"POS{pos_idx+1}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{structure_tag:>4s}{white_space:>5s}{white_space:>5s}{structure_pos[0]:>10.4f}{structure_pos[1]:>10.4f}{structure_pos[2]:>10.4f}{structure_pos[3]:>10.4f}{structure_pos[4]:>10.4f}{structure_pos[5]:>10.4f}"
            body.append(body_item_str)

        for pos_idx in range(0, len(all_structure_pos)):
            structure_vel = [0] * 6
            structure_tag = f"VEL{pos_idx+1}"
            body_item_str = f"{white_space:>1s}{white_space:>3s}{white_space:>2s}{structure_tag:>4s}{white_space:>5s}{white_space:>5s}{structure_vel[0]:>10.4f}{structure_vel[1]:>10.4f}{structure_vel[2]:>10.4f}{structure_vel[3]:>10.4f}{structure_vel[4]:>10.4f}{structure_vel[5]:>10.4f}"
            body.append(body_item_str)

        return body

    def get_dc_18_body(self, cfg, dc_cfg):
        raw_data = dc_cfg["data"]["raw"]
        element_type = dc_cfg["data"]["element_type"]
        body = []

        structure = raw_data["structure"]
        node_array = raw_data["nodes"]
        for node in node_array:
            body_item_idx = node_array.index(node)
            node = node_array[body_item_idx]

            body_item_str = f"{white_space:>1s}{white_space:>3s}{body_item_idx+1:>2d}{element_type:>4s}{structure:>5d}{node:>5d}"
            body.append(body_item_str)

        return body

    # ---------------------------------------------------------------------
    # New inspection helpers for viscous damping workflow
    # ---------------------------------------------------------------------

    def inspect_dat_file(self, dat_path: str) -> DATInspectionResult:
        """Inspect an AQWA .dat file and return parsed deck metadata."""

        decks = self._read_decks_from_file(dat_path)
        options = self._parse_options(decks)
        frequency_grid = self._parse_deck6(decks.get(6, []))
        drag_matrix = self._parse_deck7(decks.get(7, []))
        deck_one = self._parse_deck1(decks.get(1, []))
        total_mass = self._parse_deck3(decks.get(3, []))
        inertia_map = self._parse_deck4(decks.get(4, []))
        if total_mass:
            inertia_map.setdefault("I33", total_mass)
        restoring_map = self._parse_deck5(decks.get(5, []))

        return DATInspectionResult(
            dat_path=dat_path,
            decks=decks,
            options=options,
            frequency_grid=frequency_grid,
            drag_matrix=drag_matrix,
            deck_one=deck_one,
            mass=total_mass,
            inertia=inertia_map,
            restoring=restoring_map,
        )

    def validate_ldrg_option(self, dat_path: str) -> None:
        """Raise an error if OPTIONS LDRG is not enabled in the .dat file."""

        inspection = self.inspect_dat_file(dat_path)
        if not inspection.options.get("LDRG", False):
            raise ValueError(
                f"OPTIONS LDRG not enabled in AQWA input file: {dat_path}"
            )

    def validate_cog_element(self, dat_path: str, expected_element: int = 98000) -> None:
        """Raise an error if the expected CoG element is missing from Deck 1."""

        inspection = self.inspect_dat_file(dat_path)
        if inspection.deck_one.cog_element != expected_element:
            raise ValueError(
                "CoG element validation failed. "
                f"Expected element {expected_element} not located in Deck 1 for {dat_path}."
            )

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _read_decks_from_file(self, dat_path: str) -> Dict[int, List[str]]:
        decks: Dict[int, List[str]] = {}
        current_deck: Optional[int] = None
        deck_pattern = re.compile(r"DECK\s+(\d+)", re.IGNORECASE)

        with open(dat_path, "r", encoding="utf-8", errors="ignore") as handle:
            for raw_line in handle:
                line = raw_line.rstrip("\n")
                deck_match = deck_pattern.search(line)
                if deck_match:
                    current_deck = int(deck_match.group(1))
                    decks[current_deck] = []
                    continue
                if current_deck is None:
                    continue
                decks[current_deck].append(line)

        return decks

    def _parse_options(self, decks: Dict[int, List[str]]) -> Dict[str, bool]:
        options: Dict[str, bool] = {}
        for lines in decks.values():
            for line in lines:
                if "OPTIONS" in line.upper():
                    tokens = line.upper().split()
                    for token in tokens:
                        if token in {"OPTIONS", "OPTION"}:
                            continue
                        options[token] = True
        return options

    def _parse_deck6(self, deck_lines: Iterable[str]) -> DATFrequencyGrid:
        grid = DATFrequencyGrid()
        for line in deck_lines:
            tokens = line.split()
            if not tokens:
                continue
            # Frequency is typically the last entry on the line
            try:
                freq = float(tokens[-1])
            except (ValueError, IndexError):
                continue

            if freq <= 0:
                period = math.inf
            else:
                period = 1.0 / freq

            grid.frequencies.append(freq)
            grid.periods.append(period)

        return grid

    def _parse_deck7(self, deck_lines: Iterable[str]) -> Optional[DATDragMatrix]:
        if np is None:
            return None

        dof_labels = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
        matrix = np.zeros((len(dof_labels), len(dof_labels)), dtype=float)
        row_labels: List[str] = []

        for line in deck_lines:
            tokens = line.split()
            if len(tokens) < 8:
                continue
            label = tokens[0].strip()
            try:
                row_idx = int(tokens[1]) - 1
            except ValueError:
                continue
            try:
                values = [float(value) for value in tokens[2:8]]
            except ValueError:
                continue
            if 0 <= row_idx < len(dof_labels):
                matrix[row_idx, : len(values)] = values
                row_labels.append(f"{label}_{row_idx + 1}")

        if not row_labels:
            return None

        return DATDragMatrix(matrix=matrix, row_labels=row_labels, column_labels=dof_labels)

    def _parse_deck3(self, deck_lines: Iterable[str]) -> float:
        total_mass = 0.0
        for line in deck_lines:
            floats = FLOAT_PATTERN.findall(line)
            if not floats:
                continue
            try:
                mass_val = float(floats[-1])
            except ValueError:
                continue
            total_mass += mass_val
        return total_mass

    def _parse_deck4(self, deck_lines: Iterable[str]) -> Dict[str, float]:
        inertia_map = {"I44": 0.0, "I55": 0.0, "I66": 0.0}
        for line in deck_lines:
            floats = FLOAT_PATTERN.findall(line)
            if len(floats) < 6:
                continue
            try:
                Ixx, _, _, Iyy, _, Izz = [float(val) for val in floats[-6:]]
            except ValueError:
                continue
            inertia_map["I44"] += Ixx
            inertia_map["I55"] += Iyy
            inertia_map["I66"] += Izz
        return inertia_map

    def _parse_deck5(self, deck_lines: Iterable[str]) -> Dict[str, float]:
        if np is None:
            return {}

        floats: List[float] = []
        for line in deck_lines:
            floats.extend(
                float(value)
                for value in FLOAT_PATTERN.findall(line)
                if self._is_float(value)
            )

        if len(floats) < 36:
            return {}

        matrix = np.asarray(floats[:36], dtype=float).reshape(6, 6)
        return {
            "C33": matrix[2, 2],
            "C44": matrix[3, 3],
            "C55": matrix[4, 4],
        }

    def _is_float(self, token: str) -> bool:
        try:
            float(token)
            return True
        except ValueError:
            return False

    def _parse_deck1(self, deck_lines: Iterable[str]) -> DATDeckOneMetadata:
        lines = list(deck_lines)
        cog_element = None
        cog_coordinates: Optional[Tuple[float, float, float]] = None

        for line in lines:
            ints = re.findall(r"\d+", line)
            if not ints:
                continue
            element_id = int(ints[0])
            if element_id == 98000:
                cog_element = element_id
                floats = FLOAT_PATTERN.findall(line)
                if len(floats) >= 3:
                    try:
                        coords: Tuple[float, float, float] = tuple(
                            float(value) for value in floats[-3:]
                        )
                        cog_coordinates = coords
                    except ValueError:
                        cog_coordinates = None
                break

        return DATDeckOneMetadata(
            cog_element=cog_element,
            cog_coordinates=cog_coordinates,
            raw_lines=lines,
        )

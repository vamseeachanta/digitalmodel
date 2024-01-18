import copy
import os
import re
from collections import OrderedDict

from assetutilities.common.saveData import saveDataYaml
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.utilities import add_cwd_to_filename


class OrcInstallation:
    def __init__(self):
        pass

    def create_model_for_water_depth(self, cfg):
        self.cfg = cfg

        cfg_str = cfg["structure"]
        model_file = OrderedDict({})

        #     Read in the base model file(s)
        ref_model_file_name = cfg_str["reference_model_file"]
        ref_model_file_name = add_cwd_to_filename(ref_model_file_name, os.getcwd())
        ref_model_file = OrderedDict(ymlInput(ref_model_file_name))
        ref_elevation_filename = cfg_str["reference_elevation_file"]
        ref_elevation_filename = add_cwd_to_filename(
            ref_elevation_filename, os.getcwd()
        )
        ref_elevation_file = OrderedDict(ymlInput(ref_elevation_filename))

        model_file.update({"BaseFile": cfg_str["BaseFile"]})

        #     Generate model for each elevation
        for delta_elevation in cfg_str["delta_elevations"]:
            orc_object = "6DBuoys"
            self.update_elevation_for_orc_object(
                cfg_str, model_file, ref_elevation_file, delta_elevation, orc_object
            )
            orc_object = "3DBuoys"
            self.update_elevation_for_orc_object(
                cfg_str, model_file, ref_elevation_file, delta_elevation, orc_object
            )

            orc_object = "Lines"
            self.update_lines_for_installation(
                cfg_str, model_file, ref_model_file, delta_elevation, orc_object
            )

            elevation_for_file_name = int(
                round(
                    ref_elevation_file["6DBuoys"][cfg_str["6DBuoys"][0]]["InitialZ"]
                    + delta_elevation
                )
            )
            elevation_str = str(elevation_for_file_name).zfill(
                5 if delta_elevation < 0 else 4
            )

            file_base_name = "_el_" + elevation_str + "m"
            file_name = os.path.join(
                cfg["Analysis"]["analysis_root_folder"], file_base_name
            )
            saveDataYaml(model_file, file_name, default_flow_style="OrderedDumper")

            # Generate another new model with appropriate mudmat orientation
            model_str_orientation = OrderedDict({})
            model_str_orientation.update({"BaseFile": file_base_name + ".yml"})
            model_str_orientation.update(
                {"6DBuoys": {cfg_str["6DBuoys"][0]: {"InitialRotation3": 0}}}
            )

            file_base_name = "_el_" + elevation_str + "m" + "_str_orentation"
            file_name = os.path.join(
                cfg["Analysis"]["analysis_root_folder"], file_base_name
            )
            saveDataYaml(
                model_str_orientation, file_name, default_flow_style="OrderedDumper"
            )

            # Generate new model with appropriate final model name
            model_file = OrderedDict({})
            model_file.update({"BaseFile": file_base_name + ".yml"})
            file_base_name = "el_" + elevation_str + "m"
            file_name = os.path.join(
                cfg["Analysis"]["analysis_root_folder"], file_base_name
            )
            saveDataYaml(model_file, file_name, default_flow_style="OrderedDumper")

        return self.cfg

    def update_lines_for_installation(
        self, cfg_str, model_file, ref_model_file, delta_elevation, orc_object
    ):
        if orc_object in cfg_str or len(cfg_str[orc_object]) > 0:
            for line in cfg_str[orc_object]:
                line_dict = OrderedDict({})
                for line_item_key in line:
                    if "name" in line_item_key:
                        pass
                    elif "TargetSegmentLength" in line_item_key:
                        line_dict.update({line_item_key: line[line_item_key]})
                    elif "End" in line_item_key:
                        ref_end_elevation = round(
                            ref_model_file[orc_object][line["name"]][line_item_key], 1
                        )
                        line_dict.update(
                            {line_item_key: ref_end_elevation + delta_elevation}
                        )
                    elif "length_index" in line_item_key:
                        length_index = line[line_item_key]

                        ref_line_length = round(
                            ref_model_file[orc_object][line["name"]]["Sections"][
                                length_index
                            ]["Length"],
                            1,
                        )
                        line_length = ref_line_length + abs(delta_elevation)
                        line_dict.update(
                            {"Length[" + str(length_index) + "]": line_length}
                        )
                    else:
                        raise KeyError(
                            "Key not found in line item. Programming/library updates required."
                        )

                model_file.update({orc_object: {line["name"]: line_dict}})

    def update_elevation_for_orc_object(
        self, cfg_str, model_file, ref_elevation_file, delta_elevation, orc_object
    ):
        if orc_object in cfg_str or len(cfg_str[orc_object]) > 0:
            for buoy in cfg_str[orc_object]:
                reference_elevation = round(
                    ref_elevation_file[orc_object][buoy]["InitialZ"], 1
                )
                model_file.update(
                    {
                        orc_object: {
                            buoy: {"InitialZ": reference_elevation + delta_elevation}
                        }
                    }
                )

    def create_installation_depth_model(self, cfg=None):
        if cfg is None:
            raise ValueError("cfg is None")

        model_file = OrderedDict(ymlInput(cfg["reference_elevation_file"]))
        file_directory = os.path.dirname(cfg["reference_elevation_file"])

        for delta_elevation in cfg["delta_elevations"]:
            model_file_updated = copy.deepcopy(model_file)
            model_file_updated["6DBuoys"][cfg["structure"]][
                "InitialZ"
            ] -= delta_elevation
            model_file_updated["3DBuoys"][cfg["masterlink"]][
                "InitialZ"
            ] -= delta_elevation
            model_file_updated["Lines"][cfg["crane_wire"]]["EndBZ"] -= delta_elevation
            model_file_updated["Lines"][cfg["intermediate_sling"]][
                "EndBZ"
            ] -= delta_elevation
            model_file_updated["Lines"][cfg["crane_wire"]][
                "Length[2]"
            ] += delta_elevation

            file_name = (
                os.path.join(
                    file_directory,
                    cfg["output_basefile"]
                    + "_"
                    + str(
                        -int(
                            model_file_updated["6DBuoys"][cfg["structure"]]["InitialZ"]
                        )
                    ),
                )
                + "m"
            )
            saveDataYaml(
                model_file_updated, file_name, default_flow_style="OrderedDumper"
            )

    def create_installation_depth_model2(self, cfg=None):
        # For this function, the entire Line object should be defined to avoid serious redefining errors else Orcaflex will assign default LineType to the undefined parameters
        if cfg is None:
            raise ValueError("cfg is None")

        model_file = OrderedDict(ymlInput(cfg["reference_elevation_file"]))
        file_directory = os.path.dirname(cfg["reference_elevation_file"])

        for delta_elevation in cfg["delta_elevations"]:
            model_file_updated = copy.deepcopy(model_file)
            model_file_updated["6DBuoys"][cfg["structure"]][
                "InitialZ"
            ] -= delta_elevation
            model_file_updated["3DBuoys"][cfg["masterlink"]][
                "InitialZ"
            ] -= delta_elevation
            model_file_updated["Lines"][cfg["crane_wire"]]["EndBZ"] -= delta_elevation
            if cfg["intermediate_sling"] is not None:
                model_file_updated["Lines"][cfg["intermediate_sling"]][
                    "EndBZ"
                ] -= delta_elevation
            model_file_updated["Lines"][
                # TODO Covert this index into array parameter in yaml input file
                cfg["crane_wire"]
            ]["Sections"][1]["Length"] += delta_elevation

            file_name = (
                os.path.join(
                    file_directory,
                    cfg["output_basefile"]
                    + "_"
                    + str(
                        -int(
                            model_file_updated["6DBuoys"][cfg["structure"]]["InitialZ"]
                        )
                    ),
                )
                + "m"
            )
            saveDataYaml(
                model_file_updated, file_name, default_flow_style="OrderedDumper"
            )

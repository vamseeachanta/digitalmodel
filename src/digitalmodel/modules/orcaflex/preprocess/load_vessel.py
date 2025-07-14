import os
import argparse
try:
    import OrcFxAPI
except Exception:
    raise RuntimeError("OrcaFlex license not available. Run on different computer")
import logging

from assetutilities.common.utilities import is_file_valid_func


class LoadVessel:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg['orcaflex']['preprocess']['load_vessel']['input_prgram'] == 'aqwa':
            self.aqwa_groups(cfg)
        
        return cfg
    
    def aqwa_groups(self, cfg):
        load_vessel_cfg = cfg['preprocess'].copy()
        groups = load_vessel_cfg['load_vessel']['groups']
        for group in groups:
            self.aqwa_group_item(cfg, group)

    def aqwa_group_item(self, cfg, group):
        '''
        AQWA import on to a template with existing data
        '''

        parser = argparse.ArgumentParser()
        parser.add_argument("filePath", help="AQWA results file to import.")
        args = parser.parse_args()

        input_file = group['input_file']
        output_template = group['output_template']
        output_file = group['output_file']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        file_is_valid, input_file = is_file_valid_func(input_file, analysis_root_folder)

        if not file_is_valid:
            raise Exception(f"File not found: {file_name}")

        file_is_valid, output_template = is_file_valid_func(output_template, analysis_root_folder)
        
        if not file_is_valid:
            raise Exception(f"Output Template for vessel import not found: {file_name}")
        
        model = OrcFxAPI.Model(output_template, threadCount=1)

        importFileType = OrcFxAPI.iftAQWA
        # vt = model.CreateObject(OrcFxAPI.otVesselType)
        vt_names = group['vt_names']

        bodyMaps = []
        for vt_name in vt_names:
            bodyMaps.append(OrcFxAPI.VesselTypeDataDiffractionImportBodyMap(vt_name, "Draught1"))
        bodyMaps = tuple(bodyMaps)

        multibodyGroupName = group['multibodyGroupName']
        requestedData = (
            OrcFxAPI.vdtStructure,
            OrcFxAPI.vdtLoadRAOs,
            OrcFxAPI.vdtStiffnessAddedMassDamping,
            # OrcFxAPI.vdtOtherDamping,
            OrcFxAPI.vdtDisplacementRAOs,
            OrcFxAPI.vdtNewmanQTFs,
            OrcFxAPI.vdtFullQTFs,
            # OrcFxAPI.vdtSymmetry,
            # OrcFxAPI.vdtDrawing
        )
        calculationMethods = ("lmHaskind",)

        success, messages = model.ImportVesselTypeData(
            input_file,
            importFileType,
            bodyMaps,
            calculationMethods,
            vt_names,
            multibodyGroupName,
            requestedData,
            clearExistingData=False,
        )

        logging.warning(f"AQWA data import: {messages}")

        if success:
            output_file_name = os.path.join(analysis_root_folder, output_file)
            # model.SaveData(args.filePath.replace(".lis", ".dat"))
            model.SaveData(output_file_name)
        else:
            logging.error(f"Failed to import AQWA data: {messages}")
            raise Exception(f"Failed to import AQWA data: {input_file}")

import os
import pandas as pd
import numpy as np
from pathlib import Path

try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")


from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcModalAnalysis:

    def __init__(self):
        self.modes_df_columns = [
            'modeIndex', 'name', 'node', 'dof', 'shapeWrtGlobal'
        ]
        self.modes_summary_df_columns = [
            'modeIndex', 'period', 'name', 'abs_max_dof', 'max_dof_values',
            'max_dof_nodes', 'max_dof_percentages', 'modes_selected'
        ]
        self.all_file_summary_df_columns = ['Label'
                                           ] + self.modes_summary_df_columns

    def run_modal_analysis(self, cfg=None):
        self.cfg = cfg
        self.all_file_summary = {}
        self.all_file_selected_modes = {}
        if cfg['default']['Analysis']['Analyze']['modal']:
            files = self.cfg['Files'] if 'Files' in self.cfg else []
            for file in files:
                all_modes_summary_df, selected_modes = self.run_by_file(
                    file=file['Name'])
                self.all_file_summary.update(
                    {file['Label']: all_modes_summary_df})
                self.all_file_selected_modes.update(
                    {file['Label']: selected_modes})

            self.summarize_all_files()

        return self.cfg

    def summarize_all_files(self):
        files = self.cfg['Files'] if 'Files' in self.cfg else []
        dof_analysis = self.cfg['default']['Analysis']['Analyze']['modal'][
            'dof_analysis']['dofs']
        for dof in dof_analysis:
            all_file_summary_df = pd.DataFrame(
                columns=self.all_file_summary_df_columns)
            for file in files:
                selected_modes_dof_df = self.all_file_selected_modes[
                    file['Label']][dof]

                for row_i in range(0, len(selected_modes_dof_df)):
                    all_file_summary_df.loc[len(all_file_summary_df.index)] = [
                        file['Label']
                    ] + all_file_summary_df.iloc[row_i].tolist()
            file_name = self.cfg.Analysis[
                'result_folder'] + "Data\\" + self.cfg.Analysis[
                    'file_name_for_overwrite'] + f'_all_file_summary_{dof}.csv'
            all_file_summary_df.to_csv(file_name, index=False)

    def run_by_file(self, file=None):
        print(f"Processing file: {file} ... START")
        model = OrcFxAPI.Model()
        model.LoadData(file)
        model.CalculateStatics()
        ou.save_sim_file(model, file)
        lastMode = self.cfg['default']['Analysis']['Analyze']['modal'][
            'lastMode']
        spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=True,
                                                   lastMode=lastMode)

        modes = OrcFxAPI.Modes(model, spec)

        all_modes_summary_df, selected_modes = self.save_mode_shapes(
            modes, file)

        return all_modes_summary_df, selected_modes

    def explore_mode_shapes(self, modes):
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
        for modeIndex in range(modes.modeCount):

            details = modes.modeDetails(modeIndex)
            print(details.modeNumber)
            print(details.period)
            # Nodal 3 dof displacements
            print(details.shapeWrtGlobal)
            # TODO Investigate why same value for all modes?
            print(details.percentageInAxialDirection)

            for dofIndex in range(modes.dofCount):
                print(f"{modes.owner[dofIndex].name} node {modes.nodeNumber[dofIndex]:2} " \
                f"dof {modes.dof[dofIndex]}: {details.shapeWrtGlobal[dofIndex]}")

    def save_mode_shapes(self, modes, file):
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
        all_modes_df = pd.DataFrame(columns=self.modes_df_columns)

        all_modes_summary_df = pd.DataFrame(
            columns=self.modes_summary_df_columns)
        for modeIndex in range(modes.modeCount):
            print(f"Processing mode {modeIndex} ... START")
            mode_df = pd.DataFrame(columns=self.modes_df_columns).copy()
            details = modes.modeDetails(modeIndex)
            for dofIndex in range(modes.dofCount):
                name = modes.owner[dofIndex].name
                node = modes.nodeNumber[dofIndex]
                dof = modes.dof[dofIndex]
                shapeWrtGlobal = details.shapeWrtGlobal[dofIndex]
                mode_array = [modeIndex, name, node, dof, shapeWrtGlobal]
                mode_df.loc[len(mode_df.index)] = mode_array

            all_modes_df = pd.concat([all_modes_df, mode_df], ignore_index=True)

            mode_summary_df = self.get_mode_summary(mode_df)
            mode_summary_df['period'] = details.period
            mode_summary_df['modeIndex'] = modeIndex

            all_modes_summary_df = pd.concat(
                [all_modes_summary_df, mode_summary_df], ignore_index=True)
            print(f"Processing mode {modeIndex} ... COMPLETE")

        file_name = self.cfg.Analysis['result_folder'] + "Data\\" + Path(
            file).stem + '_modes.csv'
        if self.cfg['default']['Analysis']['Analyze']['modal']['mode_shapes']:
            all_modes_df.to_csv(file_name, index=False)
        all_modes_summary_df, selected_modes = self.save_summary_files(
            file, all_modes_summary_df)
        return all_modes_summary_df, selected_modes

    def save_summary_files(self, file, all_modes_summary_df):
        file_name = self.cfg.Analysis['result_folder'] + "Data\\" + Path(
            file).stem + '_modes_summary.csv'
        all_modes_summary_df.sort_values(by=['name', 'modeIndex'], inplace=True)
        all_modes_summary_df.to_csv(file_name, index=False)
        dof_analysis = self.cfg['default']['Analysis']['Analyze']['modal'][
            'dof_analysis']['dofs']

        selected_modes = {}
        for dof in dof_analysis:
            file_name = self.cfg.Analysis['result_folder'] + "Data\\" + Path(
                file).stem + f'_modes_summary_{dof}.csv'

            selected_file_index = []
            for i in range(0, len(all_modes_summary_df)):
                modes_selected = all_modes_summary_df.iloc[i]['modes_selected']
                if modes_selected[dof]:
                    selected_file_index.append(i)

            selected_modes_dof_summary_df = all_modes_summary_df.iloc[
                selected_file_index].copy()
            selected_modes_dof_summary_df.reset_index(inplace=True)
            selected_modes_dof_summary_df.to_csv(file_name, index=False)

            selected_modes.update({dof: selected_modes_dof_summary_df})
        return all_modes_summary_df, selected_modes

    def get_mode_summary(self, mode_df) -> pd.DataFrame:
        mode_summary_df = pd.DataFrame(columns=self.modes_summary_df_columns)
        ObjectNames = self.cfg['default']['Analysis']['Analyze']['modal'][
            'ObjectName']

        for ObjectName in ObjectNames:
            ObjectName_mode_df = mode_df[mode_df['name'] == ObjectName].copy()
            max_dof_values, max_dof_nodes, max_dof_percentage, abs_max_dof, modes_selected = self.get_mode_dof_responses(
                ObjectName_mode_df)

            ObjectName_summary = [
                None, None, ObjectName, abs_max_dof, max_dof_values,
                max_dof_nodes, max_dof_percentage, modes_selected
            ]
            mode_summary_df.loc[len(mode_summary_df.index)] = ObjectName_summary

        return mode_summary_df

    def get_mode_dof_responses(self, ObjectName_mode_df):
        dofs = list(ObjectName_mode_df['dof'].unique())
        max_dof_values = {}
        max_dof_nodes = {}
        for dof in dofs:
            ObjectName_mode_dof_df = ObjectName_mode_df[
                ObjectName_mode_df['dof'] == dof]
            dof_idxmax = ObjectName_mode_dof_df.shapeWrtGlobal.idxmax()
            dof_idxmin = ObjectName_mode_dof_df.shapeWrtGlobal.idxmin()
            mode_dof_max = ObjectName_mode_df.shapeWrtGlobal.loc[dof_idxmax]
            mode_dof_min = ObjectName_mode_df.shapeWrtGlobal.loc[dof_idxmin]

            if abs(mode_dof_max) > abs(mode_dof_min):
                shapeWrtGlobal = ObjectName_mode_df.shapeWrtGlobal.loc[
                    dof_idxmax]
                node = ObjectName_mode_df.node.loc[dof_idxmin]

            else:
                shapeWrtGlobal = ObjectName_mode_df.shapeWrtGlobal.loc[
                    dof_idxmin]
                node = ObjectName_mode_df.node.loc[dof_idxmin]

            max_dof_values.update({dof: round(shapeWrtGlobal, 3)})
            max_dof_nodes.update({dof: node})

        max_dof_percentage = {}
        max_dof_sum_square = sum(np.square(list(max_dof_values.values())))
        abs_max_dof = max(np.absolute(list(max_dof_values.values())))

        for dof in dofs:
            dof_percent = abs(max_dof_values[dof]) / max_dof_sum_square * 100
            max_dof_percentage.update({dof: round(dof_percent, 1)})

        dofs_analysis = self.cfg['default']['Analysis']['Analyze']['modal'][
            'dof_analysis']['dofs']
        if dofs_analysis is None:
            dofs_analysis = dofs
        modes_selected = {}
        threshold_values = self.cfg['default']['Analysis']['Analyze']['modal'][
            'dof_analysis']['threshold_percentages']
        for dof in dofs_analysis:
            if max_dof_percentage[dof] > threshold_values[dof]:
                modes_selected.update({dof: True})
            else:
                modes_selected.update({dof: False})

        return max_dof_values, max_dof_nodes, max_dof_percentage, abs_max_dof, modes_selected

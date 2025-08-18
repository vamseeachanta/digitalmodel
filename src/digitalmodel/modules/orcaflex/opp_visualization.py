try:
    import OrcFxAPI
except Exception:
    print("OrcaFlex license not available. Run on different computer")
import os
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from typing import Dict, List, Tuple, Optional
from loguru import logger
from assetutilities.common.yml_utilities import is_file_valid_func
from digitalmodel.common.parallel_processing import should_use_parallel


class OPPVisualization:
    def __init__(self) -> None:
        pass

    def get_visualizations(self, cfg):
        """Main entry point for visualization processing with parallel support.
        
        Uses centralized parallel processing logic:
        - Default: 30 workers (optimized for OrcaFlex servers)
        - Automatically adjusts to CPU count and number of files
        - Configurable via 'parallel_processing' section in config
        """
        # Get all input files
        all_input_files = self._collect_input_files(cfg)
        
        # Set task type for visualization (I/O bound)
        if 'parallel_processing' not in cfg:
            cfg['parallel_processing'] = {}
        cfg['parallel_processing']['task_type'] = 'io_bound'
        
        # Determine if we should use parallel processing
        use_parallel, num_workers, log_msg = should_use_parallel(
            cfg=cfg,
            num_items=len(all_input_files),
            module_name='OPP Visualization'
        )
        
        logger.info(log_msg)
        
        if not use_parallel:
            # Use sequential processing
            self.save_views_for_files(cfg)
        else:
            # Use parallel processing
            logger.info(f"Processing {len(all_input_files)} files with {num_workers} parallel workers")
            self._save_views_parallel(cfg, all_input_files, num_workers)

    def is_file_valid(self, file_name):
        is_file_valid, file_name = is_file_valid_func(file_name)

        return is_file_valid, file_name

    def save_views_for_files(self, cfg):
        model = OrcFxAPI.Model()
        combined_model = None

        orcaflex_extensions = cfg.file_management["input_files"].keys()

        for file_ext in orcaflex_extensions:
            raw_input_files_for_ext = cfg.file_management["input_files"][file_ext]

            for input_file_index in range(0, len(raw_input_files_for_ext)):
                input_file = raw_input_files_for_ext[input_file_index]

                try:
                    model.LoadData(input_file)
                except:
                    logger.error(f"Error loading file: {input_file}")
                    continue

                if cfg["visualization_settings"]["combined"]:
                    print("Combined model code in library does not exist")
                    # combined_model = self.combine_models(combined_model, model)

                model = self.set_general_visualization_settings(model, cfg)
                model.CalculateStatics()
                self.save_all_views(cfg, model, input_file)

            # TODO
            # if cfg['visualization_settings']['combined']:
            #     combined_model.CalculateStatics()

    def set_general_visualization_settings(self, model, cfg):
        # TODO for TDP Colour change
        # line = model[cfg['visualization_settings']['tdp_line']]
        # line.ContactPenColour = 128 * 65536 + 128 * 256 + 128

        env = model["Environment"]
        # env.SeabedPenStyle = "Clear"
        # env.SeabedProfilePenStyle = "Clear"
        env.SeaSurfacePenStyle = "Clear"
        model.general.NorthDirectionDefined = "No"

        # TODO for vessel settings
        # vessel = model["SevenArctic"]
        # x_value = vessel.InitialX
        # y_value = vessel.InitialY
        # heading = vessel.InitialHeading

        hide_items = cfg["visualization_settings"]["hide_items"]

        all_objects = []
        for obj in model.objects:
            Name = str(obj)
            all_objects.append(Name)
        for item in hide_items:
            if item in all_objects:
                model[item].Hidden = "Yes"

        # TODO crane settings
        # crane = model["250TeCrane"]
        # crane.OutsidePenStyle = "Dot"
        # crane.InsidePenStyle = "Clear"
        # crane.NumberOfLines = 2
        return model

    def combine_models(self, combined_model, model):
        if combined_model is None:
            combined_model = model
        else:
            for obj in model.objects:
                combined_model.createObject(obj)
                line = combined_model.CreateObject(obj.type)

        combined_model.SaveData("combined_model.dat")
        return combined_model

    def save_all_views(self, cfg, model, file_name):

        viewparams_cfg = cfg["visualization_settings"]["viewparams"]
        for view_label in list(viewparams_cfg.keys()):
            viewparams = self.assign_view_parameters(model, cfg, view_label)
            self.save_image(cfg, model, file_name, viewparams, view_label)

    def assign_view_parameters(self, model, cfg, view_label):

        viewparams_view_label_cfg = cfg["visualization_settings"]["viewparams"][
            view_label
        ]
        viewparams = model.defaultViewParameters

        if "SeaSurfacePenStyle" in viewparams_view_label_cfg:
            env = model["Environment"]
            env.SeaSurfacePenStyle = viewparams_view_label_cfg["SeaSurfacePenStyle"]

        for key in viewparams_view_label_cfg:
            try:
                if key == "ViewCentre":
                    ViewCentre = viewparams_view_label_cfg["ViewCentre"]
                    for i in range(0, len(ViewCentre)):
                        viewparams.ViewCentre[i] = ViewCentre[i]
                elif key == "RelativeToObject":
                    viewparams.RelativeToObject = model[
                        viewparams_view_label_cfg["RelativeToObject"]
                    ]
                else:
                    setattr(viewparams, key, viewparams_view_label_cfg[key])
            except Exception as e:
                logger.error(str(e))

        return viewparams

    def save_image(self, cfg, model, file_name, viewparams, view_label):
        result_plot_folder = cfg["Analysis"]["result_plot_folder"]
        file_name_img = (
            os.path.basename(file_name).split(".")[0] + "_" + view_label + ".jpg"
        )
        file_name_with_path = os.path.join(result_plot_folder, file_name_img)
        logger.info(f"Saving ...  {file_name_img}  view")
        model.SaveModelView(file_name_with_path, viewparams)
    
    def _collect_input_files(self, cfg) -> List[str]:
        """Collect all input files from configuration."""
        all_files = []
        orcaflex_extensions = cfg.file_management["input_files"].keys()
        
        for file_ext in orcaflex_extensions:
            raw_input_files_for_ext = cfg.file_management["input_files"][file_ext]
            all_files.extend(raw_input_files_for_ext)
        
        return all_files
    
    def _process_single_file(self, args: Tuple[str, dict]) -> Tuple[str, bool, str, float]:
        """Process a single file for visualization (for parallel processing)."""
        input_file, cfg = args
        start_time = time.time()
        
        try:
            model = OrcFxAPI.Model()
            model.LoadData(input_file)
            model = self.set_general_visualization_settings(model, cfg)
            model.CalculateStatics()
            self.save_all_views(cfg, model, input_file)
            
            execution_time = time.time() - start_time
            logger.info(f"✓ Visualization completed: {os.path.basename(input_file)} ({execution_time:.2f}s)")
            return input_file, True, "Success", execution_time
            
        except Exception as e:
            execution_time = time.time() - start_time
            error_msg = str(e)
            logger.error(f"✗ Visualization failed: {os.path.basename(input_file)} - {error_msg} ({execution_time:.2f}s)")
            return input_file, False, error_msg, execution_time
    
    def _save_views_parallel(self, cfg: dict, input_files: List[str], max_workers: int) -> None:
        """Process multiple files in parallel for visualization."""
        total_start = time.time()
        successful = []
        failed = []
        
        logger.info(f"Starting parallel visualization for {len(input_files)} files")
        logger.info("=" * 60)
        
        # Prepare arguments for parallel processing
        args_list = [(input_file, cfg) for input_file in input_files]
        
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            # Submit all tasks
            future_to_file = {
                executor.submit(self._process_single_file, args): args[0]
                for args in args_list
            }
            
            # Process completed tasks
            for future in as_completed(future_to_file):
                input_file, success, message, exec_time = future.result()
                if success:
                    successful.append((input_file, exec_time))
                else:
                    failed.append((input_file, message, exec_time))
        
        # Print summary
        total_time = time.time() - total_start
        logger.info("=" * 60)
        logger.info("VISUALIZATION PROCESSING SUMMARY")
        logger.info("=" * 60)
        logger.info(f"Total files processed: {len(input_files)}")
        logger.info(f"Successful: {len(successful)}")
        logger.info(f"Failed: {len(failed)}")
        logger.info(f"Total execution time: {total_time:.2f}s")
        logger.info(f"Average time per file: {total_time/len(input_files):.2f}s")
        
        if failed:
            logger.info("\nFailed files:")
            for input_file, error, exec_time in failed:
                logger.error(f"  - {os.path.basename(input_file)}: {error} ({exec_time:.2f}s)")

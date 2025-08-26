# Standard library imports
import copy
import logging
import traceback
from concurrent.futures import ProcessPoolExecutor, as_completed
from typing import Dict, List, Tuple

from assetutilities.common.data import PandasChainedAssignent

# Third party imports
# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary
# Reader imports
from digitalmodel.modules.orcaflex.opp_linkedstatistics import OPPLinkedStatistics
from digitalmodel.modules.orcaflex.opp_range_graph import OPPRangeGraph
from digitalmodel.modules.orcaflex.opp_summary import OPPSummary
from digitalmodel.modules.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.common.parallel_processing import should_use_parallel
from digitalmodel.modules.orcaflex.file_size_optimizer import FileSizeOptimizer

ou = OrcaflexUtilities()  # noqa
of_objects = OrcaFlexObjects()
opp_summary = OPPSummary()
opp_ls = OPPLinkedStatistics()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()
opp_visualization = OPPVisualization()


def process_single_file(args: Tuple[str, int, dict]) -> Dict:
    """Process a single simulation file and return results."""
    file_name, file_index, cfg = args
    
    try:
        # Get model and metadata
        model_dict = ou.get_model_and_metadata(file_name=file_name)
        model = model_dict["model"]
        
        # Initialize results
        result = {
            'file_name': file_name,
            'file_index': file_index,
            'load_matrix_data': {
                'run_status': model_dict["run_status"],
                'start_time': model_dict["start_time"],
                'stop_time': model_dict["stop_time"]
            },
            'range_data': None,
            'summary': None,
            'linked_statistics': None,
            'histogram': [[]] * len(cfg.get("time_series_settings", {}).get("groups", [])),
            'error': None
        }
        
        if model is None:
            return result
            
        print(f"Post-processing file: {file_name}")
        
        # Process RangeGraph
        try:
            if cfg["orcaflex"]["postprocess"].get("RangeGraph", {}).get("flag", False):
                result['range_data'] = opp_rg.postProcessRange(
                    model, cfg, "Dummy_Object"
                )
        except Exception as e:
            logging.error(f"RangeGraph error for {file_name}: {str(e)}")
            result['range_data'] = None
            
        # Process summary
        if cfg["orcaflex"]["postprocess"]["summary"]["flag"]:
            result['summary'] = opp_summary.get_summary_for_file(
                cfg, model_dict, file_name
            )
            
        # Process linked statistics
        if cfg["orcaflex"]["postprocess"]["linked_statistics"]["flag"]:
            result['linked_statistics'] = opp_ls.get_linked_statistics(
                cfg, model, file_name
            )
            
        # Process time series
        if cfg["orcaflex"]["postprocess"]["time_series"]["flag"]:
            if "time_series_settings" in cfg and cfg["time_series_settings"]["data"]:
                opp_ts.get_time_series_data(cfg, model_dict, file_name)
                
        return result
        
    except Exception as e:
        logging.error(f"Error processing {file_name}: {str(e)}")
        return {
            'file_name': file_name,
            'file_index': file_index,
            'error': str(e),
            'traceback': traceback.format_exc()
        }


def aggregate_results(results: List[Dict], cfg: dict) -> Dict:
    """Aggregate results from all processed files."""
    aggregated = {
        'RangeAllFiles': [],
        'histogram_all_files': [],
        'linked_statistics': None,
        'summary': None,
        'load_matrix_updates': []
    }
    
    # Sort results by file_index to maintain order
    results.sort(key=lambda x: x['file_index'])
    
    for result in results:
        # Aggregate RangeGraph data
        aggregated['RangeAllFiles'].append(result.get('range_data'))
        
        # Aggregate histograms
        aggregated['histogram_all_files'].append(result.get('histogram', [[]]))
        
        # Aggregate summaries
        if result.get('summary'):
            aggregated['summary'] = opp_summary.add_file_result_to_all_results(
                aggregated['summary'], result['summary']
            )
            
        # Aggregate linked statistics
        if result.get('linked_statistics'):
            aggregated['linked_statistics'] = opp_ls.add_file_result_to_all_results(
                aggregated['linked_statistics'], result['linked_statistics']
            )
            
        # Collect load matrix updates
        if result.get('load_matrix_data'):
            aggregated['load_matrix_updates'].append({
                'file_name': result['file_name'],
                'data': result['load_matrix_data']
            })
            
    return aggregated


class OrcaFlexPostProcess:
    def __init__(self, cfg=None):
        pass

    def post_process_router(self, cfg):

        orcaflex_license_flag = ou.is_orcaflex_available()
        if not orcaflex_license_flag:
            print("Orcaflex license not available.")
            return cfg

        cfg = self.get_cfg_with_master_data(cfg)

        post_process_data_flag = False
        if cfg["orcaflex"]["postprocess"]["summary"]["flag"]:
            post_process_data_flag = True
        if cfg["orcaflex"]["postprocess"]["linked_statistics"]["flag"]:
            post_process_data_flag = True
        if cfg["orcaflex"]["postprocess"]["RangeGraph"]["flag"]:
            post_process_data_flag = True
        if cfg["orcaflex"]["postprocess"]["time_series"]["flag"]:
            post_process_data_flag = True
        if cfg["orcaflex"]["postprocess"]["cummulative_histograms"]["flag"]:
            post_process_data_flag = True

        post_process_visualization_flag = False
        if cfg["orcaflex"]["postprocess"]["visualization"]["flag"]:
            post_process_visualization_flag = True

        app_basename = cfg["meta"]["basename"]
        if post_process_data_flag:
            cfg.update({app_basename: {}})
            self.post_process(cfg)

        if post_process_visualization_flag:
            opp_visualization.get_visualizations(cfg)

        if not post_process_data_flag and not post_process_visualization_flag:
            logging.info("No postprocess specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        self.get_cfg_with_summary_master_data(cfg)

        self.get_cfg_with_time_series_master_data(cfg)

        self.get_cfg_with_linked_statistics_master_data(cfg)

        return cfg

    def _update_load_matrix(self, load_matrix, updates):
        """Update load matrix with processing results."""
        with PandasChainedAssignent():
            for update in updates:
                file_name = update['file_name']
                data = update['data']
                load_matrix.loc[
                    (load_matrix["fe_filename"] == file_name), "run_status"
                ] = data['run_status']
                load_matrix.loc[
                    (load_matrix["fe_filename"] == file_name), "start_time"
                ] = data['start_time']
                load_matrix.loc[
                    (load_matrix["fe_filename"] == file_name), "stop_time"
                ] = data['stop_time']
        return load_matrix

    def _handle_process_error(self, file_name: str, error: Exception, cfg: dict):
        """Handle and log processing errors."""
        error_msg = f"Error processing {file_name}: {str(error)}"
        logging.error(error_msg)
        
        # Optionally save error report
        if cfg.get('parallel_processing', {}).get('save_error_reports', True):
            error_file = f"{file_name}_error.log"
            try:
                with open(error_file, 'w') as f:
                    f.write(error_msg)
                    f.write(f"\n\nTraceback:\n{traceback.format_exc()}")
            except Exception as e:
                logging.error(f"Failed to write error report: {str(e)}")

    def post_process(self, cfg):
        """Process multiple simulation files with smart parallel processing."""
        
        load_matrix = ou.get_load_matrix_with_filenames(cfg)
        sim_files = cfg.file_management["input_files"]["sim"]
        
        # Set task type for post-processing (mixed CPU and I/O)
        if 'parallel_processing' not in cfg:
            cfg['parallel_processing'] = {}
        cfg['parallel_processing']['task_type'] = 'mixed'
        
        # AUTO-OPTIMIZATION: Use file size analysis to determine optimal worker count
        # This can be disabled by setting auto_optimize: false in config
        auto_optimize = cfg.get('parallel_processing', {}).get('auto_optimize', True)
        
        if auto_optimize and len(sim_files) > 1:
            try:
                # Initialize file size optimizer
                optimizer = FileSizeOptimizer(use_aggressive=True)
                
                # Get optimal thread count based on file sizes
                from multiprocessing import cpu_count
                optimal_workers, optimization_reason = optimizer.get_optimal_threads(
                    sim_files,
                    max_allowed=cpu_count()
                )
                
                # Apply optimization if max_workers not explicitly set
                current_setting = cfg['parallel_processing'].get('max_workers', 'auto')
                if current_setting == 'auto' or current_setting is None:
                    cfg['parallel_processing']['max_workers'] = optimal_workers
                    print(f"[OPP Auto-Optimization] {optimization_reason}")
                    
                    # Show file size distribution for transparency
                    stats = optimizer.analyze_files(sim_files)
                    if stats['count'] > 0:
                        print(f"  File statistics: Count={stats['count']}, "
                              f"Size range={stats['min_mb']:.1f}-{stats['max_mb']:.1f}MB, "
                              f"Median={stats['median_mb']:.1f}MB")
                elif isinstance(current_setting, int) and abs(current_setting - optimal_workers) > 4:
                    # User specified workers but very different from optimal
                    print(f"[OPP] Performance tip: Current setting uses {current_setting} workers. "
                          f"File size analysis suggests {optimal_workers} workers would be optimal "
                          f"for these {len(sim_files)} files (median size: {optimizer.analyze_files(sim_files)['median_mb']:.1f}MB)")
            except Exception as e:
                # If optimization fails, fall back to normal behavior
                logging.warning(f"File size optimization failed: {e}. Using default settings.")
        
        # Determine if we should use parallel processing
        use_parallel, num_workers, log_msg = should_use_parallel(
            cfg=cfg,
            num_items=len(sim_files),
            module_name='OPP Post-Process'
        )
        
        print(log_msg)
        
        if not use_parallel:
            # Fall back to sequential processing
            return self._post_process_sequential(cfg)
        
        # Parallel processing
        print(f"Processing {len(sim_files)} files in parallel with {num_workers} workers")
        
        # Store cfg as instance variable for access in process_single_file
        self.cfg = cfg
        
        # Prepare arguments for each file
        file_args = [(file, idx, cfg) for idx, file in enumerate(sim_files)]
        
        # Process files in parallel
        results = []
        with ProcessPoolExecutor(max_workers=num_workers) as executor:
            # Submit all tasks
            future_to_file = {
                executor.submit(process_single_file, args): args[0] 
                for args in file_args
            }
            
            # Collect results with progress tracking
            for future in as_completed(future_to_file):
                file_name = future_to_file[future]
                try:
                    result = future.result()
                    results.append(result)
                    if result.get('error'):
                        print(f"Failed: {file_name} - {result['error']}")
                        self._handle_process_error(file_name, Exception(result['error']), cfg)
                    else:
                        print(f"Completed: {file_name}")
                except Exception as e:
                    logging.error(f"Failed to process {file_name}: {str(e)}")
                    self._handle_process_error(file_name, e, cfg)
                    results.append({
                        'file_name': file_name,
                        'file_index': file_args[0][1],
                        'error': str(e)
                    })
        
                
        # Aggregate results
        aggregated = aggregate_results(results, cfg)
        
        # Update load matrix
        self._update_load_matrix(load_matrix, aggregated['load_matrix_updates'])
        
        # Save results
        opp_summary.save_summary(aggregated['summary'], cfg)
        opp_ls.save_linked_statistics(aggregated['linked_statistics'], cfg)
        
        # Store for later use
        self.RangeAllFiles = aggregated['RangeAllFiles']
        self.HistogramAllFiles = aggregated['histogram_all_files']
        
        return cfg

    def _post_process_sequential(self, cfg):
        """Process simulation files sequentially using the same process_single_file function."""
        
        load_matrix = ou.get_load_matrix_with_filenames(cfg)
        sim_files = cfg.file_management["input_files"]["sim"]
        
        print(f"Processing {len(sim_files)} files sequentially")
        
        # Process files sequentially
        results = []
        for file_index, file_name in enumerate(sim_files):
            try:
                # Use the same process_single_file function as parallel processing
                result = process_single_file((file_name, file_index, cfg))
                results.append(result)
                
                if result.get('error'):
                    print(f"Failed: {file_name} - {result['error']}")
                    self._handle_process_error(file_name, Exception(result['error']), cfg)
                else:
                    print(f"Completed: {file_name}")
                    
            except Exception as e:
                logging.error(f"Failed to process {file_name}: {str(e)}")
                self._handle_process_error(file_name, e, cfg)
                results.append({
                    'file_name': file_name,
                    'file_index': file_index,
                    'error': str(e)
                })
        
        # Use the same aggregation function as parallel processing
        aggregated = aggregate_results(results, cfg)
        
        # Update load matrix
        self._update_load_matrix(load_matrix, aggregated['load_matrix_updates'])
        
        # Save results
        opp_summary.save_summary(aggregated['summary'], cfg)
        opp_ls.save_linked_statistics(aggregated['linked_statistics'], cfg)
        
        # Store for later use
        self.RangeAllFiles = aggregated['RangeAllFiles']
        self.HistogramAllFiles = aggregated['histogram_all_files']
        
        return cfg

    def get_cfg_with_linked_statistics_master_data(self, cfg):
        if "linked_statistics_settings_master" in cfg:
            linked_statistics_settings_master = cfg[
                "linked_statistics_settings_master"
            ].copy()
            linked_statistics_settings = cfg["linked_statistics_settings"]

            for group_index in range(0, len(linked_statistics_settings["groups"])):
                group = linked_statistics_settings["groups"][group_index].copy()

                if "Columns" in linked_statistics_settings_master["groups"][0]:
                    for column_index in range(0, len(group["Columns"])):
                        column = group["Columns"][column_index].copy()
                        column = update_deep_dictionary(
                            linked_statistics_settings_master["groups"][group_index][
                                "Columns"
                            ][0],
                            column,
                        )
                        group["Columns"][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(
                    linked_statistics_settings_master["groups"][group_index], group
                )
                linked_statistics_settings["groups"][group_index] = copy.deepcopy(group)

            cfg["linked_statistics_settings"] = copy.deepcopy(
                linked_statistics_settings
            )

    def get_cfg_with_time_series_master_data(self, cfg):
        if "time_series_settings_master" in cfg:
            time_series_settings_master = cfg["time_series_settings_master"].copy()
            time_series_settings = cfg["time_series_settings"]

            for group_index in range(0, len(time_series_settings["groups"])):
                group = time_series_settings["groups"][group_index].copy()

                if "Columns" in time_series_settings_master["groups"][0]:
                    for column_index in range(0, len(group["Columns"])):
                        column = group["Columns"][column_index].copy()
                        column = update_deep_dictionary(
                            time_series_settings_master["groups"][group_index][
                                "Columns"
                            ][0],
                            column,
                        )
                        group["Columns"][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(
                    time_series_settings_master["groups"][group_index], group
                )
                time_series_settings["groups"][group_index] = copy.deepcopy(group)

            cfg["time_series_settings"] = copy.deepcopy(time_series_settings)

    def get_cfg_with_summary_master_data(self, cfg):
        if "summary_settings_master" in cfg:
            summary_settings_master = cfg["summary_settings_master"].copy()
            summary_settings_master_keys = summary_settings_master.keys()
            summary_settings_master_non_groups = summary_settings_master.copy()
            if "groups" in summary_settings_master_keys:
                summary_settings_master_non_groups.pop("groups")
            cfg["summary_settings"] = update_deep_dictionary(
                summary_settings_master_non_groups, cfg["summary_settings"]
            )
            summary_settings = cfg["summary_settings"]

            for group_index in range(0, len(summary_settings["groups"])):
                group = summary_settings["groups"][group_index].copy()

                if "Columns" in summary_settings_master["groups"][0]:
                    for column_index in range(0, len(group["Columns"])):
                        column = group["Columns"][column_index].copy()
                        column = update_deep_dictionary(
                            summary_settings_master["groups"][group_index]["Columns"][
                                0
                            ],
                            column,
                        )
                        group["Columns"][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(
                    summary_settings_master["groups"][group_index], group
                )
                summary_settings["groups"][group_index] = copy.deepcopy(group)

            cfg["summary_settings"] = copy.deepcopy(summary_settings)

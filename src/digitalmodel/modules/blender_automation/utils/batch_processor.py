"""
ABOUTME: Batch processing utilities for Blender automation
Handles parallel/sequential batch operations on multiple files with progress tracking.
"""

from pathlib import Path
from typing import List, Dict, Any, Callable, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import logging
from datetime import datetime

from ..core.blender_wrapper import BlenderWrapper


logger = logging.getLogger(__name__)


class BatchProcessor:
    """
    Batch processing for Blender operations.

    Supports parallel and sequential processing with progress tracking,
    error handling, and result aggregation.
    """

    def __init__(
        self,
        blender_wrapper: Optional[BlenderWrapper] = None,
        max_workers: int = 4
    ):
        """
        Initialize batch processor.

        Args:
            blender_wrapper: BlenderWrapper instance. Creates new one if None.
            max_workers: Maximum number of parallel workers
        """
        self.blender = blender_wrapper or BlenderWrapper()
        self.max_workers = max_workers

    def process_files(
        self,
        files: List[Path],
        operation: Callable[[Path], Dict[str, Any]],
        parallel: bool = True,
        continue_on_error: bool = True
    ) -> Dict[str, Any]:
        """
        Process multiple files with a given operation.

        Args:
            files: List of file paths to process
            operation: Function to apply to each file
            parallel: Use parallel processing
            continue_on_error: Continue processing if one file fails

        Returns:
            Dictionary with batch processing results
        """
        start_time = datetime.now()
        results = []

        if parallel:
            results = self._process_parallel(
                files,
                operation,
                continue_on_error
            )
        else:
            results = self._process_sequential(
                files,
                operation,
                continue_on_error
            )

        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()

        return {
            "total_files": len(files),
            "successful": sum(1 for r in results if r["success"]),
            "failed": sum(1 for r in results if not r["success"]),
            "duration_seconds": duration,
            "results": results
        }

    def _process_sequential(
        self,
        files: List[Path],
        operation: Callable[[Path], Dict[str, Any]],
        continue_on_error: bool
    ) -> List[Dict[str, Any]]:
        """Process files sequentially."""
        results = []

        for i, file_path in enumerate(files, 1):
            logger.info(f"Processing {i}/{len(files)}: {file_path}")

            try:
                result = operation(file_path)
                results.append({
                    "file": str(file_path),
                    "success": True,
                    "result": result
                })
            except Exception as e:
                logger.error(f"Error processing {file_path}: {e}")
                results.append({
                    "file": str(file_path),
                    "success": False,
                    "error": str(e)
                })

                if not continue_on_error:
                    break

        return results

    def _process_parallel(
        self,
        files: List[Path],
        operation: Callable[[Path], Dict[str, Any]],
        continue_on_error: bool
    ) -> List[Dict[str, Any]]:
        """Process files in parallel."""
        results = []

        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            future_to_file = {
                executor.submit(operation, file_path): file_path
                for file_path in files
            }

            completed = 0
            for future in as_completed(future_to_file):
                file_path = future_to_file[future]
                completed += 1

                logger.info(f"Completed {completed}/{len(files)}: {file_path}")

                try:
                    result = future.result()
                    results.append({
                        "file": str(file_path),
                        "success": True,
                        "result": result
                    })
                except Exception as e:
                    logger.error(f"Error processing {file_path}: {e}")
                    results.append({
                        "file": str(file_path),
                        "success": False,
                        "error": str(e)
                    })

                    if not continue_on_error:
                        # Cancel remaining futures
                        for f in future_to_file:
                            f.cancel()
                        break

        return results

    def convert_directory(
        self,
        input_dir: Path,
        output_dir: Path,
        input_format: str,
        output_format: str,
        parallel: bool = True,
        recursive: bool = False
    ) -> Dict[str, Any]:
        """
        Convert all files in directory from one format to another.

        Args:
            input_dir: Input directory path
            output_dir: Output directory path
            input_format: Input file format (e.g., 'obj', 'stl')
            output_format: Output file format (e.g., 'fbx', 'gltf')
            parallel: Use parallel processing
            recursive: Search subdirectories recursively

        Returns:
            Dictionary with conversion results
        """
        input_dir = Path(input_dir)
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Find all input files
        if recursive:
            files = list(input_dir.rglob(f"*.{input_format}"))
        else:
            files = list(input_dir.glob(f"*.{input_format}"))

        logger.info(f"Found {len(files)} {input_format} files to convert")

        def convert_operation(file_path: Path) -> Dict[str, Any]:
            """Convert single file."""
            # Import to temporary .blend
            import tempfile
            temp_blend = Path(tempfile.mktemp(suffix=".blend"))

            try:
                # Import
                import_result = self.blender.import_file(
                    file_path,
                    input_format,
                    temp_blend
                )

                if not import_result["success"]:
                    raise RuntimeError(f"Import failed: {import_result.get('error')}")

                # Export
                output_file = output_dir / f"{file_path.stem}.{output_format}"
                export_result = self.blender.export_file(
                    temp_blend,
                    output_file,
                    output_format
                )

                # Cleanup
                temp_blend.unlink(missing_ok=True)

                return export_result

            except Exception as e:
                temp_blend.unlink(missing_ok=True)
                raise

        return self.process_files(
            files,
            convert_operation,
            parallel=parallel
        )

    def render_directory(
        self,
        blend_files: List[Path],
        output_dir: Path,
        resolution: tuple = (1920, 1080),
        samples: int = 128,
        parallel: bool = False  # Rendering is resource-intensive
    ) -> Dict[str, Any]:
        """
        Render multiple Blender files to images.

        Args:
            blend_files: List of .blend files
            output_dir: Output directory for rendered images
            resolution: Output resolution (width, height)
            samples: Number of render samples
            parallel: Use parallel rendering (use cautiously)

        Returns:
            Dictionary with render results
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        def render_operation(blend_file: Path) -> Dict[str, Any]:
            """Render single file."""
            output_image = output_dir / f"{blend_file.stem}.png"

            script = f"""
import bpy

scene = bpy.context.scene
scene.render.resolution_x = {resolution[0]}
scene.render.resolution_y = {resolution[1]}
scene.render.image_settings.file_format = 'PNG'
scene.render.filepath = '{output_image}'

if scene.render.engine == 'CYCLES':
    scene.cycles.samples = {samples}
elif scene.render.engine == 'BLENDER_EEVEE':
    scene.eevee.taa_render_samples = {samples}

bpy.ops.render.render(write_still=True)
"""

            return self.blender.run_script(
                script,
                background=True,
                blend_file=blend_file
            )

        return self.process_files(
            blend_files,
            render_operation,
            parallel=parallel
        )

    def apply_operation_to_directory(
        self,
        blend_files: List[Path],
        operation_script: str,
        save_files: bool = True,
        parallel: bool = True
    ) -> Dict[str, Any]:
        """
        Apply a custom Blender script to multiple files.

        Args:
            blend_files: List of .blend files
            operation_script: Python script to execute
            save_files: Save files after operation
            parallel: Use parallel processing

        Returns:
            Dictionary with operation results
        """
        def apply_operation(blend_file: Path) -> Dict[str, Any]:
            """Apply operation to single file."""
            script = operation_script

            if save_files:
                script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{blend_file}')"

            return self.blender.run_script(
                script,
                background=True,
                blend_file=blend_file
            )

        return self.process_files(
            blend_files,
            apply_operation,
            parallel=parallel
        )

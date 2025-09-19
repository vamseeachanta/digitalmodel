"""
Binary file handler for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Optional, Union
import logging

from .base import BaseMinimizer

logger = logging.getLogger(__name__)


class BinaryMinimizer(BaseMinimizer):
    """Handler for binary files."""
    
    # Common binary extensions
    BINARY_EXTENSIONS = {
        # Images
        '.jpg', '.jpeg', '.png', '.gif', '.bmp', '.svg', '.ico', '.webp', '.tiff',
        # Videos
        '.mp4', '.avi', '.mov', '.wmv', '.mkv', '.flv', '.webm', '.m4v',
        # Audio
        '.mp3', '.wav', '.flac', '.aac', '.ogg', '.wma', '.m4a',
        # Documents
        '.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx', '.odt',
        # Archives
        '.zip', '.tar', '.gz', '.bz2', '.7z', '.rar', '.xz',
        # Executables
        '.exe', '.dll', '.so', '.dylib', '.app', '.deb', '.rpm', '.msi',
        # Data
        '.db', '.sqlite', '.mdb', '.accdb',
        # Models/CAD
        '.stl', '.obj', '.fbx', '.dae', '.3ds', '.dwg', '.dxf',
        # Simulation
        '.sim', '.dat', '.mat', '.h5', '.hdf5', '.nc', '.netcdf',
        # Fonts
        '.ttf', '.otf', '.woff', '.woff2', '.eot',
        # Other
        '.bin', '.iso', '.img', '.dmg'
    }
    
    # Special handling for specific types
    ORCAFLEX_EXTENSIONS = {'.sim', '.yml', '.dat'}
    IMAGE_EXTENSIONS = {'.jpg', '.jpeg', '.png', '.gif', '.bmp', '.svg', '.webp'}
    DOCUMENT_EXTENSIONS = {'.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx'}
    
    def __init__(self, max_size: int = 1024):
        """
        Initialize binary minimizer.
        
        Args:
            max_size: Maximum size for stub files (very small)
        """
        super().__init__(max_size)
    
    def can_handle(self, file_path: Path) -> bool:
        """
        Check if this handler can process the file.
        
        Args:
            file_path: Path to file
            
        Returns:
            True if this is a binary file
        """
        return file_path.suffix.lower() in self.BINARY_EXTENSIONS
    
    def minimize(self, file_path: Path, preserve: bool = False) -> Optional[Union[bytes, str]]:
        """
        Create stub for binary file.
        
        Args:
            file_path: Path to binary file
            preserve: If True, return None to preserve original
            
        Returns:
            Stub content as string, or None if preserving
        """
        if preserve:
            return None
        
        try:
            stat = file_path.stat()
            ext = file_path.suffix.lower()
            
            # Get file metadata
            metadata = {
                'filename': file_path.name,
                'size': stat.st_size,
                'size_mb': stat.st_size / (1024 * 1024),
                'extension': ext,
                'type': self._get_file_type(ext)
            }
            
            # Special handling for specific types
            if ext in self.ORCAFLEX_EXTENSIONS:
                return self._create_orcaflex_stub(file_path, metadata)
            elif ext in self.IMAGE_EXTENSIONS:
                return self._create_image_stub(file_path, metadata)
            elif ext in self.DOCUMENT_EXTENSIONS:
                return self._create_document_stub(file_path, metadata)
            else:
                return self._create_generic_stub(file_path, metadata)
                
        except Exception as e:
            logger.warning(f"Error creating stub for binary {file_path}: {e}")
            return self.create_stub_metadata(file_path)
    
    def _get_file_type(self, ext: str) -> str:
        """
        Get human-readable file type.
        
        Args:
            ext: File extension
            
        Returns:
            File type description
        """
        type_map = {
            # Images
            '.jpg': 'JPEG Image', '.jpeg': 'JPEG Image', '.png': 'PNG Image',
            '.gif': 'GIF Image', '.bmp': 'Bitmap Image', '.svg': 'SVG Vector',
            # Videos
            '.mp4': 'MP4 Video', '.avi': 'AVI Video', '.mov': 'QuickTime Video',
            # Audio
            '.mp3': 'MP3 Audio', '.wav': 'WAV Audio', '.flac': 'FLAC Audio',
            # Documents
            '.pdf': 'PDF Document', '.doc': 'Word Document', '.docx': 'Word Document',
            '.xls': 'Excel Spreadsheet', '.xlsx': 'Excel Spreadsheet',
            # Archives
            '.zip': 'ZIP Archive', '.tar': 'TAR Archive', '.gz': 'Gzip Archive',
            # Executables
            '.exe': 'Windows Executable', '.dll': 'Dynamic Library',
            # Simulation
            '.sim': 'OrcaFlex Simulation', '.dat': 'Data File',
            # Other
            '.db': 'Database', '.stl': '3D Model (STL)'
        }
        
        return type_map.get(ext, f'{ext[1:].upper()} File')
    
    def _create_generic_stub(self, file_path: Path, metadata: dict) -> str:
        """
        Create generic stub file.
        
        Args:
            file_path: Original file path
            metadata: File metadata
            
        Returns:
            Stub content
        """
        stub = f"""# Binary File Stub
# Original: {metadata['filename']}
# Type: {metadata['type']}
# Size: {metadata['size']:,} bytes ({metadata['size_mb']:.2f} MB)
# Extension: {metadata['extension']}

This is a placeholder for the binary file.
The original file has been excluded to reduce go-by folder size.

To use this file:
1. Obtain the original from the source location
2. Replace this stub with the actual file
3. Verify the file size matches the metadata above
"""
        return stub
    
    def _create_orcaflex_stub(self, file_path: Path, metadata: dict) -> str:
        """
        Create OrcaFlex-specific stub.
        
        Args:
            file_path: Original file path
            metadata: File metadata
            
        Returns:
            Stub content
        """
        stub = f"""# OrcaFlex Model Stub
# Original: {metadata['filename']}
# Type: OrcaFlex Simulation File
# Size: {metadata['size']:,} bytes ({metadata['size_mb']:.2f} MB)

## Model Information
This is a placeholder for an OrcaFlex simulation file.

### Typical Contents:
- Vessel data
- Line properties
- Environmental conditions
- Simulation parameters
- Results data (if saved)

### Usage:
To work with this model:
1. Obtain the original .sim file from the source
2. Open in OrcaFlex (requires license)
3. Check model version compatibility

### Note:
OrcaFlex .sim files are binary and can be very large (often >100MB).
They contain complete model data including geometry, properties, and results.
"""
        return stub
    
    def _create_image_stub(self, file_path: Path, metadata: dict) -> str:
        """
        Create image file stub.
        
        Args:
            file_path: Original file path
            metadata: File metadata
            
        Returns:
            Stub content
        """
        # Try to get image dimensions if possible
        dimensions = "Unknown"
        
        stub = f"""# Image File Stub
# Original: {metadata['filename']}
# Type: {metadata['type']}
# Size: {metadata['size']:,} bytes ({metadata['size_mb']:.2f} MB)
# Dimensions: {dimensions}

## Image Information
This is a placeholder for an image file.

### Suggested Handling:
- For documentation: Reference by filename
- For web use: Create thumbnail version
- For analysis: Note visual content type

### Original Location:
{file_path}
"""
        return stub
    
    def _create_document_stub(self, file_path: Path, metadata: dict) -> str:
        """
        Create document file stub.
        
        Args:
            file_path: Original file path
            metadata: File metadata
            
        Returns:
            Stub content
        """
        stub = f"""# Document File Stub
# Original: {metadata['filename']}
# Type: {metadata['type']}
# Size: {metadata['size']:,} bytes ({metadata['size_mb']:.2f} MB)

## Document Information
This is a placeholder for a document file.

### Typical Contents:
- Text content
- Formatting
- Embedded images/objects
- Metadata

### To Access Content:
1. Obtain original document
2. Open with appropriate application:
   - PDF: Adobe Reader, Browser
   - Word: Microsoft Word, LibreOffice
   - Excel: Microsoft Excel, LibreOffice Calc

### Note:
Consider extracting text content for searchability
if the document contains important reference information.
"""
        return stub
    
    def create_thumbnail_placeholder(self, file_path: Path) -> str:
        """
        Create a thumbnail placeholder for image files.
        
        Args:
            file_path: Path to image file
            
        Returns:
            SVG placeholder content
        """
        # Simple SVG placeholder
        svg = f"""<svg width="200" height="150" xmlns="http://www.w3.org/2000/svg">
  <rect width="200" height="150" fill="#f0f0f0" stroke="#ccc" stroke-width="2"/>
  <text x="100" y="65" text-anchor="middle" font-family="Arial" font-size="14" fill="#666">
    Image Placeholder
  </text>
  <text x="100" y="85" text-anchor="middle" font-family="Arial" font-size="10" fill="#999">
    {file_path.name}
  </text>
</svg>"""
        return svg
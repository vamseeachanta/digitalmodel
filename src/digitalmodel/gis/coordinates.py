from dataclasses import dataclass
from typing import List, Tuple, Optional
from .core.crs import CRSDefinition
from .core.coordinate_transformer import CoordinateTransformer

@dataclass
class CoordinatePoint:
    x: float  # longitude or Easting
    y: float  # latitude or Northing
    z: Optional[float] = 0.0
    crs: str = "EPSG:4326"  # Default to WGS84

    def __post_init__(self):
        # Validate and normalise CRS to canonical EPSG:<code> form
        try:
            if ":" in self.crs:
                parts = self.crs.split(":")
                if parts[0].upper() != "EPSG":
                    raise ValueError("Only EPSG codes are supported currently")
                CRSDefinition.from_epsg(int(parts[-1]))
            elif self.crs.upper() == "WGS84":
                self.crs = "EPSG:4326"
            else:
                # Bare numeric string (e.g. "3857") — normalise to EPSG:<code>
                epsg_code = int(self.crs)
                CRSDefinition.from_epsg(epsg_code)
                self.crs = f"EPSG:{epsg_code}"
        except (ValueError, TypeError):
            raise ValueError(f"Invalid CRS: {self.crs}")

    def to_crs(self, target_crs: str) -> 'CoordinatePoint':
        """Transform this point to a new CRS."""
        if self.crs == target_crs:
            return self
        
        source = CRSDefinition.from_epsg(int(self.crs.split(":")[-1])) if ":" in self.crs else CRSDefinition.wgs84()
        target = CRSDefinition.from_epsg(int(target_crs.split(":")[-1]))
        
        transformer = CoordinateTransformer(source, target)
        new_coords = transformer.transform_point(self.x, self.y, self.z)
        
        return CoordinatePoint(
            x=new_coords[0], 
            y=new_coords[1], 
            z=new_coords[2] if len(new_coords) > 2 else self.z, 
            crs=target_crs
        )

def transform_points(points: List[CoordinatePoint], target_crs: str) -> List[CoordinatePoint]:
    """Batch transform a list of points to a new CRS."""
    return [pt.to_crs(target_crs) for pt in points]

def get_utm_crs(longitude: float, latitude: float) -> str:
    """Determine the UTM CRS for a given geographic location."""
    crs_def = CRSDefinition.utm_from_longitude(longitude, latitude)
    return f"EPSG:{crs_def.epsg_code}"

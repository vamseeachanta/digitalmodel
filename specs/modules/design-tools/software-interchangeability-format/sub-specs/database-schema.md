# Database Schema

This is the database schema implementation for the spec detailed in @specs/modules/design-tools/software-interchangeability-format/spec.md

> Created: 2025-08-20
> Version: 1.0.0

## Schema Structure

### Core Interchange Format Schema (JSON Schema)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://digitalmodel.com/schemas/interchange/v1.0.0",
  "title": "Engineering Interchange Format",
  "type": "object",
  "required": ["version", "metadata", "geometries"],
  "properties": {
    "version": {
      "type": "string",
      "pattern": "^\\d+\\.\\d+\\.\\d+$",
      "description": "Semantic version of the interchange format"
    },
    "metadata": {
      "$ref": "#/definitions/metadata"
    },
    "units": {
      "$ref": "#/definitions/units"
    },
    "coordinate_systems": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/coordinate_system"
      }
    },
    "materials": {
      "type": "object",
      "additionalProperties": {
        "$ref": "#/definitions/material"
      }
    },
    "geometries": {
      "type": "object",
      "additionalProperties": {
        "$ref": "#/definitions/geometry"
      }
    },
    "assemblies": {
      "type": "array",
      "items": {
        "$ref": "#/definitions/assembly"
      }
    }
  },
  "definitions": {
    "metadata": {
      "type": "object",
      "properties": {
        "created": {
          "type": "string",
          "format": "date-time"
        },
        "modified": {
          "type": "string",
          "format": "date-time"
        },
        "author": {
          "type": "string"
        },
        "organization": {
          "type": "string"
        },
        "project": {
          "type": "string"
        },
        "description": {
          "type": "string"
        },
        "source_application": {
          "type": "object",
          "properties": {
            "name": {"type": "string"},
            "version": {"type": "string"}
          }
        },
        "custom": {
          "type": "object",
          "additionalProperties": true
        }
      }
    },
    "units": {
      "type": "object",
      "properties": {
        "length": {
          "type": "string",
          "enum": ["meter", "millimeter", "centimeter", "inch", "foot"]
        },
        "mass": {
          "type": "string",
          "enum": ["kilogram", "gram", "pound", "ton"]
        },
        "time": {
          "type": "string",
          "enum": ["second", "minute", "hour"]
        },
        "angle": {
          "type": "string",
          "enum": ["radian", "degree"]
        }
      }
    },
    "coordinate_system": {
      "type": "object",
      "required": ["id", "origin", "axes"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "origin": {
          "$ref": "#/definitions/point3d"
        },
        "axes": {
          "type": "object",
          "properties": {
            "x": {"$ref": "#/definitions/vector3d"},
            "y": {"$ref": "#/definitions/vector3d"},
            "z": {"$ref": "#/definitions/vector3d"}
          }
        }
      }
    },
    "material": {
      "type": "object",
      "required": ["id", "name"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "density": {"type": "number"},
        "elastic_modulus": {"type": "number"},
        "poisson_ratio": {"type": "number"},
        "yield_strength": {"type": "number"},
        "color": {
          "type": "object",
          "properties": {
            "r": {"type": "number", "minimum": 0, "maximum": 1},
            "g": {"type": "number", "minimum": 0, "maximum": 1},
            "b": {"type": "number", "minimum": 0, "maximum": 1},
            "a": {"type": "number", "minimum": 0, "maximum": 1}
          }
        },
        "custom_properties": {
          "type": "object",
          "additionalProperties": true
        }
      }
    },
    "geometry": {
      "type": "object",
      "required": ["id", "type"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "type": {
          "type": "string",
          "enum": ["nurbs_surface", "mesh", "curve", "point_cloud", "solid"]
        },
        "material_id": {"type": "string"},
        "coordinate_system_id": {"type": "string"},
        "data": {
          "oneOf": [
            {"$ref": "#/definitions/nurbs_surface_data"},
            {"$ref": "#/definitions/mesh_data"},
            {"$ref": "#/definitions/curve_data"},
            {"$ref": "#/definitions/solid_data"}
          ]
        },
        "attributes": {
          "type": "object",
          "additionalProperties": true
        }
      }
    },
    "nurbs_surface_data": {
      "type": "object",
      "required": ["degree_u", "degree_v", "knots_u", "knots_v", "control_points"],
      "properties": {
        "degree_u": {"type": "integer", "minimum": 1},
        "degree_v": {"type": "integer", "minimum": 1},
        "knots_u": {
          "type": "array",
          "items": {"type": "number"}
        },
        "knots_v": {
          "type": "array",
          "items": {"type": "number"}
        },
        "control_points": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {"$ref": "#/definitions/point4d"}
          }
        },
        "is_closed_u": {"type": "boolean"},
        "is_closed_v": {"type": "boolean"}
      }
    },
    "mesh_data": {
      "type": "object",
      "required": ["vertices", "faces"],
      "properties": {
        "vertices": {
          "type": "array",
          "items": {"$ref": "#/definitions/point3d"}
        },
        "faces": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {"type": "integer", "minimum": 0}
          }
        },
        "normals": {
          "type": "array",
          "items": {"$ref": "#/definitions/vector3d"}
        },
        "uv_coordinates": {
          "type": "array",
          "items": {
            "type": "array",
            "items": {"type": "number"},
            "minItems": 2,
            "maxItems": 2
          }
        }
      }
    },
    "curve_data": {
      "type": "object",
      "properties": {
        "curve_type": {
          "type": "string",
          "enum": ["polyline", "nurbs", "arc", "circle", "ellipse"]
        },
        "points": {
          "type": "array",
          "items": {"$ref": "#/definitions/point3d"}
        },
        "degree": {"type": "integer"},
        "knots": {
          "type": "array",
          "items": {"type": "number"}
        },
        "weights": {
          "type": "array",
          "items": {"type": "number"}
        }
      }
    },
    "solid_data": {
      "type": "object",
      "properties": {
        "representation": {
          "type": "string",
          "enum": ["brep", "csg", "voxel"]
        },
        "faces": {
          "type": "array",
          "items": {"type": "string"}
        },
        "edges": {
          "type": "array",
          "items": {"type": "string"}
        },
        "vertices": {
          "type": "array",
          "items": {"$ref": "#/definitions/point3d"}
        }
      }
    },
    "assembly": {
      "type": "object",
      "required": ["id", "components"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "components": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "geometry_id": {"type": "string"},
              "transform": {"$ref": "#/definitions/transform_matrix"}
            }
          }
        }
      }
    },
    "point3d": {
      "type": "array",
      "items": {"type": "number"},
      "minItems": 3,
      "maxItems": 3
    },
    "point4d": {
      "type": "array",
      "items": {"type": "number"},
      "minItems": 4,
      "maxItems": 4
    },
    "vector3d": {
      "type": "array",
      "items": {"type": "number"},
      "minItems": 3,
      "maxItems": 3
    },
    "transform_matrix": {
      "type": "array",
      "items": {
        "type": "array",
        "items": {"type": "number"},
        "minItems": 4,
        "maxItems": 4
      },
      "minItems": 4,
      "maxItems": 4
    }
  }
}
```

## Binary Attachment Schema

For large geometry datasets, binary attachments can be referenced:

```json
{
  "geometry": {
    "id": "geom_001",
    "type": "mesh",
    "data": {
      "$binary_ref": "attachments/geom_001.msgpack",
      "format": "msgpack",
      "compression": "gzip",
      "checksum": "sha256:abc123..."
    }
  }
}
```

## Database Storage Schema (Optional)

For systems requiring database storage of interchange documents:

### PostgreSQL Schema

```sql
-- Main document table
CREATE TABLE interchange_documents (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    version VARCHAR(20) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    modified_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    author VARCHAR(255),
    organization VARCHAR(255),
    project VARCHAR(255),
    metadata JSONB,
    units JSONB,
    checksum VARCHAR(64)
);

-- Geometries table
CREATE TABLE geometries (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    document_id UUID REFERENCES interchange_documents(id) ON DELETE CASCADE,
    geometry_id VARCHAR(255) NOT NULL,
    name VARCHAR(255),
    geometry_type VARCHAR(50) NOT NULL,
    material_id VARCHAR(255),
    coordinate_system_id VARCHAR(255),
    data JSONB,
    binary_data BYTEA,
    attributes JSONB,
    UNIQUE(document_id, geometry_id)
);

-- Materials table
CREATE TABLE materials (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    document_id UUID REFERENCES interchange_documents(id) ON DELETE CASCADE,
    material_id VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    properties JSONB,
    UNIQUE(document_id, material_id)
);

-- Coordinate systems table
CREATE TABLE coordinate_systems (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    document_id UUID REFERENCES interchange_documents(id) ON DELETE CASCADE,
    system_id VARCHAR(255) NOT NULL,
    name VARCHAR(255),
    origin FLOAT[3],
    x_axis FLOAT[3],
    y_axis FLOAT[3],
    z_axis FLOAT[3],
    UNIQUE(document_id, system_id)
);

-- Assemblies table
CREATE TABLE assemblies (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    document_id UUID REFERENCES interchange_documents(id) ON DELETE CASCADE,
    assembly_id VARCHAR(255) NOT NULL,
    name VARCHAR(255),
    parent_id UUID REFERENCES assemblies(id),
    transform_matrix FLOAT[16],
    UNIQUE(document_id, assembly_id)
);

-- Assembly components junction table
CREATE TABLE assembly_components (
    assembly_id UUID REFERENCES assemblies(id) ON DELETE CASCADE,
    geometry_id UUID REFERENCES geometries(id) ON DELETE CASCADE,
    transform_matrix FLOAT[16],
    PRIMARY KEY (assembly_id, geometry_id)
);

-- Indexes for performance
CREATE INDEX idx_geometries_document ON geometries(document_id);
CREATE INDEX idx_geometries_type ON geometries(geometry_type);
CREATE INDEX idx_materials_document ON materials(document_id);
CREATE INDEX idx_assemblies_document ON assemblies(document_id);
CREATE INDEX idx_documents_project ON interchange_documents(project);
CREATE INDEX idx_documents_created ON interchange_documents(created_at);
```

## Version Migration Schema

```sql
-- Version tracking table
CREATE TABLE schema_versions (
    version VARCHAR(20) PRIMARY KEY,
    applied_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    migration_script TEXT
);

-- Migration log
CREATE TABLE migration_log (
    id SERIAL PRIMARY KEY,
    document_id UUID REFERENCES interchange_documents(id),
    from_version VARCHAR(20),
    to_version VARCHAR(20),
    migrated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    status VARCHAR(50),
    error_message TEXT
);
```

## Rationale

- **JSON Schema**: Industry standard for validation with good tooling support
- **Binary Attachments**: Allows efficient storage of large datasets while maintaining readability
- **PostgreSQL JSONB**: Provides indexing and querying capabilities for JSON data
- **UUID Identifiers**: Globally unique identifiers prevent conflicts in distributed systems
- **Separate Tables**: Normalized structure for efficient queries while maintaining flexibility
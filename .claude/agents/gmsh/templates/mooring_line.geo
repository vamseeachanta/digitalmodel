// Mooring Line Template Geometry
// Parametric model for mooring line with chain-wire-chain configuration
// Author: GMSH Agent
// Version: 1.0

// ============================================================================
// PARAMETERS (can be modified)
// ============================================================================

// Mooring line configuration
total_length = DefineNumber[1500.0, Name "Mooring/Total Length (m)"];
fairlead_x = DefineNumber[0.0, Name "Mooring/Fairlead X (m)"];
fairlead_y = DefineNumber[0.0, Name "Mooring/Fairlead Y (m)"];
fairlead_z = DefineNumber[-15.0, Name "Mooring/Fairlead Z (m)"];
anchor_x = DefineNumber[1200.0, Name "Mooring/Anchor X (m)"];
anchor_y = DefineNumber[0.0, Name "Mooring/Anchor Y (m)"];
anchor_z = DefineNumber[-350.0, Name "Mooring/Anchor Z (m)"];

// Line segments
top_chain_length = DefineNumber[100.0, Name "Segments/Top Chain Length (m)"];
wire_length = DefineNumber[1300.0, Name "Segments/Wire Length (m)"];
bottom_chain_length = DefineNumber[100.0, Name "Segments/Bottom Chain Length (m)"];

// Line properties
chain_diameter = DefineNumber[0.15, Name "Properties/Chain Diameter (m)"];
wire_diameter = DefineNumber[0.12, Name "Properties/Wire Diameter (m)"];
chain_bar_diameter = DefineNumber[0.12, Name "Properties/Chain Bar Diameter (m)"];

// Catenary parameters
pretension = DefineNumber[500.0, Name "Catenary/Pretension (kN)"];
line_weight_chain = DefineNumber[0.4, Name "Properties/Chain Weight in Water (kN/m)"];
line_weight_wire = DefineNumber[0.2, Name "Properties/Wire Weight in Water (kN/m)"];

// Discretization
num_segments = DefineNumber[50, Name "Mesh/Number of Segments"];
num_elements_per_segment = DefineNumber[10, Name "Mesh/Elements per Segment"];

// Mesh parameters
mesh_size_fine = DefineNumber[0.5, Name "Mesh/Fine Size (m)"];
mesh_size_medium = DefineNumber[2.0, Name "Mesh/Medium Size (m)"];
mesh_size_coarse = DefineNumber[5.0, Name "Mesh/Coarse Size (m)"];

// Touch down point calculation parameters
enable_touchdown = DefineNumber[1, Name "Catenary/Calculate Touchdown", Choices{0,1}];
seabed_stiffness = DefineNumber[100.0, Name "Seabed/Stiffness (kN/m/m)"];
seabed_friction = DefineNumber[0.5, Name "Seabed/Friction Coefficient"];

// ============================================================================
// CATENARY CALCULATION
// ============================================================================

// Calculate horizontal distance
dx = anchor_x - fairlead_x;
dy = anchor_y - fairlead_y;
dz = anchor_z - fairlead_z;
horizontal_distance = Sqrt(dx*dx + dy*dy);
vertical_distance = Abs(dz);

// Estimate catenary shape parameter (simplified)
catenary_parameter = pretension / ((line_weight_chain + line_weight_wire) / 2);

// ============================================================================
// GEOMETRY CREATION
// ============================================================================

// Clear any existing geometry
Delete All;

// ----------------------------------------------------------------------------
// Create Mooring Line Points
// ----------------------------------------------------------------------------

// Start point (fairlead)
Point(1) = {fairlead_x, fairlead_y, fairlead_z, mesh_size_fine};

// End point (anchor)
Point(2) = {anchor_x, anchor_y, anchor_z, mesh_size_fine};

// Calculate intermediate points along catenary
line_points = {1};  // Start with fairlead point
segment_points = {};

For i In {1:num_segments-1}
    t = i / num_segments;
    
    // Linear interpolation for x and y
    x = fairlead_x + t * dx;
    y = fairlead_y + t * dy;
    
    // Catenary curve for z (simplified)
    If (enable_touchdown == 1 && t > 0.7)
        // Near seabed - transition to linear
        z_catenary = fairlead_z + t * dz;
        z = Max(z_catenary, anchor_z + 5.0);  // Keep slightly above seabed
    Else
        // Catenary shape
        s = t * horizontal_distance;
        z_offset = catenary_parameter * (Cosh(s/catenary_parameter) - Cosh(0));
        z = fairlead_z - t * vertical_distance + z_offset * 0.1;  // Scaling factor for realistic sag
    EndIf
    
    // Determine mesh size based on segment type
    If (t * total_length < top_chain_length)
        // Top chain segment
        mesh_size = mesh_size_fine;
        segment_type = 1;
    ElseIf (t * total_length < top_chain_length + wire_length)
        // Wire segment
        mesh_size = mesh_size_medium;
        segment_type = 2;
    Else
        // Bottom chain segment
        mesh_size = mesh_size_fine;
        segment_type = 3;
    EndIf
    
    p = newp;
    Point(p) = {x, y, z, mesh_size};
    line_points[] += p;
    segment_points[i-1] = p;
EndFor

line_points[] += 2;  // Add anchor point

// ----------------------------------------------------------------------------
// Create Line Segments
// ----------------------------------------------------------------------------

line_segments = {};
For i In {0:num_segments-1}
    l = newl;
    Line(l) = {line_points[i], line_points[i+1]};
    line_segments[i] = l;
EndFor

// ----------------------------------------------------------------------------
// Create Connection Hardware (simplified representation)
// ----------------------------------------------------------------------------

// Fairlead connection
fairlead_connector = newp;
Point(fairlead_connector) = {fairlead_x, fairlead_y, fairlead_z + 1.0, mesh_size_fine};
fairlead_line = newl;
Line(fairlead_line) = {fairlead_connector, 1};

// Anchor point
anchor_point = newp;
Point(anchor_point) = {anchor_x, anchor_y, anchor_z - 2.0, mesh_size_fine};
anchor_line = newl;
Line(anchor_line) = {2, anchor_point};

// ----------------------------------------------------------------------------
// Create Chain Links (detailed geometry for close-up analysis)
// ----------------------------------------------------------------------------

If (chain_bar_diameter > 0 && num_segments < 20)  // Only for small models
    // Create detailed chain link at fairlead
    link_center = newp;
    Point(link_center) = {fairlead_x + 0.5, fairlead_y, fairlead_z, mesh_size_fine/5};
    
    // Create elliptical link shape
    link_points = {};
    num_link_points = 8;
    
    For i In {0:num_link_points-1}
        angle = 2*Pi*i/num_link_points;
        p = newp;
        Point(p) = {
            fairlead_x + 0.5 + chain_diameter * Cos(angle) / 2,
            fairlead_y + chain_diameter * Sin(angle) / 2,
            fairlead_z,
            mesh_size_fine/5
        };
        link_points[i] = p;
    EndFor
    
    // Connect link points
    link_lines = {};
    For i In {0:num_link_points-1}
        j = (i + 1) % num_link_points;
        l = newl;
        Circle(l) = {link_points[i], link_center, link_points[j]};
        link_lines[i] = l;
    EndFor
EndIf

// ----------------------------------------------------------------------------
// Create Touchdown Zone (if enabled)
// ----------------------------------------------------------------------------

If (enable_touchdown == 1)
    // Find approximate touchdown point
    touchdown_ratio = 0.75;  // Approximate position along line
    touchdown_index = Floor(touchdown_ratio * num_segments);
    
    If (touchdown_index > 0 && touchdown_index < num_segments)
        touchdown_point = line_points[touchdown_index];
        
        // Create seabed interaction zone
        seabed_zone_points = {};
        zone_radius = 20.0;
        num_zone_points = 8;
        
        For i In {0:num_zone_points-1}
            angle = 2*Pi*i/num_zone_points;
            p = newp;
            Point(p) = {
                Point{touchdown_point}[0] + zone_radius * Cos(angle),
                Point{touchdown_point}[1] + zone_radius * Sin(angle),
                anchor_z,
                mesh_size_medium
            };
            seabed_zone_points[i] = p;
        EndFor
        
        // Create zone boundary
        zone_lines = {};
        For i In {0:num_zone_points-1}
            j = (i + 1) % num_zone_points;
            l = newl;
            Line(l) = {seabed_zone_points[i], seabed_zone_points[j]};
            zone_lines[i] = l;
        EndFor
        
        // Create zone surface
        zone_loop = newll;
        Line Loop(zone_loop) = {zone_lines[]};
        zone_surface = news;
        Plane Surface(zone_surface) = {zone_loop};
    EndIf
EndIf

// ----------------------------------------------------------------------------
// Create Buoyancy Modules (if needed)
// ----------------------------------------------------------------------------

num_buoys = DefineNumber[0, Name "Buoyancy/Number of Modules"];
If (num_buoys > 0)
    buoy_diameter = DefineNumber[2.0, Name "Buoyancy/Module Diameter (m)"];
    buoy_length = DefineNumber[5.0, Name "Buoyancy/Module Length (m)"];
    buoy_start = DefineNumber[200.0, Name "Buoyancy/Start Distance (m)"];
    buoy_spacing = DefineNumber[50.0, Name "Buoyancy/Spacing (m)"];
    
    For i In {0:num_buoys-1}
        buoy_distance = buoy_start + i * buoy_spacing;
        buoy_index = Floor(buoy_distance / total_length * num_segments);
        
        If (buoy_index > 0 && buoy_index < num_segments)
            buoy_point = line_points[buoy_index];
            
            // Create simplified buoy geometry (cylinder)
            buoy_center = newp;
            Point(buoy_center) = {
                Point{buoy_point}[0],
                Point{buoy_point}[1],
                Point{buoy_point}[2] + buoy_diameter/2,
                mesh_size_fine
            };
        EndIf
    EndFor
EndIf

// ============================================================================
// PHYSICAL GROUPS (for boundary conditions and material assignment)
// ============================================================================

// Separate line segments by type
top_chain_lines = {};
wire_lines = {};
bottom_chain_lines = {};

For i In {0:num_segments-1}
    line_length_position = i * total_length / num_segments;
    
    If (line_length_position < top_chain_length)
        top_chain_lines[] += line_segments[i];
    ElseIf (line_length_position < top_chain_length + wire_length)
        wire_lines[] += line_segments[i];
    Else
        bottom_chain_lines[] += line_segments[i];
    EndIf
EndFor

// Physical lines
If (#top_chain_lines[] > 0)
    Physical Line("Top_Chain") = {top_chain_lines[]};
EndIf
If (#wire_lines[] > 0)
    Physical Line("Wire_Rope") = {wire_lines[]};
EndIf
If (#bottom_chain_lines[] > 0)
    Physical Line("Bottom_Chain") = {bottom_chain_lines[]};
EndIf

// Connection hardware
Physical Line("Fairlead_Connection") = {fairlead_line};
Physical Line("Anchor_Connection") = {anchor_line};

// Physical points
Physical Point("Fairlead") = {1};
Physical Point("Anchor") = {2};
Physical Point("Fairlead_Connector") = {fairlead_connector};
Physical Point("Anchor_Point") = {anchor_point};

// Touchdown zone (if created)
If (enable_touchdown == 1 && touchdown_index > 0)
    Physical Point("Touchdown") = {touchdown_point};
    Physical Surface("Seabed_Contact_Zone") = {zone_surface};
EndIf

// ============================================================================
// MESH FIELDS (for mesh refinement control)
// ============================================================================

// Field 1: Distance from fairlead (finer mesh near fairlead)
Field[1] = Distance;
Field[1].NodesList = {1};

// Field 2: Threshold for fairlead refinement
Field[2] = Threshold;
Field[2].IField = 1;
Field[2].LcMin = mesh_size_fine;
Field[2].LcMax = mesh_size_medium;
Field[2].DistMin = 10.0;
Field[2].DistMax = 100.0;

// Field 3: Distance from anchor (finer mesh near anchor)
Field[3] = Distance;
Field[3].NodesList = {2};

// Field 4: Threshold for anchor refinement
Field[4] = Threshold;
Field[4].IField = 3;
Field[4].LcMin = mesh_size_fine;
Field[4].LcMax = mesh_size_medium;
Field[4].DistMin = 10.0;
Field[4].DistMax = 100.0;

// Field 5: Touchdown zone refinement (if enabled)
If (enable_touchdown == 1 && touchdown_index > 0)
    Field[5] = Distance;
    Field[5].NodesList = {touchdown_point};
    
    Field[6] = Threshold;
    Field[6].IField = 5;
    Field[6].LcMin = mesh_size_fine;
    Field[6].LcMax = mesh_size_medium;
    Field[6].DistMin = 20.0;
    Field[6].DistMax = 100.0;
    
    // Combine all fields
    Field[7] = Min;
    Field[7].FieldsList = {2, 4, 6};
    Background Field = 7;
Else
    // Combine fairlead and anchor fields
    Field[7] = Min;
    Field[7].FieldsList = {2, 4};
    Background Field = 7;
EndIf

// ============================================================================
// MESH GENERATION OPTIONS
// ============================================================================

// 1D mesh algorithm
Mesh.Algorithm = 1;

// Mesh optimization
Mesh.Optimize = 1;

// Element order (1=linear, 2=quadratic)
Mesh.ElementOrder = 1;

// Recombine into quads (for 2D)
Mesh.RecombineAll = 0;

// Save all elements
Mesh.SaveAll = 1;

// ============================================================================
// INFO
// ============================================================================

Printf("Mooring Line Geometry Created");
Printf("  Total Length: %g m", total_length);
Printf("  Fairlead Position: (%g, %g, %g) m", fairlead_x, fairlead_y, fairlead_z);
Printf("  Anchor Position: (%g, %g, %g) m", anchor_x, anchor_y, anchor_z);
Printf("  Segments: Top Chain=%g m, Wire=%g m, Bottom Chain=%g m", 
       top_chain_length, wire_length, bottom_chain_length);
Printf("  Number of Elements: %g", num_segments);
Printf("  Pretension: %g kN", pretension);
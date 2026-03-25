// Seabed Terrain Template Geometry
// Parametric model for seabed with features like slopes, valleys, and mounds
// Author: GMSH Agent
// Version: 1.0

// ============================================================================
// PARAMETERS (can be modified)
// ============================================================================

// Domain dimensions
domain_length_x = DefineNumber[2000.0, Name "Domain/Length X (m)"];
domain_length_y = DefineNumber[2000.0, Name "Domain/Length Y (m)"];
water_depth_nominal = DefineNumber[200.0, Name "Domain/Nominal Water Depth (m)"];

// Terrain features
enable_slope = DefineNumber[1, Name "Features/Enable Slope", Choices{0,1}];
slope_gradient = DefineNumber[5.0, Name "Features/Slope Gradient (degrees)"];
slope_direction = DefineNumber[0.0, Name "Features/Slope Direction (degrees from X)"];

enable_valley = DefineNumber[1, Name "Features/Enable Valley", Choices{0,1}];
valley_depth = DefineNumber[20.0, Name "Features/Valley Depth (m)"];
valley_width = DefineNumber[200.0, Name "Features/Valley Width (m)"];
valley_length = DefineNumber[1000.0, Name "Features/Valley Length (m)"];
valley_x = DefineNumber[0.0, Name "Features/Valley Center X (m)"];
valley_y = DefineNumber[0.0, Name "Features/Valley Center Y (m)"];
valley_angle = DefineNumber[30.0, Name "Features/Valley Angle (degrees)"];

enable_mound = DefineNumber[1, Name "Features/Enable Mound", Choices{0,1}];
mound_height = DefineNumber[15.0, Name "Features/Mound Height (m)"];
mound_radius = DefineNumber[100.0, Name "Features/Mound Radius (m)"];
mound_x = DefineNumber[300.0, Name "Features/Mound Center X (m)"];
mound_y = DefineNumber[300.0, Name "Features/Mound Center Y (m)"];

enable_scour = DefineNumber[1, Name "Features/Enable Scour Pit", Choices{0,1}];
scour_depth = DefineNumber[5.0, Name "Features/Scour Depth (m)"];
scour_radius = DefineNumber[30.0, Name "Features/Scour Radius (m)"];
scour_x = DefineNumber[-200.0, Name "Features/Scour Center X (m)"];
scour_y = DefineNumber[-200.0, Name "Features/Scour Center Y (m)"];

enable_ridges = DefineNumber[1, Name "Features/Enable Sand Ridges", Choices{0,1}];
ridge_height = DefineNumber[2.0, Name "Features/Ridge Height (m)"];
ridge_wavelength = DefineNumber[50.0, Name "Features/Ridge Wavelength (m)"];
ridge_angle = DefineNumber[45.0, Name "Features/Ridge Angle (degrees)"];

// Seabed properties
roughness_height = DefineNumber[0.01, Name "Properties/Roughness Height (m)"];
sediment_layers = DefineNumber[3, Name "Properties/Number of Sediment Layers"];

// Grid resolution
grid_nx = DefineNumber[40, Name "Grid/Number of Points X"];
grid_ny = DefineNumber[40, Name "Grid/Number of Points Y"];

// Mesh parameters
mesh_size_fine = DefineNumber[5.0, Name "Mesh/Fine Size (m)"];
mesh_size_medium = DefineNumber[20.0, Name "Mesh/Medium Size (m)"];
mesh_size_coarse = DefineNumber[50.0, Name "Mesh/Coarse Size (m)"];
mesh_vertical_layers = DefineNumber[5, Name "Mesh/Vertical Layers in Sediment"];

// ============================================================================
// FUNCTIONS FOR TERRAIN GENERATION
// ============================================================================

// Function to calculate seabed elevation at a given point
Function CalculateElevation
    // Input: x_coord, y_coord
    // Output: z_elevation
    
    z_elevation = -water_depth_nominal;
    
    // Add slope
    If (enable_slope == 1)
        slope_rad = slope_gradient * Pi / 180;
        slope_dir_rad = slope_direction * Pi / 180;
        dx_slope = x_coord * Cos(slope_dir_rad) + y_coord * Sin(slope_dir_rad);
        z_elevation = z_elevation + dx_slope * Tan(slope_rad);
    EndIf
    
    // Add valley
    If (enable_valley == 1)
        valley_rad = valley_angle * Pi / 180;
        dx_valley = (x_coord - valley_x) * Cos(valley_rad) + (y_coord - valley_y) * Sin(valley_rad);
        dy_valley = -(x_coord - valley_x) * Sin(valley_rad) + (y_coord - valley_y) * Cos(valley_rad);
        
        If (Abs(dx_valley) < valley_length/2 && Abs(dy_valley) < valley_width/2)
            valley_factor = Cos(Pi * dy_valley / valley_width) * Cos(Pi * dx_valley / valley_length);
            valley_factor = Max(valley_factor, 0);
            z_elevation = z_elevation - valley_depth * valley_factor;
        EndIf
    EndIf
    
    // Add mound
    If (enable_mound == 1)
        dist_mound = Sqrt((x_coord - mound_x)^2 + (y_coord - mound_y)^2);
        If (dist_mound < mound_radius)
            mound_factor = Cos(Pi * dist_mound / (2 * mound_radius));
            z_elevation = z_elevation + mound_height * mound_factor^2;
        EndIf
    EndIf
    
    // Add scour pit
    If (enable_scour == 1)
        dist_scour = Sqrt((x_coord - scour_x)^2 + (y_coord - scour_y)^2);
        If (dist_scour < scour_radius)
            scour_factor = 1 - (dist_scour / scour_radius)^2;
            z_elevation = z_elevation - scour_depth * scour_factor;
        EndIf
    EndIf
    
    // Add sand ridges
    If (enable_ridges == 1)
        ridge_rad = ridge_angle * Pi / 180;
        ridge_coord = x_coord * Cos(ridge_rad) + y_coord * Sin(ridge_rad);
        ridge_phase = 2 * Pi * ridge_coord / ridge_wavelength;
        z_elevation = z_elevation + ridge_height * Sin(ridge_phase);
    EndIf
    
    Return z_elevation;
EndFunction

// ============================================================================
// GEOMETRY CREATION
// ============================================================================

// Clear any existing geometry
Delete All;

// ----------------------------------------------------------------------------
// Create Seabed Surface Grid Points
// ----------------------------------------------------------------------------

seabed_points = {};
point_counter = 0;

For i In {0:grid_nx-1}
    For j In {0:grid_ny-1}
        x_coord = -domain_length_x/2 + i * domain_length_x/(grid_nx-1);
        y_coord = -domain_length_y/2 + j * domain_length_y/(grid_ny-1);
        
        // Calculate elevation using terrain function
        z_elevation = -water_depth_nominal;
        
        // Add slope
        If (enable_slope == 1)
            slope_rad = slope_gradient * Pi / 180;
            slope_dir_rad = slope_direction * Pi / 180;
            dx_slope = x_coord * Cos(slope_dir_rad) + y_coord * Sin(slope_dir_rad);
            z_elevation = z_elevation + dx_slope * Tan(slope_rad);
        EndIf
        
        // Add valley
        If (enable_valley == 1)
            valley_rad = valley_angle * Pi / 180;
            dx_valley = (x_coord - valley_x) * Cos(valley_rad) + (y_coord - valley_y) * Sin(valley_rad);
            dy_valley = -(x_coord - valley_x) * Sin(valley_rad) + (y_coord - valley_y) * Cos(valley_rad);
            
            If (Abs(dx_valley) < valley_length/2 && Abs(dy_valley) < valley_width/2)
                valley_factor = Cos(Pi * dy_valley / valley_width) * Cos(Pi * dx_valley / valley_length);
                valley_factor = (valley_factor > 0) ? valley_factor : 0;
                z_elevation = z_elevation - valley_depth * valley_factor;
            EndIf
        EndIf
        
        // Add mound
        If (enable_mound == 1)
            dist_mound = Sqrt((x_coord - mound_x)^2 + (y_coord - mound_y)^2);
            If (dist_mound < mound_radius)
                mound_factor = Cos(Pi * dist_mound / (2 * mound_radius));
                z_elevation = z_elevation + mound_height * mound_factor * mound_factor;
            EndIf
        EndIf
        
        // Add scour pit
        If (enable_scour == 1)
            dist_scour = Sqrt((x_coord - scour_x)^2 + (y_coord - scour_y)^2);
            If (dist_scour < scour_radius)
                scour_factor = 1 - (dist_scour / scour_radius)^2;
                z_elevation = z_elevation - scour_depth * scour_factor;
            EndIf
        EndIf
        
        // Add sand ridges
        If (enable_ridges == 1)
            ridge_rad = ridge_angle * Pi / 180;
            ridge_coord = x_coord * Cos(ridge_rad) + y_coord * Sin(ridge_rad);
            ridge_phase = 2 * Pi * ridge_coord / ridge_wavelength;
            z_elevation = z_elevation + ridge_height * Sin(ridge_phase);
        EndIf
        
        // Determine mesh size based on features
        mesh_size = mesh_size_coarse;
        
        // Finer mesh near valley
        If (enable_valley == 1)
            dist_valley = Sqrt((x_coord - valley_x)^2 + (y_coord - valley_y)^2);
            If (dist_valley < valley_width)
                mesh_size = mesh_size_medium;
            EndIf
        EndIf
        
        // Finer mesh near mound
        If (enable_mound == 1)
            dist_mound = Sqrt((x_coord - mound_x)^2 + (y_coord - mound_y)^2);
            If (dist_mound < mound_radius * 1.5)
                mesh_size = mesh_size_fine;
            EndIf
        EndIf
        
        // Finer mesh near scour
        If (enable_scour == 1)
            dist_scour = Sqrt((x_coord - scour_x)^2 + (y_coord - scour_y)^2);
            If (dist_scour < scour_radius * 2)
                mesh_size = mesh_size_fine;
            EndIf
        EndIf
        
        // Create point
        p = newp;
        Point(p) = {x_coord, y_coord, z_elevation, mesh_size};
        seabed_points[point_counter] = p;
        point_counter = point_counter + 1;
    EndFor
EndFor

// ----------------------------------------------------------------------------
// Create Seabed Surface
// ----------------------------------------------------------------------------

// Create horizontal lines
h_lines = {};
h_line_counter = 0;

For i In {0:grid_nx-1}
    For j In {0:grid_ny-2}
        p1 = seabed_points[i * grid_ny + j];
        p2 = seabed_points[i * grid_ny + j + 1];
        l = newl;
        Line(l) = {p1, p2};
        h_lines[h_line_counter] = l;
        h_line_counter = h_line_counter + 1;
    EndFor
EndFor

// Create vertical lines
v_lines = {};
v_line_counter = 0;

For i In {0:grid_nx-2}
    For j In {0:grid_ny-1}
        p1 = seabed_points[i * grid_ny + j];
        p2 = seabed_points[(i + 1) * grid_ny + j];
        l = newl;
        Line(l) = {p1, p2};
        v_lines[v_line_counter] = l;
        v_line_counter = v_line_counter + 1;
    EndFor
EndFor

// Create surface patches
surface_patches = {};
patch_counter = 0;

For i In {0:grid_nx-2}
    For j In {0:grid_ny-2}
        // Get the four lines forming the patch
        l1 = h_lines[i * (grid_ny - 1) + j];
        l2 = v_lines[i * grid_ny + j + 1];
        l3 = -h_lines[(i + 1) * (grid_ny - 1) + j];
        l4 = -v_lines[i * grid_ny + j];
        
        // Create line loop and surface
        ll = newll;
        Line Loop(ll) = {l1, l2, l3, l4};
        s = news;
        Plane Surface(s) = {ll};
        surface_patches[patch_counter] = s;
        patch_counter = patch_counter + 1;
    EndFor
EndFor

// ----------------------------------------------------------------------------
// Create Sediment Layers (if requested)
// ----------------------------------------------------------------------------

If (sediment_layers > 1)
    layer_thickness = 10.0;  // Thickness of each sediment layer
    
    For layer In {1:sediment_layers-1}
        layer_points = {};
        
        For k In {0:point_counter-1}
            orig_point = seabed_points[k];
            x = Point{orig_point}[0];
            y = Point{orig_point}[1];
            z = Point{orig_point}[2] - layer * layer_thickness;
            mesh_size = Point{orig_point}[3];
            
            p = newp;
            Point(p) = {x, y, z, mesh_size};
            layer_points[k] = p;
        EndFor
        
        // Store layer points for volume creation
        seabed_points[layer * point_counter : (layer + 1) * point_counter - 1] = layer_points[];
    EndFor
EndIf

// ----------------------------------------------------------------------------
// Create Special Feature Markers
// ----------------------------------------------------------------------------

// Mark valley centerline
If (enable_valley == 1)
    valley_start = newp;
    valley_end = newp;
    Point(valley_start) = {
        valley_x - valley_length/2 * Cos(valley_angle * Pi / 180),
        valley_y - valley_length/2 * Sin(valley_angle * Pi / 180),
        -water_depth_nominal - valley_depth,
        mesh_size_fine
    };
    Point(valley_end) = {
        valley_x + valley_length/2 * Cos(valley_angle * Pi / 180),
        valley_y + valley_length/2 * Sin(valley_angle * Pi / 180),
        -water_depth_nominal - valley_depth,
        mesh_size_fine
    };
    valley_centerline = newl;
    Line(valley_centerline) = {valley_start, valley_end};
EndIf

// Mark mound peak
If (enable_mound == 1)
    mound_peak = newp;
    Point(mound_peak) = {mound_x, mound_y, -water_depth_nominal + mound_height, mesh_size_fine};
EndIf

// Mark scour center
If (enable_scour == 1)
    scour_center = newp;
    Point(scour_center) = {scour_x, scour_y, -water_depth_nominal - scour_depth, mesh_size_fine};
EndIf

// ============================================================================
// PHYSICAL GROUPS (for boundary conditions and material assignment)
// ============================================================================

// Physical surfaces
Physical Surface("Seabed_Surface") = {surface_patches[]};

// Feature-specific groups
If (enable_valley == 1)
    Physical Line("Valley_Centerline") = {valley_centerline};
    Physical Point("Valley_Start") = {valley_start};
    Physical Point("Valley_End") = {valley_end};
EndIf

If (enable_mound == 1)
    Physical Point("Mound_Peak") = {mound_peak};
EndIf

If (enable_scour == 1)
    Physical Point("Scour_Center") = {scour_center};
EndIf

// Boundary edges
boundary_lines = {};
// Bottom edge (y = -domain_length_y/2)
For i In {0:grid_nx-2}
    boundary_lines[] += v_lines[i * grid_ny];
EndFor
// Top edge (y = domain_length_y/2)
For i In {0:grid_nx-2}
    boundary_lines[] += v_lines[i * grid_ny + grid_ny - 1];
EndFor
// Left edge (x = -domain_length_x/2)
For j In {0:grid_ny-2}
    boundary_lines[] += h_lines[j];
EndFor
// Right edge (x = domain_length_x/2)
For j In {0:grid_ny-2}
    boundary_lines[] += h_lines[(grid_nx - 1) * (grid_ny - 1) + j];
EndFor

Physical Line("Domain_Boundary") = {boundary_lines[]};

// Corner points
corner_SW = seabed_points[0];
corner_SE = seabed_points[grid_ny - 1];
corner_NW = seabed_points[(grid_nx - 1) * grid_ny];
corner_NE = seabed_points[grid_nx * grid_ny - 1];

Physical Point("Corner_SW") = {corner_SW};
Physical Point("Corner_SE") = {corner_SE};
Physical Point("Corner_NW") = {corner_NW};
Physical Point("Corner_NE") = {corner_NE};

// ============================================================================
// MESH FIELDS (for adaptive mesh refinement)
// ============================================================================

// Field 1: Distance from mound (if enabled)
If (enable_mound == 1)
    Field[1] = Distance;
    Field[1].NodesList = {mound_peak};
    
    Field[2] = Threshold;
    Field[2].IField = 1;
    Field[2].LcMin = mesh_size_fine;
    Field[2].LcMax = mesh_size_coarse;
    Field[2].DistMin = mound_radius;
    Field[2].DistMax = mound_radius * 3;
EndIf

// Field 3: Distance from scour (if enabled)
If (enable_scour == 1)
    Field[3] = Distance;
    Field[3].NodesList = {scour_center};
    
    Field[4] = Threshold;
    Field[4].IField = 3;
    Field[4].LcMin = mesh_size_fine;
    Field[4].LcMax = mesh_size_medium;
    Field[4].DistMin = scour_radius;
    Field[4].DistMax = scour_radius * 3;
EndIf

// Field 5: Valley refinement (if enabled)
If (enable_valley == 1)
    Field[5] = Distance;
    Field[5].EdgesList = {valley_centerline};
    
    Field[6] = Threshold;
    Field[6].IField = 5;
    Field[6].LcMin = mesh_size_medium;
    Field[6].LcMax = mesh_size_coarse;
    Field[6].DistMin = valley_width / 2;
    Field[6].DistMax = valley_width * 1.5;
EndIf

// Combine all active fields
active_fields = {};
If (enable_mound == 1)
    active_fields[] += 2;
EndIf
If (enable_scour == 1)
    active_fields[] += 4;
EndIf
If (enable_valley == 1)
    active_fields[] += 6;
EndIf

If (#active_fields[] > 0)
    Field[10] = Min;
    Field[10].FieldsList = {active_fields[]};
    Background Field = 10;
EndIf

// ============================================================================
// MESH GENERATION OPTIONS
// ============================================================================

// 2D mesh algorithm (1=MeshAdapt, 2=Automatic, 5=Delaunay, 6=Frontal, 8=DelQuad)
Mesh.Algorithm = 6;

// Recombine into quads
Mesh.RecombineAll = 0;  // Set to 1 for quad elements

// Mesh optimization
Mesh.Optimize = 1;
Mesh.OptimizeNetgen = 1;

// Smoothing
Mesh.Smoothing = 5;

// Element quality
Mesh.QualityType = 2;

// Save all elements
Mesh.SaveAll = 1;

// ============================================================================
// INFO
// ============================================================================

Printf("Seabed Terrain Geometry Created");
Printf("  Domain: %g x %g m", domain_length_x, domain_length_y);
Printf("  Water Depth: %g m (nominal)", water_depth_nominal);
Printf("  Grid Resolution: %g x %g", grid_nx, grid_ny);
Printf("  Features Enabled:");
If (enable_slope == 1)
    Printf("    - Slope: %g degrees", slope_gradient);
EndIf
If (enable_valley == 1)
    Printf("    - Valley: %g m deep, %g m wide", valley_depth, valley_width);
EndIf
If (enable_mound == 1)
    Printf("    - Mound: %g m high, %g m radius", mound_height, mound_radius);
EndIf
If (enable_scour == 1)
    Printf("    - Scour: %g m deep, %g m radius", scour_depth, scour_radius);
EndIf
If (enable_ridges == 1)
    Printf("    - Sand Ridges: %g m high, %g m wavelength", ridge_height, ridge_wavelength);
EndIf
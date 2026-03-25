// Offshore Platform Template Geometry
// Parametric model for typical offshore platform structure
// Author: GMSH Agent
// Version: 1.0

// ============================================================================
// PARAMETERS (can be modified)
// ============================================================================

// Platform dimensions
deck_length = DefineNumber[40.0, Name "Platform/Deck Length (m)"];
deck_width = DefineNumber[30.0, Name "Platform/Deck Width (m)"];
deck_height = DefineNumber[3.0, Name "Platform/Deck Height (m)"];
deck_elevation = DefineNumber[25.0, Name "Platform/Deck Elevation (m)"];

// Jacket structure
num_legs = DefineNumber[4, Name "Jacket/Number of Legs"];
leg_spacing_x = DefineNumber[25.0, Name "Jacket/Leg Spacing X (m)"];
leg_spacing_y = DefineNumber[20.0, Name "Jacket/Leg Spacing Y (m)"];
leg_diameter_top = DefineNumber[2.0, Name "Jacket/Leg Diameter Top (m)"];
leg_diameter_bottom = DefineNumber[2.5, Name "Jacket/Leg Diameter Bottom (m)"];
water_depth = DefineNumber[100.0, Name "Environment/Water Depth (m)"];
mudline_penetration = DefineNumber[30.0, Name "Jacket/Mudline Penetration (m)"];

// Bracing
num_brace_levels = DefineNumber[5, Name "Jacket/Number of Brace Levels"];
brace_diameter = DefineNumber[1.0, Name "Jacket/Brace Diameter (m)"];

// Helideck
helideck_radius = DefineNumber[15.0, Name "Helideck/Radius (m)"];
helideck_elevation = DefineNumber[5.0, Name "Helideck/Elevation above deck (m)"];

// Mesh parameters
mesh_size_fine = DefineNumber[0.5, Name "Mesh/Fine Size (m)"];
mesh_size_medium = DefineNumber[1.0, Name "Mesh/Medium Size (m)"];
mesh_size_coarse = DefineNumber[2.0, Name "Mesh/Coarse Size (m)"];

// ============================================================================
// GEOMETRY CREATION
// ============================================================================

// Clear any existing geometry
Delete All;

// ----------------------------------------------------------------------------
// Create Deck Structure
// ----------------------------------------------------------------------------

// Deck corners
Point(1) = {-deck_length/2, -deck_width/2, deck_elevation, mesh_size_medium};
Point(2) = { deck_length/2, -deck_width/2, deck_elevation, mesh_size_medium};
Point(3) = { deck_length/2,  deck_width/2, deck_elevation, mesh_size_medium};
Point(4) = {-deck_length/2,  deck_width/2, deck_elevation, mesh_size_medium};

Point(5) = {-deck_length/2, -deck_width/2, deck_elevation + deck_height, mesh_size_medium};
Point(6) = { deck_length/2, -deck_width/2, deck_elevation + deck_height, mesh_size_medium};
Point(7) = { deck_length/2,  deck_width/2, deck_elevation + deck_height, mesh_size_medium};
Point(8) = {-deck_length/2,  deck_width/2, deck_elevation + deck_height, mesh_size_medium};

// Deck edges
Line(1) = {1, 2}; Line(2) = {2, 3}; Line(3) = {3, 4}; Line(4) = {4, 1};
Line(5) = {5, 6}; Line(6) = {6, 7}; Line(7) = {7, 8}; Line(8) = {8, 5};
Line(9) = {1, 5}; Line(10) = {2, 6}; Line(11) = {3, 7}; Line(12) = {4, 8};

// Deck surfaces
Line Loop(1) = {1, 2, 3, 4}; Plane Surface(1) = {1};     // Bottom
Line Loop(2) = {5, 6, 7, 8}; Plane Surface(2) = {2};     // Top
Line Loop(3) = {1, 10, -5, -9}; Plane Surface(3) = {3};  // Front
Line Loop(4) = {2, 11, -6, -10}; Plane Surface(4) = {4}; // Right
Line Loop(5) = {3, 12, -7, -11}; Plane Surface(5) = {5}; // Back
Line Loop(6) = {4, 9, -8, -12}; Plane Surface(6) = {6};  // Left

// Deck volume
Surface Loop(1) = {1, 2, 3, 4, 5, 6};
Volume(1) = {1};

// ----------------------------------------------------------------------------
// Create Jacket Legs
// ----------------------------------------------------------------------------

leg_points_top = {};
leg_points_bottom = {};
leg_lines = {};
leg_surfaces = {};

For i In {0:num_legs-1}
    angle = 2*Pi*i/num_legs + Pi/4;  // 45 degree offset for square arrangement
    
    If (i == 0 || i == 2)
        x_pos = leg_spacing_x/2 * Cos(angle);
        y_pos = leg_spacing_y/2 * Sin(angle);
    Else
        x_pos = leg_spacing_x/2 * Cos(angle);
        y_pos = leg_spacing_y/2 * Sin(angle);
    EndIf
    
    // Top of leg (at deck level)
    p_top = newp;
    Point(p_top) = {x_pos, y_pos, deck_elevation, mesh_size_fine};
    leg_points_top[i] = p_top;
    
    // Bottom of leg (at mudline)
    p_bottom = newp;
    Point(p_bottom) = {x_pos*1.2, y_pos*1.2, -water_depth-mudline_penetration, mesh_size_coarse};
    leg_points_bottom[i] = p_bottom;
    
    // Create leg as tapered cylinder
    l = newl;
    Line(l) = {p_top, p_bottom};
    leg_lines[i] = l;
EndFor

// ----------------------------------------------------------------------------
// Create Bracing Between Legs
// ----------------------------------------------------------------------------

brace_lines = {};
brace_count = 0;

For level In {1:num_brace_levels}
    z_level = deck_elevation - (deck_elevation + water_depth) * level / (num_brace_levels + 1);
    
    // Horizontal braces
    For i In {0:num_legs-1}
        j = (i + 1) % num_legs;
        
        // Interpolate position along legs
        t = level / (num_brace_levels + 1);
        
        // Create intermediate points on legs
        p1 = newp;
        x1 = (1-t) * Point{leg_points_top[i]}[0] + t * Point{leg_points_bottom[i]}[0];
        y1 = (1-t) * Point{leg_points_top[i]}[1] + t * Point{leg_points_bottom[i]}[1];
        Point(p1) = {x1, y1, z_level, mesh_size_medium};
        
        p2 = newp;
        x2 = (1-t) * Point{leg_points_top[j]}[0] + t * Point{leg_points_bottom[j]}[0];
        y2 = (1-t) * Point{leg_points_top[j]}[1] + t * Point{leg_points_bottom[j]}[1];
        Point(p2) = {x2, y2, z_level, mesh_size_medium};
        
        // Create brace
        l = newl;
        Line(l) = {p1, p2};
        brace_lines[brace_count] = l;
        brace_count = brace_count + 1;
    EndFor
    
    // Diagonal braces (X-bracing)
    If (level < num_brace_levels)
        For i In {0:num_legs-1:2}
            j = (i + 1) % num_legs;
            k = (i + 2) % num_legs;
            
            // Create diagonal connections
            t1 = level / (num_brace_levels + 1);
            t2 = (level + 1) / (num_brace_levels + 1);
            
            // Points at current level
            p1 = newp;
            x1 = (1-t1) * Point{leg_points_top[i]}[0] + t1 * Point{leg_points_bottom[i]}[0];
            y1 = (1-t1) * Point{leg_points_top[i]}[1] + t1 * Point{leg_points_bottom[i]}[1];
            z1 = deck_elevation - (deck_elevation + water_depth) * t1;
            Point(p1) = {x1, y1, z1, mesh_size_medium};
            
            // Points at next level
            p2 = newp;
            x2 = (1-t2) * Point{leg_points_top[k]}[0] + t2 * Point{leg_points_bottom[k]}[0];
            y2 = (1-t2) * Point{leg_points_top[k]}[1] + t2 * Point{leg_points_bottom[k]}[1];
            z2 = deck_elevation - (deck_elevation + water_depth) * t2;
            Point(p2) = {x2, y2, z2, mesh_size_medium};
            
            // Create diagonal brace
            l = newl;
            Line(l) = {p1, p2};
            brace_lines[brace_count] = l;
            brace_count = brace_count + 1;
        EndFor
    EndIf
EndFor

// ----------------------------------------------------------------------------
// Create Helideck
// ----------------------------------------------------------------------------

// Helideck center
helideck_center = newp;
Point(helideck_center) = {0, 0, deck_elevation + deck_height + helideck_elevation, mesh_size_fine};

// Helideck perimeter points
helideck_points = {};
num_helideck_points = 16;

For i In {0:num_helideck_points-1}
    angle = 2*Pi*i/num_helideck_points;
    p = newp;
    Point(p) = {
        helideck_radius * Cos(angle),
        helideck_radius * Sin(angle),
        deck_elevation + deck_height + helideck_elevation,
        mesh_size_fine
    };
    helideck_points[i] = p;
EndFor

// Create helideck circle
helideck_lines = {};
For i In {0:num_helideck_points-1}
    j = (i + 1) % num_helideck_points;
    l = newl;
    Line(l) = {helideck_points[i], helideck_points[j]};
    helideck_lines[i] = l;
EndFor

// Create helideck surface
helideck_loop = newll;
Line Loop(helideck_loop) = {helideck_lines[]};
helideck_surface = news;
Plane Surface(helideck_surface) = {helideck_loop};

// ----------------------------------------------------------------------------
// Create Pile/Foundation Points
// ----------------------------------------------------------------------------

pile_points = {};
For i In {0:num_legs-1}
    p = newp;
    Point(p) = {
        Point{leg_points_bottom[i]}[0],
        Point{leg_points_bottom[i]}[1],
        Point{leg_points_bottom[i]}[2] - 10,  // Extended into seabed
        mesh_size_coarse
    };
    pile_points[i] = p;
    
    // Connect to leg bottom
    l = newl;
    Line(l) = {leg_points_bottom[i], p};
EndFor

// ============================================================================
// PHYSICAL GROUPS (for boundary conditions and material assignment)
// ============================================================================

// Physical volumes
Physical Volume("Deck") = {1};

// Physical surfaces
Physical Surface("Deck_Top") = {2};
Physical Surface("Deck_Bottom") = {1};
Physical Surface("Helideck") = {helideck_surface};

// Physical lines (for structural members)
Physical Line("Legs") = {leg_lines[]};
Physical Line("Braces") = {brace_lines[]};

// Physical points (for boundary conditions)
Physical Point("Pile_Tips") = {pile_points[]};
Physical Point("Leg_Tops") = {leg_points_top[]};

// ============================================================================
// MESH FIELDS (for mesh refinement control)
// ============================================================================

// Field 1: Distance from legs (finer mesh near legs)
Field[1] = Distance;
Field[1].NodesList = {leg_points_top[], leg_points_bottom[]};

// Field 2: Threshold for leg refinement
Field[2] = Threshold;
Field[2].IField = 1;
Field[2].LcMin = mesh_size_fine;
Field[2].LcMax = mesh_size_coarse;
Field[2].DistMin = 2.0;
Field[2].DistMax = 10.0;

// Field 3: Distance from deck
Field[3] = Box;
Field[3].VIn = mesh_size_medium;
Field[3].VOut = mesh_size_coarse;
Field[3].XMin = -deck_length/2 - 5;
Field[3].XMax = deck_length/2 + 5;
Field[3].YMin = -deck_width/2 - 5;
Field[3].YMax = deck_width/2 + 5;
Field[3].ZMin = deck_elevation - 5;
Field[3].ZMax = deck_elevation + deck_height + helideck_elevation + 5;

// Field 4: Combine all fields
Field[4] = Min;
Field[4].FieldsList = {2, 3};

// Set background field
Background Field = 4;

// ============================================================================
// MESH GENERATION OPTIONS
// ============================================================================

// Mesh algorithm (1=MeshAdapt, 2=Automatic, 5=Delaunay, 6=Frontal, 7=BAMG, 8=DelQuad)
Mesh.Algorithm = 6;

// 3D mesh algorithm (1=Delaunay, 3=Initial mesh only, 4=Frontal, 7=MMG3D, 9=R-tree, 10=HXT)
Mesh.Algorithm3D = 4;

// Mesh optimization
Mesh.Optimize = 1;
Mesh.OptimizeNetgen = 1;

// Element quality
Mesh.QualityType = 2; // 0=SICN, 1=SIGE, 2=Gamma

// Element order (1=linear, 2=quadratic)
Mesh.ElementOrder = 1;

// Save all elements
Mesh.SaveAll = 1;

// Output format (1=msh, 2=unv, 10=automatic, 16=vtk, 27=stl, 30=mesh, 31=bdf, 32=cgns, 33=med, 40=ply2)
Mesh.Format = 1;

// ============================================================================
// INFO
// ============================================================================

Printf("Offshore Platform Geometry Created");
Printf("  Deck: %g x %g x %g m", deck_length, deck_width, deck_height);
Printf("  Water Depth: %g m", water_depth);
Printf("  Number of Legs: %g", num_legs);
Printf("  Helideck Radius: %g m", helideck_radius);
Printf("  Mesh Sizes: Fine=%g, Medium=%g, Coarse=%g", mesh_size_fine, mesh_size_medium, mesh_size_coarse);
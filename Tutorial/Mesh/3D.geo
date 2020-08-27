// GEOMETRY ----------------------------------------------------------------
// Define points for one face of the domain
Point(1) = {0, 0, 0};
Point(2) = {2, 0, 0};
Point(3) = {2, 0, 5};
Point(4) = {0, 0, 5};
// Define lines using the previously defined points
Line(5) = {1, 2};
Line(6) = {2, 3};
Line(7) = {3, 4};
Line(8) = {4, 1};
// Define a closed system of lines using the previously defined lines
Curve Loop(9) = {5, 6, 7, 8};
// Define the face as a plane surface
Plane Surface(10) = {9};
// Extrude the 2D face into a 3D volume and store it in an array
tmp[] = Extrude {0, 2, 0} {Surface{10};};
// Define a physical volume using the extruded entity
// "Physical" entities are what the mesh is applied to
Physical Volume(11) = tmp[1];

// MESH ---------------------------------------------------------------------
// Create field entity
Field[1] = MathEval;
// Define mesh size as a function of spatial coordinates
Field[1].F = "(-z + 6) / 10";
// Set this field as the background field
Background Field = 1;
// Generate 3D mesh
Mesh 3;



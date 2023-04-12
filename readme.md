# shape rotator

## Code
- Generally a variable `X0` is the untransformed version of `X`

TODO: 

- ambo
- kis
- gyro
- reflect
- resize
- ap ->stl
- readme
- wiki

## Usage
1. generate polyhedra from a seed (character representing platonic solid)
    - e.g. "T" for tetrahedron
2. modify seed polyhedron using John Conway polyhedron notation 
    - e.g. "dkT" = (dual (kis tetrahedron))
3. below are output options, from most to least likely of getting done:
    - easiest would be ASCII STL; see resources
    - lay out as a planar (polyhedral) graph in SVG format (this can be done with all outer edges as sides to a hidden face)
    - project the shape onto a plane in SVG format 
        - can get the user to specify a camera angle
        - extra ambitious: interactive SVG option
4. we can check graph isomorphism vs known polyhedra database add a caption to the output if there's a match

## Resources

- wikipedia: https://en.wikipedia.org/wiki/Conway_polyhedron_notation
- operators in simple english: https://www.georgehart.com/virtual-polyhedra/conway_notation.html
- 3d views of a bunch of named polyhedra: http://dmccooey.com/polyhedra/index.html
    - on this page, we are only concerned with platonic, archimedian, and catalan solids i think
    - it would be nice if we could add some stats to the output like these have. we'd just have to calculate them
        - very easy: vertices, edges
        - maybe: faces, symmetry stats
- online visualizer that already does a pretty excellent job: https://levskaya.github.io/polyhedronisme/
- 3D wireframes in SVG: https://prideout.net/blog/svg_wireframes/#examples
- SVG blog posts (interactivity): https://www.petercollingridge.co.uk/search/?query=svg&page=1
- STL might be easier than SVG: https://en.wikipedia.org/wiki/STL_(file_format)
    - Snub cube example: https://en.wikipedia.org/wiki/Snub_cube
- Projection onto the plane: https://en.wikipedia.org/wiki/Orthographic_projection
- Polyhedral graph: https://en.wikipedia.org/wiki/Polyhedral_graph
    - Do the numbers of faces, edges, and vertices of a convex polyhedron uniquely specify its graph? I would think no, but I can't find the answer. So probably no.
- List of all Johnson solids (for later later): https://en.wikipedia.org/wiki/Johnson_solid

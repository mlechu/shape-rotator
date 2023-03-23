# shape rotator

- represent everything as a graph? 
    - Maybe a matrix would be more efficient, if I can figure out how the operators and their corresponding matrices on the wikipedia work
- user can generate polyhedra from a seed (platonic solid)
- user can modify polyhedra using john conway polyhedron notation
- output:
    - lay out as a planar graph in SVG format (this can be done with all outer edges as sides to a hidden face)
    - maybe try for a 3d rendering, also SVG, if we get the user to specify a camera angle and we have time
- we can check graph isomorphism vs known polyhedra database add a caption to the output if there's a match

## resources

- wikipedia: https://en.wikipedia.org/wiki/Conway_polyhedron_notation
- operators in simple english: https://www.georgehart.com/virtual-polyhedra/conway_notation.html
- 3d views of a bunch of named polyhedra: http://dmccooey.com/polyhedra/index.html
    - on this page, we are only concerned with platonic, archimedian, and catalan solids i think
    - it would be nice if we could add some stats to the output like these have. we'd just have to calculate them
        - very easy: vertices, edges
        - maybe: faces, symmetry stats
- online visualizer that already does a pretty excellent job: https://levskaya.github.io/polyhedronisme/

I would not be surprised if something above promises to solve some sort of open problem in computer science lol. we'll figure out what features to add as we go.

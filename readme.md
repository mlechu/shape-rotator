# shape rotator

## Usage
TODO

## Internals
- Until it's time to render them, [polyhedra are represented as follows](./abstract_polytope.pl):
    ```
    ap([face([edge(point(1,2,3), point(4,5,6))
            edge(point(1,2,3), point(9,8,7))
            edge(point(4,5,6), point(9,8,7))
            ...])
        face([...])
        face([...])
        face([...])
        ...])
    ```
    The order of `face`s within the `ap` does not matter, nor does the order of the edges within the face.

    The order of `point`s within the `edge(A, B)` does matter (`A @=< B`) for easier edge comparison.

- For STL output, each face is broken [into triangles](https://en.wikipedia.org/wiki/Triangle_fan).
- Several assumptions are made about the properties of the polyhedron. If you manage to violate them, you'll probably get some fun visual bugs (or a div/0 error.)
    - Every `face(edge1, edge2, edge3, ...)` lies flat in some plane. This is probably the easiest to break.
    - Every 2d and 3d shape is always convex (and consequently, the midpoint of any face is inside the face)
    - No edge or face would intersect the origin when extended. I think we even assume that the origin always lies inside the 3d shape.
- Generally a variable `X0` is the untransformed version of `X`

## Resources

- wikipedia: https://en.wikipedia.org/wiki/Conway_polyhedron_notation
- operators in simple english: https://www.georgehart.com/virtual-polyhedra/conway_notation.html
- 3d views of a bunch of named polyhedra: http://dmccooey.com/polyhedra/index.html
- online visualizer that already does a pretty excellent job: https://levskaya.github.io/polyhedronisme/
- Projection onto the plane: https://en.wikipedia.org/wiki/Orthographic_projection
- Polyhedral graph: https://en.wikipedia.org/wiki/Polyhedral_graph
    - Do the numbers of faces, edges, and vertices of a convex polyhedron uniquely specify its graph? I would think no, but I can't find the answer. So probably no.
- List of all Johnson solids (for later later): https://en.wikipedia.org/wiki/Johnson_solid

## Features to add
- gyro operator
- reflect (easy)
- number of vertices, edges, faces
- symmetry stats
- SVG output (project to 2d)
- Comparison with known polyhedra (graph isomorphism)
- Better normalization

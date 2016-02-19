# MeshLoop {#MeshLoop}

A very powerful component to make iterative meshes.

![Example of looping a box with changing scale, position and rotation](comp-meshloop-example.png)

This component can only be used in the @ref MeshProducers "Mesh.Producers" property.

See also: @ref Mesh

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?p=2578#p2578)

## Properties

@dl

@dt Scale
@dd Modify the mesh scale on each iteration.

@dt Count
@dd Number of Iterations to be executed.

@dt RecursionCount
@dd When this value is larger than zero and the loop is on the last iteration, instead of running the OnIteration producers, it will reload the whole mesh. This is useful for making, e.g., iterative tree branches.

@dt Iterations
@dd Read only. Iteration counter that can be used in expression of the iterated mesh.

@dt Position
@dd Modify the mesh position on each iteration.

@dt Rotation
@dd Modify the mesh rotation on each iteration.

@dlx

## List Properties

@dl

@dt OnIteration
@dd One or more components defining the iterated mesh.

@dlx

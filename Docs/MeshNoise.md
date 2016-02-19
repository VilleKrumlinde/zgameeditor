# MeshNoise {#MeshNoise}

A mesh producer that filters an incoming mesh with Perlin noise. Try to build a mesh with a MeshSphere and then add a MeshNoise to see its effect.

This component can only be used in the @ref MeshProducers "Mesh.Producers" property.

See also: @ref Mesh

## Properties

@dl

@dt Scale
@dd Modifies the scale of the mesh.

@dt NoiseSpeed
@dd Every incoming vertex in multiplied with this amount before sent in to the noise function.

@dt NoiseScale
@dd The output of the noise function is scaled with this value.

@dt SymmetryX, SymmetryY, SymmetryZ
@dd Set to control if the noise should be applied symmetrically in each axis.

@dlx

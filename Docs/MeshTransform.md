# MeshTransform {#MeshTransform}

Use this component to scale/move/rotate a mesh.

This component can only be used in the @ref MeshProducers "Mesh.Producers" property.

See also: @ref Mesh

## Properties

@dl

@dt Scale
@dd Modify the mesh scale.

@dt Position
@dd Modify the mesh position.

@dt Rotation
@dd Modify the mesh rotation.

@dt Accumulate
@dd The component keeps a internal matrix that it multiplies with the rotation/translation/scale properties whenever it executes. When accumulate property is set then the transformation builds on the previous transform. When the Accumulate property is not set then it simply resets this internal matrix so that the transform starts from a identity matrix.

@dlx

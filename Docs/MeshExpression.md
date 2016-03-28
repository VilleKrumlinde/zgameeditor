# MeshExpression {#MeshExpression}

A mesh producer that filters an incoming mesh with an expression.

This component can only be used in the @ref MeshProducers "Mesh.Producers" property.

Example usage: See "BoidMesh" in @ref Steering sample project.

See also: @ref Mesh

## Properties

@dl

@dt Expression
@dd An @ref ScriptingLanguage "expression" that is called once for every vertex in the mesh. Value of this property is specified in @ref CodeEditor "Code editor". The special read/write property "V" holds the current vertex. Modify this value to change the vertex. Example of a valid expression: "this.V.X*=2;". The read/write property "N" holds the current normal. The write-only property "C" allows writing to the current vertex color if VertexColors is on. The read/write property "TexCoord" holds the current texture coordinate.

@dt AutoNormals
@dd If this property is on all normals will be automatically regenerated after a the MeshExpression is executed. Turn off this if you want to modify normals in the expression.

@dt VertexColors
@dd If this property is on then the expression can set vertex colors.

@dt HasTexCoords
@dd If this property is on then the expression can set texture coordinates.

@dlx

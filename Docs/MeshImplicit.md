# MeshImplicit {#MeshImplicit}

A mesh producer that generates a mesh based on implicit functions.

@note The implicit meshes components are highly experimental. Setting invalid property values may lock up the editor. Make sure you save your data often!

This component can only be used in the @ref MeshProducers "Mesh.Producers" property.

See also: @ref Mesh

## Properties

@dl

@dt Scale
@dd Scale of implicit mesh.

@dt TriangleSize
@dd The size of generated triangles. The lower the value, the more detailed mesh will be produced. Be careful when setting very low values (below 0.05) because it will generate many thousands of triangles and take a long time to finish.

@dt Bounds
@dd Controls the outer bound of the area that will be evaluated. Set this value to cut-off large meshes.

@dt MultipleSurfaces
@dd If this property is set then the whole area will be searched for surfaces (much slower). Only set this if you have more than one separate surface that needs to be generated. For better performance it is very important to use the Bounds-parameter to limit the area that is searched.

@dlx

## List Properties

@dl

@dt @anchor MeshImplicitFunction Function
@dd The implicit function that will be evaluated for defining the mesh. The following components can be used here:

* @subpage ImplicitCombine
* @subpage ImplicitExpression
* @subpage ImplicitPrimitive
* @subpage ImplicitWarp

@dlx

# ImplicitWarp {#ImplicitWarp}

Warps the result of an implicit function.

This component can only be used in the @ref MeshImplicitFunction "MeshImplicit.Function" property.

See also: @ref MeshImplicit

## Properties

@dl

@dt Position
@dd Modifies position of Function (input surface) before applying warp.

@dt Scale
@dd Modifies scale of Function (input surface) before applying warp.

@dt Rotation
@dd Modifies rotation of Function (input surface) before applying warp.

@dt Kind
@dd The kind of warp that used be performed: Twist, Bend or Taper.

@dt WarpAmount
@dd Controls how much the warp will affect the final surface.

@dlx

## List Properties

@dl

@dt Function
@dd List of implicit function components forming an input that will be warped. The following components can be used here:

* @ref ImplicitCombine
* @ref ImplicitExpression
* @ref ImplicitPrimitive
* @ref ImplicitWarp

@dlx

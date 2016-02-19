# ImplicitExpression {#ImplicitExpression}

An implicit function defined by an expression.

This component can only be used in the @ref MeshImplicitFunction "MeshImplicit.Function" property.

See also: @ref MeshImplicit

## Properties

@dl

@dt Expression
@dd An expression that is called when the implicit surface polygonizer is sampling the surface. The special read-only properties "X","Y" and "Z" holds the current coordinates. Return a value representing the distance to the surface. Example of a valid expression that defines a sphere:

    return (X*X) + (Y*Y) + (Z*Z) - 1;
@dlx

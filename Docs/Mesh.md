# Mesh {#Mesh}

A representation of geometry that are used for 3D-graphics.

Example usage: See "BallMesh" in @ref ZPong sample project.

## Properties

@dl

@dt CurrentRecursion
@dd Read only. Reflects the current recursion depth if mesh has MeshLoop producer(s). It can be used in expressions to, for instance, reduce triangle count as mesh grew smaller, or set colors based on depth. Examples see in [the related Forum post](http://www.emix8.org/forum/viewtopic.php?p=2588#p2588).

@dlx

## List Properties

@dl

@dt @anchor MeshProducers Producers
@dd A list of components that provide the definition of the mesh. The following producer components are available:

* @subpage MeshBox
* @subpage MeshCombine
* @subpage MeshExpression
* @subpage MeshImplicit
* @subpage MeshImport
* @subpage MeshLoad
* @subpage MeshLoop
* @subpage MeshNoise
* @subpage MeshSphere
* @subpage MeshTransform

@dlx

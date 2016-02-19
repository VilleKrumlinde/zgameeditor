# Light {#Light}

Light used for rendering of objects. It should be placed in the ZApplicationLights "ZApplication.Lights" property.

OpenGL guarantees support for 8 lights.

When no lights are defined, the default is the active single-light model placed with @ref ZApplicationLightPosition "ZApplication.LightPosition".

## Properties

@dl

@dt Position
@dd Position of light.

@dt Color
@dd Color of light.

@dt Enabled
@dd Enables/disables the light. Disabled light is not taken into account for rendering objects.

@dt Kind
@dd One of the following:

* Directional - an infinitely distanced light with parallel light rays of the same intensity everywhere pointed from the direction of light Position to the point (0,0,0).
* Point - a light placed at a given position in space and giving off equal amounts of light in all directions. Distance from light does not influence intensity of light rays.
* Spot -  a light that radiates light from given position in a cone. Light intensity can be lower at borders of cone and higher in center. 

@dt SpotDirection
@dd Direction of spot light; axis of light cone. It is ignored for other kinds of light.

@dt SpotExponent
@dd Determines distribution of light intensity in cone of spot light.If 0 the light intensity is equal in the whole cone. If &gt;0 the intensity at border decreases. It is ignored for other kinds of light.

@dt SpotCutoff
@dd Determines angle of spot light cone around its axis. It is ignored for other kinds of light.

@dlx

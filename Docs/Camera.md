# Camera {#Camera}

Defines camera that can be used for rendering. Several cameras can be defined in project and then switched in runtime by assigning to the @ref ZApplicationCamera "ZApplication.Camera" property.

## Properties

@dl

@dt Kind
@dd Kind of camera. It can be one of the following:

* Perspective - usual 3D view
* Orthographic - akin to architectural drawings

@dt Position
@dd Position of camera.

@dt Rotation
@dd Rotation of camera.

@dt ClipNear
@dd The near clipping distance of OpenGL graphics. Objects that are closer to the camera than this value will not be drawn.

@dt ClipFar
@dd The far clipping distance of OpenGL graphics. Objects that are further away from the camera than this value will not be drawn.

@dt OrthoZoom
@dd Orthographic zoom. 1 means normal zoom, from 0 to 1 enlarges the rendered scene, and &gt; 1 renders the scene smaller. This property is applied only if Camera.Kind is set to Orthographic.

@dt FOV
@dd Camera Field Of View. Default value is 45 degrees.

@dlx

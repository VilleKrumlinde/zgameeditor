# Shader {#Shader}

This component allows you to use OpenGL shaders.

Example usage: @ref ShaderDemo sample project.

See also: @ref Material

External links: [OpenGL Shading Language on Wikipedia](https://en.wikipedia.org/wiki/OpenGL_Shading_Language) 

## Properties

@dl

@dt VertexShaderSource
@dd GLSL source code for the vertex shader.

  If the @ref ZApplicationGLBase "ZApplication.GLBase" property is set to "Compatible", you can use anything from [GLSL 1.1 shading language (external link)](http://www-evasion.imag.fr/Membres/Sebastien.Barbier/Enseignement/glsl_quickref.pdf).
  
  If the @ref ZApplicationGLBase "ZApplication.GLBase" property is set to "ES2/GL3", the shader can use the following built-in variables set by ZGameEditor:

  Variable | Semantics
  ---------|----------
  uniform mat4 modelViewProjectionMatrix | The current model-view-projection matrix.
  uniform mat4 modelViewMatrix | The current model-view matrix.
  uniform mat4 projectionMatrix | The current projection matrix.
  uniform mat3 normalMatrix | The current normal matrix. It is inverse-transpose of the upper 3x3 of the model-view matrix.
  uniform mat4 textureMatrix | The current texture matrix updated via RenderText or Material.TextureX/Y/Scale properties.
  uniform vec4 globalColor | The current color that is set by RenderSetColor.Color or Material.Color properties.
  attribute vec4 position | Vertex position in model.
  attribute vec3 normal | Vertex normal.
  attribute vec4 color | Vertex color.
  attribute vec2 texCoord | Vertex texture coordinate.
  
@dt FragmentShaderSource
@dd GLSL source code for the fragment (pixel) shader.

@dt UpdateVarsOnEachUse
@dd Setting this property on will update uniform variables before each usage of shader. If it is unset, shader variables are updated only on the beginning of rendering cycle.

@dt Handle
@dd Read only. The OpenGL handle of the shader. Use this when you want to make direct OpenGL calls. This value is available only in runtime. Example of usage Handle to obtain handle to attribute variable "position" in MyShader:

    positionAttrVar = glGetAttribLocation(MyShader.Handle, "position");

@dlx

## List Properties

@dl

@dt @anchor ShaderUniformVariables UniformVariables
@dd A list of uniform variables that may be referenced in the GLSL code. See @subpage ShaderVariable.

@dlx

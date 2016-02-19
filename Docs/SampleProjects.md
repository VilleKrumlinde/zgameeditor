# Sample Projects {#SampleProjects}

# Viewing Sample Projects

ZGameEditor includes several sample projects located in the _Projects_ sub-folder of the installation directory.

To open and run a sample project do the following steps:

1. Run the ZGameEditor - execute the ZGameEditor.exe file from the unzipped directory. The editor window appears.

2. Press the "File / Open..." menu item. The Open dialog appears.

3. Select a sample project file, e.g., the `ZPong.zgeproj` project and press the "Open" button. Project is loaded into editor and you can see its structure in @ref ProjectTree "Project tree" panel.

4. To run the project as a standalone application, press the F9 key.

5. Play the game and when finished, press the Escape key to return to the editor.

6. You can browse the project tree, properties of components and see how it's made.

7. You can modify properties of components or add/remove additional components to customize the project as you wish. To preview the project in editor itself, press the "Start" button in the ZApplication's @ref PreviewWindow or press the ALT+Enter key. Components and their properties can be changed during application previewing.

8. To stop previewing, press the "Stop" button or press ALT+Enter key again. 

Follows the description of some of the sample projects that are included in ZGameEditor distribution.

# ZBlast
@anchor ZBlast

Arena style shooting game with abstract graphics.

![&nbsp;](proj-zblast.png)

# FpsDemo
@anchor FpsDemo

A simple First Person Shooter style demo demonstrating that 3d games are possible with ZGameEditor.

![](proj-fpsdemo.png)

# ShaderDemo
@anchor ShaderDemo

Demonstrates the use of GLSL vertex and fragment shaders.

![](proj-shaderdemo.png)

# CleanseCube
@anchor CleanseCube

A short non-interactive demo that demonstrates the use of imported bitmap graphics and a warped 3D cube.

![](proj-cleanse.png)

# ZPong
@anchor ZPong

A simple "Pong"-clone. Demonstrates stereo sound and simple game logic.

![](proj-zpong.png)

# Particles
@anchor Particles

Particle effects. Can also be generated as a screen saver.

![](proj-particles.png)

# TripleE
@anchor TripleE

A small shoot-em-up game.

![](proj-triplee.png)

Features:
* title and game over screen
* scoring
* levels and stages
* on-the-fly generation of models, every enemy is slightly different

# Steering
@anchor Steering

Demonstrates steering behaviors.

![](proj-steering.png)

# About

This is the source for the About box animation inside ZGameEditor.

![](proj-about.png)

# Implicit
@anchor Implicit

A test project for the experimental implicit surface polygonizer.

![](proj-implicit.png)

# FileDemo
@anchor FileDemo

Demonstrates how to use the @ref File component to load and save data from a text file.

![](proj-filedemo.png)

# YakYakReader
@anchor YakYakReader

Displays the [YakYak](http://www.yakyak.org/index.php) active topics. Left mouse click on a topic opens this in a Web browser.

![](proj-yakyak.png)

# RenderTexture
@anchor RenderTexture

Non-interactive demo that demonstrates usage of @ref RenderTarget and @ref SetRenderTarget components. It renders a dynamic scene to  texture which is in turn used for rendering each size of a rotating cube.

![](proj-rendertexture.png)

# RenderPass
@anchor RenderPass

Demonstrates usage of two render passes; see @ref ZApplicationRenderPasses "ZApplication.RenderPasses" for details. In the first render pass a rotating spiral is rendered to a @ref RenderTarget, and in the second render pass the RenderTarget is used as a texture for rendering two sprites and also the large spiral is rendered to the screen.

![](proj-renderpass.png)

# ModPlay
@anchor ModPlay

Plays a music file using the external library BASS.dll. Shows also the text with the current volume level and illustrates this value as size of the rendered beams.

![](proj-modplay.png)
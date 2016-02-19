# RenderNet {#RenderNet}

Render a 2d-net with XCount*YCount number of vertices using the current material. The vertices and texture coordinates can be modified using a expression before rendering. This can be used for animating water-like surfaces.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

## Properties

@dl

@dt XCount
@dd Number of vertices in the X axis.

@dt YCount
@dd Number of vertices in the Y axis.

@dt RenderVertexExpression
@dd An expression that will be called once for each vertex before rendering starts. The following properties can be modified in the expression: Vertex, TexCoord and Color. Since this expression will be called many times (XCount*YCount each frame) in realtime, be careful of using large expressions or performance will suffer. Example for 20x20 net with vertex colors:

    float d = sqrt(Vertex.X*Vertex.X + Vertex.Y*Vertex.Y);
    Vertex.X *= 10;
    Vertex.Y *= 10;
    Vertex.Z = cos(d*10);
    Color.R = abs(Vertex.Z);
    Color.G = Color.R;
    Color.B = 1.5 - Color.R;

In the Model preview check "Update time" to see changing of the mesh in time.
 
@dt VertexColors
@dd If set the RenderVertexExpression computes vertex colors. If not set, the vertex colors are not computed/changed.

@dt Vertex
@dd Position of the current vertex. It can be read and changed in the RenderVertexExpression, but not outside this component.

@dt TexCoord
@dd Texture coordinated for the current vertex. It can be read and changed in the RenderVertexExpression, but not outside this component.

@dt Color
@dd Color of the current vertex. It can be read and changed in the RenderVertexExpression, but not outside this component.

@dlx

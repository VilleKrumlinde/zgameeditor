# FileMoveData {#FileMoveData}

Transfer data between a file and a variable. This component can only be used in File.OnRead or File.OnWrite list properties.

See also: @ref File, @ref FileAction.

Example usage: @ref FileDemo sample project.

## Properties

@dl

@dt Property
@dd A reference to the floating point property that will receive the value from a file. Value of this property is an @ref ScriptingLanguage "expression" specified in @ref CodeEditor "Code editor". Examples: "TileId.Value", "EnemyModel.Position.X".

@dlx

# File {#File}

Represents a file on disk. This can be used to read data into variables, or writing data from variables.

See also: @ref FileAction, @ref FileMoveData

Example usage: @ref FileDemo and @ref FpsDemo sample projects.

## Properties

@dl

@dt @anchor FileFileName FileName
@dd The name of the file that will be opened, in a path relative to the current project. Examples: "Data.txt", "levels\Level".

@dt FileNameFloatRef
@dd The name of a property that will have its value appended to the FileName. The value is rounded to the nearest integer. For example if FileName is "LevelMap" and FileNameFloatRef is set to a variable named "LevelNr" with a value of 5 then the file opened will be "LevelMap5".

@dt FileEmbedded
@dd Embed a file into your project so that you only need to redistribute the EXE file alone. If this property is set then FileName and FileNameFloatRef properties are ignored.

@dt Encoding
@dd Controls how data will be parsed when reading from the file.

* Char: One byte is read at a time. The value read is an integer between 0 and 255 (the ASCII value of the character in a text file).
* Binary: Four bytes is read at a time. This can be used to read variables with the full decimal precision.

@dt TargetArray
@dd Set this to an array that the data will be written to or read from.

@dt Position
@dd Read only. Current read/write position in the file.

@dt Size
@dd Read only. Size of the file. This is set when FileAction Read is executed.

@dlx

## List Properties

@dl

@dt @anchor FileOnRead OnRead
@dd A list of the commands that will be executed when a read FileAction is issued. For reading data into variables use the @ref FileMoveData component.

@dt @anchor FileOnWrite OnWrite
@dd A list of the commands that will be executed when a write FileAction is issued. For writing data from variables use the @ref FileMoveData component.

@dlx

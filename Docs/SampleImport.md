# SampleImport {#SampleImport}

Producer that imports a sound from RAW or OGG audio file. The data is copied into the project, so the imported file is no longer referred.

This component can only be used in the @ref SampleProducers "Sample.Producers" property.

See also: @ref Sample, @ref SampleExpression

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?t=711)

## Properties

@dl

@dt SampleData
@dd Data of imported sample. Size shows its size in bytes, the "Clear" button clears the imported data, and the "Import" button imports the sample data from selected file. Importing a sample file also sets values of SampleRate, SampleFormat and SampleFileFormat properties, which can be later changed.

@dt SampleRate
Sample rate for RAW data expressed in Hz.

@dt SampleFormat
@dd Sample bit format for RAW data. It can be:

* 8 bit signed, or
* 16 bit signed

@dt SampleFileFormat
@dd Format of imported data. It can be:

* RAW, or
* OGG

@dlx

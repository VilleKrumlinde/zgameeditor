# Sample {#Sample}

Defines a single sound. It can either be procedural (computed by expression), imported from file or imported and then procedurally processed. Sample is used to define @ref Sound.

See also: @ref Sound, @ref SampleExpression, @ref SampleImport

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?t=711)

## Properties

@dl

@dt Length
@dd Length of sample in seconds. After importing a sample from file to the @ref SampleImport component, the Length property is automatically set to the length of the imported sample.

@dlx

## List Properties

@dl

@dt @anchor SampleProducers Producers
@dd List of sample producers executed in the specified order. One producer can take output of the previous one and change it. Sample producer can be one of the following:

* @subpage SampleExpression
* @subpage SampleImport

@dlx

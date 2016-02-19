# SampleExpression {#SampleExpression}

A sample producer that can generate or modify a sample sound.

This component can only be used in the @ref SampleProducers "Sample.Producers" property.

See also: @ref Sample, @ref SampleImport

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?t=711)

## Properties

@dl

@dt Expression
@dd An expression which defines or modifies sample sound. The expression can use special properties "Sample" and "Time"; see later. The keyword "this" refers to the current SampleExpression component.

Example of expression for generating a sound:

    float T = this.Time*PI*2;
    this.Sample = sin(T*165+32+16*cos(T/16)*sin(T*123.75));

Example of expression for modifying of sound from previous producer:

    this.Sample *= sin(Time * freq) * volume;
 
@dt Sample
@dd Read-write property initially set to the current signal value of sample sound. Its value is in interval from -1 to 1. This property can be accessed only from the Expression property.

@dt Time
@dd Read-only property set to the current playing time of sample sound. This property can be accessed only from the Expression property.

@dlx

# KeyPress {#KeyPress}

Test keyboard input, and executes a component list if a key is pressed.

## Properties

@dl

@dt @anchor Keys Keys @dd A string of characters for which keypresses will be
detected. Use capital letters. To test for the A key just set the value "A". If
several characters are set, then the @ref KeyIndex property will be set to the
index of which character was pressed. For instance if keys are set to "QA" then
if the Q key is pressed KeyIndex will have the value 0, and if the A key is
pressed it will have the value 1.

Special characters and their meaning:

- "{" : left mouse button
- "|" : middle mouse button
- "}" : right mouse button
- "^" : cursor up
- "\_" : cursor down
- "<" : cursor left
- ">" : cursor right

@dt CharCode @dd This is an alternative way of specifying which key is pressed.

Special values and their meaning:

- 9 : Tab
- 13 : Return
- 16 : Shift
- 17 : Control
- 27 : Escape
- 253 : Back button on Android
- 254 : Android application got focus
- 255 : Android application lost focus

@note To use the CharCode property the Keys property must be blank. They cannot
be combined. Also character codes are not guaranteed to be consistent on other
platforms (Mac and Linux).

@dt RepeatDelay @dd A delay in seconds for which a keypress is repeated.

@dt @anchor KeyIndex KeyIndex @dd See the description for @ref Keys property.

@dlx

## List Properties

@dl

@dt OnPressed @dd The list of components that is executed when a keypress is
detected.

@dlx

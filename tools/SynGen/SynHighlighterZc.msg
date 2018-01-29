TSynZcSyn   {first Identifier is considered to be the Class Name }
tk              {second Identifier is considered to be the Identifier Prefix }
IdentStart '_', 'a'..'z', 'A'..'Z':: '_', '0'..'9', 'a'..'z', 'A'..'Z'::

TOKENTYPES
Identifier
Comment Foreground=clNavy
Space
String Foreground=clRed
Key Style=[fsBold]
|><|

KEYS
int
float
string
void
while
if
for
break
continue
switch
case
return
else
model
const
private
inline
|><|

CHARS

'A'..'Z', 'a'..'z', '_':: Ident
BeginProc
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
EndProc

|><|

ENCLOSEDBY

Comment,CppComment,//,
Comment,CStyleComment,/*,*/,MultiLine
String,String,","

|><|

(**
===============================================================================================
Name    : LibXmlParserU

Project : All Projects using the Unicode versions of Delphi (Delphi 9 and up) and FPC

Subject : Progressive XML 1.0 Parser for all types of XML 1.0 Files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tübingen
          Germany
E-Mail:   stefan@destructor.de
URL:      www.destructor.de/xmlparser
===============================================================================================
Source, Legals ("Licence")
--------------------------
The official site to get this parser is http://www.destructor.de/xmlparser/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

In short: Usage and distribution of this source code is free.
          You use it completely on your own risk.
          You can use it for personal or commercial work.
===============================================================================================
!!!  All parts of this code which are not finished or not conforming exactly to
     the XmlSpec are marked with three exclamation marks

-!-  Parts where the parser may be able to detect errors in the document's syntax are
     marked with the dash-exlamation mark-dash sequence.
===============================================================================================
Terminology:
------------
- Start:   Start of a buffer part
- Final:   End (last character) of a buffer part
- DTD:     Document Type Definition
- DTDc:    Document Type Declaration
- XMLSpec: The current W3C XML 1.0 Recommendation (version 1.0 TE as of 2004-02-04), Chapter No.
- Cur*:    Fields concerning the "Current" part passed back by the "Scan" method
===============================================================================================
Scanning the XML document
-------------------------
- Create TXmlParser Instance                     MyXml := TXmlParser.Create;
- Load XML Document                              MyXml.LoadFromFile (Filename);
                                                   or .LoadFromBuffer
                                                   or .LoadFromString
                                                   or .LoadFromStrings
                                                   or .SetBuffer
- Start Scanning                                 MyXml.StartScan;
- Scan Loop                                      while MyXml.Scan do
- Test for Part Type                               case MyXml.CurPartType of
- Handle Parts                                       ... : ;;;
- Handle Parts                                       ... : ;;;
- Handle Parts                                       ... : ;;;
                                                     end;
- Destroy                                        MyXml.Free;
===============================================================================================
Loading the XML document
------------------------
You can load the XML document from a file with the "LoadFromFile" method.
It is beyond the scope of this parser to perform HTTP or FTP accesses. If you want your
application to handle such requests (URLs), you can load the XML via HTTP or FTP or whatever
protocol and hand over the data buffer using the "LoadFromBuffer" or "SetBuffer" method.
"LoadFromBuffer" loads the internal buffer of TXmlParser with the given null-terminated
string, thereby creating a copy of that buffer.
"SetBuffer" just takes the pointer to another buffer, which means that the given
buffer pointer must be valid while the document is accessed via TXmlParser.
===============================================================================================
Encodings:
----------
This XML parser only reads 8-Bit encodings (like UTF-8, ISO-8859-x, Windows-12xx, ASCII etc.)
It is not able to read UTF-16/UCS-2 or UTF-32/UCS-4 files. You'll have to translate these
to UTF-8 first. However, as storage of XML in UTF-16 is unusual these days (see
http://www.utf8everywhere.org/), I think this is not a major restriction.
The methods .LoadFromString and .LoadFromStrings can be used to pass UTF-16 strings to
the parser, those will do the UTF-8 encoding.
===============================================================================================
Buffer Handling
---------------
- The document must be loaded completely into RAM
- The TXmlParser instance can either "own" the buffer itself (then, FBufferSize is > 0)
  or reference the buffer of another instance or object (then, FBuffersize is 0 and
  FBuffer is not NIL)
- The Property DocBuffer passes back a pointer to the first byte of the document. If there
  is no document stored (FBuffer is NIL), the DocBuffer returns a pointer to a NULL character.
===============================================================================================
Whitespace Handling
-------------------
The TXmlParser property "Normalize" determines how Whitespace is returned in Text Content:
While Normalize is true, all leading and trailing whitespace characters are trimmed of, all
Whitespace is converted to Space #x20 characters and contiguous Whitespace characters are
compressed to one.
If the "Scan" method reports a ptContent part, the application can get the original text
with all whitespace characters by extracting the characters from "CurStart" to "CurFinal".
If the application detects an xml:space attribute, it can set "Normalize" accordingly or
use CurStart/CurFinal.
Please note that TXmlParser does _not_ normalize Line Breaks to single LineFeed characters
as the XMLSpec requires (XMLSpec 2.11).
The xml:space attribute is not handled by TXmlParser. This is on behalf of the application.
===============================================================================================
Non-XML-Conforming
------------------
TXmlParser does not conform 100 % exactly to the XMLSpec:
- UTF-16 is not supported (XMLSpec 2.2)
  (Workaround: Convert UTF-16 to UTF-8 and hand the buffer over to TXmlParser)
- Line breaks are not normalized to single Linefeed #x0A characters (XMLSpec 2.11)
  (Workaround: The Application can access the text contents on its own [CurStart, CurFinal],
  thereby applying every normalization it wishes to)
- See also the code parts marked with three consecutive exclamation marks. These are
  parts which are not finished in the current code release.

This list may be incomplete, so it may grow if I get to know any other points.
As work on the parser proceeds, this list may also shrink.
===============================================================================================
Things Todo
-----------
- Introduce a new event/callback which is called when there is an error in the XML
===============================================================================================
Change History, Version numbers
-------------------------------
The Date is given in ISO Year-Month-Day (YYYY-MM-DD) order.
Versions are counted from 2.0.0 beginning with the version from 2014-05-15.
Unreleased versions don't get a version number.

Date        Author Version Changes
-----------------------------------------------------------------------------------------------
2014-05-15  HeySt  2.0.0   Copied from 1.0.20, reworked for Unicode support
2014-05-27  HeySt  2.0.1   Changes for FPC, .LoadFromString, .LoadFromStrings
2016-04-25  HeySt  2.0.2   .LoadFromBuffer with Size parameter, .LoadFromStream
2018-02-07  HeySt          (Re)Introduced the TXmlScanner and TXmlEasyScanner classes
2018-10-08  HeySt  2.0.3   Memory Leak fix in TXmlParser.LoadFromStream
2019-02-28  HeySt  2.0.4   Off-by-one fixed in LoadFromStream, wrote terminator beyond buffer
2019-08-04  HeySt  2.0.5   PresetEncoding and IgnoreEncoding
*)

(*$IFDEF FPC *)
  (*$MODE delphiunicode *)   // It's Unicode FreePascal. Welcome.
(*$ELSE *)
  (*$IFNDEF UNICODE *)  This code is only for the Unicode versions of Delphi.          (*$ENDIF *)
(*$ENDIF *)
(*$IFDEF MANAGEDCODE *) This code will not compile as Managed Code. Too many pointers. (*$ENDIF *)
(*$IFDEF CLR *)         This code will not compile as Managed Code. Too many pointers. (*$ENDIF *)

(*$R-  Switch Range Checking Off              *)
(*$B-  Switch Complete Boolean Evaluation Off *)
(*$WARN SYMBOL_DEPRECATED OFF *)

(*.$R LibXmlComps.dcr *)   // Only needed when you want to register the TXmlScanner and TEasyXmlScanner components

unit LibXmlParserU;

interface

uses
  Windows, SysUtils, Classes, Contnrs;

const
  CVersion      = '2.0.5';           // Release version number
  CUnknownChar  : char = #$00BF;     // Replacement for unknown/untransformable character references (inverted question mark)
  CP_UTF8       = 65001;             // UTF-8 Codepage number

type
  TCodePage    = longword;
  TPartType    = // --- Document Part Types
                 (ptNone,            // Nothing
                  ptXmlProlog,       // XML Prolog                  XmlSpec 2.8 / 4.3.1
                  ptComment,         // Comment                     XmlSpec 2.5
                  ptPI,              // Processing Instruction      XmlSpec 2.6
                  ptDtdc,            // Document Type Declaration   XmlSpec 2.8
                  ptStartTag,        // Start Tag                   XmlSpec 3.1
                  ptEmptyTag,        // Empty-Element Tag           XmlSpec 3.1
                  ptEndTag,          // End Tag                     XmlSpec 3.1
                  ptContent,         // Text Content between Tags
                  ptCData);          // CDATA Section               XmlSpec 2.7

  TDtdElemType = // --- DTD Elements
                 (deElement,         // !ELEMENT declaration
                  deAttList,         // !ATTLIST declaration
                  deEntity,          // !ENTITY declaration
                  deNotation,        // !NOTATION declaration
                  dePI,              // PI in DTD
                  deComment,         // Comment in DTD
                  deError);          // Error found in the DTD

type
  TAttrList    = class;
  TEntityStack = class;
  TNvpList     = class;
  TElemDef     = class;
  TElemList    = class;
  TEntityDef   = class;
  TNotationDef = class;

  TDtdElementRec = record    // --- This record is returned by the DTD parser callback function
                     Start, Final : PAnsiChar;                         // Start/End of the Element's Declaration
                     case ElementType : TDtdElemType OF                // Type of the Element
                       deElement,                                      // <!ELEMENT>
                       deAttList  : (ElemDef      : TElemDef);         // <!ATTLIST>
                       deEntity   : (EntityDef    : TEntityDef);       // <!ENTITY>
                       deNotation : (NotationDef  : TNotationDef);     // <!NOTATION>
                       dePI       : (Target       : PChar;             // <?PI ?>
                                     Content      : PChar;
                                     AttrList     : TAttrList);
                       deError    : (Pos          : PAnsiChar);            // Error
                       // deComment : ((No additional fields here));   // <!-- Comment -->
                   end;

  TXmlParser = class
               protected                            // --- Internal Properties and Methods
                 FBuffer         : PAnsiChar;       // nil if there is no buffer available
                 FBufferSize     : NativeInt;       // 0 if the buffer is not owned by the Document instance
                 FSource         : string;          // Name of Source of document. Filename for Documents loaded with LoadFromFile

                 FXmlVersion     : string;          // XML version from Document header. Default is '1.0'
                 FEncoding       : string;          // Encoding from Document header (always uppercase). Default is 'UTF-8'
                 FCodePage       : TCodePage;       // Numerical code page, corresponds with FEncoding
                 FIgnoreEncoding : boolean;         // Ignore Encoding in XML Prolog and use pre-set FEncoding and FCodePage
                 FStandalone     : boolean;         // Standalone declaration from Document header. Default is 'yes'
                 FRootName       : string;          // Name of the Root Element (= DTD name)
                 FDtdcFinal      : PAnsiChar;       // Pointer to the '>' character terminating the DTD declaration

                 FNormalize      : boolean;         // If true: Pack Whitespace and don't return empty contents
                 EntityStack     : TEntityStack;    // Entity Stack for Parameter and General Entities
                 FCurEncoding    : string;          // Current Encoding during parsing (always uppercase)
                 FCurCodePage    : TCodePage;       // Numerical code page, corresponds with FCurEncoding

                 procedure AnalyzeProlog;                                                 // Analyze XML Prolog or Text Declaration
                 procedure AnalyzeComment (Start : PAnsiChar; var Final : PAnsiChar);     // Analyze Comments
                 procedure AnalyzePI      (Start : PAnsiChar; var Final : PAnsiChar);     // Analyze Processing Instructions (PI)
                 procedure AnalyzeDtdc;                                                   // Analyze Document Type Declaration
                 procedure AnalyzeDtdElements (Start : PAnsiChar; out Final : PAnsiChar); // Analyze DTD declarations
                 procedure AnalyzeTag;                                                    // Analyze Start/end/Empty-Element Tags
                 procedure AnalyzeCData;                                                  // Analyze CDATA Sections
                 procedure AnalyzeText (var IsDone : boolean);                            // Analyze Text Content between Tags
                 procedure AnalyzeElementDecl  (Start : PAnsiChar; out Final : PAnsiChar);
                 procedure AnalyzeAttListDecl  (Start : PAnsiChar; out Final : PAnsiChar);
                 procedure AnalyzeEntityDecl   (Start : PAnsiChar; out Final : PAnsiChar);
                 procedure AnalyzeNotationDecl (Start : PAnsiChar; out Final : PAnsiChar);

                 procedure PushPE (var Start : PAnsiChar);
                 procedure ReplaceCharacterEntities (var Str : string);
                 procedure ReplaceParameterEntities (var Str : string);

                 function GetDocBuffer : PAnsiChar;  // Returns FBuffer or a pointer to a NUL char if Buffer is empty
               public                         // --- Document Properties
                 property XmlVersion : string      read FXmlVersion;             // XML version from the Document Prolog
                 property Encoding   : string      read FEncoding;               // Document Encoding from Prolog
                 property CodePage   : TCodePage   read FCodePage;               // CodePage of Encoding
                 property Standalone : boolean     read FStandalone;             // Standalone Declaration from Prolog
                 property RootName   : string      read FRootName;               // Name of the Root Element
                 property Normalize  : boolean     read FNormalize write FNormalize; // True if Content is to be normalized
                 property Source     : string      read FSource;                 // Name of Document Source (Filename)
                 property DocBuffer  : PAnsiChar   read GetDocBuffer;            // Returns document buffer
               public                         // --- DTD Objects
                 Elements    : TElemList;     // Elements: List of TElemDef (contains Attribute Definitions)
                 Entities    : TNvpList;      // General Entities: List of TEntityDef
                 ParEntities : TNvpList;      // Parameter Entities: List of TEntityDef
                 Notations   : TNvpList;      // Notations: List of TNotationDef
               public
                 constructor Create;
                 destructor Destroy;                                      override;

                 // --- Document Handling
                 procedure PresetEncoding  (Encoding : string);                   // Pre-sets the root document encoding
                 function  LoadFromFile    (Filename : string;
                                            FileMode : integer = fmOpenRead OR fmShareDenyNone) : boolean;
                                                                                  // Loads Document from given file
                 function  LoadFromBuffer  (Buffer : PAnsiChar; Size : integer = -1) : boolean;  // Loads Document from another buffer. Size < 0: buffer is null terminated
                 function  LoadFromString  (const XmlString : string) : boolean;  // Loads Document from UnicodeString (UTF-16!)
                 function  LoadFromStrings (Xml : TStrings) : boolean;            // Loads Document from TStrings buffer
                 function  LoadFromStream  (Stream : TStream) : boolean;          // Loads Document from Stream
                 procedure SetBuffer       (Buffer : PAnsiChar);                  // References another buffer
                 procedure Clear;                                                 // Clear Document
               public
                 // --- Scanning through the document
                 CurPartType : TPartType;                             // Current Type
                 CurName     : string;                                // Current Name
                 CurContent  : string;                                // Current Normalized Content
                 CurStart    : PAnsiChar;                             // Current First character
                 CurFinal    : PAnsiChar;                             // Current Last character
                 CurAttr     : TAttrList;                             // Current Attribute List
                 property CurEncoding    : string    read FCurEncoding;
                 property CurCodePage    : TCodePage read FCurCodePage;
                 property IgnoreEncoding : boolean   read FIgnoreEncoding;
                 procedure StartScan;
                 function  Scan : boolean;

                 // --- Events / Callbacks
                 function  LoadExternalEntity (SystemId, PublicId, Notation : string) : TXmlParser;  virtual;
                 procedure DtdElementFound    (DtdElementRec : TDtdElementRec);                      virtual;
               end;

  TValueType   = // --- Attribute Value Type
                 (vtNormal,       // Normal specified Attribute
                  vtImplied,      // #IMPLIED attribute value
                  vtFixed,        // #FIXED attribute value
                  vtDefault);     // Attribute value from default value in !ATTLIST declaration

  TAttrDefault = // --- Attribute Default Type
                 (adDefault,      // Normal default value
                  adRequired,     // #REQUIRED attribute
                  adImplied,      // #IMPLIED attribute
                  adFixed);       // #FIXED attribute

  TAttrType    = // --- Type of attribute
                 (atUnknown,      // Unknown type
                  atCData,        // Character data only
                  atID,           // ID
                  atIdRef,        // ID Reference
                  atIdRefs,       // Several ID References, separated by Whitespace
                  atEntity,       // Name of an unparsed Entity
                  atEntities,     // Several unparsed Entity names, separated by Whitespace
                  atNmToken,      // Name Token
                  atNmTokens,     // Several Name Tokens, separated by Whitespace
                  atNotation,     // A selection of Notation names (Unparsed Entity)
                  atEnumeration); // Enumeration

  TElemType    = // --- Element content type
                 (etEmpty,        // Element is always empty
                  etAny,          // Element can have any mixture of PCDATA and any elements
                  etChildren,     // Element must contain only elements
                  etMixed);       // Mixed PCDATA and elements

  TNvpNode  = class               // Name-Value Pair Node
                 Name  : string;
                 Value : string;
                 constructor Create (TheName : string = ''; TheValue : string = '');
              end;

  TNvpList  = class (TObjectList)       // Name-Value Pair List
                procedure Add   (Node  : TNvpNode);
                function  Node  (Name  : string)  : TNvpNode;          overload;
                function  Node  (Index : integer) : TNvpNode;          overload;
                function  Value (Name  : string)  : string;            overload;
                function  Value (Index : integer) : string;            overload;
                function  Name  (Index : integer) : string;
              end;

  TAttr     = class (TNvpNode)          // Attribute of a Start-Tag or Empty-Element-Tag
                 ValueType : TValueType;
                 AttrType  : TAttrType;
               end;

  TAttrList = class (TNvpList)          // List of Attributes
                procedure Analyze (Start : PAnsiChar; var Final : PAnsiChar; CodePage : integer);
              end;

  TEntityStack = class (TObjectList)    // Stack where current position is stored before parsing entities
                 protected
                   Owner : TXmlParser;
                 public
                   constructor Create (TheOwner : TXmlParser);
                   procedure Push (LastPos : PAnsiChar);                                                            overload;
                   procedure Push (ExternalEntity : TXmlParser; LastPos : PAnsiChar);                               overload;
                   procedure Push (LastPos : PAnsiChar; const EntityReplacement : string; out NewPos : PAnsiChar);  overload;
                   function  Pop : PAnsiChar;         // Returns next char or NIL if EOF is reached. Frees Instance.
                 end;

  TAttrDef    = class (TNvpNode)        // Represents a <!ATTLIST Definition. "Value" is the default value
                  TypeDef     : string;           // Type definition from the DTD
                  Notations   : string;           // Notation List, separated by pipe symbols '|'
                  AttrType    : TAttrType;        // Attribute Type
                  DefaultType : TAttrDefault;     // Default Type
                end;

  TElemDef    = class (TNvpList)       // Represents a <!ELEMENT Definition. Is a list of TAttrDef-Nodes
                  Name       : string;            // Element name
                  ElemType   : TElemType;         // Element type
                  Definition : string;            // Element definition from DTD
                end;

  TElemList   = class (TObjectList)    // List of TElemDef nodes
                  function  Node (Name : string) : TElemDef;
                  procedure Add (Node : TElemDef);
                end;

  TEntityDef  = class (TNvpNode)       // Represents a <!ENTITY Definition.
                  SystemId     : string;
                  PublicId     : string;
                  NotationName : string;
                end;

  TNotationDef = class (TNvpNode)      // Represents a <!NOTATION Definition. Value is the System ID
                   PublicId : string;
                 end;

const
  CWhitespace   = [#32, #9, #13, #10];                // Whitespace characters (XmlSpec 2.3)
  CLetter       = [#$41..#$5A, #$61..#$7A, #$C0..#$D6, #$D8..#$F6, #$F8..#$FF];
  CDigit        = [#$30..#$39];
  CNameChar     = CLetter + CDigit + ['.', '-', '_', ':', #$B7];
  CNameStart    = CLetter + ['_', ':'];
  CQuoteChar    = ['"', ''''];
  CPubidChar    = [#32, ^M, ^J, #9, 'a'..'z', 'A'..'Z', '0'..'9',
                   '-', '''', '(', ')', '+', ',', '.', '/', ':',
                   '=', '?', ';', '!', '*', '#', '@', '$', '_', '%'];

  CDStart       = '<![CDATA[';
  CDEnd         = ']]>';

  CUtf8BOM      = #$EF#$BB#$BF;   // The Byte Order Mark (U+FEFF) coded in UTF-8

  // --- Name Constants for the above enumeration types
  CPartType_Name    : array [TPartType] of string =
                      ('', 'XML Prolog', 'Comment', 'PI',
                       'DTD Declaration', 'Start Tag', 'Empty Tag', 'End Tag',
                       'Text', 'CDATA');
  CValueType_Name   : array [TValueType]    of string = ('Normal', 'Implied', 'Fixed', 'Default');
  CAttrDefault_Name : array [TAttrDefault]  of string = ('Default', 'Required', 'Implied', 'Fixed');
  CElemType_Name    : array [TElemType]     of string = ('Empty', 'Any', 'Childs only', 'Mixed');
  CAttrType_Name    : array [TAttrType]     of string = ('Unknown', 'CDATA',
                                                         'ID', 'IDREF', 'IDREFS',
                                                         'ENTITY', 'ENTITIES',
                                                         'NMTOKEN', 'NMTOKENS',
                                                         'Notation', 'Enumeration');

function  BufToStr    (const Start, Finish : PAnsiChar;              const CodePage : TCodePage) : string;  overload;
function  BufToStr    (const Start : PAnsiChar; const Len : integer; const CodePage : TCodePage) : string;  overload;
function  BufToStr    (const Start : PAnsiChar;                      const CodePage : TCodePage) : string;  overload;
function  ConvertWs   (Source: string; PackWs: boolean) : string;                           // Convert Whitespace to spaces #x20
function  TrimWs      (const Source : string) : string;                                     // Trim Whitespace

type
  TEncodingRec = record
                   Encoding : string;
                   CodePage : TCodePage;
                   Name     : string;
                 end;
const
  CEncodingCount = 23;
  CEncoding : array [1..CEncodingCount] of TEncodingRec = (
              (Encoding: 'UTF-8';        CodePage : 65001; Name : 'Unicode UTF-8'),
              (Encoding: 'ISO-8859-1';   CodePage : 28591; Name : 'Latin-1'),
              (Encoding: 'ISO-8859-2';   CodePage : 28592; Name : 'Latin-2'),
              (Encoding: 'ISO-8859-3';   CodePage : 28593; Name : 'Latin-3'),
              (Encoding: 'ISO-8859-4';   CodePage : 28594; Name : 'Baltic'),
              (Encoding: 'ISO-8859-5';   CodePage : 28595; Name : 'Cyrillic'),
              (Encoding: 'ISO-8859-6';   CodePage : 28596; Name : 'Arabic'),
              (Encoding: 'ISO-8859-8';   CodePage : 28598; Name : 'Hebrew'),
              (Encoding: 'ISO-8859-9';   CodePage : 28599; Name : 'Turkish'),
              (Encoding: 'ISO-8859-15';  CodePage : 28605; Name : 'Latin 9'),
              (Encoding: 'IBM850';       CodePage :   850; Name : 'Western DOS Latin 1'),
              (Encoding: 'WINDOWS-1250'; CodePage :  1250; Name : 'Central European'),
              (Encoding: 'WINDOWS-1251'; CodePage :  1251; Name : 'Cyrillic'),
              (Encoding: 'WINDOWS-1252'; CodePage :  1252; Name : 'Western'),
              (Encoding: 'WINDOWS-1253'; CodePage :  1253; Name : 'Greek'),
              (Encoding: 'WINDOWS-1254'; CodePage :  1254; Name : 'Turkish'),
              (Encoding: 'WINDOWS-1255'; CodePage :  1255; Name : 'Hebrew'),
              (Encoding: 'WINDOWS-1256'; CodePage :  1256; Name : 'Arabic'),
              (Encoding: 'WINDOWS-1257'; CodePage :  1257; Name : 'Baltic'),
              (Encoding: 'WINDOWS-1258'; CodePage :  1258; Name : 'Vietnamese'),
              (Encoding: 'US-ASCII';     CodePage : 20127; Name : 'US-ASCII'),
              (Encoding: 'ASCII';        CodePage : 20127; Name : 'US-ASCII'),
              (Encoding: 'UTF-7';        CodePage : 65000; Name : 'Unicode UTF-7')
              );

function EncodingToCodePage (const Encoding : string) : TCodePage;

(*
===============================================================================================
TCustomXmlScanner: event based component wrapper for TXmlParser
===============================================================================================
*)

type
  TXmlPrologEvent   = procedure (Sender : TObject; XmlVersion, Encoding: string; Standalone : boolean) of object;
  TCommentEvent     = procedure (Sender : TObject; Comment : string)                                   of object;
  TPIEvent          = procedure (Sender : TObject; Target, Content: string; Attributes : TAttrList)    of object;
  TDtdEvent         = procedure (Sender : TObject; RootElementName : string)                           of object;
  TStartTagEvent    = procedure (Sender : TObject; TagName : string; Attributes : TAttrList)           of object;
  TEndTagEvent      = procedure (Sender : TObject; TagName : string)                                   of object;
  TContentEvent     = procedure (Sender : TObject; Content : string)                                   of object;
  TElementEvent     = procedure (Sender : TObject; ElemDef : TElemDef)                                 of object;
  TEntityEvent      = procedure (Sender : TObject; EntityDef : TEntityDef)                             of object;
  TNotationEvent    = procedure (Sender : TObject; NotationDef : TNotationDef)                         of object;
  TErrorEvent       = procedure (Sender : TObject; ErrorPos : PAnsiChar)                               of object;
  TExternalEvent    = procedure (Sender : TObject; SystemId, PublicId, NotationId : string;
                                 var Result : TXmlParser)                                              of object;

  TCustomXmlScanner = class (TComponent)
    protected
      FXmlParser            : TXmlParser;
      FOnXmlProlog          : TXmlPrologEvent;
      FOnComment            : TCommentEvent;
      FOnPI                 : TPIEvent;
      FOnDtdRead            : TDtdEvent;
      FOnStartTag           : TStartTagEvent;
      FOnEmptyTag           : TStartTagEvent;
      FOnEndTag             : TEndTagEvent;
      FOnContent            : TContentEvent;
      FOnCData              : TContentEvent;
      FOnElement            : TElementEvent;
      FOnAttList            : TElementEvent;
      FOnEntity             : TEntityEvent;
      FOnNotation           : TNotationEvent;
      FOnDtdError           : TErrorEvent;
      FOnLoadExternal       : TExternalEvent;
      FStopParser           : boolean;
      function  GetNormalize : boolean;
      procedure SetNormalize (Value : boolean);

      procedure WhenXmlProlog(XmlVersion, Encoding: string; Standalone : boolean); virtual;
      procedure WhenComment  (Comment : string);                                   virtual;
      procedure WhenPI       (Target, Content: string; Attributes : TAttrList);    virtual;
      procedure WhenDtdRead  (RootElementName : string);                           virtual;
      procedure WhenStartTag (TagName : string; Attributes : TAttrList);           virtual;
      procedure WhenEmptyTag (TagName : string; Attributes : TAttrList);           virtual;
      procedure WhenEndTag   (TagName : string);                                   virtual;
      procedure WhenContent  (Content : string);                                   virtual;
      procedure WhenCData    (Content : string);                                   virtual;
      procedure WhenElement  (ElemDef : TElemDef);                                 virtual;
      procedure WhenAttList  (ElemDef : TElemDef);                                 virtual;
      procedure WhenEntity   (EntityDef : TEntityDef);                             virtual;
      procedure WhenNotation (NotationDef : TNotationDef);                         virtual;
      procedure WhenDtdError (ErrorPos : PAnsiChar);                               virtual;

    public
      constructor Create (AOwner: TComponent); override;
      destructor Destroy;                      override;

      procedure LoadFromFile   (Filename : TFilename);   // Load XML Document from file
      procedure LoadFromBuffer (Buffer : PAnsiChar);     // Load XML Document from buffer
      procedure SetBuffer      (Buffer : PAnsiChar);     // Refer to Buffer
      function  GetFilename : TFilename;

      procedure Execute;                                 // Perform scanning

    protected
      property XmlParser            : TXmlParser        read FXmlParser;
      property StopParser           : boolean           read FStopParser           write FStopParser;
      property Filename             : TFilename         read GetFilename           write LoadFromFile;
      property Normalize            : boolean           read GetNormalize          write SetNormalize;
      property OnXmlProlog          : TXmlPrologEvent   read FOnXmlProlog          write FOnXmlProlog;
      property OnComment            : TCommentEvent     read FOnComment            write FOnComment;
      property OnPI                 : TPIEvent          read FOnPI                 write FOnPI;
      property OnDtdRead            : TDtdEvent         read FOnDtdRead            write FOnDtdRead;
      property OnStartTag           : TStartTagEvent    read FOnStartTag           write FOnStartTag;
      property OnEmptyTag           : TStartTagEvent    read FOnEmptyTag           write FOnEmptyTag;
      property OnEndTag             : TEndTagEvent      read FOnEndTag             write FOnEndTag;
      property OnContent            : TContentEvent     read FOnContent            write FOnContent;
      property OnCData              : TContentEvent     read FOnCData              write FOnCData;
      property OnElement            : TElementEvent     read FOnElement            write FOnElement;
      property OnAttList            : TElementEvent     read FOnAttList            write FOnAttList;
      property OnEntity             : TEntityEvent      read FOnEntity             write FOnEntity;
      property OnNotation           : TNotationEvent    read FOnNotation           write FOnNotation;
      property OnDtdError           : TErrorEvent       read FOnDtdError           write FOnDtdError;
      property OnLoadExternal       : TExternalEvent    read FOnLoadExternal       write FOnLoadExternal;
    end;

// ============================================================================================
// TXmlScanner: Event based XML Scanning
// ============================================================================================

type
  TXmlScanner = class (TCustomXmlScanner)
                public
                  property XmlParser;
                  property StopParser;
                published
                  property Filename;
                  property Normalize;
                  property OnXmlProlog;
                  property OnComment;
                  property OnPI;
                  property OnDtdRead;
                  property OnStartTag;
                  property OnEmptyTag;
                  property OnEndTag;
                  property OnContent;
                  property OnCData;
                  property OnElement;
                  property OnAttList;
                  property OnEntity;
                  property OnNotation;
                  property OnDtdError;
                  property OnLoadExternal;
                end;

  TEasyXmlScanner = class (TCustomXmlScanner)  // Leaves out events and properties which you are unlikely to use for "normal" XML files.
                    protected
                      procedure WhenCData (Content : string); override;
                    public
                      property XmlParser;
                      property StopParser;
                    published
                      property Filename;
                      property Normalize;
                      property OnComment;
                      property OnPI;
                      property OnStartTag;
                      property OnEmptyTag;
                      property OnEndTag;
                      property OnContent;             // CDATA sections also trigger "OnContent" events
                      property OnLoadExternal;
                    end;

procedure Register;

(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

implementation

type
  TCharset = set of AnsiChar;

procedure Register;
begin
  RegisterComponents ('XML', [TXmlScanner, TEasyXmlScanner]);
end;



(*
===============================================================================================
"Special" Helper Functions

Don't ask me why. But including these functions makes the parser *DRAMATICALLY* faster
on my K6-233 and Pentium-M machine. You can test it yourself just by commenting them out.
They do exactly the same as the Assembler routines defined in SysUtils.
(This is where you can see how great the Delphi compiler really is. The compiled code is
faster than hand-coded assembler! :-)
===============================================================================================
--> Just move this line below the StrScan function -->  *)

function StrPos (const Str, SearchStr : PAnsiChar) : PAnsiChar;
         // Same functionality as SysUtils.StrPos
var
  First : AnsiChar;
  Len   : integer;
begin
  First  := SearchStr^;
  Len    := StrLen (SearchStr);
  Result := Str;
  repeat
    if Result^ = First then
      if StrLComp (Result, SearchStr, Len) = 0 then break;
    if Result^ = #0 then
      exit (nil);
    inc (Result);
  until false;
end;


function StrScan (const Start : PAnsiChar; const Ch : AnsiChar) : PAnsiChar;
         // Same functionality as SysUtils.StrScan
begin
  Result := Start;
  while Result^ <> Ch do begin
    if Result^ = #0 then
      exit (nil);
    inc (Result);
    end;
end;


// ============================================================================================
// Unicode conversion functions
// The "string" type of Delphi uses UTF-16 as its encoding
// ============================================================================================

function EncodingToCodePage (const Encoding : string) : TCodePage;
var
  i : integer;
begin
  for i := 1 to CEncodingCount do
    if AnsiSameText (Encoding, CEncoding [i].Encoding) then
      exit (CEncoding [i].CodePage);
  Result := CP_UTF8;
end;


function  UnicodeCharToString (const U : longword) : string; overload;
begin
  if U < $10000
    then Result := chr (U)
    else Result := chr ($D800 or (((U - $00010000) shr 10) and $000003FF)) +   // Build surrogate pair
                   chr ($DC00 or (((U - $00010000)         and $000003FF)));
end;


function  UnicodeCharToString (const CharEntityReference : string) : string; overload;
          // CharEntityReference: '#n' (decimal) or '#xn' (hexadecimal)
var
  U : integer;
begin
  if Length (CharEntityReference) < 2 then exit ('');
  if CharEntityReference [2] = 'x'
    then U := StrToIntDef ('$' + Copy (string (CharEntityReference), 3, MaxInt), ord (CUnknownChar))
    else U := StrToIntDef (      Copy (string (CharEntityReference), 2, MaxInt), ord (CUnknownChar));
  if U < $10000
    then Result := chr (U)
    else Result := chr ($D800 or ((U - $10000) shr 10)) + chr ($DC00 or (U and $03FF));  // Build surrogate pair
end;


(*
===============================================================================================
Helper Functions
===============================================================================================
*)

function  DelChars (Source : string; CharsToDelete : TCharset) : string;
          // Delete all "CharsToDelete" from the string
var
  i : integer;
begin
  Result := Source;
  for i := Length (Result) downto 1 do
    if CharInSet (Result [i], CharsToDelete) then
      Delete (Result, I, 1);
end;


function  TrimWs (const Source : string) : string;
          // Trimms off Whitespace characters from both ends of the string
var
  IL, IR, L : integer;
begin
  L := Length (Source);
  IL := 1;
  while (IL <= L) and CharInSet (Source [IL], CWhitespace) do inc (IL);
  IR := Length (Source);
  while (IR > 0)  and CharInSet (Source [IR], CWhitespace) do dec (IR);
  Result := Copy (Source, IL, IR-IL+1);
end;

function TrimAndPackSpace (Source : string) : string;
          // Trim and pack contiguous space (#x20) characters
          // Needed for attribute value normalization of non-CDATA attributes (XMLSpec 3.3.3)
var
  I, T : INTEGER;
begin
  // --- Trim Left
  T := 1;
  for I := 1 to Length (Source) do
    if Source [I] = #32
      then inc (T)
      else break;
  if T > 1
    then Result := Copy (Source, T, MaxInt)
    else Result := Source;

  // --- Trim Right
  I := Length (Result);
  while (I > 1) and (Result [I] = #32) do
    dec (I);
  Delete (Result, I + 1, Length (Result)-I);

  // --- Pack
  for I := Length (Result) downto 2 do
    IF (Result [I] = #32) and (Result [I - 1] = #32) then
      Delete (Result, I, 1);
end;


function  ConvertWs (Source: string; PackWs: boolean) : string;
          // Converts all Whitespace characters to the Space #x20 character
          // If "PackWs" is true, contiguous Whitespace characters are packed to one
var
  i : integer;
begin
  Result := Source;
  for i := Length (Result) downto 1 do
    if CharInSet (Result [I], CWhitespace) then
      if PackWs and (I > 1) and CharInSet (Result [I - 1], CWhitespace)
        then Delete (Result, I, 1)
        else Result [I] := #32;
end;


function  BufToStr (const Start, Finish : PAnsiChar; const CodePage : TCodePage) : string;    overload;
var
  RBS : RawByteString;
  Len : integer;
begin
  Len := Finish - Start + 1;
  SetLength (RBS, Len);
  StrMove (PAnsiChar (RBS), Start, Len);
  SetCodePage (RBS, CodePage, false);
  Result := string (RBS);
end;


function  BufToStr  (const Start : PAnsiChar; const Len : integer; const CodePage : TCodePage) : string;  overload;
var
  RBS : RawByteString;
begin
  SetLength (RBS, Len);
  StrMove (PAnsiChar (RBS), Start, Len);
  SetCodePage (RBS, CodePage, false);
  Result := string (RBS);
end;


function  BufToStr (const Start : PAnsiChar; const CodePage : TCodePage) : string;    overload;
var
  RBS : RawByteString;
  Len : integer;
begin
  Len := StrEnd (Start) - Start;
  SetLength (RBS, Len);
  StrMove (PAnsiChar (RBS), Start, Len);
  SetCodePage (RBS, CodePage, false);
  Result := string (RBS);
end;


function  StrScanE (const Source : PAnsiChar; const CharToScanFor : AnsiChar) : PAnsiChar;
          // If "CharToScanFor" is not found, StrScanE returns the last char of the
          // buffer instead of NIL
begin
  Result := StrScan (Source, CharToScanFor);
  if Result = nil then
    Result := StrEnd (Source)-1;
end;


procedure ExtractName (Start : PAnsiChar; Terminators : TCharset; out Final : PAnsiChar);
          (* Extracts the complete Name beginning at "Start".
             It is assumed that the name is contained in Markup, so the '>' character is
             always a Termination.
             Start:       IN  Pointer to first char of name. Is always considered to be valid
             Terminators: IN  Characters which terminate the name
             Final:       OUT Pointer to last char of name *)
begin
  Final := Start;
  Include (Terminators, #0);
  Include (Terminators, '>');
  while not CharInset ((Final + 1)^, Terminators) do
    inc (Final);
end;


procedure ExtractQuote (Start : PAnsiChar; out Content : string; out Final : PAnsiChar; const CodePage : TCodePage);
          (* Extract a string which is contained in single or double Quotes.
             Start:    IN   Pointer to opening quote
             Content:  OUT  The quoted string
             Final:    OUT  Pointer to closing quote *)
begin
  Final := StrScan (Start+1, Start^);
  if Final = nil then begin
    Final := StrEnd (Start + 1) - 1;
    Content := BufToStr (Start, Final, CodePage);
    end
  else
    Content := BufToStr (Start + 1, Final - 1 - Start, CodePage);
end;


(*
===============================================================================================
TEntityStackNode
This Node is pushed to the "Entity Stack" whenever the parser parses entity replacement text.
The "Instance" field holds the Instance pointer of an External Entity buffer. When it is
popped, the Instance is freed.
The "Encoding" field holds the name of the Encoding. External Parsed Entities may have
another encoding as the document entity (XmlSpec 4.3.3). So when there is an "<?xml" PI
found in the stream (= Text Declaration at the beginning of external parsed entities), the
Encoding found there is used for the External Entity (is assigned to TXmlParser.CurEncoding)
Default Encoding is for the Document Entity is UTF-8. It is assumed that External Entities
have the same Encoding as the Document Entity, unless they carry a Text Declaration.
===============================================================================================
*)

TYPE
  TEntityStackNode = class
                       ExternalEntity : TXmlParser;
                       Encoding       : string;
                       CodePage       : TCodePage;
                       LastPos        : PAnsiChar;
                       ScanContent    : Utf8String;
                       destructor Destroy; override;
                     end;

destructor TEntityStackNode.Destroy;
begin
  if Assigned (ExternalEntity) then
    ExternalEntity.Free;
  inherited Destroy;
end;

(*
===============================================================================================
TEntityStack
For nesting of Entities.
When there is an entity reference found in the data stream, the corresponding entity
definition is searched and the current position is pushed to this stack.
From then on, the program scans the entitiy replacement text as if it were normal content.
When the parser reaches the end of an entity, the current position is popped off the
stack again.
===============================================================================================
*)

constructor TEntityStack.Create (TheOwner : TXmlParser);
begin
  inherited Create;
  Owner := TheOwner;
end;


procedure TEntityStack.Push (LastPos : PAnsiChar);
begin
  Push (nil, LastPos);
end;


procedure TEntityStack.Push (ExternalEntity : TXmlParser; LastPos : PAnsiChar);
var
  ESN : TEntityStackNode;
begin
  ESN := TEntityStackNode.Create;
  ESN.ExternalEntity := ExternalEntity;
  ESN.Encoding       := Owner.FCurEncoding;  // Save current Encoding
  ESN.CodePage       := Owner.FCurCodePage;
  ESN.LastPos        := LastPos;
  Add (ESN);
end;

procedure TEntityStack.Push (LastPos : PAnsiChar; const EntityReplacement : string; out NewPos : PAnsiChar);
var
  ESN : TEntityStackNode;
begin
  ESN := TEntityStackNode.Create;
  ESN.ExternalEntity := nil;
  ESN.Encoding       := Owner.FCurEncoding;  // Save current Encoding
  ESN.CodePage       := Owner.FCurCodePage;
  ESN.LastPos        := LastPos;
  ESN.ScanContent    := Utf8String (EntityReplacement);
  Owner.FCurEncoding := 'UTF-8';
  Owner.FCurCodePage := CP_UTF8;
  NewPos             := PAnsiChar (ESN.ScanContent);
  Add (ESN);
end;


function  TEntityStack.Pop : PAnsiChar;
var
  ESN : TEntityStackNode;
begin
  if Count > 0 then begin
    ESN := TEntityStackNode (Items [Count - 1]);
    Result := ESN.LastPos;
    if ESN.Encoding <> '' then begin
      Owner.FCurEncoding := ESN.Encoding;   // Restore current Encoding
      Owner.FCurCodePage := ESN.CodePage;
      end;
    Delete (Count - 1);
    end
  else
    Result := nil;
end;


(*
===============================================================================================
TExternalID
-----------
XmlSpec 4.2.2:  ExternalID ::= 'SYSTEM' S SystemLiteral |
                               'PUBLIC' S PubidLiteral S SystemLiteral
XmlSpec 4.7:    PublicID   ::= 'PUBLIC' S PubidLiteral
SystemLiteral and PubidLiteral are quoted
===============================================================================================
*)

type
  TExternalID = class
                  PublicId : string;
                  SystemId : string;
                  Final    : PAnsiChar;
                  constructor Create (Start : PAnsiChar; CodePage : TCodePage);
                end;

constructor TExternalID.Create (Start : PAnsiChar; CodePage : TCodePage);
begin
  inherited Create;
  Final := Start;
  if StrLComp (Start, 'SYSTEM', 6) = 0 then begin
    while not CharInSet (Final^, (CQuoteChar + [#0, '>', '['])) do inc (Final);
    if not CharInSet (Final^, CQuoteChar) then exit;
    ExtractQuote (Final, SystemID, Final, CodePage);
    end
  else if StrLComp (Start, 'PUBLIC', 6) = 0 then begin
    while not CharInSet (Final^, (CQuoteChar + [#0, '>', '['])) do inc (Final);
    if not CharInSet (Final^, CQuoteChar) then exit;
    ExtractQuote (Final, PublicID, Final, CodePage);
    inc (Final);
    while not CharInSet (Final^, (CQuoteChar + [#0, '>', '['])) do inc (Final);
    if not CharInSet (Final^, CQuoteChar) then exit;
    ExtractQuote (Final, SystemID, Final, CodePage);
    end;
end;


(*
===============================================================================================
TXmlParser
===============================================================================================
*)

constructor TXmlParser.Create;
begin
  inherited Create;
  FBuffer     := nil;
  FBufferSize := 0;
  Elements    := TElemList.Create;
  Entities    := TNvpList.Create;
  ParEntities := TNvpList.Create;
  Notations   := TNvpList.Create;
  CurAttr     := TAttrList.Create;
  EntityStack := TEntityStack.Create (Self);
  FIgnoreEncoding := false;
  Clear;
end;


destructor TXmlParser.Destroy;
begin
  Clear;
  Elements.Free;
  Entities.Free;
  ParEntities.Free;
  Notations.Free;
  CurAttr.Free;
  EntityStack.Free;
  inherited Destroy;
end;


procedure TXmlParser.Clear;
          // Free Buffer and clear all object attributes
begin
  if (FBufferSize > 0) and (FBuffer <> nil) then
    FreeMem (FBuffer);
  FBuffer         := nil;
  FBufferSize     := 0;
  FSource         := '';
  FXmlVersion     := '';
  FEncoding       := 'UTF-8';
  FCodePage       := CP_UTF8;
  FCurEncoding    := 'UTF-8';
  FCurCodePage    := CP_UTF8;
  FStandalone     := false;
  FRootName       := '';
  FDtdcFinal      := nil;
  FNormalize      := true;
  Elements.Clear;
  Entities.Clear;
  ParEntities.Clear;
  Notations.Clear;
  CurAttr.Clear;
  EntityStack.Clear;
end;


function  TXmlParser.LoadFromFile (Filename : string; FileMode : integer = fmOpenRead OR fmShareDenyNone) : boolean;
          // Loads Document from given file
          // Returns TRUE if successful
var
  f           : file;
  ReadIn      : integer;
  OldFileMode : integer;
begin
  Clear;
  if not FileExists (Filename) then
    exit (false);

  // --- Open File
  OldFileMode := System.FileMode;
  try
    System.FileMode := FileMode;
    try
      AssignFile (f, Filename);
      Reset (f, 1);
    except
      exit (false);
      end;

    try
      // --- Allocate Memory
      try
        FBufferSize := Filesize (f) + 1;
        GetMem (FBuffer, FBufferSize);
      except
        Clear;
        exit (false);
        end;

      // --- Read File
      try
        BlockRead (f, FBuffer^, FBufferSize, ReadIn);
        (FBuffer + ReadIn)^ := #0;  // NULL termination
      except
        Clear;
        exit (false);
        end;
    finally
      CloseFile (f);
      end;

    FSource := Filename;
    Result  := true;
  finally
    System.FileMode := OldFileMode;
    end;
end;


function  TXmlParser.LoadFromBuffer (Buffer : PAnsiChar; Size : integer) : boolean;
          // Loads Document from another buffer.
          // Returns TRUE if successful
          // If "Size" is < 0, Buffer is assumed to be null terminated
          // The "Source" property becomes '<MEM>' if successful
var
  Terminator : PAnsiChar;
begin
  Clear;
  if Size < 0
    then FBufferSize := StrLen (Buffer) + 1
    else FBufferSize := Size + 1;
  try
    GetMem (FBuffer, FBufferSize);
  except
    Clear;
    exit (false);
    end;
  if Size < 0 then
    StrCopy (FBuffer, Buffer)
  else begin
    Move (Buffer^, FBuffer^, Size);
    Terminator  := FBuffer + Size;
    Terminator^ := #0;
    end;
  FSource := '<MEM>';
  Result  := true;
end;


function  TXmlParser.LoadFromStream  (Stream : TStream) : boolean;
          // Loads Document from stream.
          // Returns TRUE if successful
          // The "Source" property becomes '<MEM>' if successful
var
  Terminator : PAnsiChar;
begin
  Clear;
  if Stream.Size > MaxInt then begin
    Result := false;
    exit;
    end;

  FBufferSize := Stream.Size + 1;              // +1 for null termination
  try
    GetMem (FBuffer, FBufferSize);
    Stream.Seek (0, soBeginning);
    Stream.Read (FBuffer^, FBufferSize - 1);
    Terminator  := FBuffer + FBufferSize - 1;  // Last byte in the buffer
    Terminator^ := #0;                         // Set to zero
  except
    Clear;
    Result := false;
    exit;
    end;
  FSource := '<MEM>';
  Result  := true;
end;


function  TXmlParser.LoadFromString (const XmlString : string) : boolean;
          // Loads Document from UnicodeString (UTF-16!)
          // Converts the document encoding to UTF-8
          // Leaves the XML itself unchanged, so the XML Prolog will not be changed to
          // say 'encoding="UTF-8"'.

  function Utf8Length (S : string) : integer;
  var
    Ch : char;
  begin
    Result := 0;
    for Ch in S do
      case Ch of
        #$0000..#$007F : inc (Result);      // 0xxxxxxx
        #$0080..#$07FF : inc (Result, 2);   // 110xxxxx 10xxxxxx
        #$0800..#$D7FF,
        #$E000..#$FFFF : inc (Result, 3);   // 1110xxxx 10xxxxxx 10xxxxxx
        #$D800..#$DFFF : inc (Result, 2);   // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  - Surrogate Pair Area
                               //     ^-- 2 for the first + 2 for the second word = 4 for the character
        end;
  end;

begin
  Clear;
  PresetEncoding ('UTF-8');   // The file will be UTF-8, independent of the encoding specified in the XML Prolog
  FBufferSize := Utf8Length (XmlString) + 1;
  try
    GetMem (FBuffer, FBufferSize);
    FBufferSize := UnicodeToUtf8 (FBuffer, FBufferSize, PWideChar (XmlString), Length (XmlString));
  except
    Clear;
    exit (false);
    end;
  FSource := '<STR>';
  Result  := true;
end;


function  TXmlParser.LoadFromStrings (Xml : TStrings) : boolean;
          // Loads Document from TStrings buffer
begin
  Result := LoadFromString (Xml.Text);
end;


procedure TXmlParser.SetBuffer (Buffer : PAnsiChar);      // References another buffer
begin
  Clear;
  FBuffer     := Buffer;
  FBufferSize := 0;
  FSource := '<REFERENCE>';
end;


procedure TXmlParser.PresetEncoding (Encoding: string);
          // Can be called prior to loading the XML. If Encoding is a non-empty string,
          // it will be set as the encoding and the encoding in the XML Prolog will be ignored.
          // Can be used for cases when the XML has already been re-encoded and the
          // XML Prolog is unchanged.
begin
  if Encoding = '' then begin
    FEncoding       := 'UTF-8';
    FCodepage       := CP_UTF8;
    FIgnoreEncoding := false;
    end
  else begin
    FEncoding       := AnsiUpperCase (Encoding);
    FCodepage       := EncodingToCodePage (Encoding);
    FIgnoreEncoding := true;
    end;
end;

//-----------------------------------------------------------------------------------------------
// Scanning through the document
//-----------------------------------------------------------------------------------------------

procedure TXmlParser.StartScan;
begin
  CurPartType := ptNone;
  CurName     := '';
  CurContent  := '';
  CurStart    := NIL;
  CurFinal    := NIL;
  CurAttr.Clear;
  EntityStack.Clear;
end;


function  TXmlParser.Scan : boolean;
          // Scans the next Part
          // Returns TRUE if a part could be found, FALSE if there is no part any more
          //
          // "IsDone" can be set to FALSE by AnalyzeText in order to go to the next part
          // if there is no Content due to normalization
var
  IsDone : boolean;
begin
  repeat
    IsDone := true;

    // --- Start of next Part
    if CurStart = nil then begin
      CurStart := DocBuffer;
      if StrLComp (CurStart, CUtf8BOM, 3) = 0 then begin
        inc (CurStart, 3);                                // Skip UTF-8 BOM
        FEncoding := 'UTF-8';
        FCodePage := CP_UTF8;
        end;
      while (CurStart^ <> '<') and (CurStart^ <> #0) do   // Scan to first opening angle bracket (<)
        inc (CurStart);
      end
    else
      CurStart := CurFinal + 1;
    CurFinal := CurStart;

    // --- End of Document or Pop off a new part from the Entity stack?
    if CurStart^ = #0 then
      CurStart := EntityStack.Pop;

    // --- No Document or End Of Document: Terminate Scan
    if (CurStart = nil) or (CurStart^ = #0) then begin
      CurStart := StrEnd (DocBuffer);
      CurFinal := CurStart - 1;
      EntityStack.Clear;
      exit (false);
      end;

    if (StrLComp (CurStart, '<?xml', 5) = 0) and
       CharInSet ((CurStart + 5)^, CWhitespace) then AnalyzeProlog                            // XML Declaration, Text Declaration
    else if StrLComp (CurStart, '<?',        2) = 0 then AnalyzePI (CurStart, CurFinal)       // PI
    else if StrLComp (CurStart, '<!--',      4) = 0 then AnalyzeComment (CurStart, CurFinal)  // Comment
    else if StrLComp (CurStart, '<!DOCTYPE', 9) = 0 then AnalyzeDtdc                          // DTDc
    else if StrLComp (CurStart, CDStart, Length (CDStart)) = 0 then AnalyzeCdata              // CDATA Section
    else if StrLComp (CurStart, '<',         1) = 0 then AnalyzeTag                           // Start-Tag, end-Tag, Empty-Element-Tag
    else AnalyzeText (IsDone);                                                                // Text Content
  until IsDone;
  Result := true;
end;


procedure TXmlParser.AnalyzeProlog;
          // Analyze XML Prolog or Text Declaration
var
  F : PAnsiChar;
begin
  CurAttr.Analyze (CurStart + 5, F, FCurCodePage);
  if EntityStack.Count = 0 then begin
    FXmlVersion := CurAttr.Value ('version');
    if not FIgnoreEncoding then begin
      FEncoding   := AnsiUpperCase (CurAttr.Value ('encoding'));
      FCodePage   := EncodingToCodePage (FEncoding);
      end;
    FStandalone := CurAttr.Value ('standalone') = 'yes';
    end;
  CurFinal := StrPos (F, '?>');
  if CurFinal <> nil
    then inc (CurFinal)
    else CurFinal := StrEnd (CurStart) - 1;
  if EntityStack.Count = 0 then begin
    FCurEncoding := FEncoding;
    FCurCodePage := FCodePage;
    end
  else begin
    FCurEncoding := AnsiUpperCase (CurAttr.Value ('encoding'));
    if FCurEncoding = '' then
      FCurEncoding := 'UTF-8';   // Default XML Encoding is UTF-8
    FCurCodePage := EncodingToCodePage (FCurEncoding);
    end;
  CurPartType  := ptXmlProlog;
  CurName      := '';
  CurContent   := '';
end;


procedure TXmlParser.AnalyzeComment (Start : PAnsiChar; var Final : PAnsiChar);
          // Analyze Comments
begin
  Final := StrPos (Start + 4, '-->');
  if Final = nil
    then Final := StrEnd (Start)-1
    else inc (Final, 2);
  CurPartType := ptComment;
end;


procedure TXmlParser.AnalyzePI (Start : PAnsiChar; var Final : PAnsiChar);
          // Analyze Processing Instructions (PI)
var
  F : PAnsiChar;
begin
  CurPartType := ptPI;
  Final := StrPos (Start+2, '?>');
  if Final = nil
    then Final := StrEnd (Start)-1
    else inc (Final);
  ExtractName (Start + 2, CWhitespace + ['?', '>'], F);
  CurName := BufToStr (Start+2, F, FCurCodePage);
  CurContent := BufToStr (F+1, Final-2, FCurCodePage);
  CurAttr.Analyze (F + 1, F, FCurCodePage);
end;


procedure TXmlParser.AnalyzeDtdc;
          (* Analyze Document Type Declaration
                 doctypedecl  ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
                 markupdecl   ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
                 PEReference  ::= '%' Name ';'

                 elementdecl  ::= '<!ELEMENT' S Name S contentspec S?                    '>'
                 AttlistDecl  ::= '<!ATTLIST' S Name AttDef* S?                          '>'
                 EntityDecl   ::= '<!ENTITY' S Name S EntityDef S?                       '>' |
                                  '<!ENTITY' S '%' S Name S PEDef S?                     '>'
                 NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S?      '>'
                 PI           ::=  '<?' PITarget (S (Char* - (Char* '?>' Char* )))?     '?>'
                 Comment      ::= '<!--' ((Char - '-') | ('-' (Char - '-')))*          '-->'  *)
type
  TPhase = (phName, phDtd, phInternal, phFinishing);
var
  Phase       : TPhase;
  F           : PAnsiChar;
  ExternalID  : TExternalID;
  ExternalDTD : TXmlParser;
  DER         : TDtdElementRec;
begin
  DER.Start := CurStart;
  EntityStack.Clear;    // Clear stack for Parameter Entities
  CurPartType := ptDtdc;

  // --- Don't read DTDc twice
  IF FDtdcFinal <> NIL THEN begin
    CurFinal := FDtdcFinal;
    EXIT;
    end;

  // --- Scan DTDc
  CurFinal := CurStart + 9;    // First char after '<!DOCTYPE'
  Phase    := phName;
  REPEAT
    case CurFinal^ OF
      '%' : begin
              PushPE (CurFinal);
              continue;
            end;
      #0  : if EntityStack.Count = 0 THEN
              break
            else begin
              CurFinal := EntityStack.Pop;
              continue;
              end;
      '[' : begin
              Phase := phInternal;
              AnalyzeDtdElements (CurFinal+1, CurFinal);
              continue;
            end;
      ']' : Phase := phFinishing;
      '>' : break;
      else  if not CharInSet (CurFinal^, CWhitespace) then begin
              case Phase of
                phName : if CharInSet (CurFinal^, CNameStart) then begin
                           ExtractName (CurFinal, CWhitespace + ['[', '>'], F);
                           FRootName := BufToStr (CurFinal, F, FCurCodePage);
                           CurFinal := F;
                           Phase := phDtd;
                           end;
                phDtd  : if (StrLComp (CurFinal, 'SYSTEM', 6) = 0) or
                            (StrLComp (CurFinal, 'PUBLIC', 6) = 0) then begin
                           ExternalID  := TExternalID.Create (CurFinal, FCurCodePage);
                           ExternalDTD := NIL;
                           TRY
                             ExternalDTD := LoadExternalEntity (ExternalId.SystemId, ExternalID.PublicId, '');
                             F := StrPos (ExternalDtd.DocBuffer, '<!');
                             if F <> nil then
                               AnalyzeDtdElements (F, F);
                             CurFinal := ExternalID.Final;
                           finally
                             ExternalDTD.Free;
                             ExternalID.Free;
                             end;
                           end;
                else     begin
                           DER.ElementType := deError;
                           DER.Pos         := CurFinal;
                           DER.Final       := CurFinal;
                           DtdElementFound (DER);
                         end;
                end;
              end;
      end;
    inc (CurFinal);
  until false;

  CurPartType := ptDtdc;
  CurName     := '';
  CurContent  := '';

  // -!- It is an error in the document if "EntityStack" is not empty now
  if EntityStack.Count > 0 then begin
    DER.ElementType := deError;
    DER.Final       := CurFinal;
    DER.Pos         := CurFinal;
    DtdElementFound (DER);
    end;

  EntityStack.Clear;    // Clear stack for General Entities
  FDtdcFinal := CurFinal;
end;


procedure TXmlParser.AnalyzeDtdElements (Start : PAnsiChar; out Final : PAnsiChar);
          // Analyze the "Elements" of a DTD contained in the external or
          // internal DTD subset.
var
  DER : TDtdElementRec;
begin
  Final := Start;
  repeat
    case Final^ of
      '%' : begin
              PushPE (Final);
              continue;
            end;
      #0  : if EntityStack.Count = 0 then
              break
            else begin
              CurFinal := EntityStack.Pop;
              continue;
              end;
      ']',
      '>' : break;
      '<' : if      StrLComp (Final, '<!ELEMENT',   9) = 0 then AnalyzeElementDecl  (Final, Final)
            else if StrLComp (Final, '<!ATTLIST',   9) = 0 then AnalyzeAttListDecl  (Final, Final)
            else if StrLComp (Final, '<!ENTITY',    8) = 0 then AnalyzeEntityDecl   (Final, Final)
            else if StrLComp (Final, '<!NOTATION', 10) = 0 then AnalyzeNotationDecl (Final, Final)
            else if StrLComp (Final, '<?',          2) = 0 then begin   // PI in DTD
              DER.ElementType := dePI;
              DER.Start       := Final;
              AnalyzePI (Final, Final);
              DER.Target      := PChar (CurName);
              DER.Content     := PChar (CurContent);
              DER.AttrList    := CurAttr;
              DER.Final       := Final;
              DtdElementFound (DER);
              end
            else if StrLComp (Final, '<!--', 4) = 0 then begin   // Comment in DTD
              DER.ElementType := deComment;
              DER.Start       := Final;
              AnalyzeComment  (Final, Final);
              DER.Final       := Final;
              DtdElementFound (DER);
              end
            else begin
              DER.ElementType := deError;
              DER.Start       := Final;
              DER.Pos         := Final;
              DER.Final       := Final;
              DtdElementFound (DER);
              end;
      end;
    inc (Final);
  until false;
end;


procedure TXmlParser.AnalyzeTag;
          // Analyze Start Tags, end Tags, and Empty-Element Tags

  procedure NormalizeAttrValue (Attr : TAttr);
            // According to XML 1.0 Specification, Third Edition, 2004-02-04, Chapter 3.3.3
            // This cannot be switched off because XMLSpec says it MUST be done

            (* XmlSpec 3.3.3:
            Before the value of an attribute is passed to the application, the XML processor
            MUST normalize the attribute value by applying the algorithm below.

            1. All line breaks MUST have been normalized on input to #xA
               (this parser doesn't do that !!!)
            2. begin with a normalized value consisting of the empty string.
            3. For each character, entity reference, or character reference in the unnormalized
               attribute value, beginning with the first and continuing to the last, do the following:
               - For a character reference, append the referenced character to the normalized value.
               - For an entity reference, recursively apply step 3 of this algorithm to the replacement text of the entity.
               - For a white space character (#x20, #xD, #xA, #x9), append a space character (#x20) to the normalized value.
               - For another character, append the character to the normalized value.

            If the attribute type is not CDATA, then the XML processor MUST further process the
            normalized attribute value by discarding any leading and trailing space (#x20)
            characters, and by replacing sequences of space (#x20) characters by a single
            space (#x20) character.
            Note that if the unnormalized attribute value contains a character reference to a
            white space character other than space (#x20), the normalized value contains the
            referenced character itself (#xD, #xA or #x9). This contrasts with the case where
            the unnormalized value contains a white space character (not a reference), which
            is replaced with a space character (#x20) in the normalized value and also contrasts
            with the case where the unnormalized value contains an entity reference whose
            replacement text contains a white space character; being recursively processed, the
            white space character is replaced with a space character (#x20) in the normalized
            value.
            All attributes for which no declaration has been read SHOULD be treated by a
            non-validating processor as if declared CDATA.
            *)

    function NormalizeAttrValStr (Str : string) : string;
              // Delivers a normalized and encoded representation of the attribute value in Str.
              // Will be called recursively for parsed general entities.
              // When IsEncoded is TRUE, the string in Str is already encoded in the target charset.
    var
      i              : integer;
      EntLen         : integer;
      PSemi          : PChar;
      Len            : integer;
      EntityDef      : TEntityDef;
      EntName        : string;
      Repl           : string;        // Replacement
      ExternalEntity : TXmlParser;
      EncLen         : integer;       // Length of untranscoded part
    begin
      Len    := Length (Str);
      SetLength (Result, Len);
      Result := '';
      EncLen := 0;
      i      := 1;
      while i <= Len do begin
        case Str [i] of
           #9,
          #10,
          #13 : begin
                  Result := Result + Copy (Str, i - EncLen, EncLen) + #32;
                  EncLen := 0;
                end;
          '&' : begin
                  PSemi := SysUtils.StrScan (PChar (Str) + i + 1, ';');
                  Repl  := '';
                  if PSemi <> nil then begin
                    EntLen  := PSemi - PChar (Str) - i;
                    EntName := Copy (Str, i + 1, EntLen);
                    if      EntName = 'lt'   then Repl := '<'
                    else if EntName = 'gt'   then Repl := '>'
                    else if EntName = 'amp'  then Repl := '&'
                    else if EntName = 'apos' then Repl := ''''
                    else if EntName = 'quot' then Repl := '"'
                    else if Copy (EntName, 1, 1) = '#' then Repl := UnicodeCharToString (EntName)   // Character Reference
                    else begin  // Resolve General Entity Reference
                      EntityDef := TEntityDef (Entities.Node (EntName));
                      if EntityDef = nil then                // Unknown Entity
                        Repl := EntName
                      else begin                             // Known Entity
                        if EntityDef.Value <> '' then        // Internal Entity
                          Repl := NormalizeAttrValStr (EntityDef.Value)
                        else begin                           // External Entity
                          ExternalEntity := NIL;
                          TRY
                            ExternalEntity := LoadExternalEntity (EntityDef.SystemId, EntityDef.PublicId, EntityDef.NotationName);
                            ExternalEntity.Normalize := Self.Normalize;
                            Repl           := '';            // External Entity can have a different encoding than parent entity
                            ExternalEntity.StartScan;
                            while ExternalEntity.Scan do
                              if (ExternalEntity.CurPartType = ptContent) or (ExternalEntity.CurPartType = ptCData) then
                                Repl := Repl + ExternalEntity.CurContent;
                            Repl := NormalizeAttrValStr (Repl);      // Recursively resolve Entity References
                          finally
                            ExternalEntity.Free;
                            end;
                          end;
                        end
                      end;
                    end
                  else
                    EntLen := 0;
                  Result := Result + Copy (Str, i - EncLen, EncLen) + Repl;
                  EncLen := 0;
                  i := i + EntLen + 1;
                end;
          else inc (EncLen);
          end;
        inc (i);
        end;
      Result := Result + Copy (Str, Len - EncLen + 1, EncLen);
    end;

  begin
    if (Attr.AttrType in [atCData, atUnknown])
      then Attr.Value := NormalizeAttrValStr (Attr.Value)
      else Attr.Value := TrimAndPackSpace (NormalizeAttrValStr (Attr.Value));
  end;

var
  S, F    : PAnsiChar;
  Attr    : TAttr;
  ElemDef : TElemDef;
  AttrDef : TAttrDef;
  I       : integer;
begin
  CurPartType := ptStartTag;
  S := CurStart + 1;
  if S^ = '/' then begin
    CurPartType := ptEndTag;
    inc (S);
    end;
  ExtractName (S, CWhitespace + ['/'], F);
  CurName := BufToStr (S, F, FCurCodePage);
  CurAttr.Analyze (F + 1, CurFinal, FCurCodePage);
  if CurFinal^ = '/' then
    CurPartType := ptEmptyTag;
  CurFinal := StrScanE (CurFinal, '>');

  // --- Set Default Attribute values for nonexistent attributes
  if (CurPartType = ptStartTag) or (CurPartType = ptEmptyTag) then begin
    ElemDef := Elements.Node (CurName);
    if ElemDef <> nil then begin
      for I := 0 to ElemDef.Count - 1 do begin
        AttrDef := TAttrDef (ElemDef [I]);
        Attr := TAttr (CurAttr.Node (AttrDef.Name));
        if (Attr = nil) and (AttrDef.Value <> '') then begin
          Attr           := TAttr.Create (AttrDef.Name, AttrDef.Value);
          Attr.ValueType := vtDefault;
          CurAttr.Add (Attr);
          end;
        if Attr <> nil then begin
          case AttrDef.DefaultType OF
            adDefault  : ;
            adRequired : ; // -!- It is an error in the document if "Attr.Value" is an empty string
            adImplied  : Attr.ValueType := vtImplied;
            adFixed    : begin
                           Attr.ValueType := vtFixed;
                           Attr.Value     := AttrDef.Value;
                         end;
            end;
          Attr.AttrType := AttrDef.AttrType;
          end;
        end;
      end;

    // --- Normalize Attribute Values
    for I := 0 to CurAttr.Count - 1 do
      NormalizeAttrValue (TAttr (CurAttr [I]));
    end;
end;


procedure TXmlParser.AnalyzeCData;
          // Analyze CDATA Sections
begin
  CurPartType := ptCData;
  CurFinal := StrPos (CurStart, CDEnd);
  if CurFinal = nil then begin
    CurFinal   := StrEnd (CurStart) - 1;
    CurContent := BufToStr (CurStart, CurFinal, FCurCodePage);
    end
  else begin
    CurContent := BufToStr (CurStart+Length (CDStart), CurFinal - 1, FCurCodePage);
    inc (CurFinal, Length (CDEnd) - 1);
    end;
end;


procedure TXmlParser.AnalyzeText (var IsDone : boolean);
          (* Analyzes Text Content between Tags. CurFinal will point to the last content character.
             Content ends at a '<' character or at the end of the document.
             Entity References and Character References are resolved.
             If Normalize is TRUE, contiguous Whitespace Characters will be compressed to
             one Space #x20 character, Whitespace at the beginning and end of content will
             be trimmed off and content which is or becomes empty is not returned to
             the application (in this case, "IsDone" is set to FALSE which causes the
             Scan method to proceed directly to the next part. *)

  procedure ProcessEntity;
            (* Is called if there is an ampsersand '&' character found in the document.
               IN  "CurFinal" points to the ampersand
               OUT "CurFinal" points to the first character after the semi-colon ';' *)
  var
    P              : PAnsiChar;
    Name           : string;
    EntityDef      : TEntityDef;
    ExternalEntity : TXmlParser;
  begin
    P := StrScan (CurFinal, ';');     // P points to the semi-colon that terminates the entity reference
    if P <> nil then begin
      Name := BufToStr (CurFinal + 1, P - 1, FCurCodePage);

      // Is it a Character Reference?
      if (CurFinal + 1)^ = '#' then begin
        CurContent := CurContent + UnicodeCharToString (Name);
        CurFinal   := P + 1;
        exit;
        end

      // Is it a Predefined Entity?
      else if Name = 'lt'   then begin CurContent := CurContent + '<';  CurFinal := P + 1; exit; end
      else if Name = 'gt'   then begin CurContent := CurContent + '>';  CurFinal := P + 1; exit; end
      else if Name = 'amp'  then begin CurContent := CurContent + '&';  CurFinal := P + 1; exit; end
      else if Name = 'apos' then begin CurContent := CurContent + ''''; CurFinal := P + 1; exit; end
      else if Name = 'quot' then begin CurContent := CurContent + '"';  CurFinal := P + 1; exit; end;

      // Replace with Entity from DTD
      EntityDef := TEntityDef (Entities.Node (Name));
      if EntityDef <> nil then begin
        if EntityDef.Value <> '' then begin
          EntityStack.Push (P + 1, EntityDef.Value, CurFinal);
          end
        else begin
          ExternalEntity := LoadExternalEntity (EntityDef.SystemId, EntityDef.PublicId, EntityDef.NotationName);
          EntityStack.Push (ExternalEntity, P + 1);
          CurFinal := ExternalEntity.DocBuffer;
          end;
        end
      else begin
        CurContent := CurContent + Name;
        CurFinal   := P + 1;
        end;
      end
    else begin
      inc (CurFinal);
      end;
  end;

var
  C  : integer;
begin
  CurFinal    := CurStart;
  CurPartType := ptContent;
  CurContent  := '';
  C           := 0;
  repeat
    case CurFinal^ of
      '&' : begin
              CurContent := CurContent + BufToStr (CurFinal-C, C, FCurCodePage);
              C := 0;
              ProcessEntity;
              continue;
            end;
      #0  : begin
              if EntityStack.Count = 0 then
                break
              else begin
                CurContent := CurContent + BufToStr (CurFinal - C, C, FCurCodePage);
                C := 0;
                CurFinal := EntityStack.Pop;
                continue;
                end;
            end;
      '<' : break;
      else inc (C);
      end;
    inc (CurFinal);
  until false;
  CurContent := CurContent + BufToStr (CurFinal-C, C, FCurCodePage);
  dec (CurFinal);

  if FNormalize then begin
    CurContent := ConvertWs (TrimWs (CurContent), true);
    IsDone     := CurContent <> '';    // IsDone will only get FALSE if Normalize is TRUE
    end;
end;


procedure TXmlParser.AnalyzeElementDecl  (Start : PAnsiChar; out Final : PAnsiChar);
          (* Parse <!ELEMENT declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 3.2:
                 elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
                 contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
                 Mixed       ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'   |
                                 '(' S? '#PCDATA' S? ')'
                 children    ::= (choice | seq) ('?' | '*' | '+')?
                 choice      ::= '(' S? cp ( S? '|' S? cp )* S? ')'
                 cp          ::= (Name | choice | seq) ('?' | '*' | '+')?
                 seq         ::= '(' S? cp ( S? ',' S? cp )* S? ')'

             More simply:
                 contentspec ::= EMPTY
                                 ANY
                                 '(#PCDATA)'
                                 '(#PCDATA | A | B)*'
                                 '(A, B, C)'
                                 '(A | B | C)'
                                 '(A?, B*, C+),
                                 '(A, (B | C | D)* )'                       *)
var
  Element : TElemDef;
  Elem2   : TElemDef;
  F       : PAnsiChar;
  DER     : TDtdElementRec;
begin
  Element   := TElemDef.Create;
  Final     := Start + 9;
  DER.Start := Start;
  repeat
    if Final^ = '>' then break;
    if CharInSet (Final^, CNameStart) and (Element.Name = '') then begin
      ExtractName (Final, CWhitespace, F);
      Element.Name := BufToStr (Final, F, FCurCodePage);
      Final := F;
      F := StrScan (Final + 1, '>');
      if F = nil then begin
        Element.Definition := BufToStr (Final, StrEnd (Final), FCurCodePage);
        Final := StrEnd (Final);
        break;
        end
      else begin
        Element.Definition := BufToStr (Final + 1, F - 1, FCurCodePage);
        Final := F;
        break;
        end;
      end;
    inc (Final);
  until false;
  Element.Definition := DelChars (Element.Definition, CWhitespace);
  ReplaceParameterEntities (Element.Definition);
  if      Element.Definition = 'EMPTY' then Element.ElemType := etEmpty
  else if Element.Definition = 'ANY'   then Element.ElemType := etAny
  else if Copy (Element.Definition, 1, 8) = '(#PCDATA' then Element.ElemType := etMixed
  else if Copy (Element.Definition, 1, 1) = '('        then Element.ElemType := etChildren
  else Element.ElemType := etAny;

  Elem2 := Elements.Node (Element.Name);
  if Elem2 <> nil then
    Elements.Delete (Elements.IndexOf (Elem2));
  Elements.Add (Element);
  Final := StrScanE (Final, '>');
  DER.ElementType := deElement;
  DER.ElemDef  := Element;
  DER.Final    := Final;
  DtdElementFound (DER);
end;


procedure TXmlParser.AnalyzeAttListDecl  (Start : PAnsiChar; out Final : PAnsiChar);
          (* Parse <!ATTLIST declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 3.3:
                 AttlistDecl    ::= '<!ATTLIST' S Name AttDef* S? '>'
                 AttDef         ::= S Name S AttType S DefaultDecl
                 AttType        ::= StringType | TokenizedType | EnumeratedType
                 StringType     ::= 'CDATA'
                 TokenizedType  ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
                 EnumeratedType ::= NotationType | Enumeration
                 NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
                 Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
                 DefaultDecl    ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
                 AttValue       ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
            Examples:
                 <!ATTLIST address
                           A1 CDATA "Default"
                           A2 ID    #REQUIRED
                           A3 IDREF #IMPLIED
                           A4 IDREFS #IMPLIED
                           A5 ENTITY #FIXED "&at;&#252;"
                           A6 ENTITIES #REQUIRED
                           A7 NOTATION (WMF | DXF) "WMF"
                           A8 (A | B | C) #REQUIRED>                *)
TYPE
  TPhase = (phElementName, phName, phType, phNotationContent, phDefault);
var
  Phase       : TPhase;
  F           : PAnsiChar;
  ElementName : string;
  ElemDef     : TElemDef;
  AttrDef     : TAttrDef;
  AttrDef2    : TAttrDef;
  Strg        : string;
  DER         : TDtdElementRec;
begin
  Final     := Start + 9;   // The character after <!ATTLIST
  Phase     := phElementName;
  DER.Start := Start;
  AttrDef   := nil;
  ElemDef   := nil;
  repeat
    if not CharInSet (Final^, CWhitespace) then
      case Final^ of
        '%' : begin
                PushPE (Final);
                continue;
              end;
        #0  : if EntityStack.Count = 0 then
                break
              else begin
                Final := EntityStack.Pop;
                continue;
                end;
        '>' : break;
        else  case Phase of
                phElementName     : begin
                                      ExtractName (Final, CWhitespace + CQuoteChar + ['#'], F);
                                      ElementName := BufToStr (Final, F, FCurCodePage);
                                      Final := F;
                                      ElemDef := Elements.Node (ElementName);
                                      if ElemDef = nil then begin
                                        ElemDef := TElemDef.Create;
                                        ElemDef.Name       := ElementName;
                                        ElemDef.Definition := 'ANY';
                                        ElemDef.ElemType   := etAny;
                                        Elements.Add (ElemDef);
                                        end;
                                      Phase := phName;
                                    end;
                phName            : begin
                                      AttrDef := TAttrDef.Create;
                                      ExtractName (Final, CWhitespace + CQuoteChar + ['#'], F);
                                      AttrDef.Name := BufToStr (Final, F, FCurCodePage);
                                      Final := F;
                                      AttrDef2 := TAttrDef (ElemDef.Node (AttrDef.Name));
                                      if AttrDef2 <> nil then
                                        ElemDef.Delete (ElemDef.IndexOf (AttrDef2));
                                      ElemDef.Add (AttrDef);
                                      Phase := phType;
                                    end;
                phType            : begin
                                      if Final^ = '(' then begin
                                        F := StrScan (Final + 1, ')');
                                        if F <> nil
                                          then AttrDef.TypeDef := BufToStr (Final + 1, F - 1, FCurCodePage)
                                          else AttrDef.TypeDef := BufToStr (Final + 1, FCurCodePage);
                                        AttrDef.TypeDef := DelChars (AttrDef.TypeDef, CWhitespace);
                                        AttrDef.AttrType := atEnumeration;
                                        ReplaceParameterEntities (AttrDef.TypeDef);
                                        ReplaceCharacterEntities (AttrDef.TypeDef);
                                        Phase := phDefault;
                                        end
                                      else if StrLComp (Final, 'NOTATION', 8) = 0 then begin
                                        inc (Final, 8);
                                        AttrDef.AttrType := atNotation;
                                        Phase := phNotationContent;
                                        end
                                      else begin
                                        ExtractName (Final, CWhitespace+CQuoteChar+['#'], F);
                                        AttrDef.TypeDef := BufToStr (Final, F, FCurCodePage);
                                        if      AttrDef.TypeDef = 'CDATA'    then AttrDef.AttrType := atCData
                                        else if AttrDef.TypeDef = 'ID'       then AttrDef.AttrType := atId
                                        else if AttrDef.TypeDef = 'IDREF'    then AttrDef.AttrType := atIdRef
                                        else if AttrDef.TypeDef = 'IDREFS'   then AttrDef.AttrType := atIdRefs
                                        else if AttrDef.TypeDef = 'ENTITY'   then AttrDef.AttrType := atEntity
                                        else if AttrDef.TypeDef = 'ENTITIES' then AttrDef.AttrType := atEntities
                                        else if AttrDef.TypeDef = 'NMTOKEN'  then AttrDef.AttrType := atNmToken
                                        else if AttrDef.TypeDef = 'NMTOKENS' then AttrDef.AttrType := atNmTokens;
                                        Phase := phDefault;
                                        end
                                    end;
                phNotationContent : begin
                                      F := StrScan (Final, ')');
                                      if F <> nil then
                                        AttrDef.Notations := BufToStr (Final + 1, F - 1, CurCodePage)
                                      else begin
                                        AttrDef.Notations := BufToStr (Final + 1, CurCodePage);
                                        Final := StrEnd (Final);
                                        end;
                                      ReplaceParameterEntities (AttrDef.Notations);
                                      AttrDef.Notations := DelChars (AttrDef.Notations, CWhitespace);
                                      Phase := phDefault;
                                    end;
                phDefault :         begin
                                      if Final^ = '#' then begin
                                        ExtractName (Final, CWhiteSpace + CQuoteChar, F);
                                        Strg := BufToStr (Final, F, FCurCodePage);
                                        Final := F;
                                        ReplaceParameterEntities (Strg);
                                        if      Strg = '#REQUIRED' then begin AttrDef.DefaultType := adRequired; Phase := phName; end
                                        else if Strg = '#IMPLIED'  then begin AttrDef.DefaultType := adImplied;  Phase := phName; end
                                        else if Strg = '#FIXED'    then       AttrDef.DefaultType := adFixed;
                                        end
                                      else if CharInSet (Final^, CQuoteChar) then begin
                                        ExtractQuote (Final, AttrDef.Value, Final, FCurCodePage);
                                        ReplaceParameterEntities (AttrDef.Value);
                                        ReplaceCharacterEntities (AttrDef.Value);
                                        Phase := phName;
                                        end;
                                      if Phase = phName then begin
                                        AttrDef := nil;
                                        end;
                                    end;
                end;
        end;
    inc (Final);
  until false;

  Final := StrScan (Final, '>');

  DER.ElementType := deAttList;
  DER.ElemDef  := ElemDef;
  DER.Final    := Final;
  DtdElementFound (DER);
end;


procedure TXmlParser.AnalyzeEntityDecl   (Start : PAnsiChar; out Final : PAnsiChar);
          (* Parse <!ENTITY declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 4.2:
                 EntityDecl  ::= '<!ENTITY' S Name S EntityDef S? '>'   |
                                 '<!ENTITY' S '%' S Name S PEDef S? '>'
                 EntityDef   ::= EntityValue | (ExternalID NDataDecl?)
                 PEDef       ::= EntityValue | ExternalID
                 NDataDecl   ::= S 'NDATA' S Name
                 EntityValue ::= '"' ([^%&"] | PEReference | EntityRef | CharRef)* '"'   |
                                 "'" ([^%&'] | PEReference | EntityRef | CharRef)* "'"
                 PEReference ::= '%' Name ';'

             Examples
                 <!ENTITY test1 "Stefan Heymann">                   <!-- Internal, general, parsed              -->
                 <!ENTITY test2 SYSTEM "ent2.xml">                  <!-- External, general, parsed              -->
                 <!ENTITY test2 SYSTEM "ent3.gif" NDATA gif>        <!-- External, general, unparsed            -->
                 <!ENTITY % test3 "<!ELEMENT q ANY>">               <!-- Internal, parameter                    -->
                 <!ENTITY % test6 SYSTEM "ent6.xml">                <!-- External, parameter                    -->
                 <!ENTITY test4 "&test1; ist lieb">                 <!-- IGP, Replacement text <> literal value -->
                 <!ENTITY test5 "<p>Dies ist ein Test-Absatz</p>">  <!-- IGP, See XmlSpec 2.4                   -->
          *)
type
  TPhase = (phName, phContent, phNData, phNotationName, phFinalGT);
var
  Phase         : TPhase;
  IsParamEntity : boolean;
  F             : PAnsiChar;
  ExternalID    : TExternalID;
  EntityDef     : TEntityDef;
  EntityDef2    : TEntityDef;
  DER           : TDtdElementRec;
begin
  Final         := Start + 8;   // First char after <!ENTITY
  DER.Start     := Start;
  Phase         := phName;
  IsParamEntity := false;
  EntityDef     := TEntityDef.Create;
  repeat
    if not CharInSet (Final^, CWhitespace) then
      case Final^ of
        '%' : IsParamEntity := TRUE;
        '>' : BREAK;
        else  case Phase of
                phName         : if Final^ in CNameStart then begin
                                   ExtractName (Final, CWhitespace + CQuoteChar, F);
                                   EntityDef.Name := BufToStr (Final, F, FCurCodePage);
                                   Final := F;
                                   Phase := phContent;
                                   end;
                phContent      : if Final^ in CQuoteChar then begin
                                   ExtractQuote (Final, EntityDef.Value, Final, FCurCodePage);
                                   Phase := phFinalGT;
                                   end
                                 else if (StrLComp (Final, 'SYSTEM', 6) = 0) or
                                         (StrLComp (Final, 'PUBLIC', 6) = 0) then begin
                                   ExternalID := TExternalID.Create (Final, FCurCodePage);
                                   EntityDef.SystemId := ExternalID.SystemId;
                                   EntityDef.PublicId := ExternalID.PublicId;
                                   Final      := ExternalID.Final;
                                   Phase      := phNData;
                                   ExternalID.Free;
                                   end;
                phNData        : if StrLComp (Final, 'NDATA', 5) = 0 then begin
                                   inc (Final, 4);
                                   Phase := phNotationName;
                                   end;
                phNotationName : if Final^ in CNameStart then begin
                                   ExtractName (Final, CWhitespace + ['>'], F);
                                   EntityDef.NotationName := BufToStr (Final, F, FCurCodePage);
                                   Final := F;
                                   Phase := phFinalGT;
                                   end;
                phFinalGT      : ; // -!- There is an error in the document if this branch is called
                end;
        end;
    inc (Final);
  until false;
  if IsParamEntity then begin
    EntityDef2 := TEntityDef (ParEntities.Node (EntityDef.Name));
    if EntityDef2 <> NIL then
      ParEntities.Delete (ParEntities.IndexOf (EntityDef2));
    ParEntities.Add (EntityDef);
    ReplaceCharacterEntities (EntityDef.Value);
    end
  else begin
    EntityDef2 := TEntityDef (Entities.Node (EntityDef.Name));
    if EntityDef2 <> NIL then
      Entities.Delete (Entities.IndexOf (EntityDef2));
    Entities.Add (EntityDef);
    ReplaceParameterEntities (EntityDef.Value);  //  Create replacement texts (see XmlSpec 4.5)
    ReplaceCharacterEntities (EntityDef.Value);
    end;
  Final := StrScanE (Final, '>');

  DER.ElementType := deEntity;
  DER.EntityDef   := EntityDef;
  DER.Final       := Final;
  DtdElementFound (DER);
end;


procedure TXmlParser.AnalyzeNotationDecl (Start : PAnsiChar; out Final : PAnsiChar);
          // Parse <!NOTATION declaration starting at "Start"
          // Final must point to the terminating '>' character
          // XmlSpec 4.7: NotationDecl ::=  '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
type
  TPhase = (phName, phExtId, phEnd);
var
  ExternalID  : TExternalID;
  Phase       : TPhase;
  F           : PAnsiChar;
  NotationDef : TNotationDef;
  DER         : TDtdElementRec;
begin
  Final       := Start + 10;   // Character after <!NOTATION
  DER.Start   := Start;
  Phase       := phName;
  NotationDef := TNotationDef.Create;
  repeat
    if not CharInSet (Final^, CWhitespace) THEN
      case Final^ OF
        '>',
        #0   : break;
        else   case Phase of
                 phName  : begin
                             ExtractName (Final, CWhitespace + ['>'], F);
                             NotationDef.Name := BufToStr (Final, F, FCurCodePage);
                             Final := F;
                             Phase := phExtId;
                           end;
                 phExtId : begin
                             ExternalID := TExternalID.Create (Final, FCurCodePage);
                             NotationDef.Value    := ExternalID.SystemId;
                             NotationDef.PublicId := ExternalID.PublicId;
                             Final := ExternalId.Final;
                             ExternalId.Free;
                             Phase := phEnd;
                           end;
                 phEnd   : ;   // -!- There is an error in the document if this branch is called
                 end;
        end;
    inc (Final);
  until false;
  Notations.Add (NotationDef);
  Final := StrScanE (Final, '>');

  DER.ElementType := deNotation;
  DER.NotationDef := NotationDef;
  DER.Final       := Final;
  DtdElementFound (DER);
end;


procedure TXmlParser.PushPE (var Start : PAnsiChar);
          (* If there is a parameter entity reference found in the data stream,
             the current position will be pushed to the entity stack.
             Start:  IN  Pointer to the '%' character starting the PE reference
                     OUT Pointer to first character of PE replacement text *)
var
  P         : PAnsiChar;
  EntityDef : TEntityDef;
begin
  P := StrScan (Start, ';');
  if P <> nil then begin
    EntityDef := TEntityDef (ParEntities.Node (BufToStr (Start + 1, P - 1, FCurCodePage)));
    if EntityDef <> nil
      then EntityStack.Push (P + 1, EntityDef.Value, Start)
      else Start := P + 1;
    end;
end;


procedure TXmlParser.ReplaceCharacterEntities (var Str : string);
          // Replaces all Character References in the string
var
  Start  : integer;
  PAmp   : PChar;
  PSemi  : PChar;
  PosAmp : integer;
  Len    : integer;        // Length of complete Character Reference
  Repl   : string;         // Replacement Text
begin
  if Str = '' then exit;
  Start := 1;
  repeat
    PAmp := SysUtils.StrPos (PChar (Str) + Start - 1, '&#');
    if PAmp = nil then break;
    PSemi := SysUtils.StrScan (PAmp + 3, ';');
    if PSemi = nil then break;
    PosAmp := PAmp - PChar (Str) + 1;
    Len    := PSemi - PAmp + 1;
    if (PAmp + 2)^ = 'x'
      then Repl := UnicodeCharToString (StrToIntDef ('$' + Copy (Str, PosAmp + 3, Len - 4), 32))
      else Repl := UnicodeCharToString (StrToIntDef (      Copy (Str, PosAmp + 2, Len - 3), 32));
    Delete (Str, PosAmp, Len);
    Insert (Repl, Str, PosAmp);
    Start := PosAmp + Length (Repl);
  until false;
end;


procedure TXmlParser.ReplaceParameterEntities (var Str : string);
          // Recursively replaces all Parameter Entity References in the string
  procedure ReplaceEntities (var Str : string);
  var
    Start   : integer;
    PAmp    : PChar;
    PSemi   : PChar;
    PosAmp  : integer;
    Len     : integer;
    Entity  : TEntityDef;
    Repl    : string;        // Replacement
  begin
    if Str = '' then exit;
    Start := 1;
    repeat
      PAmp := SysUtils.StrPos (PChar (Str) + Start - 1, '%');
      if PAmp = nil then break;
      PSemi := SysUtils.StrScan (PAmp + 2, ';');
      if PSemi = nil then break;
      PosAmp := PAmp - PChar (Str) + 1;
      Len    := PSemi - PAmp + 1;
      Entity := TEntityDef (ParEntities.Node (Copy (Str, PosAmp + 1, Len - 2)));
      if Entity <> nil then begin
        Repl := Entity.Value;
        ReplaceEntities (Repl);    // Recursion
        end
      else
        Repl := Copy (Str, PosAmp, Len);
      Delete (Str, PosAmp, Len);
      Insert (Repl, Str, PosAmp);
      Start := PosAmp + Length (Repl);
    until false;
  end;
begin
  ReplaceEntities (Str);
end;


function  TXmlParser.LoadExternalEntity (SystemId, PublicId, Notation : string) : TXmlParser;
          // This will be called whenever there is a Parsed External Entity or
          // the DTD External Subset has to be loaded.
          // It must create a TXmlParser instance and load the desired Entity.
          // This instance of LoadExternalEntity assumes that "SystemId" is a valid
          // file name (relative to the Document source) and loads this file using
          // the LoadFromFile method.

  function HasAbsolutePath (Filename : string) : boolean;
  begin
    Result := ((Copy (Filename, 1, 2) = '\\') or (Copy (Filename, 2, 1) = ':'));
  end;

var
  Filename : string;
begin
  // --- Convert System ID to complete filename
  Filename := StringReplace (SystemId, '/', '\', [rfReplaceAll]);
  if Copy (FSource, 1, 1) <> '<' then
    if not HasAbsolutePath (Filename) then
      Filename := ExtractFilePath (FSource) + Filename;

  // --- Load the File
  Result := TXmlParser.Create;
  Result.LoadFromFile (Filename);
end;


procedure TXmlParser.DtdElementFound (DtdElementRec : TDtdElementRec);
          // This method is called for every element which is found in the DTD
          // declaration. The variant record TDtdElementRec is passed which
          // holds informations about the element.
          // You can override this function to handle DTD declarations.
          // Note that when you parse the same document instance a second time,
          // the DTD will not get parsed again.
begin
end;


function TXmlParser.GetDocBuffer: PAnsiChar;
         // Returns FBuffer or a pointer to a NUL char if Buffer is empty
begin
  if FBuffer = nil
    then Result := #0
    else Result := FBuffer;
end;


(*
===============================================================================================
TNvpNode
--------
Node base class for the TNvpList
===============================================================================================
*)

constructor TNvpNode.Create (TheName, TheValue : string);
begin
  inherited Create;
  Name  := TheName;
  Value := TheValue;
end;


(*
===============================================================================================
TNvpList
--------
A generic List of Name-Value Pairs, based on TObjectList
===============================================================================================
*)

procedure TNvpList.Add (Node : TNvpNode);
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    if Node.Name > TNvpNode (Items [i]).Name then begin
      Insert (i + 1, Node);
      exit;
      end;
  Insert (0, Node);
end;


function  TNvpList.Node (Name : string) : TNvpNode;
          // Binary search for Node
var
  L, H : integer;    // Low, High Limit
  T, C : integer;    // Test Index, Comparison result
  Last : integer;    // Last Test Index
begin
  if Count = 0 then begin
    Result := nil;
    exit;
    end;
  L    := 0;
  H    := Count;
  Last := -1;
  repeat
    T := (L + H) div 2;
    if T = Last then break;
    Result := TNvpNode (Items [T]);
    C := AnsiCompareStr (Result.Name, Name);
    if      C = 0 then exit
    else if C < 0 then L := T
    else               H := T;
    Last := T;
  until false;
  Result := nil;
end;


function  TNvpList.Node (Index : integer) : TNvpNode;
begin
  if (Index < 0) or (Index >= Count)
    then Result := nil
    else Result := TNvpNode (Items [Index]);
end;


function  TNvpList.Value (Name : string) : string;
var
  Nvp : TNvpNode;
begin
  Nvp := TNvpNode (Node (Name));
  if Nvp <> nil
    then Result := Nvp.Value
    else Result := '';
end;


function  TNvpList.Value (Index : integer) : string;
begin
  if (Index < 0) or (Index >= Count)
    then Result := ''
    else Result := TNvpNode (Items [Index]).Value;
end;


function  TNvpList.Name (Index : integer) : string;
begin
  if (Index < 0) or (Index >= Count)
    then Result := ''
    else Result := TNvpNode (Items [Index]).Name;
end;


(*
===============================================================================================
TAttrList
List of Attributes. The "Analyze" method extracts the Attributes from the given Buffer.
Is used for extraction of Attributes in Start-Tags, Empty-Element Tags and the "pseudo"
attributes in XML Prologs, Text Declarations and PIs.
===============================================================================================
*)

procedure TAttrList.Analyze (Start : PAnsiChar; var Final : PAnsiChar; CodePage : integer);
          // Analyze the Buffer for Attribute=Name pairs.
          // Terminates when there is a character which is not IN CNameStart
          // (e.g. '?>' or '>' or '/>')
type
  TPhase = (phName, phEq, phValue);
var
  Phase : TPhase;
  F     : PAnsiChar;
  Name  : string;
  Value : string;
  Attr  : TAttr;
begin
  Clear;
  Phase := phName;
  Final := Start;
  repeat
    if (Final^ = #0) or (Final^ = '>') then break;
    if not CharInSet (Final^, CWhitespace) then
      case Phase of
        phName  : begin
                    if not (Final^ IN CNameStart) then break;
                    ExtractName (Final, CWhitespace + ['=', '/'], F);
                    Name := BufToStr (Final, F, CodePage);
                    Final := F;
                    Phase := phEq;
                  end;
        phEq    : begin
                    if Final^ = '=' then
                      Phase := phValue
                  end;
        phValue : begin
                    if CharInSet (Final^, CQuoteChar) then begin
                      ExtractQuote (Final, Value, F, CodePage);
                      Attr := TAttr.Create;
                      Attr.Name      := Name;
                      Attr.Value     := Value;
                      Attr.ValueType := vtNormal;
                      Add (Attr);
                      Final := F;
                      Phase := phName;
                      end;
                  end;
        end;
    inc (Final);
  until false;
end;


(*
===============================================================================================
TElemList
List of TElemDef nodes.
===============================================================================================
*)

function  TElemList.Node (Name : string) : TElemDef;
          // Binary search for the Node with the given Name
var
  L, H : integer;    // Low, High Limit
  T, C : integer;    // Test Index, Comparison result
  Last : integer;    // Last Test Index
begin
  if Count = 0 then begin
    Result := nil;
    exit;
    end;

  L    := 0;
  H    := Count;
  Last := -1;
  repeat
    T := (L + H) div 2;
    if T = Last then break;
    Result := TElemDef (Items [T]);
    C := AnsiCompareStr (Result.Name, Name);
    if      C = 0 then exit
    else if C < 0 then L := T
    else               H := T;
    Last := T;
  until false;
  Result := nil;
end;


procedure TElemList.Add (Node : TElemDef);
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if Node.Name > TElemDef (Items [i]).Name then begin
      Insert (i + 1, Node);
      exit;
      end;
  Insert (0, Node);
end;


(*
===============================================================================================
TScannerXmlParser
A TXmlParser descendant for the TCustomXmlScanner component
===============================================================================================
*)

TYPE
  TScannerXmlParser = class (TXmlParser)
                       Scanner : TCustomXmlScanner;
                       constructor Create (TheScanner : TCustomXmlScanner);
                       function  LoadExternalEntity (SystemId, PublicId, Notation : string) : TXmlParser;  override;
                       procedure DtdElementFound (DtdElementRec : TDtdElementRec);                         override;
                      end;

constructor TScannerXmlParser.Create (TheScanner : TCustomXmlScanner);
begin
  inherited Create;
  Scanner := TheScanner;
end;

function  TScannerXmlParser.LoadExternalEntity (SystemId, PublicId, Notation : string) : TXmlParser;
begin
  if Assigned (Scanner.FOnLoadExternal)
    then Scanner.FOnLoadExternal (Scanner, SystemId, PublicId, Notation, Result)
    else Result := inherited LoadExternalEntity (SystemId, PublicId, Notation);
end;

procedure TScannerXmlParser.DtdElementFound (DtdElementRec : TDtdElementRec);
begin
  with DtdElementRec do
    case ElementType of
      deElement  : Scanner.WhenElement  (ElemDef);
      deAttList  : Scanner.WhenAttList  (ElemDef);
      deEntity   : Scanner.WhenEntity   (EntityDef);
      deNotation : Scanner.WhenNotation (NotationDef);
      dePI       : Scanner.WhenPI       (Target, Content, AttrList);
      deComment  : Scanner.WhenComment  (BufToStr (Start, Final, FCurCodePage));
      deError    : Scanner.WhenDtdError (Pos);
      end;
end;


(*
===============================================================================================
TCustomXmlScanner
===============================================================================================
*)

constructor TCustomXmlScanner.Create (AOwner: TComponent);
begin
  inherited;
  FXmlParser := TScannerXmlParser.Create (Self);
end;

destructor TCustomXmlScanner.Destroy;
begin
  FXmlParser.Free;
  inherited;
end;

procedure TCustomXmlScanner.LoadFromFile (Filename : TFilename);
          // Load XML Document from file
begin
  FXmlParser.LoadFromFile (Filename);
end;

procedure TCustomXmlScanner.LoadFromBuffer (Buffer : PAnsiChar);
          // Load XML Document from buffer
begin
  FXmlParser.LoadFromBuffer (Buffer);
end;

procedure TCustomXmlScanner.SetBuffer (Buffer : PAnsiChar);
          // Refer to Buffer
begin
  FXmlParser.SetBuffer (Buffer);
end;

function  TCustomXmlScanner.GetFilename : TFilename;
begin
  Result := FXmlParser.Source;
end;

function  TCustomXmlScanner.GetNormalize : boolean;
begin
  Result := FXmlParser.Normalize;
end;

procedure TCustomXmlScanner.SetNormalize (Value : boolean);
begin
  FXmlParser.Normalize := Value;
end;

procedure TCustomXmlScanner.WhenXmlProlog(XmlVersion, Encoding: string; Standalone : boolean);
          // Is called when the parser has parsed the <? xml ?> declaration of the prolog
begin
  if Assigned (FOnXmlProlog) then
    FOnXmlProlog (Self, XmlVersion, Encoding, Standalone);
end;

procedure TCustomXmlScanner.WhenComment  (Comment : string);
          // Is called when the parser has parsed a <!-- comment -->
begin
  if Assigned (FOnComment) then
    FOnComment (Self, Comment);
end;

procedure TCustomXmlScanner.WhenPI (Target, Content: string; Attributes : TAttrList);
          // Is called when the parser has parsed a <?processing instruction ?>
begin
  if Assigned (FOnPI) then
    FOnPI (Self, Target, Content, Attributes);
end;

procedure TCustomXmlScanner.WhenDtdRead (RootElementName : string);
          // Is called when the parser has completely parsed the DTD
begin
  if Assigned (FOnDtdRead) then
    FOnDtdRead (Self, RootElementName);
end;

procedure TCustomXmlScanner.WhenStartTag (TagName : string; Attributes : TAttrList);
          // Is called when the parser has parsed a start tag like <p>
begin
  if Assigned (FOnStartTag) then
    FOnStartTag (Self, TagName, Attributes);
end;

procedure TCustomXmlScanner.WhenEmptyTag (TagName : string; Attributes : TAttrList);
          // Is called when the parser has parsed an Empty Element Tag like <br/>
begin
  if Assigned (FOnEmptyTag) then
    FOnEmptyTag (Self, TagName, Attributes);
end;

procedure TCustomXmlScanner.WhenEndTag (TagName : string);
          // Is called when the parser has parsed an end Tag like </p>
begin
  if Assigned (FOnEndTag) then
    FOnEndTag (Self, TagName);
end;

procedure TCustomXmlScanner.WhenContent (Content : string);
          // Is called when the parser has parsed an element's text content
begin
  if Assigned (FOnContent) then
    FOnContent (Self, Content);
end;

procedure TCustomXmlScanner.WhenCData (Content : string);
          // Is called when the parser has parsed a CDATA section
begin
  if Assigned (FOnCData) then
    FOnCData (Self, Content);
end;

procedure TCustomXmlScanner.WhenElement (ElemDef : TElemDef);
          // Is called when the parser has parsed an <!ELEMENT> definition
          // inside the DTD
begin
  if Assigned (FOnElement) then
    FOnElement (Self, ElemDef);
end;

procedure TCustomXmlScanner.WhenAttList (ElemDef : TElemDef);
          // Is called when the parser has parsed an <!ATTLIST> definition
          // inside the DTD
begin
  if Assigned (FOnAttList) then
    FOnAttList (Self, ElemDef);
end;

procedure TCustomXmlScanner.WhenEntity   (EntityDef : TEntityDef);
          // Is called when the parser has parsed an <!ENTITY> definition
          // inside the DTD
begin
  if Assigned (FOnEntity) then
    FOnEntity (Self, EntityDef);
end;

procedure TCustomXmlScanner.WhenNotation (NotationDef : TNotationDef);
          // Is called when the parser has parsed a <!NOTATION> definition
          // inside the DTD
begin
  if Assigned (FOnNotation) then
    FOnNotation (Self, NotationDef);
end;

procedure TCustomXmlScanner.WhenDtdError (ErrorPos : PAnsiChar);
          // Is called when the parser has found an Error in the DTD
begin
  if Assigned (FOnDtdError) then
    FOnDtdError (Self, ErrorPos);
end;

procedure TCustomXmlScanner.Execute;
          // Perform scanning
          // Scanning is done synchronously, i.e. you can expect events to be triggered
          // in the order of the XML data stream. Execute will finish when the whole XML
          // document has been scanned or when the StopParser property has been set to TRUE.
begin
  FStopParser := false;
  FXmlParser.StartScan;
  while FXmlParser.Scan and (not FStopParser) do
    case FXmlParser.CurPartType of
      ptNone      : ;
      ptXmlProlog : WhenXmlProlog (FXmlParser.XmlVersion, FXmlParser.Encoding, FXmlParser.Standalone);
      ptComment   : WhenComment   (BufToStr (FXmlParser.CurStart, FXmlParser.CurFinal, FXmlParser.CurCodePage));
      ptPI        : WhenPI        (FXmlParser.CurName, FXmlParser.CurContent, FXmlParser.CurAttr);
      ptDtdc      : WhenDtdRead   (FXmlParser.RootName);
      ptStartTag  : WhenStartTag  (FXmlParser.CurName, FXmlParser.CurAttr);
      ptEmptyTag  : WhenEmptyTag  (FXmlParser.CurName, FXmlParser.CurAttr);
      ptEndTag    : WhenEndTag    (FXmlParser.CurName);
      ptContent   : WhenContent   (FXmlParser.CurContent);
      ptCData     : WhenCData     (FXmlParser.CurContent);
      end;
end;

// ============================================================================================
// TEasyXmlScanner
// ============================================================================================

procedure TEasyXmlScanner.WhenCData (Content : string);
begin
  inherited WhenContent (Content);
end;


end.

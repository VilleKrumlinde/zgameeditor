(*======================================================================*
 | unitPEFile                                                           |
 |                                                                      |
 | Windows PE File Decoder unit                                         |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ----------------------------------------- |
 | 1.0      19/10/2000  CPWW  Original                                  |
 | 1.1      31/05/2001  CPWW  Fixed crash when no resource section.     |
 |                      CPWW  Fixed crash when 'VirtualSize' contains   |
 |                            0, and SizeOfRawData doesn't.             |
 *======================================================================*)

// A PE file looks like this...
//
//   [ DOS Header      ]     First word is 'MK'
//   [ COFF header     ]     Starts at DOSHdr._lfaNew.  First dword is COFF signature
//   [ Optional header ]     Follows COFF header.  First word is IMAGE_NT_OPTIONAL_HDR_MAGIC
//     [ Data Directory  ]   Really part of the optional header

//   [ Image Sections Headers ] Starts at optionalHeader + COFFHdr.SizeOfOptionalHeader

//   [ Section data]         Each one pointed to by it's Image Section Header

unit unitPEFile;

interface

uses Windows, Classes, SysUtils, ConTnrs, unitResourceDetails, Winapi.ImageHlp;

type

{$POINTERMATH ON}

TPEModule = class;

//----------------------------------------------------------------------
// TImageSection class

TImageSection = class
private
  fParent: TPEModule;
  fSectionHeader : TImageSectionHeader;
  fRawData : TMemoryStream;
  fUninitializedDataSize : Integer;

  function GetSectionName: ansistring;
public
  constructor Create (AParent : TPEModule; const AHeader : TImageSectionHeader; rawData : pointer);
  destructor Destroy; override;
  property Parent : TPEModule read fParent;

  property SectionName : ansistring read GetSectionName;
  property SectionHeader : TImageSectionHeader read fSectionHeader;
  property RawData : TMemoryStream read fRawData;
end;

TImageImportDirectory = packed record
  Characteristics : DWORD; // This is an RVA to a list of pointers. Each of these points to there function name
  TimeDateStamp : DWORD;   // The time/date stamp indicating when the file was built
  ForwarderChain : DWORD;  // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
  Name : DWORD;            // This is an RVA to a NULL-terminated ASCII ansistring containing the imported DLL's name
  FirstThunk : DWORD;      //  Another RVA to a list pointers. Each of these points to their function name
end;
PImageImportDirectory = ^TImageImportDirectory;

//----------------------------------------------------------------------
// TPEModule class

PImageOptionalHeader = ^TImageOptionalHeader;


TPEModule = class (TResourceModule)
private
  fDOSHeader : TImageDosHeader;
  fCOFFHeader : TImageFileHeader;
  fOptionalHeader : PImageOptionalHeader;
  fSectionList : TObjectList;                   // List of TImageSection objects
  fDOSStub : TMemoryStream;
  fCommentBlock : PAnsiChar;
  fCommentSize : Integer;
  fEndComment : PAnsiChar;
  fEndCommentSize : Integer;

  function GetOptionalHeader: TImageOptionalHeader;
  function GetImageSection(index: Integer): TImageSection;
  function GetImageSectionCount: Integer;
  function GetDataDictionary(index: Integer): PImageDataDirectory;
  function GetDataDictionaryCount: Integer;
  function GetDOSHeader: TImageDosHeader;
  function GetCOFFHeader: TImageFileHeader;
  function GetExportCount: Integer;
  function GetImportCount: Integer;
  function GetResourceSection (var offset : Integer) : TImageSection;
  function GetImportSection (var offset : Integer): TImageSection;
  function GetExportSection (var offset : Integer): TImageSection;
  function GetImport(idx: Integer): PImageImportDirectory;
  function GetImportSectionData: PAnsiChar;
  function GetExportSectionData: PAnsiChar;

protected
  procedure Decode (memory : pointer; exeSize : Integer); virtual;
  procedure Encode; virtual;
  property OptionalHeaderPtr : PImageOptionalHeader read fOptionalHeader;
  function FindDictionaryEntrySection (entryNo : Integer; var offset : Integer): Integer;

public
  constructor Create;
  destructor Destroy; override;

  property DOSHeader : TImageDosHeader read GetDOSHeader;
  property COFFHeader : TImageFileHeader read GetCOFFHeader;
  property OptionalHeader : TImageOptionalHeader read GetOptionalHeader;

  property ImageSectionCount : Integer read GetImageSectionCount;
  property ImageSection [index : Integer] : TImageSection read GetImageSection;

  property DataDictionaryCount : Integer read GetDataDictionaryCount;
  property DataDictionary [index : Integer] : PImageDataDirectory read GetDataDictionary;

  property ImportCount : Integer read GetImportCount;
  property Import [idx : Integer] : PImageImportDirectory read GetImport;
  property ImportSectionData : PAnsiChar read GetImportSectionData;
  property ExportSectionData : PAnsiChar read GetExportSectionData;
  property ExportCount : Integer read GetExportCount;

  procedure GetExportDetails (idx : Integer; var name : ansistring; var ordinal : DWORD);


  procedure LoadFromStream (s : TStream); override;
  procedure LoadFromFile (const name : ansistring); override;

  procedure SaveToStream (s : TStream); override;
//  procedure SaveToFile (const name : ansistring); override;
end;

//----------------------------------------------------------------------
// TResourceDirectoryTable record

TResourceDirectoryTable = packed record
  characteristics : DWORD; // Resource flags, reserved for future use; currently set to zero.
  timeDateStamp : DWORD;   // Time the resource data was created by the resource compiler.
  versionMajor : WORD;     // Major version number, set by the user.
  versionMinor : WORD;     // Minor version number.
  cNameEntries : WORD;     // Number of directory entries, immediately following the table, that use strings to identify Type, Name, or Language (depending on the level of the table).
  cIDEntries : WORD;       // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
end;
PResourceDirectoryTable = ^TResourceDirectoryTable;

//----------------------------------------------------------------------
// TPEModule record

TResourceDirectoryEntry = packed record
  name : DWORD;         // RVA Address of integer or ansistring that gives the Type, Name, or Language identifier, depending on level of table.
  RVA : DWORD;          // RVA High bit 0. Address of a Resource Data Entry (a leaf).
                        // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table (the next level down).
end;
PResourceDirectoryEntry = ^TResourceDirectoryEntry;

//----------------------------------------------------------------------
// TResourceDirectoryEntry record

TResourceDataEntry = packed record
  OffsetToData : DWORD;
  Size : DWORD;
  CodePage : DWORD;
  Reserved : DWORD;
end;
PResourceDataEntry = ^TResourceDataEntry;

//----------------------------------------------------------------------
// TPEResourceModule class

TPEResourceModule = class (TPEModule)
private
  fDetailList : TObjectList;             // List of TResourceDetails objects

protected
  procedure Decode (memory : pointer; exeSize : Integer); override;
  procedure Encode; override;
  function GetResourceCount: Integer;  override;
  function GetResourceDetails(idx: Integer): TResourceDetails; override;
public
  constructor Create;
  destructor Destroy; override;


  property ResourceCount : Integer read GetResourceCount;
  property ResourceDetails [idx : Integer] : TResourceDetails read GetResourceDetails;
  procedure DeleteResource (resourceNo : Integer); override;
  procedure InsertResource (idx : Integer; details : TResourceDetails); override;
  function AddResource (details : TResourceDetails) : Integer; override;
  function IndexOfResource (details : TResourceDetails) : Integer; override;
  procedure SortResources; override;
end;


EPEException = class (Exception);

implementation

{ TPEModule }
resourcestring
  rstInvalidDOSSignature   = 'Invalid DOS signature';
  rstInvalidCOFFSignature  = 'Invalid COFF signature';
  rstInvalidOptionalHeader = 'Invalid Windows Image';
  rstBadDictionaryIndex    = 'Index exceeds data dictionary count';
  rstBadLangID             = 'Unsupported non-integer language ID in resource';
  rstEncode                = 'Error encoding module';

type
  TResourceNode = class
    count : Integer;
    nodes : array of record
      id : ansistring;
      intID : boolean;
      case leaf : boolean of
        false : (next : TResourceNode);
        true : (data : TMemoryStream; CodePage : DWORD)
      end;

    constructor Create (const AType, AName : ansistring; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    constructor CreateNameNode (const AName : ansistring; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    constructor CreateLangNode (ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure Add (const AType, AName : ansistring; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure AddName (const AName : ansistring; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure AddLang (ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    function IsID (idx : Integer): boolean;
    destructor Destroy; override;
  end;

(*----------------------------------------------------------------------*
 | constructor PEModule.Create                                          |
 |                                                                      |
 | Constructor for TPEModule instance.  Create empty section list       |
 *----------------------------------------------------------------------*)
constructor TPEModule.Create;
begin
  inherited Create;
  fSectionList := TObjectList.Create;
  fDOSStub := TMemoryStream.Create;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Decode                                            |
 |                                                                      |
 | Decode the PE file.  Load the DOS header, the COFF header and the    |
 | 'optional' header, then load each section into fSectionList          |
 *----------------------------------------------------------------------*)
procedure TPEModule.Decode (Memory : pointer; exeSize : Integer);
var
  offset : LongInt;
  i : Integer;
  sectionHeader : PImageSectionHeader;
  commentOffset : Integer;
begin
  fSectionList.Clear;

                                // Check it's really a PE file.
  if PWORD (Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create (rstInvalidDOSSignature);

                                // Load the DOS header
  fDOSHeader := PImageDosHeader (Memory)^;

  offset := fDOSHeader._lfanew;
  fDOSStub.Write ((PAnsiChar (Memory) + sizeof (fDOSHeader))^, fDOSHeader._lfanew - sizeof (fDOSHeader));

                                // Check the COFF signature
  if PDWORD (PAnsiChar (Memory) + offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create (rstInvalidCOFFSignature);

                                // Load the COFF header
  Inc (offset, sizeof (DWORD));       
  fCOFFHeader := PImageFileHEader (PAnsiChar (Memory) + offset)^;

  Inc (offset, sizeof (fCOFFHeader));

                                // Check the Optional Header signature.  nb
                                // the optional header is compulsory for
                                // 32 bit windows modules!
  if PWORD (PAnsiChar (Memory) + offset)^ <> IMAGE_NT_OPTIONAL_HDR_MAGIC then
    raise EPEException.Create (rstInvalidOptionalHeader);

                                // Save the 'optional' header
  ReallocMem (fOptionalHeader, fCOFFHeader.SizeOfOptionalHeader);
  Move ((PAnsiChar (Memory) + Offset)^, fOptionalHeader^, fCOFFHeader.SizeOfOptionalHeader);

  Inc (offset, fCOFFHeader.SizeOfOptionalHeader);

  sectionHeader := PImageSectionHeader (PAnsiChar (memory) + offset);
  commentOffset := offset + fCOFFHeader.NumberOfSections * sizeof (TImageSectionHeader);

// Save padding between the end of the section headers, and the start of the
// 1st section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  fCommentSize := Integer (sectionHeader^.PointerToRawData) - commentOffset;

  if fCommentSize > 0 then
  begin
    GetMem (fCommentBlock, fCommentSize);
    Move ((PAnsiChar (memory) + commentOffset)^, fCommentBlock^, fCommentSize)
  end;
                                // Now save each image section in the fSectionList
  for i := 0 to fCOFFHeader.NumberOfSections - 1 do
  begin
    sectionHeader := PImageSectionHeader (PAnsiChar (memory) + offset);
    fSectionList.Add (TImageSection.Create (self, sectionHeader^, PAnsiChar (memory) + sectionHeader^.PointertoRawData));
    Inc (offset, sizeof (TImageSectionHeader));
  end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

// Save the padding between the last section and the end of the file.
// This appears to hold debug info and things ??

  fEndCommentSize := exeSize - i;
  if fEndCommentSize > 0 then
  begin
    GetMem (fEndComment, fEndCommentSize);
    Move ((PAnsiChar (memory) + i)^, fEndComment^, fEndCommentSize)
  end
end;

(*----------------------------------------------------------------------*
 | destructor PEModule.Destroy                                          |
 |                                                                      |
 | Destructor for TPEModule instance.                                   |
 *----------------------------------------------------------------------*)
destructor TPEModule.Destroy;
begin
  ReallocMem (fOptionalHeader, 0);
  fSectionList.Free;
  fDOSStub.Free;
  ReallocMem (fCommentBlock, 0);
  ReallocMem (fEndComment, 0);
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Encode                                            |
 |                                                                      |
 | Fix up the data prior to writing to stream.                          |
 |                                                                      |
 | Ensure that the headers match what we've got...                      |
 *----------------------------------------------------------------------*)
procedure TPEModule.Encode;
var
  offset : DWORD;
  i : Integer;
  section : TImageSection;
  align : Integer;
  addrAlign : Integer;
  address : Integer;
  alignedSize, AddrAlignedSize : Integer;
  codeSize, iDataSize, uDataSize, iSize : Integer;
begin
  codeSize := 0;
  iDataSize := 0;
  uDataSize := 0;
                                               // Use the DOS stub from their .EXE
  fDOSHeader._lfanew := sizeof (fDosHeader) + fDOSStub.Size;

                                               // Fixup sections count
  fCOFFHeader.NumberOfSections := fSectionList.Count;

  iSize :=  fDOSHeader._lfanew +               // File offset for start of sections
            SizeOf (DWORD) +                   // NT signature
            sizeof (fCoffHeader) +
            fCOFFHeader.SizeOfOptionalHeader +
            fSectionList.Count * sizeof (TImageSectionHeader);

  offset := iSize + fCommentSize;

  align := fOptionalHeader^.FileAlignment;
  addrAlign := fOptionalHeader^.SectionAlignment;

  address := addrAlign;
  offset := DWORD ((integer (offset) + align - 1) div align * align);

                                                // First section starts at $1000 (when loaded)
                                                // and at 'offset' in file.

  fOptionalHeader^.SizeOfHeaders := DWORD ((integer (iSize) + align - 1) div align * align);

  fOptionalHeader^.BaseOfCode := $ffffffff;
  fOptionalHeader^.CheckSum := 0;               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize  := DWORD ((integer (iSize) + addrAlign - 1) div addrAlign * addrAlign);

  for i := 0 to fSectionList.Count - 1 do      // Recalculate the section offsets
  begin
    section := TImageSection (fSectionList [i]);

    section.fSectionHeader.PointerToRawData := offset;
    section.fSectionHeader.VirtualAddress := address;

// Virtual size is size of data in memory, and is not padded to an 'alignment'.
//
// SizeOfRawData is size of data in file, padded to (file) alignment.
//
// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.
//
// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with zeros when it's loaded.
//
// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.
//
// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    section.fSectionHeader.Misc.VirtualSize := section.fRawData.Size + section.fUninitializedDataSize;
    section.fSectionHeader.SizeOfRawData := (section.fRawData.Size + align - 1) div align * align;

    alignedSize := (Integer (section.fSectionHeader.Misc.VirtualSize) + align - 1) div align * align;
    addrAlignedSize := (Integer (section.fSectionHeader.Misc.VirtualSize) + addrAlign - 1) div addrAlign * addrAlign;

    if (section.fSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
    begin
      Inc (codeSize, alignedSize);
      if DWORD (address) < fOptionalHeader^.BaseOfCode then
        fOptionalHeader^.BaseOfCode := address
    end
    else
      if (section.fSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
        Inc (iDataSize, alignedSize)
      else
        if (section.fSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
          Inc (uDataSize, alignedSize);

    Inc (iSize, addrAlignedSize);
    Inc (offset, section.fSectionHeader.SizeOfRawData);
    Inc (address, (Integer (section.fSectionHeader.Misc.VirtualSize) + addrAlign - 1) div addrAlign * addrAlign);
  end;

  fOptionalHeader^.SizeOfCode := codeSize;
  fOptionalHeader^.SizeOfInitializedData := iDataSize;
  fOptionalHeader^.SizeOfUninitializedData := uDataSize;

  i := SizeOf (DWORD) +                   // NT signature
       sizeof (fCoffHeader) +
       fCOFFHeader.SizeOfOptionalHeader +
       codeSize;

  i := (i + addrAlign - 1) div addrAlign * addrAlign;

  // With explorer.exe, codeSize is $14800, i is 148E8, so aligned 'i' is $15000
  // .. so BaseOfData should be $15000 + BaseOfCode ($1000) = $16000.
  //
  // ... but it's not - it's $15000, which means that the last $8e8 bytes of code
  // should be stampled over by the data!
  //
  // But obviously explorer.exe works, so I'm, missing a trick here.  Never mind - it
  // doesn't do any harm making it $16000 instead, and the formula works for everything
  // else I've tested...

  {$ifdef Win32}
  //todo: What should happen here in 64 bit?
  fOptionalHeader^.BaseOfData := fOptionalHeader.BaseOfCode + DWORD (i);
  {$endif}

  fOptionalHeader^.SizeOfImage := iSize;
end;

(*----------------------------------------------------------------------*
 | function PEModule.FindDictionaryEntrySection                         |
 |                                                                      |
 | Return the index of the specified section.  The 'entryNo' to find    |
 | should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in         |
 | Windows.pas.                                                         |
 *----------------------------------------------------------------------*)
function TPEModule.FindDictionaryEntrySection (entryNo: Integer; var offset : Integer): Integer;
var
  i : Integer;
  p : PImageDataDirectory;
begin
  result := -1;
  p := DataDictionary [entryNo];
                                // Find section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (p^.VirtualAddress >= ImageSection [i].fSectionHeader.VirtualAddress) and (p^.VirtualAddress < ImageSection [i].fSectionHeader.VirtualAddress + ImageSection [i].fSectionHeader.Misc.VirtualSize) then
    begin
      result := i;
      offset := p^.VirtualAddress - ImageSection [i].fSectionHeader.VirtualAddress;
      break
    end
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetCOFFHeader                                     |
 |                                                                      |
 | Return COFF header                                                   |
 *----------------------------------------------------------------------*)
function TPEModule.GetCOFFHeader: TImageFileHeader;
begin
  result := fCoffHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionary                                 |
 |                                                                      |
 | Return the data dictionary for a specified                           |
 | IMAGE_DIRECTORY_ENTRY_xxxx  index                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionary(index: Integer): PImageDataDirectory;
var
  p : PImageDataDirectory;
begin
  if index < DataDictionaryCount then
  begin
    p := @fOptionalHeader.DataDirectory [0];
    Inc (p, index);
    result := p
  end
  else
    raise ERangeError.Create (rstBadDictionaryIndex);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionaryCount                            |
 |                                                                      |
 | Return no of entries in the Data Directory                           |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionaryCount: Integer;
begin
  result := fOptionalHeader^.NumberOfRvaAndSizes
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDosHeader                                      |
 |                                                                      |
 | Return DOS header                                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  result := fDOSHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSection () : TImageSection                |
 |                                                                      |
 | Get the specified image section                                      |
 *----------------------------------------------------------------------*)
function TPEModule.GetExportCount: Integer;
var
  ExportSection : PImageExportDirectory;
  section : TImageSection;
  offset : Integer;
begin
  section := GetExportSection (offset);
  if Assigned (section) then
  begin
    ExportSection := PImageExportDirectory (PAnsiChar (section.fRawData.memory) + offset);
    result := ExportSection^.NumberOfNames
  end
  else
    result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var name: ansistring;
  var ordinal: DWORD);
var
  ExportSection : PImageExportDirectory;
  section : TImageSection;
  offset : Integer;
  po : DWORD;
  pw : PWORD;
  p : PDWORD;
  data : PAnsiChar;
begin
  section := GetExportSection (offset);
  if Assigned (section) then
  begin
    data := GetExportSectionData;
    ExportSection := PImageExportDirectory (PAnsiChar (section.fRawData.memory) + offset);
    po := DWORD (ExportSection^.AddressOfNameOrdinals);
    pw := PWORD (Data + po);
    Inc (pw, idx);
    ordinal := pw^;

    po := DWORD (ExportSection^.AddressOfNames);
    p := PDWORD (Data + po);
    Inc (p, idx);
    name := data + p^
  end
end;

function TPEModule.GetExportSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  offset := 0;
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_EXPORT, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

function TPEModule.GetExportSectionData: PAnsiChar;
var
  section : TImageSection;
  offset : Integer;
begin
  section := GetExportSection (offset);
  result := PAnsiChar (section.fRawData.Memory) - section.fSectionHeader.VirtualAddress;
end;

function TPEModule.GetImageSection(index: Integer): TImageSection;
begin
  result := TImageSection (fSectionList [index]);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Return no of image sections                                          |
 *----------------------------------------------------------------------*)
function TPEModule.GetImageSectionCount: Integer;
begin
  result := fSectionList.Count
end;

function DirValid (dir : PImageImportDirectory) : boolean;
begin
  DirValid := (dir^.Characteristics <> 0) or (dir^.TimeDateStamp <> 0) or (dir^.ForwarderChain <> 0) or
              (dir^.Name <> 0) or (dir^.FirstThunk <> 0)
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Get the optional header                                              |
 *----------------------------------------------------------------------*)
function TPEModule.GetImport(idx: Integer): PImageImportDirectory;
var
  ImportSection : PImageImportDirectory;
  section : TImageSection;
  offset : Integer;

begin
  section := GetImportSection (offset);
  result := Nil;
  if Assigned (section) then
  begin
    ImportSection := PImageImportDirectory (PAnsiChar (section.fRawData.memory) + offset);

    while DirValid (ImportSection) and (idx > 0) do
    begin
      Inc (ImportSection);
      Dec (idx)
    end;

    if DirValid (ImportSection) then
      result := ImportSection
  end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection : PImageImportDirectory;
  section : TImageSection;
  offset : Integer;
begin
  section := GetImportSection (offset);
  result := 0;
  if Assigned (section) then
  begin
    ImportSection := PImageImportDirectory (PAnsiChar (section.fRawData.memory) + offset);

    while DirValid (ImportSection) do
    begin
      Inc (result);
      Inc (ImportSection)
    end
  end
end;

function TPEModule.GetImportSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_IMPORT, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

function TPEModule.GetImportSectionData: PAnsiChar;
var
  section : TImageSection;
  offset : Integer;
begin
  section := GetImportSection (offset);
  result := PAnsiChar (section.fRawData.Memory) - section.fSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TImageOptionalHeader;
begin
  result := fOptionalHeader^
end;

function TPEModule.GetResourceSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a file                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromFile(const name: ansistring);
var
  f : TFileStream;
begin
  f := TFileStream.Create (String(name), fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (f)
  finally
    f.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a stream                                        |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromStream(s: TStream);
var
  m : TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    m.CopyFrom (s, 0);

    Decode (m.memory, m.size)
  finally
    m.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToFile                                       |
 |                                                                      |
 | Save the module to a file                                            |
 *----------------------------------------------------------------------*)
(*
procedure TPEModule.SaveToFile(const name: ansistring);
var
  f : TFileStream;
begin
  f := TFileStream.Create (name, fmCreate);
  try
    SaveToStream (f)
  finally
    f.Free
  end
end;
*)
(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToStream                                     |
 |                                                                      |
 | Save the module to a stream                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.SaveToStream(s: TStream);
var
  NTSignature : DWORD;
  i : Integer;
  section : TImageSection;
  paddingSize, paddingLen : Integer;
  padding : PAnsiChar;
  f : TMemoryStream;
  oldCheckSum, newCheckSum : DWORD;
  ntHeaders : PImageNTHEaders;
  ckOffset : DWORD;
begin
  Encode;                       // Encode the data.

  NTSignature := IMAGE_NT_SIGNATURE;

                                // Write the DOS stub
  s.Write (fDOSHeader, sizeof (fDOSHeader));
  s.CopyFrom (fDOSStub, 0);

                                // Write NT sig and COFF header
  s.Write (NTSignature, sizeof (NTSignature));
  s.Write (fCOFFHeader, sizeof (fCOFFHeader));
  ckOffset := s.Position + Integer (@fOptionalHeader^.CheckSum) - Integer (@fOptionalHeader^);
  s.Write (fOptionalHeader^, fCOFFHeader.SizeOfOptionalHeader);

                                // Write the section headers
  for i := 0 to fSectionList.Count - 1 do
  begin
    section := TImageSection (fSectionList [i]);
    s.Write (section.fSectionHeader, sizeof (section.fSectionHeader))
  end;

  if fCommentSize > 0 then      // Save the 'comment' section.  See 'Decode' for details
    s.Write (fCommentBlock^, fCommentSize);

                                // Write the sections
  padding := Nil;
  paddingLen := 0;
  try
    for i := 0 to fSectionList.Count - 1 do
    begin
                                // Write padding up to file offset of the section
      section := TImageSection (fSectionList [i]);
      paddingSize := section.fSectionHeader.PointerToRawData - DWORD (s.Position);

      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory (padding, paddingLen);
      end;

      if paddingSize > 0 then   // Put our signature at the start of the first
          s.Write (padding^, paddingSize);

                                // Write the section data.
      s.CopyFrom (section.fRawData, 0);

                                // Write data
      with section.fSectionHeader do
        paddingSize := SizeOfRawData - misc.VirtualSize;

                                // Pad data
      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory (padding, paddingLen);
      end;

      if paddingSize > 0 then
        s.Write (padding^, paddingSize)

    end;

    if fEndCommentSize > 0 then  // Save the debug info.
      s.Write (fEndComment^, fEndCommentSize)
  finally
    ReallocMem (padding, 0)
  end;

  f := TMemoryStream.Create;    // Now calculate the checksum....
  try
    s.Seek (0, soFromBeginning);
    f.LoadFromStream (s);
    ntHeaders := ChecksumMappedFile (f.Memory, f.Size, @oldCheckSum, @newCheckSum);

    if Assigned (ntHeaders) then
    begin
      s.Seek (ckOffset, soFromBeginning);
      s.Write (newChecksum, sizeof (newChecksum))
    end
  finally
    f.Free
  end;

  s.Seek (0, soFromEnd);
end;


{ TImageSection }

(*----------------------------------------------------------------------*
 | constructor TImageSection.Create                                     |
 |                                                                      |
 | Constructor for TImageSection.                                       |
 *----------------------------------------------------------------------*)
constructor TImageSection.Create(AParent: TPEModule;
  const AHeader : TImageSectionHeader; rawData : pointer);
begin
  fSectionHeader := AHeader;
  fRawData := TMemoryStream.Create;

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if fSectionHeader.Misc.VirtualSize <= fSectionHeader.SizeOfRawData then
  begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if fSectionHeader.Misc.VirtualSize = 0 then
      fSectionHeader.Misc.VirtualSize := fSectionHeader.SizeOfRawData;
    fRawData.Write (rawData^, fSectionHeader.Misc.VirtualSize)
  end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
  begin
    fRawData.Write (rawData^, fSectionHeader.SizeOfRawData);
    fUninitializedDataSize := fSectionHeader.Misc.VirtualSize - fSectionHeader.SizeOfRawData;
  end;

  fParent := AParent;
end;

(*----------------------------------------------------------------------*
 | function TImageSection.GetSectionName                                |
 |                                                                      |
 | Return the section name - eg. .data                                  |
 *----------------------------------------------------------------------*)
function TImageSection.GetSectionName: ansistring;
begin
  result := PAnsiChar (@fSectionHeader.Name)
end;

(*----------------------------------------------------------------------*
 | destructor TImageSection.Destroy                                     |
 |                                                                      |
 | destructor for TImageSection.                                        |
 *----------------------------------------------------------------------*)
destructor TImageSection.destroy;
begin
  fRawData.Free;
  inherited;
end;

{ TPEResourceModule }

(*----------------------------------------------------------------------*
 | procedure TPEResourceModule.DeleteResource                           |
 |                                                                      |
 | Delete the specified resource (by index)                             |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.DeleteResource(resourceNo: Integer);
var
  res : TResourceDetails;
begin
  res := ResourceDetails [resourceNo];
  inherited;
  resourceNo := IndexOfResource (Res);
  if resourceNo <> -1 then
    fDetailList.Delete (resourceNo);
end;

(*----------------------------------------------------------------------*
 | constructor TPEResourceModule.Create                                 |
 |                                                                      |
 | Constructor for TPEResourceModule                                    |
 *----------------------------------------------------------------------*)
constructor TPEResourceModule.Create;
begin
  inherited Create;
  fDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TPEResourceModule.Destroy                                 |
 |                                                                      |
 | Destructor for TPEResourceModule                                     |
 *----------------------------------------------------------------------*)
destructor TPEResourceModule.Destroy;
begin
  fDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Decode                                    |
 |                                                                      |
 | Decode the section's resource tree into a list of resource details   |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Decode;
var
  section : TImageSection;
  tp, name : ansistring;
  lang : Integer;
  offset : Integer;

  // Get ansistring resource name
  function GetResourceStr (IdorName : boolean; section : TImageSection; n : DWORD) : ansistring;
  var
    p : PWideChar;
  begin
    if IdorName then
      result := ansistring(IntToStr (n))
    else
    begin
      p := PWideChar (pansichar(section.fRawData.Memory) + (n and $7fffffff));
      result := ResourceWideCharToStr (p, CP_ACP)
    end
  end;

  // (recursively) get resources
  procedure GetResource (offset, level : Integer);
  var
    entry : PResourceDirectoryEntry;
    i, count : Integer;
    IDorName : boolean;
    dataEntry : PResourceDataEntry;
    table : PResourceDirectoryTable;
    details : TResourceDetails;
  begin
    table := PResourceDirectoryTable (PAnsiChar (section.fRawData.memory) + offset);
    with table^ do
      count := cNameEntries + cIDEntries;

    entry := PResourceDirectoryEntry (PAnsiChar (section.fRawData.memory) + offset + sizeof (TResourceDirectoryTable));
    for i := 0 to count - 1 do
    begin
      idOrName := i >= table^.cNameEntries;
      case level of
        0 : tp := GetResourceStr (IDOrName, section, entry^.name);
        1 :
            name := GetResourceStr (IDOrName, section, entry^.name);
        2 :
          begin
            if not IdOrName then
              raise EPEException.Create (rstBadLangID);

            lang := entry^.name
          end
      end;

      if (entry^.RVA and $80000000) > 0 then // Not a leaf node - traverse the tree
        GetResource (entry^.RVA and $7fffffff, level + 1)
      else
      begin
                                             // It's a leaf node - create resource details
        dataEntry := PResourceDataEntry (PAnsiChar (section.fRawData.Memory) + entry^.RVA);
        details := TResourceDetails.CreateResourceDetails (self, lang, name, tp, dataEntry^.Size, PAnsiChar (section.fRawData.Memory) + dataEntry^.OffsetToData - section.fSectionHeader.VirtualAddress);
        details.CodePage := dataEntry^.CodePage;
        details.Characteristics := table^.characteristics;
        details.DataVersion := DWORD (table^.versionMajor) * 65536 + DWORD (table^.versionMinor);
        fDetailList.Add (details);

      end;

      Inc (entry)
    end
  end;

begin
  inherited;
  section := GetResourceSection (offset);
  if section <> nil then
    GetResource (offset, 0)
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceCount                          |
 |                                                                      |
 | Return the number of resources in the resource section               |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceCount: Integer;
begin
  result := fDetailList.Count
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceDetails                        |
 |                                                                      |
 | Get the resource details for the specified resource.                 |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceDetails(
  idx: Integer): TResourceDetails;
begin
  result := TResourceDetails (fDetailList [idx]);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.IndexOfResource                           |
 |                                                                      |
 | Return the index of the specified resource details in the resource   |
 *----------------------------------------------------------------------*)
function TPEResourceModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  result := fDetailList.IndexOf (details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.InsertResource                            |
 |                                                                      |
 | Insert a resource in the list.                                       |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  fDetailList.Insert (idx, details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Encode                                    |
 |                                                                      |
 | Complicated?  I'll give you complicated ...                          |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Encode;
var
  i : Integer;
  details : TResourceDetails;
  section : TImageSection;
  root : TResourceNode;
  versMajor, versMinor : word;
  TimeStamp : DWORD;
  nameSize, nameOffset, namePos, tableOffset : DWORD;
  deOffset, dePos, deSize : DWORD;
  dataOffset, dataPos, dataSize : DWORD;
  offset : Integer;

  nameTable : PAnsiChar;
  deTable : PAnsiChar;
  data : PAnsiChar;
  zeros : PAnsiChar;

  //------------------------------------------------------------------
  // Calculate offset and size of name table and DirectoryEntry table.
  // Calculate size of data

  procedure GetNameTableSize (node : TResourceNode);
  var
    i : Integer;
  begin
    Inc (nameOffset, sizeof (TResourceDirectoryTable));
    Inc (deOffset, sizeof (TResourceDirectoryTable));

    for i := 0 to node.count - 1 do
    begin
      Inc (nameOffset, sizeof (TResourceDirectoryEntry));
      Inc (deOffset, sizeof (TResourceDirectoryEntry));

      if not node.nodes [i].intID then
        Inc (nameSize, Length (node.nodes [i].id) * sizeof (WideChar) + sizeof (word));

      if not node.nodes [i].leaf then
        GetNameTableSize (node.nodes [i].next)
      else
      begin
        Inc (nameOffset, sizeof (TResourceDataEntry));
        Inc (deSize, sizeof (TResourceDataEntry));
        dataSize := (dataSize + DWORD (node.nodes [i].data.Size) + 3) div 4 * 4;
      end
    end
  end;

  //------------------------------------------------------------------
  // Save a node to section.fRawData (and save it's child nodes recursively)

  procedure SaveToSection (node : TResourceNode);
  var
    table : TResourceDirectoryTable;
    entry : TResourceDirectoryEntry;
    dataEntry : PResourceDataEntry;
    i, n : Integer;
    w : WideString;
    wl : word;

  //------------------------------------------------------------------
  // Save entry (i), and the child nodes

    procedure SaveNode (i : Integer);
    begin
      if node.nodes [i].intID then      // id is a simple integer
        entry.name := StrToInt (String(node.nodes [i].id))
      else
      begin                             // id is an offset to a name in the
                                        // name table.
        entry.name := nameOffset + namePos + $80000000;
        w := String(node.nodes [i].id);
        wl := Length (node.nodes [i].id);
        Move (wl, nameTable [namePos], sizeof (wl));
        Inc (namePos, sizeof (wl));
        Move (w [1], nameTable [namePos], wl * sizeof (WideChar));
        Inc (namePos, wl * sizeof (WideChar))
      end;

      if node.nodes [i].leaf then       // RVA points to a TResourceDataEntry in the
      begin                             // data entry table.
        entry.RVA := deOffset + dePos;
        dataEntry := PResourceDataEntry (deTable + dePos);
        dataEntry^.CodePage := node.nodes [i].CodePage;
        dataEntry^.Reserved := 0;
        dataEntry^.Size := node.nodes [i].data.Size;
        dataEntry^.OffsetToData := dataOffset + dataPos + section.fSectionHeader.VirtualAddress;

        Move (node.nodes [i].data.memory^, data [dataPos], dataEntry^.Size);

        Inc (dePos, sizeof (TResourceDataEntry));
        dataPos := (dataPos + dataEntry^.size + 3) div 4 * 4;
        section.fRawData.Write (entry, sizeof (entry));
      end
      else                              // RVA points to another table.
      begin
        entry.RVA := $80000000 + tableOffset;
        section.fRawData.Write (entry, sizeof (entry));
        n := section.fRawData.Position;
        SaveToSection (node.nodes [i].next);
        section.fRawData.Seek (n, soFromBeginning);
      end
    end;

  begin { SaveToSection }
    table.characteristics := 0;
    table.timeDateStamp := TimeStamp;
    table.versionMajor := versMajor;
    table.versionMinor := versMinor;
    table.cNameEntries := 0;
    table.cIDEntries := 0;

                                        // Calculate no of integer and ansistring IDs
    for i := 0 to node.count - 1 do
      if node.nodes [i].intID then
        Inc (table.cIDEntries)
      else
        Inc (table.cNameEntries);

    section.fRawData.Seek (tableOffset, soFromBeginning);
    section.fRawData.Write (table, sizeof (table));

    tableOffset := tableOffset + sizeof (TResourceDirectoryTable) + DWORD (node.Count) * sizeof (TResourceDirectoryEntry);

                                        // The docs suggest that you save the nodes
                                        // with ansistring entries first.  Goodness knows why,
                                        // but play along...
    for i := 0 to node.count - 1 do
      if not node.nodes [i].intID then
       SaveNode (i);

    for i := 0 to node.count - 1 do
      if node.nodes [i].intID then
       SaveNode (i);

    section.fRawData.Seek (0, soFromEnd);
  end;


begin { Encode }
  section := GetResourceSection (offset);

                                        // Get the details in a tree structure
  root := Nil;
  data := Nil;
  deTable := Nil;
  zeros := Nil;

  try
    for i := 0 to fDetailList.Count - 1 do
    begin
      details := TResourceDetails (fDetailList.Items [i]);
      if root = Nil then
        root := TResourceNode.Create (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
      else
        root.Add (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
    end;

                                          // Save elements of their original EXE
    versMajor := PResourceDirectoryTable (section.fRawData.Memory)^.versionMajor;
    versMinor := PResourceDirectoryTable (section.fRawData.Memory)^.versionMinor;
    TimeStamp := PResourceDirectoryTable (section.fRawData.Memory)^.timeDateStamp;


    section.fRawData.Clear;               // Clear the data.  We're gonna recreate
                                          // it from our resource details.

    nameSize := 0; nameOffset := offset;
    deSize := 0; deOffset := offset;
    dataSize := 0;

    GetNameTableSize (root);              // Calculate sizes and offsets of the
                                          // name table, the data entry table and
                                          // the size of the data.

                                          // Calculate the data offset.  Must be aligned.
    dataOffset := (nameOffset + nameSize + 15) div 16 * 16;

                                          // Initialize globals...
    namePos := 0;                         //   Offset of next entry in the ansistring table
    dePos := 0;                           //   Offset of next entry in the data entry table
    dataPos := 0;                         //   Offset of next data block.
    tableOffset := 0;                     //   Offset of next TResourceDirectoryTable


    GetMem (nameTable, nameSize);         // Allocate buffers for tables
    GetMem (data, dataSize);
    GetMem (deTable, deSize);

    SaveToSection (root);               // Do the work.

                                        // Save the tables
    section.fRawData.Write (deTable^, deSize);
    section.fRawData.Write (nameTable^, nameSize);

                                        // Add padding so the data goes on a
                                        // 16 byte boundary.
    if DWORD (section.fRawData.Position) < dataOffset then
    begin
      GetMem (zeros, dataOffset - DWORD (section.fRawData.Position));
      ZeroMemory (zeros, dataOffset - DWORD (section.fRawData.Position));
      section.fRawData.Write (zeros^, dataOffset - DWORD (section.fRawData.Position))
    end;

                                        // Write the data.
    section.fRawData.Write (data^, dataSize);

    inherited; // **** Must call inherited !

  finally       // Tidy up.
    ReallocMem (zeros, 0);
    FreeMem (nameTable);
    FreeMem (deTable);
    FreeMem (data);
    root.Free
  end
end;


{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: ansistring; ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;

begin
  for i := 0 to count - 1 do
    if AType = nodes [i].id then
    begin
      nodes [i].next.AddName (AName, ALang, aData, codePage);
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AType;
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateNameNode (AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if AnsiString( IntToStr (ALang) ) = nodes [i].id then
    begin
      nodes [i].data := aData;
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString( IntToStr (ALang) );
  nodes [count - 1].intId := True;
  nodes [count - 1].leaf := True;
  nodes [count - 1].data := aData;
  nodes [count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: ansistring; ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if AName = nodes [i].id then
    begin
      nodes [i].next.AddLang (ALang, aData, codePage);
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AName;
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateLangNode (ALang, aData, codePage)
end;

constructor TResourceNode.Create(const AType, AName: ansistring;
  ALang: Integer; aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AType;
  nodes [count - 1].intID := isID (count - 1);
  nodes [0].leaf := False;
  nodes [0].next := TResourceNode.CreateNameNode (AName, ALang, aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AnsiString( IntToStr (ALang) );
  nodes [count - 1].intID := True;
  nodes [0].leaf := True;
  nodes [0].data := aData;
  nodes [0].CodePage := codePage
end;

constructor TResourceNode.CreateNameNode(const AName: ansistring;
  ALang: Integer; aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AName;
  nodes [count - 1].intID := isID (count - 1);

  nodes [0].leaf := False;
  nodes [0].next := TResourceNode.CreateLangNode (ALang, aData, codePage)
end;

destructor TResourceNode.Destroy;
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if not nodes [i].leaf then
      nodes [i].next.Free;

  inherited;
end;

function TResourceNode.IsID (idx : Integer): boolean;
var
  i : Integer;
begin
  result := True;
  for i := 1 to Length (nodes [idx].id) do
    if not (nodes [idx].id [i] in ['0'..'9']) then
    begin
      result := False;
      break
    end;

  if result then
    result := AnsiString( IntToStr (StrToInt (String(nodes [idx].id))) ) = nodes [idx].id;
end;

function TPEResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := fDetailList.Add (details);
end;

procedure TPEResourceModule.SortResources;
begin
  fDetailList.Sort (compareDetails);
end;

end.

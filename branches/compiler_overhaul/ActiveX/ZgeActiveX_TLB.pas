unit ZgeActiveX_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 5081 $
// File generated on 2007-06-04 17:26:58 from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\Data\Delphi32\ZzDC\ActiveX\ZgeActiveX.tlb (1)
// LIBID: {F4C7D0BD-A8E9-45BC-99B6-35C84B2180EE}
// LCID: 0
// Helpfile: 
// HelpString: ZgeActiveX Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ZgeActiveXMajorVersion = 1;
  ZgeActiveXMinorVersion = 0;

  LIBID_ZgeActiveX: TGUID = '{F4C7D0BD-A8E9-45BC-99B6-35C84B2180EE}';

  IID_IZgeActiveXControl: TGUID = '{8C1FB6F5-666A-44D3-A98C-8B24C1B13CE1}';
  CLASS_ZgeActiveXControl: TGUID = '{49F96FB9-0D7F-4E4E-ADBC-442EAB031529}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IZgeActiveXControl = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ZgeActiveXControl = IZgeActiveXControl;


// *********************************************************************//
// Interface: IZgeActiveXControl
// Flags:     (4096) Dispatchable
// GUID:      {8C1FB6F5-666A-44D3-A98C-8B24C1B13CE1}
// *********************************************************************//
  IZgeActiveXControl = interface(IDispatch)
    ['{8C1FB6F5-666A-44D3-A98C-8B24C1B13CE1}']
  end;


implementation


end.

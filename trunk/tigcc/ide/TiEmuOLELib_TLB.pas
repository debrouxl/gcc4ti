unit TiEmuOLELib_TLB;

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

// PASTLWTR : $Revision$
// File generated on 10/29/06 7:32:18 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: G:\Delphi6\test\tiemu.tlb (1)
// LIBID: {6140DC7C-3D74-41F9-A787-5946994AA8F2}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\SYSTEM\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (C:\WINDOWS\SYSTEM\stdvcl40.dll)
// Errors:
//   Error creating palette bitmap of (TTiEmuOLE) : No Server registered for this CoClass
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

uses ActiveX, Classes, Graphics, OleServer, StdVCL, Variants, Windows;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  TiEmuOLELibMajorVersion = 1;
  TiEmuOLELibMinorVersion = 0;

  LIBID_TiEmuOLELib: TGUID = '{6140DC7C-3D74-41F9-A787-5946994AA8F2}';

  IID_ITiEmuOLE: TGUID = '{E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}';
  CLASS_TiEmuOLE: TGUID = '{B2A17B13-9D6F-4DD4-A2A9-6FE06ADC1D33}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITiEmuOLE = interface;
  ITiEmuOLEDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TiEmuOLE = ITiEmuOLE;


// *********************************************************************//
// Interface: ITiEmuOLE
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}
// *********************************************************************//
  ITiEmuOLE = interface(IDispatch)
    ['{E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}']
    function  image_loaded: WordBool; safecall;
    function  emulated_calc_type: SYSINT; safecall;
    function  emulated_hw_version: SYSINT; safecall;
    function  emulated_os_version: WideString; safecall;
    function  ready_for_transfers: WordBool; safecall;
    function  send_file(const filename: WideString): WordBool; safecall;
    function  debug_file(const filename: WideString): WordBool; safecall;
    function  reset_calc(clearmem: WordBool): WordBool; safecall;
    function  execute_command(const command: WideString): WordBool; safecall;
    function  turn_calc_on: WordBool; safecall;
    function  enter_debugger: WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  ITiEmuOLEDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}
// *********************************************************************//
  ITiEmuOLEDisp = dispinterface
    ['{E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}']
    function  image_loaded: WordBool; dispid 1610743808;
    function  emulated_calc_type: SYSINT; dispid 1610743809;
    function  emulated_hw_version: SYSINT; dispid 1610743810;
    function  emulated_os_version: WideString; dispid 1610743811;
    function  ready_for_transfers: WordBool; dispid 1610743812;
    function  send_file(const filename: WideString): WordBool; dispid 1610743813;
    function  debug_file(const filename: WideString): WordBool; dispid 1610743814;
    function  reset_calc(clearmem: WordBool): WordBool; dispid 1610743815;
    function  execute_command(const command: WideString): WordBool; dispid 1610743816;
    function  turn_calc_on: WordBool; dispid 1610743817;
    function  enter_debugger: WordBool; dispid 1610743818;
  end;

// *********************************************************************//
// The Class CoTiEmuOLE provides a Create and CreateRemote method to          
// create instances of the default interface ITiEmuOLE exposed by              
// the CoClass TiEmuOLE. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTiEmuOLE = class
    class function Create: ITiEmuOLE;
    class function CreateRemote(const MachineName: string): ITiEmuOLE;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TTiEmuOLE
// Help String      : TiEmu OLE Interface Class
// Default Interface: ITiEmuOLE
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TTiEmuOLEProperties= class;
{$ENDIF}
  TTiEmuOLE = class(TOleServer)
  private
    FIntf:        ITiEmuOLE;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TTiEmuOLEProperties;
    function      GetServerProperties: TTiEmuOLEProperties;
{$ENDIF}
    function      GetDefaultInterface: ITiEmuOLE;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ITiEmuOLE);
    procedure Disconnect; override;
    function  image_loaded: WordBool;
    function  emulated_calc_type: SYSINT;
    function  emulated_hw_version: SYSINT;
    function  emulated_os_version: WideString;
    function  ready_for_transfers: WordBool;
    function  send_file(const filename: WideString): WordBool;
    function  debug_file(const filename: WideString): WordBool;
    function  reset_calc(clearmem: WordBool): WordBool;
    function  execute_command(const command: WideString): WordBool;
    function  turn_calc_on: WordBool;
    function  enter_debugger: WordBool;
    property  DefaultInterface: ITiEmuOLE read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TTiEmuOLEProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TTiEmuOLE
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TTiEmuOLEProperties = class(TPersistent)
  private
    FServer:    TTiEmuOLE;
    function    GetDefaultInterface: ITiEmuOLE;
    constructor Create(AServer: TTiEmuOLE);
  protected
  public
    property DefaultInterface: ITiEmuOLE read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

implementation

uses ComObj;

class function CoTiEmuOLE.Create: ITiEmuOLE;
begin
  Result := CreateComObject(CLASS_TiEmuOLE) as ITiEmuOLE;
end;

class function CoTiEmuOLE.CreateRemote(const MachineName: string): ITiEmuOLE;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TiEmuOLE) as ITiEmuOLE;
end;

procedure TTiEmuOLE.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{B2A17B13-9D6F-4DD4-A2A9-6FE06ADC1D33}';
    IntfIID:   '{E9373BD9-7363-427F-A2A6-1E8BA91FFB3E}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TTiEmuOLE.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ITiEmuOLE;
  end;
end;

procedure TTiEmuOLE.ConnectTo(svrIntf: ITiEmuOLE);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TTiEmuOLE.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TTiEmuOLE.GetDefaultInterface: ITiEmuOLE;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TTiEmuOLE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TTiEmuOLEProperties.Create(Self);
{$ENDIF}
end;

destructor TTiEmuOLE.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TTiEmuOLE.GetServerProperties: TTiEmuOLEProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TTiEmuOLE.image_loaded: WordBool;
begin
  Result := DefaultInterface.image_loaded;
end;

function  TTiEmuOLE.emulated_calc_type: SYSINT;
begin
  Result := DefaultInterface.emulated_calc_type;
end;

function  TTiEmuOLE.emulated_hw_version: SYSINT;
begin
  Result := DefaultInterface.emulated_hw_version;
end;

function  TTiEmuOLE.emulated_os_version: WideString;
begin
  Result := DefaultInterface.emulated_os_version;
end;

function  TTiEmuOLE.ready_for_transfers: WordBool;
begin
  Result := DefaultInterface.ready_for_transfers;
end;

function  TTiEmuOLE.send_file(const filename: WideString): WordBool;
begin
  Result := DefaultInterface.send_file(filename);
end;

function  TTiEmuOLE.debug_file(const filename: WideString): WordBool;
begin
  Result := DefaultInterface.debug_file(filename);
end;

function  TTiEmuOLE.reset_calc(clearmem: WordBool): WordBool;
begin
  Result := DefaultInterface.reset_calc(clearmem);
end;

function  TTiEmuOLE.execute_command(const command: WideString): WordBool;
begin
  Result := DefaultInterface.execute_command(command);
end;

function  TTiEmuOLE.turn_calc_on: WordBool;
begin
  Result := DefaultInterface.turn_calc_on;
end;

function  TTiEmuOLE.enter_debugger: WordBool;
begin
  Result := DefaultInterface.enter_debugger;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TTiEmuOLEProperties.Create(AServer: TTiEmuOLE);
begin
  inherited Create;
  FServer := AServer;
end;

function TTiEmuOLEProperties.GetDefaultInterface: ITiEmuOLE;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TTiEmuOLE]);
end;

end.


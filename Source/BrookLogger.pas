unit BrookLogger;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
{$IFDEF FPC}
  Math,
{$ELSE}
  Types,
  IOUtils,
{$ENDIF}
  SysUtils,
  DateUtils,
  Classes,
  BrookExtra;

const
  BROOK_LOGGER_OUTPUT_NAME = 'Console';
  BROOK_LOGGER_TAG = 'BrookLogger_';

resourcestring
  SBrookEmptyOutputName = 'Empty output name.';
  SBrookActiveOutput = 'Active output log.';
  SBrookInactiveOutput = 'Inactive output log.';
  SBrookInvalidOutputClass = 'Invalid output class: %s.';
  SBrookUnknownOutputName = 'Unknown output name: %s.';
  SBrookLevelInfo = 'INFO';
  SBrookLevelHint = 'HINT';
  SBrookLevelWarn = 'WARN';
  SBrookLevelDebug = 'DEBUG';
  SBrookLevelError = 'ERROR';

type
  TBrookLoggerOutput = class abstract(TPersistent)
  private
    FFilters: TStringList;
    FOptions: TStringList;
    class function InternalFormat(const ALevel,
      AMessage: string): string; inline;
  protected
    class function FormatLog(const ALevel, AMessage: string): string; virtual;
    class function FormatFail(const ALevel: string;
      AException: Exception): string; virtual;
  public
    constructor Create(AFilters, AOptions: TStringList); virtual;
    class function GetRegisterAlias: string; virtual;
    class function GetName: string; virtual; abstract;
    function IsFiltered(const ALevel: string): Boolean; virtual;
    procedure Log(const ALevel, AMessage: string); virtual; abstract;
    procedure Fail(const ALevel: string;
      AException: Exception); virtual; abstract;
    property Filters: TStringList read FFilters;
    property Options: TStringList read FOptions;
  end;

  TBrookLoggerOutputClass = class of TBrookLoggerOutput;

  TBrookLoggerOutputConsole = class(TBrookLoggerOutput)
  public
    class function GetName: string; override;
    procedure Log(const ALevel, AMessage: string); override;
    procedure Fail(const ALevel: string; AException: Exception); override;
  end;

  TBrookLoggerOutputFile = class(TBrookLoggerOutput)
  private
    FHandle: TFileStream;
    FEncoding: TEncoding;
    FLastDate: TDate;
    FDirectory: string;
    FFileName: TFileName;
    procedure SetDirectory(const AValue: string);
  protected
    function CreateFile(AEncoding: TEncoding;
      const AFileName: TFileName): TFileStream; overload; virtual;
    function CreateFile(
      const AFileName: TFileName): TFileStream; overload; virtual;
    function RecreateFile(const AFileName: TFileName): TFileStream; virtual;
    procedure UpgradeFile; virtual;
    procedure WriteLog(const AMsg: string); inline;
    property Handle: TFileStream read FHandle;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetName: string; override;
    procedure Log(const ALevel, AMessage: string); override;
    procedure Fail(const ALevel: string; AException: Exception); override;
    property Directory: string read FDirectory write SetDirectory;
    property FileName: TFileName read FFileName;
  end;

  TBrookLoggerLevels = class(TPersistent)
  private
    FInfo: string;
    FHint: string;
    FWarn: string;
    FDebug: string;
    FError: string;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    property Info: string read FInfo write FInfo;
    property Hint: string read FHint write FHint;
    property Warn: string read FWarn write FWarn;
    property Debug: string read FDebug write FDebug;
    property Error: string read FError write FError;
  end;

{$IFNDEF FPC}

  TBrookLoggerTraceProc = reference to procedure;

  TBrookLoggerTraceFunc<T> = reference to function: T;

{$ENDIF}

  TBrookLogger = class(TComponent)
  private
    FOutput: TBrookLoggerOutput;
    FFilters: TStringList;
    FOptions: TStringList;
    FLevels: TBrookLoggerLevels;
    FOutputName: string;
    FStreamedActive: Boolean;
    FActive: Boolean;
    function GetOutput: TBrookLoggerOutput;
    procedure SetActive(AValue: Boolean);
    procedure SetOutputName(const AValue: string);
    function IsActiveStored: Boolean;
    function IsOutputNameStored: Boolean;
    procedure SetFilters(AValue: TStringList);
    procedure SetOptions(AValue: TStringList);
  protected
    procedure Loaded; override;
    function CreateFilters: TStringList; virtual;
    function CreateOptions: TStringList; virtual;
    function CreateOutput(AFilters,
      AOptions: TStringList): TBrookLoggerOutput; virtual;
    function CreateLevels: TBrookLoggerLevels; virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; inline;
    procedure CheckInactive; inline;
    procedure CheckOutputName; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetOutputClass: TBrookLoggerOutputClass; inline;
    procedure Open;
    procedure Close;
    procedure Log(const ALevel, AMessage: string); inline;
    procedure Fail(const ALevel: string; AException: Exception); inline;
    procedure Info(const AMessage: string); inline;
    procedure Hint(const AMessage: string); inline;
    procedure Warn(const AMessage: string); inline;
    procedure Debug(const AMessage: string); inline;
    procedure Error(AException: Exception); inline;
{$IFNDEF FPC}
    procedure Trace(AProc: TBrookLoggerTraceProc; const AStartingMsg,
      AFinishingMsg: string); overload; inline;
    procedure Trace(AProc: TBrookLoggerTraceProc); overload; inline;
    function Trace<T>(AFunc: TBrookLoggerTraceFunc<T>; const AStartingMsg,
      AFinishingMsg: string): T; overload; inline;
    function Trace<T>(AFunc: TBrookLoggerTraceFunc<T>): T; overload; inline;
{$ENDIF}
    property Output: TBrookLoggerOutput read GetOutput;
  published
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    property Levels: TBrookLoggerLevels read FLevels write FLevels;
    property OutputName: string read FOutputName write SetOutputName
      stored IsOutputNameStored;
    property Filters: TStringList read FFilters write SetFilters;
    property Options: TStringList read FOptions write SetOptions;
  end;

implementation

{ TBrookLoggerOutput }

constructor TBrookLoggerOutput.Create(AFilters, AOptions: TStringList);
begin
  inherited Create;
  if not Assigned(AFilters) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AFilters']);
  if not Assigned(AOptions) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AOptions']);
  FFilters := AFilters;
  FOptions := AOptions;
end;

class function TBrookLoggerOutput.GetRegisterAlias: string;
begin
  Result := Concat(BROOK_LOGGER_TAG, GetName);
end;

class function TBrookLoggerOutput.InternalFormat(const ALevel,
  AMessage: string): string;
begin
  Result := Concat(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' ', ALevel,
    ': ', AMessage.TrimRight);
end;

class function TBrookLoggerOutput.FormatLog(const ALevel,
  AMessage: string): string;
begin
  Result := InternalFormat(ALevel, AMessage);
end;

class function TBrookLoggerOutput.FormatFail(const ALevel: string;
  AException: Exception): string;
begin
  if not Assigned(AException) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AException']);
  Result := FormatLog(ALevel, Concat(AException.ClassName, ': ',
    AException.Message));
end;

function TBrookLoggerOutput.IsFiltered(const ALevel: string): Boolean;
begin
  Result := (FFilters.Count > 0) and (FFilters.IndexOf(ALevel) > -1);
end;

{ TBrookLoggerOutputConsole }

class function TBrookLoggerOutputConsole.GetName: string;
begin
  Result := 'Console';
end;

procedure TBrookLoggerOutputConsole.Log(const ALevel, AMessage: string);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputConsole.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsConsole and not IsFiltered(ALevel) then
    WriteLn(ErrOutput, FormatFail(ALevel, AException));
end;

{ TBrookLoggerOutputFile }

procedure TBrookLoggerOutputFile.AfterConstruction;
begin
  inherited AfterConstruction;
  if not FDirectory.IsEmpty then
    Exit;
  FDirectory := FOptions.Values['Directory'];
  if FDirectory.IsEmpty then
    FDirectory :=
  {$IFDEF FPC}
      GetUserDir
  {$ELSE}
      TPath.GetHomePath
  {$ENDIF};
end;

destructor TBrookLoggerOutputFile.Destroy;
begin
  FHandle.Free;
  inherited Destroy;
end;

function TBrookLoggerOutputFile.CreateFile(AEncoding: TEncoding;
  const AFileName: TFileName): TFileStream;
var
  VMode: Word;
begin
  FEncoding := AEncoding;
  if not Assigned(FEncoding) then
    FEncoding := TEncoding.Default;
  if FileExists(AFileName) then
    VMode := fmOpenReadWrite
  else
    VMode := fmCreate;
  VMode := VMode or fmShareDenyWrite;
  Result := TFileStream.Create(AFileName, VMode, BROOK_FILE_RIGHTS);
end;

function TBrookLoggerOutputFile.CreateFile(
  const AFileName: TFileName): TFileStream;
begin
  Result := CreateFile(TEncoding.UTF8, AFileName);
end;

function TBrookLoggerOutputFile.RecreateFile(
  const AFileName: TFileName): TFileStream;
begin
  FHandle.Free;
  Result := CreateFile(FEncoding, AFileName);
end;

procedure TBrookLoggerOutputFile.UpgradeFile;
var
  VDate: TDate;
begin
  VDate := Date;
  if CompareDateTime(IncDay(FLastDate), VDate) <> LessThanValue then
    Exit;
  FLastDate := VDate;
  if not FDirectory.IsEmpty then
    ForceDirectories(FDirectory);
  FFileName := Concat(IncludeTrailingPathDelimiter(FDirectory),
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''), '_',
    FormatDateTime('yyyymmdd', FLastDate), '.log');
  FHandle := RecreateFile(FFileName);
end;

procedure TBrookLoggerOutputFile.SetDirectory(const AValue: string);
begin
  if AValue = FDirectory then
    Exit;
  FDirectory := AValue;
  FLastDate := 0;
end;

procedure TBrookLoggerOutputFile.WriteLog(const AMsg: string);
var
  S: string;
begin
  if not Assigned(FHandle) then
    Exit;
  S := Concat(AMsg, sLineBreak);
  FHandle.Seek(0, TSeekOrigin.soEnd);
  FHandle.WriteBuffer(S[1], Length(S));
end;

class function TBrookLoggerOutputFile.GetName: string;
begin
  Result := 'File';
end;

procedure TBrookLoggerOutputFile.Log(const ALevel, AMessage: string);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatLog(ALevel, AMessage));
end;

procedure TBrookLoggerOutputFile.Fail(const ALevel: string;
  AException: Exception);
begin
  if IsFiltered(ALevel) then
    Exit;
  UpgradeFile;
  WriteLog(FormatFail(ALevel, AException));
end;

{ TBrookLoggerLevels }

procedure TBrookLoggerLevels.Assign(ASource: TPersistent);
var
  VSrc: TBrookLoggerLevels;
begin
  if ASource is TBrookLoggerLevels then
  begin
    VSrc := ASource as TBrookLoggerLevels;
    FInfo := VSrc.Info;
    FHint := VSrc.Hint;
    FWarn := VSrc.Warn;
    FDebug := VSrc.Debug;
    FError := VSrc.Error;
  end
  else
    inherited Assign(ASource);
end;

{ TBrookLogger }

constructor TBrookLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilters := CreateFilters;
  FOptions := CreateOptions;
  FLevels := CreateLevels;
  FLevels.Info := SBrookLevelInfo;
  FLevels.Hint := SBrookLevelHint;
  FLevels.Warn := SBrookLevelWarn;
  FLevels.Debug := SBrookLevelDebug;
  FLevels.Error := SBrookLevelError;
  FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

destructor TBrookLogger.Destroy;
begin
  SetActive(False);
  FLevels.Free;
  FOptions.Free;
  FFilters.Free;
  inherited Destroy;
end;

function TBrookLogger.CreateFilters: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Add(SBrookLevelInfo);
  Result.Add(SBrookLevelHint);
  Result.Add(SBrookLevelDebug);
end;

function TBrookLogger.CreateOptions: TStringList;
begin
  Result := TStringList.Create;
end;

function TBrookLogger.GetOutputClass: TBrookLoggerOutputClass;
var
  C: TPersistentClass;
  N: string;
begin
  N := Concat(BROOK_LOGGER_TAG, FOutputName);
  C := Classes.GetClass(N);
  if Assigned(C) and (not C.InheritsFrom(TBrookLoggerOutput)) then
    raise EInvalidCast.CreateFmt(SBrookInvalidOutputClass, [C.ClassName]);
  Result := TBrookLoggerOutputClass(C);
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt(SBrookUnknownOutputName, [FOutputName]);
end;

function TBrookLogger.CreateOutput(AFilters,
  AOptions: TStringList): TBrookLoggerOutput;
begin
  Result := GetOutputClass.Create(AFilters, AOptions);
end;

function TBrookLogger.CreateLevels: TBrookLoggerLevels;
begin
  Result := TBrookLoggerLevels.Create;
end;

procedure TBrookLogger.CheckOutputName;
begin
  if FOutputName.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyOutputName);
end;

procedure TBrookLogger.CheckActive;
begin
  if not Active then
    raise EInvalidOpException.Create(SBrookInactiveOutput);
end;

procedure TBrookLogger.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveOutput);
end;

procedure TBrookLogger.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

procedure TBrookLogger.DoOpen;
begin
  if Assigned(FOutput) then
    Exit;
  CheckOutputName;
  FOutput := CreateOutput(FFilters, FOptions);
  FActive := True;
end;

procedure TBrookLogger.DoClose;
begin
  if not Assigned(FOutput) then
    Exit;
  FOutput.Destroy;
  FOutput := nil;
  FActive := False;
end;

function TBrookLogger.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookLogger.GetOutput: TBrookLoggerOutput;
begin
  CheckActive;
  Result := FOutput;
end;

function TBrookLogger.IsOutputNameStored: Boolean;
begin
  Result := FOutputName <> BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookLogger.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
    FActive := AValue
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookLogger.SetFilters(AValue: TStringList);
begin
  FFilters.Assign(AValue);
end;

procedure TBrookLogger.SetOptions(AValue: TStringList);
begin
  FOptions.Assign(AValue);
end;

procedure TBrookLogger.SetOutputName(const AValue: string);
begin
  if FOutputName = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FOutputName := AValue;
  if FOutputName.IsEmpty then
    FOutputName := BROOK_LOGGER_OUTPUT_NAME;
end;

procedure TBrookLogger.Open;
begin
  SetActive(True);
end;

procedure TBrookLogger.Close;
begin
  SetActive(False);
end;

procedure TBrookLogger.Log(const ALevel, AMessage: string);
begin
  if Active then
    Output.Log(ALevel, AMessage);
end;

procedure TBrookLogger.Fail(const ALevel: string; AException: Exception);
begin
  if Active then
    Output.Fail(ALevel, AException);
end;

procedure TBrookLogger.Info(const AMessage: string);
begin
  Log(FLevels.Info, AMessage);
end;

procedure TBrookLogger.Hint(const AMessage: string);
begin
  Log(FLevels.Hint, AMessage);
end;

procedure TBrookLogger.Warn(const AMessage: string);
begin
  Log(FLevels.Warn, AMessage);
end;

procedure TBrookLogger.Debug(const AMessage: string);
begin
  Log(FLevels.Debug, AMessage);
end;

procedure TBrookLogger.Error(AException: Exception);
begin
  Fail(FLevels.Error, AException);
end;

{$IFNDEF FPC}

procedure TBrookLogger.Trace(AProc: TBrookLoggerTraceProc; const AStartingMsg,
  AFinishingMsg: string);
begin
  if not Assigned(AProc) then
    Exit;
  try
    Info(AStartingMsg);
    AProc;
    Info(AFinishingMsg);
  except
    on E: Exception do
      Error(E);
  end;
end;

procedure TBrookLogger.Trace(AProc: TBrookLoggerTraceProc);
begin
  if not Assigned(AProc) then
    Exit;
  try
    AProc;
  except
    on E: Exception do
      Error(E);
  end;
end;

function TBrookLogger.Trace<T>(AFunc: TBrookLoggerTraceFunc<T>): T;
begin
  if not Assigned(AFunc) then
    Exit;
  try
    Result := AFunc;
  except
    on E: Exception do
      Error(E);
  end;
end;

function TBrookLogger.Trace<T>(AFunc: TBrookLoggerTraceFunc<T>;
  const AStartingMsg, AFinishingMsg: string): T;
begin
  if not Assigned(AFunc) then
    Exit;
  try
    Info(AStartingMsg);
    Result := AFunc;
    Info(AFinishingMsg);
  except
    on E: Exception do
      Error(E);
  end;
end;

{$ENDIF}

initialization
  RegisterClassAlias(TBrookLoggerOutputConsole,
    TBrookLoggerOutputConsole.GetRegisterAlias);
  RegisterClassAlias(TBrookLoggerOutputFile,
    TBrookLoggerOutputFile.GetRegisterAlias);

finalization
  UnregisterClass(TBrookLoggerOutputConsole);
  UnregisterClass(TBrookLoggerOutputFile);

end.

unit Gemini.Files;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  System.Net.URLClient, Gemini.API.Params, Gemini.API, Gemini.Async.Support;

type
  TStateType = (
    /// <summary>
    /// The default value. This value is used if the state is omitted.
    /// </summary>
    STATE_UNSPECIFIED,
    /// <summary>
    /// File is being processed and cannot be used for inference yet.
    /// </summary>
    PROCESSING,
    /// <summary>
    /// File is processed and available for inference.
    /// </summary>
    ACTIVE,
    /// <summary>
    /// File failed processing.
    /// </summary>
    FAILED
  );

  TStateTypeHelper = record helper for TStateType
    function ToString: string;
    class function Create(const Value: string): TStateType; static;
  end;

  TStateTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TFileParams = class(TJSONParam)
  public
    function DisplayName(const Value: string): TFileParams;
  end;

  TFileError = class
  private
    FCode: Int64;
    FMessage: string;
    FDetails: TArray<string>;
  public
    property Code: Int64 read FCode write FCode;
    property Message: string read FMessage write FMessage;
    property Details: TArray<string> read FDetails write FDetails;
  end;

  TFileContent = class
  private
    FName: string;
    FDisplayName: string;
    FMimeType: string;
    FSizeBytes: string;
    FCreateTime: string;
    FUpdateTime: string;
    FExpirationTime: string;
    FSha256Hash: string;
    FUri: string;
    [JsonReflectAttribute(ctString, rtString, TStateTypeInterceptor)]
    FState: TStateType;
    FError: TFileError;
  public
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property MimeType: string read FMimeType write FMimeType;
    property SizeBytes: string read FSizeBytes write FSizeBytes;
    property CreateTime: string read FCreateTime write FCreateTime;
    property UpdateTime: string read FUpdateTime write FUpdateTime;
    property ExpirationTime: string read FExpirationTime write FExpirationTime;
    property Sha256Hash: string read FSha256Hash write FSha256Hash;
    property Uri: string read FUri write FUri;
    property State: TStateType read FState write FState;
    property Error: TFileError read FError write FError;
    destructor Destroy; override;
  end;

  TFile = class
  private
    FFile: TFileContent;
  public
    property &File: TFileContent read FFile write FFile;
    destructor Destroy; override;
  end;

  TFiles = class
  private
    FFiles: TArray<TFileContent>;
    FNextPageToken: string;
  public
    property Files: TArray<TFileContent> read FFiles write FFiles;
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    destructor Destroy; override;
  end;

  TFileDelete = class
  end;

  TFilesRes = class(TGeminiAPIRoute)
  strict private
    function GetUriHeaders(const FilePath: string): TNetHeaders;
    function GetUploadHeaders(const FilePath: string): TNetHeaders;
    function ExtractUriFromHeaders(const FilePath: string; const ParamProc: TProc<TFileParams>): string;
    function TryToObtainUriValue(const FilePath: string; const DisplayName: string = ''): string;
  protected
    function UploadRaw(const FilePath: string; const DisplayName: string = ''): TFile;
  end;

  TAsynFile = TAsynCallBack<TFile>;

  TAsynFiles = TAsynCallBack<TFiles>;

  TAsynFileDelete = TAsynCallBack<TFileDelete>;

  TAsynFileContent = TAsynCallBack<TFileContent>;

  TFilesRoute = class(TFilesRes)
  public
    procedure AsynUpload(const FilePath: string; const DisplayName: string;
      CallBacks: TFunc<TAsynFile>); overload;
    procedure AsynUpload(const FilePath: string; CallBacks: TFunc<TAsynFile>); overload;
    procedure AsynList(CallBacks: TFunc<TAsynFiles>); overload;
    procedure AsynList(const PageSize: Integer; const PageToken: string;
      CallBacks: TFunc<TAsynFiles>); overload;
    procedure AsynDelete(const FileName: string; CallBacks: TFunc<TAsynFileDelete>);
    procedure AsynRetrieve(const FileName: string; CallBacks: TFunc<TAsynFileContent>);

    function Upload(const FilePath: string; const DisplayName: string = ''): TFile;
    function List: TFiles; overload;
    function List(const PageSize: Integer; const PageToken: string): TFiles; overload;
    function Delete(const FileName: string): TFileDelete;
    function Retrieve(const FileName: string): TFileContent;
  end;

implementation

uses
  System.StrUtils, Gemini.NetEncoding.Base64, System.Rtti, Rest.Json;

{ TFilesRoute }

function TFilesRoute.List: TFiles;
begin
  Result := API.Get<TFiles>('files');
end;

procedure TFilesRoute.AsynList(CallBacks: TFunc<TAsynFiles>);
begin
  with TAsynCallBackExec<TAsynFiles, TFiles>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFiles
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsynDelete(const FileName: string;
  CallBacks: TFunc<TAsynFileDelete>);
begin
  with TAsynCallBackExec<TAsynFileDelete, TFileDelete>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileDelete
      begin
        Result := Self.Delete(FileName);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsynList(const PageSize: Integer; const PageToken: string;
  CallBacks: TFunc<TAsynFiles>);
begin
  with TAsynCallBackExec<TAsynFiles, TFiles>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFiles
      begin
        Result := Self.List(PageSize, PageToken);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsynRetrieve(const FileName: string;
  CallBacks: TFunc<TAsynFileContent>);
begin
  with TAsynCallBackExec<TAsynFileContent, TFileContent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileContent
      begin
        Result := Self.Retrieve(FileName);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsynUpload(const FilePath: string;
  CallBacks: TFunc<TAsynFile>);
begin
  AsynUpload(FilePath, EmptyStr, CallBacks);
end;

procedure TFilesRoute.AsynUpload(const FilePath, DisplayName: string;
  CallBacks: TFunc<TAsynFile>);
begin
  with TAsynCallBackExec<TAsynFile, TFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFile
      begin
        Result := Self.Upload(FilePath, DisplayName);
      end);
  finally
    Free;
  end;
end;

function TFilesRoute.Delete(const FileName: string): TFileDelete;
begin
  Result := API.Delete<TFileDelete>(CheckFileName(FileName));
end;

function TFilesRoute.List(const PageSize: Integer;
  const PageToken: string): TFiles;
begin
  Result := API.Get<TFiles>('files', ParamsBuilder(PageSize, PageToken));
end;

function TFilesRoute.Retrieve(const FileName: string): TFileContent;
begin
  Result := API.Get<TFileContent>(CheckFileName(FileName));
end;

function TFilesRoute.Upload(const FilePath, DisplayName: string): TFile;
begin
  Result := UploadRaw(FilePath, DisplayName);
end;

{ TFileParams }

function TFileParams.DisplayName(const Value: string): TFileParams;
begin
  Result := TFileParams(Add('file', TJSONObject.Create.AddPair('display_name', Value)));
end;

{ TStateTypeHelper }

class function TStateTypeHelper.Create(const Value: string): TStateType;
begin
  var index := IndexStr(AnsiUpperCase(Value), [
        'STATE_UNSPECIFIED', 'PROCESSING', 'ACTIVE', 'FAILED']);
  if index = -1 then
    raise Exception.CreateFmt('"TStateType" unknown : %s', [Value]);
  Result := TStateType(index);
end;

function TStateTypeHelper.ToString: string;
begin
  case Self of
    STATE_UNSPECIFIED:
      Exit('STATE_UNSPECIFIED');
    PROCESSING:
      Exit('PROCESSING');
    ACTIVE:
      Exit('ACTIVE');
    FAILED:
      Exit('FAILED');
  end;
end;

{ TStateTypeInterceptor }

function TStateTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TStateType>.ToString;
end;

procedure TStateTypeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TStateType.Create(Arg)));
end;

{ TFilesRes }

function TFilesRes.ExtractUriFromHeaders(const FilePath: string;
  const ParamProc: TProc<TFileParams>): string;
begin
  API.CustomHeaders := GetUriHeaders(FilePath);
  try
    var Path := 'upload/v1beta/files';
    var Name := 'x-goog-upload-url';
    Result := API.Find<TFileParams>(Path, Name, ParamProc).Replace(#10, '').Replace(#13, '');
  finally
    API.CustomHeaders := [];
  end;
end;

function TFilesRes.GetUploadHeaders(const FilePath: string): TNetHeaders;
begin
  Result :=
    [TNetHeader.Create('Content-Length', FileSize(FilePath).ToString)] +
    [TNetHeader.Create('X-Goog-Upload-Offset', 0.ToString)] +
    [TNetHeader.Create('X-Goog-Upload-Command', 'upload, finalize')];
end;

function TFilesRes.GetUriHeaders(const FilePath: string): TNetHeaders;
begin
  Result :=
    [TNetHeader.Create('X-Goog-Upload-Protocol', 'resumable')] +
    [TNetHeader.Create('X-Goog-Upload-Command', 'start')] +
    [TNetHeader.Create('X-Goog-Upload-Header-Content-Length', FileSize(FilePath).ToString)] +
    [TNetHeader.Create('X-Goog-Upload-Header-Content-Type', ResolveMimeType(FilePath))];
end;

function TFilesRes.TryToObtainUriValue(const FilePath,
  DisplayName: string): string;
begin
  if Trim(DisplayName).IsEmpty then
    Result := ExtractUriFromHeaders(FilePath, nil)
  else
    Result := ExtractUriFromHeaders(FilePath,
      procedure (Params: TFileParams)
      begin
        Params.DisplayName(DisplayName);
      end);
end;

function TFilesRes.UploadRaw(const FilePath, DisplayName: string): TFile;
begin
  API.CustomHeaders := [];

  {--- Try to obtain an URI }
  var URI := TryToObtainUriValue(FilePath, DisplayName);
  if URI.IsEmpty then
    raise Exception.Create('The server has not provided a URI.');

  {--- Upload the file using a URI }
  API.CustomHeaders := GetUploadHeaders(FilePath);
  try
    Result := API.UploadRaw<TFile>(URI, FilePath);
  finally
    API.CustomHeaders := [];
  end;
end;

{ TFile }

destructor TFile.Destroy;
begin
  if Assigned(FFile) then
    FFile.Free;
  inherited;
end;

{ TFileContent }

destructor TFileContent.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

{ TFiles }

destructor TFiles.Destroy;
begin
  for var Item in FFiles do
    Item.Free;
  inherited;
end;

end.

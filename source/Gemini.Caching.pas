unit Gemini.Caching;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON, Gemini.API.Params, Gemini.API,
  Gemini.Tools, Gemini.Async.Support, Gemini.Chat;

type
  TCacheParams = class(TJSONParam)
  public
    function Contents(const Value: TArray<TContentPayload>): TCacheParams;
    function Tools(const Value: TArray<TToolPluginParams>): TCacheParams;
    function ToolConfig(const Value: TToolMode; AllowedFunctionNames: TArray<string> = []): TCacheParams;
    function ExpireTime(const Value: string): TCacheParams;
    function ttl(const Value: string): TCacheParams;
    function Name(const Value: string): TCacheParams;
    function DisplayName(const Value: string): TCacheParams;
    function Model(const Value: string): TCacheParams;
    function SystemInstruction(const Value: string): TCacheParams;
    class function New(ParamProc: TProcRef<TCacheParams>): TCacheParams; static;
  end;

  TCacheUpdateParams = class(TJSONParam)
  public
    function ExpireTime(const Value: string): TCacheParams;
    function ttl(const Value: string): TCacheParams;
    function Name(const Value: string): TCacheParams;
  end;

  TContents = TChatContent;

  TSystemInstruction = TChatContent;

  TCache = class
  private
    FContents: TArray<TContents>;
    FTools: TArray<TTool>;
    FCreateTime: string;
    FUpdateTime: string;
    FUsageMetadata: TUsageMetadata;
    FExpireTime: string;
    FTTL: string;
    FName: string;
    FDisplayName: string;
    FModel: string;
    FSystemInstruction: TSystemInstruction;
    FToolConfig: TToolConfig;
  public
    property Contents: TArray<TContents> read FContents write FContents;
    property Tools: TArray<TTool> read FTools write FTools;
    property CreateTime: string read FCreateTime write FCreateTime;
    property UpdateTime: string read FUpdateTime write FUpdateTime;
    property UsageMetadata: TUsageMetadata read FUsageMetadata write FUsageMetadata;
    property ExpireTime: string read FExpireTime write FExpireTime;
    property TTL: string read FTTL write FTTL;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Model: string read FModel write FModel;
    property SystemInstruction: TSystemInstruction read FSystemInstruction write FSystemInstruction;
    property ToolConfig: TToolConfig read FToolConfig write FToolConfig;
    destructor Destroy; override;
  end;

  TCacheContents = class
  private
    FCachedContents: TArray<TCache>;
    FNextPageToken: string;
  public
    property CachedContents: TArray<TCache> read FCachedContents write FCachedContents;
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    destructor Destroy; override;
  end;

  TCacheDelete = class
  end;

  TAsynCache = TAsynCallBack<TCache>;

  TAsynCacheContents = TAsynCallBack<TCacheContents>;

  TAsynCacheDelete = TAsynCallBack<TCacheDelete>;

  TCachingRoute = class(TGeminiAPIRoute)
    procedure ASynCreate(ParamProc: TProc<TCacheParams>; CallBacks: TFunc<TAsynCache>); overload;
    procedure ASynCreate(const Value: TJSONObject; CallBacks: TFunc<TAsynCache>); overload;
    procedure ASynList(const PageSize: Integer; const PageToken: string;
      CallBacks: TFunc<TAsynCacheContents>);
    procedure ASynRetrieve(const CacheName: string; CallBacks: TFunc<TAsynCache>);
    procedure ASynUpdate(const CacheName: string; ParamProc: TProc<TCacheUpdateParams>;
      CallBacks: TFunc<TAsynCache>); overload;
    procedure ASynUpdate(const CacheName: string; const ttl: string;
      CallBacks: TFunc<TAsynCache>); overload;
    procedure ASynDelete(const CacheName: string; CallBacks: TFunc<TAsynCacheDelete>);

    function Create(ParamProc: TProc<TCacheParams>): TCache; overload;
    function Create(const Value: TJSONObject): TCache; overload;
    function List(const PageSize: Integer; const PageToken: string): TCacheContents; overload;
    function Retrieve(const CacheName: string): TCache;
    function Update(const CacheName: string; ParamProc: TProc<TCacheUpdateParams>): TCache; overload;
    function Update(const CacheName: string; Value: TJSONObject): TCache; overload;
    function Update(const CacheName: string; const ttl: string): TCache; overload;
    function Delete(const CacheName: string): TCacheDelete;
  end;

implementation

{ TCacheParams }

function TCacheParams.Contents(
  const Value: TArray<TContentPayload>): TCacheParams;
begin
  var JSONContents := TJSONArray.Create;
  for var Item in Value do
    JSONContents.Add(Item.Detach);
  Result := TCacheParams(Add('contents', JSONContents));
end;

function TCacheParams.DisplayName(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('displayName', Value));
end;

function TCacheParams.ExpireTime(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('expireTime', Value));
end;

function TCacheParams.Model(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('model', Value));
end;

function TCacheParams.Name(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('name', Value));
end;

class function TCacheParams.New(ParamProc: TProcRef<TCacheParams>): TCacheParams;
begin
  Result := TCacheParams.Create;
  if Assigned(ParamProc) then
    ParamProc(Result);
end;

function TCacheParams.SystemInstruction(const Value: string): TCacheParams;
begin
  var PartsJSON := TJSONObject.Create.AddPair('parts', TJSONObject.Create.AddPair('text', Value));
  Result := TCacheParams(Add('systemInstruction', PartsJSON));
end;

function TCacheParams.ToolConfig(const Value: TToolMode;
  AllowedFunctionNames: TArray<string>): TCacheParams;
begin
  var Temp := TJSONObject.Create.AddPair('mode', Value.ToString);
  if Length(AllowedFunctionNames) > 0 then
    begin
      var JSONArray := TJSONArray.Create;
      for var Item in AllowedFunctionNames do
        begin
          JSONArray.Add(Item);
        end;
      Temp.AddPair('allowedFunctionNames', JSONArray);
    end;

  Result := TCacheParams(Add('toolConfig',
              TJSONObject.Create.AddPair('function_calling_config', Temp)));
end;

function TCacheParams.Tools(
  const Value: TArray<TToolPluginParams>): TCacheParams;
begin
  var JSONFuncs := TJSONArray.Create;
  for var Item in value do
    begin
      JSONFuncs.Add(Item.ToJson);
    end;
  var JSONDeclaration := TJSONObject.Create.AddPair('function_declarations', JSONFuncs);

  var JSONTool := TJSONArray.Create.Add(JSONDeclaration);

  Result := TCacheParams(Add('tools', JSONTool));
end;

function TCacheParams.ttl(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('ttl', Value));
end;

{ TCache }

destructor TCache.Destroy;
begin
  for var Item in FContents do
    Item.Free;
  for var Item in FTools do
    Item.Free;
  if Assigned(FUsageMetadata) then
    FUsageMetadata.Free;
  if Assigned(FSystemInstruction) then
    FSystemInstruction.Free;
  if Assigned(FToolConfig) then
    FToolConfig.Free;
  inherited;
end;

{ TCachingRoute }

procedure TCachingRoute.ASynCreate(ParamProc: TProc<TCacheParams>;
  CallBacks: TFunc<TAsynCache>);
begin
  with TAsynCallBackExec<TAsynCache, TCache>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCache
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynCreate(const Value: TJSONObject;
  CallBacks: TFunc<TAsynCache>);
begin
  with TAsynCallBackExec<TAsynCache, TCache>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCache
      begin
        Result := Self.Create(Value);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynDelete(const CacheName: string;
  CallBacks: TFunc<TAsynCacheDelete>);
begin
  with TAsynCallBackExec<TAsynCacheDelete, TCacheDelete>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCacheDelete
      begin
        Result := Self.Delete(CacheName);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynList(const PageSize: Integer;
  const PageToken: string; CallBacks: TFunc<TAsynCacheContents>);
begin
  with TAsynCallBackExec<TAsynCacheContents, TCacheContents>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCacheContents
      begin
        Result := Self.List(PageSize, PageToken);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynRetrieve(const CacheName: string;
  CallBacks: TFunc<TAsynCache>);
begin
  with TAsynCallBackExec<TAsynCache, TCache>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCache
      begin
        Result := Self.Retrieve(CacheName);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynUpdate(const CacheName, ttl: string;
  CallBacks: TFunc<TAsynCache>);
begin
  with TAsynCallBackExec<TAsynCache, TCache>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCache
      begin
        Result := Self.Update(CacheName, ttl);
      end);
  finally
    Free;
  end;
end;

procedure TCachingRoute.ASynUpdate(const CacheName: string;
  ParamProc: TProc<TCacheUpdateParams>; CallBacks: TFunc<TAsynCache>);
begin
  with TAsynCallBackExec<TAsynCache, TCache>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TCache
      begin
        Result := Self.Update(CacheName, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TCachingRoute.Create(const Value: TJSONObject): TCache;
begin
  try
    Result := API.Post<TCache>('cachedContents', Value);
  finally
    Value.Free;
  end;
end;

function TCachingRoute.Create(ParamProc: TProc<TCacheParams>): TCache;
begin
  Result := API.Post<TCache, TCacheParams>('cachedContents', ParamProc);
end;

function TCachingRoute.Delete(const CacheName: string): TCacheDelete;
begin
  Result := API.Delete<TCacheDelete>(CacheName);
end;

function TCachingRoute.List(const PageSize: Integer;
  const PageToken: string): TCacheContents;
begin
  Result := API.Get<TCacheContents>('cachedContents', ParamsBuilder(PageSize, PageToken));
end;

function TCachingRoute.Retrieve(const CacheName: string): TCache;
begin
  Result := API.Get<TCache>(CacheName);
end;

function TCachingRoute.Update(const CacheName, ttl: string): TCache;
begin
  var Cache := TCacheUpdateParams.Create.ttl(ttl);
  try
    Result := Update(CacheName, Cache.JSON)
  finally
    Cache.Free;
  end;
end;

function TCachingRoute.Update(const CacheName: string;
  Value: TJSONObject): TCache;
begin
  Result := API.Patch<TCache>(CacheName, Value);
end;

function TCachingRoute.Update(const CacheName: string;
  ParamProc: TProc<TCacheUpdateParams>): TCache;
begin
  Result := API.Patch<TCache, TCacheUpdateParams>(CacheName, ParamProc);
end;

{ TCacheContents }

destructor TCacheContents.Destroy;
begin
  for var Item in FCachedContents do
    Item.Free;
  inherited;
end;

{ TCacheUpdateParams }

function TCacheUpdateParams.ExpireTime(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('expireTime', Value));
end;

function TCacheUpdateParams.Name(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('name', Value));
end;

function TCacheUpdateParams.ttl(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('ttl', Value));
end;

end.

unit Gemini.Caching;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Gemini.API.Params, Gemini.API, Gemini.Safety, Gemini.Schema,
  Gemini.Tools, Gemini.Async.Support, Gemini.Chat;

type
  TCacheParams = class(TJSONParam)
  public
    function Contents(const Value: TArray<TContentPayload>): TCacheParams;
    function Tools(const Value: TArray<TToolPluginParams>): TCacheParams;
    function ToolConfig(const Value: TToolMode; AllowedFunctionNames: TArray<string> = []): TCacheParams;
    function ExpireTime(const Value: string): TCacheParams;
    function Ttl(const Value: string): TCacheParams;
    function Name(const Value: string): TCacheParams;
    function DisplayName(const Value: string): TCacheParams;
    function Model(const Value: string): TCacheParams;
    function SystemInstruction(const Value: string): TCacheParams;
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

  TCachingRoute = class(TGeminiAPIRoute)
    function Create(ParamProc: TProc<TCacheParams>): TCache;
    function List(const PageSize: Integer; const PageToken: string): TCacheContents; overload;
    function Retrieve(const CacheName: string): TCache;
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

function TCacheParams.Ttl(const Value: string): TCacheParams;
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

function TCachingRoute.Create(ParamProc: TProc<TCacheParams>): TCache;
begin
  Result := API.Post<TCache, TCacheParams>('cachedContents', ParamProc);
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

{ TCacheContents }

destructor TCacheContents.Destroy;
begin
  for var Item in FCachedContents do
    Item.Free;
  inherited;
end;

end.

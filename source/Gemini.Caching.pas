unit Gemini.Caching;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON, Gemini.API.Params, Gemini.API,
  Gemini.Tools, Gemini.Async.Support, Gemini.Chat, Gemini.Functions.Core;

type
  /// <summary>
  /// Represents parameters for configuring cached content in the caching API.
  /// </summary>
  /// <remarks>
  /// Use this class to set various parameters when creating or updating cached content.
  /// Supports method chaining for setting multiple parameters in a fluent style.
  /// </remarks>
  TCacheParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the content of the current conversation with the model.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TContentPayload</c> instances representing the messages exchanged in the conversation, including both user and assistant messages.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// For single-turn queries, this array contains a single message. For multi-turn conversations, include the entire conversation history and the latest message.
    /// </remarks>
    function Contents(const Value: TArray<TContentPayload>): TCacheParams;
    /// <summary>
    /// Specifies a list of tools that the model may use to generate the next response.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>IFunctionCore</c> instances representing the tools available to the model.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// A tool is a piece of code that allows the model to interact with external systems or perform actions outside its knowledge base.
    /// Supported tools include functions and code execution capabilities. Refer to the Function Calling and Code Execution guides for more information.
    /// </remarks>
    function Tools(const Value: TArray<IFunctionCore>): TCacheParams; overload;
    /// <summary>
    /// Specifies whether code execution is available as a tool for the model.
    /// </summary>
    /// <param name="CodeExecution">
    /// A boolean value where <c>True</c> enables code execution as a tool, and <c>False</c> disables it.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// When enabled, the model can generate code that can be executed to perform tasks.
    /// </remarks>
    function Tools(const CodeExecution: Boolean): TCacheParams; overload;
    /// <summary>
    /// Configures the tool settings for any tools specified in the request.
    /// </summary>
    /// <param name="Value">
    /// A <c>TToolMode</c> value specifying the mode in which tools are used by the model.
    /// </param>
    /// <param name="AllowedFunctionNames">
    /// Optional. An array of strings representing the names of functions that the model is allowed to use.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify how the model should use the available tools, including restricting which functions can be called.
    /// Refer to the Function Calling guide for usage examples.
    /// <para>
    /// - This config is shared for all tools.
    /// </para>
    /// </remarks>
    function ToolConfig(const Value: TToolMode; AllowedFunctionNames: TArray<string> = []): TCacheParams;
    /// <summary>
    /// Input only. New TTL for this resource, input only.
    /// </summary>
    /// <param name="Value">
    /// A duration in seconds with up to nine fractional digits, ending with 's'. Example: "3.5s".
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// The JSON representation for Duration is a String that ends in s to indicate seconds and is preceded by the number of seconds, with nanoseconds expressed as fractional seconds.
    /// </remarks>
    function ttl(const Value: string): TCacheParams;
    /// <summary>
    /// Optional. Identifier. The resource name referring to the cached content.
    /// </summary>
    /// <param name="Value">
    /// Format: cachedContents/{id}
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    function Name(const Value: string): TCacheParams;
    /// <summary>
    /// Optional. Immutable. The user-generated meaningful display name of the cached content.
    /// </summary>
    /// <param name="Value">
    /// Maximum 128 Unicode characters.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    function DisplayName(const Value: string): TCacheParams;
    /// <summary>
    /// Required. Immutable. The name of the Model to use for cached content
    /// </summary>
    /// <param name="Value">
    /// Format: models/{model}
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    function Model(const Value: string): TCacheParams;
    /// <summary>
    /// Optional. Input only. Immutable. Developer set system instruction.
    /// </summary>
    /// <param name="Value">
    /// Currently text only.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheParams</c> instance, allowing for method chaining.
    /// </returns>
    function SystemInstruction(const Value: string): TCacheParams;
    class function New(ParamProc: TProcRef<TCacheParams>): TCacheParams; static;
  end;

  /// <summary>
  /// Updates CachedContent resource.
  /// </summary>
  /// <remarks>
  /// Only expiration is updatable
  /// </remarks>
  TCacheUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Input only. New TTL for this resource, input only.
    /// </summary>
    /// <param name="Value">
    /// A duration in seconds with up to nine fractional digits, ending with 's'. Example: "3.5s".
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheUpdateParams</c> instance, allowing for method chaining.
    /// </returns>
    function ttl(const Value: string): TCacheParams;
    /// <summary>
    /// Optional. Identifier. The resource name referring to the cached content.
    /// </summary>
    /// <param name="Value">
    /// Format: cachedContents/{id}
    /// </param>
    /// <returns>
    /// Returns the updated <c>TCacheUpdateParams</c> instance, allowing for method chaining.
    /// </returns>
    function Name(const Value: string): TCacheParams;
  end;

  /// <summary>
  /// Content that has been preprocessed and can be used in subsequent request to GenerativeService.
  /// </summary>
  /// <remarks>
  /// Cached content can be only used with model it was created for.
  /// </remarks>
  TCache = class
  private
    FCreateTime: string;
    FUpdateTime: string;
    FUsageMetadata: TUsageMetadata;
    FExpireTime: string;
    FName: string;
    FDisplayName: string;
    FModel: string;
  public
    /// <summary>
    /// Creation time of the cache entry.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property CreateTime: string read FCreateTime write FCreateTime;
    /// <summary>
    /// When the cache entry was last updated in UTC time.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property UpdateTime: string read FUpdateTime write FUpdateTime;
    /// <summary>
    /// Metadata on the usage of the cached content.
    /// </summary>
    property UsageMetadata: TUsageMetadata read FUsageMetadata write FUsageMetadata;
    /// <summary>
    /// Timestamp in UTC of when this resource is considered expired. This is always provided on output, regardless of what was sent on input.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property ExpireTime: string read FExpireTime write FExpireTime;
    /// <summary>
    /// Identifier. The resource name referring to the cached content.
    /// </summary>
    /// <remarks>
    /// Format: cachedContents/{id}
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// Optional. Immutable. The user-generated meaningful display name of the cached content.
    /// </summary>
    /// <remarks>
    /// Maximum 128 Unicode characters.
    /// </remarks>
    property DisplayName: string read FDisplayName write FDisplayName;
    /// <summary>
    /// Required. Immutable. The name of the Model to use for cached content.
    /// </summary>
    /// <remarks>
    /// Format: models/{model}
    /// </remarks>
    property Model: string read FModel write FModel;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Lists CachedContents.
  /// </summary>
  TCacheContents = class
  private
    FCachedContents: TArray<TCache>;
    FNextPageToken: string;
  public
    /// <summary>
    /// List of cached contents.
    /// </summary>
    property CachedContents: TArray<TCache> read FCachedContents write FCachedContents;
    /// <summary>
    /// A token, which can be sent as pageToken to retrieve the next page.
    /// </summary>
    /// <remarks>
    /// If this field is omitted, there are no subsequent pages.
    /// </remarks>
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Class defined for compatibility with asynchrony handling.
  /// </summary>
  TCacheDelete = class
  end;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TCache</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCache</c> type extends the <c>TAsynParams&lt;TCache&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynCache = TAsynCallBack<TCache>;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TCacheContents</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCacheContents</c> type extends the <c>TAsynParams&lt;TCacheContents&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynCacheContents = TAsynCallBack<TCacheContents>;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TCacheDelete</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynCacheDelete</c> type extends the <c>TAsynParams&lt;TCacheDelete&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynCacheDelete = TAsynCallBack<TCacheDelete>;

  /// <summary>
  /// Provides methods to interact with the caching API for creating, retrieving, updating, listing, and deleting cached content both asynchronously and synchronously.
  /// </summary>
  TCachingRoute = class(TGeminiAPIRoute)
    /// <summary>
    /// Asynchronously creates a new cached content using specified parameters.
    /// </summary>
    /// <param name="ParamProc">A procedure to configure the cache parameters.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// This method initiates an asynchronous operation to create cached content.
    /// The provided callbacks will be invoked to handle success, error, and progress updates.
    /// </remarks>
    procedure ASynCreate(ParamProc: TProc<TCacheParams>; CallBacks: TFunc<TAsynCache>); overload;
    /// <summary>
    /// Asynchronously creates a new cached content from a JSON object.
    /// </summary>
    /// <param name="Value">A JSON object containing the cache parameters.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// Use this method when you have the cache parameters prepared in a JSON format.
    /// The method takes ownership of the provided JSON object and will free it after use.
    /// </remarks>
    procedure ASynCreate(const Value: TJSONObject; CallBacks: TFunc<TAsynCache>); overload;
    /// <summary>
    /// Asynchronously lists cached contents with pagination support.
    /// </summary>
    /// <param name="PageSize">The maximum number of cached contents to return per page.</param>
    /// <param name="PageToken">The token for the page of results to retrieve.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// If <c>PageToken</c> is empty, the first page is returned.
    /// The provided callbacks will be invoked as the operation progresses.
    /// </remarks>
    procedure ASynList(const PageSize: Integer; const PageToken: string;
      CallBacks: TFunc<TAsynCacheContents>);
    /// <summary>
    /// Asynchronously retrieves a cached content by its name.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to retrieve, in the format 'cachedContents/{id}'.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    procedure ASynRetrieve(const CacheName: string; CallBacks: TFunc<TAsynCache>);
    /// <summary>
    /// Asynchronously updates a cached content using specified parameters.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to update.</param>
    /// <param name="ParamProc">A procedure to configure the update parameters.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// Only the TTL can be updated for a cached content.
    /// </remarks>
    procedure ASynUpdate(const CacheName: string; ParamProc: TProc<TCacheUpdateParams>;
      CallBacks: TFunc<TAsynCache>); overload;
    /// <summary>
    /// Asynchronously updates the TTL (Time To Live) of a cached content.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to update.</param>
    /// <param name="ttl">The new TTL value, specified as a duration string (e.g., '3600s' for one hour).</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// This overload simplifies updating the TTL without the need to specify other parameters.
    /// </remarks>
    procedure ASynUpdate(const CacheName: string; const ttl: string;
      CallBacks: TFunc<TAsynCache>); overload;
    /// <summary>
    /// Asynchronously deletes a cached content by its name.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to delete.</param>
    /// <param name="CallBacks">A function that returns asynchronous callbacks for handling the operation's lifecycle.</param>
    /// <remarks>
    /// Upon successful deletion, the callbacks will be invoked with the result.
    /// </remarks>
    procedure ASynDelete(const CacheName: string; CallBacks: TFunc<TAsynCacheDelete>);
    /// <summary>
    /// Synchronously creates a new cached content using specified parameters.
    /// </summary>
    /// <param name="ParamProc">A procedure to configure the cache parameters.</param>
    /// <returns>The created <c>TCache</c> object representing the cached content.</returns>
    /// <remarks>
    /// This method blocks until the cached content is created.
    /// </remarks>
    function Create(ParamProc: TProc<TCacheParams>): TCache; overload;
    /// <summary>
    /// Synchronously creates a new cached content from a JSON object.
    /// </summary>
    /// <param name="Value">A <c>TJSONObject</c> containing the cache parameters.</param>
    /// <returns>The created <c>TCache</c> object representing the cached content.</returns>
    /// <remarks>
    /// The method takes ownership of the provided JSON object and will free it after use.
    /// </remarks>
    function Create(const Value: TJSONObject): TCache; overload;
    /// <summary>
    /// Synchronously lists cached contents with pagination support.
    /// </summary>
    /// <param name="PageSize">The maximum number of cached contents to return per page.</param>
    /// <param name="PageToken">The token for the page of results to retrieve.</param>
    /// <returns>A <c>TCacheContents</c> object containing the list of cached contents and a next page token, if any.</returns>
    /// <remarks>
    /// If <c>PageToken</c> is empty, the first page is returned.
    /// </remarks>
    function List(const PageSize: Integer; const PageToken: string): TCacheContents; overload;
    /// <summary>
    /// Synchronously retrieves a cached content by its name.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to retrieve.</param>
    /// <returns>The retrieved <c>TCache</c> object representing the cached content.</returns>
    function Retrieve(const CacheName: string): TCache;
    /// <summary>
    /// Synchronously updates a cached content using specified parameters.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to update.</param>
    /// <param name="ParamProc">A procedure to configure the update parameters.</param>
    /// <returns>The updated <c>TCache</c> object representing the cached content.</returns>
    /// <remarks>
    /// Only the TTL can be updated for a cached content.
    /// </remarks>
    function Update(const CacheName: string; ParamProc: TProc<TCacheUpdateParams>): TCache; overload;
    /// <summary>
    /// Synchronously updates a cached content from a JSON object.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to update.</param>
    /// <param name="Value">A <c>TJSONObject</c> containing the update parameters.</param>
    /// <returns>The updated <c>TCache</c> object representing the cached content.</returns>
    /// <remarks>
    /// The method takes ownership of the provided JSON object and will free it after use.
    /// </remarks>
    function Update(const CacheName: string; Value: TJSONObject): TCache; overload;
    /// <summary>
    /// Synchronously updates the TTL (Time To Live) of a cached content.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to update.</param>
    /// <param name="ttl">The new TTL value, specified as a duration string (e.g., '3600s' for one hour).</param>
    /// <returns>The updated <c>TCache</c> object representing the cached content.</returns>
    /// <remarks>
    /// This overload simplifies updating the TTL without the need to specify other parameters.
    /// </remarks>
    function Update(const CacheName: string; const ttl: string): TCache; overload;
    /// <summary>
    /// Synchronously deletes a cached content by its name.
    /// </summary>
    /// <param name="CacheName">The unique name of the cached content to delete.</param>
    /// <returns>A <c>TCacheDelete</c> object indicating the result of the deletion.</returns>
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

function TCacheParams.Tools(const CodeExecution: Boolean): TCacheParams;
begin
  if not CodeExecution then
    Exit(Self);
  var JSONCodeExecution := TJSONObject.Create.AddPair('codeExecution', TJSONObject.Create);
  Result := TCacheParams(Add('tools', TJSONArray.Create.Add(JSONCodeExecution)));
end;

function TCacheParams.Tools(
  const Value: TArray<IFunctionCore>): TCacheParams;
begin
  var JSONFuncs := TJSONArray.Create;
  for var Item in value do
    begin
      JSONFuncs.Add(TToolPluginParams.Add(Item).ToJson);
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
  if Assigned(FUsageMetadata) then
    FUsageMetadata.Free;
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

function TCacheUpdateParams.Name(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('name', Value));
end;

function TCacheUpdateParams.ttl(const Value: string): TCacheParams;
begin
  Result := TCacheParams(Add('ttl', Value));
end;

end.

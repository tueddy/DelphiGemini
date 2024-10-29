unit Gemini.Embeddings;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Json.Types, System.JSON,
  Gemini.API.Params, Gemini.API, Gemini.Async.Params, Gemini.Async.Support;

type
  /// <summary>
  /// Type of task for which the embedding will be used.
  /// </summary>
  TTaskType = (
    /// <summary>
    /// Unset value, which will default to one of the other enum values.
    /// </summary>
    TASK_TYPE_UNSPECIFIED,
    /// <summary>
    /// Specifies the given text is a query in a search/retrieval setting.
    /// </summary>
    RETRIEVAL_QUERY,
    /// <summary>
    /// Specifies the given text is a document from the corpus being searched.
    /// </summary>
    RETRIEVAL_DOCUMENT,
    /// <summary>
    /// Specifies the given text will be used for STS.
    /// </summary>
    SEMANTIC_SIMILARITY,
    /// <summary>
    /// Specifies that the given text will be classified.
    /// </summary>
    CLASSIFICATION,
    /// <summary>
    /// Specifies that the embeddings will be used for clustering.
    /// </summary>
    CLUSTERING,
    /// <summary>
    /// Specifies that the given text will be used for question answering.
    /// </summary>
    QUESTION_ANSWERING,
    /// <summary>
    /// Specifies that the given text will be used for fact verification.
    /// </summary>
    FACT_VERIFICATION,
    /// <summary>
    /// Internal value
    /// </summary>
    NONE
  );

  /// <summary>
  /// Helper record for the <c>TTaskType</c> enumeration, providing utility methods for converting
  /// between <c>TTaskType</c> values and their string representations.
  /// </summary>
  TTaskTypeHelper = record helper for TTaskType
    /// Converts the current <c>TTaskType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TTaskType</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// Represents the parameters required to create an embedding request.
  /// </summary>
  /// <remarks>
  /// This record encapsulates all necessary information for generating embeddings, including the model to use,
  /// the content to embed, the type of task, an optional title, and the desired output dimensionality.
  /// </remarks>
  TEmbeddingRequestParams = record
  private
    FModel: string;
    FContent: TArray<string>;
    FTaskType: TTaskType;
    FTitle: string;
    FOutputDimensionality: Integer;
  public
    /// <summary>
    /// The content to embed.
    /// </summary>
    /// <remarks>
    /// Required. Only the parts.text fields will be counted.
    /// </remarks>
    function Content(const Value: TArray<string>): TEmbeddingRequestParams;
    /// <summary>
    /// Optional task type for which the embeddings will be used. Can only be set for models/embedding-001.
    /// </summary>
    /// <remarks>
    /// Optional
    /// </remarks>
    function TaskType(const Value: TTaskType): TEmbeddingRequestParams;
    /// <summary>
    /// An optional title for the text.
    /// </summary>
    /// <remarks>
    /// Optional. Only applicable when TaskType is RETRIEVAL_DOCUMENT.
    /// <para>
    /// - Note: Specifying a title for RETRIEVAL_DOCUMENT provides better quality embeddings for retrieval.
    /// </para>
    /// </remarks>
    function Title(const Value: string): TEmbeddingRequestParams;
    /// <summary>
    /// Optional reduced dimension for the output embedding.
    /// </summary>
    /// <remarks>
    /// If set, excessive values in the output embedding are truncated from the end.
    /// <para>
    /// - Supported by newer models since 2024 only.
    /// </para>
    /// </remarks>
    function OutputDimensionality(const Value: Integer): TEmbeddingRequestParams;
    /// <summary>
    /// Converts the embedding request parameters to a JSON object.
    /// </summary>
    /// <returns>
    /// A <c>TJSONObject</c> representing the embedding request parameters.
    /// </returns>
    function ToJson: TJSONObject;
    /// <summary>
    /// Creates a new instance of <c>TEmbeddingRequestParams</c> with specified settings.
    /// </summary>
    /// <param name="Params">
    /// A procedure reference used to configure the embedding request parameters.
    /// </param>
    /// <returns>
    /// A fully initialized instance of <c>TEmbeddingRequestParams</c>.
    /// </returns>
    class function Create(Params: TProcRef<TEmbeddingRequestParams>): TEmbeddingRequestParams; static;
  end;

  /// <summary>
  /// Represents the parameters required for a batch embedding request.
  /// </summary>
  /// <remarks>
  /// This class encapsulates a collection of embedding requests, allowing for the processing of multiple inputs
  /// in a single API call. Each request in the batch must conform to the specified model.
  /// </remarks>
  TEmbeddingBatchParams = class(TJSONParam)
  public
    /// <summary>
    /// Embed requests for the batch.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TEmbeddingRequestParams</c> representing the individual embedding requests.
    /// Each request must match the model specified in the <c>TEmbeddingRequestParams</c>.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingBatchParams</c> to allow for method chaining.
    /// </returns>
    /// <remarks>
    /// This method is required to specify the requests to be included in the batch embedding process.
    /// </remarks>
    function Requests(const Value: TArray<TEmbeddingRequestParams>): TEmbeddingBatchParams;
  end;

  /// <summary>
  /// Represents the parameters required to create an embedding request.
  /// </summary>
  /// <remarks>
  /// This class encapsulates all necessary information for generating embeddings, including the model to use,
  /// the content to embed, the type of task, an optional title, and the desired output dimensionality.
  /// </remarks>
  TEmbeddingParams = class(TJSONParam)
  public
    /// <summary>
    /// The content to embed.
    /// </summary>
    /// <param name="Value">
    /// An array of strings representing the text content to be embedded.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> to allow for method chaining.
    /// </returns>
    /// <remarks>
    /// This parameter is required and only the parts.text fields will be counted.
    /// </remarks>
    function Content(const Value: TArray<string>): TEmbeddingParams;
    /// <summary>
    /// Optional task type for which the embeddings will be used.
    /// </summary>
    /// <param name="Value">
    /// The type of task to be performed with the embeddings, specified as a <c>TTaskType</c> value.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> to allow for method chaining.
    /// </returns>
    /// <remarks>
    /// This parameter is optional and can only be set for models/embedding-001.
    /// </remarks>
    function TaskType(const Value: TTaskType): TEmbeddingParams;
    /// <summary>
    /// An optional title for the text.
    /// </summary>
    /// <param name="Value">
    /// A string representing the title to be associated with the text content.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> to allow for method chaining.
    /// </returns>
    /// <remarks>
    /// This parameter is optional and only applicable when <c>TaskType</c> is <c>RETRIEVAL_DOCUMENT</c>.
    /// Specifying a title can enhance the quality of embeddings for retrieval.
    /// </remarks>
    function Title(const Value: string): TEmbeddingParams;
    /// <summary>
    /// Optional reduced dimension for the output embedding.
    /// </summary>
    /// <param name="Value">
    /// An integer representing the desired dimensionality for the output embedding.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> to allow for method chaining.
    /// </returns>
    /// <remarks>
    /// If set, excessive values in the output embedding are truncated from the end.
    /// This feature is supported by newer models since 2024 only.
    /// </remarks>
    function OutputDimensionality(const Value: Integer): TEmbeddingParams;
  end;

  /// <summary>
  /// Represents the content embedding generated from input text.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the embedding values produced by an embedding model,
  /// allowing for access to the numerical representation of the content.
  /// </remarks>
  TEmbeddingValues = class
  private
    FValues: TArray<Double>;
  public
    /// <summary>
    /// The embedding values.
    /// </summary>
    property Values: TArray<Double> read FValues write FValues;
  end;

  /// <summary>
  /// Represents an embedding generated from input content.
  /// </summary>
  /// <remarks>
  /// The <c>TEmbedding</c> class encapsulates a numerical representation of text content,
  /// generated by an embedding model. It can be used for various tasks, such as search, retrieval,
  /// and natural language processing.
  /// </remarks>
  TEmbedding = class
  private
    FEmbedding: TEmbeddingValues;
  public
    /// <summary>
    /// The embedding generated from the input content.
    /// </summary>
    property Embedding: TEmbeddingValues read FEmbedding write FEmbedding;
    /// <summary>
    /// Destructor to free resources associated with the embedding.
    /// </summary>
    /// <remarks>
    /// This destructor ensures that the memory allocated for the <c>TEmbeddingValues</c>
    /// instance is properly released when the <c>TEmbedding</c> instance is destroyed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the embedding generated from input content.
  /// </summary>
  /// <remarks>
  /// This class encapsulates the embedding result, providing access to the generated
  /// content embedding, which can be utilized for various applications such as search,
  /// retrieval, and natural language processing tasks.
  /// </remarks>
  TEmbeddings = class
  private
    FEmbeddings: TArray<TEmbeddingValues>;
  public
    /// <summary>
    /// The embeddings for each request, in the same order as provided in the batch request.
    /// </summary>
    property Embeddings: TArray<TEmbeddingValues> read FEmbeddings write FEmbeddings;
    /// <summary>
    /// Destructor to free resources associated with the embedding.
    /// </summary>
    /// <remarks>
    /// This destructor ensures that the memory allocated for the <c>TEmbeddingValues</c>
    /// instance is properly released when the <c>TEmbedding</c> instance is destroyed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request using <c>TEmbedding</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynEmbedding</c> type extends the <c>TAsynParams&lt;TEmbedding&gt;</c> record to handle the lifecycle of an asynchronous model operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking model search operations and is specifically tailored for scenarios where multiple model choices are required.
  /// </remarks>
  TAsynEmbedding = TAsynCallBack<TEmbedding>;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request using <c>TEmbeddings</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynEmbeddings</c> type extends the <c>TAsynParams&lt;TEmbeddings&gt;</c> record to handle the lifecycle of an asynchronous model operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking model search operations and is specifically tailored for scenarios where multiple model choices are required.
  /// </remarks>
  TAsynEmbeddings = TAsynCallBack<TEmbeddings>;

  /// <summary>
  /// Represents the route for embedding operations in the Gemini API.
  /// </summary>
  /// <remarks>
  /// This class provides methods for performing both synchronous and asynchronous embedding requests
  /// using various models. It manages the currently used model and facilitates embedding operations
  /// by allowing users to set parameters and handle results through callbacks.
  /// </remarks>
  TEmbeddingsRoute = class(TGeminiAPIRoute)
  public
    /// <summary>
    /// Performs an asynchronous embedding request and invokes a callback with the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that will be called with an instance of <c>TAsynEmbedding</c> containing the embedding results once the request is complete.
    /// </param>
    /// <remarks>
    /// Use this method to perform an embedding request that returns immediately, allowing other operations to continue while waiting for the response.
    /// <code>
    ///  Gemini.Embeddings.AsynCreate('model_name',
    ///    procedure (Params: TEmbeddingParams)
    ///    begin
    ///      Params.Content(['Str1']);
    ///    end,
    ///    function : TAsynEmbedding
    ///    begin
    ///      Result.Sender := Obj;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Handle the start of the process
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Integration: TEmbedding)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; ErrorMsg: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(const ModelName: string; ParamProc: TProc<TEmbeddingParams>;
      CallBacks: TFunc<TAsynEmbedding>); overload;
    /// <summary>
    /// Performs an asynchronous embedding request and invokes a callback with the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model.
    /// </param>
    /// <param name="Value">
    /// An array of string
    /// </param>
    /// <param name="CallBacks">
    /// A function that will be called with an instance of <c>TAsynEmbedding</c> containing the embedding results once the request is complete.
    /// </param>
    /// <remarks>
    /// Use this method to perform an embedding request that returns immediately, allowing other operations to continue while waiting for the response.
    /// <code>
    ///  Gemini.Embeddings.AsynCreate('model_name',
    ///    procedure (Params: TEmbeddingParams)
    ///    begin
    ///      Params.Content(['Str1']);
    ///    end,
    ///    function : TAsynEmbedding
    ///    begin
    ///      Result.Sender := Obj;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Handle the start of the process
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Integration: TEmbedding)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; ErrorMsg: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(const ModelName: string; Value: TArray<string>;
      CallBacks: TFunc<TAsynEmbedding>); overload;
    /// <summary>
    /// Performs an asynchronous embedding request for batch and invokes a callback with the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that will be called with an instance of <c>TAsynEmbeddings</c> containing the embedding results once the request is complete.
    /// </param>
    /// <remarks>
    /// Use this method to perform an embedding request that returns immediately, allowing other operations to continue while waiting for the response.
    /// <code>
    ///  Gemini.Embeddings.AsynCreateBatch('model_name',
    ///    procedure (Params: TEmbeddingBatchParams)
    ///    begin
    ///      Params.Content(['Str1']);
    ///    end,
    ///    function : TAsynEmbeddings
    ///    begin
    ///      Result.Sender := Obj;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Handle the start of the process
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Integration: TEmbeddings)
    ///        begin
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; ErrorMsg: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynCreateBatch(const ModelName: string; ParamProc: TProc<TEmbeddingBatchParams>;
      CallBacks: TFunc<TAsynEmbeddings>);
    /// <summary>
    /// Performs a synchronous embedding request and returns the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <returns>
    /// An instance of <c>TEmbedding</c> containing the embedding results.
    /// </returns>
    /// <remarks>
    /// Use this method to perform an embedding request that blocks until the response is received.
    /// <code>
    ///   // Example usage:
    ///  var Integration := Gemini.Embeddings.Create('model_name',
    ///          procedure (Params: TEmbeddingParams)
    ///          begin
    ///            Params.Content(['str1']);
    ///          end);
    ///  try
    ///     // Use Embeddings as needed
    ///  finally
    ///    Embeddings.Free;
    ///  end;
    /// </code>
    /// </remarks>
    function Create(const ModelName: string; ParamProc: TProc<TEmbeddingParams>): TEmbedding; overload;
    /// <summary>
    /// Performs a synchronous embedding request and returns the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model
    /// </param>
    /// <param name="Value">
    /// An array of string
    /// </param>
    /// <returns>
    /// An instance of <c>TEmbedding</c> containing the embedding results.
    /// </returns>
    /// <remarks>
    /// Use this method to perform an embedding request that blocks until the response is received.
    /// <code>
    ///   // Example usage:
    ///  var Integration := Gemini.Embeddings.Create('mon_model', ['Str1']);
    ///  try
    ///     // Use Embeddings as needed
    ///  finally
    ///    Embeddings.Free;
    ///  end;
    /// </code>
    /// </remarks>
    function Create(const ModelName: string; Value: TArray<string>): TEmbedding; overload;
    /// <summary>
    /// Performs a synchronous embedding request for a batch and returns the result.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <returns>
    /// An instance of <c>TEmbeddings</c> containing the embedding results.
    /// </returns>
    /// <remarks>
    /// Use this method to perform an embedding request that blocks until the response is received.
    /// <code>
    ///   // Example usage:
    ///  var Integration := Gemini.Embeddings.CreateBatch('model_name',
    ///     procedure (Parameters: TEmbeddingBatchParams)
    ///     begin
    ///       Parameters.Requests(
    ///         [
    ///          TEmbeddingRequestParams.Create(
    ///             procedure (var Params: TEmbeddingRequestParams)
    ///             begin
    ///               Params.Content(['str1', 'str2']);
    ///               Params.OutputDimensionality(10); //Optional
    ///             end)
    ///         ]);
    ///     end);
    ///  try
    ///    // Use Embeddings as needed
    ///  finally
    ///    Embeddings.Free;
    ///  end;
    /// </code>
    /// </remarks>
    function CreateBatch(const ModelName: string; ParamProc: TProc<TEmbeddingBatchParams>): TEmbeddings;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TTaskTypeHelper }

function TTaskTypeHelper.ToString: string;
begin
  case Self of
    TASK_TYPE_UNSPECIFIED:
      Exit('TASK_TYPE_UNSPECIFIED');
    RETRIEVAL_QUERY:
      Exit('RETRIEVAL_QUERY');
    RETRIEVAL_DOCUMENT:
      Exit('RETRIEVAL_DOCUMENT');
    SEMANTIC_SIMILARITY:
      Exit('SEMANTIC_SIMILARITY');
    CLASSIFICATION:
      Exit('CLASSIFICATION');
    CLUSTERING:
      Exit('CLUSTERING');
    QUESTION_ANSWERING:
      Exit('QUESTION_ANSWERING');
    FACT_VERIFICATION:
      Exit('FACT_VERIFICATION');
    NONE:
      Exit('NONE');
  end;
end;

{ TEmbeddings }

destructor TEmbeddings.Destroy;
begin
  for var Item in FEmbeddings do
    Item.Free;
  inherited;
end;

{ TEmbeddingsRoute }

function TEmbeddingsRoute.Create(const ModelName: string;
  ParamProc: TProc<TEmbeddingParams>): TEmbedding;
begin
  GeminiLock.Acquire;
  try
    Result := API.Post<TEmbedding, TEmbeddingParams>(SetModel(ModelName, ':embedContent'), ParamProc);
  finally
    GeminiLock.Release;
  end;
end;

procedure TEmbeddingsRoute.AsynCreate(const ModelName: string;
  ParamProc: TProc<TEmbeddingParams>; CallBacks: TFunc<TAsynEmbedding>);
begin
  with TAsynCallBackExec<TAsynEmbedding, TEmbedding>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEmbedding
      begin
        Result := Self.Create(ModelName, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TEmbeddingsRoute.AsynCreate(const ModelName: string;
  Value: TArray<string>; CallBacks: TFunc<TAsynEmbedding>);
begin
  with TAsynCallBackExec<TAsynEmbedding, TEmbedding>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEmbedding
      begin
        Result := Self.Create(ModelName, Value);
      end);
  finally
    Free;
  end;
end;

procedure TEmbeddingsRoute.AsynCreateBatch(const ModelName: string;
  ParamProc: TProc<TEmbeddingBatchParams>; CallBacks: TFunc<TAsynEmbeddings>);
begin
  with TAsynCallBackExec<TAsynEmbeddings, TEmbeddings>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEmbeddings
      begin
        Result := Self.CreateBatch(ModelName, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TEmbeddingsRoute.Create(const ModelName: string;
  Value: TArray<string>): TEmbedding;
begin
  Result := Create(ModelName,
    procedure (Params: TEmbeddingParams)
    begin
      Params.Content(Value);
    end);
end;

function TEmbeddingsRoute.CreateBatch(const ModelName: string;
  ParamProc: TProc<TEmbeddingBatchParams>): TEmbeddings;
begin
  GeminiLock.Acquire;
  try
    Result := API.Post<TEmbeddings, TEmbeddingBatchParams>(SetModel(ModelName, ':batchEmbedContents'), ParamProc);
  finally
    GeminiLock.Release;
  end;
end;

{ TEmbeddingBatchParams }

function TEmbeddingBatchParams.Requests(
  const Value: TArray<TEmbeddingRequestParams>): TEmbeddingBatchParams;
begin
  var RequestsJSON := TJSONArray.Create;
  for var Item in Value do
    begin
      RequestsJSON.Add(Item.ToJson);
    end;
  Result := TEmbeddingBatchParams(Add('requests', RequestsJSON));
end;

{ TEmbeddingRequestParams }

function TEmbeddingRequestParams.Content(const Value: TArray<string>): TEmbeddingRequestParams;
begin
  FContent := Value;
  Result := Self;
end;

class function TEmbeddingRequestParams.Create(
  Params: TProcRef<TEmbeddingRequestParams>): TEmbeddingRequestParams;
begin
  Result.FModel := TEmbeddingsRoute.CurrentModel;
  Result.FOutputDimensionality := 768;
  Result.FTaskType := NONE;
  Result.FTitle := EmptyStr;
  if Assigned(Params) then
    Params(Result);
end;

function TEmbeddingRequestParams.OutputDimensionality(
  const Value: Integer): TEmbeddingRequestParams;
begin
  FOutputDimensionality := Value;
  Result := Self;
end;

function TEmbeddingRequestParams.TaskType(const Value: TTaskType): TEmbeddingRequestParams;
begin
  if FModel <> 'models/embedding-001' then
    FTaskType := Value;
  Result := Self;
end;

function TEmbeddingRequestParams.Title(const Value: string): TEmbeddingRequestParams;
begin
  if FModel <> 'models/embedding-001' then
    FTitle := Value;
  Result := Self;
end;

function TEmbeddingRequestParams.ToJson: TJSONObject;
begin
  var PartsJSON := TJSONArray.Create;
  for var Item in FContent do
    begin
      PartsJSON.add(TJSONObject.Create.AddPair('text', Item));
    end;
  var ContentJSON := TJSONObject.Create.AddPair('parts', PARTSJSON);

  Result := TJSONObject.Create;

  if not FModel.IsEmpty then
    Result := Result.AddPair('model', FModel);

  Result := Result.AddPair('content', ContentJSON);

  {--- Optional }
  if FTaskType <> none then
    Result := Result.AddPair('taskType', FTaskType.ToString);

  {--- Optional }
  if not FTitle.IsEmpty and (FTaskType = RETRIEVAL_DOCUMENT) then
    Result := Result.AddPair('title', FTitle);

  {--- Optional }
  if (FOutputDimensionality <> 768) then 
    Result := Result.AddPair('outputDimensionality', FOutputDimensionality);
end;

{ TEmbedding }

destructor TEmbedding.Destroy;
begin
  if Assigned(FEmbedding) then
    FEmbedding.Free;
  inherited;
end;

{ TEmbeddingParams }

function TEmbeddingParams.Content(
  const Value: TArray<string>): TEmbeddingParams;
begin
  var PartsJSON := TJSONArray.Create;
  for var Item in Value do
    begin
      PartsJSON.Add(TJSONObject.Create.AddPair('text', Item));
    end;
  var ContentJSON := TJSONObject.Create;
  ContentJSON.AddPair('parts', PartsJSON);

  Result := TEmbeddingParams(
    Add('model', TEmbeddingsRoute.CurrentModel).
    Add('content', ContentJSON));
end;

function TEmbeddingParams.OutputDimensionality(
  const Value: Integer): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('outputDimensionality', Value));
end;

function TEmbeddingParams.TaskType(const Value: TTaskType): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('taskType', Value.ToString));
end;

function TEmbeddingParams.Title(const Value: string): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('title', Value));
end;

end.

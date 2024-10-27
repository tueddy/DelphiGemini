unit Gemini.Models;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Json.Types,
  Gemini.API.Params, Gemini.API, Gemini.Async.Support;

type
  /// <summary>
  /// Represents a model with various properties used in text generation and processing tasks.
  /// </summary>
  /// <remarks>
  /// The <c>TModel</c> class encapsulates various attributes and parameters of a model used for text generation.
  /// This includes properties like the model's resource name, version, description, token limits, and more.
  /// <para>
  /// - <c>Name</c>: The unique resource identifier for the model, formatted as models/{baseModelId}-{version}. This is used to distinguish between different models and their variants.
  /// </para>
  /// <para>
  /// - <c>BaseModelId</c>: The identifier of the base model. It is required when making generation requests to the backend. For example, "gemini-1.5-flash" is a possible base model ID.
  /// </para>
  /// <para>
  /// - <c>Version</c>: Indicates the version of the model, such as "1.0" or "1.5". This property reflects the major version of the model used for generation tasks.
  /// </para>
  /// <para>
  /// - <c>DisplayName</c>: A human-readable name for the model (e.g., "Gemini 1.5 Flash"). This is primarily used for display purposes and can include any UTF-8 characters, up to 128 in length.
  /// </para>
  /// <para>
  /// - <c>Description</c>: A brief explanation of the model’s functionality and purpose. This helps users understand the model’s capabilities at a glance.
  /// </para>
  /// <para>
  /// - <c>InputTokenLimit</c> and <c>OutputTokenLimit</c>: These properties define the maximum number of tokens that can be processed as input and generated as output, respectively. They set constraints on the model's input and output capacities.
  /// </para>
  /// <para>
  /// - <c>SupportedGenerationMethods</c>: Lists the generation methods supported by the model. These are usually defined as Pascal-case strings, such as "generateMessage" or "generateContent". These methods define the various ways the model can be used.
  /// </para>
  /// <para>
  /// - <c>Temperature</c> and <c>MaxTemperature</c>: These parameters control the randomness and variability of the model's responses. A higher temperature results in more diverse outputs, while lower values make responses more deterministic. The temperature value can range between 0.0 and <c>MaxTemperature</c>.
  /// </para>
  /// <para>
  /// - <c>TopP</c>: This property is used for nucleus sampling, which considers the smallest set of tokens whose cumulative probability is at least the value of <c>TopP</c>. This helps ensure more focused and coherent responses by limiting the selection to the most probable tokens.
  /// </para>
  /// <para>
  /// - <c>TopK</c>: Controls top-k sampling, where only the top K most probable tokens are considered during generation. If this property is not set, the model will not use top-k sampling for generating responses.
  /// </para>
  /// This class serves as a foundational data structure to configure and manage various aspects of a model's behavior in AI-driven text generation systems.
  /// </remarks>
  TModel = class
  private
    FName: string;
    FBaseModelId: string;
    FVersion: string;
    FDisplayName: string;
    FDescription: string;
    FInputTokenLimit: string;
    FOutputTokenLimit: string;
    FSupportedGenerationMethods: TArray<string>;
    FTemperature: double;
    FMaxTemperature: double;
    FTopP: double;
    FTopK: integer;
  public
    /// <summary>
    /// The resource name of the Model.
    /// </summary>
    /// <remarks>
    /// Refer to Model variants for all allowed values.
    /// Format: models/{model} with a {model} naming convention of:
    ///   "{baseModelId}-{version}"
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// he name of the base model, pass this to the generation request.
    /// </summary>
    /// <remarks>
    /// Examples: gemini-1.5-flash
    /// </remarks>
    property BaseModelId: string read FBaseModelId write FBaseModelId;
    /// <summary>
    /// The version number of the model.
    /// </summary>
    /// <remarks>
    /// This represents the major version (1.0 or 1.5)
    /// </remarks>
    property Version: string read FVersion write FVersion;
    /// <summary>
    /// The human-readable name of the model. E.g. "Gemini 1.5 Flash".
    /// </summary>
    /// <remarks>
    /// The name can be up to 128 characters long and can consist of any UTF-8 characters.
    /// </remarks>
    property DisplayName: string read FDisplayName write FDisplayName;
    /// <summary>
    /// A short description of the model.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Maximum number of input tokens allowed for this model.
    /// </summary>
    property InputTokenLimit: string read FInputTokenLimit write FInputTokenLimit;
    /// <summary>
    /// Maximum number of output tokens available for this model.
    /// </summary>
    property OutputTokenLimit: string read FOutputTokenLimit write FOutputTokenLimit;
    /// <summary>
    /// The model's supported generation methods.
    /// </summary>
    /// <remarks>
    /// The corresponding API method names are defined as Pascal case strings, such as generateMessage and generateContent.
    /// </remarks>
    property SupportedGenerationMethods: TArray<string> read FSupportedGenerationMethods write FSupportedGenerationMethods;
    /// <summary>
    /// Controls the randomness of the output.
    /// </summary>
    /// <remarks>
    /// Values can range over [0.0,maxTemperature], inclusive. A higher value will produce responses that are more varied, while a value closer to 0.0 will typically result in less surprising responses from the model. This value specifies default to be used by the backend while making the call to the model.
    /// </remarks>
    property Temperature: double read FTemperature write FTemperature;
    /// <summary>
    /// The maximum temperature this model can use.
    /// </summary>
    property MaxTemperature: double read FMaxTemperature write FMaxTemperature;
    /// <summary>
    /// For Nucleus sampling.
    /// </summary>
    /// <remarks>
    /// Nucleus sampling considers the smallest set of tokens whose probability sum is at least topP. This value specifies default to be used by the backend while making the call to the model.
    /// </remarks>
    property TopP: double read FTopP write FTopP;
    /// <summary>
    /// For Top-k sampling.
    /// </summary>
    /// <remarks>
    /// Top-k sampling considers the set of topK most probable tokens. This value specifies default to be used by the backend while making the call to the model. If empty, indicates the model doesn't use top-k sampling, and topK isn't allowed as a generation parameter.
    /// </remarks>
    property TopK: integer read FTopK write FTopK;
  end;

  /// <summary>
  /// Represents a collection of models, along with pagination support for retrieving additional models.
  /// </summary>
  /// <remarks>
  /// The <c>TModels</c> class manages an array of <c>TModel</c> instances and includes pagination capabilities.
  /// It allows easy access to a list of models and handles memory management by freeing individual models when
  /// the instance of the class is destroyed.
  /// </remarks>
  TModels = class
  private
    FModels: TArray<TModel>;
    FNextPageToken: string;
  public
    /// <summary>
    /// Gets or sets the array of models.
    /// </summary>
    /// <remarks>
    /// The <c>Models</c> property contains a list of model instances that can be accessed or modified.
    /// Each item in this array represents an individual model with its own properties and settings.
    /// </remarks>
    property Models: TArray<TModel> read FModels write FModels;
    /// <summary>
    /// Gets or sets the pagination token for retrieving the next page of models.
    /// </summary>
    /// <remarks>
    /// If the model list is paginated, this token can be used in subsequent requests to retrieve the next
    /// set of models. This token is managed by the API and allows efficient handling of large datasets.
    /// </remarks>
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    /// <summary>
    /// Destructor for the <c>TModels</c> class. Frees the memory associated with each model in the array.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that all models in the <c>FModels</c> array are properly freed before
    /// the <c>TModels</c> instance is destroyed, thus preventing memory leaks.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request using <c>TModel</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModel</c> type extends the <c>TAsynParams&lt;TModel&gt;</c> record to handle the lifecycle of an asynchronous model operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking model search operations and is specifically tailored for scenarios where multiple model choices are required.
  /// </remarks>
  TAsynModel = TAsynCallBack<TModel>;

  /// <summary>
  /// Manages asynchronous callbacks for a model search request that returns a collection of models using <c>TModels</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynModels</c> type extends the <c>TAsynCallBack&lt;TModels&gt;</c> record to handle the lifecycle of an asynchronous operation
  /// involving multiple models. This includes event handlers that trigger at key points in the process, such as when the operation begins,
  /// completes successfully, or encounters an error.
  /// This structure is designed for non-blocking operations that return a collection of models. The <c>TModels</c> type represents
  /// a collection of <c>TModel</c> instances, making this type useful when working with multiple model objects in asynchronous requests.
  /// </remarks>
  TAsynModels = TAsynCallBack<TModels>;

  /// <summary>
  /// Handles API routes related to model retrieval and provides methods to list or fetch specific models.
  /// </summary>
  /// <remarks>
  /// The <c>TModelsRoute</c> class extends <c>TModelsRouteParams</c> and offers methods to retrieve models from the API.
  /// It includes overloaded <c>List</c> methods that allow for listing all models, fetching paginated results, or retrieving a specific model by name.
  /// This class interacts with the underlying API to fetch model data and returns instances of <c>TModels</c> or <c>TModel</c>.
  /// </remarks>
  TModelsRoute = class(TGeminiAPIRoute)
  public
    /// <summary>
    /// Asynchronously retrieves the list of all available models.
    /// </summary>
    /// <param name="CallBacks">
    /// A <c>TFunc&lt;TAsynModels&gt;</c> representing the callback to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to fetch all available models asynchronously.
    /// The <paramref name="CallBacks"/> function is invoked when the operation completes,
    /// either successfully or with an error.
    /// <code>
    /// Gemini.Models.AsynList(
    ///    function : TAsynModels
    ///    begin
    ///      Result.Sender := Memo1;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; List: TModels)
    ///        begin
    ///          var M := Sender as TMemo;
    ///          // Handle the display
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///
    ///    end
    ///  );
    /// </code>
    /// </remarks>
    procedure AsynList(CallBacks: TFunc<TAsynModels>); overload;
    /// <summary>
    /// Asynchronously retrieves a paginated list of models.
    /// </summary>
    /// <param name="PageSize">
    /// The number of models to return per page.
    /// </param>
    /// <param name="PageToken">
    /// A token used to retrieve the next page of models.
    /// </param>
    /// <param name="CallBacks">
    /// A <c>TFunc&lt;TAsynModels&gt;</c> representing the callback to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// This method allows for paginated retrieval of models asynchronously. The <paramref name="PageSize"/>
    /// parameter specifies how many models to return per request, while the <paramref name="PageToken"/> helps
    /// retrieve the next set of models. If no token is provided, the first page is returned.
    /// <code>
    ///
    /// //Declare global variable var Next: string;
    ///
    /// Gemini.Models.AsynList(5, Next,
    ///    function : TAsynModels
    ///    begin
    ///      Result.Sender := Memo1;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; List: TModels)
    ///        begin
    ///          var M := Sender as TMemo;
    ///          // Handle the display
    ///          Next := List.NextPageToken;
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///
    ///    end
    ///  );
    /// </code>
    /// </remarks>
    procedure AsynList(const PageSize: Integer; const PageToken: string;
      CallBacks: TFunc<TAsynModels>); overload;
    /// <summary>
    /// Asynchronously retrieves a specific model by its name, ensuring the name is properly formatted.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model to retrieve. If the model name does not already start with 'models/',
    /// it will be prefixed with 'models/'.
    /// </param>
    /// <param name="CallBacks">
    /// A <c>TFunc&lt;TAsynModel&gt;</c> representing the callback to handle the asynchronous result, including
    /// the start, success, and error handling processes.
    /// </param>
    /// <param name="LowerCase">
    /// An optional boolean flag (default: False). If set to True, the model name will be converted to lowercase
    /// before processing. This ensures uniformity when querying the API asynchronously.
    /// </param>
    /// <remarks>
    /// This method sends a request to the API to asynchronously retrieve a specific model by its name.
    /// The <paramref name="ModelName"/> parameter is required and can be automatically adjusted with the
    /// <c>LowerCase</c> flag. The model name will be properly formatted to ensure compatibility with the
    /// LLM Gemini framework.
    /// <code>
    /// Gemini.Models.AsynList('Gemini-1.5-flash',
    ///     function : TAsynModel
    ///     begin
    ///       Result.Sender := Memo1;
    ///
    ///       Result.OnStart :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Handle the start
    ///         end;
    ///
    ///       Result.OnSuccess :=
    ///         procedure (Sender: TObject; List: TModel)
    ///         begin
    ///           var M := Sender as TMemo;
    ///           // Handle the display
    ///         end;
    ///
    ///       Result.OnError :=
    ///         procedure (Sender: TObject; Error: string)
    ///         begin
    ///           // Handle the error message
    ///         end
    ///     end,
    ///     True);  // Optional LowerCase flag
    /// </code>
    /// The <c>LowerCase</c> flag ensures that model names are standardized before retrieval in asynchronous calls.
    /// </remarks>
    procedure AsynList(const ModelName: string; CallBacks: TFunc<TAsynModel>;
      LowerCase: Boolean = False); overload;
    /// <summary>
    /// Retrieves the list of all available models.
    /// </summary>
    /// <returns>
    /// A <c>TModels</c> object containing the list of models.
    /// </returns>
    /// <remarks>
    /// This method sends a request to the API to fetch all available models without pagination or filtering.
    /// It returns a <c>TModels</c> object containing the collection of models.
    /// <code>
    /// var List := Gemini.Models.List;
    /// try
    ///   for var Item in List.Models do
    ///     WriteLn( Item.DisplayName );
    /// finally
    ///   List.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TModels; overload;
    /// <summary>
    /// Retrieves a paginated list of models.
    /// </summary>
    /// <param name="PageSize">
    /// The number of models to return per page.
    /// </param>
    /// <param name="PageToken">
    /// A token used to retrieve the next page of models.
    /// </param>
    /// <returns>
    /// A <c>TModels</c> object containing the list of models for the specified page.
    /// </returns>
    /// <remarks>
    /// This overloaded <c>List</c> method allows for paginated retrieval of models.
    /// The page size specifies how many models to return per request, while the page token
    /// helps retrieve the next set of models. If no page token is provided, the first page is returned.
    /// <code>
    ///
    /// //Declare global variable var Next: string;
    ///
    /// var List := Gemini.Models.List(5, Next);
    /// try
    ///   for var Item in List.Models do
    ///     WriteLn( Item.DisplayName );
    ///   Next := List.NextPageToken;
    /// finally
    ///   List.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List(const PageSize: Integer; const PageToken: string): TModels; overload;
    /// <summary>
    /// Retrieves a specific model by its name, ensuring the name is properly formatted.
    /// </summary>
    /// <param name="ModelName">
    /// The name of the model to retrieve. If the model name does not already start with 'models/',
    /// it will be prefixed with 'models/'.
    /// </param>
    /// <param name="LowerCase">
    /// An optional boolean flag (default: False). If set to True, the model name will be converted to lowercase
    /// before processing, ensuring uniformity when querying the API.
    /// </param>
    /// <returns>
    /// A <c>TModel</c> object representing the requested model.
    /// </returns>
    /// <remarks>
    /// This method sends a request to the API to retrieve a model by its unique name. The <c>ModelName</c>
    /// parameter is required and can be automatically adjusted with the <c>LowerCase</c> flag. The model name
    /// will be properly formatted to ensure compatibility with the LLM Gemini framework.
    /// <code>
    /// var Model := Gemini.Models.List('Gemini-1.5-flash', True);
    /// try
    ///    WriteLn( Model.DisplayName );
    /// finally
    ///    Model.Free;
    /// end;
    /// </code>
    /// The <c>LowerCase</c> flag ensures that model names are standardized before retrieval.
    /// </remarks>
    function List(const ModelName: string; LowerCase: Boolean = False): TModel; overload;
  end;

implementation

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FModels do
    Item.Free;
  inherited;
end;

{ TModelsRoute }

procedure TModelsRoute.AsynList(CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsynList(const PageSize: Integer;
  const PageToken: string; CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List(PageSize, PageToken);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsynList(const ModelName: string;
  CallBacks: TFunc<TAsynModel>; LowerCase: Boolean);
begin
  with TAsynCallBackExec<TAsynModel, TModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModel
      begin
        Result := List(ModelName, LowerCase);
      end);
  finally
    Free;
  end;
end;

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

function TModelsRoute.List(const ModelName: string; LowerCase: Boolean): TModel;
begin
  Result := API.Get<TModel>(ModelName);
end;

function TModelsRoute.List(const PageSize: Integer;
  const PageToken: string): TModels;
begin
  Result := API.Get<TModels>('models', ParamsBuilder(PageSize, PageToken));
end;

end.

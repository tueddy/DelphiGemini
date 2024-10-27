unit Gemini;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, Gemini.API, System.Net.URLClient,
  Gemini.Chat, Gemini.Models, Gemini.Embeddings, Gemini.Files, Gemini.Caching,
  Gemini.FineTunings;

type
  /// <summary>
  /// The <c>IGemini</c> interface provides access to the various features and routes of the Gemini AI API.
  /// This interface allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This interface should be implemented by any class that wants to provide a structured way of accessing
  /// the Gemini AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  ///
  /// To use this interface, instantiate a class that implements it, set the required properties such as
  /// <see cref="Token"/> and <see cref="BaseURL"/>, and call the relevant methods for the desired operations.
  /// <code>
  ///   var Gemini: IGemini := TGemini.Create(API_TOKEN);
  /// </code>
  /// <seealso cref="TGemini"/>
  /// </remarks>
  IGemini = interface
    ['{7E69221E-3C24-4B38-9AE9-894714CA9A47}']
    function GetAPI: TGeminiAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetModelsRoute: TModelsRoute;
    function GetEmbeddingsRoute: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetCachingRoute: TCachingRoute;
    function GetFineTuneRoute : TFineTuneRoute;

    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TGeminiAPI for making API calls.
    /// </returns>
    property API: TGeminiAPI read GetAPI;
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.Gemini.com/v1
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the models API.
    /// Allows for retrieving and managing available models, including those fine-tuned for specific tasks.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Allows for generating vector embeddings from text input, useful for tasks like semantic search and similarity comparisons.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding-related operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddingsRoute;
    // TODO
    property Files: TFilesRoute read GetFilesRoute;
    property Caching: TCachingRoute read GetCachingRoute;
    property FineTune: TFineTuneRoute read GetFineTuneRoute;
  end;

  /// <summary>
  /// The <c>TGeminiFactory</c> class is responsible for creating instances of
  /// the <see cref="IGemini"/> interface. It provides a factory method to instantiate
  /// the interface with a provided API token and optional header configuration.
  /// </summary>
  /// <remarks>
  /// This class provides a convenient way to initialize the <see cref="IGemini"/> interface
  /// by encapsulating the necessary configuration details, such as the API token and header options.
  /// By using the factory method, users can quickly create instances of <see cref="IGemini"/> without
  /// manually setting up the implementation details.
  /// </remarks>
  TGeminiFactory = class
    /// <summary>
    /// Creates an instance of the <see cref="IGemini"/> interface with the specified API token
    /// and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Gemini API services.
    /// </param>
    /// <param name="Option">
    /// An optional header configuration of type <see cref="THeaderOption"/> to customize the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IGemini"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateInstance(const AToken: string): IGemini;
  end;

  /// <summary>
  /// The TGemini class provides access to the various features and routes of the Gemini AI API.
  /// This class allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This class should be implemented by any class that wants to provide a structured way of accessing
  /// the Gemini AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  /// <seealso cref="TGemini"/>
  /// </remarks>
  TGemini = class(TInterfacedObject, IGemini)
  strict private

  private
    FAPI: TGeminiAPI;

    FChatRoute: TChatRoute;
    FModelsRoute: TModelsRoute;
    FEmbeddingsRoute: TEmbeddingsRoute;
    FFilesRoute: TFilesRoute;
    FCachingRoute: TCachingRoute;
    FFineTuneRoute: TFineTuneRoute;

    function GetAPI: TGeminiAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);

    function GetChatRoute: TChatRoute;
    function GetModelsRoute: TModelsRoute;
    function GetEmbeddingsRoute: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetCachingRoute: TCachingRoute;
    function GetFineTuneRoute : TFineTuneRoute;

  public
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TGeminiAPI for making API calls.
    /// </returns>
    property API: TGeminiAPI read GetAPI;
    /// <summary>
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.Gemini.com/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the models API.
    /// Allows for retrieving and managing available models, including those fine-tuned for specific tasks.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Allows for generating vector embeddings from text input, useful for tasks like semantic search and similarity comparisons.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding-related operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddingsRoute;
    // TODO
    property Files: TFilesRoute read GetFilesRoute;
    property Caching: TCachingRoute read GetCachingRoute;
    property FineTune: TFineTuneRoute read GetFineTuneRoute;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TGemini"/> class with optional header configuration.
    /// </summary>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor is typically used when no API token is provided initially.
    /// The token can be set later via the <see cref="Token"/> property.
    /// </remarks>
    constructor Create; overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="TGemini"/> class with the provided API token and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with the Gemini AI API.
    /// </param>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor allows the user to specify an API token at the time of initialization.
    /// </remarks>
    constructor Create(const AToken: string); overload;
    /// <summary>
    /// Releases all resources used by the current instance of the <see cref="TGemini"/> class.
    /// </summary>
    /// <remarks>
    /// This method is called to clean up any resources before the object is destroyed.
    /// It overrides the base <see cref="TInterfacedObject.Destroy"/> method.
    /// </remarks>
    destructor Destroy; override;
  end;

implementation

{ TGemini }

constructor TGemini.Create;
begin
  inherited Create;
  FAPI := TGeminiAPI.Create;
end;

constructor TGemini.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TGemini.Destroy;
begin
  FChatRoute.Free;
  FModelsRoute.Free;
  FEmbeddingsRoute.Free;
  FFilesRoute.Free;
  FCachingRoute.Free;
  FFineTuneRoute.Free;
  FAPI.Free;
  inherited;
end;

function TGemini.GetAPI: TGeminiAPI;
begin
  Result := FAPI;
end;

function TGemini.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TGemini.GetCachingRoute: TCachingRoute;
begin
  if not Assigned(FCachingRoute) then
    FCachingRoute := TCachingRoute.CreateRoute(API);
  Result := FCachingRoute;
end;

function TGemini.GetChatRoute: TChatRoute;
begin
  if not Assigned(FChatRoute) then
    FChatRoute := TChatRoute.CreateRoute(API);
  Result := FChatRoute;
end;

function TGemini.GetEmbeddingsRoute: TEmbeddingsRoute;
begin
  if not Assigned(FEmbeddingsRoute) then
    FEmbeddingsRoute := TEmbeddingsRoute.CreateRoute(API);
  Result := FEmbeddingsRoute;
end;

function TGemini.GetFilesRoute: TFilesRoute;
begin
  if not Assigned(FFilesRoute) then
    FFilesRoute := TFilesRoute.CreateRoute(API);
  Result := FFilesRoute;
end;

function TGemini.GetFineTuneRoute: TFineTuneRoute;
begin
  if not Assigned(FFineTuneRoute) then
    FFineTuneRoute := TFineTuneRoute.CreateRoute(API);
  Result := FFineTuneRoute;
end;

function TGemini.GetModelsRoute: TModelsRoute;
begin
  if not Assigned(FModelsRoute) then
    FModelsRoute := TModelsRoute.CreateRoute(API);
  Result := FModelsRoute;
end;

function TGemini.GetToken: string;
begin
  Result := FAPI.Token;
end;

procedure TGemini.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TGemini.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TGeminiFactory }

class function TGeminiFactory.CreateInstance(const AToken: string): IGemini;
begin
  Result := TGemini.Create(AToken);
end;

end.

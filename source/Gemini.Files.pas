unit Gemini.Files;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Net.URLClient,
  Gemini.API.Params, Gemini.API, Gemini.Async.Support;

type
  /// <summary>
  /// States for the lifecycle of a File, representing its readiness for use and the outcome of processing.
  /// </summary>
  /// <remarks>
  /// The `TStateType` enumeration describes the current status of a file in the Gemini API.
  /// Files typically transition through these states sequentially, from `PROCESSING` to `ACTIVE`,
  /// or, in case of errors, to `FAILED`.
  /// <para>
  /// Detailed State Descriptions:
  /// </para>
  /// <para>
  /// - `STATE_UNSPECIFIED`: The default state used when no other state has been specified.
  ///   This may indicate that the file is new and has not yet been submitted for processing.
  /// </para>
  /// <para>
  /// - `PROCESSING`: The file is currently undergoing processing and is not yet available for inference.
  ///   In this state, the file is temporarily inaccessible for operations until processing is complete.
  /// </para>
  /// <para>
  /// - `ACTIVE`: The file has been successfully processed and is ready for inference or other actions.
  ///   This state indicates that the file is fully functional and available.
  /// </para>
  /// <para>
  /// - `FAILED`: An error occurred during processing, and the file is unavailable for further use.
  ///   This state is final, and the file may need to be deleted or re-uploaded for reprocessing.
  /// </para>
  /// Note: Transitioning between states is typically managed by the server, based on processing
  /// results. Developers should account for possible delays when a file is in `PROCESSING`,
  /// especially when building synchronous operations dependent on the file's availability.
  /// </remarks>
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

  /// <summary>
  /// Helper record for the <c>TStateType</c> enumeration, providing utility methods for converting
  /// between <c>TStateType</c> values and their string representations.
  /// </summary>
  TStateTypeHelper = record helper for TStateType
    /// <summary>
    /// Converts the current <c>TStateType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TStateType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TStateType</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TStateType</c>.
    /// </param>
    /// <returns>
    /// The <c>TStateType</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TStateType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TStateType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TStateType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TStateTypeInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TStateType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TStateType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TStateType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TStateType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TStateType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TStateType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TStateType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// The class <c>TFileParams</c>managing the parameters provided in the request to obtain the download URI for a file.
  /// </summary>
  TFileParams = class(TJSONParam)
  public
    /// <summary>
    /// Optional. The human-readable display name for the File.
    /// </summary>
    /// <param name="Value">
    /// The display name must be no more than 512 characters in length, including spaces. Example: "Welcome Image"
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFileParams</c> value.
    /// </returns>
    function DisplayName(const Value: string): TFileParams;
  end;

  /// <summary>
  /// The Status class <c>TStatus</c> defines a logical error model that is suitable for different programming environments, including REST APIs and RPC APIs. It is used by gRPC. Each Status message contains three pieces of data: error code, error message, and error details.
  /// </summary>
  /// <remarks>
  /// You can find out more about this error model and how to work with it in the API Design Guide.
  /// <para>
  /// - https://google.aip.dev/193
  /// </para>
  /// </remarks>
  TStatus = class
  private
    FCode: Int64;
    FMessage: string;
    FDetails: TArray<string>;
  public
    /// <summary>
    /// The status code, which should be an enum value of google.rpc.Code.
    /// </summary>
    /// <remarks>
    /// https://github.com/grpc
    /// </remarks>
    property Code: Int64 read FCode write FCode;
    /// <summary>
    /// A developer-facing error message, which should be in English.
    /// </summary>
    /// <remarks>
    /// Any user-facing error message should be localized and sent in the google.rpc.Status.details field, or localized by the client.
    /// </remarks>
    property Message: string read FMessage write FMessage;
    /// <summary>
    /// A list of messages that carry the error details. There is a common set of message types for APIs to use.
    /// </summary>
    /// <remarks>
    /// An object containing fields of an arbitrary type. An additional field "@type" contains a URI identifying the type. Example: { "id": 1234, "@type": "types.example.com/standard/id" }.
    /// </remarks>
    property Details: TArray<string> read FDetails write FDetails;
  end;

  /// <summary>
  /// The <c>TFileContent</c> class is responsible for handling files uploaded to the API.
  /// </summary>
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
    FError: TStatus;
  public
    /// <summary>
    /// Immutable. Identifier. The File resource name.
    /// </summary>
    /// <remarks>
    /// The ID (name excluding the "files/" prefix) can contain up to 40 characters that are lowercase alphanumeric or dashes (-).
    /// <para>
    /// The ID cannot start or end with a dash. If the name is empty on create, a unique name will be generated. Example: files/123-456
    /// </para>
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// Optional. The human-readable display name for the File.
    /// </summary>
    /// <remarks>
    /// The display name must be no more than 512 characters in length, including spaces. Example: "Welcome Image"
    /// </remarks>
    property DisplayName: string read FDisplayName write FDisplayName;
    /// <summary>
    /// Output only. MIME type of the file.
    /// </summary>
    property MimeType: string read FMimeType write FMimeType;
    /// <summary>
    /// Output only. Size of the file in bytes.
    /// </summary>
    property SizeBytes: string read FSizeBytes write FSizeBytes;
    /// <summary>
    /// Output only. The timestamp of when the File was created.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property CreateTime: string read FCreateTime write FCreateTime;
    /// <summary>
    /// Output only. The timestamp of when the File was last updated.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property UpdateTime: string read FUpdateTime write FUpdateTime;
    /// <summary>
    /// Output only. The timestamp of when the File will be deleted. Only set if the File is scheduled to expire.
    /// </summary>
    /// <remarks>
    /// A timestamp in RFC3339 UTC "Zulu" format, with nanosecond resolution and up to nine fractional digits. Examples: "2014-10-02T15:01:23Z" and "2014-10-02T15:01:23.045123456Z".
    /// </remarks>
    property ExpirationTime: string read FExpirationTime write FExpirationTime;
    /// <summary>
    /// Output only. SHA-256 hash of the uploaded bytes.
    /// </summary>
    /// <remarks>
    /// A base64-encoded string.
    /// </remarks>
    property Sha256Hash: string read FSha256Hash write FSha256Hash;
    /// <summary>
    /// Output only. The uri of the File.
    /// </summary>
    property Uri: string read FUri write FUri;
    /// <summary>
    /// Output only. Processing state of the File.
    /// </summary>
    property State: TStateType read FState write FState;
    /// <summary>
    /// Output only. Error status if File processing failed.
    /// </summary>
    property Error: TStatus read FError write FError;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a file entity within the Gemini API, encapsulating the file's metadata and content.
  /// </summary>
  /// <remarks>
  /// The <c>TFile</c> class contains a <c>TFileContent</c> object which holds detailed information about the file, such as its name, size, MIME type, and processing state.
  /// Used to manage file operations, such as uploading, retrieving, listing, and deleting files via the Gemini API.
  /// </remarks>
  TFile = class
  private
    FFile: TFileContent;
  public
    /// <summary>
    /// Gets or sets the metadata content of the file.
    /// </summary>
    /// <value>
    /// An instance of <c>TFileContent</c> containing the file's metadata.
    /// </value>
    property &File: TFileContent read FFile write FFile;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a collection of file entities retrieved from the Gemini API, including pagination support.
  /// </summary>
  /// <remarks>
  /// The <c>TFiles</c> class contains an array of <c>TFileContent</c> objects, each representing the metadata and content of a file.
  /// Additionally, it includes a <c>NextPageToken</c> property to handle pagination when listing multiple files.
  /// This class is used to manage and navigate through the list of files available in the Gemini API.
  /// </remarks>
  TFiles = class
  private
    FFiles: TArray<TFileContent>;
    FNextPageToken: string;
  public
    /// <summary>
    /// An array of <c>TFileContent</c> objects representing the files retrieved from the API.
    /// </summary>
    /// <value>
    /// Each element in the array contains metadata such as the file's name, size, MIME type, and processing state.
    /// </value>
    property Files: TArray<TFileContent> read FFiles write FFiles;
    /// <summary>
    /// A token used to retrieve the next page of results in a paginated list of files.
    /// </summary>
    /// <remarks>
    /// Use this token in subsequent API requests to continue listing files from where the previous request left off.
    /// </remarks>
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Class defined for compatibility with asynchrony handling.
  /// </summary>
  TFileDelete = class
  end;

  /// <summary>
  /// Manages API routes for file operations within the Gemini API, including uploading and retrieving files.
  /// </summary>
  /// <remarks>
  /// The <c>TFilesRes</c> class inherits from <c>TGeminiAPIRoute</c> and provides methods for constructing necessary HTTP headers,
  /// extracting upload URIs from API responses, and performing raw file uploads. It serves as a foundational class for handling
  /// lower-level file operations, facilitating the interaction between the application and the Gemini API for file management tasks.
  /// </remarks>
  TFilesRes = class(TGeminiAPIRoute)
  strict private
    /// <summary>
    /// Constructs the HTTP headers required to obtain the upload URI for a specified file.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file for which the upload URI is being requested.
    /// </param>
    /// <returns>
    /// An array of <c>TNetHeader</c> objects containing the necessary headers for the URI request.
    /// </returns>
    function GetUriHeaders(const FilePath: string): TNetHeaders;
    /// <summary>
    /// Constructs the HTTP headers required to perform the actual file upload.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file that is to be uploaded.
    /// </param>
    /// <returns>
    /// An array of <c>TNetHeader</c> objects containing the necessary headers for the upload request.
    /// </returns>
    function GetUploadHeaders(const FilePath: string): TNetHeaders;
    /// <summary>
    /// Extracts the upload URI from the API response headers after initiating a file upload.
    /// </summary>
    /// <remarks>
    /// This method first constructs and sets the necessary custom headers to initiate a URI request
    /// for the specified file. It sends this request to the 'upload/v1beta/files' endpoint.
    /// If successful, the URI is extracted from the response header 'x-goog-upload-url', and any
    /// newline characters are removed. If no URI is provided by the server, this function will
    /// return an empty string, indicating the operation was unsuccessful.
    ///
    /// Use cases:
    /// - This method is called internally when attempting to upload a file, and a successful response
    ///   indicates that the server has provided a valid upload URI for the file.
    /// - If a display name is provided in the `ParamProc` parameter, it will be included as a
    ///   parameter in the URI request, which can be useful for descriptive logging and retrieval purposes.
    ///
    /// </remarks>
    /// <param name="FilePath">
    /// The local filesystem path to the file for which the upload URI is being requested.
    /// </param>
    /// <param name="ParamProc">
    /// An optional procedure to configure additional file parameters, such as a display name,
    /// before extracting the URI.
    /// </param>
    /// <returns>
    /// A string representing the upload URI extracted from the API response headers. If the
    /// request is unsuccessful, it returns an empty string.
    /// </returns>
    function ExtractUriFromHeaders(const FilePath: string; const ParamProc: TProc<TFileParams>): string;
    /// <summary>
    /// Attempts to obtain the upload URI for a specified file, optionally including a display name.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file for which the upload URI is being requested.
    /// </param>
    /// <param name="DisplayName">
    /// An optional human-readable name for the file. If provided, it will be included in the URI request parameters.
    /// </param>
    /// <returns>
    /// A string representing the obtained upload URI. If the display name is provided, it will be included in the URI parameters.
    /// </returns>
    function TryToObtainUriValue(const FilePath: string; const DisplayName: string = ''): string;
  protected
    /// <summary>
    /// Uploads a file to the Gemini API using the specified file path and display name.
    /// </summary>
    /// <param name="FilePath">
    /// The local path to the file to be uploaded.
    /// </param>
    /// <param name="DisplayName">
    /// An optional human-readable name for the file. If provided, it will be used as the display name in the API.
    /// </param>
    /// <returns>
    /// An instance of <c>TFile</c> representing the uploaded file's metadata.
    /// </returns>
    function UploadRaw(const FilePath: string; const DisplayName: string = ''): TFile;
  end;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TFile</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynFile</c> type extends the <c>TAsynParams&lt;TFile&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynFile = TAsynCallBack<TFile>;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TFiles</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynFiles</c> type extends the <c>TAsynParams&lt;TFiles&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynFiles = TAsynCallBack<TFiles>;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TFileDelete</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynFileDelete</c> type extends the <c>TAsynParams&lt;TFileDelete&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynFileDelete = TAsynCallBack<TFileDelete>;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TFileContent</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynFileContent</c> type extends the <c>TAsynParams&lt;TFileContent&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynFileContent = TAsynCallBack<TFileContent>;

  /// <summary>
  /// Provides high-level API routes for managing files within the Gemini API, including uploading, listing, deleting, and retrieving files both synchronously and asynchronously.
  /// </summary>
  /// <remarks>
  /// The <c>TFilesRoute</c> class inherits from <c>TFilesRes</c> and extends its functionality by offering both synchronous and asynchronous methods for file operations.
  /// It facilitates interactions with the Gemini API by encapsulating the complexities of API calls, allowing developers to easily perform file management tasks such as uploading files with optional display names, listing files with pagination support, deleting specific files, and retrieving file content.
  /// The asynchronous methods leverage callbacks to handle operations without blocking the main thread, enhancing the application's responsiveness.
  /// </remarks>
  TFilesRoute = class(TFilesRes)
  public
    /// <summary>
    /// Asynchronously uploads a file to the Gemini API with an optional display name.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file that is to be uploaded.
    /// </param>
    /// <param name="DisplayName">
    /// An optional human-readable name for the file. If provided, it will be used as the display name in the API.
    /// </param>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFile</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    /// AsynUpLoad('File_Path', 'display_name',
    ///    function : TAsynFile
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFile)
    ///        begin
    ///          // Trigger the success method
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynUpload(const FilePath: string; const DisplayName: string;
      CallBacks: TFunc<TAsynFile>); overload;
    /// <summary>
    /// Asynchronously uploads a file to the Gemini API without specifying a display name.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file that is to be uploaded.
    /// </param>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFile</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    /// AsynUpLoad('File_Path',
    ///    function : TAsynFile
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFile)
    ///        begin
    ///          // Trigger the success method
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynUpload(const FilePath: string; CallBacks: TFunc<TAsynFile>); overload;
    /// <summary>
    /// Asynchronously retrieves a list of files from the Gemini API.
    /// </summary>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFiles</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    ///   AsynList(
    ///    function : TAsynFiles
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFiles)
    ///        begin
    ///          // Trigger the success method
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynList(CallBacks: TFunc<TAsynFiles>); overload;
    /// <summary>
    /// Asynchronously retrieves a paginated list of files from the Gemini API.
    /// </summary>
    /// <param name="PageSize">
    /// The maximum number of files to return in the response.
    /// </param>
    /// <param name="PageToken">
    /// A token identifying the page of results to retrieve. This is typically obtained from a previous list response.
    /// </param>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFiles</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    ///  // Declare the variable "Next" as a string type earlier in the code.
    ///   AsynList(20, Next,
    ///    function : TAsynFiles
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFiles)
    ///        begin
    ///          // Trigger the success method
    ///          Next := Uploaded.NextPageToken;
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynList(const PageSize: Integer; const PageToken: string;
      CallBacks: TFunc<TAsynFiles>); overload;
    /// <summary>
    /// Asynchronously deletes a specified file from the Gemini API.
    /// </summary>
    /// <param name="FileName">
    /// The name of the file to be deleted.
    /// </param>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFileDelete</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    ///   AsynDelete('FileName_to_delete',
    ///    function : TAsynFileDelete
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFileDelete)
    ///        begin
    ///          // Trigger the success method
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynDelete(const FileName: string; CallBacks: TFunc<TAsynFileDelete>);
    /// <summary>
    /// Asynchronously retrieves the content and metadata of a specified file from the Gemini API.
    /// </summary>
    /// <param name="FileName">
    /// The name of the file to be retrieved.
    /// </param>
    /// <param name="CallBacks">
    /// A callback function that handles the asynchronous response encapsulated in a <c>TAsynFileContent</c> object.
    /// </param>
    /// <remarks>
    /// <code>
    ///   AsynRetrieve('FileName_to_retrieve',
    ///    function : TAsynFileContent
    ///    begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject)
    ///        begin
    ///           // Trigger the start method
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Uploaded: TFileContent)
    ///        begin
    ///          // Trigger the success method
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Trigger the error method
    ///        end;
    ///    end);
    /// </code>
    /// </remarks>
    procedure AsynRetrieve(const FileName: string; CallBacks: TFunc<TAsynFileContent>);
    /// <summary>
    /// Synchronously uploads a file to the Gemini API with an optional display name.
    /// </summary>
    /// <param name="FilePath">
    /// The local filesystem path to the file that is to be uploaded.
    /// </param>
    /// <param name="DisplayName">
    /// An optional human-readable name for the file. If provided, it will be used as the display name in the API.
    /// </param>
    /// <returns>
    /// An instance of <c>TFile</c> representing the uploaded file's metadata.
    /// </returns>
    function Upload(const FilePath: string; const DisplayName: string = ''): TFile;
    /// <summary>
    /// Synchronously retrieves a list of all files from the Gemini API.
    /// </summary>
    /// <returns>
    /// An instance of <c>TFiles</c> containing an array of <c>TFileContent</c> objects and a pagination token.
    /// </returns>
    function List: TFiles; overload;
    /// <summary>
    /// Synchronously retrieves a paginated list of files from the Gemini API.
    /// </summary>
    /// <param name="PageSize">
    /// The maximum number of files to return in the response.
    /// </param>
    /// <param name="PageToken">
    /// A token identifying the page of results to retrieve. This is typically obtained from a previous list response.
    /// </param>
    /// <returns>
    /// An instance of <c>TFiles</c> containing an array of <c>TFileContent</c> objects and a pagination token.
    /// </returns>
    function List(const PageSize: Integer; const PageToken: string): TFiles; overload;
    /// <summary>
    /// Synchronously deletes a specified file from the Gemini API.
    /// </summary>
    /// <param name="FileName">
    /// The name of the file to be deleted.
    /// </param>
    /// <returns>
    /// An instance of <c>TFileDelete</c> representing the result of the delete operation.
    /// </returns>
    function Delete(const FileName: string): TFileDelete;
    /// <summary>
    /// Synchronously retrieves the content and metadata of a specified file from the Gemini API.
    /// </summary>
    /// <param name="FileName">
    /// The name of the file to be retrieved.
    /// </param>
    /// <returns>
    /// An instance of <c>TFileContent</c> containing the file's metadata and content.
    /// </returns>
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
    raise Exception.Create('The server has not provided a URI as expected.');

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

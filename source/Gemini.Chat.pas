unit Gemini.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Gemini.API.Params, Gemini.API, Gemini.Safety, Gemini.Schema,
  Gemini.Tools, Gemini.Async.Support, Gemini.Functions.Core, Gemini.GoogleSearch;

type
  /// <summary>
  /// Kind of tool call
  /// </summary>
  TToolKind = (
    /// <summary>
    /// Enable code execution
    /// </summary>
    CodeExecution,
    /// <summary>
    /// Enable google search
    /// </summary>
    GoogleSearch
  );

  /// <summary>
  /// Type of message role
  /// </summary>
  TMessageRole = (
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    model
  );

  /// <summary>
  /// Helper record for the <c>TMessageRole</c> enumeration, providing utility methods for converting
  /// between <c>TMessageRole</c> values and their string representations.
  /// </summary>
  TMessageRoleHelper = record helper for TMessageRole
    /// <summary>
    /// Converts the current <c>TMessageRole</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TMessageRole</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TMessageRole</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TMessageRole</c>.
    /// </param>
    /// <returns>
    /// The <c>TMessageRole</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TMessageRole; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TMessageRole</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TMessageRole</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TMessageRoleInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TMessageRole</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TMessageRole</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TMessageRole</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TMessageRole</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TMessageRole</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TMessageRole</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TMessageRole</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Defines the reason why the model stopped generating tokens.
  /// </summary>
  TFinishReason = (
    /// <summary>
    /// Default value. This value is unused.
    /// </summary>
    FINISH_REASON_UNSPECIFIED,
    /// <summary>
    /// Natural stop point of the model or provided stop sequence.
    /// </summary>
    STOP,
    /// <summary>
    /// The maximum number of tokens as specified in the request was reached.
    /// </summary>
    MAX_TOKENS,
    /// <summary>
    /// The response candidate content was flagged for safety reasons.
    /// </summary>
    SAFETY,
    /// <summary>
    /// The response candidate content was flagged for recitation reasons.
    /// </summary>
    RECITATION,
    /// <summary>
    /// The response candidate content was flagged for using an unsupported language.
    /// </summary>
    LANGUAGE,
    /// <summary>
    ///  Unknown reason.
    /// </summary>
    OTHER,
    /// <summary>
    /// Token generation stopped because the content contains forbidden terms.
    /// </summary>
    BLOCKLIST,
    /// <summary>
    /// Token generation stopped for potentially containing prohibited content.
    /// </summary>
    PROHIBITED_CONTENT,
    /// <summary>
    /// Token generation stopped because the content potentially contains Sensitive Personally Identifiable Information (SPII).
    /// </summary>
    SPII,
    /// <summary>
    /// The function call generated by the model is invalid.
    /// </summary>
    MALFORMED_FUNCTION_CALL
  );

  /// <summary>
  /// Helper record for the <c>TFinishReason</c> enumeration, providing utility methods for converting
  /// between <c>TFinishReason</c> values and their string representations.
  /// </summary>
  TFinishReasonHelper = record helper for TFinishReason
    /// <summary>
    /// Converts the current <c>TFinishReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TFinishReason</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TFinishReason</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TFinishReason</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TFinishReason</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TFinishReason</c> values.
    /// </remarks>
    class function Create(const Value: string): TFinishReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TFinishReason</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TFinishReason</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TFinishReasonInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TFinishReason</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TFinishReason</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFinishReason</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TFinishReason</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TFinishReason</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TFinishReason</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TFinishReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Supported programming languages for the generated code.
  /// </summary>
  TLangueType = (
    /// <summary>
    /// Language not specified. This value should not be used.
    /// </summary>
    LANGUAGE_UNSPECIFIED,
    /// <summary>
    /// Python >= 3.10, with numpy and simpy available
    /// </summary>
    PYTHON
  );

  /// <summary>
  /// Helper record for the <c>TLangueType</c> enumeration, providing utility methods for conversion between string representations and <c>TLangueType</c> values.
  /// </summary>
  TLangueTypeHelper = record helper for TLangueType
    /// <summary>
    /// Converts the current <c>TLangueType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TLangueType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TLangueType</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TLangueType</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TLangueType</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TLangueType</c> values.
    /// </remarks>
    class function Create(const Value: string): TLangueType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TLangueType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TLangueType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TLangueTypeInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TLangueType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TLangueType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TLangueType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TLangueType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TLangueType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TLangueType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TLangueType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Enumeration of possible outcomes of the code execution.
  /// </summary>
  TOutcomeType = (
    /// <summary>
    /// State not specified. This value should not be used.
    /// </summary>
    OUTCOME_UNSPECIFIED,
    /// <summary>
    /// The code execution was successful.
    /// </summary>
    OUTCOME_OK,
    /// <summary>
    /// Code execution completed, but with failure. stderr should contain the reason.
    /// </summary>
    OUTCOME_FAILED,
    /// <summary>
    /// Code execution took too long and was canceled. Partial output may or may not be present.
    /// </summary>
    OUTCOME_DEADLINE_EXCEEDED
  );

  /// <summary>
  /// Helper record for the <c>TOutcomeType</c> enumeration, providing utility methods for conversion between string representations and <c>TOutcomeType</c> values.
  /// </summary>
  TOutcomeTypeHelper = record helper for TOutcomeType
    /// <summary>
    /// Converts the current <c>TOutcomeType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TOutcomeType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TOutcomeType</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TOutcomeType</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TOutcomeType</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TOutcomeType</c> values.
    /// </remarks>
    class function Create(const Value: string): TOutcomeType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TOutcomeType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TOutcomeType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TOutcomeTypeInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TOutcomeType</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TOutcomeType</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TOutcomeType</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TOutcomeType</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TOutcomeType</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TOutcomeType</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TOutcomeType</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Raw media bytes.
  /// </summary>
  /// <remarks>
  /// Text should not be sent as raw bytes, use the 'text' field.
  /// </remarks>
  TInlineData = class(TJSONParam)
    /// <summary>
    /// The IANA standard MIME type of the source data.
    /// </summary>
    /// <remarks>
    /// Examples: - image/png - image/jpeg If an unsupported MIME type is provided, an error will be returned.
    /// </remarks>
    function MimeType(const Value: string): TInlineData;
    /// <summary>
    /// Raw bytes for media formats.
    /// </summary>
    /// <remarks>
    /// A base64-encoded string.
    /// </remarks>
    function Data(const Value: string): TInlineData;
  end;

  /// <summary>
  /// URI based data.
  /// </summary>
  TFileData = class(TJSONParam)
    /// <summary>
    /// The IANA standard MIME type of the source data.
    /// </summary>
    function MimeType(const Value: string): TFileData;
    /// <summary>
    /// Uri of thsdata
    /// </summary>
    function FileUri(const Value: string): TFileData;
  end;

  /// <summary>
  /// Inline text setter class.
  /// </summary>
  TTextData = class(TJSONParam)
    /// <summary>
    /// Inline text.
    /// </summary>
    function Text(const Value: string): TTextData;
  end;

  /// <summary>
  /// Represents the content payload of a chat message, including the message sender's role and its content parts.
  /// </summary>
  /// <remarks>
  /// The <c>TContentPayload</c> class allows you to construct messages with various content parts, specify the role of the message sender (user or assistant), and attach any additional data such as files or media.
  /// This class is essential for building messages in a chat application, particularly when interacting with AI models that require structured message input.
  /// </remarks>
  TContentPayload = class(TJSONParam)
  public
    /// <summary>
    /// Sets the role of the message sender.
    /// </summary>
    /// <param name="Value">
    /// The role of the message sender, specified as a <c>TMessageRole</c> enumeration value (either <c>user</c> or <c>model</c>).
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentPayload</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Setting the role is useful for multi-turn conversations to distinguish between messages from the user and responses from the model.
    /// If not set, the role can be left blank or unset for single-turn conversations.
    /// </remarks>
    function Role(const Value: TMessageRole): TContentPayload;
    /// <summary>
    /// Adds content parts to the message with specified text and attachments.
    /// </summary>
    /// <param name="Value">
    /// The text content of the message to be included as a part.
    /// </param>
    /// <param name="Attached">
    /// An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentPayload</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Each part of the message may have different MIME types.
    /// This method allows combining text and attachments into a single message payload.
    /// </remarks>
    function Parts(const Value: string; const Attached: TArray<string>): TContentPayload; overload;
    /// <summary>
    /// Adds content parts to the message with specified attachments.
    /// </summary>
    /// <param name="Attached">
    /// An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TContentPayload</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Each part of the message may have different MIME types.
    /// This method is useful when the message consists only of attachments without any text content.
    /// </remarks>
    function Parts(const Attached: TArray<string>): TContentPayload; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance with specified text content and optional attachments.
    /// </summary>
    /// <param name="Text">
    /// The text content of the message.
    /// </param>
    /// <param name="Attached">
    /// Optional. An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance containing the specified text and attachments.
    /// </returns>
    class function Add(const Text: string; const Attached: TArray<string> = []): TContentPayload; reintroduce; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance with a specified role and optional attachments.
    /// </summary>
    /// <param name="Role">
    /// The role of the message sender, specified as a <c>TMessageRole</c> enumeration value (either <c>user</c> or <c>model</c>).
    /// </param>
    /// <param name="Attached">
    /// Optional. An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance with the specified role and attachments.
    /// </returns>
    class function Add(const Role: TMessageRole;
      const Attached: TArray<string> = []): TContentPayload; reintroduce; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance with specified role, text content, and optional attachments.
    /// </summary>
    /// <param name="Role">
    /// The role of the message sender, specified as a <c>TMessageRole</c> enumeration value (either <c>user</c> or <c>model</c>).
    /// </param>
    /// <param name="Text">
    /// The text content of the message.
    /// </param>
    /// <param name="Attached">
    /// Optional. An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance containing the specified role, text, and attachments.
    /// </returns>
    class function Add(const Role: TMessageRole; const Text: string;
      const Attached: TArray<string> = []): TContentPayload; reintroduce; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance representing the assistant's message with specified text content and optional attachments.
    /// </summary>
    /// <param name="Value">
    /// The text content of the assistant's message.
    /// </param>
    /// <param name="Attached">
    /// Optional. An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance representing the assistant's message.
    /// </returns>
    class function Assistant(const Value: string; const Attached: TArray<string> = []): TContentPayload; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance representing the assistant's message with specified attachments.
    /// </summary>
    /// <param name="Attached">
    /// An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance representing the assistant's message with attachments.
    /// </returns>
    class function Assistant(const Attached: TArray<string>): TContentPayload; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance representing the user's message with specified text content and optional attachments.
    /// </summary>
    /// <param name="Value">
    /// The text content of the user's message.
    /// </param>
    /// <param name="Attached">
    /// Optional. An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance representing the user's message.
    /// </returns>
    class function User(const Value: string; const Attached: TArray<string> = []): TContentPayload; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance representing the user's message with specified attachments.
    /// </summary>
    /// <param name="Attached">
    /// An array of strings representing attached data or file URIs to be included as parts.
    /// </param>
    /// <returns>
    /// Returns a new <c>TContentPayload</c> instance representing the user's message with attachments.
    /// </returns>
    class function User(const Attached: TArray<string>): TContentPayload; overload;
    /// <summary>
    /// Creates a new <c>TContentPayload</c> instance and allows configuration through a procedure reference.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure reference that receives a <c>TContentPayload</c> instance to configure its properties.
    /// </param>
    /// <returns>
    /// Returns a new configured <c>TContentPayload</c> instance.
    /// </returns>
    class function New(const ParamProc: TProcRef<TContentPayload>): TContentPayload; static;
  end;

  /// <summary>
  /// Represents the content payload of a chat message, including the message sender's role and its content parts.
  /// </summary>
  /// <remarks>
  /// The <c>TPayLoad</c> class allows you to construct messages with various content parts, specify the role of the message sender (user or assistant), and attach any additional data such as files or media.
  /// This class is essential for building messages in a chat application, particularly when interacting with AI models that require structured message input.
  /// </remarks>
  TPayLoad = TContentPayload;

  /// <summary>
  /// Represents the configuration options for generating model outputs in a chat or completion request.
  /// </summary>
  /// <remarks>
  /// The <c>TGenerationConfig</c> class allows you to specify various parameters that control the behavior of the AI model during text generation.
  /// These settings can influence the randomness, length, and format of the generated responses.
  /// Not all parameters are configurable for every model; some models may ignore certain settings.
  /// </remarks>
  TGenerationConfig = class(TJSONParam)
  public
    /// <summary>
    /// Specifies a set of character sequences that will stop the output generation.
    /// </summary>
    /// <param name="Value">
    /// An array of strings representing the stop sequences. The API will stop generating further tokens when any of these sequences are encountered.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// You can specify up to 5 stop sequences. If a stop sequence is encountered, it will not be included in the response.
    /// </remarks>
    function StopSequences(const Value: TArray<string>): TGenerationConfig;
    /// <summary>
    /// Sets the MIME type of the generated candidate text.
    /// </summary>
    /// <param name="Value">
    /// A string representing the MIME type of the response content. Supported MIME types include:
    /// <para>
    /// - <c>text/plain</c> (default): Text output.
    /// </para>
    /// <para>
    /// - <c>application/json</c>: JSON response in the response candidates.
    /// </para>
    /// <para>
    /// - <c>text/x.enum</c>: ENUM as a string response in the response candidates.
    /// </para>
    /// Refer to the documentation for a complete list of supported MIME types.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    function ResponseMimeType(const Value: string): TGenerationConfig;
    /// <summary>
    /// Specifies the output schema of the generated candidate text using a <c>TSchemaParams</c> instance.
    /// </summary>
    /// <param name="Value">
    /// A <c>TSchemaParams</c> instance representing the desired output schema for the response.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Schemas must be a subset of the OpenAPI schema and can be objects, primitives, or arrays.
    /// If set, a compatible <c>ResponseMimeType</c> must also be specified. Compatible MIME types include <c>application/json</c>.
    /// Refer to the JSON text generation guide for more details.
    /// </remarks>
    function ResponseSchema(const Value: TSchemaParams): TGenerationConfig; overload;
    /// <summary>
    /// Specifies the output schema of the generated candidate text using a JSON object.
    /// </summary>
    /// <param name="Value">
    /// A <c>TJSONObject</c> instance representing the desired output schema for the response.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Schemas must be a subset of the OpenAPI schema and can be objects, primitives, or arrays.
    /// If set, a compatible <c>ResponseMimeType</c> must also be specified. Compatible MIME types include <c>application/json</c>.
    /// Refer to the JSON text generation guide for more details.
    /// </remarks>
    function ResponseSchema(const Value: TJSONObject): TGenerationConfig; overload;
    /// <summary>
    /// Specifies the output schema of the generated candidate text using a procedure to configure <c>TSchemaParams</c>.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure reference that receives a <c>TSchemaParams</c> instance to configure the desired output schema.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Schemas must be a subset of the OpenAPI schema and can be objects, primitives, or arrays.
    /// If set, a compatible <c>ResponseMimeType</c> must also be specified. Compatible MIME types include <c>application/json</c>.
    /// Refer to the JSON text generation guide for more details.
    /// </remarks>
    function ResponseSchema(const ParamProc: TProcRef<TSchemaParams>): TGenerationConfig; overload;
    /// <summary>
    /// Sets the number of generated response candidates to return.
    /// </summary>
    /// <param name="Value">
    /// An integer specifying the number of response candidates. Currently, this value can only be set to 1.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// If not set, this defaults to 1.
    /// </remarks>
    function CandidateCount(const Value: Integer): TGenerationConfig;
    /// <summary>
    /// Specifies the maximum number of tokens to include in a response candidate.
    /// </summary>
    /// <param name="Value">
    /// An integer representing the maximum number of tokens for the response.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// The default value varies by model. Refer to the model's <c>output_token_limit</c> attribute returned from the <c>getModel</c> function.
    /// </remarks>
    function MaxOutputTokens(const Value: Integer): TGenerationConfig;
    /// <summary>
    /// Controls the randomness of the output by setting the temperature parameter.
    /// </summary>
    /// <param name="Value">
    /// A double value ranging from 0.0 to 2.0, where higher values produce more random outputs, and lower values make the output more deterministic.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// The default value varies by model. Refer to the model's <c>temperature</c> attribute returned from the <c>getModel</c> function.
    /// </remarks>
    function Temperature(const Value: Double): TGenerationConfig;
    /// <summary>
    /// Sets the maximum cumulative probability of tokens to consider when sampling, known as nucleus sampling (Top-p).
    /// </summary>
    /// <param name="Value">
    /// A double value between 0.0 and 1.0 representing the cumulative probability threshold.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// The model uses a combination of Top-k and Top-p sampling. Tokens are considered based on their cumulative probability until the threshold is reached.
    /// The default value varies by model and is specified by the model's <c>top_p</c> attribute returned from the <c>getModel</c> function.
    /// </remarks>
    function TopP(const Value: Double): TGenerationConfig;
    /// <summary>
    /// Sets the maximum number of tokens to consider when sampling, known as Top-k sampling.
    /// </summary>
    /// <param name="Value">
    /// An integer specifying the number of top tokens to consider during sampling.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Some models may not support Top-k sampling. The default value varies by model and is specified by the model's <c>top_k</c> attribute returned from the <c>getModel</c> function.
    /// </remarks>
    function TopK(const Value: Integer): TGenerationConfig;
    /// <summary>
    /// Applies a presence penalty to the next token's log probabilities if the token has already appeared in the response.
    /// </summary>
    /// <param name="Value">
    /// A double value representing the penalty. Positive values discourage repetition, while negative values encourage it.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// This penalty is binary (on/off) and does not depend on the number of times the token has been used after the first occurrence.
    /// Use <c>FrequencyPenalty</c> for a penalty that increases with each use.
    /// </remarks>
    function PresencePenalty(const Value: Double): TGenerationConfig;
    /// <summary>
    /// Applies a frequency penalty to the next token's log probabilities, proportional to the number of times each token has been used in the response so far.
    /// </summary>
    /// <param name="Value">
    /// A double value representing the penalty. Positive values discourage repetition proportionally to frequency; negative values encourage repetition.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Caution: Negative values can cause the model to start repeating tokens excessively, potentially leading to outputs like "the the the...".
    /// </remarks>
    function FrequencyPenalty(const Value: Double): TGenerationConfig;
    /// <summary>
    /// Specifies whether to include log probabilities of tokens in the response.
    /// </summary>
    /// <param name="Value">
    /// A boolean value where <c>True</c> indicates that log probabilities should be included in the response.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    function ResponseLogprobs(const Value: Boolean): TGenerationConfig;
    /// <summary>
    /// Sets the number of top log probabilities to return at each decoding step.
    /// </summary>
    /// <param name="Value">
    /// An integer specifying the number of top tokens to include in the log probabilities.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TGenerationConfig</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Only valid if <c>ResponseLogprobs</c> is set to <c>True</c>. This parameter determines the number of top tokens for which log probabilities are returned in each decoding step.
    /// </remarks>
    function Logprobs(const Value: Integer): TGenerationConfig;
    /// <summary>
    /// Creates a new <c>TGenerationConfig</c> instance and allows configuration through a procedure reference.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure reference that receives a <c>TGenerationConfig</c> instance to configure its properties.
    /// </param>
    /// <returns>
    /// Returns a new configured <c>TGenerationConfig</c> instance.
    /// </returns>
    class function New(const ParamProc: TProcRef<TGenerationConfig>): TGenerationConfig; overload;
  end;

  /// <summary>
  /// Represents the set of parameters used to configure a chat interaction with an AI model.
  /// </summary>
  /// <remarks>
  /// The <c>TChatParams</c> class allows you to define various settings that control how the AI model behaves during a chat session.
  /// You can specify the messages to send, tools the model can use, safety settings, system instructions, and generation configurations.
  /// By customizing these parameters, you can fine-tune the AI's responses to better suit your application's needs.
  /// </remarks>
  TChatParams = class(TJSONParam)
    /// <summary>
    /// Sets the content of the current conversation with the model.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TContentPayload</c> instances representing the messages exchanged in the conversation, including both user and assistant messages.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// For single-turn queries, this array contains a single message. For multi-turn conversations, include the entire conversation history and the latest message.
    /// </remarks>
    function Contents(const Value: TArray<TContentPayload>): TChatParams;
    /// <summary>
    /// Specifies a list of tools that the model may use to generate the next response.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>IFunctionCore</c> instances representing the tools available to the model.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// A tool is a piece of code that allows the model to interact with external systems or perform actions outside its knowledge base.
    /// Supported tools include functions and code execution capabilities. Refer to the Function Calling and Code Execution guides for more information.
    /// </remarks>
    function Tools(const Value: TArray<IFunctionCore>): TChatParams; overload;
    /// <summary>
    /// Enables a specific tool for the chat interaction based on the provided tool kind and threshold.
    /// </summary>
    /// <param name="Value">
    /// Specifies the type of tool to enable. This can be either <c>TToolKind.CodeExecution</c> to allow code execution capabilities or <c>TToolKind.GoogleSearch</c> to enable Google search functionality.
    /// </param>
    /// <param name="Threshold">
    /// An optional parameter that sets the activation threshold for the specified tool. The default value is <c>0.7</c>.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, facilitating method chaining for configuring multiple parameters.
    /// </returns>
    /// <remarks>
    /// This method allows you to specify additional functionalities that the AI model can utilize during the chat session.
    /// <para>
    /// - <c>TToolKind.CodeExecution</c>: Enables the model to generate and execute code snippets, which can be useful for tasks requiring computational operations or automation.
    /// </para>
    /// <para>
    /// - <c>TToolKind.GoogleSearch</c>: Allows the model to perform Google searches to fetch real-time information, enhancing its ability to provide up-to-date responses.
    /// </para>
    /// The <c>Threshold</c> parameter determines the sensitivity or confidence level required for the tool to be activated. Adjusting this value can help control how often the tool is utilized based on the context of the conversation.
    /// </remarks>
    function Tools(const Value: TToolKind; const Threshold: Double = 0.7): TChatParams; overload;
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
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify how the model should use the available tools, including restricting which functions can be called.
    /// Refer to the Function Calling guide for usage examples.
    /// </remarks>
    function ToolConfig(const Value: TToolMode; AllowedFunctionNames: TArray<string> = []): TChatParams;
    /// <summary>
    /// Specifies safety settings to block unsafe content.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TSafety</c> instances representing safety settings for different harm categories.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// These settings are enforced on both the request and the response.
    /// There should not be more than one setting for each safety category.
    /// The API will block any content that fails to meet the thresholds set by these settings.
    /// This list overrides the default settings for each specified category.
    /// If a category is not specified, the API uses the default safety setting for that category.
    /// Supported harm categories include <c>HARM_CATEGORY_HATE_SPEECH</c>, <c>HARM_CATEGORY_SEXUALLY_EXPLICIT</c>, <c>HARM_CATEGORY_DANGEROUS_CONTENT</c>, and <c>HARM_CATEGORY_HARASSMENT</c>.
    /// Refer to the documentation for detailed information on available safety settings and how to incorporate safety considerations into your application.
    /// </remarks>
    function SafetySettings(const Value: TArray<TSafety>): TChatParams;
    /// <summary>
    /// Sets developer-defined system instructions for the model.
    /// </summary>
    /// <param name="Value">
    /// A string containing the system instruction text.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this to provide guidelines or constraints for the model's behavior during the chat session.
    /// </remarks>
    function SystemInstruction(const Value: string): TChatParams;
    /// <summary>
    /// Configures generation options for the model's outputs.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure reference that receives a <c>TGenerationConfig</c> instance to configure various generation settings.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify parameters such as temperature, maximum tokens, response format, and other generation options.
    /// Not all parameters are configurable for every model.
    /// </remarks>
    function GenerationConfig(const ParamProc: TProcRef<TGenerationConfig>): TChatParams;
    /// <summary>
    /// Specifies the name of the cached content to use as context for the prediction.
    /// </summary>
    /// <param name="Value">
    /// A string representing the name of the cached content in the format <c>"cachedContents/{cachedContent}"</c>.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TChatParams</c> instance, allowing for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this to provide additional context to the model by referencing pre-cached content.
    /// </remarks>
    function CachedContent(const Value: string): TChatParams;
    /// <summary>
    /// Creates a new <c>TChatParams</c> instance and allows configuration through a procedure reference.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure reference that receives a <c>TChatParams</c> instance to configure its properties.
    /// </param>
    /// <returns>
    /// Returns a new configured <c>TChatParams</c> instance.
    /// </returns>
    class function New(const ParamProc: TProcRef<TChatParams>): TChatParams; overload;
  end;

  /// <summary>
  /// Interceptor class for converting <c>args</c> and <c>response</c> values into JSON string format in JSON deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>args</c>, <c>response</c> and theirs string equivalent during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TArgsFixInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// When JSON deserialization, converts <c>args</c>, <c>response<c/> values into JSON string to retrieve arguments made by the tool.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>input</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>args</c> or <c>response</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>args</c> or <c>response</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// A predicted FunctionCall returned from the model that contains a string representing the FunctionDeclaration.name with the arguments and their values.
  /// </summary>
  TFunctionCall = class
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TArgsFixInterceptor)]
    FArgs: string;
  public
    /// <summary>
    /// Required. The name of the function to call. Must be a-z, A-Z, 0-9, or contain underscores and dashes, with a maximum length of 63.
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Optional. The function parameters and values in JSON object format.
    /// </summary>
    property Args: string read FArgs write FArgs;
  end;

  /// <summary>
  /// Raw media bytes.
  /// </summary>
  /// <remarks>
  /// Text should not be sent as raw bytes, use the 'text' field.
  /// </remarks>
  TInlineDataPart = class
  private
    FMimeType: string;
    FData: string;
  public
    /// <summary>
    /// The IANA standard MIME type of the source data.
    /// </summary>
    property MimeType: string read FMimeType write FMimeType;
    /// <summary>
    /// Raw bytes for media formats.
    /// </summary>
    property Data: string read FData write FData;
  end;

  /// <summary>
  /// URI based data.
  /// </summary>
  TFileDataPart = class
  private
    FMimeType: string;
    FFileUri: string;
  public
    /// <summary>
    /// The IANA standard MIME type of the source data.
    /// </summary>
    property MimeType: string read FMimeType write FMimeType;
    /// <summary>
    /// Uri of the data
    /// </summary>
    property FileUri: string read FFileUri write FFileUri;
  end;

  /// <summary>
  /// The result output from a FunctionCall that contains a string representing the FunctionDeclaration.name and a structured JSON object containing any output from the function is used as context to the model.
  /// </summary>
  /// <remarks>
  /// This should contain the result of aFunctionCall made based on model prediction.
  /// </remarks>
  TFunctionResponsePart = class
  private
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TArgsFixInterceptor)]
    FResponse: string;
  public
    /// <summary>
    /// Required. The name of the function to call.
    /// </summary>
    /// <remarks>
    /// Must be a-z, A-Z, 0-9, or contain underscores and dashes, with a maximum length of 63.
    /// </remarks>
    property Name: string read FName write FName;
    /// <summary>
    /// Required. The function response in JSON object format.
    /// </summary>
    property Response: string read FResponse write FResponse;
  end;

  /// <summary>
  /// Code generated by the model that is meant to be executed, and the result returned to the model.
  /// </summary>
  /// <remarks>
  /// Only generated when using the CodeExecution tool, in which the code will be automatically executed, and a corresponding CodeExecutionResult will also be generated.
  /// </remarks>
  TExecutableCodePart = class
  private
    [JsonReflectAttribute(ctString, rtString, TLangueTypeInterceptor)]
    FLanguage: TLangueType;
    FCode: string;
  public
    /// <summary>
    /// Required. Programming language of the code.
    /// </summary>
    property Language: TLangueType read FLanguage write FLanguage;
    /// <summary>
    /// Required. The code to be executed.
    /// </summary>
    property Code: string read FCode write FCode;
  end;

  /// <summary>
  /// Result of executing the ExecutableCode.
  /// </summary>
  /// <remarks>
  /// Only generated when using the CodeExecution, and always follows a part containing the ExecutableCode.
  /// </remarks>
  TCodeExecutionResult = class
  private
    [JsonReflectAttribute(ctString, rtString, TOutcomeTypeInterceptor)]
    FOutcome: TOutcomeType;
    FOutput: string;
  public
    /// <summary>
    /// Required. Outcome of the code execution.
    /// </summary>
    property Outcome: TOutcomeType read FOutcome write FOutcome;
    /// <summary>
    /// Optional. Contains stdout when code execution is successful, stderr or other description otherwise.
    /// </summary>
    property Output: string read FOutput write FOutput;
  end;

  /// <summary>
  /// A datatype containing media that is part of a multi-part Content message.
  /// </summary>
  /// <remarks>
  /// <para>
  /// - A Part consists of data which has an associated datatype. A Part can only contain one of the accepted types in Part.data.
  /// </para>
  /// <para>
  /// - A Part must have a fixed IANA MIME type identifying the type and subtype of the media if the inlineData field is filled with raw bytes.
  /// </para>
  /// <para>
  /// Data can be only one of the following field of the class
  /// </para>
  /// </remarks>
  TChatPart = class
  private
    FText: string;
    FFunctionCall: TFunctionCall;
    FFunctionResponse: TFunctionResponsePart;
    FInlineData: TInlineDataPart;
    FFileData: TFileDataPart;
    FExecutableCode: TExecutableCodePart;
    FCodeExecutionResult: TCodeExecutionResult;
  public
    /// <summary>
    /// Inline text.
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// A predicted FunctionCall returned from the model that contains a string representing the FunctionDeclaration.name with the arguments and their values.
    /// </summary>
    property FunctionCall: TFunctionCall read FFunctionCall write FFunctionCall;
    /// <summary>
    /// The result output of a FunctionCall that contains a string representing the FunctionDeclaration.name and a structured JSON object containing any output from the function is used as context to the model.
    /// </summary>
    property FunctionResponse: TFunctionResponsePart read FFunctionResponse write FFunctionResponse;
    /// <summary>
    /// Inline media bytes.
    /// </summary>
    property InlineData: TInlineDataPart read FInlineData write FInlineData;
    /// <summary>
    /// URI based data.
    /// </summary>
    property FileData: TFileDataPart read FFileData write FFileData;
    /// <summary>
    /// Code generated by the model that is meant to be executed.
    /// </summary>
    property ExecutableCode: TExecutableCodePart read FExecutableCode write FExecutableCode;
    /// <summary>
    /// Result of executing the ExecutableCode.
    /// </summary>
    property CodeExecutionResult: TCodeExecutionResult read FCodeExecutionResult write FCodeExecutionResult;
    destructor Destroy; override;
  end;

  /// <summary>
  /// The base structured datatype containing multi-part content of a message.
  /// </summary>
  /// <remarks>
  /// A Content includes a role field designating the producer of the Content and a parts field containing multi-part data that contains the content of the message turn.
  /// </remarks>
  TChatContent = class
  private
    FParts: TArray<TChatPart>;
    [JsonReflectAttribute(ctString, rtString, TMessageRoleInterceptor)]
    FRole: TMessageRole;
  public
    /// <summary>
    /// Ordered Parts that constitute a single message. Parts may have different MIME types.
    /// </summary>
    property Parts: TArray<TChatPart> read FParts write FParts;
    /// <summary>
    /// Optional. The producer of the content. Must be either 'user' or 'model'.
    /// </summary>
    /// <remarks>
    /// Useful to set for multi-turn conversations, otherwise can be left blank or unset.
    /// </remarks>
    property Role: TMessageRole read FRole write FRole;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Safety rating for a piece of content.
  /// </summary>
  /// <remarks>
  /// The safety rating contains the category of harm and the harm probability level in that category for a piece of content. Content is classified for safety across a number of harm categories and the probability of the harm classification is included here.
  /// </remarks>
  TSafetyRatings = class
  private
    [JsonReflectAttribute(ctString, rtString, THarmCategoryInterceptor)]
    FCategory: THarmCategory;
    [JsonReflectAttribute(ctString, rtString, THarmProbabilityInterceptor)]
    FProbability: THarmProbability;
    FBlocked: Boolean;
  public
    /// <summary>
    /// Required. The category for this rating.
    /// </summary>
    property Category: THarmCategory read FCategory write FCategory;
    /// <summary>
    /// Required. The probability of harm for this content.
    /// </summary>
    property Probability: THarmProbability read FProbability write FProbability;
    /// <summary>
    /// Was this content blocked because of this rating?
    /// </summary>
    property Blocked: Boolean read FBlocked write FBlocked;
  end;

  /// <summary>
  /// A citation to a source for a portion of a specific response.
  /// </summary>
  TCitationSource = class
  private
    FStartIndex: Int64;
    FEndIndex: Int64;
    FUri: string;
    FLicense: string;
  public
    /// <summary>
    /// Optional. Start of segment of the response that is attributed to this source.
    /// </summary>
    /// <remarks>
    /// Index indicates the start of the segment, measured in bytes
    /// </remarks>
    property StartIndex: Int64 read FStartIndex write FStartIndex;
    /// <summary>
    /// Optional. End of the attributed segment, exclusive.
    /// </summary>
    property EndIndex: Int64 read FEndIndex write FEndIndex;
    /// <summary>
    /// Optional. URI that is attributed as a source for a portion of the text.
    /// </summary>
    property Uri: string read FUri write FUri;
    /// <summary>
    /// Optional. License for the GitHub project that is attributed as a source for segment.
    /// </summary>
    /// <remarks>
    /// License info is required for code citations.
    /// </remarks>
    property License: string read FLicense write FLicense;
  end;

  /// <summary>
  /// A collection of source attributions for a piece of content.
  /// </summary>
  TCitationMetadata = class
  private
    FCitationSources: TArray<TCitationSource>;
  public
    /// <summary>
    /// Citations to sources for a specific response.
    /// </summary>
    property CitationSources: TArray<TCitationSource> read FCitationSources write FCitationSources;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Candidate for the logprobs token and score.
  /// </summary>
  TCandidate = class
  private
    FToken: string;
    FTokenId: Int64;
    FLogProbability: Double;
  public
    /// <summary>
    /// The candidate’s token string value.
    /// </summary>
    property Token: string read FToken write FToken;
    /// <summary>
    /// The candidate’s token id value.
    /// </summary>
    property TokenId: Int64 read FTokenId write FTokenId;
    /// <summary>
    /// The candidate's log probability.
    /// </summary>
    property LogProbability: Double read FLogProbability write FLogProbability;
  end;

  /// <summary>
  /// Candidates with top log probabilities at each decoding step.
  /// </summary>
  TTopCandidates = class
  private
    FCandidates: TArray<TCandidate>;
  public
    /// <summary>
    /// Sorted by log probability in descending order.
    /// </summary>
    property Candidates: TArray<TCandidate> read Fcandidates write Fcandidates;
  end;

  /// <summary>
  /// Logprobs Result
  /// </summary>
  TLogprobsResult = class
  private
    FTopCandidates: TArray<TTopCandidates>;
    FChosenCandidates: TArray<TCandidate>;
  public
    /// <summary>
    /// Length = total number of decoding steps.
    /// </summary>
    property TopCandidates: TArray<TTopCandidates> read FTopCandidates write FTopCandidates;
    /// <summary>
    /// Length = total number of decoding steps. The chosen candidates may or may not be in topCandidates.
    /// </summary>
    property ChosenCandidates: TArray<TCandidate> read FChosenCandidates write FChosenCandidates;
    destructor Destroy; override;
  end;

  /// <summary>
  /// A response candidate generated from the model.
  /// </summary>
  TChatCandidate = class
  private
    FContent: TChatContent;
    [JsonReflectAttribute(ctString, rtString, TFinishReasonInterceptor)]
    FFinishReason: TFinishReason;
    FSafetyRatings: TArray<TSafetyRatings>;
    FCitationMetadata: TCitationMetadata;
    FTokenCount: Int64;
    FAvgLogprobs: Double;
    FLogprobsResult: TLogprobsResult;
    FGroundingMetadata: TGroundingMetadata;
    FIndex: Int64;
  public
    /// <summary>
    /// Output only. Generated content returned from the model.
    /// </summary>
    property Content: TChatContent read FContent write FContent;
    /// <summary>
    /// Optional. Output only. The reason why the model stopped generating tokens.
    /// </summary>
    /// <remarks>
    /// If empty, the model has not stopped generating tokens.
    /// </remarks>
    property FinishReason: TFinishReason read FFinishReason write FFinishReason;
    /// <summary>
    /// List of ratings for the safety of a response candidate.
    /// </summary>
    /// <remarks>
    /// There is at most one rating per category.
    /// </remarks>
    property SafetyRatings: TArray<TSafetyRatings> read FSafetyRatings write FSafetyRatings;
    /// <summary>
    /// Output only. Citation information for model-generated candidate.
    /// </summary>
    /// <summary>
    /// This field may be populated with recitation information for any text included in the content. These are passages that are "recited" from copyrighted material in the foundational LLM's training data.
    /// </summary>
    property CitationMetadata: TCitationMetadata read FCitationMetadata write FCitationMetadata;
    /// <summary>
    /// Output only. Token count for this candidate.
    /// </summary>
    property TokenCount: Int64 read FTokenCount write FTokenCount;
    /// <summary>
    /// Output only.
    /// </summary>
    property AvgLogprobs: Double read FAvgLogprobs write FAvgLogprobs;
    /// <summary>
    /// Output only. Log-likelihood scores for the response tokens and top tokens
    /// </summary>
    property LogprobsResult: TLogprobsResult read FLogprobsResult write FLogprobsResult;
    /// <summary>
    /// Contains metadata related to the grounding of the chat candidate's content.
    /// </summary>
    /// <remarks>
    /// The <c>GroundingMetadata</c> property provides detailed information about the sources and contextual grounding of the generated content.
    /// <para><c>SearchEntryPoint</c>: The initial point or context from which the search was conducted.</para>
    /// <para><c>GroundingChunks</c>: A collection of web sources that support the generated content, each containing a URI and title.</para>
    /// <para><c>WebSearchQueries</c>: The specific search queries used to retrieve information relevant to the content.</para>
    /// This metadata enhances transparency by linking the generated responses to their original sources, facilitating verification and trust in the content provided by the model.
    /// </remarks>
    property GroundingMetadata: TGroundingMetadata read FGroundingMetadata write FGroundingMetadata;
    /// <summary>
    /// Output only. Index of the candidate in the list of response candidates.
    /// </summary>
    property Index: Int64 read FIndex write FIndex;
    destructor Destroy; override;
  end;

  /// <summary>
  /// A set of the feedback metadata the prompt specified in GenerateContentRequest.content.
  /// </summary>
  TPromptFeedback = class
  private
    [JsonReflectAttribute(ctString, rtString, TBlockReasonInterceptor)]
    FBlockReason: TBlockReason;
    FSafetyRatings: TArray<TsafetyRatings>;
  public
    /// <summary>
    /// Optional. If set, the prompt was blocked and no candidates are returned. Rephrase the prompt.
    /// </summary>
    property BlockReason: TBlockReason read FBlockReason write FBlockReason;
    /// <summary>
    /// Ratings for safety of the prompt. There is at most one rating per category.
    /// </summary>
    property SafetyRatings: TArray<TsafetyRatings> read FSafetyRatings write FSafetyRatings;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Contains metadata about the token usage in a generation request.
  /// </summary>
  /// <remarks>
  /// The <c>TUsageMetadata</c> class provides detailed information regarding the number of tokens used during a request to the AI model.
  /// This includes counts for the prompt, any cached content, the generated response candidates, and the total tokens used.
  /// This information is valuable for monitoring and optimizing token usage, which can impact both performance and cost.
  /// </remarks>
  TUsageMetadata = class
  private
    FPromptTokenCount: Int64;
    FCachedContentTokenCount: Int64;
    FCandidatesTokenCount: Int64;
    FTotalTokenCount: Int64;
  public
    /// <summary>
    /// Gets or sets the number of tokens used in the prompt.
    /// </summary>
    /// <remarks>
    /// When <c>CachedContent</c> is set, this value still represents the total effective prompt size, including the tokens in the cached content.
    /// </remarks>
    property PromptTokenCount: Int64 read FPromptTokenCount write FPromptTokenCount;
    /// <summary>
    /// Gets or sets the number of tokens in the cached part of the prompt (the cached content).
    /// </summary>
    property CachedContentTokenCount: Int64 read FCachedContentTokenCount write FCachedContentTokenCount;
    /// <summary>
    /// Gets or sets the total number of tokens across all the generated response candidates.
    /// </summary>
    property CandidatesTokenCount: Int64 read FCandidatesTokenCount write FCandidatesTokenCount;
    /// <summary>
    /// Gets or sets the total token count for the generation request, including both the prompt and the response candidates.
    /// </summary>
    property TotalTokenCount: Int64 read FTotalTokenCount write FTotalTokenCount;
  end;

  /// <summary>
  /// Response from the model supporting multiple candidate responses.
  /// </summary>
  /// <remarks>
  /// Safety ratings and content filtering are reported for both prompt in GenerateContentResponse.prompt_feedback and for each candidate in finishReason and in safetyRatings. The API: - Returns either all requested candidates or none of them - Returns no candidates at all only if there was something wrong with the prompt (check promptFeedback) - Reports feedback on each candidate in finishReason and safetyRatings.
  /// </remarks>
  TChat = class
  private
    FCandidates: TArray<TChatCandidate>;
    FPromptFeedback: TPromptFeedback;
    FUsageMetadata: TUsageMetadata;
  public
    /// <summary>
    /// Candidate responses from the model.
    /// </summary>
    property Candidates: TArray<TChatCandidate> read FCandidates write FCandidates;
    /// <summary>
    /// Returns the prompt's feedback related to the content filters.
    /// </summary>
    property PromptFeedback: TPromptFeedback read FPromptFeedback write FPromptFeedback;
    /// <summary>
    /// Output only. Metadata on the generation requests' token usage.
    /// </summary>
    property UsageMetadata: TUsageMetadata read FUsageMetadata write FUsageMetadata;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = reference to procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = TAsynCallBack<TChat>;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = TAsynStreamCallBack<TChat>;

  /// <summary>
  /// The <c>TChatRoute</c> class inherits from <c>TGeminiAPIRoute</c> and provides an interface for managing various interactions with the chat API.
  /// It supports creating chat completion requests in synchronous, asynchronous, and streaming modes, offering mechanisms to handle responses generated by the model.
  /// </summary>
  /// <remarks>
  /// This class facilitates sending messages to a chat model, receiving responses, and managing them, whether synchronously or asynchronously.
  /// The primary methods in the class are:
  /// <para>
  /// - <c>Create</c> : Sends a chat request and waits for a full response.
  /// </para>
  /// <para>
  /// - <c>AsynCreate</c> : Performs an asynchronous chat completion request with event handling.
  /// </para>
  /// <para>
  /// - <c>CreateStream</c> : Initiates a chat completion request in streaming mode, receiving tokens progressively.
  /// </para>
  /// <para>
  /// - <c>ASynCreateStream</c> : Performs an asynchronous request in streaming mode with event handling.
  /// </para>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including callbacks for processing responses or errors.
  /// </remarks>
  TChatRoute = class(TGeminiAPIRoute)
  private
    procedure ResetStream;
  public
    /// <summary>
    /// Create an asynchronous completion for chat message
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the chat request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided callBacks.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var Gemini := TGeminiFactory.CreateInstance(BaererKey);
    /// Gemini.Chat.AsynCreate(
    ///   procedure (Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///   function: TAsynChat
    ///   begin
    ///     Result.Sender := Memo1;  // Instance passed to callback parameter
    ///
    ///     Result.OnStart := nil;   // If nil then; Can be omitted
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Chat: TChat)
    ///     begin
    ///       var M := Sender as TMemo; // Because Result.Sender = Memo1
    ///       // Handle success operation
    ///     end;
    ///
    ///     Result.OnError := procedure (Sender: TObject; Value: string)
    ///     begin
    ///       // Handle error message
    ///     end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(const ModelName: string; ParamProc: TProc<TChatParams>;
      CallBacks: TFunc<TAsynChat>);
    /// <summary>
    /// Creates an asynchronous streaming chat completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynChatStream</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// CheckBox1.Checked := False;  //Click to stop the streaming
    /// // WARNING - Move the following line into the main OnCreate
    /// //var Gemini := TGeminiFactory.CreateInstance(BaererKey);
    /// Gemini.Chat.AsynCreateStream(
    ///   procedure(Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///
    ///   function: TAsynChatStream
    ///   begin
    ///     Result.Sender := Memo1; // Instance passed to callback parameter
    ///     Result.OnProgress :=
    ///         procedure (Sender: TObject; Chat: TChat)
    ///         begin
    ///           // Handle progressive updates to the chat response
    ///         end;
    ///     Result.OnSuccess :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Handle success when the operation completes
    ///         end;
    ///     Result.OnError :=
    ///         procedure (Sender: TObject; Value: string)
    ///         begin
    ///           // Handle error message
    ///         end;
    ///     Result.OnDoCancel :=
    ///         function: Boolean
    ///         begin
    ///           Result := CheckBox1.Checked; // Click on checkbox to cancel
    ///         end;
    ///     Result.OnCancellation :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Processing when process has been canceled
    ///         end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreateStream(const ModelName: string; ParamProc: TProc<TChatParams>;
      CallBacks: TFunc<TAsynChatStream>);
    /// <summary>
    /// Creates a completion for the chat message using the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, setting token limits, etc.
    /// </param>
    /// <returns>
    /// Returns a <c>TChat</c> object that contains the chat response, including the choices generated by the model.
    /// </returns>
    /// <exception cref="GeminiExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="GeminiExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The <c>Create</c> method sends a chat completion request and waits for the full response. The returned <c>TChat</c> object contains the model's generated response, including multiple choices if available.
    ///
    /// Example usage:
    /// <code>
    ///   var Gemini := TGeminiFactory.CreateInstance(BaererKey);
    ///   var Chat := Gemini.Chat.Create(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     for var Item in Chat.Candidates do
    ///       WriteLn(Item.Text);
    ///   finally
    ///     Chat.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Create(const ModelName: string; ParamProc: TProc<TChatParams>): TChat;
    /// <summary>
    /// Creates a chat message completion with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, and adjusting other settings like token limits or temperature.
    /// </param>
    /// <param name="Event">
    /// A callback of type <c>TChatEvent</c> that is triggered with each chunk of data received during the streaming process. It includes the current state of the <c>TChat</c> object, a flag indicating if the stream is done, and a boolean to handle cancellation.
    /// </param>
    /// <returns>
    /// Returns <c>True</c> if the streaming process started successfully, <c>False</c> otherwise.
    /// </returns>
    /// <remarks>
    /// This method initiates a chat request in streaming mode, where the response is delivered incrementally in real-time.
    /// The <c>Event</c> callback will be invoked multiple times as tokens are received.
    /// When the response is complete, the <c>IsDone</c> flag will be set to <c>True</c>, and the <c>Chat</c> object will be <c>nil</c>.
    /// The streaming process can be interrupted by setting the <c>Cancel</c> flag to <c>True</c> within the event.
    ///
    /// Example usage:
    /// <code>
    ///   var Gemini := TGeminiFactory.CreateInstance(BaererKey);
    ///   Gemini.Chat.CreateStream(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///       Params.Stream(True);
    ///     end,
    ///
    ///     procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       // Handle displaying
    ///     end
    ///   );
    /// </code>
    /// </remarks>
    function CreateStream(const ModelName: string; ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
  end;

implementation

uses
  System.StrUtils, System.Math, System.Rtti, Rest.Json, Gemini.Async.Params,
  Gemini.NetEncoding.Base64;

type
  TAttachedManager = record
  private
    function IsUri(const FilePath: string): Boolean;
    function GetMimeType(const FilePath: string): string;
    function GetEncoded(const FilePath: string): string;
  public
    function ToJson(const FilePath: string): TJSONObject;
  end;

{ TChatRoute }

procedure TChatRoute.AsynCreate(const ModelName: string;
  ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChat>);
begin
  with TAsynCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ModelName, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TChatRoute.AsynCreateStream(const ModelName: string;
  ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChatStream>);
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
        procedure()
        begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStream(ModelName, ParamProc,
                procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  if not IsDone and Assigned(Chat) then
                    begin
                      var LocalChat := Chat;
                      Chat := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChat);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
                          end;
                        end);
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnSuccess(Sender);
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
        end);
  Task.Start;
end;

function TChatRoute.Create(const ModelName: string; ParamProc: TProc<TChatParams>): TChat;
begin
  GeminiLock.Acquire;
  try
    Result := API.Post<TChat, TChatParams>(SetModel(ModelName, ':generateContent'), ParamProc);
  finally
    GeminiLock.Release;
  end;
end;

function TChatRoute.CreateStream(const ModelName: string; ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
  Prev: Integer;
  Starting: Boolean;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Prev := 0;
    Starting := True;
    Result := API.Post<TChatParams>(SetModel(ModelName, ':streamGenerateContent'), ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Chat: TChat;
        TextBuffer: string;
        Line: string;
        Ret: Integer;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        Ret := TextBuffer.Length - 1;
        Line := TextBuffer.Substring(RetPos, Ret - RetPos);
        RetPos := Ret + 1;

        Chat := nil;
        Data := Line.Trim([' ', #13, #10]);
        Data := Copy(Data, 2, Data.Length-1);

        IsDone := not Starting and Data.IsEmpty;
        Starting := False;

        if not IsDone then
          try
            Chat := TJson.JsonToObject<TChat>(Data);
            Prev := RetPos;
          except
            Chat := nil;
            RetPos := Prev;
          end;

        try
          Event(Chat, IsDone, AAbort);
        finally
          Chat.Free;
        end;
      end);
  finally
    ResetStream;
    Response.Free;
  end;
end;

procedure TChatRoute.ResetStream;
begin
  AsynCreate('gemini-1.5-pro',
    procedure (Params : TChatParams)
    begin
      Params.Contents([TContentPayload.Add(user, 'nill')]);
    end,
    function : TAsynChat
    begin
    end);
end;

{ TChatParams }

function TChatParams.CachedContent(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('cachedContent', Value));
end;

function TChatParams.Contents(
  const Value: TArray<TContentPayload>): TChatParams;
begin
  var JSONContents := TJSONArray.Create;
  for var Item in Value do
    JSONContents.Add(Item.Detach);
  Result := TChatParams(Add('contents', JSONContents));
end;

function TChatParams.GenerationConfig(const ParamProc: TProcRef<TGenerationConfig>): TChatParams;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TGenerationConfig.Create;
      ParamProc(Value);
      Result := TChatParams(Add('generationConfig', Value.Detach));
    end
  else Result := Self;
end;

class function TChatParams.New(
  const ParamProc: TProcRef<TChatParams>): TChatParams;
begin
  Result := TChatParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TChatParams.SafetySettings(
  const Value: TArray<TSafety>): TChatParams;
begin
  var JSONSafetySettings := TJSONArray.Create;
  for var Item in Value do
    JSONSafetySettings.Add(Item.ToJson);
  Result := TChatParams(Add('safetySettings', JSONSafetySettings));
end;

function TChatParams.SystemInstruction(const Value: string): TChatParams;
begin
  var PartsJSON := TJSONObject.Create.AddPair('parts', TJSONObject.Create.AddPair('text', Value));
  Result := TChatParams(Add('system_instruction', PartsJSON));
end;

function TChatParams.ToolConfig(const Value: TToolMode; AllowedFunctionNames: TArray<string>): TChatParams;
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
  Result := TChatParams(Add('toolConfig',
              TJSONObject.Create.AddPair('function_calling_config', Temp)));
end;

function TChatParams.Tools(const Value: TArray<IFunctionCore>): TChatParams;
begin
  var JSONFuncs := TJSONArray.Create;
  for var Item in value do
    begin
      JSONFuncs.Add(TToolPluginParams.Add(Item).ToJson);
    end;
  var JSONDeclaration := TJSONObject.Create.AddPair('function_declarations', JSONFuncs);
  Result := TChatParams(Add('tools', TJSONArray.Create.Add(JSONDeclaration)));
end;

function TChatParams.Tools(const Value: TToolKind; const Threshold: Double): TChatParams;
begin
  case Value of
    CodeExecution:
      begin
        var JSONCodeExecution := TJSONObject.Create.AddPair('codeExecution', TJSONObject.Create);
        Result := TChatParams(Add('tools', TJSONArray.Create.Add(JSONCodeExecution)));
      end;
    GoogleSearch:
      begin
        var JSONValue := TGoogleSearchRetrieval.Create.DynamicRetrievalConfig('MODE_DYNAMIC', Threshold);
        Result := TChatParams(Add('tools', TJSONObject.Create.AddPair('google_search_retrieval', JSONValue.Detach)));
      end;
    else
      Result := Self;
  end;
end;

{ TMessageRoleHelper }

class function TMessageRoleHelper.Create(const Value: string): TMessageRole;
begin
  var index := IndexStr(AnsiLowerCase(Value), ['user', 'model']);
  if index = -1 then
    raise Exception.Create('String role value not correct');
  Result := TMessageRole(index);
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    user:
      Exit('user');
    model:
      Exit('model');
  end;
end;

{ TChat }

destructor TChat.Destroy;
begin
  for var Item in FCandidates do
    Item.Free;
  if Assigned(FPromptFeedback) then
    FPromptFeedback.Free;
  if Assigned(FUsageMetadata) then
    FUsageMetadata.Free;
  inherited;
end;

{ TChatCandidate }

destructor TChatCandidate.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  for var Item in FSafetyRatings do
    Item.Free;
  if Assigned(FCitationMetadata) then
    FCitationMetadata.Free;
  if Assigned(FlogprobsResult) then
    FlogprobsResult.Free;
  if Assigned(FGroundingMetadata) then
    FGroundingMetadata.Free;
  inherited;
end;

{ TChatContent }

destructor TChatContent.Destroy;
begin
  for var Item in FParts do
    Item.Free;
  inherited;
end;

{ TPromptFeedback }

destructor TPromptFeedback.Destroy;
begin
  for var Item in FSafetyRatings do
    Item.Free;
  inherited;
end;

{ TFinishReasonHelper }

class function TFinishReasonHelper.Create(const Value: string): TFinishReason;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
         'FINISH_REASON_UNSPECIFIED', 'STOP', 'MAX_TOKENS', 'SAFETY',
         'RECITATION', 'LANGUAGE', 'OTHER', 'BLOCKLIST', 'PROHIBITED_CONTENT',
         'SPII', 'MALFORMED_FUNCTION_CALL']);
  if Index = -1 then
    raise Exception.CreateFmt('"FinishReason" unknown : %s', [Value]);
  Result := TFinishReason(Index);
end;

function TFinishReasonHelper.ToString: string;
begin
  case Self of
    FINISH_REASON_UNSPECIFIED:
      Exit('FINISH_REASON_UNSPECIFIED');
    STOP:
      Exit('STOP');
    MAX_TOKENS:
      Exit('MAX_TOKENS');
    SAFETY:
      Exit('SAFETY');
    RECITATION:
      Exit('RECITATION');
    LANGUAGE:
      Exit('LANGUAGE');
    OTHER:
      Exit('OTHER');
    BLOCKLIST:
      Exit('BLOCKLIST');
    PROHIBITED_CONTENT:
      Exit('PROHIBITED_CONTENT');
    SPII:
      Exit('SPII');
    MALFORMED_FUNCTION_CALL:
      Exit('MALFORMED_FUNCTION_CALL');
  end;
end;

{ TFinishReasonInterceptor }

function TFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFinishReason>.ToString;
end;

procedure TFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFinishReason.Create(Arg)));
end;

{ TCitationMetadata }

destructor TCitationMetadata.Destroy;
begin
  for var Item in FCitationSources do
    Item.Free;
  inherited;
end;

{ TLogprobsResult }

destructor TLogprobsResult.Destroy;
begin
  for var Item in FTopCandidates do
    Item.Free;
  for var Item in FChosenCandidates do
    Item.Free;
  inherited;
end;

{ TGenerationConfig }

function TGenerationConfig.CandidateCount(
  const Value: Integer): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('candidateCount', Value));
end;

function TGenerationConfig.FrequencyPenalty(
  const Value: Double): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('frequencyPenalty', Value));
end;

function TGenerationConfig.Logprobs(const Value: Integer): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('logprobs', Value));
end;

function TGenerationConfig.MaxOutputTokens(const Value: Integer): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('maxOutputTokens', Value));
end;

class function TGenerationConfig.New(
  const ParamProc: TProcRef<TGenerationConfig>): TGenerationConfig;
begin
  Result := TGenerationConfig.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TGenerationConfig.PresencePenalty(
  const Value: Double): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('presencePenalty', Value));
end;

function TGenerationConfig.ResponseLogprobs(
  const Value: Boolean): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('responseLogprobs', Value));
end;

function TGenerationConfig.ResponseMimeType(const Value: string): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('responseMimeType', Value));
end;

function TGenerationConfig.ResponseSchema(
  const Value: TJSONObject): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('responseSchema', Value));
end;

function TGenerationConfig.ResponseSchema(
  const ParamProc: TProcRef<TSchemaParams>): TGenerationConfig;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TSchemaParams.Create;
      ParamProc(Value);
      Result := TGenerationConfig(Add('responseSchema', Value.Detach));
    end
  else Result := Self;
end;

function TGenerationConfig.ResponseSchema(
  const Value: TSchemaParams): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('responseSchema', Value.Detach));
end;

function TGenerationConfig.StopSequences(
  const Value: TArray<string>): TGenerationConfig;
begin
  if Length(Value) = 0 then
    Exit(Self);
  Result := TGenerationConfig(Add('stopSequences', Value));
end;

function TGenerationConfig.Temperature(const Value: Double): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('temperature', Value));
end;

function TGenerationConfig.TopK(const Value: Integer): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('topK', Value));
end;

function TGenerationConfig.TopP(const Value: Double): TGenerationConfig;
begin
  Result := TGenerationConfig(Add('topP', Value));
end;

{ TMessageRoleInterceptor }

function TMessageRoleInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TMessageRole>.ToString;
end;

procedure TMessageRoleInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TMessageRole.Create(Arg)));
end;

{ TChatPart }

destructor TChatPart.Destroy;
begin
  if Assigned(FFunctionCall) then
    FFunctionCall.Free;
  if Assigned(FFunctionResponse) then
    FFunctionResponse.Free;
  if Assigned(FInlineData) then
    FInlineData.Free;
  if Assigned(FFileData) then
    FFileData.Free;
  if Assigned(FExecutableCode) then
    FExecutableCode.Free;
  if Assigned(FCodeExecutionResult) then
    FCodeExecutionResult.Free;
  inherited;
end;

{ TArgsFixInterceptor }

procedure TArgsFixInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  Arg := Format('{%s}', [Trim(Arg.Replace('`', '"').Replace(#10, ''))]);
  while Arg.Contains(', ') do Arg := Arg.Replace(', ', ',');
  Arg := Arg.Replace(',', ', ');
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, Arg);
end;

{ TContentPayload }

class function TContentPayload.Add(const Role: TMessageRole;
  const Text: string; const Attached: TArray<string>): TContentPayload;
begin
  Result := TContentPayload.Create.Role(Role).Parts(Text, Attached);
end;

class function TContentPayload.Assistant(
  const Attached: TArray<string>): TContentPayload;
begin
  Result := Add(TMessageRole.model, Attached);
end;

class function TContentPayload.Assistant(const Value: string;
  const Attached: TArray<string>): TContentPayload;
begin
  Result := Add(Value, Attached).Role(TMessageRole.model);
end;

class function TContentPayload.Add(const Role: TMessageRole;
  const Attached: TArray<string>): TContentPayload;
begin
  Result := TContentPayload.Create.Role(Role).Parts(Attached);
end;

class function TContentPayload.New(
  const ParamProc: TProcRef<TContentPayload>): TContentPayload;
begin
  Result := TContentPayload.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TContentPayload.Parts(const Attached: TArray<string>): TContentPayload;
var
  Convert: TAttachedManager;
begin
  var JSONParts := TJSONArray.Create;
  for var Item in Attached do
    JSONParts.Add(Convert.ToJson(Item));
  Result := TContentPayload(Add('parts', JSONParts));
end;

class function TContentPayload.Add(const Text: string;
  const Attached: TArray<string>): TContentPayload;
begin
  Result := TContentPayload.Create.Parts(Text, Attached);
end;

function TContentPayload.Role(const Value: TMessageRole): TContentPayload;
begin
  Result := TContentPayload(Add('role', Value.ToString));
end;

class function TContentPayload.User(
  const Attached: TArray<string>): TContentPayload;
begin
  Result := Add(TMessageRole.user, Attached);
end;

class function TContentPayload.User(const Value: string;
  const Attached: TArray<string>): TContentPayload;
begin
  Result := Add(Value, Attached).Role(TMessageRole.user);
end;

function TContentPayload.Parts(const Value: string;
  const Attached: TArray<string>): TContentPayload;
var
  Convert: TAttachedManager;
begin
  var JSONParts := TJSONArray.Create;
  if not Value.IsEmpty then
    JSONParts.Add(TTextData.Create.Text(Value).Detach);
  for var Item in Attached do
    JSONParts.Add(Convert.ToJson(Item));
  Result := TContentPayload(Add('parts', JSONParts));
end;

{ TTextData }

function TTextData.Text(const Value: string): TTextData;
begin
  Result := TTextData(Add('text', Value));
end;

{ TInlineData }

function TInlineData.Data(const Value: string): TInlineData;
begin
  Result := TInlineData(Add('data', Value));
end;

function TInlineData.MimeType(const Value: string): TInlineData;
begin
  Result := TInlineData(Add('mime_type', Value));
end;

{ TFileData }

function TFileData.FileUri(const Value: string): TFileData;
begin
  Result := TFileData(Add('file_uri', Value));
end;

function TFileData.MimeType(const Value: string): TFileData;
begin
  Result := TFileData(Add('mime_type', Value));
end;

{ TAttachedManager }

function TAttachedManager.GetEncoded(const FilePath: string): string;
begin
  Result := EncodeBase64(FilePath);
end;

function TAttachedManager.GetMimeType(const FilePath: string): string;
begin
  Result := EmptyStr;
  if not IsUri(FilePath) then
    begin
      Result := ResolveMimeType(FilePath);
    end;
end;

function TAttachedManager.IsUri(const FilePath: string): Boolean;
begin
  Result := FilePath.ToLower.StartsWith('http');
end;

function TAttachedManager.ToJson(const FilePath: string): TJSONObject;
begin
  if IsUri(FilePath) then
    begin
      Result := TFileData.Create.FileUri(FilePath).Detach;
      Result := TJSONObject.Create.AddPair('file_data', Result);
    end
  else
    begin
      Result := TInlineData.Create.MimeType(GetMimeType(FilePath)).Data(GetEncoded(FilePath)).Detach;
      Result := TJSONObject.Create.AddPair('inline_data', Result);
    end;
end;

{ TLangueTypeHelper }

class function TLangueTypeHelper.Create(const Value: string): TLangueType;
begin
  var Index := IndexStr(AnsiUpperCase(Value), ['LANGUAGE_UNSPECIFIED', 'PYTHON']);
  if Index = -1 then
    raise Exception.CreateFmt('"Langue type" unknown : %s', [Value]);
  Result := TLangueType(Index);
end;

function TLangueTypeHelper.ToString: string;
begin
  case Self of
    LANGUAGE_UNSPECIFIED:
      Exit('LANGUAGE_UNSPECIFIED');
    PYTHON:
      Exit('PYTHON');
  end;
end;

{ TLangueTypeInterceptor }

function TLangueTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TLangueType>.ToString;
end;

procedure TLangueTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TLangueType.Create(Arg)));
end;

{ TOutcomeTypeHelper }

class function TOutcomeTypeHelper.Create(
  const Value: string): TOutcomeType;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
    'OUTCOME_UNSPECIFIED', 'OUTCOME_OK', 'OUTCOME_FAILED', 'OUTCOME_DEADLINE_EXCEEDED']);
  if Index = -1 then
    raise Exception.CreateFmt('"Code result type" unknown : %s', [Value]);
  Result := TOutcomeType(Index);
end;

function TOutcomeTypeHelper.ToString: string;
begin
  case Self of
    OUTCOME_UNSPECIFIED:
      Exit('OUTCOME_UNSPECIFIED');
    OUTCOME_OK:
      Exit('OUTCOME_OK');
    OUTCOME_FAILED:
      Exit('OUTCOME_FAILED');
    OUTCOME_DEADLINE_EXCEEDED:
      Exit('OUTCOME_DEADLINE_EXCEEDED');
  end;
end;

{ TOutcomeTypeInterceptor }

function TOutcomeTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TOutcomeType>.ToString;
end;

procedure TOutcomeTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TOutcomeType.Create(Arg)));
end;

end.

unit Gemini.Safety;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  Gemini.API.Params;

type
  /// <summary>
  /// Block at and beyond a specified harm probability.
  /// </summary>
  THarmBlockThreshold = (
    /// <summary>
    /// Threshold is unspecified.
    /// </summary>
    HARM_BLOCK_THRESHOLD_UNSPECIFIED,
    /// <summary>
    /// Content with NEGLIGIBLE will be allowed.
    /// </summary>
    BLOCK_LOW_AND_ABOVE,
    /// <summary>
    /// Content with NEGLIGIBLE and LOW will be allowed.
    /// </summary>
    BLOCK_MEDIUM_AND_ABOVE,
    /// <summary>
    /// Content with NEGLIGIBLE, LOW, and MEDIUM will be allowed.
    /// </summary>
    BLOCK_ONLY_HIGH,
    /// <summary>
    /// All content will be allowed.
    /// </summary>
    BLOCK_NONE,
    /// <summary>
    /// Turn off the safety filter.
    /// </summary>
    OFF
  );

  /// <summary>
  /// Helper record for the <c>THarmBlockThreshold</c> enumeration, providing utility methods for converting
  /// between <c>THarmBlockThreshold</c> values and their string representations.
  /// </summary>
  THarmBlockThresholdHelper = record helper for THarmBlockThreshold
    /// <summary>
    /// Converts the current <c>THarmBlockThreshold</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>THarmBlockThreshold</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// The category of a rating.
  /// </summary>
  /// <remarks>
  /// These categories cover various kinds of harms that developers may wish to adjust.
  /// </remarks>
  THarmCategory = (
    /// <summary>
    /// Harassment content.
    /// </summary>
    /// <remarks>
    /// Works only with a Gemini-type model.
    /// </remarks>
    HARM_CATEGORY_HARASSMENT,
    /// <summary>
    /// Hate speech and content.
    /// </summary>
    /// <remarks>
    /// Works only with a Gemini-type model.
    /// </remarks>
    HARM_CATEGORY_HATE_SPEECH,
    /// <summary>
    /// Sexually explicit content.
    /// </summary>
    /// <remarks>
    /// Works only with a Gemini-type model.
    /// </remarks>
    HARM_CATEGORY_SEXUALLY_EXPLICIT,
    /// <summary>
    /// Dangerous content.
    /// </summary>
    /// <remarks>
    /// Works only with a Gemini-type model.
    /// </remarks>
    HARM_CATEGORY_DANGEROUS_CONTENT,
    /// <summary>
    /// Content that may be used to harm civic integrity.
    /// </summary>
    /// <remarks>
    /// Works only with a Gemini-type model.
    /// </remarks>
    HARM_CATEGORY_CIVIC_INTEGRITY
  );

  /// <summary>
  /// Helper record for the <c>THarmCategory</c> enumeration, providing utility methods for converting
  /// between <c>THarmCategory</c> values and their string representations.
  /// </summary>
  THarmCategoryHelper = record helper for THarmCategory
    /// <summary>
    /// Converts the current <c>THarmCategory</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>THarmCategory</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>THarmCategory</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>THarmCategory</c>.
    /// </param>
    /// <returns>
    /// The <c>THarmCategory</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): THarmCategory; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>THarmCategory</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>THarmCategory</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  THarmCategoryInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>THarmCategory</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>THarmCategory</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>THarmCategory</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>THarmCategory</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>THarmCategory</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>THarmCategory</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>THarmCategory</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Specifies the reason why the prompt was blocked.
  /// </summary>
  TBlockReason = (
    /// <summary>
    /// Default value. This value is unused.
    /// </summary>
    BLOCK_REASON_UNSPECIFIED,
    /// <summary>
    /// Prompt was blocked due to safety reasons. Inspect safetyRatings to understand which safety category blocked it.
    /// </summary>
    SAFETY,
    /// <summary>
    /// Prompt was blocked due to unknown reasons.
    /// </summary>
    OTHER,
    /// <summary>
    /// Prompt was blocked due to the terms which are included from the terminology blocklist.
    /// </summary>
    BLOCKLIST,
    /// <summary>
    /// Prompt was blocked due to prohibited content.
    /// </summary>
    PROHIBITED_CONTENT
  );

  /// <summary>
  /// Helper record for the <c>TBlockReason</c> enumeration, providing utility methods for converting
  /// between <c>TBlockReason</c> values and their string representations.
  /// </summary>
  TBlockReasonHelper = record helper for TBlockReason
    /// <summary>
    /// Converts the current <c>TBlockReason</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TBlockReason</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TBlockReason</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TBlockReason</c>.
    /// </param>
    /// <returns>
    /// The <c>TBlockReason</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TBlockReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TBlockReason</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TBlockReason</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TBlockReasonInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TBlockReason</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TBlockReason</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TBlockReason</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TBlockReason</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TBlockReason</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TBlockReason</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TBlockReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// The probability that a piece of content is harmful.
  /// </summary>
  /// <remarks>
  /// The classification system gives the probability of the content being unsafe. This does not indicate the severity of harm for a piece of content.
  /// </remarks>
  THarmProbability = (
    /// <summary>
    /// Probability is unspecified.
    /// </summary>
    HARM_PROBABILITY_UNSPECIFIED,
    /// <summary>
    /// Content has a negligible chance of being unsafe.
    /// </summary>
    NEGLIGIBLE,
    /// <summary>
    /// Content has a low chance of being unsafe.
    /// </summary>
    LOW,
    /// <summary>
    /// Content has a medium chance of being unsafe.
    /// </summary>
    MEDIUM,
    /// <summary>
    /// Content has a high chance of being unsafe.
    /// </summary>
    HIGH
  );

  /// <summary>
  /// Helper record for the <c>THarmProbability</c> enumeration, providing utility methods for converting
  /// between <c>THarmProbability</c> values and their string representations.
  /// </summary>
  THarmProbabilityHelper = record helper for THarmProbability
    /// <summary>
    /// Converts the current <c>THarmProbability</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>THarmProbability</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>THarmProbability</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>THarmProbability</c>.
    /// </param>
    /// <returns>
    /// The <c>THarmProbability</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): THarmProbability; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>THarmProbability</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>THarmProbability</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  THarmProbabilityInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>THarmProbability</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>THarmProbability</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>THarmProbability</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>THarmProbability</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>THarmProbability</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>THarmProbability</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>THarmProbability</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Represents a safety setting that affects the safety-blocking behavior.
  /// </summary>
  /// <remarks>
  /// By setting safety configurations for specific categories, you can adjust the allowed probability that content is blocked based on harm categories.
  /// </remarks>
  TSafety = record
  private
    FCategory: THarmCategory;
    FThreshold: THarmBlockThreshold;
  public
    /// <summary>
    /// Sets the category for this safety setting.
    /// </summary>
    /// <param name="Value">
    /// The harm category to set.
    /// </param>
    /// <returns>
    /// The updated <c>TSafety</c> record with the specified category.
    /// </returns>
    function Category(Value: THarmCategory): TSafety;
    /// <summary>
    /// Sets the probability threshold at which harm is blocked for this safety setting.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set.
    /// </param>
    /// <returns>
    /// The updated <c>TSafety</c> record with the specified threshold.
    /// </returns>
    function Threshold(Value: THarmBlockThreshold): TSafety;
    /// <summary>
    /// Converts this <c>TSafety</c> record to a JSON object.
    /// </summary>
    /// <returns>
    /// A <c>TJSONObject</c> representing this safety setting.
    /// </returns>
    function ToJson: TJSONObject;
    /// <summary>
    /// Creates a new <c>TSafety</c> record with the specified category and threshold.
    /// </summary>
    /// <param name="Category">
    /// The harm category to set.
    /// </param>
    /// <param name="Threshold">
    /// The harm block threshold to set.
    /// </param>
    /// <returns>
    /// A new <c>TSafety</c> record with the specified category and threshold.
    /// </returns>
    class function New(Category: THarmCategory; Threshold: THarmBlockThreshold): TSafety; static;
    /// <summary>
    /// Returns an array of <c>TSafety</c> settings that do not block any content.
    /// </summary>
    /// <returns>
    /// An array of <c>TSafety</c> records with all categories set to <c>BLOCK_NONE</c>.
    /// </returns>
    class function DontBlock: TArray<TSafety>; static;
    /// <summary>
    /// Creates a <c>TSafety</c> setting for the Sexually Explicit category with the specified threshold.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set for the Sexually Explicit category.
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record for the Sexually Explicit category with the specified threshold.
    /// </returns>
    class function SexuallyExplicit(const Value: THarmBlockThreshold): TSafety; static;
    /// <summary>
    /// Creates a <c>TSafety</c> setting for the Hate Speech category with the specified threshold.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set for the Hate Speech category.
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record for the Hate Speech category with the specified threshold.
    /// </returns>
    class function HateSpeech(const Value: THarmBlockThreshold): TSafety; static;
    /// <summary>
    /// Creates a <c>TSafety</c> setting for the Harassment category with the specified threshold.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set for the Harassment category.
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record for the Harassment category with the specified threshold.
    /// </returns>
    class function Harassment(const Value: THarmBlockThreshold): TSafety; static;
    /// <summary>
    /// Creates a <c>TSafety</c> setting for the Dangerous Content category with the specified threshold.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set for the Dangerous Content category.
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record for the Dangerous Content category with the specified threshold.
    /// </returns>
    class function DangerousContent(const Value: THarmBlockThreshold): TSafety; static;
    /// <summary>
    /// Creates a <c>TSafety</c> setting for the Civic Integrity category with the specified threshold.
    /// </summary>
    /// <param name="Value">
    /// The harm block threshold to set for the Civic Integrity category.
    /// </param>
    /// <returns>
    /// A <c>TSafety</c> record for the Civic Integrity category with the specified threshold.
    /// </returns>
    class function CivicIntegrity(const Value: THarmBlockThreshold): TSafety; static;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ THarmBlockThresholdHelper }

function THarmBlockThresholdHelper.ToString: string;
begin
  case Self of
    HARM_BLOCK_THRESHOLD_UNSPECIFIED:
      Exit('HARM_BLOCK_THRESHOLD_UNSPECIFIED');
    BLOCK_LOW_AND_ABOVE:
      Exit('BLOCK_LOW_AND_ABOVE');
    BLOCK_MEDIUM_AND_ABOVE:
      Exit('BLOCK_MEDIUM_AND_ABOVE');
    BLOCK_ONLY_HIGH:
      Exit('BLOCK_ONLY_HIGH');
    BLOCK_NONE:
      Exit('BLOCK_NONE');
    OFF:
      Exit('OFF');
  end;
end;

{ THarmCategoryHelper }

class function THarmCategoryHelper.Create(const Value: string): THarmCategory;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
        'HARM_CATEGORY_HARASSMENT', 'HARM_CATEGORY_HATE_SPEECH',
        'HARM_CATEGORY_SEXUALLY_EXPLICIT', 'HARM_CATEGORY_DANGEROUS_CONTENT',
        'HARM_CATEGORY_CIVIC_INTEGRITY' ]);
  if Index = -1 then
    raise Exception.CreateFmt('"HarmCategory" unknown : %s', [Value]);
  Result := THarmCategory(Index);
end;

function THarmCategoryHelper.ToString: string;
begin
  case Self of
    HARM_CATEGORY_HARASSMENT:
      Exit('HARM_CATEGORY_HARASSMENT');
    HARM_CATEGORY_HATE_SPEECH:
      Exit('HARM_CATEGORY_HATE_SPEECH');
    HARM_CATEGORY_SEXUALLY_EXPLICIT:
      Exit('HARM_CATEGORY_SEXUALLY_EXPLICIT');
    HARM_CATEGORY_DANGEROUS_CONTENT:
      Exit('HARM_CATEGORY_DANGEROUS_CONTENT');
    HARM_CATEGORY_CIVIC_INTEGRITY:
      Exit('HARM_CATEGORY_CIVIC_INTEGRITY');
  end;
end;

{ THarmCategoryInterceptor }

function THarmCategoryInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<THarmCategory>.ToString;
end;

procedure THarmCategoryInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(THarmCategory.Create(Arg)));
end;

{ TBlockReasonHelper }

class function TBlockReasonHelper.Create(const Value: string): TBlockReason;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
        'BLOCK_REASON_UNSPECIFIED', 'SAFETY', 'OTHER', 'BLOCKLIST', 'PROHIBITED_CONTENT' ]);
  if Index = -1 then
    raise Exception.CreateFmt('"BlockReason" unknown : %s', [Value]);
  Result := TBlockReason(Index);
end;

function TBlockReasonHelper.ToString: string;
begin
  case Self of
    BLOCK_REASON_UNSPECIFIED:
      Exit('BLOCK_REASON_UNSPECIFIED');
    SAFETY:
      Exit('SAFETY');
    OTHER:
      Exit('OTHER');
    BLOCKLIST:
      Exit('BLOCKLIST');
    PROHIBITED_CONTENT:
      Exit('PROHIBITED_CONTENT');
  end;
end;

{ TBlockReasonInterceptor }

function TBlockReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TBlockReason>.ToString;
end;

procedure TBlockReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TBlockReason.Create(Arg)));
end;

{ THarmProbabilityHelper }

class function THarmProbabilityHelper.Create(
  const Value: string): THarmProbability;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
         'HARM_PROBABILITY_UNSPECIFIED', 'NEGLIGIBLE', 'LOW', 'MEDIUM', 'HIGH']);
  if Index = -1 then
    raise Exception.CreateFmt('"HarmProbability" unknown : %s', [Value]);
  Result := THarmProbability(Index);
end;

function THarmProbabilityHelper.ToString: string;
begin
  case Self of
    HARM_PROBABILITY_UNSPECIFIED:
      Exit('HARM_PROBABILITY_UNSPECIFIED');
    NEGLIGIBLE:
      Exit('NEGLIGIBLE');
    LOW:
      Exit('LOW');
    MEDIUM:
      Exit('MEDIUM');
    HIGH:
      Exit('HIGH');
  end;
end;

{ THarmProbabilityInterceptor }

function THarmProbabilityInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<THarmProbability>.ToString;
end;

procedure THarmProbabilityInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(THarmProbability.Create(Arg)));
end;

{ TSafety }

function TSafety.Category(Value: THarmCategory): TSafety;
begin
  FCategory := Value;
  Result := Self;
end;

class function TSafety.CivicIntegrity(
  const Value: THarmBlockThreshold): TSafety;
begin
  Result := TSafety.New(HARM_CATEGORY_CIVIC_INTEGRITY, Value);
end;

class function TSafety.DangerousContent(
  const Value: THarmBlockThreshold): TSafety;
begin
  Result := TSafety.New(HARM_CATEGORY_DANGEROUS_CONTENT, Value);
end;

class function TSafety.DontBlock: TArray<TSafety>;
begin
  Result := [
    SexuallyExplicit(BLOCK_NONE),
    HateSpeech(BLOCK_NONE),
    Harassment(BLOCK_NONE),
    DangerousContent(BLOCK_NONE),
    CivicIntegrity(BLOCK_NONE)];
end;

class function TSafety.Harassment(
  const Value: THarmBlockThreshold): TSafety;
begin
  Result := TSafety.New(HARM_CATEGORY_HARASSMENT, Value);
end;

class function TSafety.HateSpeech(
  const Value: THarmBlockThreshold): TSafety;
begin
  Result := TSafety.New(HARM_CATEGORY_HATE_SPEECH, Value);
end;

class function TSafety.New(Category: THarmCategory;
  Threshold: THarmBlockThreshold): TSafety;
begin
  Result := Result.Category(Category).Threshold(Threshold);
end;

class function TSafety.SexuallyExplicit(
  const Value: THarmBlockThreshold): TSafety;
begin
  Result := TSafety.New(HARM_CATEGORY_SEXUALLY_EXPLICIT, Value);
end;

function TSafety.Threshold(Value: THarmBlockThreshold): TSafety;
begin
  FThreshold := Value;
  Result := Self;
end;

function TSafety.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.
    AddPair('category', FCategory.ToString).
    AddPair('threshold', FThreshold.ToString);
end;

end.

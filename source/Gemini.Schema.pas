unit Gemini.Schema;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  Gemini.API.Params;

type
  /// <summary>
  /// Type contains the list of OpenAPI data types as defined by https://spec.openapis.org/oas/v3.0.3#data-types
  /// </summary>
  TSchemaType = (
    /// <summary>
    /// Not specified, should not be used.
    /// </summary>
    TYPE_UNSPECIFIED,
    /// <summary>
    /// String type.
    /// </summary>
    stSTRING,
    /// <summary>
    /// Number type.
    /// </summary>
    stNUMBER,
    /// <summary>
    /// Integer type.
    /// </summary>
    stINTEGER,
    /// <summary>
    /// Boolean type.
    /// </summary>
    stBOOLEAN,
    /// <summary>
    /// Array type.
    /// </summary>
    stARRAY,
    /// <summary>
    /// Object type.
    /// </summary>
    stOBJECT
  );

  TSchemaTypeHelper = record helper for TSchemaType
    function ToString: string;
  end;

  TSchemaParams = class;

  TPropertyItem = record
  public
    class function Add(Key: string; Value: TSchemaType): TJSONPair; static;
  end;

  TSchemaParams = class(TJSONParam)
  public
    function &Type(const Value: TSchemaType): TSchemaParams;
    function Format(const Value: string): TSchemaParams;
    function Description(const Value: string): TSchemaParams;
    function Nullable(const Value: Boolean): TSchemaParams;
    function Enum(const Value: TArray<string>): TSchemaParams;
    function MaxItems(const Value: string): TSchemaParams;
    function MinItems(const Value: string): TSchemaParams;
    function Properties(const Key: string; const Value: TSchemaParams): TSchemaParams; overload;
    function Properties(const Key: string; const ParamProc: TProcRef<TSchemaParams>): TSchemaParams; overload;
    function Properties(const Value: TArray<TJSONPair>): TSchemaParams; overload;
    function Required(const Value: TArray<string>): TSchemaParams;
    function Items(const Value: TSchemaParams): TSchemaParams; overload;
    function Items(const ParamProc: TProcRef<TSchemaParams>): TSchemaParams; overload;
    class function New: TSchemaParams; overload;
    class function New(const ParamProc: TProcRef<TSchemaParams>): TSchemaParams; overload;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TSchemaTypeHelper }

function TSchemaTypeHelper.ToString: string;
begin
  case Self of
    TYPE_UNSPECIFIED:
      Exit('type_unspecified');
    stSTRING:
      Exit('string');
    stNUMBER:
      Exit('number');
    stINTEGER:
      Exit('integer');
    stBOOLEAN:
      Exit('boolean');
    stARRAY:
      Exit('array');
    stOBJECT:
      Exit('object');
  end;
end;

{ TSchemaParams }

function TSchemaParams.Description(const Value: string): TSchemaParams;
begin
  Result := TSchemaParams(Add('description', Value));
end;

function TSchemaParams.Enum(const Value: TArray<string>): TSchemaParams;
begin
  Result := TSchemaParams(Add('enum', Value));
end;

function TSchemaParams.Format(const Value: string): TSchemaParams;
begin
  Result := TSchemaParams(Add('format', Value));
end;

function TSchemaParams.Items(
  const ParamProc: TProcRef<TSchemaParams>): TSchemaParams;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TSchemaParams.Create;
      ParamProc(Value);
      Result := Items(Value);
    end
  else Result := Self;
end;

function TSchemaParams.Items(const Value: TSchemaParams): TSchemaParams;
begin
  Result := TSchemaParams(Add('items', Value));
end;

function TSchemaParams.MaxItems(const Value: string): TSchemaParams;
begin
  Result := TSchemaParams(Add('maxItems', Value));
end;

function TSchemaParams.MinItems(const Value: string): TSchemaParams;
begin
  Result := TSchemaParams(Add('minItems', Value));
end;

class function TSchemaParams.New: TSchemaParams;
begin
  Result := TSchemaParams.Create;
end;

class function TSchemaParams.New(
  const ParamProc: TProcRef<TSchemaParams>): TSchemaParams;
begin
  Result := TSchemaParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TSchemaParams.Nullable(const Value: Boolean): TSchemaParams;
begin
  Result := TSchemaParams(Add('nullable', Value.ToString));
end;

function TSchemaParams.Properties(
  const Value: TArray<TJSONPair>): TSchemaParams;
begin
  var JSONValue := TJSONObject.Create;
  for var Item in Value do
    begin
      JSONValue.AddPair(Item);
    end;
  Result := TSchemaParams(Add('properties', JSONValue));
end;

function TSchemaParams.Properties(const Key: string;
  const ParamProc: TProcRef<TSchemaParams>): TSchemaParams;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TSchemaParams.Create;
      ParamProc(Value);
      Result := Properties(Key, Value);
    end
  else Result := Self;
end;

function TSchemaParams.Properties(const Key: string;
  const Value: TSchemaParams): TSchemaParams;
begin
  Result := TSchemaParams(Add(Key, Value.Detach));
end;

function TSchemaParams.Required(const Value: TArray<string>): TSchemaParams;
begin
  Result := TSchemaParams(Add('required', Value));
end;

function TSchemaParams.&Type(const Value: TSchemaType): TSchemaParams;
begin
  Result := TSchemaParams(Add('type', Value.ToString));
end;

{ TPropertyItem }

class function TPropertyItem.Add(Key: string; Value: TSchemaType): TJSONPair;
begin
  Result := TJSONPair.Create(Key, Value.ToString);
end;

end.

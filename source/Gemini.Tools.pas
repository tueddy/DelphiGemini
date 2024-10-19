unit Gemini.Tools;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  Gemini.API.Params, Gemini.Schema;

type
  TToolMode = (
    /// <summary>
    /// Unspecified function calling mode. This value should not be used.
    /// </summary>
    MODE_UNSPECIFIED,
    /// <summary>
    /// Default model behavior, model decides to predict either a function call or a natural language response.
    /// </summary>
    AUTO,
    /// <summary>
    /// Model is constrained to always predicting a function call only. If "allowedFunctionNames" are set, the
    //// predicted function call will be limited to any one of "allowedFunctionNames", else the predicted function
    /// call will be any one of the provided "functionDeclarations".
    /// </summary>
    ANY,
    /// <summary>
    /// Model will not predict any function call. Model behavior is same as when not passing any function declarations.
    /// </summary>
    NONE
  );

  TToolModeHelper = record helper for TToolMode
    function ToString: string;
  end;

  TCodeExecution = class(TJSONParam)
  end;

  TFunctionDeclaration = class(TJSONParam)
  public
    function Name(const Value: string): TFunctionDeclaration;
    function Description(const Value: string): TFunctionDeclaration;
    function Parameters(const Value: TSchemaParams): TFunctionDeclaration; overload;
    function Parameters(const ParamProc: TProcRef<TSchemaParams>): TFunctionDeclaration; overload;
    class function New: TFunctionDeclaration; overload;
    class function New(const ParamProc: TProcRef<TFunctionDeclaration>): TFunctionDeclaration; overload;
  end;

  TToolParams = class(TJSONParam)
  public
    function FunctionDeclarations(const Value: TArray<TFunctionDeclaration>): TToolParams;
    function CodeExecution(const Value: TCodeExecution): TToolParams;
    class function New: TToolParams; overload;
    class function New(const ParamProc: TProcRef<TToolParams>): TToolParams; overload;
  end;

  TFunctionCallingConfig = class(TJSONParam)
  public
    function Mode(const Value: TToolMode): TFunctionCallingConfig;
    function AllowedFunctionNames(const Value: TArray<string>): TFunctionCallingConfig;
    class function New(const Mode: TToolMode; const FunctionNames: TArray<string>): TFunctionCallingConfig; overload;
    class function New(const ParamProc: TProcRef<TFunctionCallingConfig>): TFunctionCallingConfig; overload;
  end;

  TToolConfig = class(TJSONParam)
  public
    function FunctionCallingConfig(const Value: TFunctionCallingConfig): TToolConfig; overload;
    function FunctionCallingConfig(const ParamProc: TProcRef<TFunctionCallingConfig>): TToolConfig; overload;
    class function New: TToolConfig; overload;
    class function New(const ParamProc: TProcRef<TToolConfig>): TToolConfig; overload;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TFunctionDeclaration }

function TFunctionDeclaration.Description(
  const Value: string): TFunctionDeclaration;
begin
  Result := TFunctionDeclaration(Add('description', Value));
end;

function TFunctionDeclaration.Name(const Value: string): TFunctionDeclaration;
begin
  Result := TFunctionDeclaration(Add('name', Value));
end;

class function TFunctionDeclaration.New: TFunctionDeclaration;
begin
  Result := TFunctionDeclaration.Create;
end;

class function TFunctionDeclaration.New(
  const ParamProc: TProcRef<TFunctionDeclaration>): TFunctionDeclaration;
begin
  Result := TFunctionDeclaration.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

function TFunctionDeclaration.Parameters(
  const ParamProc: TProcRef<TSchemaParams>): TFunctionDeclaration;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TSchemaParams.Create;
      ParamProc(Value);
      Result := Parameters(Value);
    end
  else Result := Self;
end;

function TFunctionDeclaration.Parameters(
  const Value: TSchemaParams): TFunctionDeclaration;
begin
  Result := TFunctionDeclaration(Add('parameters', Value.Detach));
end;

{ TToolParams }

function TToolParams.CodeExecution(const Value: TCodeExecution): TToolParams;
begin
  if Assigned(Value) then
    Result := TToolParams(Add('codeExecution', Value.Detach)) else
    Result := Self;
end;

function TToolParams.FunctionDeclarations(
  const Value: TArray<TFunctionDeclaration>): TToolParams;
begin
  var JSONDeclarations := TJSONArray.Create;
  for var Item in Value do
    begin
      JSONDeclarations.Add(Item.Detach);
    end;
  Result := TToolParams(Add('functionDeclarations', JSONDeclarations));
end;

class function TToolParams.New(
  const ParamProc: TProcRef<TToolParams>): TToolParams;
begin
  Result := TToolParams.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

class function TToolParams.New: TToolParams;
begin
  Result := TToolParams.Create;
end;

{ TToolModeHelper }

function TToolModeHelper.ToString: string;
begin
  case Self of
    MODE_UNSPECIFIED:
      Exit('MODE_UNSPECIFIED');
    AUTO:
      Exit('AUTO');
    ANY:
      Exit('ANY');
    NONE:
      Exit('NONE');
  end;
end;

{ TFunctionCallingConfig }

function TFunctionCallingConfig.AllowedFunctionNames(
  const Value: TArray<string>): TFunctionCallingConfig;
begin
  Result := TFunctionCallingConfig(Add('allowedFunctionNames', Value));
end;

function TFunctionCallingConfig.Mode(
  const Value: TToolMode): TFunctionCallingConfig;
begin
  Result := TFunctionCallingConfig(Add('mode', Value.ToString));
end;

class function TFunctionCallingConfig.New(
  const ParamProc: TProcRef<TFunctionCallingConfig>): TFunctionCallingConfig;
begin
  Result := TFunctionCallingConfig.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

class function TFunctionCallingConfig.New(const Mode: TToolMode;
  const FunctionNames: TArray<string>): TFunctionCallingConfig;
begin
  Result := TFunctionCallingConfig.Create.
              Mode(Mode).
              AllowedFunctionNames(FunctionNames);
end;

{ TToolConfig }

function TToolConfig.FunctionCallingConfig(
  const Value: TFunctionCallingConfig): TToolConfig;
begin
  Result := TToolConfig(Add('functionCallingConfig', Value.Detach));
end;

function TToolConfig.FunctionCallingConfig(
  const ParamProc: TProcRef<TFunctionCallingConfig>): TToolConfig;
begin
  if Assigned(ParamProc) then
    begin
      var Value := TFunctionCallingConfig.Create;
      ParamProc(Value);
      Result := TToolConfig(Add('functionCallingConfig', Value.Detach));
    end
  else Result := Self;
end;

class function TToolConfig.New(
  const ParamProc: TProcRef<TToolConfig>): TToolConfig;
begin
  Result := TToolConfig.Create;
  if Assigned(ParamProc) then
    begin
      ParamProc(Result);
    end;
end;

class function TToolConfig.New: TToolConfig;
begin
  Result := TToolConfig.Create;
end;

end.

unit Gemini.Tools;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  Gemini.API.Params, Gemini.Schema, Gemini.Functions.Core;

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

  TToolPluginParams = record
  private
    FFunction: IFunctionCore;
  public
    /// <summary>
    /// This method converts the TFunctionCore instance to a JSON object containing the type and
    /// representation of the function, and handles exceptions by deleting the JSON object and
    /// propagating the exception if an error occurs
    /// </summary>
    function ToJson: TJSONObject;
    /// <summary>
    /// The function properties
    /// </summary>
    property &Function: IFunctionCore read FFunction write FFunction;
    class function Add(const AFunction: IFunctionCore): TToolPluginParams; static;
  end;

implementation

uses
  System.StrUtils;

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

{ TToolPluginParams }

class function TToolPluginParams.Add(const AFunction: IFunctionCore): TToolPluginParams;
begin
  Result.&Function := AFunction;
end;

function TToolPluginParams.ToJson: TJSONObject;
begin
  Result := FFunction.ToJson;
end;

end.

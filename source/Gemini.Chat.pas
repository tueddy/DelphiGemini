unit Gemini.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Gemini.API.Params, Gemini.API;

type

  TChatParams = class(TJSONParam)

  end;

  TChat = class

  end;

  TChatRoute = class(TGeminiAPIRoute)
  public
    function Create(const Model: string; ParamProc: TProc<TChatParams>): TChat;
  end;

implementation

{ TChatRoute }

function TChatRoute.Create(const Model: string; ParamProc: TProc<TChatParams>): TChat;
begin
//  'gemini-1.5-flash'
  Result := API.Post<TChat, TChatParams>(Format('messages/%s', [Model]), ParamProc);
end;

end.

unit Gemini.Errors;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  REST.Json.Types;

type
  TErrorCore = class abstract
  end;

  TErrorObject = class
  private
    FCode: Int64;
    FMessage: string;
    FStatus: string;
  public
    property Code: Int64 read FCode write FCode;
    property Message: string read FMessage write FMessage;
    property Status: string read FStatus write FStatus;
  end;

  TError = class(TErrorCore)
  private
    FError: TErrorObject;
  public
    property Error: TErrorObject read FError write FError;
    destructor Destroy; override;
  end;

implementation

{ TError }

destructor TError.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

end.

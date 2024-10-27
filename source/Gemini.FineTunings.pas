unit Gemini.FineTunings;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiGemini
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Gemini.API.Params, Gemini.API, Gemini.Safety, Gemini.Async.Support,
  Gemini.Chat,

  Vcl.Dialogs;

type
  TModelState = (
    /// <summary>
    /// Default value. This value is not used.
    /// </summary>
    STATE_UNSPECIFIED,
    /// <summary>
    /// The model is being created.
    /// </summary>
    CREATING,
    /// <summary>
    /// The model is ready to use.
    /// </summary>
    ACTIVE,
    /// <summary>
    /// Failed to create model.
    /// </summary>
    FAILED
  );

  TModelStateHelper = record helper for TModelState
    function ToString: string;
    class function Create(const Value: string): TModelState; static;
  end;

  TModelStateInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  THyperparametersParams = class(TJSONParam)
  public
    function LearningRate(const Value: Double): THyperparametersParams;
    function LearningRateMultiplier(const Value: Double): THyperparametersParams;
    function EpochCount(const Value: Integer): THyperparametersParams;
    function BatchSize(const Value: Integer): THyperparametersParams;
  end;

  TTrainingDataParams = class(TJSONParam)
  public
    function Output(const Value: string): TTrainingDataParams;
    function TextInput(const Value: string): TTrainingDataParams;
    class function New(const Input, Output: string): TTrainingDataParams;
  end;

  TrainData = TTrainingDataParams;

  TTuningTaskParams = class(TJSONParam)
  public
    function TrainingData(const JSONLFileName: string): TTuningTaskParams; overload;
    function TrainingData(const Value: TArray<TrainData>): TTuningTaskParams; overload;
    function Hyperparameters(ParamProc: TProcRef<THyperparametersParams>): TTuningTaskParams;
  end;

  TTunedModelParams = class(TJSONParam)
  public
    function DisplayName(const Value: string): TTunedModelParams;
    function Description(const Value: string): TTunedModelParams;
    function TuningTask(const Value: TTuningTaskParams): TTunedModelParams; overload;
    function ReaderProjectNumbers(const Value: TArray<Integer>): TTunedModelParams;
    function TunedModelSource(const Value: string): TTunedModelParams;
    function BaseModel(const Value: string): TTunedModelParams;
    function Temperature(const Value: Double): TTunedModelParams;
    function TopP(const Value: Double): TTunedModelParams;
    function TopK(const Value: Integer): TTunedModelParams;
  end;

  TMetadata = class
  private
    [JsonNameAttribute('@type')]
    FType: string;
    FTotalSteps: string;
    FTunedModel: string;
  public
    property &Type: string read FType write FType;
    property TotalSteps: string read FTotalSteps write FTotalSteps;
    property TunedModel: string read FTunedModel write FTunedModel;
  end;

  TTunedModelTraining = class
  private
    FName: string;
    FMetadata: TMetadata;
  public
    property Name: string read FName write FName;
    property Metadata: TMetadata read FMetadata write FMetadata;
    destructor Destroy; override;
  end;

  TTuningSnapshot = class
  private
    FStep: Integer;
    FEpoch: Integer;
    FMeanLoss: Double;
    FComputeTime: string;
  public
    property Step: Integer read FStep write FStep;
    property Epoch: Integer read FEpoch write FEpoch;
    property MeanLoss: Double read FMeanLoss write FMeanLoss;
    property ComputeTime: string read FComputeTime write FComputeTime;
  end;

  THyperParameters = class
  private
    FLearningRate: Double;
    FLearningRateMultiplier: Double;
    FEpochCount: Integer;
    FBatchSize: Integer;
  public
    property LearningRate: Double read FLearningRate write FLearningRate;
    property LearningRateMultiplier: Double read FLearningRateMultiplier write FLearningRateMultiplier;
    property EpochCount: Integer read FEpochCount write FEpochCount;
    property BatchSize: Integer read FBatchSize write FBatchSize;
  end;

  TTuningTask = class
  private
    FStartTime: string;
    FCompleteTime: string;
    FSnapshots: TArray<TTuningSnapshot>;
    FHyperparameters: THyperParameters;
  public
    property StartTime: string read FStartTime write FStartTime;
    property CompleteTime: string read FCompleteTime write FCompleteTime;
    property Snapshots: TArray<TTuningSnapshot> read Fsnapshots write Fsnapshots;
    property Hyperparameters: THyperParameters read FHyperparameters write FHyperparameters;
    destructor Destroy; override;
  end;

  TTunedModelSource = class
  private
    FTunedModel: string;
    FBaseModel: string;
  public
    property TunedModel: string read FTunedModel write FTunedModel;
    property BaseModel: string read FBaseModel write FBaseModel;
  end;

  TTunedModel = class
  private
    FName: string;
    FDisplayName: string;
    FDescription: string;
    [JsonReflectAttribute(ctString, rtString, TModelStateInterceptor)]
    FState: TModelState;
    FCreateTime: string;
    FUpdateTime: string;
    FTuningTask: TTuningTask;
    FReaderProjectNumbers: TArray<Int64>;
    FTunedModelSource: TTunedModelSource;
    FBaseModel: string;
    FTemperature: Double;
    FTopP: Double;
    FTopK: Integer;
  public
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Description: string read FDescription write FDescription;
    property State: TModelState read FState write FState;
    property CreateTime: string read FCreateTime write FCreateTime;
    property UpdateTime: string read FUpdateTime write FUpdateTime;
    property TuningTask: TTuningTask read FTuningTask write FTuningTask;
    property ReaderProjectNumbers: TArray<Int64> read FReaderProjectNumbers write FReaderProjectNumbers;
    property TunedModelSource: TTunedModelSource read FTunedModelSource write FTunedModelSource;
    property BaseModel: string read FBaseModel write FBaseModel;
    property Temperature: Double read FTemperature write FTemperature;
    property TopP: Double read FTopP write FTopP;
    property TopK: Integer read FTopK write FTopK;
    destructor Destroy; override;
  end;

  TTuneModels = class
  private
    FTunedModels: TArray<TTunedModel>;
    FNextPageToken: string;
  public
    property TunedModels: TArray<TTunedModel> read FTunedModels write FTunedModels;
    property NextPageToken: string read FNextPageToken write FNextPageToken;
    destructor Destroy; override;
  end;

  TTuneModelDelete = class
  end;

  TFineTuneRoute = class(TGeminiAPIRoute)
    function Create(const Value: TJSONObject): TTunedModelTraining;
    function List(const PageSize: Integer;
      const PageToken, Filter: string): TTuneModels;
    function Retrieve(const TuneModelName: string): TTunedModel;
//    function Update(const Value: TJSONObject; const UpdateMask: string): TTunedModel;
    function Delete(const TuneModelName: string): TTuneModelDelete;
  end;

implementation

uses
  System.StrUtils, System.IOUtils, System.Rtti, Rest.Json, Gemini.Async.Params;

type
  TTuningTaskHelper = record
    class function FileDataToJSONArray(const JSONLFileName: string): TJSONArray; static;
    class function ExamplesBuilder(const Value: TArray<TrainData>): TJSONObject; overload; static;
    class function ExamplesBuilder(const JSONLFileName: string): TJSONObject; overload; static;
  end;

{ TModelStateHelper }

class function TModelStateHelper.Create(const Value: string): TModelState;
begin
  var Index := IndexStr(AnsiUpperCase(Value), [
         'STATE_UNSPECIFIED', 'CREATING', 'ACTIVE', 'FAILED']);
  if Index = -1 then
    raise Exception.CreateFmt('"Model state" unknown : %s', [Value]);
  Result := TModelState(Index);
end;

function TModelStateHelper.ToString: string;
begin
  case Self of
    STATE_UNSPECIFIED:
      Exit('STATE_UNSPECIFIED');
    CREATING:
      Exit('CREATING');
    ACTIVE:
      Exit('ACTIVE');
    FAILED:
      Exit('FAILED');
  end;
end;

{ TModelStateInterceptor }

function TModelStateInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TModelState>.ToString;
end;

procedure TModelStateInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TModelState.Create(Arg)));
end;

{ TTunedModelParams }

function TTunedModelParams.BaseModel(const Value: string): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('baseModel', Value));
end;

function TTunedModelParams.Description(const Value: string): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('description', Value));
end;

function TTunedModelParams.DisplayName(const Value: string): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('displayName', Value));
end;

function TTunedModelParams.ReaderProjectNumbers(
  const Value: TArray<Integer>): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('readerProjectNumbers', Value));
end;

function TTunedModelParams.Temperature(const Value: Double): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('temperature', Value));
end;

function TTunedModelParams.TopK(const Value: Integer): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('topK', Value));
end;

function TTunedModelParams.TopP(const Value: Double): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('topP', Value));
end;

function TTunedModelParams.TunedModelSource(
  const Value: string): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('tunedModelSource', TJSONObject.Create.AddPair('tunedModel', Value)));
end;

function TTunedModelParams.TuningTask(
  const Value: TTuningTaskParams): TTunedModelParams;
begin
  Result := TTunedModelParams(Add('tuningTask', Value.Detach));
end;

{ TTrainingDataParams }

class function TTrainingDataParams.New(const Input,
  Output: string): TTrainingDataParams;
begin
  Result := TTrainingDataParams.Create.TextInput(Input).Output(Output);
end;

function TTrainingDataParams.Output(const Value: string): TTrainingDataParams;
begin
  Result := TTrainingDataParams(Add('output', Value));
end;

function TTrainingDataParams.TextInput(
  const Value: string): TTrainingDataParams;
begin
  Result := TTrainingDataParams(Add('textInput', Value));
end;

{ THyperparametersParams }

function THyperparametersParams.BatchSize(const Value: Integer): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('batchSize', Value));
end;

function THyperparametersParams.EpochCount(const Value: Integer): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('epochCount', Value));
end;

function THyperparametersParams.LearningRate(const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('learningRate', Value));
end;

function THyperparametersParams.LearningRateMultiplier(
  const Value: Double): THyperparametersParams;
begin
  Result := THyperparametersParams(Add('learningRateMultiplier', Value));
end;

{ TTuningTaskParams }

function TTuningTaskParams.Hyperparameters(
  ParamProc: TProcRef<THyperparametersParams>): TTuningTaskParams;
begin
  var Params := THyperparametersParams.Create;
  if Assigned(ParamProc) then
    ParamProc(Params);
  Result := TTuningTaskParams(Add('hyperparameters', Params.Detach));
end;

function TTuningTaskParams.TrainingData(
  const Value: TArray<TrainData>): TTuningTaskParams;
begin
  Result := TTuningTaskParams(Add('training_data', TTuningTaskHelper.ExamplesBuilder(Value)));
end;

function TTuningTaskParams.TrainingData(
  const JSONLFileName: string): TTuningTaskParams;
begin
  Result := TTuningTaskParams(Add('training_data', TTuningTaskHelper.ExamplesBuilder(JSONLFileName)));
end;

{ TTuningTaskHelper }

class function TTuningTaskHelper.ExamplesBuilder(
  const Value: TArray<TrainData>): TJSONObject;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  var JSONExamples := TJSONObject.Create.AddPair('examples', JSONArray);
  Result := TJSONObject.Create.AddPair('examples', JSONExamples);
end;

class function TTuningTaskHelper.ExamplesBuilder(
  const JSONLFileName: string): TJSONObject;
begin
  if not FileExists(JSONLFileName) then
    raise Exception.CreateFmt('Training file not found : %s', [JSONLFileName]);
  var JSONExamples := TJSONObject.Create.AddPair('examples', FileDataToJSONArray(JSONLFileName));
  Result := TJSONObject.Create.AddPair('examples', JSONExamples);
end;

class function TTuningTaskHelper.FileDataToJSONArray(
  const JSONLFileName: string): TJSONArray;
var
  Ligne: string;
begin
  Result := TJSONArray.Create;
  var JSONLFile := TFileStream.Create(JSONLFileName, fmOpenRead or fmShareDenyWrite);
  var StreamReader := TStreamReader.Create(JSONLFile, TEncoding.UTF8);
  try
    while not StreamReader.EndOfStream do
      begin
        Ligne := StreamReader.ReadLine;
        Result.Add(TJSONObject.ParseJSONValue(Ligne) as TJSONObject)
      end;
  finally
    StreamReader.Free;
    JSONLFile.Free;
  end;
end;

{ TFineTuneRoute }

function TFineTuneRoute.Create(
  const Value: TJSONObject): TTunedModelTraining;
begin
  Result := API.Post<TTunedModelTraining>('tunedModels', Value);
  Value.Free;
end;

function TFineTuneRoute.Delete(const TuneModelName: string): TTuneModelDelete;
begin
  Result := API.Delete<TTuneModelDelete>(TuneModelName);
end;

function TFineTuneRoute.List(const PageSize: Integer;
  const PageToken, Filter: string): TTuneModels;
begin
  Result := API.Get<TTuneModels>('tunedModels', ParamsBuilder(PageSize, PageToken, Filter));
end;

function TFineTuneRoute.Retrieve(const TuneModelName: string): TTunedModel;
begin
  Result := API.Get<TTunedModel>(TuneModelName);
end;

//function TFineTuneRoute.Update(const Value: TJSONObject;
//  const UpdateMask: string): TTunedModel;
//begin
//
//end;

{ TTunedModelTraining }

destructor TTunedModelTraining.Destroy;
begin
  if Assigned(FMetadata) then
    FMetadata.Free;
  inherited;
end;

{ TTuningTask }

destructor TTuningTask.Destroy;
begin
  for var Item in FSnapshots do
    Item.Free;
  if Assigned(FHyperparameters) then
    FHyperparameters.Free;
  inherited;
end;

{ TTunedModel }

destructor TTunedModel.Destroy;
begin
  if Assigned(FTuningTask) then
    FTuningTask.Free;
  if Assigned(FTunedModelSource) then
    FTunedModelSource.Free;
  inherited;
end;

{ TTuneModels }

destructor TTuneModels.Destroy;
begin
  for var Item in FTunedModels do
    Item.Free;
  inherited;
end;

end.

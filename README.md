# Delphi Gemini API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20the%2010/15/2024-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Remarks](#remarks)
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Asynchronous callback mode management](#Asynchronous-callback-mode-management)
    - [Gemini Models Overview](#Gemini-Models-Overview)
    - [Embeddings](#embeddings)
    - [Generate text](#Generate-text)
        - [Generate text from text-only input](#Generate-text-from-text-only-input)
- [Contributing](#contributing)
- [License](#license)

<br/>

# Introduction

Welcome to the unofficial Delphi Gemini API library! This project is designed to offer a seamless interface for Delphi developers to interact with the Gemini public API, enabling easy integration of advanced natural language processing capabilities into your Delphi applications. Whether you're looking to generate text, create embeddings, use conversational models, or generate code, this library provides a straightforward and efficient solution.

Gemini is a robust natural language processing API that empowers developers to add sophisticated AI features to their applications. For more information, refer to the official [Gemini documentation](https://ai.google.dev/gemini-api/docs).

<br/>

# Remarks

> [!IMPORTANT]
>
> This is an unofficial library. **Google** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [Gemini](https://ai.google.dev/api) public API.

<br/>

# Usage

<br/>

## Initialization

To initialize the API instance, you need to [obtain an API key from Google](https://aistudio.google.com/app/apikey?hl=fr).

Once you have a token, you can initialize `IGemini` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

> [!NOTE]
>```Pascal
>uses Gemini;
>
>var Gemini := TGeminiFactory.CreateInstance(API_KEY);
>```

<br/>

## Asynchronous callback mode management

In the context of asynchronous methods, for a method that does not involve streaming, callbacks use the following generic record: `TAsynCallBack<T> = record` defined in the `Gemini.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnError: TProc<TObject, string>; 
```
<br/>

For methods requiring streaming, callbacks use the generic record `TAsynStreamCallBack<T> = record`, also defined in the `Gemini.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnProgress: TProc<TObject, T>;
       OnError: TProc<TObject, string>;
       OnCancellation: TProc<TObject>;
       OnDoCancel: TFunc<Boolean>;
```

The name of each property is self-explanatory; if needed, refer to the internal documentation for more details.

<br/>

## Gemini Models Overview

List the various models available in the API. You can refer to the Models documentation to understand what models are available. See [Models Documentation](https://ai.google.dev/gemini-api/docs/models/gemini?hl=fr).

Alongside its standard models, the `Gemini` API also includes experimental models offered in Preview mode. These models are intended for testing and feedback purposes and are not suitable for production use. `Google` releases these experimental models to gather insights from users, but there's no commitment that they will be developed into stable models in the future.

Retrieving the list of available models through the API.

1. **Synchronously**

```Pascal
// uses Gemini, Gemini.Models;

  var List := Gemini.Models.List;
  try
    for var Item in List.Models do
      WriteLn( Item.DisplayName );
  finally
    List.Free;
  end;
```

2. **Asynchronously** : Using query parameters

```Pascal
// uses Gemini, Gemini.Models;

// Declare "Next" a global variable, var Next: string;

  Gemini.Models.AsynList(5, Next,
    function : TAsynModels
    begin
      Result.Sender := Memo1;   // Set a TMemo on the form

      Result.OnStart :=
        procedure (Sender: TObject)
        begin
          // Handle the start
        end;

      Result.OnSuccess :=
        procedure (Sender: TObject; List: TModels)
        begin
          var M := Sender as TMemo;
          for var Item in List.Models do
            begin
              M.Text := M.Text + sLineBreak + Item.DisplayName;
              Next := List.NextPageToken;
            end;
          M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
        end;

      Result.OnError :=
        procedure (Sender: TObject; Error: string)
        begin
          //Handle the error message
        end
    end);
```
The previous example displays the models in batches of 5.

3. **Asynchronously** : Retrive a model.

```Pascal
// uses Gemini, Gemini.Models;

// Set a TMemo on the form

  Gemini.Models.AsynList('models/Gemini-1.5-flash',
    function : TAsynModel
    begin
      Result.OnSuccess :=
        procedure (Sender: TObject; List: TModel)
        begin
          Memo1.Text := Memo1.Text + sLineBreak + List.DisplayName;
          M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
        end;
    end);
```

<br/>

## Embeddings

**Embeddings** are numerical representations of text inputs that enable a variety of unique applications, including *clustering*, *measuring similarity*, and *information retrieval*. For an introduction, take a look at the [Embeddings guide](https://ai.google.dev/gemini-api/docs/embeddings). <br/>
See also the [embeddings models](https://ai.google.dev/gemini-api/docs/models/gemini#text-embedding).

In the following examples, we will use the procedure 'Display' to simplify the examples.

```Pascal
  procedure Display(Sender: TObject; const Chat: TChat);
  begin
    var M := Sender as TMemo;
    for var Item in Embed.Embedding.Values do
      begin
        M.Lines.Text := M.Text + sLineBreak + Item.ToString;
      end;
    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
  end;
```

1. **Synchronously** : Get the vector representation of the text *'This is an example'*.

```Pascal
// uses Gemini, Gemini.Embeddings; 

  var Integration := Gemini.Embeddings.Create('models/text-embedding-004',
            procedure (Params: TEmbeddingParams)
            begin
              Params.Content(['This is an example']);
            end);
  // For displaying, add a TMemo on the form
  try
    Display(Memo1, Integration)
  finally
    Integration.Free;
  end;
```

2. **Asynchronously** : Get the vector representation of the text *'This is an example'* and *'Second example'*.<br/>
 - The vectors will be of reduced dimension (20).

```Pascal
// uses Gemini, Gemini.Embeddings; 

    Gemini.Embeddings.AsynCreateBatch('models/text-embedding-004',
       procedure (Parameters: TEmbeddingBatchParams)
       begin
         Parameters.Requests(
           [
            TEmbeddingRequestParams.Create(
              procedure (var Params: TEmbeddingRequestParams)
              begin
                Params.Content(['This is an example']);
                Params.OutputDimensionality(20);
              end),

              TEmbeddingRequestParams.Create(
              procedure (var Params: TEmbeddingRequestParams)
              begin
                Params.Content(['Second example']);
                Params.OutputDimensionality(20);
              end)
           ]);
       end,
       // For displaying, add a TMemo on the form
       function : TAsynEmbeddings
       begin
         Result.Sender := Memo1; // Set a TMemo on the form
         Result.OnSuccess := Display;
       end);  
```
<br/>

## Generate text

The Gemini API enables text generation from a variety of inputs, including text, images, video, and audio. It can be used for a range of applications, such as:

- Creative writing
- Describing or interpreting media assets
- Text completion
- Summarizing open-form text
- Translating between languages
- Chatbots
- Your own unique use cases

In the following examples, we will use two procedures ('Display' and 'DisplayError') to simplify the examples.

```Pascal
  procedure Display(Sender: TObject; const Chat: TChat);
  begin
    var M := Sender as TMemo;
    for var Item in Chat.Candidates do
      begin
        if Item.FinishReason = STOP then
          for var SubItem in Item.Content.Parts do
            begin
              M.Lines.Text := M.Text + sLineBreak + SubItem.Text;
            end;
        M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
      end;
  end;
```

```Pascal
  procedure DisplayError(Sender: TObject; Error: string);
  begin
    var M := Sender as TMemo;
    M.Lines.Text := M.Text + sLineBreak + Error;
    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
  end;  
```

<br/>

### Generate text from text-only input

Synchronous mode
```Pascal
// uses Gemini, Gemini.Chat;

  var Chat := Gemini.Chat.Create('models/gemini-1.5-pro',
    procedure (Params: TChatParams)
    begin
      Params.Contents([TPayload.Add('Write a story about a magic backpack.')]);
    end);
  // For displaying, add a TMemo on the form
  try
    Display(Memo1, Chat);
  finally
    Chat.Free;
  end;
```

Asynchronous mode
```Pascal
// uses Gemini, Gemini.Chat;

  Gemini.Chat.AsynCreate('models/gemini-1.5-pro',
    procedure (Params: TChatParams)
    begin
      Params.Contents([TPayload.Add('Write a story about a magic backpack.')]);
    end,
    // For displaying, add a TMemo on the form
    function : TAsynChat
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := DisplayError;
    end);
```

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
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
        - [Generate text from text-and-image input](#Generate-text-from-text-and-image-input)
        - [Generate a text stream](#Generate-a-text-stream)
        - [Build an interactive chat](#Build-an-interactive-chat)
        - [Configure text generation](#Configure-text-generation)
    - [Document processing](#Document-processing)
        - [Upload a document and generate content](#Upload-a-document-and-generate-content)
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

> [!TIP]
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

In the following examples, we will use the procedures 'Display' to simplify the examples.

> [!TIP]
>```Pascal
>  procedure Display(Sender: TObject; Embed: TEmbedding); overload;
>  begin
>    var M := Sender as TMemo;
>    for var Item in Embed.Embedding.Values do
>      begin
>        M.Lines.Text := M.Text + sLineBreak + Item.ToString;
>      end;
>    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>  end;
>
>  procedure Display(Sender: TObject; Embed: TEmbeddings); overload;
>  begin
>    var M := Sender as TMemo;
>    for var Item in Embed.Embeddings do
>      begin
>        for var Value in Item.Values do
>          begin
>            M.Lines.Text := M.Text + sLineBreak + Value.ToString;
>          end;
>        M.Lines.Text := M.Text + sLineBreak;
>      end;
>    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>end;
>```

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

The Gemini API enables [`text generation`](https://ai.google.dev/api/generate-content#method:-models.generatecontent) from a variety of inputs, including text, images, video, and audio. It can be used for a range of applications, such as:

- Creative writing
- Describing or interpreting media assets
- Text completion
- Summarizing open-form text
- Translating between languages
- Chatbots
- Your own unique use cases

In the following examples, we will use two procedures ('Display' and 'DisplayError') to simplify the examples.
> [!TIP] 
>```Pascal
>  procedure Display(Sender: TObject; Chat: TChat); overload;
>  begin
>    var M := Sender as TMemo;
>    for var Item in Chat.Candidates do
>      begin
>        if Item.FinishReason = STOP then
>          for var SubItem in Item.Content.Parts do
>            begin
>              M.Lines.Text := M.Text + sLineBreak + SubItem.Text;
>            end;
>        M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>      end;
>  end;
>```
>
>```Pascal
>  procedure DisplayError(Sender: TObject; Error: string);
>  begin
>    var M := Sender as TMemo;
>    M.Lines.Text := M.Text + sLineBreak + Error;
>    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>  end;  
>```

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

In this example, the prompt ("Write a story about a magic backpack") doesn’t include output examples, system instructions, or formatting details, making it a [`zero-shot`](https://ai.google.dev/gemini-api/docs/models/generative-models#zero-shot-prompts) approach. In some cases, using a [`one-shot`](https://ai.google.dev/gemini-api/docs/models/generative-models#one-shot-prompts) or [`few-shot`](https://ai.google.dev/gemini-api/docs/models/generative-models#few-shot-prompts) prompt could generate responses that better match user expectations. You might also consider adding [`system instructions`](https://ai.google.dev/gemini-api/docs/system-instructions?lang=rest) to guide the model in understanding the task or following specific guidelines.

<br/>

### Generate text from text-and-image input

The Gemini API supports multimodal inputs that combine text with media files. The example below demonstrates how to generate text from an input that includes both text and images.

```Pascal
  var Ref := 'D:\MyFolder\Images\Image.png';
  Gemini.Chat.AsynCreate('models/gemini-1.5-pro',
    procedure (Params: TChatParams)
    begin
      Params.Contents([TPayload.Add('Describe this image.', [Ref])]);
    end,
    // For displaying, add a TMemo on the form
    function : TAsynChat
    begin
      Result.Sender := Memo1;
      Result.OnSuccess := Display;
      Result.OnError := DisplayError;
    end);
```

In multimodal prompting, as with text-only prompting, various strategies and refinements can be applied. Based on the results from this example, you may want to add additional steps to the prompt or provide more specific instructions. For further information, [refer to strategies for file-based prompting](https://ai.google.dev/gemini-api/docs/file-prompting-strategies).

<br/>

### Generate a text stream

The model typically returns a response only after finishing the entire text generation process. Faster interactions can be achieved by enabling streaming, allowing partial results to be handled as they’re generated.

The example below demonstrates how to implement streaming using the [`streamGenerateContent`](https://ai.google.dev/api/generate-content#method:-models.streamgeneratecontent) method to generate text from a text-only input prompt.

Declare this method for displaying.

> [!TIP]
>```Pascal
>  procedure Display(Sender: TObject; Candidate: TChatCandidate); overload;
>  begin
>    var M := Sender as TMemo;
>    var Buffer := Candidate.Content.Parts[0].Text;
>    for var i := 1 to Length(Buffer) do
>      begin
>        M.Lines.Text := M.Text + Buffer[i];
>        M.Lines.BeginUpdate;
>        try
>          Application.ProcessMessages;
>          M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>        finally
>          M.Lines.EndUpdate;
>        end;
>      end;
>  end;
>```
>
>```Pascal
>  procedure DisplayCR(Sender: TObject);
>  begin
>    var M := Sender as TMemo;
>    M.Lines.Text := M.Text + sLineBreak;
>    M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>  end;
>

```Pascal
// uses Gemini, Gemini.Chat, Gemini.Safety;

  Gemini.Chat.CreateStream('models/gemini-1.5-flash',
    procedure (Params: TChatParams)
    begin
      Params.Contents([TPayload.Add('Write a story about a magic backpack.')]);
    end,
    // For displaying, add a TMemo on the form
    procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if IsDone then
        begin
          DisplayCR(Memo1);
        end;
      if Assigned(Chat) then
        begin
          for var Item in Chat.Candidates do
            begin
              if Item.FinishReason <> TFinishReason.SAFETY then
                begin
                  Display(Memo1, Item);
                end;
            end;
        end;
    end);
```

<br/>

### Build an interactive chat

You can leverage the Gemini API to create interactive chat experiences tailored for your users. By using the API’s chat feature, you can gather multiple rounds of questions and responses, enabling users to progress gradually toward their answers or receive assistance with complex, multi-part issues. This functionality is especially useful for applications that require continuous communication, such as chatbots, interactive learning tools, or customer support assistants.

Here’s an example of a basic chat implementation:

```Pascal
// uses Gemini, Gemini.Chat, Gemini.Safety;

  Gemini.Chat.CreateStream('models/gemini-1.5-flash',
    procedure (Params: TChatParams)
    begin
      Params.Contents([
        TPayload.User('Hello'),
        TPayload.Assistant('Great to meet you. What would you like to know?'),
        TPayload.User('I have two dogs in my house. How many paws are in my house?')
      ]);
    end,
    // For displaying, add a TMemo on the form
    procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if IsDone then
        begin
          DisplayCR(Memo1);
        end;
      if Assigned(Chat) then
        begin
          for var Item in Chat.Candidates do
            begin
              if Item.FinishReason <> TFinishReason.SAFETY then
                begin
                  Display(Memo1, Item);
                end;
            end;
        end;
    end);  
```
<br/>

Here’s an example of a asynchronous chat implementation

Declare this method for displaying.
> [!TIP]
>```Pascal
>  procedure DisplayStream(Sender: TObject; Chat: TChat);
>  begin
>    Display(Sender, Chat.Candidates[0]);
>  end;
>```

```Pascal
// uses Gemini, Gemini.Chat, Gemini.Safety;

  Gemini.Chat.AsynCreateStream('models/gemini-1.5-flash',
    procedure (Params: TChatParams)
    begin
      Params.Contents([
        TPayload.User('Hello'),
        TPayload.Assistant('Great to meet you. What would you like to know?'),
        TPayload.User('I have two dogs in my house. How many paws are in my house?')
      ]);
    end,
    // For displaying, add a TMemo on the form    
    function : TAsynChatStream
    begin
      Result.Sender := Memo1;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := DisplayCR;
      Result.OnError := DisplayError;
    end);
```

<br/>

### Configure text generation

Each prompt sent to the model includes settings that control how responses are generated. You can adjust these settings using the `GenerationConfig`, which allows you to customize various [parameters](https://ai.google.dev/gemini-api/docs/models/generative-models#model-parameters). If no configurations are applied, the model will rely on default settings, which may differ depending on the model.
 
Here's an example demonstrating how to adjust several of these options.

```Pascal
// uses Gemini, Gemini.Chat, Gemini.Safety;
  var GenerateContent := Gemini.Chat.Create('models/gemini-1.5-flash',
    procedure (Params: TChatParams)
    begin
      Params.Contents([TPayload.Add('Write a story about a magic backpack.')]);

      {--- Specifies safety settings to block unsafe content. }
      Params.SafetySettings([
        TSafety.DangerousContent(BLOCK_ONLY_HIGH),
        TSafety.HateSpeech(BLOCK_MEDIUM_AND_ABOVE) ]);

      {--- Configures generation options for the model's outputs. }
      Params.GenerationConfig(
        procedure (var Params: TGenerationConfig)
        begin
          Params.StopSequences(['Title']);
          Params.Temperature(1.0);
          Params.MaxOutputTokens(800);
          Params.TopP(0.8);
          Params.TopK(10);
        end);
    end);
```

- The Gemini API offers adjustable safety settings that you can configure during the prototyping phase to decide if your application needs a stricter or more flexible safety setup. [Refer to the official documentation](https://ai.google.dev/gemini-api/docs/safety-settings).
See also the `Gemini.Safety.pas` unit and the `TSafety` record.

- The generation configuration allows for setting up the output production of a model. A complete description of the manageable parameters can be found at the following [`GitHub address`](https://github.com/google-gemini/generative-ai-python/blob/main/docs/api/google/generativeai/types/GenerationConfig.md). Internally, these parameters are defined within the `TGenerationConfig` class, which extends TJSONParam in the `Gemini.Chat.pas` unit.

<br/>

## Document processing

The Gemini API can handle and perform inference on uploaded PDF documents. Once a PDF is provided, the Gemini API can:

- Describe or answer questions about the content
- Summarize the content
- Generate extrapolations based on the content

This guide illustrates various methods for prompting the Gemini API using uploaded PDF documents. All outputs are text-only.

The `Gemini 1.5 Pro` and `1.5 Flash` models can handle up to **3,600 pages** per document. Supported file types for text data include:

- PDF: `application/pdf`
- JavaScript: `application/x-javascript, text/javascript`
- Python: `application/x-python, text/x-python`
- TXT: `text/plain`
- HTML: `text/html`
- CSS: `text/css`
- Markdown: `text/md`
- CSV: `text/csv`
- XML: `text/xml`
- RTF: `text/rtf`
Each page consists of **258 tokens**.

To achieve optimal results:

Ensure pages are oriented correctly before uploading. Use **high-quality images** without blurring. If uploading a single page, add the text prompt following the page.

There aren’t any strict pixel limits for documents beyond the model’s context capacity. Larger pages are scaled down to a maximum of **3072x3072 pixels** while keeping their aspect ratio, whereas smaller pages are scaled up to **768x768 pixels**. However, there’s no cost savings for using smaller images, other than reduced bandwidth, nor any performance boost for higher-resolution pages.

<br/>

### Upload a document and generate content

You can upload documents of any size by using the File API. Always rely on the File API whenever the combined size of the request—including files, text prompt, system instructions, and any other data—exceeds 20 MB.

> [!WARNING]



<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
namespace Figurine

open System
open PuppeteerSharp

module Task =
    open System.Threading.Tasks

    let force (t: Task<'t>) = t.Result
    let forceUnit (t: Task) = t.Wait()

    let withTimeout (taskFn: unit -> Task<'t>) = failwith "implement withTimeout"

module Response =
    let check (r: Response) = if not r.Ok then failwith "bad response"

module Types =
    type BrowserCtx = {
        /// the actual unit of work
        page: Page
        dispose : unit -> unit
        compareTimeout : System.TimeSpan
        navigateTimeout : System.TimeSpan
    }
    with
        member x.CurrentUrl = x.page.Url

        interface IDisposable with
            member x.Dispose () = x.dispose ()

module Browser =
    open Types

    type LaunchMethod =
    | Launch of LaunchOptions
    | Connect of ConnectOptions

    let create (method: LaunchMethod) =
        let factory =
            match method with
            | Launch opts -> Puppeteer.LaunchAsync opts
            | Connect opts -> Puppeteer.ConnectAsync opts
        let b = Task.force factory
        let p = b.NewPageAsync() |> Task.force

        { page = p
          dispose = b.Dispose
          compareTimeout = TimeSpan.FromSeconds 10.
          navigateTimeout = TimeSpan.FromSeconds 30. }

    let createFromExe (path: string) =
        create (Launch (LaunchOptions(ExecutablePath = path)))

module Functions =
    open Types
    open Newtonsoft.Json.Linq

    let (|JsonString|_|) (desired: string) (expected: JToken) =
        if expected.Type = JTokenType.String && string expected = desired
        then Some ()
        else None

    let js script (d: BrowserCtx) = failwith "boom"
    let puts text (d: BrowserCtx) = failwith "boom"
    let waitFor condition (d: BrowserCtx) = failwith "boom"

    let findSelector selector (d: BrowserCtx) =
        d.page.QuerySelectorAsync selector // todo: timeout
        |> Task.force
        |> Option.ofObj

    let findManySelector selector (d: BrowserCtx) =
        d.page.QuerySelectorAllAsync selector // todo: timeout
        |> Task.force

    let url u (d: BrowserCtx) =
        d.page.GoToAsync u
        |> Task.force
        |> Response.check

    let write (text: string) (element: ElementHandle) =
        element.TypeAsync(text) |> Task.forceUnit

    let read (element: ElementHandle) (d: BrowserCtx) =
        match element.RemoteObject.["className"] with
        | JsonString "HtmlInputElement"
        | JsonString "HtmlTextAreaElement" ->
            element.GetPropertyAsync("value")
            |> Task.force
            |> fun js -> string js.RemoteObject

        // TODO: implement select text get here
        | _ -> element.GetPropertyAsync("innerText")
               |> Task.force
               |> fun js -> string js.RemoteObject


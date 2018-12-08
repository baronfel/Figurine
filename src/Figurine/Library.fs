namespace Figurine

open System
open PuppeteerSharp

[<RequireQualifiedAccess>]
module Task =
    open System.Threading.Tasks
    open System.Threading
    
    let force (t : Task<'t>) = t.Result
    let forceUnit (t : Task) = t.Wait()
    
    let withTimeout (timeSpan : System.TimeSpan) (taskFn : unit -> Task<'t>) =
        async { 
            let tcs = new TaskCompletionSource<bool>()
            let cts = new CancellationTokenSource(timeSpan)
            use reg =
                cts.Token.Register
                    (System.Action<obj>
                         (fun s -> 
                         (s :?> TaskCompletionSource<bool>).TrySetResult(true) 
                         |> ignore), tcs)
            let t = taskFn()
            let! first = Task.WhenAny(t :> Task, tcs.Task) |> Async.AwaitTask
            if not <| first.Equals(t) then 
                return failwithf "could not complete operation in %f seconds" 
                           timeSpan.TotalSeconds
            else return! Async.AwaitTask t
        }
        |> Async.StartAsTask
    
    let withTimeoutUnit (timeSpan : System.TimeSpan) (taskFn : unit -> Task) =
        withTimeout timeSpan 
            (fun () -> taskFn().ContinueWith<unit> (fun t -> ()))

module Response =
    let check (r : Response) =
        if not r.Ok then failwith "bad response"

module Types =
    type BrowserCtx =
        { /// the actual unit of work
          page : Page
          dispose : unit -> unit
          compareTimeout : System.TimeSpan
          navigateTimeout : System.TimeSpan }
        member x.CurrentUrl = x.page.Url
        interface IDisposable with
            member x.Dispose() = x.dispose()

module Handle =
    let className (handle : ElementHandle) = handle.RemoteObject.["className"]
    
    type PropertyResult =
        { ``type`` : string
          value : string }
    
    let property propName (ctx : Types.BrowserCtx) (handle : ElementHandle) =
        fun () -> handle.GetPropertyAsync(propName)
        |> Task.withTimeout (ctx.compareTimeout)
        |> Task.force
        |> fun js -> 
            let jobj = js.RemoteObject :?> Newtonsoft.Json.Linq.JObject
            let typed = jobj.ToObject<PropertyResult>()
            typed.value
    
    let children selector (ctx : Types.BrowserCtx) (handle : ElementHandle) =
        fun () -> handle.QuerySelectorAllAsync selector
        |> Task.withTimeout ctx.compareTimeout
        |> Task.force

module Browser =
    open Types
    
    type LaunchMethod =
        | Launch of LaunchOptions
        | Connect of ConnectOptions
    
    let create (method : LaunchMethod) =
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
    
    let createFromExe (path : string) =
        create (Launch(LaunchOptions(ExecutablePath = path)))

module Functions =
    open Types
    open Newtonsoft.Json.Linq
    
    let private (|JsonString|_|) (desired : string) (expected : JToken) =
        if expected.Type = JTokenType.String && string expected = desired then 
            Some()
        else None
    
    /// execute the given script (along with provided arguments) on the current page
    let js script args (d : BrowserCtx) =
        d.page.SetJavaScriptEnabledAsync true |> Task.forceUnit
        fun () -> d.page.EvaluateFunctionAsync(script, args)
        |> Task.withTimeout d.compareTimeout
        |> Task.force
    
    let private sleepSpan = TimeSpan.FromSeconds 0.5
    
    /// wait on this page until the condition holds
    let waitFor (condition : unit -> bool) (d : BrowserCtx) =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        
        let rec wait span =
            if sw.Elapsed >= span then 
                failwith "Timed out checking for condition"
            if condition() then ()
            else 
                System.Threading.Thread.Sleep sleepSpan
                wait (span - sleepSpan)
        wait d.compareTimeout
        sw.Stop()
    
    /// try to find an element on the page that matches the given selector
    let findSelector selector (d : BrowserCtx) =
        fun () -> d.page.QuerySelectorAsync selector
        |> Task.withTimeout d.compareTimeout
        |> Task.force
        |> Option.ofObj
    
    /// find all elements on the page that match the given selector
    let findManySelector selector (d : BrowserCtx) =
        fun () -> d.page.QuerySelectorAllAsync selector
        |> Task.withTimeout d.compareTimeout
        |> Task.force
    
    /// retrieve the current URL for the page
    let currentUrl (d : BrowserCtx) = d.page.Url
    
    /// navigate the page to the given URL
    let url u (d : BrowserCtx) =
        if currentUrl d = u then ()
        else 
            fun () -> d.page.GoToAsync u
            |> Task.withTimeout d.navigateTimeout
            |> Task.force
            |> Response.check
    
    /// write the provided text to the element
    let write (text : string) (element : ElementHandle) (d : BrowserCtx) =
        fun () -> element.TypeAsync(text)
        |> Task.withTimeoutUnit d.compareTimeout
        |> Task.force
    
    /// click the provided element with the left mouse button
    let click (element : ElementHandle) =
        element.ClickAsync(Input.ClickOptions()) |> Task.forceUnit
    
    /// read the inner value from the element.
    /// if the element is an input or text, the `value` is read
    /// if the element is a `select`, the `innerText` of the selected `option` is read (TODO: handle multiselect)
    /// for all other elements, the `innerText` is read
    let read (element : ElementHandle) (d : BrowserCtx) =
        match Handle.className element with
        | JsonString "HTMLInputElement" | JsonString "HTMLTextAreaElement" -> 
            Handle.property "value" d element
        | JsonString "HTMLSelectElement" -> 
            let currentValue = Handle.property "value" d element
            let options = Handle.children "option" d element
            let selectedOption =
                options 
                |> Array.find 
                       (fun option -> 
                       Handle.property "value" d option = currentValue)
            Handle.property "innerText" d selectedOption
        | _ -> Handle.property "innerText" d element

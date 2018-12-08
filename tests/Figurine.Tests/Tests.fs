module Tests

open Expecto
open Figurine
open Figurine.Functions
open Newtonsoft.Json.Linq
open PuppeteerSharp

let chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
let googleHomePage = "https://www.google.com/"
let wikipedia = "https://www.wikipedia.org"
let germanWikipediaDefault =
    "https://de.wikipedia.org/wiki/Wikipedia:Hauptseite"

[<Tests>]
let tests =
    testList "samples" 
        [ testCase "Go to google" 
          <| fun () -> 
              use ctx = Browser.createFromExe chromePath
              url googleHomePage ctx
              Expect.equal ctx.CurrentUrl googleHomePage 
                  "should have gone to google"
          
          // pending because the input element on google's search page doesn't show the typed value in the innerText
          testCase "Can manipulate element via selector" 
          <| fun () -> 
              use ctx = Browser.createFromExe chromePath
              url wikipedia ctx
              let element = findSelector "input[name=search]" ctx
              Expect.isSome element "should have found the search input"
              let greeting = "Hello from Figurine!"
              write greeting element.Value ctx
              let written = read element.Value ctx
              Expect.equal written greeting 
                  "should have been able to round-trip the greeting"
          
          testCase "Can get all elements and print them" 
          <| fun () -> 
              use ctx = Browser.createFromExe chromePath
              url googleHomePage ctx
              let elements = findManySelector "*" ctx
              Expect.isGreaterThan elements.Length 0 
                  "should have found some elements on the page"
          testCase "Can use waitFor to wait for page transitions" <| fun () -> 
              use ctx = Browser.createFromExe chromePath
              url wikipedia ctx
              let germanLink = findSelector "a[id=js-link-box-de]" ctx
              async { 
                  do! Async.Sleep 2000
                  click germanLink.Value
              }
              |> Async.Start
              waitFor (fun () -> currentUrl ctx = germanWikipediaDefault) ctx ]

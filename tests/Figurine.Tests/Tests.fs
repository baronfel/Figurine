module Tests


open Expecto
open Figurine
open Figurine.Functions
open PuppeteerSharp

let chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
let googleHomePage = "https://www.google.com/"

[<Tests>]
let tests =
  testList "samples" [
    testCase "Go to google" <| fun () ->

      let ctx = Browser.createFromExe chromePath

      url googleHomePage ctx

      Expect.equal ctx.CurrentUrl googleHomePage "should have gone to google"

    // pending because the input element on google's search page doesn't show the typed value in the innerText
    ptestCase "Can manipulate element via selector" <| fun () ->
        let ctx = Browser.createFromExe chromePath

        url googleHomePage ctx

        let element = findSelector "input[aria-label=Search]" ctx
        Expect.isSome element "should have found the search input"

        let greeting = "Hello from Figurine!"
        write greeting element.Value

        let written = read element.Value ctx

        Expect.equal written greeting "should have been able to round-trip the greeting"

    testCase "Can get all elements and print them" <| fun () ->
        let ctx = Browser.createFromExe chromePath
        url googleHomePage ctx

        let elements = findManySelector "*" ctx

        Expect.isGreaterThan elements.Length 0 "should have found some elements on the page"

  ]

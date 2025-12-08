open App
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Serilog
open StarFederation.Datastar.DependencyInjection

let configureApp (app: WebApplication) =
    app.UseGiraffe(Index.Handler.app)
        
let configureServices (services: IServiceCollection) =
    services
        .AddSerilog()
        .AddDatastar()
        .AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
    Log.Logger <-
        LoggerConfiguration()
            .WriteTo.Console()
            .CreateLogger()
    try
        try
            let builder = WebApplication.CreateBuilder(args)
            configureServices builder.Services
            let app = builder.Build()
            configureApp app
            Log.Information("🚀 Launching app...")
            app.Run("https://0.0.0.0:5000")
            0 // Exit code
        with ex ->
            Log.Fatal(ex, "Application terminated unexpectedly")
            1 // Exit code
    finally
        Log.CloseAndFlush()

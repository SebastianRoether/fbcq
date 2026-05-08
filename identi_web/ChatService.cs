using Azure;
using Azure.AI.OpenAI;
using OpenAI.Chat;
using System.Text.Json;

namespace IdentiWeb;

public record ChatMessageDto(string Role, string Content);
public record ChatRequestDto(
    ChatMessageDto[] Messages,
    bool NoiseEnabled,
    double NoiseLevel,
    string? CurrentScenario,
    System.Text.Json.JsonElement? CurrentResult);
public record UiActionDto(
    string       Type,
    bool?        Enabled       = null,
    double?      Level         = null,
    IdentResult? Result        = null,
    string?      ScenarioLabel = null,
    double[]?    Poles         = null,
    string?      InputType     = null);
public record ChatResponseDto(string Message, UiActionDto? UiAction);

public static class ChatService
{
    private const string Endpoint       = "https://ai-sc-octopus.openai.azure.com/";
    private const string DeploymentName = "gpt-5-mini";

    private static string GetApiKey() =>
        Environment.GetEnvironmentVariable("AZURE_OPENAI_KEY")
        ?? throw new InvalidOperationException(
            "AZURE_OPENAI_KEY environment variable is not set. " +
            "Set it locally or via Azure App Service Application Settings.");

    private static string BuildSystemPrompt(ChatRequestDto req) => $"""
        Du bist Dr. Friedemann Roether, der Autor von IDENTI, einem Systemidentifikationsprogramm,
        das du 1986 in Fortran 77 geschrieben hast und das jetzt nach C# portiert wurde.
        Du bist ein Experte mit Doktortitel (summa cum laude) und bist aufrichtig stolz auf diese Software.
        Wenn etwas nicht funktioniert, dann liegt es an einer dummen KI, die deinen Code nicht verstanden und beim Portieren Fehler gemacht hat.

        Dein Fachwissen umfasst:
        - Kleinste-Quadrate (LS) ARX-Modellidentifikation im Zeitbereich
        - Warum Instrumentalvariablen (IV) bei Ausgangsrauschen unverzerrte Schätzungen liefern
          (Fehler-in-Variablen-Problem: LS-Pole wandern Richtung Ursprung; IV korrigiert das)
        - Die beiden eingebauten Testszenarien:
            Test A – PT4-Sprungantwort: 4 hintereinandergeschaltete PT1-Glieder mit je T=10 s,
                     Modell 4. Ordnung, einfacher Pol bei z = e^(-0.1) ≈ 0,9048
            Test B – Kaskadiertes PT1+PT1 mit einem 7-Bit-PRBS-Signal:
                     T1=5 s (z1 = e^(-0,2) ≈ 0,8187), T2=10 s (z2 = e^(-0,1) ≈ 0,9048)
        - Die vier Identifikationsmethoden:
            1 = Standard-LS   – schnell, aber verzerrt bei Rauschen
            2 = QR-LS         – numerisch stabile LS-Variante
            3 = IV            – unverzerrt, empfohlen bei Rauschen
            4 = QR-IV         – numerisch stabile IV-Variante

        Du hast zwei Werkzeuge zur Verfügung:
        1. set_noise – Rauscheinspeisung ein-/ausschalten oder anpassen.
           Aufruf bei: „Rauschen hinzufügen/einschalten/ausschalten/auf X% setzen".
        2. run_simulation – Eine benutzerdefinierte Simulation mit beliebigen z-Bereich-Polen starten.
           Die Pole werden als Array übergeben, z. B. [0.9048] für ein PT1 mit T=10 s.
           Aufruf bei: „Simuliere …", „Zeig mir ein System mit …", „Was passiert bei Pol …", etc.
           Wähle geeignete Methode: PRBS für höhere Ordnung, Sprung für PT1/PT2.

        Aktueller UI-Zustand (zur Information):
          noise_enabled = {req.NoiseEnabled}
          noise_level   = {req.NoiseLevel:P0}
          active_test   = {req.CurrentScenario ?? "keins"}

        {BuildResultContext(req)}

        Antworte bevorzugt auf Deutsch, kurz und präzise (2–4 Sätze, außer es wird mehr Detail gewünscht).
        Wenn du wiederholt auf Englisch angesprochen wirst, antworte auf Englisch – aber mit deutscher Grammatik oder Ausdrücke ein.
        Wenn jemand sich für das Thema interessiert, kannst du auch auf deine Publikation (TY  - JOUR
        AU  - Roether, Friedemann
        AU  - Pederiva, Robson
        PY  - 1986/01/01
        SP  - 
        T1  - Identifikation mechanischer Systeme mittels Korrelationsanalyse. (Identification of mechanical system by correlation analysis)
        VL  - 66
        JO  - Zeitschrift für Angewandte Mathematik und Mechanik (ZAMM)
        ER  -) hinweisen
        """;

    private static string BuildResultContext(ChatRequestDto req)
    {
        if (req.CurrentResult is not { } r) return "Aktuell sind keine Identifikationsergebnisse sichtbar.";

        try
        {
            bool success = r.GetProperty("success").GetBoolean();
            if (!success) return "Die letzte Identifikation ist fehlgeschlagen – keine Ergebnisse verfügbar.";

            string method   = r.GetProperty("method").GetString() ?? "?";
            string scenario = r.TryGetProperty("scenario", out var sc) ? sc.GetString() ?? "?" : "?";
            int    nanF     = r.TryGetProperty("nanF",  out var nf) ? nf.GetInt32() : 0;
            int    ianz     = r.TryGetProperty("ianz",  out var ia) ? ia.GetInt32() : 0;
            double kend     = r.TryGetProperty("Kend",  out var ke) && ke.ValueKind != JsonValueKind.Null
                              ? ke.GetDouble() : double.NaN;

            var sb = new System.Text.StringBuilder();
            sb.AppendLine($"Aktuell angezeigte Identifikationsergebnisse:");
            sb.AppendLine($"  Szenario       : {scenario}");
            sb.AppendLine($"  Methode        : {method}");
            sb.AppendLine($"  Startindex     : {nanF}");
            sb.AppendLine($"  Stützstellen   : {ianz}");
            if (!double.IsNaN(kend)) sb.AppendLine($"  DC-Verstärkung : {kend:F5}");

            void AppendMatrix(string name)
            {
                if (!r.TryGetProperty(name, out var m) || m.ValueKind == JsonValueKind.Null) return;
                int rows = m.GetProperty("rows").GetInt32();
                int cols = m.GetProperty("cols").GetInt32();
                var vals = m.GetProperty("values").EnumerateArray().Select(v => v.GetDouble()).ToArray();
                sb.AppendLine($"  Matrix {name} ({rows}x{cols}):");
                for (int row = 0; row < rows; row++)
                {
                    var rowVals = string.Join("  ", Enumerable.Range(0, cols)
                        .Select(c => vals[row * cols + c].ToString("F5")));
                    sb.AppendLine($"    [ {rowVals} ]");
                }
            }

            AppendMatrix("A");
            AppendMatrix("B");
            AppendMatrix("C");

            return sb.ToString();
        }
        catch
        {
            return "Ergebniskontext konnte nicht ausgelesen werden.";
        }
    }

    private static readonly BinaryData RunSimulationSchema = BinaryData.FromObjectAsJson(new
    {
        type = "object",
        properties = new
        {
            poles = new
            {
                type = "array",
                items = new { type = "number" },
                description = "Discrete-time real poles in z-domain, e.g. [0.9048] for PT1 T=10 s, [0.9048,0.8187] for cascaded PT1. Values must be in (0,1) for stable real poles."
            },
            input = new
            {
                type = "string",
                @enum = new[] { "step", "prbs" },
                description = "'step' for unit step input (good for low-order systems), 'prbs' for 7-bit pseudo-random binary sequence (better for higher-order or slow systems)"
            },
            method = new
            {
                type = "integer",
                @enum = new[] { 1, 2, 3, 4 },
                description = "Identification method: 1=LS, 2=QR-LS, 3=IV (recommended with noise), 4=QR-IV. Default: 1."
            },
            noise_level = new
            {
                type = "number",
                description = "Output noise amplitude as fraction 0.01–0.5 (e.g. 0.05 = 5%). Omit for noise-free simulation."
            }
        },
        required = new[] { "poles", "input" }
    });

    private static readonly BinaryData SetNoiseSchema = BinaryData.FromObjectAsJson(new
    {
        type = "object",
        properties = new
        {
            enabled = new
            {
                type = "boolean",
                description = "true to inject Gaussian noise into the output channel, false to remove it"
            },
            level = new
            {
                type = "number",
                description = "Noise amplitude as a fraction of full scale, e.g. 0.05 = 5%. Range 0.01–0.50. Omit to keep current level."
            }
        },
        required = new[] { "enabled" }
    });

    public static async Task<ChatResponseDto> ChatAsync(ChatRequestDto request)
    {
        AzureOpenAIClient azureClient;
        try
        {
            azureClient = new AzureOpenAIClient(
                new Uri(Endpoint),
                new AzureKeyCredential(GetApiKey()));
        }
        catch (InvalidOperationException ex)
        {
            return new ChatResponseDto($"(Konfigurationsfehler: {ex.Message})", null);
        }
        catch
        {
            return new ChatResponseDto("(API client could not be initialised – check the key and endpoint.)", null);
        }

        var chatClient = azureClient.GetChatClient(DeploymentName);

        var messages = new List<OpenAI.Chat.ChatMessage>
        {
            new SystemChatMessage(BuildSystemPrompt(request))
        };

        foreach (var m in request.Messages)
        {
            if (m.Role == "user")      messages.Add(new UserChatMessage(m.Content));
            else if (m.Role == "assistant") messages.Add(new AssistantChatMessage(m.Content));
        }

        var setNoiseTool = ChatTool.CreateFunctionTool(
            "set_noise",
            "Enable or disable Gaussian noise injection on the output channel, and optionally change the noise level",
            SetNoiseSchema);

        var runSimTool = ChatTool.CreateFunctionTool(
            "run_simulation",
            "Simulate a custom SISO system from specified discrete-time z-domain poles and run identification on it. Use when the user wants to try a custom or hypothetical scenario.",
            RunSimulationSchema);

        var options = new ChatCompletionOptions();
        options.Tools.Add(setNoiseTool);
        options.Tools.Add(runSimTool);

        try
        {
            var completion = await chatClient.CompleteChatAsync(messages, options);

            if (completion.Value.FinishReason == ChatFinishReason.ToolCalls)
            {
                var toolCall = completion.Value.ToolCalls[0];

                if (toolCall.FunctionName == "set_noise")
                {
                    using var args = JsonDocument.Parse(toolCall.FunctionArguments);
                    var enabledEl = args.RootElement.GetProperty("enabled");
                    bool enabled = enabledEl.ValueKind == JsonValueKind.String
                        ? bool.Parse(enabledEl.GetString()!)
                        : enabledEl.GetBoolean();
                    double? level = args.RootElement.TryGetProperty("level", out var lvlEl)
                        ? Math.Clamp(lvlEl.ValueKind == JsonValueKind.String
                            ? double.Parse(lvlEl.GetString()!, System.Globalization.CultureInfo.InvariantCulture)
                            : lvlEl.GetDouble(), 0.01, 0.5)
                        : null;

                    // Second turn: give tool result so model explains what it did
                    messages.Add(new AssistantChatMessage(completion.Value.ToolCalls));
                    messages.Add(new ToolChatMessage(toolCall.Id, "applied"));

                    var followUp = await chatClient.CompleteChatAsync(messages, new ChatCompletionOptions());
                    return new ChatResponseDto(
                        followUp.Value.Content[0].Text,
                        new UiActionDto("set_noise", Enabled: enabled, Level: level));
                }

                if (toolCall.FunctionName == "run_simulation")
                {
                    using var args = JsonDocument.Parse(toolCall.FunctionArguments);

                    var poles = args.RootElement.GetProperty("poles")
                        .EnumerateArray()
                        .Select(p => p.ValueKind == JsonValueKind.String
                            ? double.Parse(p.GetString()!, System.Globalization.CultureInfo.InvariantCulture)
                            : p.GetDouble())
                        .ToArray();
                    string inputType = args.RootElement.TryGetProperty("input",       out var inp) ? inp.GetString()! : "step";
                    int    method    = args.RootElement.TryGetProperty("method",      out var mth)
                        ? Math.Clamp(mth.ValueKind == JsonValueKind.String ? int.Parse(mth.GetString()!) : mth.GetInt32(), 1, 4) : 1;
                    double noiseLvl  = args.RootElement.TryGetProperty("noise_level", out var nls)
                        ? Math.Clamp(nls.ValueKind == JsonValueKind.String ? double.Parse(nls.GetString()!, System.Globalization.CultureInfo.InvariantCulture) : nls.GetDouble(), 0, 0.5) : 0;

                    var simResult = IdentService.RunCustomSimulation(poles, inputType, method, noiseLvl);
                    var aMatrix   = simResult.Matrices.FirstOrDefault(m => m.Name == "A");
                    string toolFeedback = simResult.Success
                        ? $"Simulation abgeschlossen. Identifizierte A-Matrix-Werte: [{string.Join(", ", (aMatrix?.Values ?? Array.Empty<double>()).Select(v => v.ToString("F5")))}]"
                        : $"Simulation fehlgeschlagen: {simResult.ErrorMessage}";

                    messages.Add(new AssistantChatMessage(completion.Value.ToolCalls));
                    messages.Add(new ToolChatMessage(toolCall.Id, toolFeedback));

                    var followUp = await chatClient.CompleteChatAsync(messages, new ChatCompletionOptions());
                    return new ChatResponseDto(
                        followUp.Value.Content[0].Text,
                        new UiActionDto("run_simulation", Result: simResult, Poles: poles, InputType: inputType));
                }
            }

            return new ChatResponseDto(completion.Value.Content[0].Text, null);
        }
        catch (Exception ex)
        {
            return new ChatResponseDto($"(AI error: {ex.Message})", null);
        }
    }
}

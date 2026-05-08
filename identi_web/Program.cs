using IdentiWeb;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

app.UseDefaultFiles();
app.UseStaticFiles();

// ── API endpoints ─────────────────────────────────────────────

app.MapGet("/api/auto-a", (double noise = 0, int method = 1) =>
    Results.Json(IdentService.RunAutoTestA(Math.Clamp(noise, 0, 1), Math.Clamp(method, 1, 4))));

app.MapGet("/api/auto-b", (double noise = 0, int method = 1) =>
    Results.Json(IdentService.RunAutoTestB(Math.Clamp(noise, 0, 1), Math.Clamp(method, 1, 4))));

// Custom simulation: Friedi-generated or re-run of stored custom scenario
app.MapPost("/api/sim", (SimRequest req) =>
    Results.Json(IdentService.RunCustomSimulation(
        req.Poles,
        req.InputType ?? "step",
        Math.Clamp(req.Method, 1, 4),
        Math.Clamp(req.Noise, 0, 0.5))));

app.MapPost("/api/chat", async (ChatRequestDto req) =>
    Results.Json(await ChatService.ChatAsync(req)));

app.Run();

public record SimRequest(double[] Poles, string? InputType, int Method = 1, double Noise = 0);


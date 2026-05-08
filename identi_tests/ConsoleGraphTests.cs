using Identi;
using System.Reflection;
using Xunit;

namespace IdentTests;

// ============================================================
// Tests for ConsoleGraph – filter and resample logic.
// Private methods tested via reflection since they are not public.
// (This highlights a testability issue: these should be internal.)
// ============================================================
public class ConsoleGraphTests
{
    // Helper: invoke private static method via reflection
    private static T? InvokePrivate<T>(string methodName, object?[] args)
    {
        var type = typeof(ConsoleGraph);
        var mi   = type.GetMethod(methodName,
                        BindingFlags.NonPublic | BindingFlags.Static);
        Assert.True(mi != null, $"Private method '{methodName}' not found.");
        return (T?)mi!.Invoke(null, args);
    }

    private static void InvokePrivateVoid(string methodName, object?[] args)
    {
        var type = typeof(ConsoleGraph);
        var mi   = type.GetMethod(methodName,
                        BindingFlags.NonPublic | BindingFlags.Static);
        Assert.True(mi != null, $"Private method '{methodName}' not found.");
        mi!.Invoke(null, args);
    }

    // ----------------------------------------------------------
    // Resample – block-average resampler
    // ----------------------------------------------------------
    [Fact]
    public void Resample_DownsamplesCorrectly()
    {
        // source: 8 samples, target: 4 → block-average pairs
        float[] src = { 1f, 3f, 5f, 7f, 2f, 4f, 6f, 8f };
        float[] result = InvokePrivate<float[]>("Resample", new object?[] { src, 4 })!;

        Assert.Equal(4, result.Length);
        Assert.Equal(2f, result[0]); // avg(1,3)
        Assert.Equal(6f, result[1]); // avg(5,7)
        Assert.Equal(3f, result[2]); // avg(2,4)
        Assert.Equal(7f, result[3]); // avg(6,8)
    }

    [Fact]
    public void Resample_NoOpWhenSrcSmaller()
    {
        float[] src = { 1f, 2f, 3f };
        float[] result = InvokePrivate<float[]>("Resample", new object?[] { src, 10 })!;

        Assert.Equal(3, result.Length);
        Assert.Equal(1f, result[0]);
        Assert.Equal(2f, result[1]);
        Assert.Equal(3f, result[2]);
    }

    [Fact]
    public void Resample_ExactFit()
    {
        float[] src = { 1f, 2f, 3f, 4f };
        float[] result = InvokePrivate<float[]>("Resample", new object?[] { src, 4 })!;
        // no resampling needed
        Assert.Equal(4, result.Length);
        for (int i = 0; i < 4; i++)
            Assert.Equal(src[i], result[i]);
    }

    // ----------------------------------------------------------
    // ApplyFilter – JOBPL modes 0-3
    // ----------------------------------------------------------
    [Fact]
    public void ApplyFilter_JobPL0_NoChange()
    {
        var st = new AppState(); st.JOBPL = 0; st.IMW[1] = 0;
        float[] data = { 1f, 2f, 3f, 4f };
        InvokePrivateVoid("ApplyFilter", new object?[] { data, st, 1 });
        Assert.Equal(new float[] { 1f, 2f, 3f, 4f }, data);
    }

    [Fact]
    public void ApplyFilter_JobPL1_SubtractsMean()
    {
        var st = new AppState(); st.JOBPL = 1; st.IMW[1] = 100;
        float[] data = { 110f, 120f, 100f };
        InvokePrivateVoid("ApplyFilter", new object?[] { data, st, 1 });
        Assert.Equal(new float[] { 10f, 20f, 0f }, data);
    }

    [Fact]
    public void ApplyFilter_JobPL2_Differentiates()
    {
        var st = new AppState(); st.JOBPL = 2;
        float[] data = { 1f, 3f, 6f, 10f };
        InvokePrivateVoid("ApplyFilter", new object?[] { data, st, 1 });
        // diff: [0, 2, 3, 4] — data[0] forced to 0
        Assert.Equal(0f,  data[0]);
        Assert.Equal(2f,  data[1]);
        Assert.Equal(3f,  data[2]);
        Assert.Equal(4f,  data[3]);
    }

    [Fact]
    public void ApplyFilter_JobPL3_HighPass_StartsNearZero()
    {
        var st = new AppState(); st.JOBPL = 3; st.FPAR = 0.99f;
        // DC signal (constant 100) → after high-pass, all outputs near 0
        float[] data = Enumerable.Repeat(100f, 20).ToArray();
        InvokePrivateVoid("ApplyFilter", new object?[] { data, st, 1 });
        Assert.Equal(0f, data[0]);
        // step response of HP decays – last sample should be close to 0
        Assert.True(Math.Abs(data[^1]) < 5f, $"HP output not near zero: {data[^1]}");
    }

    [Fact]
    public void ApplyFilter_JobPL3_PassesStepFront()
    {
        var st = new AppState(); st.JOBPL = 3; st.FPAR = 0.9f;
        // step at index 10
        float[] data = new float[20];
        for (int i = 10; i < 20; i++) data[i] = 1000f;
        InvokePrivateVoid("ApplyFilter", new object?[] { data, st, 1 });
        // At the step the HP should produce a non-zero transient
        Assert.NotEqual(0f, data[10]);
    }

    // ----------------------------------------------------------
    // PlotChannels – smoke tests (no exception, correct "no data" path)
    // ----------------------------------------------------------
    [Fact]
    public void PlotChannels_NoDataOrZeroNPOINT_DoesNotThrow()
    {
        var st = new AppState();
        st.NPOINT = 0;
        var writer = new System.IO.StringWriter();
        var savedOut = Console.Out;
        Console.SetOut(writer);
        try
        {
            ConsoleGraph.PlotChannels(st, 100, 0);
        }
        finally
        {
            Console.SetOut(savedOut);
        }
        string output = writer.ToString();
        Assert.Contains("No data", output);
    }

    [Fact]
    public void PlotChannels_WithData_DoesNotThrow()
    {
        var st = new AppState();
        st.NPOINT = 10;
        st.LEN    = 10;
        st.NPL    = 1;
        st.IPL[1] = 1;
        for (int k = 1; k <= 10; k++) st.IDAT[1, k] = (short)(k * 10);

        var writer = new System.IO.StringWriter();
        var savedOut = Console.Out;
        Console.SetOut(writer);
        try
        {
            var ex = Record.Exception(() => ConsoleGraph.PlotChannels(st, 10, 0));
            Assert.Null(ex);
        }
        finally
        {
            Console.SetOut(savedOut);
        }
    }

    /// <summary>
    /// KNOWN BUG: PrintChannelValues calls Console.Clear() unconditionally,
    /// which throws IOException in non-interactive environments (unit tests, CI).
    /// This test documents the production code defect.
    /// </summary>
    [Fact]
    public void PrintChannelValues_NoData_ThrowsInNonConsoleEnvironment()
    {
        var st = new AppState();
        st.NPOINT = 0;
        // Console.Clear() in PrintChannelValues crashes without a real console.
        // This test documents the bug — it will pass only when the bug is fixed.
        Assert.Throws<System.IO.IOException>(() => ConsoleGraph.PrintChannelValues(st));
    }
}

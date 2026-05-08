using Identi;

namespace IdentiWeb;

// ============================================================
// IdentService – wraps the Identi core library for the web UI.
//
// Provides three operations:
//   RunAutoTestA  – PT4 step response (menu "a")
//   RunAutoTestB  – 2nd-order PRBS     (menu "b")
//   RunCustom     – arbitrary config + LS identification
//
// All results are returned as plain C# records; no Console I/O.
// ============================================================

public record ChannelSeries(string Label, string Unit, double[] Samples);

public record ModelMatrixResult(string Name, int Rows, int Cols, double[] Values);

public record IdentResult(
    bool   Success,
    string ErrorMessage,
    string Method,
    int    NanF,
    int    Ianz,
    int[]  InputChannels,
    int[]  OutputChannels,
    int[]  StructureIndices,
    ModelMatrixResult[] Matrices,
    ChannelSeries[]     Channels,
    ChannelSeries?      Prediction);  // model-simulated output, null if failed

public static class IdentService
{
    private const int LTH   = 30;
    private const int LTH2  = 61;
    private const int LTHLM = 38;
    private const int LM    = 8;
    private const int LU    = 10;

    // ── Auto test A: PT4 step response ─────────────────────────
    public static IdentResult RunAutoTestA(double noiseLevel = 0, int method = 1)
    {
        const int    len    = 400;
        const int    stepAt = 50;
        const double a      = 0.904837418036;
        const double b      = 1.0 - a;

        var st = new AppState();
        st.NADU = 1; st.NDAU = 1; st.LEN = len;
        st.NM = 1; st.NU = 1; st.NYI[1] = 4; st.N = 4;
        st.IAD[1] = 1; st.IDA[1] = st.NADMAX + 1;
        st.NANF = 5; st.IANZ = len - 5; st.IGLW = 0;

        int chIn = st.NADMAX + 1, chOut = 1;
        st.PMIN[chOut] = 0f; st.PMAX[chOut] = 1f;
        st.PMIN[chIn]  = 0f; st.PMAX[chIn]  = 1f;
        st.TEXT[chOut] = "Output (PT4)"; st.PBEZ[chOut] = "[-]";
        st.TEXT[chIn]  = "Input (step)"; st.PBEZ[chIn]  = "[-]";

        const short rawOne = 4096;
        for (int k = 1; k <= len; k++)
            st.IDAT[chIn, k] = k >= stepAt ? rawOne : (short)0;

        double x1=0, x2=0, x3=0, x4=0, uPrev=0;
        for (int k = 1; k <= len; k++)
        {
            double uCurr = st.IDAT[chIn, k];
            double nx1 = a*x1+b*uPrev, nx2 = a*x2+b*x1,
                   nx3 = a*x3+b*x2,    nx4 = a*x4+b*x3;
            x1=nx1; x2=nx2; x3=nx3; x4=nx4;
            st.IDAT[chOut, k] = (short)Math.Clamp((int)Math.Round(x4), -32000, 32000);
            uPrev = uCurr;
        }
        st.NPOINT = len; st.IZ = len;
        if (noiseLevel > 0) AddNoise(st.IDAT, chOut, len, noiseLevel * 4096);

        string labelA = noiseLevel > 0 ? "Auto Test A – PT4 Step Response (+ noise)" : "Auto Test A – PT4 Step Response";
        return RunIdentification(st, len, chIn, chOut, labelA, method);
    }

    // ── Auto test B: 2nd-order cascade PRBS ────────────────────
    public static IdentResult RunAutoTestB(double noiseLevel = 0, int method = 1)
    {
        const double z1 = 0.818730753078, z2 = 0.904837418036;
        const int    len = 400;
        const short  hi = 2048, lo = -2048;

        var st = new AppState();
        st.NADU = 1; st.NDAU = 1; st.LEN = len;
        st.NM = 1; st.NU = 1; st.NYI[1] = 2; st.N = 2;
        st.IAD[1] = 1; st.IDA[1] = st.NADMAX + 1;
        st.NANF = 3; st.IANZ = len - 3; st.IGLW = 0;

        int chIn = st.NADMAX + 1, chOut = 1;
        st.PMIN[chOut] = -15f; st.PMAX[chOut] = 15f;
        st.PMIN[chIn]  = -1f;  st.PMAX[chIn]  = 1f;
        st.TEXT[chOut] = "Output (2×PT1)"; st.PBEZ[chOut] = "[V]";
        st.TEXT[chIn]  = "Input (7-bit PRBS)"; st.PBEZ[chIn] = "[V]";

        int[] reg = new int[8];
        for (int i = 1; i <= 7; i++) reg[i] = 1;
        for (int k = 1; k <= len; k++)
        {
            st.IDAT[chIn, k] = reg[7] == 1 ? hi : lo;
            int fb = reg[7] ^ reg[6];
            for (int r = 7; r > 1; r--) reg[r] = reg[r-1];
            reg[1] = fb;
        }

        double xPrev=0, yPrev=0, uPrev=0;
        for (int k = 1; k <= len; k++)
        {
            double xCurr = z1*xPrev + (1-z1)*uPrev;
            double yCurr = z2*yPrev + (1-z2)*xPrev;
            st.IDAT[chOut, k] = (short)Math.Clamp((long)Math.Round(yCurr), -32000, 32000);
            xPrev = xCurr; yPrev = yCurr; uPrev = st.IDAT[chIn, k];
        }
        st.NPOINT = len; st.IZ = len;
        if (noiseLevel > 0) AddNoise(st.IDAT, chOut, len, noiseLevel * 4096);

        string labelB = noiseLevel > 0 ? "Auto Test B – Cascaded PT1 PRBS (+ noise)" : "Auto Test B – Cascaded PT1 PRBS";
        return RunIdentification(st, len, chIn, chOut, labelB, method);
    }

    // ── Custom: caller provides pre-configured AppState + data ─
    public static IdentResult RunCustom(
        short[,] idat, int len, int chIn, int chOut,
        int order, int method = 1)
    {
        var st = new AppState();
        st.NADU = 1; st.NDAU = 1; st.LEN = len;
        st.NM = 1; st.NU = 1; st.NYI[1] = order; st.N = order;
        st.IAD[1] = chOut; st.IDA[1] = chIn;
        st.NANF = order + 1; st.IANZ = len - order - 1; st.IGLW = 0;
        for (int k = 1; k <= len; k++)
        {
            st.IDAT[chIn,  k] = idat[chIn,  k];
            st.IDAT[chOut, k] = idat[chOut, k];
        }
        st.NPOINT = len; st.IZ = len;
        return RunIdentification(st, len, chIn, chOut, $"Custom LS (order {order})", method);
    }

    // ── Custom simulation: build system from z-domain poles ─────
    /// <summary>
    /// Simulates a SISO system as a cascade of PT1 stages defined by
    /// the supplied z-domain poles, then runs identification on the result.
    /// </summary>
    public static IdentResult RunCustomSimulation(
        double[] poles, string inputType, int method = 1, double noiseLevel = 0)
    {
        // Clamp poles to stable real range; limit to max model order
        poles = poles.Select(p => Math.Clamp(p, 0.01, 0.9999)).Take(LTH).ToArray();
        if (poles.Length == 0) poles = new[] { 0.9 };

        int   order  = poles.Length;
        const int len = 400;
        bool  isPrbs = string.Equals(inputType, "prbs", StringComparison.OrdinalIgnoreCase);

        var st = new AppState();
        st.NADU = 1; st.NDAU = 1; st.LEN = len;
        st.NM = 1; st.NU = 1; st.NYI[1] = order; st.N = order;
        st.IAD[1] = 1; st.IDA[1] = st.NADMAX + 1;
        st.NANF = order + 1; st.IANZ = len - order - 1; st.IGLW = 0;

        int chIn = st.NADMAX + 1, chOut = 1;
        st.PMIN[chOut] = 0f; st.PMAX[chOut] = 1f;
        st.PMIN[chIn]  = 0f; st.PMAX[chIn]  = 1f;

        string polesStr = string.Join(", ", poles.Select(p => p.ToString("F4")));
        st.TEXT[chOut] = $"Output (ord. {order})";      st.PBEZ[chOut] = "[-]";
        st.TEXT[chIn]  = isPrbs ? "Input (7-bit PRBS)" : "Input (step)";
        st.PBEZ[chIn]  = "[-]";

        // ── Generate input signal ──────────────────────────────
        if (!isPrbs)
        {
            const int   stepAt = 50;
            const short rawOne = 4096;
            for (int k = 1; k <= len; k++)
                st.IDAT[chIn, k] = k >= stepAt ? rawOne : (short)0;
        }
        else
        {
            int[] reg = new int[8];
            for (int i = 1; i <= 7; i++) reg[i] = 1;
            for (int k = 1; k <= len; k++)
            {
                st.IDAT[chIn, k] = reg[7] == 1 ? (short)2048 : (short)-2048;
                int fb = reg[7] ^ reg[6];
                for (int r = 7; r > 1; r--) reg[r] = reg[r - 1];
                reg[1] = fb;
            }
        }

        // ── Simulate cascade of PT1 stages ─────────────────────
        // states[i] are in the same raw-count units as IDAT (no extra scaling)
        var    states = new double[order + 1];
        double uPrev  = 0;
        for (int k = 1; k <= len; k++)
        {
            double u  = st.IDAT[chIn, k];
            var    ns = new double[order + 1];
            ns[1] = poles[0] * states[1] + (1 - poles[0]) * uPrev;
            for (int i = 2; i <= order; i++)
                ns[i] = poles[i - 1] * states[i] + (1 - poles[i - 1]) * states[i - 1];
            Array.Copy(ns, 1, states, 1, order);
            st.IDAT[chOut, k] = (short)Math.Clamp((long)Math.Round(states[order]), -32000, 32000);
            uPrev = u;
        }

        st.NPOINT = len; st.IZ = len;
        if (noiseLevel > 0) AddNoise(st.IDAT, chOut, len, noiseLevel * 4096);

        string label = $"Custom {order}. Ordnung — Pole: [{polesStr}] — {(isPrbs ? "PRBS" : "Sprung")}";
        return RunIdentification(st, len, chIn, chOut, label, method);
    }

    // ── Shared identification + result assembly ─────────────────
    private static IdentResult RunIdentification(
        AppState st, int len, int chIn, int chOut,
        string label, int method = 1)
    {
        var dmat  = new float[LTH+1, LTHLM+1];
        var rh    = new float[LTH+1, LTH+1];
        var bWork = new float[LTH+1, LU+1];
        var ipos0 = new int  [LTH+1, LM+1];
        var wksp  = new float[LTH+1, LTH2+1];

        int job = method;
        // Suppress the Fortran-era debug Console.Write calls inside Xident/Mwrite
        // (these are intentional in the console app but unwanted in the web service).
        var savedOut = Console.Out;
        Console.SetOut(TextWriter.Null);
        try
        {
            Identification.Xident(
                st.W, st.IDAT, st.IDA, st.IAD,
                st.LEN, st.NANF, st.IANZ, st.NPOINT,
                st.NYI, st.NU, st.NM,
                dmat, rh, bWork, ipos0, st.IGLW, wksp,
                ref job);
        }
        finally
        {
            Console.SetOut(savedOut);
        }

        // Build time series (normalise raw counts to physical values)
        var channels = new List<ChannelSeries>();
        foreach (int ch in new[]{ chOut, chIn })
        {
            float pmin = st.PMIN[ch], pmax = st.PMAX[ch];
            double[] samples = new double[len];
            for (int k = 1; k <= len; k++)
                samples[k-1] = pmin + (st.IDAT[ch, k] / 4096.0) * (pmax - pmin);
            channels.Add(new ChannelSeries(st.TEXT[ch].Trim(), st.PBEZ[ch].Trim(), samples));
        }

        if (job != 0)
        {
            string msg = job == 1 ? "Singular data matrix" : "Unstable auxiliary model";
            return new IdentResult(false, msg, label,
                st.NANF, st.IANZ, GetChannels(st.IDA, st.NU),
                GetChannels(st.IAD, st.NM), GetIndices(st.NYI, st.NM),
                [], channels.ToArray(), null);
        }

        st.JOBID = method;

        // Extract A, B, C, Kend matrices
        int n = st.N, nu = st.NU, nm = st.NM;
        int npw = n * (nm + nu);
        var A = new float[n+1, n+1];
        var B = new float[n+1, nu+1];
        var C = new float[nm+1, n+1];
        Identification.Xmdecd(A, n, B, nu, C, nm, st.W, npw, st.NYI, out int ierr);

        var matrices = new List<ModelMatrixResult>();
        if (ierr == 0)
        {
            matrices.Add(ExtractMatrix(A, n, n, "A"));
            matrices.Add(ExtractMatrix(B, n, nu, "B"));
            matrices.Add(ExtractMatrix(C, nm, n, "C"));

            // Steady-state gain Kend = C*(I-A)^-1*B
            var ia = new float[n+1, n+1];
            for (int i = 1; i <= n; i++)
                for (int j = 1; j <= n; j++)
                    ia[i,j] = (i==j ? 1f : 0f) - A[i,j];
            var tmp  = new float[n+1, nu+1];
            var piv2 = new int[n+1];
            MatrixLib.Mimprd(tmp, ia, n, B, nu, piv2, 1, out int ierr2);
            if (ierr2 == 0)
            {
                var Kend = new float[nm+1, nu+1];
                for (int i = 1; i <= nm; i++)
                    for (int j = 1; j <= nu; j++)
                    {
                        float s = 0f;
                        for (int k = 1; k <= n; k++) s += C[i,k]*tmp[k,j];
                        Kend[i,j] = s;
                    }
                matrices.Add(ExtractMatrix(Kend, nm, nu, "Kend"));
            }
        }

        // Simulate model prediction forward in time (replaces console menu 8-4)
        ChannelSeries? prediction = null;
        if (ierr == 0)
            prediction = SimulatePrediction(st, len, chOut);

        return new IdentResult(true, "", label,
            st.NANF, st.IANZ,
            GetChannels(st.IDA, st.NU),
            GetChannels(st.IAD, st.NM),
            GetIndices(st.NYI, st.NM),
            matrices.ToArray(), channels.ToArray(), prediction);
    }

    // ── One-step-ahead backward prediction ─────────────────────────────────────
    // The A matrix has eigenvalues |λ|≈0.867 (stable), so ANY free-run decays
    // to zero regardless of direction.  The meaningful overlay is a
    // ONE-STEP-AHEAD predictor: at each position k, re-initialise the state
    // from actual measurements at k+1..k+n, then run n+1 Xmodel steps.
    // After n+1 steps, ypred[1] = W[1]·y[k+n]+…+W[n]·y[k+1]  — exactly what
    // the backward LS minimised, so it tracks PRBS data correctly.
    private static ChannelSeries? SimulatePrediction(AppState st, int len, int chOut)
    {
        int n = st.N, nm = st.NM, nu = st.NU;
        int npw = n * (nm + nu);
        if (n <= 0 || npw <= 0) return null;

        var x     = new float[n  + 1];
        var x1    = new float[n  + 1];
        var u     = new float[nu + 1];
        var ypred = new float[nm + 1];

        double[] prediction = new double[len];
        float pmin2   = st.PMIN[chOut], pmax2 = st.PMAX[chOut];
        float adRange = st.IADMAX - st.IADMIN;

        // For each position k we need k+n ≤ len (to have actual future samples).
        for (int k = 1; k <= len - n; k++)
        {
            // Initialise state using actual data at positions k+n … k+1.
            int npsInit = k + n;
            Identification.Xmodin(x, n, st.IDAT, st.LEN,
                npsInit, st.IDA, st.IAD,
                nm, st.NYI, st.W, npw, u, nu, st.IGLW);

            // Run n+1 Xmodel steps (backward in time).
            // Steps 0..n-1 shift actual values through the trivial states.
            // Step n yields ypred[1] = backward-AR prediction for position k.
            bool ok = true;
            for (int step = 0; step <= n; step++)
            {
                int uPos = npsInit - step;
                if (uPos < 1)   uPos = 1;
                if (uPos > len) uPos = len;
                for (int j = 1; j <= nu; j++)
                    u[j] = st.IDAT[st.IDA[j], uPos];

                Identification.Xmodel(ypred, nm, x, n,
                    st.W, u, nu, st.NYI, x1, 1e25f, st.IGLW, out int ierr2);
                if (ierr2 != 0) { ok = false; break; }
            }
            if (!ok) break;

            prediction[k - 1] = pmin2 + (ypred[1] / adRange) * (pmax2 - pmin2);
        }

        string label = (st.TEXT[chOut] ?? "").Trim();
        string unit  = (st.PBEZ[chOut] ?? "").Trim();
        return new ChannelSeries($"{label} (model)", unit, prediction);
    }

    // ── Gaussian noise injection ──────────────────────────────────────────────
    private static void AddNoise(short[,] idat, int ch, int len, double sigma)
    {
        var rng = new Random();
        for (int k = 1; k <= len; k++)
        {
            // Box-Muller transform
            double u1 = 1.0 - rng.NextDouble();
            double u2 = 1.0 - rng.NextDouble();
            double gauss = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2);
            idat[ch, k] = (short)Math.Clamp(
                (long)Math.Round(idat[ch, k] + gauss * sigma),
                short.MinValue, short.MaxValue);
        }
    }

    private static ModelMatrixResult ExtractMatrix(float[,] m, int rows, int cols, string name)
    {
        var vals = new double[rows * cols];
        int idx = 0;
        for (int r = 1; r <= rows; r++)
            for (int c = 1; c <= cols; c++)
                vals[idx++] = m[r, c];
        return new ModelMatrixResult(name, rows, cols, vals);
    }

    private static int[] GetChannels(int[] arr, int count)
    {
        var r = new int[count];
        for (int i = 0; i < count; i++) r[i] = arr[i+1];
        return r;
    }

    private static int[] GetIndices(int[] nyi, int nm)
    {
        var r = new int[nm];
        for (int i = 0; i < nm; i++) r[i] = nyi[i+1];
        return r;
    }
}

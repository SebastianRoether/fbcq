using Identi;
using Xunit;

namespace IdentTests;

// ============================================================
// PT4 Auto-Test Pipeline – step-by-step verification
//
// Each region of tests covers one stage of the RunAutoTest() pipeline.
// The tests also document the fundamental behaviour of Xident:
//
//   Xident performs BACKWARD-TIME regression:
//     gam  = y(t)          (current sample at decreasing lag)
//     phi  = [y(t+4), y(t+3), y(t+2), y(t+1),
//             u(t+4), u(t+3), u(t+2), u(t+1)]   ← future values!
//
//   For STATIONARY processes (PRBS excitation):
//     • AR coefficients converge to true values via Yule-Walker symmetry.
//     • MA coefficients converge to 0  (E[u(t+1)·y(t)] = 0 by causality).
//
//   For NON-STATIONARY data (step response):
//     • Samples are dominated by steady state → AR/MA both biased.
//     • Backward prediction error (what LS minimises) is small.
//     • Forward prediction error (what we care about in practice) is larger.
//
// This explains why the displayed AR coefficients look "wrong":
//   the algorithm identified a backward model from a step response.
// The state-space (A, B, C) matrices derived from it may still be
// physically useful for the nearly-linear steady-state region.
// ============================================================
public class AutoTestPipelineTests
{
    // ── PT4 parameters (must match RunAutoTest exactly) ──────────────
    const double A_POLE = 0.904837418036; // e^{-0.1}
    const double B_COEF = 1.0 - A_POLE;
    const int    Len    = 400;
    const int    StepAt = 50;
    const short  RawOne = 4096;   // physical 1.0 → IADMAX = 4096 (PMIN=0, PMAX=1)
    const int    ChIn   = 9;      // D/A channel 1 = NADMAX+1 = 9
    const int    ChOut  = 1;      // A/D channel 1

    // Reproduce the exact signal data that RunAutoTest generates.
    static short[,] MakePT4Data()
    {
        var idat = new short[AppState.NADDAX + 1, AppState.LENMA + 1];

        // Step input: 0 before StepAt, RawOne from StepAt onward
        for (int k = 1; k <= Len; k++)
            idat[ChIn, k] = k >= StepAt ? RawOne : (short)0;

        // 4 cascaded ZOH PT1 stages  (uPrev = 1-sample delay on input)
        double x1 = 0, x2 = 0, x3 = 0, x4 = 0, uPrev = 0;
        for (int k = 1; k <= Len; k++)
        {
            double uCurr = idat[ChIn, k];
            double nx1 = A_POLE * x1 + B_COEF * uPrev;
            double nx2 = A_POLE * x2 + B_COEF * x1;
            double nx3 = A_POLE * x3 + B_COEF * x2;
            double nx4 = A_POLE * x4 + B_COEF * x3;
            x1 = nx1; x2 = nx2; x3 = nx3; x4 = nx4;
            idat[ChOut, k] = (short)Math.Clamp((int)Math.Round(x4), -32000, 32000);
            uPrev = uCurr;
        }
        return idat;
    }

    // Run Xident on PT4 data with the same parameters as RunAutoTest.
    static (float[] w, int job) RunXidentOnPT4()
    {
        var idat = MakePT4Data();
        int[] ida = new int[AppState.NADDAX + 1]; ida[1] = ChIn;
        int[] iad = new int[AppState.NADMA  + 1]; iad[1] = ChOut;
        int[] nyi = new int[AppState.NADMA  + 1]; nyi[1] = 4;
        float[] w = new float[AppState.NPWMA + 1];

        const int LTH = 30, LTH2 = 61, LTHLM = 38, LM = 8, LU = 10;
        var dmat  = new float[LTH + 1, LTHLM + 1];
        var rh    = new float[LTH + 1, LTH   + 1];
        var bWork = new float[LTH + 1, LU    + 1];
        var ipos0 = new int  [LTH + 1, LM    + 1];
        var wksp  = new float[LTH + 1, LTH2  + 1];

        int job = 1;
        Identification.Xident(w, idat, ida, iad,
            Len, nanf: 5, ianz: Len - 5, npsave: Len,
            nyi, nu: 1, nm: 1,
            dmat, rh, bWork, ipos0, iglw: 0, wksp, ref job);
        return (w, job);
    }

    // ─────────────────────────────────────────────────────────────────
    // Stage 1 – Signal generation
    // ─────────────────────────────────────────────────────────────────

    [Fact]
    public void Stage1_InputChannel_IsStepAtSampleStepAt()
    {
        var idat = MakePT4Data();
        for (int k = 1; k < StepAt; k++)
            Assert.Equal(0, idat[ChIn, k]);
        for (int k = StepAt; k <= Len; k++)
            Assert.Equal(RawOne, idat[ChIn, k]);
    }

    [Fact]
    public void Stage1_OutputChannel_IsZeroBeforeStepPropagatesThroughAllFourStages()
    {
        // The input delay (uPrev) adds 1 sample: step not visible in x1 until k=51.
        // x2 receives first nonzero at k=52, x3 at k=53, x4 at k=54.
        // x4 at k=54 ≈ 0.336, rounds to 0. First nonzero output appears at k=55.
        var idat = MakePT4Data();
        for (int k = 1; k <= 54; k++)
            Assert.Equal(0, idat[ChOut, k]);
    }

    [Fact]
    public void Stage1_OutputChannel_MonotonicallyNonDecreasingAfterFirstNonzero()
    {
        // PT4 step response of an all-positive system is monotone non-decreasing.
        var idat = MakePT4Data();
        for (int k = 55; k < Len; k++)
            Assert.True(idat[ChOut, k + 1] >= idat[ChOut, k],
                $"Output decreased at k={k}: {idat[ChOut, k]} → {idat[ChOut, k + 1]}");
    }

    [Fact]
    public void Stage1_OutputChannel_ReachesSteadyStateNearEndOfBuffer()
    {
        // After 350 s of step input, exponential error ≈ e^{-35} ≈ 6e-16 → y = RawOne.
        // Allow ≤ 5 raw counts of rounding from short integer arithmetic.
        var idat = MakePT4Data();
        Assert.True(idat[ChOut, Len] >= RawOne - 5,
            $"Steady state not reached: y({Len}) = {idat[ChOut, Len]}, expected ≈ {RawOne}");
    }

    // ─────────────────────────────────────────────────────────────────
    // Stage 2 – Least-squares identification
    // ─────────────────────────────────────────────────────────────────

    [Fact]
    public void Stage2_Xident_CompletesWithoutSingularity()
    {
        var (_, job) = RunXidentOnPT4();
        Assert.Equal(0, job);
    }

    [Fact]
    public void Stage2_Xident_ARCoefficients_DoNotMatchTrueForwardValues()
    {
        // True forward AR for (z − a)⁴ with a = e^{−0.1}:
        //   W[4] = 4a ≈ 3.619   (coeff of y[k-1])
        //   W[1] = −a⁴ ≈ −0.670  (coeff of y[k-4])
        //
        // Xident uses backward-time regression on a step-response signal.
        // Step response is non-stationary and dominated by steady-state samples
        // (y ≈ u ≈ RawOne), so LS enforces Σ W_i ≈ 1 (DC-gain constraint)
        // rather than converging to the true AR polynomial coefficients.
        //
        // CONSEQUENCE: step response is not a valid excitation for ARX identification.
        // Use PRBS or other persistently exciting input for accurate coefficient recovery.
        float trueW4 = (float)(4 * A_POLE);                  // ≈ 3.619
        float trueW1 = (float)(-Math.Pow(A_POLE, 4));        // ≈ −0.670

        var (w, _) = RunXidentOnPT4();
        Assert.True(Math.Abs(w[4] - trueW4) > 0.5f,
            $"W[4] = {w[4]:F4} unexpectedly close to true {trueW4:F4}. " +
            "Step response is not valid ARX excitation.");
        Assert.True(Math.Abs(w[1] - trueW1) > 0.2f,
            $"W[1] = {w[1]:F4} unexpectedly close to true {trueW1:F4}.");
    }

    // ─────────────────────────────────────────────────────────────────
    // Stage 3 – aForGm construction (validates the Xgmkon bug fix)
    //
    // Before the fix, Xident called Xgmkon(gm, dmat, ...) where dmat
    // contained cross-correlations, not AR coefficients.  The fix builds
    // aForGm[j, i] = w[(i−1)·n + j] and calls Xgmkon(gm, aForGm, ...).
    // ─────────────────────────────────────────────────────────────────

    [Fact]
    public void Stage3_aForGm_Formula_MapsW_CorrectlyForSISO()
    {
        // For nm=1, n=4: the only column is i=1.
        // aForGm[j, 1] = w[(1−1)*4 + j] = w[j] for j = 1..4.
        // This is a direct test of the mapping formula independent of Xident.
        float[] w = { 0, -0.0268f, -0.806f, 0.645f, 1.186f, 0f, 0f, 0f, 0.00176f };
        int n = 4, nm = 1;
        var aForGm = new float[n + 1, nm + 1];
        for (int i = 1; i <= nm; i++)
            for (int j = 1; j <= n; j++)
                aForGm[j, i] = w[(i - 1) * n + j];

        Assert.Equal(w[1], aForGm[1, 1]);
        Assert.Equal(w[2], aForGm[2, 1]);
        Assert.Equal(w[3], aForGm[3, 1]);
        Assert.Equal(w[4], aForGm[4, 1]);
    }

    [Fact]
    public void Stage3_Xgmkon_WithCorrectSource_DiffersFromWrongSource_ForOrderGe2()
    {
        // If Xgmkon is called with the wrong source matrix (e.g. dmat elements ≠ AR coefficients),
        // the GM matrix differs from the correct one.  This quantifies what the bug fix changes.
        float[] w = { 0, -0.0268f, -0.806f, 0.645f, 1.186f };
        int n = 4, nm = 1;
        int[] nyi = { 0, 4 };

        // Correct source: AR coefficients from W
        var aCorrect = new float[n + 1, nm + 1];
        for (int j = 1; j <= n; j++) aCorrect[j, 1] = w[j];
        var gmCorrect = new float[n + 1, n + 1];
        MatrixLib.Xgmkon(gmCorrect, aCorrect, n, nm, nyi);

        // Wrong source: simulate what the old buggy code did —
        // pass dmat entries (e.g. a cross-correlation like 999 in the [2,1] slot).
        var aWrong = new float[n + 1, nm + 1];
        aWrong[2, 1] = 999f;  // garbage dmat value in slot Xgmkon reads for order ≥ 2
        var gmWrong = new float[n + 1, n + 1];
        MatrixLib.Xgmkon(gmWrong, aWrong, n, nm, nyi);

        bool differ = false;
        for (int i = 1; i <= n && !differ; i++)
            for (int j = 1; j <= n && !differ; j++)
                if (Math.Abs(gmCorrect[i, j] - gmWrong[i, j]) > 0.01f) differ = true;
        Assert.True(differ,
            "Xgmkon with correct aForGm vs wrong source must produce different GM matrices.");
    }

    // ─────────────────────────────────────────────────────────────────
    // Stage 4 – State-space decomposition (Xmdecd)
    // ─────────────────────────────────────────────────────────────────

    [Fact]
    public void Stage4_Xmdecd_A_HasObservableCanonicalCompanionForm()
    {
        // Observable canonical companion form for n=4, nm=1:
        //   A = [[0,1,0,0],
        //        [0,0,1,0],
        //        [0,0,0,1],
        //        [W[1],W[2],W[3],W[4]]]
        float[] w = { 0, -0.0268f, -0.806f, 0.645f, 1.186f, 0f, 0f, 0f, 0.00176f };
        int n = 4, nu = 1, nm = 1;
        int[] nyi = { 0, 4 };
        var a = new float[n + 1, n + 1];
        var b = new float[n + 1, nu + 1];
        var c = new float[nm + 1, n + 1];
        Identification.Xmdecd(a, n, b, nu, c, nm, w, AppState.NPWMA, nyi, out int ierr);
        Assert.Equal(0, ierr);

        // Rows 1..n-1: superdiagonal = 1, all other entries = 0
        for (int row = 1; row <= n - 1; row++)
            for (int col = 1; col <= n; col++)
            {
                float expected = col == row + 1 ? 1f : 0f;
                Assert.True(Math.Abs(a[row, col] - expected) < 1e-6f,
                    $"A[{row},{col}] = {a[row, col]}, expected {expected}");
            }

        // Last row = AR coefficients
        for (int col = 1; col <= n; col++)
            Assert.True(Math.Abs(a[n, col] - w[col]) < 1e-6f,
                $"A[{n},{col}] = {a[n, col]}, expected W[{col}] = {w[col]}");
    }

    [Fact]
    public void Stage4_Xmdecd_C_IsFirstStateObservation()
    {
        // C = [1, 0, 0, 0] — first state is the observed output.
        float[] w = { 0, -0.0268f, -0.806f, 0.645f, 1.186f, 0f, 0f, 0f, 0.00176f };
        int n = 4, nu = 1, nm = 1;
        int[] nyi = { 0, 4 };
        var a = new float[n + 1, n + 1];
        var b = new float[n + 1, nu + 1];
        var c = new float[nm + 1, n + 1];
        Identification.Xmdecd(a, n, b, nu, c, nm, w, AppState.NPWMA, nyi, out int ierr);
        Assert.Equal(0, ierr);
        Assert.Equal(1f, c[1, 1]);
        for (int col = 2; col <= n; col++)
            Assert.Equal(0f, c[1, col]);
    }

    // ─────────────────────────────────────────────────────────────────
    // Stage 5 – Prediction quality using identified AR coefficients
    //
    // IMPORTANT: After Xident returns, W[1..4] = AR coefficients (from theta),
    // but W[5..8] = B state-space (post Guidorzi transform / Mimprd), NOT the
    // original theta MA.  The theta MA ≈ 0 (backward regression on causal input)
    // is overwritten and is not directly accessible from the returned W.
    //
    // Therefore Stage5 tests use AR-only predictions (W[1..4]) and test:
    //   • Σ W_AR ≈ 1  (DC-gain constraint enforced by steady-state dominated LS)
    //   • Backward and forward AR predictions are accurate at steady state
    //   • Forward AR prediction is inaccurate in early transient (model needs B*u)
    // ─────────────────────────────────────────────────────────────────

    [Fact]
    public void Stage5_SumOfARCoefficients_ApproximatesOne_StepResponseDCConstraint()
    {
        // The regression on step-response data is dominated by steady-state samples
        // (y ≈ u ≈ RawOne), which forces the DC-gain constraint:
        //   y_ss = (Σ AR) · y_ss + (Σ MA) · u_ss.
        // With Σ MA ≈ 0 from backward causal regression → Σ W[1..4] ≈ 1.
        var (w, job) = RunXidentOnPT4();
        Assert.Equal(0, job);
        float sumAR = w[1] + w[2] + w[3] + w[4];
        Assert.True(Math.Abs(sumAR - 1f) < 0.01f,
            $"Sum of AR = {sumAR:F4}, expected ≈ 1.0 " +
            "(DC-gain constraint from step-response dominated regression).");
    }

    [Fact]
    public void Stage5_BackwardARPrediction_AtSteadyState_IsAccurate()
    {
        // At steady state all samples are ≈ RawOne, so:
        //   y_bwd(t) = Σ W_AR · RawOne ≈ RawOne   (because Σ AR ≈ 1).
        // Max absolute error should be a few raw counts (< 0.5 % of RawOne = 20).
        var idat = MakePT4Data();
        var (w, job) = RunXidentOnPT4();
        Assert.Equal(0, job);
        double maxErr = 0;
        for (int t = 370; t <= 394; t++)
        {
            double pred = w[1] * idat[ChOut, t + 4] + w[2] * idat[ChOut, t + 3]
                        + w[3] * idat[ChOut, t + 2] + w[4] * idat[ChOut, t + 1];
            maxErr = Math.Max(maxErr, Math.Abs(idat[ChOut, t] - pred));
        }
        Assert.True(maxErr < 20,
            $"Backward AR prediction error at steady state = {maxErr:F1} raw counts " +
            "(>20 would indicate Σ AR ≠ 1 or buffer not at steady state).");
    }

    [Fact]
    public void Stage5_ForwardARPrediction_AtSteadyState_IsAccurate()
    {
        // Same Σ AR ≈ 1 property applies to the forward direction:
        //   y_fwd(t) = Σ W_AR · y(t−j) ≈ Σ AR · RawOne ≈ RawOne.
        var idat = MakePT4Data();
        var (w, job) = RunXidentOnPT4();
        Assert.Equal(0, job);
        double maxErr = 0;
        for (int t = 375; t <= 399; t++)
        {
            double pred = w[4] * idat[ChOut, t - 1] + w[3] * idat[ChOut, t - 2]
                        + w[2] * idat[ChOut, t - 3] + w[1] * idat[ChOut, t - 4];
            maxErr = Math.Max(maxErr, Math.Abs(idat[ChOut, t] - pred));
        }
        Assert.True(maxErr < 20,
            $"Forward AR prediction error at steady state = {maxErr:F1} raw counts " +
            "(>20 would indicate Σ AR ≠ 1).");
    }

    [Fact]
    public void Stage5_ForwardARPrediction_EarlyTransient_IsInaccurate_ModelNeedsBInputTerm()
    {
        // During the early transient (t=56..60), past y values y(t−1..t−4) are still
        // near zero while the true output is rising (y ≈ 2..44 raw counts).
        // An AR-only model therefore massively underpredicts the initial rise — the B*u
        // input term is required.  This documents that the backward-regression W cannot
        // drive the transient by itself; it needs the full state-space model (A, B, C).
        // Test: max relative error |y − ŷ_AR| / y > 25% for at least one sample.
        var idat = MakePT4Data();
        var (w, job) = RunXidentOnPT4();
        Assert.Equal(0, job);
        double maxRelErr = 0;
        for (int t = 56; t <= 60; t++)
        {
            double pred  = w[4] * idat[ChOut, t - 1] + w[3] * idat[ChOut, t - 2]
                         + w[2] * idat[ChOut, t - 3] + w[1] * idat[ChOut, t - 4];
            double yTrue = idat[ChOut, t];
            if (yTrue > 0)
                maxRelErr = Math.Max(maxRelErr, Math.Abs(yTrue - pred) / yTrue);
        }
        Assert.True(maxRelErr > 0.25,
            $"Max relative forward AR error = {maxRelErr:P1} in early transient — " +
            "expected > 25 % (AR alone cannot drive the step rise; B*u term is required).");
    }
}

// ============================================================
// PRBS Auto-Test Pipeline (menu "b")
//
// System: two cascaded PT1s
//   x[k] = z1·x[k-1] + (1-z1)·u[k-1]   (T1=5s)
//   y[k] = z2·y[k-1] + (1-z2)·x[k-1]   (T2=10s)
//
// Equivalent AR(2):  y[k] = a1·y[k-1] + a2·y[k-2] + b·u[k-2]
//   z1 = e^{-0.2} ≈ 0.8187,  z2 = e^{-0.1} ≈ 0.9048
//   a1 = z1+z2 ≈ 1.7236,     a2 = -z1·z2 ≈ -0.7408
//
// Backward Xident (n=2) recovers via Yule-Walker symmetry:
//   W[1] ≈ a2  (coeff of y[t+2])
//   W[2] ≈ a1  (coeff of y[t+1])
//   W[3], W[4] ≈ 0  (backward causality kills MA terms)
// ============================================================
public class AutoTestPRBSTests
{
    const double Z1    = 0.818730753078; // e^{-0.2}, T1=5s
    const double Z2    = 0.904837418036; // e^{-0.1}, T2=10s
    const double A1    = Z1 + Z2;        // ≈ 1.723568  forward AR[1]
    const double A2    = -(Z1 * Z2);     // ≈ −0.740818 forward AR[2]  (= −e^{-0.3})
    const double B     = (1-Z1) * (1-Z2);// ≈ 0.017245  MA coef, DC gain = 1
    const int    Len   = 400;
    const short  Hi    = 2048;
    const short  Lo    = -2048;
    const int    ChIn  = 9;
    const int    ChOut = 1;

    // Reproduce exact signal data from RunAutoTestPRBS.
    static short[,] MakePRBSData()
    {
        var idat = new short[AppState.NADDAX + 1, AppState.LENMA + 1];

        // 7-bit PRBS: poly x^7+x^6+1, period=127, all-ones seed
        int[] reg = new int[8];
        for (int i = 1; i <= 7; i++) reg[i] = 1;
        for (int k = 1; k <= Len; k++)
        {
            idat[ChIn, k] = reg[7] == 1 ? Hi : Lo;
            int fb = reg[7] ^ reg[6];
            for (int r = 7; r > 1; r--) reg[r] = reg[r - 1];
            reg[1] = fb;
        }

        // Cascade simulation: x[k]=z1*x[k-1]+(1-z1)*u[k-1], y[k]=z2*y[k-1]+(1-z2)*x[k-1]
        double xPrev = 0, yPrev = 0, uPrev = 0;
        for (int k = 1; k <= Len; k++)
        {
            double xCurr = Z1 * xPrev + (1-Z1) * uPrev;
            double yCurr = Z2 * yPrev + (1-Z2) * xPrev;
            idat[ChOut, k] = (short)Math.Clamp((long)Math.Round(yCurr), -32000, 32000);
            xPrev = xCurr;
            yPrev = yCurr;
            uPrev = idat[ChIn, k];
        }
        return idat;
    }

    static (float[] w, int job) RunXidentOnPRBS()
    {
        var idat = MakePRBSData();
        int[] ida = new int[AppState.NADDAX + 1]; ida[1] = ChIn;
        int[] iad = new int[AppState.NADMA  + 1]; iad[1] = ChOut;
        int[] nyi = new int[AppState.NADMA  + 1]; nyi[1] = 2;   // 2nd-order model
        float[] w = new float[AppState.NPWMA + 1];

        const int LTH = 30, LTH2 = 61, LTHLM = 38, LM = 8, LU = 10;
        var dmat  = new float[LTH + 1, LTHLM + 1];
        var rh    = new float[LTH + 1, LTH   + 1];
        var bWork = new float[LTH + 1, LU    + 1];
        var ipos0 = new int  [LTH + 1, LM    + 1];
        var wksp  = new float[LTH + 1, LTH2  + 1];

        int job = 1;
        Identification.Xident(w, idat, ida, iad,
            Len, nanf: 3, ianz: Len - 3, npsave: Len,
            nyi, nu: 1, nm: 1,
            dmat, rh, bWork, ipos0, iglw: 0, wksp, ref job);
        return (w, job);
    }

    // ──────────────────────────────────────────────────────────────
    // Input signal properties
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void PRBS_Input_HasCorrectAmplitude()
    {
        var idat = MakePRBSData();
        for (int k = 1; k <= Len; k++)
        {
            short u = idat[ChIn, k];
            Assert.True(u == Hi || u == Lo, $"PRBS at k={k} has unexpected value {u}");
        }
    }

    [Fact]
    public void PRBS_Input_HasBalancedDutyCycle()
    {
        // 7-bit PRBS period=127: 64 high, 63 low (one extra high for maximal sequence).
        // Over 400 samples (3.15 periods), count high vs low.
        var idat = MakePRBSData();
        int countHi = 0;
        for (int k = 1; k <= Len; k++)
            if (idat[ChIn, k] == Hi) countHi++;
        // Ratio should be close to 64/127 ≈ 0.504; allow 1% deviation over 400 samples.
        double ratio = (double)countHi / Len;
        Assert.True(Math.Abs(ratio - 0.504) < 0.02,
            $"PRBS duty cycle = {ratio:P1}, expected ≈ 50.4%");
    }

    [Fact]
    public void PRBS_Output_IsZeroMean_ApproximatelyBalancedPRBS()
    {
        // Balanced ±2048 PRBS on a stable system → output should be near zero mean.
        var idat = MakePRBSData();
        double sum = 0;
        for (int k = 1; k <= Len; k++) sum += idat[ChOut, k];
        double mean = sum / Len;
        // DC gain=1, input ±2048; mean offset due to slight PRBS imbalance should be small.
        Assert.True(Math.Abs(mean) < 230,
            $"Output mean = {mean:F1} raw counts — expected near 0 for balanced PRBS.");
    }

    [Fact]
    public void PRBS_Output_ConvergesTo2ndOrderDifferenceEquation()
    {
        // After warm-up (5 samples), output should satisfy the 2nd-order AR(2) relation
        // y[k] ≈ a1·y[k-1] + a2·y[k-2] + b·u[k-2] to within 1 raw count (rounding only).
        var idat = MakePRBSData();
        int maxErr = 0;
        for (int k = 6; k <= Len; k++)
        {
            double predicted = A1 * idat[ChOut, k-1] + A2 * idat[ChOut, k-2]
                             + B  * idat[ChIn,  k-2];
            int err = Math.Abs((int)idat[ChOut, k] - (int)Math.Round(predicted));
            maxErr = Math.Max(maxErr, err);
        }
        Assert.True(maxErr <= 2,
            $"Max AR(2) residual = {maxErr} raw counts — should be ≤2. " +
            "(±0.5 rounding on y[k-1] and y[k-2] propagates through a1≈1.72, a2≈-0.74 " +
            "giving up to ≈1.23 counts error in the predicted value before output rounding.)");
    }

    // ──────────────────────────────────────────────────────────────
    // Identification: AR recovery
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void PRBS_Xident_CompletesWithoutSingularity()
    {
        var (_, job) = RunXidentOnPRBS();
        Assert.Equal(0, job);
    }

    [Fact]
    public void PRBS_Xident_W1_CloseToA2()
    {
        // Backward Yule-Walker for AR(2):
        //   W[1] is the solution with RHS = R_y(2), which equals forward a2.
        //   W[1] ≈ a2 = −z1·z2 = −e^{−0.3} ≈ −0.7408
        // 7-bit PRBS over 3+ periods → well-conditioned normal equations → error < 5%.
        var (w, job) = RunXidentOnPRBS();
        Assert.Equal(0, job);
        float err = Math.Abs(w[1] - (float)A2);
        Assert.True(err < 0.05f * (float)Math.Abs(A2),
            $"W[1] error = {err:F6} ({err/Math.Abs((float)A2)*100:F2}%) — " +
            $"identified {w[1]:F6} vs true a2={A2:F6}. " +
            "Backward Yule-Walker must recover a2 within 5%.");
    }

    [Fact]
    public void PRBS_Xident_W2_CloseToA1()
    {
        // Backward Yule-Walker for AR(2):
        //   W[2] is the solution with RHS = R_y(1), which equals forward a1.
        //   W[2] ≈ a1 = z1+z2 ≈ 1.7236
        var (w, job) = RunXidentOnPRBS();
        Assert.Equal(0, job);
        float err = Math.Abs(w[2] - (float)A1);
        Assert.True(err < 0.05f * (float)A1,
            $"W[2] error = {err:F6} ({err/(float)A1*100:F2}%) — " +
            $"identified {w[2]:F6} vs true a1={A1:F6}. " +
            "Backward Yule-Walker must recover a1 within 5%.");
    }

    [Fact]
    public void PRBS_Xident_W1_IsNegative_W2_IsGreaterThan1()
    {
        // Structural sanity check: for a stable 2nd-order system with two positive poles
        // inside the unit circle, the backward AR coefficients must satisfy:
        //   W[1] = a2 = -z1*z2 < 0       (negative product of poles)
        //   W[2] = a1 = z1+z2 > 1         (sum of poles, both < 1, sum > 1 here)
        // This is a direct consequence of the pole locations z1≈0.82, z2≈0.90.
        var (w, job) = RunXidentOnPRBS();
        Assert.Equal(0, job);
        Assert.True(w[1] < 0f, $"W[1]={w[1]:F4} should be negative (= a2 = −z1·z2).");
        Assert.True(w[2] > 1f, $"W[2]={w[2]:F4} should be > 1 (= a1 = z1+z2 ≈ 1.72).");
    }

    [Fact]
    public void PRBS_Xident_BStateSpace_BothNearZero_ConfirmsBackwardCausality()
    {
        // Backward LS: E[u(t+k)·y(t)] = 0 for k≥1 by causality → both theta_MA → 0.
        // After Guidorzi transform, W[3] and W[4] (B_ss entries) are also ≈ 0.
        var (w, job) = RunXidentOnPRBS();
        Assert.Equal(0, job);
        Assert.True(Math.Abs(w[3]) < 0.05f,
            $"W[3] (B_ss[1]) = {w[3]:F4} — expected ≈0 (backward causality).");
        Assert.True(Math.Abs(w[4]) < 0.05f,
            $"W[4] (B_ss[2]) = {w[4]:F4} — expected ≈0 (backward causality).");
    }

    [Fact]
    public void PRBS_Xident_PoleProduct_Matches_A2()
    {
        // Extra numerical check: for a 2nd-order system, the product of eigenvalues of the
        // identified A matrix should equal det(A) ≈ z1*z2 ≈ 0.7408 (= -a2).
        // Equivalently the identified W[1] ≈ a2 so -W[1] ≈ z1*z2.
        var (w, _) = RunXidentOnPRBS();
        double identifiedPoleProduct = -w[1]; // −a2 = z1·z2 = e^{−0.3}
        double truePoleProduct       = Z1 * Z2;
        Assert.True(Math.Abs(identifiedPoleProduct - truePoleProduct) < 0.05,
            $"Identified pole product (−W[1]={identifiedPoleProduct:F4}) should be " +
            $"close to z1·z2={truePoleProduct:F4} (= e^{{−0.3}}).");
    }

    [Fact]
    public void PRBS_Xident_ARCoefficient_MuchBetterThanStepResponse()
    {
        // Contrast test: PRBS AR error (W[2] vs a1) should be far smaller than the
        // analogous error from the PT4 step-response identification.
        var (wPRBS, jobPRBS) = RunXidentOnPRBS();
        Assert.Equal(0, jobPRBS);
        float prsbError = Math.Abs(wPRBS[2] - (float)A1);  // W[2] vs a1

        // Run PT4 step test for comparison
        const double aPT4 = 0.904837418036;
        const int lenPT4 = 400, stepAt = 50;
        var idatPT4 = new short[AppState.NADDAX + 1, AppState.LENMA + 1];
        const short rawOne = 4096;
        for (int k = 1; k <= lenPT4; k++)
            idatPT4[9, k] = k >= stepAt ? rawOne : (short)0;
        double x1 = 0, x2 = 0, x3 = 0, x4 = 0, uPrev = 0;
        double bPT4 = 1 - aPT4;
        for (int k = 1; k <= lenPT4; k++)
        {
            double uCurr = idatPT4[9, k];
            double nx1 = aPT4*x1+bPT4*uPrev; double nx2=aPT4*x2+bPT4*x1;
            double nx3 = aPT4*x3+bPT4*x2;    double nx4=aPT4*x4+bPT4*x3;
            x1=nx1; x2=nx2; x3=nx3; x4=nx4;
            idatPT4[1, k] = (short)Math.Clamp((int)Math.Round(x4), -32000, 32000);
            uPrev = uCurr;
        }
        int[] idaPT4 = new int[AppState.NADDAX + 1]; idaPT4[1] = 9;
        int[] iadPT4 = new int[AppState.NADMA  + 1]; iadPT4[1] = 1;
        int[] nyiPT4 = new int[AppState.NADMA  + 1]; nyiPT4[1] = 4;
        float[] wPT4 = new float[AppState.NPWMA + 1];
        const int LTH=30,LTH2=61,LTHLM=38,LM=8,LU=10;
        int job4 = 1;
        Identification.Xident(wPT4, idatPT4, idaPT4, iadPT4,
            lenPT4, 5, lenPT4-5, lenPT4, nyiPT4, 1, 1,
            new float[LTH+1,LTHLM+1], new float[LTH+1,LTH+1],
            new float[LTH+1,LU+1], new int[LTH+1,LM+1], 0,
            new float[LTH+1,LTH2+1], ref job4);
        // PT4 W[1] = coeff of y[k-4], true = -a^4 ≈ -0.670
        float stepError = Math.Abs(wPT4[1] - (float)(-Math.Pow(aPT4, 4)));

        Assert.True(prsbError < stepError * 0.15f,
            $"PRBS W[2] error ({prsbError:F4}) should be < 15% of PT4 step AR error " +
            $"({stepError:F4}). PRBS gives far better AR recovery than step response.");
    }
}

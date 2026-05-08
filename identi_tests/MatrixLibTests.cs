using Identi;
using Xunit;

namespace IdentTests;

// ============================================================
// Tests for MatrixLib – linear algebra primitives.
// All arrays use 1-based indexing (element [0] unused).
// ============================================================
public class MatrixLibTests
{
    // ---- tolerance for float comparisons ----
    private const float Tol = 1e-5f;
    private static void AssertNear(float expected, float actual, float tol = Tol)
        => Assert.True(Math.Abs(expected - actual) <= tol,
            $"Expected {expected} ± {tol}, got {actual}");

    // ----------------------------------------------------------
    // Mzeset
    // ----------------------------------------------------------
    [Fact]
    public void Mzeset_ZerosAll()
    {
        var a = new float[4, 4];
        for (int i = 1; i <= 3; i++)
            for (int j = 1; j <= 3; j++) a[i, j] = (i + 1) * (j + 1);
        MatrixLib.Mzeset(a, 3, 3);
        for (int i = 1; i <= 3; i++)
            for (int j = 1; j <= 3; j++)
                Assert.Equal(0f, a[i, j]);
    }

    [Fact]
    public void Mzeset_WithColStart_ZerosSubrange()
    {
        var a = new float[4, 6];
        for (int i = 1; i <= 3; i++)
            for (int j = 1; j <= 5; j++) a[i, j] = 99f;
        // zero rows 1-3, cols 3-4 (colStart=3, n=2)
        MatrixLib.Mzeset(a, 3, 2, colStart: 3);
        // cols 1,2 untouched
        for (int i = 1; i <= 3; i++) { Assert.Equal(99f, a[i, 1]); Assert.Equal(99f, a[i, 2]); }
        // cols 3,4 zeroed
        for (int i = 1; i <= 3; i++) { Assert.Equal(0f, a[i, 3]); Assert.Equal(0f, a[i, 4]); }
        // col 5 untouched
        for (int i = 1; i <= 3; i++) Assert.Equal(99f, a[i, 5]);
    }

    // ----------------------------------------------------------
    // Vvcopy
    // ----------------------------------------------------------
    [Fact]
    public void Vvcopy_CopiesCorrectly()
    {
        float[] src = { 0, 1f, 2f, 3f };
        float[] dst = new float[4];
        MatrixLib.Vvcopy(dst, src, 3);
        Assert.Equal(1f, dst[1]); Assert.Equal(2f, dst[2]); Assert.Equal(3f, dst[3]);
    }

    [Fact]
    public void VvcopyFromCol_ExtractsColumn()
    {
        var m = new float[4, 4];
        m[1, 2] = 5f; m[2, 2] = 6f; m[3, 2] = 7f;
        float[] y = new float[4];
        MatrixLib.VvcopyFromCol(y, m, col: 2, n: 3);
        Assert.Equal(5f, y[1]); Assert.Equal(6f, y[2]); Assert.Equal(7f, y[3]);
    }

    // ----------------------------------------------------------
    // Mvprod  y = A * x
    // ----------------------------------------------------------
    [Fact]
    public void Mvprod_2x2()
    {
        // A = [[1,2],[3,4]]  x = [1,2]  → y = [5, 11]
        var a = new float[3, 3];
        a[1, 1] = 1; a[1, 2] = 2; a[2, 1] = 3; a[2, 2] = 4;
        float[] x = { 0, 1f, 2f };
        float[] y = new float[3];
        MatrixLib.Mvprod(y, a, 2, 2, x);
        AssertNear(5f, y[1]); AssertNear(11f, y[2]);
    }

    // ----------------------------------------------------------
    // Mdpsum  B = A + x*yᵀ  (rank-1 update)
    // ----------------------------------------------------------
    [Fact]
    public void Mdpsum_RankOneUpdate()
    {
        // A = [[0,0],[0,0]]  x=[1,2]  y=[3,4]  → B = [[3,4],[6,8]]
        var a = new float[3, 3];
        var b = new float[3, 3];
        float[] x = { 0, 1f, 2f };
        float[] y = { 0, 3f, 4f };
        MatrixLib.Mdpsum(b, a, x, y, 2, 2);
        AssertNear(3f, b[1, 1]); AssertNear(4f, b[1, 2]);
        AssertNear(6f, b[2, 1]); AssertNear(8f, b[2, 2]);
    }

    // ----------------------------------------------------------
    // Vvdot
    // ----------------------------------------------------------
    [Fact]
    public void Vvdot_DotProduct()
    {
        float[] x = { 0, 1f, 2f, 3f };
        float[] y = { 0, 4f, 5f, 6f };
        float d = MatrixLib.Vvdot(x, y, 3);  // 1*4 + 2*5 + 3*6 = 32
        AssertNear(32f, d);
    }

    [Fact]
    public void Vvdot_WithOffset()
    {
        float[] x = { 0, 0, 1f, 2f };  // xOfs=2 → [1, 2]
        float[] y = { 0, 3f, 4f, 0  };  // yOfs=1 → [3, 4]
        float d = MatrixLib.Vvdot(x, y, 2, xOfs: 2, yOfs: 1); // 1*3 + 2*4 = 11
        AssertNear(11f, d);
    }

    // ----------------------------------------------------------
    // Vvsum
    // ----------------------------------------------------------
    [Fact]
    public void Vvsum_AddsTwoVectors()
    {
        float[] x = { 0, 1f, 2f };
        float[] y = { 0, 3f, 4f };
        float[] z = new float[3];
        MatrixLib.Vvsum(z, x, y, 2);
        AssertNear(4f, z[1]); AssertNear(6f, z[2]);
    }

    // ----------------------------------------------------------
    // Vvssum  y += s*x
    // ----------------------------------------------------------
    [Fact]
    public void Vvssum_ScaledAccumulate()
    {
        float[] x = { 0, 2f, 3f };
        float[] y = { 0, 1f, 1f };
        MatrixLib.Vvssum(y, x, 2f, 2); // y += 2*x → [5, 7]
        AssertNear(5f, y[1]); AssertNear(7f, y[2]);
    }

    [Fact]
    public void Vvssum_ZeroScaleNoChange()
    {
        float[] x = { 0, 99f };
        float[] y = { 0, 1f };
        MatrixLib.Vvssum(y, x, 0f, 1);
        AssertNear(1f, y[1]);
    }

    // ----------------------------------------------------------
    // Vsprod  y = s*x
    // ----------------------------------------------------------
    [Fact]
    public void Vsprod_ScaledCopy()
    {
        float[] x = { 0, 3f, 4f };
        float[] y = new float[3];
        MatrixLib.Vsprod(y, x, 2f, 2);
        AssertNear(6f, y[1]); AssertNear(8f, y[2]);
    }

    // ----------------------------------------------------------
    // Indmax
    // ----------------------------------------------------------
    [Fact]
    public void Indmax_FindsMaxAbsolute()
    {
        float[] x = { 0, 1f, -5f, 3f, -2f };
        int idx = MatrixLib.Indmax(x, 4); // -5 is largest absolute, index 2
        Assert.Equal(2, idx);
    }

    [Fact]
    public void Indmax_SingleElement()
    {
        float[] x = { 0, 7f };
        Assert.Equal(1, MatrixLib.Indmax(x, 1));
    }

    // ----------------------------------------------------------
    // Sgefa1 + Sgesl1:  solve A*x = b  (LU round-trip)
    // ----------------------------------------------------------
    [Theory]
    [InlineData(2)]
    [InlineData(3)]
    public void SgefaSgesl_SolvesExactly(int n)
    {
        // Build a random-ish well-conditioned matrix
        var a = MakeTestMatrix(n);
        var aOrig = Clone(a, n);
        int[] ipvt = new int[n + 1];
        MatrixLib.Sgefa1(a, n, ipvt, out int info);
        Assert.Equal(0, info);

        // x_true = [1..n], compute b = A_orig * x_true
        float[] xTrue = new float[n + 1];
        float[] b     = new float[n + 1];
        for (int i = 1; i <= n; i++) xTrue[i] = i;
        for (int i = 1; i <= n; i++)
        {
            float s = 0f;
            for (int j = 1; j <= n; j++) s += aOrig[i, j] * xTrue[j];
            b[i] = s;
        }

        MatrixLib.Sgesl1(a, n, ipvt, b);
        for (int i = 1; i <= n; i++)
            AssertNear(xTrue[i], b[i], 1e-4f);
    }

    [Fact]
    public void Sgefa1_SingularMatrix_ReturnsNonZeroInfo()
    {
        var a = new float[3, 3]; // all zeros → singular
        int[] ipvt = new int[3];
        MatrixLib.Sgefa1(a, 2, ipvt, out int info);
        Assert.NotEqual(0, info);
    }

    // ----------------------------------------------------------
    // Srotg – Givens rotation
    // ----------------------------------------------------------
    [Fact]
    public void Srotg_BasicRotation()
    {
        float a = 3f, b = 4f;
        MatrixLib.Srotg(ref a, ref b, out float c, out float s);
        // r = 5, c = 3/5, s = 4/5
        AssertNear(5f, a);
        AssertNear(3f / 5f, c);
        AssertNear(4f / 5f, s);
        // verify c²+s²=1
        AssertNear(1f, c * c + s * s);
    }

    [Fact]
    public void Srotg_ZeroA()
    {
        float a = 0f, b = 5f;
        MatrixLib.Srotg(ref a, ref b, out float c, out float s);
        AssertNear(5f, a); // r = |b|
        AssertNear(0f, c); // c=0, s=1
    }

    [Fact]
    public void Srotg_BothZero()
    {
        float a = 0f, b = 0f;
        MatrixLib.Srotg(ref a, ref b, out float c, out float s);
        AssertNear(1f, c);
        AssertNear(0f, s);
    }

    // ----------------------------------------------------------
    // Strsl1  – upper triangular solve
    // ----------------------------------------------------------
    [Fact]
    public void Strsl1_SolvesUpperTriangular()
    {
        // T = [[2,1],[0,3]]  b=[5,6]  → x=[1.5, 2]
        var t = new float[3, 3];
        t[1, 1] = 2; t[1, 2] = 1; t[2, 2] = 3;
        float[] b = { 0, 5f, 6f };
        MatrixLib.Strsl1_clean(t, 2, b, out int info);
        Assert.Equal(0, info);
        AssertNear(2f, b[2]);
        AssertNear(1.5f, b[1]);
    }

    [Fact]
    public void Strsl1_ZeroDiagonal_ReturnsNonZeroInfo()
    {
        var t = new float[3, 3];
        float[] b = { 0, 1f, 2f };
        MatrixLib.Strsl1_clean(t, 2, b, out int info);
        Assert.NotEqual(0, info);
    }

    // ----------------------------------------------------------
    // Schud – QR rank-1 update:
    // After inserting a vector into an initially-zero R, R should
    // be upper triangular and R^T R should equal sum of outer products.
    // ----------------------------------------------------------
    [Fact]
    public void Schud_RUpperTriangular_After3Updates()
    {
        int p = 2;
        var r  = new float[p + 1, p + 1];
        var z  = new float[p + 1, 1 + 1];  // nz=1
        float[] rho = new float[2];
        float[] c   = new float[p + 1];
        float[] s   = new float[p + 1];
        float[] y   = { 0, 0f };

        float[][] xs = { new float[]{ 0, 1f, 0f },
                         new float[]{ 0, 0f, 1f },
                         new float[]{ 0, 1f, 1f } };

        foreach (var x in xs)
            MatrixLib.Schud(r, p, x, z, 0, y, rho, c, s);

        // R[2,1] must be 0 (upper triangular)
        AssertNear(0f, r[2, 1], 1e-5f);
        // R[1,1] and R[2,2] must be non-zero
        Assert.True(Math.Abs(r[1, 1]) > 1e-6f);
        Assert.True(Math.Abs(r[2, 2]) > 1e-6f);
    }

    // ----------------------------------------------------------
    // Mimprd – C = A⁻¹ B
    // ----------------------------------------------------------
    [Fact]
    public void Mimprd_InverseTimesIdentity()
    {
        // A = [[2,0],[0,4]]  B = I  → C = A⁻¹ = [[0.5,0],[0,0.25]]
        int m = 2;
        var a  = new float[m + 1, m + 1];
        var c  = new float[m + 1, m + 1];
        var bi = new float[m + 1, m + 1];
        a[1, 1] = 2f; a[2, 2] = 4f;
        bi[1, 1] = 1f; bi[2, 2] = 1f;
        int[] ipvt = new int[m + 1];
        MatrixLib.Mimprd(c, a, m, bi, m, ipvt, 1, out int info);
        Assert.Equal(0, info);
        AssertNear(0.5f,  c[1, 1]);
        AssertNear(0f,    c[1, 2]);
        AssertNear(0f,    c[2, 1]);
        AssertNear(0.25f, c[2, 2]);
    }

    // ----------------------------------------------------------
    // Strsl1 – documents the double-division bug in the public Strsl1.
    // Strsl1 does b[n] /= t[n,n] then calls Strsl1_impl which ALSO
    // does b[n] /= t[n,n], dividing b[n] twice.
    // Use Strsl1_clean instead, which delegates directly to Strsl1_impl.
    // ----------------------------------------------------------
    [Fact]
    public void Strsl1_HasDoubleDivisionBug_DiffersFromClean()
    {
        // T = [[2,1],[0,3]]  b=[5,6]  exact solution: x=[1.5, 2.0]
        var t = new float[3, 3];
        t[1, 1] = 2; t[1, 2] = 1; t[2, 2] = 3;
        float[] bBroken = { 0, 5f, 6f };
        float[] bClean  = { 0, 5f, 6f };

        MatrixLib.Strsl1(t, 2, bBroken, out _);        // buggy path
        MatrixLib.Strsl1_clean(t, 2, bClean, out _);   // correct path

        // Strsl1_clean gives the correct answer
        AssertNear(2.0f, bClean[2]);
        AssertNear(1.5f, bClean[1]);

        // Strsl1 double-divides b[2] by 3 → 6/(3·3)=0.667, not 2.0
        // This test PASSES while the bug is present; it would fail once fixed.
        Assert.True(Math.Abs(bBroken[2] - 2.0f) > 0.5f,
            "Bug appears fixed: Strsl1 no longer double-divides. Update this test.");
    }

    // ----------------------------------------------------------
    // Sgesl1 and Sgesl1_clean are both correct and produce the same result.
    // (Sgesl1 delegates to Sgesl1_impl; Sgesl1_clean also delegates to it.)
    // ----------------------------------------------------------
    [Fact]
    public void Sgesl1_And_Sgesl1Clean_GiveSameResult()
    {
        var aOrig = new float[3, 3];
        aOrig[1, 1] = 4; aOrig[1, 2] = 1;
        aOrig[2, 1] = 2; aOrig[2, 2] = 3;
        var a1 = Clone(aOrig, 2);
        var a2 = Clone(aOrig, 2);
        int[] piv1 = new int[3], piv2 = new int[3];
        MatrixLib.Sgefa1(a1, 2, piv1, out _);
        MatrixLib.Sgefa1(a2, 2, piv2, out _);
        float[] b1 = { 0, 7f, 8f };
        float[] b2 = { 0, 7f, 8f };

        MatrixLib.Sgesl1(a1, 2, piv1, b1);
        MatrixLib.Sgesl1_clean(a2, 2, piv2, b2);

        AssertNear(b1[1], b2[1]);
        AssertNear(b1[2], b2[2]);
        // Verify the solve is actually correct: A·x = [7,8] should give a unique solution
        Assert.True(Math.Abs(aOrig[1,1]*b1[1]+aOrig[1,2]*b1[2] - 7f) < 1e-4f);
    }

    // ----------------------------------------------------------
    // Sgefa1 + Sgesl1 for a 4×4 system
    // ----------------------------------------------------------
    [Fact]
    public void Sgefa1_Sgesl1_4x4_System()
    {
        var a = new float[5, 5];
        // Diagonal dominant for stability
        a[1,1]=10; a[1,2]= 1; a[1,3]= 2; a[1,4]= 0;
        a[2,1]= 1; a[2,2]=12; a[2,3]= 1; a[2,4]= 2;
        a[3,1]= 2; a[3,2]= 1; a[3,3]=11; a[3,4]= 1;
        a[4,1]= 0; a[4,2]= 2; a[4,3]= 1; a[4,4]= 9;
        var aOrig = Clone(a, 4);
        float[] xTrue = { 0, 1f, 2f, 3f, 4f };
        float[] b = new float[5];
        for (int i = 1; i <= 4; i++)
        {
            float s = 0;
            for (int j = 1; j <= 4; j++) s += aOrig[i, j] * xTrue[j];
            b[i] = s;
        }

        int[] ipvt = new int[5];
        MatrixLib.Sgefa1(a, 4, ipvt, out int info);
        Assert.Equal(0, info);
        MatrixLib.Sgesl1(a, 4, ipvt, b);
        for (int i = 1; i <= 4; i++)
            AssertNear(xTrue[i], b[i], 1e-3f);
    }

    // ----------------------------------------------------------
    // Schex right/left shift preserves upper-triangular structure
    // ----------------------------------------------------------
    [Fact]
    public void Schex_RightShift_RemainsUpperTriangular()
    {
        int p = 3;
        // Build a simple upper-triangular R by running SCHUD on identity rows
        var r = new float[p + 1, p + 1];
        var z = new float[p + 1, 1]; // dummy
        float[] rho = new float[1];
        float[] c   = new float[p + 1];
        float[] s   = new float[p + 1];
        float[] y   = new float[1];
        float[][] rows = {
            new float[]{ 0, 1f, 0f, 0f },
            new float[]{ 0, 0f, 1f, 0f },
            new float[]{ 0, 0f, 0f, 1f }
        };
        foreach (var x in rows)
            MatrixLib.Schud(r, p, x, z, 0, y, rho, c, s);

        // Right-shift columns 1..3
        MatrixLib.Schex(r, p, 1, 3, z, 0, c, s, 1);

        // After right shift, R should still be upper triangular
        Assert.Equal(0f, r[2, 1]);
        Assert.Equal(0f, r[3, 1]);
        Assert.Equal(0f, r[3, 2]);
    }

    [Fact]
    public void Schex_LeftShift_RemainsUpperTriangular()
    {
        int p = 3;
        var r = new float[p + 1, p + 1];
        var z = new float[p + 1, 1];
        float[] rho = new float[1];
        float[] c   = new float[p + 1];
        float[] s   = new float[p + 1];
        float[] y   = new float[1];
        float[][] rows = {
            new float[]{ 0, 2f, 1f, 0f },
            new float[]{ 0, 0f, 3f, 1f },
            new float[]{ 0, 0f, 0f, 4f }
        };
        foreach (var x in rows)
            MatrixLib.Schud(r, p, x, z, 0, y, rho, c, s);

        MatrixLib.Schex(r, p, 1, 3, z, 0, c, s, 2); // left shift

        Assert.Equal(0f, r[2, 1]);
        Assert.Equal(0f, r[3, 1]);
        Assert.Equal(0f, r[3, 2]);
    }

    // ----------------------------------------------------------
    // Xgmkon – Guidorzi transformation matrix
    // ----------------------------------------------------------
    [Fact]
    public void Xgmkon_FirstOrder_SISO_ReturnsIdentity()
    {
        // For order 1, nyi=[1]: GM = [[1]] regardless of A source.
        int[] nyi = { 0, 1 };
        var a   = new float[2, 2]; a[1, 1] = 999f; // source doesn't matter for order 1
        var gm  = new float[2, 2];
        MatrixLib.Xgmkon(gm, a, n: 1, nm: 1, nyi: nyi);
        AssertNear(1f, gm[1, 1]);
    }

    [Fact]
    public void Xgmkon_SecondOrder_SISO_Structure()
    {
        // nyi=[2], n=2, nm=1: GM = [[-a[2,1], 1], [1, 0]]
        // where a[2,1] is the second AR coefficient.
        int[] nyi = { 0, 2 };
        // Build a 2D source matrix from W = [w1=0.5, w2=-0.3]
        // Xgmkon accesses a[row, col=i=1] where row = ipj+ll+kk = 0+1+1 = 2.
        var a = new float[3, 3];
        a[1, 1] = 0.5f;   // W[1] - first AR coeff
        a[2, 1] = -0.3f;  // W[2] - second AR coeff (used in GM off-diagonal)
        var gm = new float[3, 3];

        MatrixLib.Xgmkon(gm, a, n: 2, nm: 1, nyi: nyi);

        // Cross-term: gm[1,1] = -a[2,1] = -(-0.3) = 0.3
        AssertNear(0.3f, gm[1, 1]);
        // Diagonal ones: gm[1,2]=1 and gm[2,1]=1
        AssertNear(1f, gm[1, 2]);
        AssertNear(1f, gm[2, 1]);
        // gm[2,2]=0 (never set → zero from Mzeset)
        AssertNear(0f, gm[2, 2]);
    }

    [Fact]
    public void Xgmkon_SecondOrder_SISO_SourceMustBeWNotInfoMatrix()
    {
        // This test documents the KNOWN BUG in Xident for order ≥ 2:
        // Xident currently calls Xgmkon(gm, dmat, ...) where dmat contains the
        // information matrix in cols 1..nth. For order ≥ 2, Xgmkon reads
        // a[ipj+ll+kk, i] = dmat[2, 1] (info matrix) instead of W[2] (theta).
        //
        // The correct source is a 2D matrix built from W[1..n*nm]:
        //   aForGm[j, i] = W[(i-1)*n + j]  for j=1..n, i=1..nm.
        //
        // For first-order (n=1), nblock=0 so the cross-terms are never accessed
        // and the bug is invisible.  For n≥2, GM is wrong → B state-space is wrong.
        //
        // Here we verify the CORRECT behaviour by passing the right source:
        int[] nyi = { 0, 2 };
        float[] w = { 0, 0.5f, -0.3f }; // W[1]=AR1, W[2]=AR2

        // Build correct source from W
        var aFromW = new float[3, 2]; aFromW[1, 1] = w[1]; aFromW[2, 1] = w[2];
        // Build wrong source from a typical info-matrix (cross-corr)
        var aWrong = new float[3, 2]; aWrong[1, 1] = 1e6f; aWrong[2, 1] = 500f;

        var gmCorrect = new float[3, 3];
        var gmWrong   = new float[3, 3];
        MatrixLib.Xgmkon(gmCorrect, aFromW, 2, 1, nyi);
        MatrixLib.Xgmkon(gmWrong,   aWrong, 2, 1, nyi);

        // Correct: gm[1,1] = -W[2] = 0.3
        AssertNear( 0.3f, gmCorrect[1, 1]);
        // Wrong: gm[1,1] = -500 (from info matrix)
        AssertNear(-500f, gmWrong[1, 1]);
        // This shows that passing the wrong source produces a completely wrong GM.
    }

    // ----------------------------------------------------------
    // helpers
    // ----------------------------------------------------------
    private static float[,] MakeTestMatrix(int n)
    {
        // Hilbert-like but scaled to be well-conditioned for small n
        var a = new float[n + 1, n + 1];
        for (int i = 1; i <= n; i++)
            for (int j = 1; j <= n; j++)
                a[i, j] = (i == j) ? (float)(n + i) : 1f / (i + j);
        return a;
    }

    private static float[,] Clone(float[,] src, int n)
    {
        var dst = new float[n + 1, n + 1];
        for (int i = 1; i <= n; i++)
            for (int j = 1; j <= n; j++)
                dst[i, j] = src[i, j];
        return dst;
    }
}

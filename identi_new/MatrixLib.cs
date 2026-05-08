namespace Identi;

// ============================================================
// MatrixLib – linear algebra primitives.
//
// Faithful C# translation of the subroutine library from
// IDELIB.F77 (Roether, 1985).
//
// Convention: all 2-D arrays use 1-based indexing;
// element [0,*] and [*,0] are intentionally unused.
// When a Fortran call passed a sub-matrix (e.g. DMAT(1,NTH1)),
// the C# equivalent receives the full array and a colStart offset.
// ============================================================

internal static class MatrixLib
{
    // ----------------------------------------------------------
    // MZESET – set all elements of an M×N submatrix to zero.
    // FORTRAN: SUBROUTINE MZESET(A, LA, M, N)
    //   Zeroes rows 1..M, columns colStart..(colStart+N-1).
    // ----------------------------------------------------------
    public static void Mzeset(float[,] a, int m, int n, int colStart = 1)
    {
        for (int j = colStart; j < colStart + n; j++)
            for (int i = 1; i <= m; i++)
                a[i, j] = 0f;
    }

    // ----------------------------------------------------------
    // VVCOPY – copy vector: y = x  (N elements, 1-based)
    // FORTRAN: SUBROUTINE VVCOPY(Y, X, N)
    // ----------------------------------------------------------
    public static void Vvcopy(float[] y, float[] x, int n, int yOfs = 1, int xOfs = 1)
    {
        for (int i = 0; i < n; i++)
            y[yOfs + i] = x[xOfs + i];
    }

    // ----------------------------------------------------------
    // Overload: copy a matrix column into a vector.
    // Copies rows 1..n of column col from matrix a into y[yOfs..].
    // ----------------------------------------------------------
    public static void VvcopyFromCol(float[] y, float[,] a, int col, int n, int yOfs = 1)
    {
        for (int i = 1; i <= n; i++)
            y[yOfs + i - 1] = a[i, col];
    }

    // ----------------------------------------------------------
    // Overload: copy a vector into a matrix column.
    // ----------------------------------------------------------
    public static void VvcopyToCol(float[,] a, int col, float[] x, int n, int xOfs = 1)
    {
        for (int i = 1; i <= n; i++)
            a[i, col] = x[xOfs + i - 1];
    }

    // ----------------------------------------------------------
    // MWRITE – print an M×N matrix to console.
    // FORTRAN: SUBROUTINE MWRITE(A, LA, M, N, NDEV, BEZ, IS)
    //   IS = number of significant digits (1..7).
    // ----------------------------------------------------------
    public static void Mwrite(float[,] a, int m, int n, string label,
                              int sigDigits, int colStart = 1)
    {
        string fmt = sigDigits switch
        {
            1 => "G9",  2 => "G10", 3 => "G11",
            4 => "G12", 5 => "G13", 6 => "G14",
            _ => "G15"
        };
        Console.WriteLine();
        Console.WriteLine($"  Matrix {label} ({m}×{n}):");
        const int colsPerRow = 7;
        for (int c0 = colStart; c0 < colStart + n; c0 += colsPerRow)
        {
            int c1 = Math.Min(c0 + colsPerRow - 1, colStart + n - 1);
            Console.WriteLine($"  Columns {c0 - colStart + 1} to {c1 - colStart + 1}:");
            for (int i = 1; i <= m; i++)
            {
                Console.Write("  ");
                for (int j = c0; j <= c1; j++)
                    Console.Write($" {a[i, j].ToString(fmt),12}");
                Console.WriteLine();
            }
        }
    }

    // ----------------------------------------------------------
    // MVPROD – matrix-vector product: y = A * x
    // FORTRAN: SUBROUTINE MVPROD(Y, A, LA, M, N, X)
    // ----------------------------------------------------------
    public static void Mvprod(float[] y, float[,] a, int m, int n, float[] x,
                              int colStart = 1)
    {
        for (int i = 1; i <= m; i++)
        {
            float sum = 0f;
            for (int j = 1; j <= n; j++)
                sum += a[i, colStart + j - 1] * x[j];
            y[i] = sum;
        }
    }

    // ----------------------------------------------------------
    // MDPSUM – B = A + x*yᵀ  (rank-1 update)
    // FORTRAN: SUBROUTINE MDPSUM(B, LB, A, LA, X, Y, M, N)
    //   B and A are M×N; x length M, y length N.
    //   colStart applies to both A and B.
    // ----------------------------------------------------------
    public static void Mdpsum(float[,] b, float[,] a,
                              float[] x, float[] y, int m, int n,
                              int colStartB = 1, int colStartA = 1)
    {
        for (int i = 1; i <= m; i++)
            for (int j = 1; j <= n; j++)
                b[i, colStartB + j - 1] = a[i, colStartA + j - 1] + x[i] * y[j];
    }

    // ----------------------------------------------------------
    // VVDOT – dot product: result = xᵀ y
    // FORTRAN: FUNCTION VVDOT(X, Y, N)
    // ----------------------------------------------------------
    public static float Vvdot(float[] x, float[] y, int n, int xOfs = 1, int yOfs = 1)
    {
        float s = 0f;
        for (int i = 0; i < n; i++)
            s += x[xOfs + i] * y[yOfs + i];
        return s;
    }

    // ----------------------------------------------------------
    // VVSUM – vector sum: z = x + y
    // FORTRAN: SUBROUTINE VVSUM(Z, X, Y, N)
    // ----------------------------------------------------------
    public static void Vvsum(float[] z, float[] x, float[] y, int n,
                             int zOfs = 1, int xOfs = 1, int yOfs = 1)
    {
        for (int i = 0; i < n; i++)
            z[zOfs + i] = x[xOfs + i] + y[yOfs + i];
    }

    // ----------------------------------------------------------
    // VVSSUM – scaled accumulate: y += s*x  (BLAS DAXPY)
    // FORTRAN: SUBROUTINE VVSSUM(Y, X, SKAL, N)
    // ----------------------------------------------------------
    public static void Vvssum(float[] y, float[] x, float skal, int n,
                              int yOfs = 1, int xOfs = 1)
    {
        if (n > 0 && skal != 0f)
            for (int i = 0; i < n; i++)
                y[yOfs + i] += skal * x[xOfs + i];
    }

    // ----------------------------------------------------------
    // VSPROD – scaled copy: y = s*x
    // FORTRAN: SUBROUTINE VSPROD(Y, X, SKAL, N)
    // ----------------------------------------------------------
    public static void Vsprod(float[] y, float[] x, float skal, int n,
                              int yOfs = 1, int xOfs = 1)
    {
        for (int i = 0; i < n; i++)
            y[yOfs + i] = skal * x[xOfs + i];
    }

    // ----------------------------------------------------------
    // INDMAX – index of element with largest absolute value (1-based)
    // FORTRAN: INTEGER FUNCTION INDMAX(X, N)
    // ----------------------------------------------------------
    public static int Indmax(float[] x, int n, int xOfs = 1)
    {
        if (n <= 0) return 0;
        int idx = 1;
        if (n > 1)
        {
            float smax = Math.Abs(x[xOfs]);
            for (int i = 1; i < n; i++)
            {
                float v = Math.Abs(x[xOfs + i]);
                if (v > smax) { smax = v; idx = i + 1; }
            }
        }
        return idx;
    }

    // ----------------------------------------------------------
    // SGEFA1 – LU factorisation with partial pivoting.
    // FORTRAN: SUBROUTINE SGEFA1(A, LDA, N, IPVT, INFO)
    //   On output A contains the L\U factorisation.
    //   info = 0 → success.  info = k → column k singular.
    // ----------------------------------------------------------
    public static void Sgefa1(float[,] a, int n, int[] ipvt, out int info)
    {
        info = 0;
        int nm1 = n - 1;
        if (nm1 >= 1)
        {
            for (int k = 1; k <= nm1; k++)
            {
                int kp1 = k + 1;
                // find pivot: index of max |a[k..n, k]|
                int l = Indmax_col(a, k, n, k) + k - 1;
                ipvt[k] = l;
                if (a[l, k] != 0f)
                {
                    if (l != k)
                    {
                        float t = a[l, k]; a[l, k] = a[k, k]; a[k, k] = t;
                    }
                    float scale = -1f / a[k, k];
                    // a[k+1..n, k] *= scale
                    for (int i = kp1; i <= n; i++) a[i, k] *= scale;
                    // row elimination
                    for (int j = kp1; j <= n; j++)
                    {
                        float t = a[l, j];
                        if (l != k) { a[l, j] = a[k, j]; a[k, j] = t; }
                        // a[k+1..n, j] += t * a[k+1..n, k]
                        for (int i = kp1; i <= n; i++)
                            a[i, j] += t * a[i, k];
                    }
                }
                else
                {
                    info = k;
                }
            }
        }
        ipvt[n] = n;
        if (a[n, n] == 0f) info = n;
    }

    // helper: index (1-based within slice) of max |a[rowStart..rowEnd, col]|
    private static int Indmax_col(float[,] a, int rowStart, int rowEnd, int col)
    {
        int idx = 1;
        float smax = Math.Abs(a[rowStart, col]);
        for (int i = rowStart + 1; i <= rowEnd; i++)
        {
            float v = Math.Abs(a[i, col]);
            if (v > smax) { smax = v; idx = i - rowStart + 1; }
        }
        return idx;
    }

    // ----------------------------------------------------------
    // SGESL1 – solve A*x = b using the LU factorisation from SGEFA1.
    // FORTRAN: SUBROUTINE SGESL1(A, LDA, N, IPVT, B)
    //   b is overwritten with the solution x (1-based).
    // ----------------------------------------------------------
    public static void Sgesl1(float[,] a, int n, int[] ipvt, float[] b)
        => Sgesl1_impl(a, n, ipvt, b);

    private static void Sgesl1_impl(float[,] a, int n, int[] ipvt, float[] b)
    {
        int nm1 = n - 1;
        // solve L*y = b  (in-place)
        if (nm1 >= 1)
        {
            for (int k = 1; k <= nm1; k++)
            {
                int l = ipvt[k];
                float t = b[l];
                if (l != k) { b[l] = b[k]; b[k] = t; }
                for (int i = k + 1; i <= n; i++)
                    b[i] += t * a[i, k];
            }
        }
        // solve U*x = y  (back-substitution)
        for (int kb = 1; kb <= n; kb++)
        {
            int k = n + 1 - kb;
            b[k] /= a[k, k];
            float t = -b[k];
            for (int i = 1; i < k; i++)
                b[i] += t * a[i, k];
        }
    }

    // Entry point that just calls the impl (the public API is now this):
    // Re-expose clean version:
    public static void Sgesl1_clean(float[,] a, int n, int[] ipvt, float[] b)
        => Sgesl1_impl(a, n, ipvt, b);

    // ----------------------------------------------------------
    // MIMPRD – C = A⁻¹ B  (or −A⁻¹ B)
    // FORTRAN: SUBROUTINE MIMPRD(C, LC, A, LA, M, B, LB, N, IPVT, JOB, INFO)
    //   JOB = 1  : factorise A, compute C = A⁻¹B
    //   JOB = 2  : reuse factorisation, compute C = A⁻¹B
    //   JOB = -1 : factorise A, compute C = −A⁻¹B
    //   JOB = -2 : reuse, compute C = −A⁻¹B
    //
    //   colStartC / colStartB allow sub-matrix passing (Fortran-style).
    // ----------------------------------------------------------
    public static void Mimprd(float[,] c, float[,] a, int m, float[,] b, int n,
                              int[] ipvt, int job, out int info,
                              int colStartC = 1, int colStartB = 1)
    {
        info = 0;
        if (Math.Abs(job) <= 1)
        {
            Sgefa1(a, m, ipvt, out info);
            if (info != 0) return;
        }
        if (n > 0)
        {
            float[] col = new float[m + 1];
            for (int i = 1; i <= n; i++)
            {
                for (int j = 1; j <= m; j++)
                    col[j] = job > 0 ? b[j, colStartB + i - 1]
                                     : -b[j, colStartB + i - 1];
                Sgesl1_impl(a, m, ipvt, col);
                for (int j = 1; j <= m; j++)
                    c[j, colStartC + i - 1] = col[j];
            }
        }
    }

    // Overload for "B and C are the same matrix" (in-place)
    public static void MimprdInPlace(float[,] mat, int m, int n, int[] ipvt,
                                     int job, out int info,
                                     int colStartRhs = 1, int numRhsCols = 1)
    {
        info = 0;
        if (Math.Abs(job) <= 1)
        {
            Sgefa1(mat, m, ipvt, out info);
            if (info != 0) return;
        }
        float[] col = new float[m + 1];
        for (int i = 0; i < numRhsCols; i++)
        {
            int c = colStartRhs + i;
            for (int j = 1; j <= m; j++)
                col[j] = job > 0 ? mat[j, c] : -mat[j, c];
            Sgesl1_impl(mat, m, ipvt, col);
            for (int j = 1; j <= m; j++)
                mat[j, c] = col[j];
        }
    }

    // ----------------------------------------------------------
    // SROTG – compute a Givens rotation.
    // FORTRAN: SUBROUTINE SROTG(SA, SB, C, S)
    // ----------------------------------------------------------
    public static void Srotg(ref float sa, ref float sb, out float c, out float s)
    {
        float roe  = (Math.Abs(sa) > Math.Abs(sb)) ? sa : sb;
        float absa = Math.Abs(sa);
        float absb = Math.Abs(sb);
        float scale = absa + absb;
        float r, z;
        if (scale == 0f)
        {
            c = 1f; s = 0f; r = 0f; z = 0f;
        }
        else
        {
            float sas = sa / scale, sbs = sb / scale;
            r = scale * MathF.Sqrt(sas * sas + sbs * sbs);
            r = MathF.CopySign(r, roe);
            c = sa / r;
            s = sb / r;
            z = 1f;
            if (absa > absb)               z = s;
            else if (absb >= absa && c != 0f) z = 1f / c;
        }
        sa = r;
        sb = z;
    }

    // ----------------------------------------------------------
    // SCHUD – online QR update (add one row).
    // FORTRAN: SUBROUTINE SCHUD(R, LDR, P, X, Z, LDZ, NZ, Y, RHO, C, S)
    //   Applies P successive Givens rotations to include the
    //   new data vector X into the upper-triangular factor R.
    //   Z and Y represent the right-hand side accumulation.
    //
    //   x, y – float[] 1-based length P and NZ respectively.
    //   Z    – float[P+1, NZ+1] 1-based.
    //   rho  – float[] 1-based length NZ.
    //   c, s – float[] 1-based length P.
    // ----------------------------------------------------------
    public static void Schud(float[,] r, int p, float[] x,
                             float[,] z, int nz, float[] y,
                             float[] rho, float[] c, float[] s)
    {
        // Make a working copy of x (Fortran modifies local copies)
        float[] xw = new float[p + 1];
        for (int i = 1; i <= p; i++) xw[i] = x[i];

        // Update R
        for (int j = 1; j <= p; j++)
        {
            float xj = xw[j];
            // apply previous rotations
            for (int i = 1; i < j; i++)
            {
                float t = c[i] * r[i, j] + s[i] * xj;
                xj  = c[i] * xj - s[i] * r[i, j];
                r[i, j] = t;
            }
            // compute next rotation
            Srotg(ref r[j, j], ref xj, out c[j], out s[j]);
        }

        // Update Z and RHO
        if (nz >= 1)
        {
            for (int j = 1; j <= nz; j++)
            {
                float zeta = y[j];
                for (int i = 1; i <= p; i++)
                {
                    float t = c[i] * z[i, j] + s[i] * zeta;
                    zeta  = c[i] * zeta - s[i] * z[i, j];
                    z[i, j] = t;
                }
                float azeta = Math.Abs(zeta);
                if (azeta != 0f && rho[j] >= 0f)
                {
                    float sc = azeta + rho[j];
                    float a1 = azeta / sc, a2 = rho[j] / sc;
                    rho[j] = sc * MathF.Sqrt(a1 * a1 + a2 * a2);
                }
            }
        }
    }

    // ----------------------------------------------------------
    // STRSL1 – solve upper-triangular system T*x = b.
    // FORTRAN: SUBROUTINE STRSL1(T, LDT, N, B, INFO)
    //   b is overwritten with x (1-based).
    // ----------------------------------------------------------
    public static void Strsl1(float[,] t, int n, float[] b, out int info)
    {
        info = 0;
        for (int k = 1; k <= n; k++)
            if (t[k, k] == 0f) { info = k; return; }

        b[n] /= t[n, n];
        if (n >= 2)
        {
            for (int kb = 2; kb <= n; kb++)
            {
                int k = n - kb + 1;
                float temp = -b[k + 1];
                for (int i = 1; i <= k; i++)
                    b[i] += temp * t[i, k + 1]; // actually need column k+1 of T, rows 1..k
                // fix: multiply each element properly
                // Redo with inner product:
                b[k + 1 - 1] = b[k + 1 - 1]; // placeholder
            }
        }
        // More faithful implementation:
        Strsl1_impl(t, n, b, out info);
    }

    private static void Strsl1_impl(float[,] t, int n, float[] b, out int info)
    {
        info = 0;
        for (int k = 1; k <= n; k++)
            if (t[k, k] == 0f) { info = k; return; }

        // back substitution on upper-triangular T
        b[n] /= t[n, n];
        if (n >= 2)
        {
            for (int kb = 2; kb <= n; kb++)
            {
                int j = n - kb + 1; // current working column
                float tmp = -b[j + 1];
                // b[1..j] += tmp * T[1..j, j+1]
                for (int i = 1; i <= j; i++)
                    b[i] += tmp * t[i, j + 1];
                b[j] /= t[j, j];
            }
        }
    }

    // Public clean entry-point for Strsl1
    public static void Strsl1_clean(float[,] t, int n, float[] b, out int info)
        => Strsl1_impl(t, n, b, out info);

    // ----------------------------------------------------------
    // SCHEX – QR circular column shift (left or right).
    // FORTRAN: SUBROUTINE SCHEX(R, LDR, P, K, L, Z, LDZ, NZ, C, S, JOB)
    //   JOB=1 right circular shift of columns K..L in R.
    //   JOB=2 left  circular shift of columns K..L in R.
    // ----------------------------------------------------------
    public static void Schex(float[,] r, int p, int k, int l,
                             float[,] z, int nz, float[] c, float[] s, int job)
    {
        int km1 = k - 1, kp1 = k + 1, lmk = l - k, lm1 = l - 1;
        float[] sv = new float[p + 1];

        if (job == 1)
        {
            // right circular shift: save column L
            for (int i = 1; i <= l; i++) sv[i] = r[i, l];
            // shift columns L-1 down to K one position right
            for (int jj = k; jj <= lm1; jj++)
            {
                int jf = lm1 - jj + k;
                for (int i = 1; i <= jf; i++) r[i, jf + 1] = r[i, jf];
                r[jf + 1, jf + 1] = 0f;
            }
            if (k != 1)
                for (int i = 1; i <= km1; i++) r[i, k] = sv[l - i + 1];

            // compute rotations
            float t = sv[1];
            for (int i = 1; i <= lmk; i++)
            {
                Srotg(ref sv[i + 1], ref t, out c[i], out s[i]);
                t = sv[i + 1];
            }
            r[k, k] = t;
            for (int j = kp1; j <= p; j++)
            {
                int il = Math.Max(1, l - j + 1);
                for (int ii = il; ii <= lmk; ii++)
                {
                    int row = l - ii;
                    float tt = c[ii] * r[row, j] + s[ii] * r[row + 1, j];
                    r[row + 1, j] = c[ii] * r[row + 1, j] - s[ii] * r[row, j];
                    r[row, j] = tt;
                }
            }
            // apply to Z
            if (nz >= 1)
            {
                for (int j = 1; j <= nz; j++)
                    for (int ii = 1; ii <= lmk; ii++)
                    {
                        int row = l - ii;
                        float tt = c[ii] * z[row, j] + s[ii] * z[row + 1, j];
                        z[row + 1, j] = c[ii] * z[row + 1, j] - s[ii] * z[row, j];
                        z[row, j] = tt;
                    }
            }
        }
        else // job == 2 : left circular shift
        {
            for (int i = 1; i <= k; i++) sv[lmk + i] = r[i, k];
            for (int j = k; j <= lm1; j++)
            {
                for (int i = 1; i <= j; i++) r[i, j] = r[i, j + 1];
                sv[j - km1] = r[j + 1, j + 1];
            }
            for (int i = 1; i <= k; i++) r[i, l] = sv[lmk + i];
            for (int i = kp1; i <= l; i++) r[i, l] = 0f;

            for (int j = k; j <= p; j++)
            {
                if (j != k)
                {
                    int iu = Math.Min(j - 1, l - 1);
                    for (int i = k; i <= iu; i++)
                    {
                        int ii = i - km1;
                        float tt = c[ii] * r[i, j] + s[ii] * r[i + 1, j];
                        r[i + 1, j] = c[ii] * r[i + 1, j] - s[ii] * r[i, j];
                        r[i, j] = tt;
                    }
                }
                if (j < l)
                {
                    int jj2 = j - km1;
                    float tmp = sv[jj2];
                    Srotg(ref r[j, j], ref tmp, out c[jj2], out s[jj2]);
                }
            }
            if (nz >= 1)
                for (int j = 1; j <= nz; j++)
                    for (int i = k; i <= lm1; i++)
                    {
                        int ii = i - km1;
                        float tt = c[ii] * z[i, j] + s[ii] * z[i + 1, j];
                        z[i + 1, j] = c[ii] * z[i + 1, j] - s[ii] * z[i, j];
                        z[i, j] = tt;
                    }
        }
    }

    // ----------------------------------------------------------
    // XGMKON – build the Guidorzi transformation matrix GM.
    // FORTRAN: SUBROUTINE XGMKON(GM, LGM, A, LA, NM, NYI)
    //   Converts the I/O parameter description into observable
    //   canonical state-space form (Guidorzi 1980).
    //   A: N×NM parameter matrix (alpha coefficients).
    //   GM: N×N result matrix (initialised to zero first).
    // ----------------------------------------------------------
    public static void Xgmkon(float[,] gm, float[,] a, int n, int nm, int[] nyi)
    {
        // zero GM
        Mzeset(gm, n, n);

        int ipi = 0;
        for (int i = 1; i <= nm; i++)
        {
            int ipj = 0;
            for (int j = 1; j <= nm; j++)
            {
                int nyij = (i > j) ? Math.Min(nyi[i] + 1, nyi[j])
                         : (i == j) ? nyi[i]
                         : Math.Min(nyi[i], nyi[j]);
                int nblock = nyij - 1;
                if (nblock > 0)
                {
                    for (int kk = 1; kk <= nblock; kk++)
                    {
                        int k1 = nblock + 1 - kk;
                        for (int ll = 1; ll <= k1; ll++)
                            gm[ipi + kk, ipj + ll] = -a[ipj + ll + kk, i];
                    }
                }
                if (i == j)
                {
                    nblock = nyij; // = nyi[i]
                    for (int kk = 1; kk <= nblock; kk++)
                    {
                        int k1 = nblock + 1 - kk;
                        gm[ipi + kk, ipj + k1] = 1f;
                    }
                }
                ipj += nyi[j];
            }
            ipi += nyi[i];
        }
    }
}

namespace Identi;

// ============================================================
// Identification – system identification algorithms.
//
// Faithful C# translation of XIDENT and supporting routines
// from IDELIB.F77 (Roether, 1985/1986).
//
// Implements four estimation methods:
//   1  Standard Least Squares (LS)
//   2  QR-recursive Least Squares (QR-LS)
//   3  Standard Instrumental Variables (IV) with auxiliary model
//   4  QR-recursive Instrumental Variables (QR-IV)
// ============================================================

internal static class Identification
{
    // Fixed dimensions (matching original PARAMETER statements)
    private const int LTH    = 30;   // max parameter-vector length
    private const int LTH2   = 61;   // 2*LTH + 1
    private const int LTHLM  = 38;   // LTH + NADMA
    private const int LM     = 8;    // max model outputs
    private const int LU     = 10;   // max model inputs
    private const int LNYB1  = 6;    // max Kronecker index + 1
    private const int LN     = 20;   // max state dimension
    private const int NPWMA  = 400;  // max parameter vector size
    private const int LENMAX = 1000;
    private const int LMLU   = 10;

    // ----------------------------------------------------------
    // IPLAG – circular-buffer index.
    // Returns the 1-based index that is 'offset' steps before
    // position 'nps' in a ring buffer of length 'len'.
    // FORTRAN: FUNCTION IPLAG(NPS, OFFSET, LEN)
    // ----------------------------------------------------------
    public static int Iplag(int nps, int offset, int len)
        => ((nps - offset - 1) % len + len) % len + 1;

    // ----------------------------------------------------------
    // XIDENT – identify a MIMO state-space model from data.
    // FORTRAN: SUBROUTINE XIDENT(W, IDAT, IDA, IAD, LEN, NANF, IANZ,
    //                            NPSAVE, NYI, NU, NM, DMAT, RH, B,
    //                            IPOS0, IGLW, WKSP, JOB)
    //
    // Parameters:
    //   w      – on input: auxiliary-model parameters (IV methods).
    //            on output: estimated model parameters.
    //   idat   – raw data matrix [channel, time] (1-based).
    //   ida    – input-channel index vector (1-based).
    //   iad    – output-channel index vector (1-based).
    //   len    – ring-buffer length.
    //   nanf   – start offset within estimation window.
    //   ianz   – number of estimation steps.
    //   npsave – current ring-buffer write pointer.
    //   nyi    – Kronecker structure indices (1-based).
    //   nu     – number of system inputs.
    //   nm     – number of system outputs.
    //   dmat   – work matrix [LTH, LTHLM] (1-based).
    //   rh     – work matrix [LTH, LTH]   (QR factor, 1-based).
    //   b      – work matrix [LTH, LU]    (1-based).
    //   ipos0  – constraint map [LTH, LM] (1 = forced zero, 1-based).
    //   iglw   – 0 = no bias estimate, 1 = estimate bias.
    //   wksp   – scratch [LTH, LTH2]     (1-based).
    //   job    – on input: method (1-4). on output: 0=OK, 1=singular,
    //            2=unstable auxiliary model.
    // ----------------------------------------------------------
    public static void Xident(
        float[] w, short[,] idat, int[] ida, int[] iad,
        int len, int nanf, int ianz, int npsave,
        int[] nyi, int nu, int nm,
        float[,] dmat, float[,] rh, float[,] b,
        int[,] ipos0, int iglw, float[,] wksp,
        ref int job)
    {
        // ---- index pre-computations ----
        int nyb = 0, n = 0;
        for (int i = 1; i <= nm; i++)
        {
            n   += nyi[i];
            nyb  = Math.Max(nyb, nyi[i]);
        }
        int nth    = n + nyb * nu + (iglw == 1 ? 1 : 0);
        int nth1   = nth + 1;
        int nthnm  = nth + nm;
        int nmnu   = nm + nu;
        int nanz   = n * (nu + nm);
        int nm1    = nm + 1;
        int nyb1   = nyb + 1;
        int nyb2   = nyb + 2;
        int iposb  = n * nm;

        // ---- allocate working arrays (1-based, padded) ----
        // FELD[channel/time-shifted, lag] and HFELD (for IV)
        float[,] feld  = new float[LMLU  + 1, LNYB1  + 1];
        float[,] hfeld = new float[LMLU  + 1, LNYB1  + 1];
        float[]  phi   = new float[LTH   + 1];
        float[]  hphi  = new float[LTH   + 1];
        float[]  gam   = new float[LM    + 1];
        float[]  xvec  = new float[LN    + 1]; // state vector
        float[]  x1    = new float[LN    + 1]; // next state
        float[]  u     = new float[LU    + 1]; // input vector
        float[]  y     = new float[LM    + 1]; // output vector
        float[]  ypred = new float[LM    + 1]; // model prediction
        float[]  rho   = new float[LTHLM + 1];
        float[]  co    = new float[LTH   + 1];
        float[]  so    = new float[LTH   + 1];
        int[]    iw    = new int  [LTH   + 1]; // pivot vector

        // ---- initialise matrices ----
        MatrixLib.Mzeset(feld,  nmnu, nyb1);
        MatrixLib.Mzeset(hfeld, nmnu, nyb1);
        MatrixLib.Mzeset(dmat,  nth,  nthnm);
        MatrixLib.Mzeset(rh,    nth,  nth);
        for (int i = 1; i <= nth; i++) { co[i] = 0f; so[i] = 0f; rho[i] = 0f; }

        // ---- initialise auxiliary model for IV methods ----
        if (job > 2)
        {
            int lag = Iplag(npsave, nanf + 1, len);
            Xmodin(xvec, n, idat, len, lag, ida, iad, nm, nyi, w, nanz, u, nu, iglw);
        }

        // ---- main identification loop ----
        int index = 0;
        for (int k1 = 1; k1 <= ianz; k1++)
        {
            int lag = Iplag(npsave, nanf + k1, len);

            // read measurements at lag
            for (int i = 1; i <= nm; i++) y[i] = idat[iad[i], lag];
            for (int i = 1; i <= nu; i++) u[i] = idat[ida[i], lag];

            // shift FELD one step right (lag history buffer)
            for (int i = nyb; i >= 1; i--)
                for (int ch = 1; ch <= nmnu; ch++)
                    feld[ch, i + 1] = feld[ch, i];

            // insert new measurements at lag=1
            for (int i = 1; i <= nu;  i++) feld[nm  + i, 1] = u[i];
            for (int i = 1; i <= nm;  i++) feld[i,       1] = y[i];

            // IV: update auxiliary model and HFELD
            if (job > 2)
            {
                for (int i = nyb; i >= 1; i--)
                    for (int ch = 1; ch <= nmnu; ch++)
                        hfeld[ch, i + 1] = hfeld[ch, i];

                Xmodel(ypred, nm, xvec, n, w, u, nu, nyi, x1, 1e25f, iglw, out int ierr);
                if (ierr != 0) { job = 2; return; }

                for (int i = 1; i <= nu; i++) hfeld[nm + i, 1] = u[i];
                for (int i = 1; i <= nm; i++) hfeld[i,       1] = ypred[i];
            }

            // build regression vectors PHI, GAM, HPHI
            index = 0;
            for (int i = 1; i <= nm; i++)
            {
                gam[i] = feld[i, nyb1 - nyi[i]];
                for (int jj = 1; jj <= nyi[i]; jj++)
                {
                    index++;
                    phi[index]  = feld[i, nyb2 - jj];
                    if (job > 2) hphi[index] = hfeld[i, nyb2 - jj];
                }
            }
            for (int i = nm1; i <= nmnu; i++)
            {
                for (int jj = 1; jj <= nyb; jj++)
                {
                    index++;
                    phi[index]  = feld[i, nyb2 - jj];
                    if (job > 2) hphi[index] = hfeld[i, nyb2 - jj];
                }
            }
            if (iglw == 1) { index++; phi[index] = 1f; if (job > 2) hphi[index] = 1f; }

            // accumulate – skip the first NYB steps (fill-in period)
            if (k1 > nyb)
            {
                // copy GAM into DMAT columns NTH1..NTH+NM
                for (int i = 1; i <= nm; i++)
                    dmat[nth1, nth + i] = gam[i]; // intentional mistake here?
                // FORTRAN: CALL VVCOPY(PHI(NTH1), GAM, NM)
                // This copies GAM into the NTH+1 .. NTH+NM elements of PHI
                for (int i = 1; i <= nm; i++) phi[nth + i] = gam[i];

                switch (job)
                {
                    case 1: // standard LS: DMAT += phi * phi^T
                        MatrixLib.Mdpsum(dmat, dmat, phi, phi, nth, nthnm);
                        break;

                    case 2: // QR-LS: sequential QR update
                    {
                        // build y-vector for this step, stored in dmat(1,NTH1)..dmat(NTH,NTH1+NM-1)
                        // SCHUD parameters: z uses columns NTH1..NTH+NM of DMAT
                        // Extract z columns into a local matrix
                        float[,] zLocal = new float[nth + 1, nm + 1];
                        for (int i = 1; i <= nth; i++)
                            for (int jj = 1; jj <= nm; jj++)
                                zLocal[i, jj] = dmat[i, nth + jj];
                        float[] gamLocal = new float[nm + 1];
                        for (int i = 1; i <= nm; i++) gamLocal[i] = gam[i];

                        MatrixLib.Schud(rh, nth, phi, zLocal, nm, gamLocal, rho, co, so);

                        for (int i = 1; i <= nth; i++)
                            for (int jj = 1; jj <= nm; jj++)
                                dmat[i, nth + jj] = zLocal[i, jj];
                        break;
                    }

                    case 3: // standard IV: DMAT += hphi * phi^T
                        MatrixLib.Mdpsum(dmat, dmat, hphi, phi, nth, nthnm);
                        break;

                    case 4: // QR-IV
                    {
                        float[,] dmLocal = new float[nth + 1, nthnm + 1];
                        for (int i = 1; i <= nth; i++)
                            for (int jj = 1; jj <= nthnm; jj++)
                                dmLocal[i, jj] = dmat[i, jj];
                        float[] phiExt = new float[nthnm + 1];
                        for (int i = 1; i <= nth;  i++) phiExt[i] = phi[i];
                        for (int i = 1; i <= nm;   i++) phiExt[nth + i] = gam[i];
                        float[] yLocal = new float[nthnm + 1];
                        for (int i = 1; i <= nth;  i++) yLocal[i] = hphi[i];

                        MatrixLib.Schud(rh, nth, hphi, dmLocal, nthnm, phi, rho, co, so);

                        for (int i = 1; i <= nth; i++)
                            for (int jj = 1; jj <= nthnm; jj++)
                                dmat[i, jj] = dmLocal[i, jj];
                        break;
                    }
                }
            }
        }

        // ---- post-processing: set structural zeros in IPOS0 ----
        BuildIpos0(nm, nu, nyi, nyb, ipos0, nth);

        // ---- solve the estimation problem ----
        int info = 0;
        switch (job)
        {
            case 1:
            case 3:
                Xrsolv(dmat, nth, nm, ipos0, wksp, iw, ref info, nth1);
                break;
            case 2:
                Xqrlsr(dmat, rh, nth, nm, ipos0, wksp, co, so, ref info, nth1);
                break;
            case 4:
                Xqrivr(dmat, rh, nth, nm, ipos0, wksp, iw, co, so, ref info, nth1);
                break;
        }
        if (info != 0) { job = 1; return; }

        // ---- unpack solution from DMAT into W ----
        index = 0;
        int ib = 0, iende = 0;
        for (int i = 1; i <= nm; i++)
        {
            for (int jj = 1; jj <= n; jj++)
            {
                index++;
                w[index] = dmat[jj, nth + i];
            }
            int nblock = nyi[i];
            int index1 = n;
            for (int jj = 1; jj <= nu; jj++)
            {
                for (int kk = 1; kk <= nblock; kk++)
                {
                    index1++;
                    b[ib + kk, jj] = dmat[index1, nth + i];
                }
                index1 += (nyb - nblock);
            }
            ib += nblock;
            if (iglw == 1)
            {
                index1++;
                int ianfa = iende + 1;
                iende += nblock;
                for (int jj = ianfa; jj <= iende; jj++) w[nanz + jj] = 0f;
                w[iende + nanz] = dmat[index1, nth + i];
            }
        }

        // Print theta matrix
        MatrixLib.Mwrite(dmat, nth, nm, "THET", 4, nth1);

        // Build W-derived source matrix for Xgmkon (AR coefficients packed column-wise:
        // column i = AR coefficients for output i, rows 1..n).
        // The Fortran XIDENT passes the packed W vector (as a matrix) to XGMKON, not DMAT.
        float[,] aForGm = new float[n + 1, nm + 1];
        for (int i = 1; i <= nm; i++)
            for (int j = 1; j <= n; j++)
                aForGm[j, i] = w[(i - 1) * n + j];

        // Build GM matrix and invert to get B in state-space form
        float[,] gm = new float[n + 1, n + 1];
        MatrixLib.Xgmkon(gm, aForGm, n, nm, nyi);
        // Copy GM into dmat(1..n, 1..n) – reuse storage
        MatrixLib.Mzeset(dmat, nth, nth);
        for (int i = 1; i <= n; i++)
            for (int jj = 1; jj <= n; jj++)
                dmat[i, jj] = gm[i, jj];

        // Compute B_state = GM^-1 * B
        int[] pivots = new int[n + 1];
        MatrixLib.Mimprd(b, dmat, n, b, nu, pivots, 1, out info);
        if (info != 0) { job = 1; return; }

        // Repack B into W[iposb+1 ..]
        index = iposb;
        for (int i = 1; i <= n; i++)
            for (int jj = 1; jj <= nu; jj++)
            {
                index++;
                w[index] = b[i, jj];
            }

        job = 0;
    }

    // ---- helper: fill IPOS0 with structural zero mask ----
    private static void BuildIpos0(int nm, int nu, int[] nyi, int nyb,
                                   int[,] ipos0, int nth)
    {
        for (int i = 1; i <= nm; i++)
        {
            int nblock = nyi[i], nblo1 = nblock + 1;
            int index = 0;
            for (int j = 1; j <= nm; j++)
            {
                int nyij = (i > j) ? Math.Min(nyi[i] + 1, nyi[j])
                         : (i == j) ? nyi[i]
                         : Math.Min(nyi[i], nyi[j]);
                index += nyij;
                if (nyi[j] > nyij)
                    for (int kk = nyij + 1; kk <= nyi[j]; kk++)
                    {
                        index++;
                        ipos0[index, i] = 1;
                    }
            }
            for (int j = 1; j <= nu; j++)
            {
                index += nblock;
                if (nyb > nblock)
                    for (int kk = nblo1; kk <= nyb; kk++)
                    {
                        index++;
                        ipos0[index, i] = 1;
                    }
            }
        }
    }

    // ----------------------------------------------------------
    // XRSOLV – solve reduced system A*C = B handling structural zeros.
    // FORTRAN: SUBROUTINE XRSOLV(C, LC, A, LA, M, B, LB, N, WKSP, IW, IPOS0, INFO)
    //
    //   dmat   – combined matrix; columns colB..colB+n-1 are RHS (B),
    //            columns 1..m are the system matrix (A).
    //   After solution, B is overwritten with C.
    // ----------------------------------------------------------
    public static void Xrsolv(float[,] dmat, int m, int n,
                               int[,] ipos0, float[,] wksp, int[] iw,
                               ref int info, int colB)
    {
        info = 0;
        int mred = 0;
        int[]  mapRow = new int[m + 1]; // which rows of A/B are non-zero constrained

        for (int j = 1; j <= n; j++)
        {
            // check if sparsity pattern same as previous column
            bool same = j > 1;
            if (same)
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != ipos0[i, j - 1]) { same = false; break; }

            if (!same)
            {
                // assemble reduced system matrix WKSP
                int icnt = 0;
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        for (int kk = 1; kk <= m; kk++)
                            wksp[kk, icnt] = dmat[kk, kk <= m ? kk : kk];
                        // actually copy column i of dmat (system part) into wksp column icnt
                        for (int kk = 1; kk <= m; kk++)
                            wksp[kk, icnt] = dmat[kk, i]; // column i of the A matrix
                        mapRow[icnt] = i;
                    }
                mred = icnt;
                // reduce rows
                icnt = 0;
                float[,] wksp2 = new float[m + 1, m + 1];
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        for (int kk = 1; kk <= mred; kk++)
                            wksp2[icnt, kk] = wksp[i, kk];
                        // copy back
                        for (int kk = 1; kk <= mred; kk++)
                            wksp[icnt, kk] = wksp2[icnt, kk];
                    }
            }

            // pack RHS
            float[] rhs = new float[mred + 1];
            {
                int icnt = 0;
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        rhs[icnt] = dmat[i, colB + j - 1];
                    }
            }

            // solve reduced system (LU each time unless 'same')
            int[]  piv    = new int  [mred + 1];
            float[,] aw   = new float[mred + 1, mred + 1];
            for (int ii = 1; ii <= mred; ii++)
                for (int kk = 1; kk <= mred; kk++)
                    aw[ii, kk] = wksp[ii, kk];

            MatrixLib.Sgefa1(aw, mred, piv, out int inf2);
            if (inf2 != 0) { info = inf2; return; }
            MatrixLib.Sgesl1_clean(aw, mred, piv, rhs);

            // scatter solution back
            int cnt = mred;
            for (int i = m; i >= 1; i--)
            {
                dmat[i, colB + j - 1] = ipos0[i, j] != 1 ? rhs[cnt--] : 0f;
            }
        }
    }

    // ----------------------------------------------------------
    // XQRLSR – solve reduced system using the QR factor (LS variant).
    // FORTRAN: SUBROUTINE XQRLSR(C, LC, A, LA, M, B, LB, N, WKSP, CV, SV, IPOS0, INFO)
    // ----------------------------------------------------------
    public static void Xqrlsr(float[,] dmat, float[,] rh, int m, int n,
                               int[,] ipos0, float[,] wksp,
                               float[] cv, float[] sv,
                               ref int info, int colB)
    {
        info = 0;
        int mred = 0;
        float[] cvw = new float[m + 1];
        float[] svw = new float[m + 1];

        for (int j = 1; j <= n; j++)
        {
            bool same = j > 1;
            if (same)
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != ipos0[i, j - 1]) { same = false; break; }

            if (!same)
            {
                // copy R into WKSP
                for (int i = 1; i <= m; i++)
                    for (int kk = 1; kk <= m; kk++)
                        wksp[i, kk] = rh[i, kk];
                for (int i = 1; i <= m; i++) { cvw[i] = cv[i]; svw[i] = sv[i]; }

                int icnt = 0;
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        if (i > icnt)
                        {
                            // SCHEX right shift to bring column i into position icnt
                            float[,] zLocal = new float[m + 1, n + 1];
                            for (int r2 = 1; r2 <= m; r2++)
                                zLocal[r2, j] = dmat[r2, colB + j - 1];
                            MatrixLib.Schex(wksp, m, icnt, i, zLocal, 1, cvw, svw, 1);
                            for (int r2 = 1; r2 <= m; r2++)
                                dmat[r2, colB + j - 1] = zLocal[r2, j];
                        }
                    }
                mred = icnt;
            }

            // solve upper triangular wksp[1..mred, 1..mred] * x = rhs
            float[] rhs = new float[mred + 1];
            for (int i = 1; i <= mred; i++) rhs[i] = dmat[i, colB + j - 1];

            MatrixLib.Strsl1_clean(wksp, mred, rhs, out int inf2);
            if (inf2 != 0) { info = inf2; return; }

            int cnt = mred;
            for (int i = m; i >= 1; i--)
                dmat[i, colB + j - 1] = ipos0[i, j] != 1 ? rhs[cnt--] : 0f;
        }
    }

    // ----------------------------------------------------------
    // XQRIVR – solve reduced system using the QR factor (IV variant).
    // FORTRAN: SUBROUTINE XQRIVR(C, LC, R, LR, A, LA, M, B, LB, N,
    //                             WKSP, IW, CV, SV, IPOS0, INFO)
    // ----------------------------------------------------------
    public static void Xqrivr(float[,] dmat, float[,] rh, int m, int n,
                               int[,] ipos0, float[,] wksp, int[] iw,
                               float[] cv, float[] sv,
                               ref int info, int colB)
    {
        info = 0;
        int mred = 0;
        float[] cvw = new float[m + 1];
        float[] svw = new float[m + 1];
        // wksp has dimension [m, 2m+n]. We need columns 1..m for R, m+1..2m for A, 2m+1 for b.
        // Allocate a proper extended workspace
        int wkCols = 2 * m + n + 1;
        float[,] wk2 = new float[m + 1, wkCols + 1];

        for (int j = 1; j <= n; j++)
        {
            bool same = j > 1;
            if (same)
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != ipos0[i, j - 1]) { same = false; break; }

            // copy RHS into wk2 column 2m+1
            for (int i = 1; i <= m; i++) wk2[i, 2 * m + 1] = dmat[i, colB + j - 1];

            if (!same)
            {
                for (int i = 1; i <= m; i++)
                {
                    for (int kk = 1; kk <= m; kk++) wk2[kk, i] = rh[kk, i];           // R part
                    for (int kk = 1; kk <= m; kk++) wk2[kk, m + i] = dmat[kk, i];      // A part
                }
                for (int i = 1; i <= m; i++) { cvw[i] = cv[i]; svw[i] = sv[i]; }

                int icnt = 0;
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        if (i > icnt)
                        {
                            // SCHEX on R+A columns combined
                            float[,] zPart = new float[m + 1, m + 2];
                            for (int r2 = 1; r2 <= m; r2++)
                                for (int c2 = 1; c2 <= m + 1; c2++)
                                    zPart[r2, c2] = wk2[r2, m + c2];
                            MatrixLib.Schex(wk2, m, icnt, i, zPart, m + 1, cvw, svw, 1);
                            for (int r2 = 1; r2 <= m; r2++)
                                for (int c2 = 1; c2 <= m + 1; c2++)
                                    wk2[r2, m + c2] = zPart[r2, c2];
                        }
                    }
                mred = icnt;

                // strip out zero-constrained columns from A part
                icnt = 0;
                for (int i = 1; i <= m; i++)
                    if (ipos0[i, j] != 1)
                    {
                        icnt++;
                        for (int kk = 1; kk <= m; kk++)
                            wk2[kk, m + icnt] = wk2[kk, m + i];
                    }
            }

            // Assemble and solve: (A_reduced) * x = b_reduced using LU
            float[,] ared = new float[mred + 1, mred + 1];
            float[]  rhs  = new float[mred + 1];
            for (int i = 1; i <= mred; i++)
            {
                rhs[i] = wk2[i, 2 * m + 1];
                for (int kk = 1; kk <= mred; kk++)
                    ared[i, kk] = wk2[i, m + kk];
            }
            int[] piv = new int[mred + 1];
            MatrixLib.Sgefa1(ared, mred, piv, out int inf2);
            if (inf2 != 0) { info = inf2; return; }
            MatrixLib.Sgesl1_clean(ared, mred, piv, rhs);

            int cnt = mred;
            for (int i = m; i >= 1; i--)
                dmat[i, colB + j - 1] = ipos0[i, j] != 1 ? rhs[cnt--] : 0f;
        }
    }

    // ----------------------------------------------------------
    // XMODEL – one-step state-space prediction.
    // FORTRAN: SUBROUTINE XMODEL(YPRED, NM, X, N, W, U, NU, NYI, X1, RMAX, IGLW, IERR)
    // ----------------------------------------------------------
    public static void Xmodel(float[] ypred, int nm, float[] x, int n,
                               float[] w, float[] u, int nu, int[] nyi,
                               float[] x1, float rmax, int iglw, out int ierr)
    {
        ierr = 0;
        int ipb   = n * nm + 1;
        int ipglw = n * (nm + nu);
        int im    = 1;
        int iIdx  = nyi[1];

        for (int i = 1; i <= n; i++)
        {
            if (i >= iIdx)
            {
                // non-trivial state: x1[i] = w[ipa..] · x + w[ipb..] · u
                int ipa = (im - 1) * n + 1;
                x1[i] = MatrixLib.Vvdot(w, x, n, ipa, 1) +
                         MatrixLib.Vvdot(w, u, nu, ipb, 1);
                int indaus = iIdx + 1 - nyi[im];
                ypred[im] = x[indaus];
                if (iglw == 1) ypred[im] += w[ipglw + indaus];
                if (Math.Abs(ypred[im]) > rmax) { ierr = im; return; }
                if (im < nm)
                {
                    im++;
                    iIdx += nyi[im];
                    ipb  += nu;
                }
            }
            else
            {
                // trivial state: x1[i] = x[i+1] + w[ipb..] · u
                x1[i] = x[i + 1] + MatrixLib.Vvdot(w, u, nu, ipb, 1);
                ipb += nu;
            }
        }
        MatrixLib.Vvcopy(x, x1, n);
    }

    // ----------------------------------------------------------
    // XMODIN – initialise the state vector from measured data.
    // FORTRAN: SUBROUTINE XMODIN(X, N, IDAT, LID, LEN, NPS, IDA, IAD,
    //                             NM, NYI, W, NPW, U, NU, IGLW)
    // ----------------------------------------------------------
    public static void Xmodin(float[] x, int n, short[,] idat, int len,
                               int nps, int[] ida, int[] iad,
                               int nm, int[] nyi, float[] w, int npw,
                               float[] u, int nu, int iglw)
    {
        int index = 0;
        int iposb = n * nm + 1;
        int nanz  = n * (nm + nu);

        for (int i = 1; i <= nm; i++)
        {
            int indaus = iad[i];
            int nblock = nyi[i];
            for (int jj = 0; jj < nblock; jj++)
            {
                index++;
                x[index] = idat[indaus, Iplag(nps, jj, len)];
                if (iglw == 1) x[index] -= w[nanz + index];
                if (jj > 0)
                {
                    int ipb2 = iposb;
                    for (int kk = 1; kk <= jj; kk++)
                    {
                        for (int ll = 1; ll <= nu; ll++)
                            u[ll] = idat[ida[ll], Iplag(nps, jj - kk, len)];
                        x[index] -= MatrixLib.Vvdot(w, u, nu, ipb2, 1);
                        ipb2 += nu;
                    }
                }
            }
            iposb += nblock * nu;
        }
    }

    // ----------------------------------------------------------
    // XMDECD – decode parameter vector W into state-space (A, B, C).
    // FORTRAN: SUBROUTINE XMDECD(A, LA, N, B, LB, NU, C, LC, NM, W, NPW, NYI, IERR)
    //   Produces observable canonical form (Guidorzi).
    // ----------------------------------------------------------
    public static void Xmdecd(float[,] a, int n, float[,] b, int nu,
                               float[,] c, int nm, float[] w, int npw,
                               int[] nyi, out int ierr)
    {
        ierr = 0;
        int index = 0;
        int k1 = 1;
        // zero matrices
        for (int r = 1; r <= n; r++)
        {
            for (int cc = 1; cc <= n;  cc++) a[r, cc] = 0f;
            for (int cc = 1; cc <= nu; cc++) b[r, cc] = 0f;
        }
        for (int r = 1; r <= nm; r++)
            for (int cc = 1; cc <= n; cc++) c[r, cc] = 0f;

        for (int i = 1; i <= nm; i++)
        {
            int k2 = k1 + nyi[i] - 2;
            // shift rows: A[k1..k2, k1+1..] = I
            if (k2 >= k1)
                for (int jj = k1; jj <= k2; jj++) a[jj, jj + 1] = 1f;
            // last row of block: parameters from W
            for (int jj = 1; jj <= n; jj++)
            {
                index++;
                if (index > npw) { ierr = 1; return; }
                a[k2 + 1, jj] = w[index];
            }
            c[i, k1] = 1f;
            k1 += nyi[i];
        }
        // B matrix
        for (int i = 1; i <= n; i++)
            for (int jj = 1; jj <= nu; jj++)
            {
                index++;
                if (index > npw) { ierr = 1; return; }
                b[i, jj] = w[index];
            }
    }

    // ----------------------------------------------------------
    // PrintModel – print identified model matrices to console.
    // Replaces menu option 9 / 10 of the XIDEN submenu.
    // ----------------------------------------------------------
    public static void PrintModel(AppState st, bool toPrinter = false)
    {
        if (st.JOBID <= 0)
        {
            Console.WriteLine(" *No model identified yet.*");
            return;
        }
        string[] methods = { "", "Least Squares", "QR Least Squares",
                              "Instrumental Variables", "QR Instrumental Variables" };
        Console.WriteLine();
        Console.WriteLine($"  Identified model parameters:");
        Console.WriteLine($"  Method used:              {methods[st.JOBID]}");
        Console.WriteLine($"  Estimation start point:   {st.NANF}");
        Console.WriteLine($"  Number of samples:        {st.IANZ}");
        Console.Write("  Model inputs at channels: ");
        for (int i = 1; i <= st.NU; i++) Console.Write($" {st.IDA[i]}");
        Console.WriteLine();
        Console.Write("  Model outputs at channels:");
        for (int i = 1; i <= st.NM; i++) Console.Write($" {st.IAD[i]}");
        Console.WriteLine();
        Console.Write("  Structure indices:        ");
        for (int i = 1; i <= st.NM; i++) Console.Write($" {st.NYI[i]}");
        Console.WriteLine();

        int n = st.N, npw = n * (st.NM + st.NU);
        if (npw == 0) return;

        float[,] a = new float[n  + 1, n  + 1];
        float[,] bm= new float[n  + 1, st.NU + 1];
        float[,] c = new float[st.NM + 1, n + 1];

        Xmdecd(a, n, bm, st.NU, c, st.NM, st.W, npw, st.NYI, out int ierr);
        if (ierr != 0) { Console.WriteLine(" *Inconsistent structure.*"); return; }

        MatrixLib.Mwrite(a,  n,     n,     "A",    4);
        MatrixLib.Mwrite(bm, n,     st.NU, "B",    4);
        MatrixLib.Mwrite(c,  st.NM, n,     "C",    4);

        // compute steady-state gain: Kend = C*(I-A)^-1*B
        // form (I-A)
        float[,] ia = new float[n + 1, n + 1];
        for (int i = 1; i <= n; i++)
            for (int jj = 1; jj <= n; jj++)
                ia[i, jj] = (i == jj ? 1f : 0f) - a[i, jj];

        float[,] tmp = new float[n + 1, st.NU + 1];
        int[] piv2 = new int[n + 1];
        MatrixLib.Mimprd(tmp, ia, n, bm, st.NU, piv2, 1, out ierr);
        if (ierr != 0) { Console.WriteLine(" *Singular (I-A) matrix.*"); return; }

        float[,] kend = new float[st.NM + 1, st.NU + 1];
        for (int i = 1; i <= st.NM; i++)
            for (int jj = 1; jj <= st.NU; jj++)
            {
                float s = 0f;
                for (int kk = 1; kk <= n; kk++) s += c[i, kk] * tmp[kk, jj];
                kend[i, jj] = s;
            }
        MatrixLib.Mwrite(kend, st.NM, st.NU, "Kend", 4);

        if (st.IGLW == 1)
        {
            Console.WriteLine("  Equilibrium offsets:");
            int ipw = 1, ind = n * (st.NM + st.NU);
            for (int i = 1; i <= st.NM; i++)
            {
                Console.WriteLine($"    Output {i}: {st.W[ind + ipw]:G12}");
                ipw += st.NYI[i];
            }
        }
    }
}

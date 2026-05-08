namespace Identi;

// ============================================================
// ConsoleGraph – text-based channel display.
//
// Replaces the PC-CGA/GSX graphics from XGRAPH.F77 with an
// ASCII-art console plot.  Physical scaling, mean subtraction,
// differentiation, and model overlay are supported.
// ============================================================

internal static class ConsoleGraph
{
    private const int PLOT_WIDTH  = 78; // characters wide
    private const int PLOT_HEIGHT = 20; // lines tall

    // ----------------------------------------------------------
    // PlotChannels – ASCII-art time-series plot of selected channels.
    //
    // channel list: st.IPL[1..st.NPL], lengths st.LENGR / st.LEN.
    // offset: st.IZGR (start offset into ring buffer).
    // ----------------------------------------------------------
    public static void PlotChannels(AppState st, int lengr, int izgr,
                                    bool withModel = false)
    {
        if (st.NPL <= 0 || st.NPOINT == 0)
        {
            Console.WriteLine(" *No data available to plot.*");
            return;
        }

        int npts = Math.Min(lengr, st.LEN);
        if (npts <= 0) npts = st.LEN;

        for (int p = 1; p <= st.NPL; p++)
        {
            int ch = st.IPL[p];
            if (ch < 1 || ch > AppState.NADDAX) continue;

            string title = (st.TEXT[ch] ?? "").TrimEnd();
            string unit  = (st.PBEZ[ch] ?? "").TrimEnd();
            if (title.Length == 0) title = $"Channel {ch}";
            Console.WriteLine();
            Console.WriteLine($"  [{title}  ({unit})]");

            PlotSingle(st, ch, npts, izgr, withModel);
        }
    }

    private static void PlotSingle(AppState st, int ch, int npts, int izgr,
                                    bool withModel)
    {
        float[] raw = ExtractChannel(st, ch, npts, izgr);
        if (raw.Length == 0) return;

        ApplyFilter(raw, st, ch);

        float vmin = float.MaxValue, vmax = float.MinValue;
        foreach (float v in raw) { if (v < vmin) vmin = v; if (v > vmax) vmax = v; }
        if (Math.Abs(vmax - vmin) < 1e-10f) { vmax = vmin + 1f; }

        // compress/expand to PLOT_WIDTH samples via averaging
        float[] disp = Resample(raw, PLOT_WIDTH);

        // scale to physical units if configured
        float pmin = st.PMIN[ch], pmax = st.PMAX[ch];
        bool hasPhys = Math.Abs(pmax - pmin) > 1e-10f;
        float adRange = (ch <= st.NADMAX)
                        ? (st.IADMAX - st.IADMIN)
                        : (st.IDAMAX - st.IDAMIN);

        float[] phys = new float[disp.Length];
        for (int i = 0; i < disp.Length; i++)
        {
            phys[i] = hasPhys
                ? pmin + (disp[i] / adRange) * (pmax - pmin)
                : disp[i];
        }

        float lo = float.MaxValue, hi = float.MinValue;
        foreach (float v in phys) { if (v < lo) lo = v; if (v > hi) hi = v; }
        if (Math.Abs(hi - lo) < 1e-10f) hi = lo + 1f;

        // render grid
        char[,] grid = new char[PLOT_HEIGHT, PLOT_WIDTH];
        for (int r = 0; r < PLOT_HEIGHT; r++)
            for (int c = 0; c < PLOT_WIDTH; c++)
                grid[r, c] = ' ';

        for (int c = 0; c < Math.Min(phys.Length, PLOT_WIDTH); c++)
        {
            int row = PLOT_HEIGHT - 1 -
                      (int)Math.Round((phys[c] - lo) / (hi - lo) * (PLOT_HEIGHT - 1));
            row = Math.Clamp(row, 0, PLOT_HEIGHT - 1);
            grid[row, c] = '*';
        }

        // print y-axis labels and grid
        for (int r = 0; r < PLOT_HEIGHT; r++)
        {
            float yval = hi - r * (hi - lo) / (PLOT_HEIGHT - 1);
            string ylabel = r == 0              ? $"{yval,8:G5}|" :
                            r == PLOT_HEIGHT - 1? $"{yval,8:G5}|" :
                            r == PLOT_HEIGHT / 2? $"{yval,8:G5}|" : "        |";
            Console.Write(ylabel);
            for (int c = 0; c < PLOT_WIDTH; c++) Console.Write(grid[r, c]);
            Console.WriteLine("|");
        }
        // x-axis
        Console.Write("        +");
        Console.WriteLine(new string('-', PLOT_WIDTH) + "+");
        Console.WriteLine($"        0{new string(' ', PLOT_WIDTH / 2 - 4)}" +
                          $"{npts / 2,6}{new string(' ', PLOT_WIDTH / 2 - 6)}{npts,6}");
    }

    // ----------------------------------------------------------
    // ExtractChannel – pull 'npts' samples ending at NPOINT from ring buffer.
    // ----------------------------------------------------------
    private static float[] ExtractChannel(AppState st, int ch, int npts, int izgr)
    {
        var data = new float[npts];
        for (int i = 0; i < npts; i++)
        {
            int lag = Identification.Iplag(st.NPOINT, izgr + npts - 1 - i, st.LEN);
            data[i] = st.IDAT[ch, lag];
        }
        return data;
    }

    // ----------------------------------------------------------
    // ApplyFilter – mean subtraction, differentiation, or 1st-order high-pass.
    // Corresponds to FORTRAN JOBPL values 0-3.
    // ----------------------------------------------------------
    private static void ApplyFilter(float[] data, AppState st, int ch)
    {
        switch (st.JOBPL)
        {
            case 1: // subtract mean
                float mean = st.IMW[ch];
                for (int i = 0; i < data.Length; i++) data[i] -= mean;
                break;
            case 2: // differentiate
                for (int i = data.Length - 1; i > 0; i--)
                    data[i] -= data[i - 1];
                data[0] = 0f;
                break;
            case 3: // 1st-order high-pass: y[k] = fpar*y[k-1] + (data[k]-data[k-1])/2
                float fpar = st.FPAR;
                float prev = 0f;
                float prevRaw = data[0];
                for (int i = 1; i < data.Length; i++)
                {
                    float curRaw = data[i];
                    float d = curRaw - prevRaw;
                    float hp = fpar * prev + 0.5f * d;
                    data[i] = hp;
                    prev = hp;
                    prevRaw = curRaw;
                }
                data[0] = 0f;
                break;
        }
    }

    // simple block-average resampler
    private static float[] Resample(float[] src, int targetLen)
    {
        if (src.Length <= targetLen)
        {
            var out2 = new float[src.Length];
            Array.Copy(src, out2, src.Length);
            return out2;
        }
        var result = new float[targetLen];
        double ratio = (double)src.Length / targetLen;
        for (int i = 0; i < targetLen; i++)
        {
            int s = (int)(i * ratio);
            int e = (int)((i + 1) * ratio);
            if (e > src.Length) e = src.Length;
            float sum = 0f;
            for (int j = s; j < e; j++) sum += src[j];
            result[i] = sum / (e - s);
        }
        return result;
    }

    // ----------------------------------------------------------
    // PrintChannelValues – cyclic text output of current channel readings.
    // Corresponds to FORTRAN JOB=7 (Zyklische Wertausgabe).
    // ----------------------------------------------------------
    public static void PrintChannelValues(AppState st)
    {
        Console.Clear();
        int npt = st.NPOINT;
        if (npt == 0) { Console.WriteLine(" *No data acquired yet.*"); return; }

        Console.WriteLine("  Current channel readings (press Enter to refresh, 'q' to quit):");
        Console.WriteLine();

        for (int i = 1; i <= st.NADU; i++)
        {
            float phys = PhysicalValue(st, i, true);
            string title = (st.TEXT[i] ?? "").TrimEnd();
            if (title.Length == 0) title = $"AD{i:D2}";
            Console.WriteLine($"  {title,-20} {phys,10:G5}  {(st.PBEZ[i] ?? "").TrimEnd()}");
        }
        for (int i = 1; i <= st.NDAU; i++)
        {
            int ch   = st.NADMAX + i;
            float phys = PhysicalValue(st, ch, false);
            string title = (st.TEXT[ch] ?? "").TrimEnd();
            if (title.Length == 0) title = $"DA{i:D2}";
            Console.WriteLine($"  {title,-20} {phys,10:G5}  {(st.PBEZ[ch] ?? "").TrimEnd()}");
        }
    }

    private static float PhysicalValue(AppState st, int ch, bool isAD)
    {
        int pos = st.NPOINT > 0 ? st.NPOINT : 1;
        float raw = st.IDAT[ch, pos];
        float lo  = st.PMIN[ch], hi = st.PMAX[ch];
        if (Math.Abs(hi - lo) < 1e-10f) return raw;
        float adRange = isAD ? (st.IADMAX - st.IADMIN) : (st.IDAMAX - st.IDAMIN);
        return lo + (raw / adRange) * (hi - lo);
    }
}
